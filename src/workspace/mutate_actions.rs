//! Durable single-file action verbs.
//!
//! Home for the action mutations that edit one active `.actions` file in place —
//! `add` today, `update`/`delete` as the durable-verbs charter routes them here.
//! Each rides the shared [`with_locked_mutation`] seam: acquire the lock, recover
//! pending intent, read trusted state, produce a pure plan, render, and stage the
//! single changed file in one journaled batch.
//!
//! This is the sibling of `archive_actions`, which owns the *two*-file archival
//! move. The split is by mutation shape, not by verb name.

use std::path::{Path, PathBuf};

use uuid::Uuid;

use crate::domain::collect_subtree_ids;
use crate::domain::update::{ActionUpdate, apply_updates, disallowed_terminal_update};
use crate::workspace::action_files::read_actions;
use crate::workspace::actions::format::require_actions_formatting;
use crate::workspace::actions::{Action, ActionList};
use crate::workspace::mutation::{WriteSet, render, validate_source_path, with_locked_mutation};
use crate::workspace::selector::{ActionSelector, unique_selector_match};
use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};

/// Result of durably inserting one action into an active file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InsertActionResult {
    pub action_id: Uuid,
    pub parent_id: Option<Uuid>,
    pub source_path: PathBuf,
}

/// Insert one action into an active list without touching the filesystem.
///
/// The new action is parented to `parent_id` (already resolved against the same
/// list) and placed immediately after that parent's full subtree, so siblings
/// stay contiguous. A parentless action is appended. The caller owns every other
/// field of `new_action`, including its id and `created_at` stamp.
pub fn plan_action_insert(
    active: &[Action],
    new_action: Action,
    parent_id: Option<Uuid>,
) -> ActionList {
    let mut list = active.to_vec();
    let mut action = new_action;
    action.parent_id = parent_id;

    let index = parent_id
        .map(|id| index_after_subtree(&list, id))
        .unwrap_or(list.len());
    list.insert(index, action);
    list
}

/// Return the insertion point immediately after `parent_id`'s full subtree.
fn index_after_subtree(actions: &[Action], parent_id: Uuid) -> usize {
    let subtree = collect_subtree_ids(actions, parent_id);
    actions
        .iter()
        .enumerate()
        .filter(|(_, action)| subtree.contains(&action.id))
        .map(|(index, _)| index + 1)
        .max()
        .unwrap_or(actions.len())
}

/// Add one action to an active `.actions` file as a single locked, journaled
/// mutation.
///
/// Runs on the shared [`with_locked_mutation`] seam: the lock is acquired and any
/// interrupted mutation is recovered before the file is read, so the parent is
/// resolved and the action inserted against trusted state. An interrupted commit
/// can only recover forward to the post-add file.
///
/// `parent` is resolved under the lock rather than trusted from a pre-lock client
/// read: an id-less parent line's in-memory UUID changes across reloads, so the
/// selector's alias/name fallback is what keeps the handoff stable — the same
/// reason [`close_action_subtree`](super::archive_actions::close_action_subtree)
/// carries a selector.
pub fn insert_action(
    workspace_root: &Path,
    source_path: &Path,
    new_action: Action,
    parent: Option<&ActionSelector>,
) -> Result<InsertActionResult, WorkspaceError> {
    let layout = resolve_workspace_layout(workspace_root);
    validate_source_path(source_path, &layout.charter_root)?;
    require_actions_formatting().map_err(WorkspaceError::Actions)?;

    with_locked_mutation(&layout, |_layout| {
        let active = read_actions(source_path)?;

        let parent_id = match parent {
            Some(selector) => Some(unique_selector_match(&active, selector)?.ok_or_else(|| {
                WorkspaceError::Actions(format!(
                    "parent action not found in source file: {}",
                    selector.name
                ))
            })?),
            None => None,
        };

        let action_id = new_action.id;
        let planned = plan_action_insert(&active, new_action, parent_id);

        let mut writes = WriteSet::new();
        writes.stage(source_path.to_path_buf(), render(&planned)?);

        Ok((
            writes,
            InsertActionResult {
                action_id,
                parent_id,
                source_path: source_path.to_path_buf(),
            },
        ))
    })
}

/// Result of durably updating one action's fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UpdateActionResult {
    pub action_id: Uuid,
    pub source_path: PathBuf,
}

/// Update one action's fields in an active `.actions` file as a single locked,
/// journaled mutation.
///
/// Runs on the shared [`with_locked_mutation`] seam, mirroring
/// [`insert_action`]: the target `selector` is re-resolved under the lock (an
/// id-less line's in-memory UUID changes across reloads), the update is applied
/// in place, and the single file is staged in one journaled batch.
///
/// A terminal `state` is rejected before the lock is taken:
/// [`disallowed_terminal_update`] steers those requests to `complete`/`cancel`,
/// which cascade to the subtree and archive it. A field update only edits the
/// one action, so it must never leave the tree half-closed.
pub fn update_action(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
    update: ActionUpdate,
) -> Result<UpdateActionResult, WorkspaceError> {
    if let Some(state) = disallowed_terminal_update(&update) {
        return Err(WorkspaceError::Actions(format!(
            "cannot set state to {state:?} via update; use complete/cancel, which cascade to the \
             subtree and archive it"
        )));
    }

    let layout = resolve_workspace_layout(workspace_root);
    validate_source_path(source_path, &layout.charter_root)?;
    require_actions_formatting().map_err(WorkspaceError::Actions)?;

    with_locked_mutation(&layout, |_layout| {
        let mut active = read_actions(source_path)?;

        let action_id = unique_selector_match(&active, selector)?.ok_or_else(|| {
            WorkspaceError::Actions(format!(
                "open action not found in source file: {}",
                selector.name
            ))
        })?;

        let target = active
            .iter_mut()
            .find(|action| action.id == action_id)
            .expect("selected action came from active list");
        apply_updates(target, update);

        let mut writes = WriteSet::new();
        writes.stage(source_path.to_path_buf(), render(&active)?);

        Ok((
            writes,
            UpdateActionResult {
                action_id,
                source_path: source_path.to_path_buf(),
            },
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::ActionState;

    fn action(name: &str, parent_id: Option<Uuid>) -> Action {
        Action {
            id: Uuid::new_v4(),
            name: name.to_string(),
            state: ActionState::NotStarted,
            parent_id,
            ..Default::default()
        }
    }

    #[test]
    fn plan_appends_a_parentless_action() {
        let a = action("first", None);
        let b = action("second", None);
        let active = vec![a.clone(), b.clone()];

        let planned = plan_action_insert(&active, action("third", None), None);

        assert_eq!(planned.len(), 3);
        assert_eq!(planned[2].name, "third");
        assert_eq!(planned[2].parent_id, None);
    }

    #[test]
    fn plan_places_a_child_after_the_parents_subtree() {
        let parent = action("parent", None);
        let child = action("existing child", Some(parent.id));
        let sibling = action("later root", None);
        let active = vec![parent.clone(), child.clone(), sibling.clone()];

        let planned = plan_action_insert(&active, action("new child", None), Some(parent.id));

        // Inserted after the parent's existing child, before the later root, and
        // parented to the requested parent.
        let names: Vec<&str> = planned.iter().map(|a| a.name.as_str()).collect();
        assert_eq!(
            names,
            ["parent", "existing child", "new child", "later root"]
        );
        assert_eq!(planned[2].parent_id, Some(parent.id));
    }
}
