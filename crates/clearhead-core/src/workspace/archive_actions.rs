//! Pure partitioning for root-tree archival. Closure is a separate transition.
use super::actions::{Action, ActionList, OutputFormat, format};
use super::mutate_actions::{ActionPrepareError, ActionResourceState};
use super::resource::{Effect, EffectBatch, ResourceLocation, ResourcePrecondition, WorkspacePath};
use crate::domain::collect_subtree_ids;
use chrono::{DateTime, Local};
use std::collections::HashSet;
use uuid::Uuid;

// Keep the module entry points on the same implementation as the root exports.
pub use super::action_lifecycle::{prepare_close_action_subtree, prepare_reopen_action_subtree};

#[derive(Debug, Clone)]
pub struct ActionArchivePlan {
    pub active_actions: ActionList,
    pub completed_actions: ActionList,
    pub archived_count: usize,
}

/// The eligible root trees, preserving factual states, dates and parent links.
pub(crate) fn eligible_archive_ids(active: &[Action]) -> HashSet<Uuid> {
    active
        .iter()
        .filter(|a| a.parent_id.is_none())
        .flat_map(|root| {
            let ids = collect_subtree_ids(active, root.id);
            if active
                .iter()
                .any(|a| ids.contains(&a.id) && !a.state.is_terminal())
            {
                Vec::new()
            } else {
                ids
            }
        })
        .collect()
}

/// Partition whole fully closed root trees. Storage never invents a closure date.
/// The time argument is retained for source compatibility; it does not affect facts.
pub fn plan_action_archive(
    active: &[Action],
    existing_completed: &[Action],
    _archived_at: DateTime<Local>,
) -> ActionArchivePlan {
    let ids = eligible_archive_ids(active);
    let mut completed_actions = existing_completed.to_vec();
    completed_actions.extend(active.iter().filter(|a| ids.contains(&a.id)).cloned());
    ActionArchivePlan {
        active_actions: active
            .iter()
            .filter(|a| !ids.contains(&a.id))
            .cloned()
            .collect(),
        completed_actions,
        archived_count: ids.len(),
    }
}

#[derive(Debug, Clone)]
pub struct PreparedArchiveOutcome {
    pub archived_count: usize,
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
}
#[derive(Debug, Clone)]
pub struct PreparedCloseOutcome {
    pub action_id: Uuid,
    pub closed_count: usize,
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
    pub already_closed: bool,
}
#[derive(Debug, Clone)]
pub struct PreparedReopenOutcome {
    pub action_id: Uuid,
    pub reopened_count: usize,
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
    pub already_open: bool,
}

/// Actions-only preparation for consumers without sidecar evidence. Native
/// storage uses `prepare_action_move`, which also moves the sidecar entries.
pub fn prepare_action_archive(
    active: ActionResourceState,
    completed: ActionResourceState,
    now: DateTime<Local>,
) -> Result<(EffectBatch, PreparedArchiveOutcome), ActionPrepareError> {
    let plan = plan_action_archive(&active.actions, &completed.actions, now);
    if completed
        .actions
        .iter()
        .any(|a| active.actions.iter().any(|source| source.id == a.id))
    {
        return Err(ActionPrepareError::Domain(
            "completed history already contains an active action".into(),
        ));
    }
    let mut effects = Vec::new();
    if plan.archived_count > 0 {
        // Destination before subtraction: interrupted delivery leaves a duplicate,
        // never a hole. The native planner uses the same ordering with sidecars.
        for (path, actions) in [
            (&completed.path, &plan.completed_actions),
            (&active.path, &plan.active_actions),
        ] {
            effects.push(Effect::Write {
                path: ResourceLocation::workspace(path.clone()),
                bytes: format(actions, OutputFormat::Actions, None, None)
                    .map_err(ActionPrepareError::Domain)?
                    .into_bytes(),
            });
        }
    }
    let preconditions = vec![
        ResourcePrecondition {
            path: ResourceLocation::workspace(active.path.clone()),
            expected: active.expected,
        },
        ResourcePrecondition {
            path: ResourceLocation::workspace(completed.path.clone()),
            expected: completed.expected,
        },
    ];
    let batch = EffectBatch::new(effects, preconditions)
        .map_err(|e| ActionPrepareError::Domain(e.to_string()))?;
    Ok((
        batch,
        PreparedArchiveOutcome {
            archived_count: plan.archived_count,
            source_path: active.path,
            completed_path: completed.path,
        },
    ))
}

#[cfg(feature = "formatting")]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::resource::ExpectedResource;
    use crate::{ActionSelector, ActionState};
    fn action(name: &str, state: ActionState, parent: Option<Uuid>) -> Action {
        Action {
            state,
            parent_id: parent,
            ..Action::new(name)
        }
    }
    fn resource(name: &str, actions: Vec<Action>) -> ActionResourceState {
        ActionResourceState {
            path: WorkspacePath::new(format!("charters/{name}")).unwrap(),
            actions,
            expected: ExpectedResource::Missing,
        }
    }
    fn written(batch: &EffectBatch, index: usize) -> Vec<Action> {
        match &batch.effects()[index] {
            Effect::Write { bytes, .. } => {
                crate::parse_actions(std::str::from_utf8(bytes).unwrap()).unwrap()
            }
            _ => panic!("expected a write"),
        }
    }
    #[test]
    fn plan_archives_complete_terminal_trees_and_preserves_existing_history() {
        let root = action("Root", ActionState::Completed, None);
        let child = action("Child", ActionState::Cancelled, Some(root.id));
        let open = action("Open", ActionState::NotStarted, None);
        let old = action("Old", ActionState::Completed, None);
        let plan = plan_action_archive(
            &[root.clone(), child.clone(), open.clone()],
            std::slice::from_ref(&old),
            Local::now(),
        );
        assert_eq!(plan.archived_count, 2);
        assert_eq!(plan.active_actions, vec![open]);
        assert_eq!(plan.completed_actions, vec![old, root, child]);
    }
    #[test]
    fn plan_keeps_terminal_root_when_a_descendant_is_open() {
        let root = action("Root", ActionState::Completed, None);
        let child = action("Child", ActionState::NotStarted, Some(root.id));
        let original = vec![root, child];
        let plan = plan_action_archive(&original, &[], Local::now());
        assert_eq!(plan.archived_count, 0);
        assert_eq!(plan.active_actions, original);
    }
    #[test]
    fn close_preparation_writes_only_active_and_preserves_descendants() {
        let root = action("Root", ActionState::NotStarted, None);
        let child = action("Child", ActionState::NotStarted, Some(root.id));
        let (batch, outcome) = prepare_close_action_subtree(
            resource("work.actions", vec![root.clone(), child.clone()]),
            resource("work.completed.actions", vec![]),
            &ActionSelector::from(&root),
            ActionState::Completed,
            Local::now(),
        )
        .unwrap();
        assert_eq!(outcome.closed_count, 1);
        assert_eq!(batch.effects().len(), 1);
        assert_eq!(batch.preconditions().len(), 2);
        let after = written(&batch, 0);
        assert_eq!(after.len(), 2);
        assert_eq!(after[0].state, ActionState::Completed);
        assert!(after[0].completed_at.is_some());
        assert_eq!(after[1], child);
    }
    #[test]
    fn archive_emits_completed_write_before_active_write() {
        let root = action("Root", ActionState::Completed, None);
        let (batch, _) = prepare_action_archive(
            resource("work.actions", vec![root.clone()]),
            resource("work.completed.actions", vec![]),
            Local::now(),
        )
        .unwrap();
        assert!(
            matches!(&batch.effects()[0], Effect::Write { path, .. } if path.path.as_str() == "charters/work.completed.actions")
        );
        assert_eq!(written(&batch, 0), vec![root]);
        assert!(written(&batch, 1).is_empty());
    }
    #[test]
    fn reopen_writes_only_active_clears_target_date_and_preserves_children() {
        let mut root = action("Root", ActionState::Completed, None);
        root.completed_at = Some(crate::domain::time::Bound::minute(Local::now()));
        let child = action("Child", ActionState::Cancelled, Some(root.id));
        let (batch, outcome) = prepare_reopen_action_subtree(
            resource("work.actions", vec![root.clone(), child.clone()]),
            resource("work.completed.actions", vec![]),
            &ActionSelector::from(&root),
            Local::now(),
        )
        .unwrap();
        assert_eq!(outcome.reopened_count, 1);
        assert_eq!(batch.effects().len(), 1);
        assert_eq!(batch.preconditions().len(), 2);
        let after = written(&batch, 0);
        assert_eq!(after[0].state, ActionState::NotStarted);
        assert!(after[0].completed_at.is_none());
        assert_eq!(after[1], child);
    }
    #[test]
    fn no_op_archive_retains_read_set_preconditions_without_effects() {
        let (batch, outcome) = prepare_action_archive(
            resource(
                "work.actions",
                vec![action("Open", ActionState::NotStarted, None)],
            ),
            resource("work.completed.actions", vec![]),
            Local::now(),
        )
        .unwrap();
        assert_eq!(outcome.archived_count, 0);
        assert!(batch.effects().is_empty());
        assert_eq!(batch.preconditions().len(), 2);
    }
    #[test]
    fn reopen_refuses_archived_subtree_and_names_root() {
        let root = action("Root", ActionState::Completed, None);
        let child = action("Child", ActionState::Cancelled, Some(root.id));
        let error = prepare_reopen_action_subtree(
            resource("work.actions", vec![]),
            resource("work.completed.actions", vec![root.clone(), child.clone()]),
            &ActionSelector::from(&child),
            Local::now(),
        )
        .unwrap_err()
        .to_string();
        assert!(error.contains("unarchive"));
        assert!(error.contains(&root.id.to_string()));
    }
    #[test]
    fn reopen_of_an_already_open_action_is_rejected() {
        let root = action("Root", ActionState::NotStarted, None);
        let error = prepare_reopen_action_subtree(
            resource("work.actions", vec![root.clone()]),
            resource("work.completed.actions", vec![]),
            &ActionSelector::from(&root),
            Local::now(),
        )
        .unwrap_err()
        .to_string();
        assert!(error.contains("already open"));
    }
}
