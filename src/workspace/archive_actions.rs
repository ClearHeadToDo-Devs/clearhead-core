//! Durable action archival.
//!
//! Action archival is a two-file mutation: terminal trees leave the active
//! `.actions` file and are appended to its sibling `.completed.actions` file.
//! Both projections are staged in one journaled batch so an interrupted write
//! can only recover forward to the complete post-archive state.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::workspace::action_files::{completed_actions_path, read_actions};
use crate::workspace::actions::{Action, ActionList, ActionState, OutputFormat, format};
use crate::workspace::durability::{PendingBatch, WorkspaceLock, recover_pending};
use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};

/// Pure result of partitioning an active action file for archival.
#[derive(Debug, Clone)]
pub struct ActionArchivePlan {
    /// Actions that remain in the primary `.actions` file.
    pub active_actions: ActionList,
    /// Full post-operation contents of the `.completed.actions` file.
    pub completed_actions: ActionList,
    /// Number of actions moved by this plan.
    pub archived_count: usize,
}

/// Result of applying an [`ActionArchivePlan`] to a workspace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionArchiveResult {
    pub archived_count: usize,
    pub source_path: PathBuf,
    pub completed_path: PathBuf,
}

/// Build an action-archive plan without reading or writing the filesystem.
///
/// Only complete terminal trees are archived. A completed/cancelled root with
/// an open descendant remains active, preserving the structural parent chain.
/// Archived actions are detached because completed files are flat history, not
/// an active hierarchy.
pub fn plan_action_archive(active: &[Action], existing_completed: &[Action]) -> ActionArchivePlan {
    let archive_root_ids: HashSet<_> = active
        .iter()
        .filter(|action| action.parent_id.is_none() && is_terminal(action))
        .filter(|root| descendants(active, root.id).into_iter().all(is_terminal))
        .map(|action| action.id)
        .collect();

    let mut archive_ids = HashSet::new();
    for root_id in archive_root_ids {
        archive_ids.insert(root_id);
        let mut frontier = vec![root_id];
        while let Some(parent_id) = frontier.pop() {
            for child in active
                .iter()
                .filter(|action| action.parent_id == Some(parent_id))
            {
                if archive_ids.insert(child.id) {
                    frontier.push(child.id);
                }
            }
        }
    }

    let mut active_actions = Vec::new();
    let mut archived_actions = Vec::new();
    for action in active {
        if archive_ids.contains(&action.id) {
            let mut archived = action.clone();
            archived.parent_id = None;
            archived_actions.push(archived);
        } else {
            active_actions.push(action.clone());
        }
    }

    let archived_count = archived_actions.len();
    let mut completed_actions = existing_completed.to_vec();
    completed_actions.extend(archived_actions);

    ActionArchivePlan {
        active_actions,
        completed_actions,
        archived_count,
    }
}

/// Atomically archive terminal action trees from one workspace action file.
///
/// The workspace lock is mandatory for this read-plan-write operation. Any
/// journal left by an interrupted mutation is recovered before files are read,
/// then the active and completed projections are committed in one
/// [`PendingBatch`]. A zero-count plan performs no writes.
pub fn archive_actions(
    workspace_root: &Path,
    source_path: &Path,
) -> Result<ActionArchiveResult, WorkspaceError> {
    let layout = resolve_workspace_layout(workspace_root);
    if !source_path.starts_with(&layout.charter_root)
        || source_path
            .file_name()
            .and_then(|name| name.to_str())
            .is_none_or(|name| !name.ends_with(".actions") || name.ends_with(".completed.actions"))
    {
        return Err(WorkspaceError::InvalidPath(source_path.to_path_buf()));
    }

    std::fs::create_dir_all(&layout.charter_root)?;
    let _lock = WorkspaceLock::try_acquire(&layout.data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(layout.data_root.clone()))?;

    recover_pending(&layout.charter_root)?;

    let completed_path = completed_actions_path(source_path);
    let active = read_actions(source_path)?;
    let completed = read_actions(&completed_path)?;
    let plan = plan_action_archive(&active, &completed);

    if plan.archived_count > 0 {
        let active_text = render(&plan.active_actions)?;
        let completed_text = render(&plan.completed_actions)?;
        let mut batch = PendingBatch::new(layout.charter_root);
        batch.stage(source_path.to_path_buf(), active_text.as_bytes())?;
        batch.stage(completed_path.clone(), completed_text.as_bytes())?;
        batch.commit()?;
    }

    Ok(ActionArchiveResult {
        archived_count: plan.archived_count,
        source_path: source_path.to_path_buf(),
        completed_path,
    })
}

fn render(actions: &[Action]) -> Result<String, WorkspaceError> {
    format(&actions.to_vec(), OutputFormat::Actions, None, None).map_err(WorkspaceError::Actions)
}

fn is_terminal(action: &Action) -> bool {
    matches!(
        action.state,
        ActionState::Completed | ActionState::Cancelled
    )
}

fn descendants(actions: &[Action], root_id: uuid::Uuid) -> Vec<&Action> {
    let mut descendants = Vec::new();
    let mut seen = HashSet::from([root_id]);
    let mut frontier = vec![root_id];
    while let Some(parent_id) = frontier.pop() {
        for child in actions
            .iter()
            .filter(|action| action.parent_id == Some(parent_id))
        {
            if seen.insert(child.id) {
                descendants.push(child);
                frontier.push(child.id);
            }
        }
    }
    descendants
}

#[cfg(test)]
mod tests {
    use super::*;
    use uuid::Uuid;

    fn action(name: &str, state: ActionState, parent_id: Option<Uuid>) -> Action {
        Action {
            name: name.to_string(),
            state,
            parent_id,
            ..Default::default()
        }
    }

    #[test]
    fn plan_archives_complete_terminal_trees_and_preserves_existing_history() {
        let root = action("done root", ActionState::Completed, None);
        let child = action("cancelled child", ActionState::Cancelled, Some(root.id));
        let open = action("still open", ActionState::NotStarted, None);
        let existing = action("older", ActionState::Completed, None);

        let plan = plan_action_archive(
            &[root.clone(), child.clone(), open.clone()],
            std::slice::from_ref(&existing),
        );

        assert_eq!(plan.archived_count, 2);
        assert_eq!(plan.active_actions.len(), 1);
        assert_eq!(plan.active_actions[0].id, open.id);
        assert_eq!(plan.completed_actions.len(), 3);
        assert_eq!(plan.completed_actions[0].id, existing.id);
        assert!(
            plan.completed_actions[1..]
                .iter()
                .all(|action| action.parent_id.is_none())
        );
    }

    #[test]
    fn plan_keeps_terminal_root_when_a_descendant_is_open() {
        let root = action("done root", ActionState::Completed, None);
        let child = action("open child", ActionState::NotStarted, Some(root.id));

        let plan = plan_action_archive(&[root, child], &[]);

        assert_eq!(plan.archived_count, 0);
        assert_eq!(plan.active_actions.len(), 2);
    }

    #[test]
    fn durable_archive_updates_active_and_completed_files() {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join("charters");
        std::fs::create_dir_all(&charters).unwrap();
        let source = charters.join("work.actions");
        let completed = charters.join("work.completed.actions");
        std::fs::write(
            &source,
            "[x] Newer #019f733d-45b2-7f21-bcad-5610887b7230\n[ ] Open\n",
        )
        .unwrap();
        std::fs::write(
            &completed,
            "[x] Older #019f733d-45c2-7dd2-91dc-8631f33c6b77\n",
        )
        .unwrap();

        let result = archive_actions(temp.path(), &source).unwrap();

        assert_eq!(result.archived_count, 1);
        let active_text = std::fs::read_to_string(&source).unwrap();
        let completed_text = std::fs::read_to_string(&completed).unwrap();
        assert!(!active_text.contains("Newer"));
        assert!(active_text.contains("Open"));
        assert!(completed_text.contains("Older"));
        assert!(completed_text.contains("Newer"));
        assert!(!charters.join(".pending").exists());
    }

    #[test]
    fn archive_recovers_an_interrupted_batch_before_planning() {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join("charters");
        std::fs::create_dir_all(&charters).unwrap();
        let source = charters.join("work.actions");
        let completed = charters.join("work.completed.actions");
        std::fs::write(&source, "[x] Done #019f733d-45b2-7f21-bcad-5610887b7230\n").unwrap();

        let source_tmp = charters.join(".tmp.source");
        let completed_tmp = charters.join(".tmp.completed");
        std::fs::write(&source_tmp, "").unwrap();
        std::fs::write(
            &completed_tmp,
            "[x] Done #019f733d-45b2-7f21-bcad-5610887b7230\n",
        )
        .unwrap();
        std::fs::write(
            charters.join(".pending"),
            format!(
                "{}\t{}\n{}\t{}\n",
                source_tmp.display(),
                source.display(),
                completed_tmp.display(),
                completed.display()
            ),
        )
        .unwrap();

        let result = archive_actions(temp.path(), &source).unwrap();

        assert_eq!(
            result.archived_count, 0,
            "recovered action must not be appended twice"
        );
        assert_eq!(std::fs::read_to_string(&source).unwrap(), "");
        assert_eq!(read_actions(&completed).unwrap().len(), 1);
        assert!(!charters.join(".pending").exists());
    }

    #[test]
    fn archive_refuses_to_race_an_existing_writer() {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join("charters");
        std::fs::create_dir_all(&charters).unwrap();
        let source = charters.join("work.actions");
        std::fs::write(&source, "[x] Done\n").unwrap();
        let _lock = WorkspaceLock::try_acquire(temp.path()).unwrap().unwrap();

        let error = archive_actions(temp.path(), &source).unwrap_err();

        assert!(matches!(error, WorkspaceError::WorkspaceLocked(_)));
        assert!(std::fs::read_to_string(&source).unwrap().contains("Done"));
        assert!(!completed_actions_path(&source).exists());
    }
}
