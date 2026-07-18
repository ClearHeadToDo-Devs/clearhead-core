//! Durable action archival.
//!
//! Action archival is a two-file mutation: terminal trees leave the active
//! `.actions` file and are appended to its sibling `.completed.actions` file.
//! Both projections are staged in one journaled batch so an interrupted write
//! can only recover forward to the complete post-archive state.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use chrono::{DateTime, Local};
use uuid::Uuid;

use crate::domain::{close_subtree, collect_subtree_ids};
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

/// Stable-enough handoff from a client-side resolution to core's locked read.
/// Inline UUID is preferred; alias/name let legacy id-less lines survive the
/// second parse without making the binary own the filesystem mutation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CloseActionSelector {
    pub id: Uuid,
    pub alias: Option<String>,
    pub name: String,
}

impl From<&Action> for CloseActionSelector {
    fn from(action: &Action) -> Self {
        Self {
            id: action.id,
            alias: action.alias.clone(),
            name: action.name.clone(),
        }
    }
}

/// Result of durably closing one selected action subtree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CloseActionResult {
    pub action_id: Uuid,
    pub closed_count: usize,
    pub source_path: PathBuf,
    pub completed_path: PathBuf,
    /// True when recovery completed this exact move before it could be planned
    /// again. This is successful convergence, not a duplicate append.
    pub already_closed: bool,
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
    validate_source_path(source_path, &layout.charter_root)?;

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

/// Close one selected subtree as a single locked, journaled core mutation.
///
/// Recovery runs after acquiring the workspace lock and before either file is
/// read. The reduced active file and full completed file are then staged in one
/// batch, so a crash cannot lose or duplicate the subtree.
pub fn close_action_subtree(
    workspace_root: &Path,
    source_path: &Path,
    selector: &CloseActionSelector,
    closing_state: ActionState,
    completed_at: DateTime<Local>,
) -> Result<CloseActionResult, WorkspaceError> {
    if !matches!(
        closing_state,
        ActionState::Completed | ActionState::Cancelled
    ) {
        return Err(WorkspaceError::Actions(
            "an action subtree can only be closed as Completed or Cancelled".to_string(),
        ));
    }

    let layout = resolve_workspace_layout(workspace_root);
    validate_source_path(source_path, &layout.charter_root)?;
    std::fs::create_dir_all(&layout.charter_root)?;
    let _lock = WorkspaceLock::try_acquire(&layout.data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(layout.data_root.clone()))?;
    recover_pending(&layout.charter_root)?;

    let completed_path = completed_actions_path(source_path);
    let mut active = read_actions(source_path)?;
    let mut completed = read_actions(&completed_path)?;

    let action_id = active
        .iter()
        .find(|action| action.id == selector.id)
        .or_else(|| {
            selector.alias.as_ref().and_then(|alias| {
                active
                    .iter()
                    .find(|action| action.alias.as_deref() == Some(alias.as_str()))
            })
        })
        .or_else(|| active.iter().find(|action| action.name == selector.name))
        .map(|action| action.id);

    let Some(action_id) = action_id else {
        if completed.iter().any(|action| {
            action.id == selector.id
                || selector
                    .alias
                    .as_ref()
                    .is_some_and(|alias| action.alias.as_deref() == Some(alias.as_str()))
                || action.name == selector.name
        }) {
            return Ok(CloseActionResult {
                action_id: selector.id,
                closed_count: 0,
                source_path: source_path.to_path_buf(),
                completed_path,
                already_closed: true,
            });
        }
        return Err(WorkspaceError::Actions(format!(
            "open action not found in source file: {}",
            selector.id
        )));
    };

    let target = active
        .iter()
        .find(|action| action.id == action_id)
        .expect("selected action came from active list");
    if matches!(
        target.state,
        ActionState::Completed | ActionState::Cancelled
    ) {
        return Err(WorkspaceError::Actions(format!(
            "action is already terminal in the active file: {action_id}"
        )));
    }

    let subtree_ids = collect_subtree_ids(&active, action_id);
    if completed
        .iter()
        .any(|action| subtree_ids.contains(&action.id))
    {
        return Err(WorkspaceError::Actions(format!(
            "completed history already contains part of subtree: {action_id}"
        )));
    }

    let mut closed = close_subtree(&active, action_id, closing_state, completed_at);
    active.retain(|action| !subtree_ids.contains(&action.id));
    let closed_count = closed.len();
    completed.append(&mut closed);

    let active_text = render(&active)?;
    let completed_text = render(&completed)?;
    let mut batch = PendingBatch::new(layout.charter_root);
    batch.stage(source_path.to_path_buf(), active_text.as_bytes())?;
    batch.stage(completed_path.clone(), completed_text.as_bytes())?;
    batch.commit()?;

    Ok(CloseActionResult {
        action_id,
        closed_count,
        source_path: source_path.to_path_buf(),
        completed_path,
        already_closed: false,
    })
}

fn validate_source_path(source_path: &Path, charter_root: &Path) -> Result<(), WorkspaceError> {
    if !source_path.starts_with(charter_root)
        || source_path
            .file_name()
            .and_then(|name| name.to_str())
            .is_none_or(|name| !name.ends_with(".actions") || name.ends_with(".completed.actions"))
    {
        return Err(WorkspaceError::InvalidPath(source_path.to_path_buf()));
    }
    Ok(())
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

    fn selector(id: Uuid, name: &str) -> CloseActionSelector {
        CloseActionSelector {
            id,
            alias: None,
            name: name.to_string(),
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
    fn close_selected_subtree_updates_both_files_in_one_batch() {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join("charters");
        std::fs::create_dir_all(&charters).unwrap();
        let source = charters.join("work.actions");
        let root_id = Uuid::parse_str("019f733d-45b2-7f21-bcad-5610887b7230").unwrap();
        std::fs::write(
            &source,
            "[ ] Selected #019f733d-45b2-7f21-bcad-5610887b7230\n> [ ] Child #019f733d-45c2-7dd2-91dc-8631f33c6b77\n[ ] Other #019f733d-45d2-7dd2-91dc-8631f33c6b77\n",
        )
        .unwrap();

        let result = close_action_subtree(
            temp.path(),
            &source,
            &selector(root_id, "Selected"),
            ActionState::Completed,
            Local::now(),
        )
        .unwrap();

        assert_eq!(result.closed_count, 2);
        assert!(!result.already_closed);
        let active = std::fs::read_to_string(&source).unwrap();
        let completed = std::fs::read_to_string(&result.completed_path).unwrap();
        assert!(!active.contains("Selected"));
        assert!(!active.contains("Child"));
        assert!(active.contains("Other"));
        assert!(completed.contains("[x] Selected"));
        assert!(completed.contains("[x] Child"));
    }

    #[test]
    fn close_recovers_interrupted_move_without_duplicate_append() {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join("charters");
        std::fs::create_dir_all(&charters).unwrap();
        let source = charters.join("work.actions");
        let completed = completed_actions_path(&source);
        let id = Uuid::parse_str("019f733d-45b2-7f21-bcad-5610887b7230").unwrap();
        std::fs::write(&source, format!("[ ] Done #{id}\n")).unwrap();

        let source_tmp = charters.join(".tmp.source");
        let completed_tmp = charters.join(".tmp.completed");
        std::fs::write(&source_tmp, "").unwrap();
        std::fs::write(&completed_tmp, format!("[x] Done #{id}\n")).unwrap();
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

        let result = close_action_subtree(
            temp.path(),
            &source,
            &selector(id, "Done"),
            ActionState::Completed,
            Local::now(),
        )
        .unwrap();

        assert!(result.already_closed);
        assert_eq!(result.closed_count, 0);
        assert_eq!(read_actions(&completed).unwrap().len(), 1);
    }

    #[test]
    fn close_refuses_to_race_an_existing_writer() {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join("charters");
        std::fs::create_dir_all(&charters).unwrap();
        let source = charters.join("work.actions");
        let id = Uuid::parse_str("019f733d-45b2-7f21-bcad-5610887b7230").unwrap();
        std::fs::write(&source, format!("[ ] Open #{id}\n")).unwrap();
        let _lock = WorkspaceLock::try_acquire(temp.path()).unwrap().unwrap();

        let error = close_action_subtree(
            temp.path(),
            &source,
            &selector(id, "Open"),
            ActionState::Cancelled,
            Local::now(),
        )
        .unwrap_err();

        assert!(matches!(error, WorkspaceError::WorkspaceLocked(_)));
        assert!(std::fs::read_to_string(&source).unwrap().contains("Open"));
        assert!(!completed_actions_path(&source).exists());
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
