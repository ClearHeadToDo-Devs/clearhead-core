use chrono::Local;
use clearhead_core::workspace::WorkspaceError;
use clearhead_core::workspace::durability::WorkspaceLock;
use clearhead_core::{ActionSelector, ActionState, completed_actions_path, read_actions};
use clearhead_workspace_fs::{archive_actions, close_action_subtree};
use uuid::Uuid;

fn setup(body: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let temp = tempfile::tempdir().unwrap();
    let charters = temp.path().join("charters");
    std::fs::create_dir_all(&charters).unwrap();
    let source = charters.join("work.actions");
    std::fs::write(&source, body).unwrap();
    (temp, source)
}

fn selector(id: Uuid, name: &str) -> ActionSelector {
    ActionSelector {
        id,
        alias: None,
        name: name.into(),
    }
}

#[test]
fn archive_updates_active_and_completed_in_one_batch() {
    let (temp, source) = setup("[x] Newer #019f733d-45b2-7f21-bcad-5610887b7230\n[ ] Open\n");
    let completed = completed_actions_path(&source);
    std::fs::write(
        &completed,
        "[x] Older #019f733d-45c2-7dd2-91dc-8631f33c6b77\n",
    )
    .unwrap();
    let result = archive_actions(temp.path(), &source).unwrap();
    assert_eq!(result.archived_count, 1);
    assert!(!std::fs::read_to_string(&source).unwrap().contains("Newer"));
    let history = std::fs::read_to_string(&completed).unwrap();
    assert!(history.contains("Older") && history.contains("Newer"));
}

#[test]
fn archive_recovers_before_planning_without_duplicate_append() {
    let (temp, source) = setup("[x] Done #019f733d-45b2-7f21-bcad-5610887b7230\n");
    let charters = source.parent().unwrap();
    let completed = completed_actions_path(&source);
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
    assert_eq!(
        archive_actions(temp.path(), &source)
            .unwrap()
            .archived_count,
        0
    );
    assert_eq!(read_actions(&completed).unwrap().len(), 1);
}

#[test]
fn close_moves_selected_subtree_and_preserves_other_actions() {
    let id: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
    let (temp, source) = setup(
        "[ ] Selected #019f733d-45b2-7f21-bcad-5610887b7230\n> [ ] Child #019f733d-45c2-7dd2-91dc-8631f33c6b77\n[ ] Other #019f733d-45d2-7dd2-91dc-8631f33c6b77\n",
    );
    let result = close_action_subtree(
        temp.path(),
        &source,
        &selector(id, "Selected"),
        ActionState::Completed,
        Local::now(),
    )
    .unwrap();
    assert_eq!(result.closed_count, 2);
    assert!(std::fs::read_to_string(&source).unwrap().contains("Other"));
    assert_eq!(read_actions(&result.completed_path).unwrap().len(), 2);
}

#[test]
fn close_recovers_completed_move_as_already_closed() {
    let id: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
    let (temp, source) = setup(&format!("[ ] Done #{id}\n"));
    let charters = source.parent().unwrap();
    let completed = completed_actions_path(&source);
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
    assert_eq!(read_actions(&completed).unwrap().len(), 1);
}

#[test]
fn close_reidentifies_idless_action_by_unique_name() {
    let (temp, source) = setup("[ ] Unique task\n");
    let selected = ActionSelector::from(&read_actions(&source).unwrap()[0]);
    let result = close_action_subtree(
        temp.path(),
        &source,
        &selected,
        ActionState::Completed,
        Local::now(),
    )
    .unwrap();
    assert_eq!(result.closed_count, 1);
}

#[test]
fn close_and_archive_refuse_lock_contention() {
    let id: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
    let (temp, source) = setup(&format!("[ ] Open #{id}\n"));
    let _lock = WorkspaceLock::try_acquire(temp.path()).unwrap().unwrap();
    assert!(matches!(
        close_action_subtree(
            temp.path(),
            &source,
            &selector(id, "Open"),
            ActionState::Cancelled,
            Local::now()
        ),
        Err(WorkspaceError::WorkspaceLocked(_))
    ));
    drop(_lock);
    std::fs::write(&source, "[x] Done\n").unwrap();
    let _lock = WorkspaceLock::try_acquire(temp.path()).unwrap().unwrap();
    assert!(matches!(
        archive_actions(temp.path(), &source),
        Err(WorkspaceError::WorkspaceLocked(_))
    ));
}

#[test]
fn source_path_validation_rejects_completed_and_outside_paths() {
    let (temp, source) = setup("");
    let completed = completed_actions_path(&source);
    std::fs::write(&completed, "").unwrap();
    assert!(matches!(
        archive_actions(temp.path(), &completed),
        Err(WorkspaceError::InvalidPath(_))
    ));
    let outside = temp.path().join("outside.actions");
    std::fs::write(&outside, "").unwrap();
    assert!(matches!(
        archive_actions(temp.path(), &outside),
        Err(WorkspaceError::InvalidPath(_))
    ));
}
