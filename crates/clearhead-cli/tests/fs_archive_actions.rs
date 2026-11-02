use clearhead_cli::filesystem::read_actions;
use clearhead_cli::filesystem::{archive_actions, transition_action};
use clearhead_core::workspace::WorkspaceError;
use clearhead_core::{ActionSelector, ActionState, completed_actions_path};
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
fn close_changes_selected_line_and_preserves_other_actions() {
    let id: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
    let (temp, source) = setup(
        "[ ] Selected #019f733d-45b2-7f21-bcad-5610887b7230\n> [ ] Child #019f733d-45c2-7dd2-91dc-8631f33c6b77\n[ ] Other #019f733d-45d2-7dd2-91dc-8631f33c6b77\n",
    );
    let result = transition_action(
        temp.path(),
        &source,
        &selector(id, "Selected"),
        ActionState::Completed,
        false,
        false,
    )
    .unwrap();
    assert!(matches!(
        result,
        clearhead_core::VerbOutcome::Completed { .. }
    ));
    let actions = read_actions(&source).unwrap();
    assert_eq!(actions.len(), 3);
    assert_eq!(actions[0].state, ActionState::Completed);
    assert!(actions[0].completed_at.is_some());
    assert_eq!(actions[1].state, ActionState::NotStarted);
    assert_eq!(actions[1].parent_id, Some(id));
    assert_eq!(actions[2].name, "Other");
    assert!(!completed_actions_path(&source).exists());
}

#[test]
fn close_reports_already_closed_when_action_is_in_completed_history() {
    let id: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
    let (temp, source) = setup("");
    let completed = completed_actions_path(&source);
    std::fs::write(&completed, format!("[x] Done #{id}\n")).unwrap();
    let error = transition_action(
        temp.path(),
        &source,
        &selector(id, "Done"),
        ActionState::Completed,
        false,
        false,
    )
    .unwrap_err();
    assert!(error.to_string().contains("already closed"));
    assert_eq!(read_actions(&completed).unwrap().len(), 1);
}

#[test]
fn close_reidentifies_idless_action_by_unique_name() {
    let (temp, source) = setup("[ ] Unique task\n");
    let selected = ActionSelector::from(&read_actions(&source).unwrap()[0]);
    let result = transition_action(
        temp.path(),
        &source,
        &selected,
        ActionState::Completed,
        false,
        false,
    )
    .unwrap();
    assert!(matches!(
        result,
        clearhead_core::VerbOutcome::Completed { .. }
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
