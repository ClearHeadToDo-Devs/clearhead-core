use clearhead_cli::filesystem::delete_action;
use clearhead_cli::filesystem::read_actions;
use clearhead_cli::filesystem::sidecar::read_sidecar;
use clearhead_cli::filesystem::sidecar::write_sidecar;
use clearhead_core::ActionSelector;
use clearhead_core::workspace::sidecar::{ActionMeta, CharterMetadata, sidecar_path};
use uuid::Uuid;

fn selector(id: Uuid, name: &str) -> ActionSelector {
    ActionSelector {
        id,
        alias: None,
        name: name.into(),
    }
}

fn workspace() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp = tempfile::tempdir().unwrap();
    let charters = temp.path().join("charters");
    std::fs::create_dir_all(&charters).unwrap();
    (temp, charters.join("work.actions"))
}

#[test]
fn delete_removes_active_subtree_and_prunes_sidecar_in_same_batch() {
    let (temp, source) = workspace();
    let parent: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
    let child: Uuid = "019f733d-45c2-7dd2-91dc-8631f33c6b77".parse().unwrap();
    std::fs::write(
        &source,
        format!("[ ] Parent #{parent}\n    >[ ] Child #{child}\n"),
    )
    .unwrap();
    let mut metadata = CharterMetadata::default();
    for id in [parent, child] {
        metadata.actions.insert(
            id.to_string(),
            ActionMeta {
                created: Some(chrono::Local::now()),
                plan: None,
                occurrence: None,
            },
        );
    }
    write_sidecar(&sidecar_path(&source), &metadata).unwrap();

    let result = delete_action(temp.path(), &source, &selector(parent, "Parent")).unwrap();

    assert_eq!(result.deleted_count, 2);
    assert!(!result.from_completed);
    assert!(read_actions(&source).unwrap().is_empty());
    assert!(
        read_sidecar(&sidecar_path(&source))
            .unwrap()
            .actions
            .is_empty()
    );
}

#[test]
fn delete_reaches_completed_subtree_without_rewriting_active() {
    let (temp, source) = workspace();
    let completed = source.parent().unwrap().join("work.completed.actions");
    let done: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
    std::fs::write(&source, "[ ] Live #019f733d-4600-7000-8000-000000000001\n").unwrap();
    let active_before = std::fs::read(&source).unwrap();
    std::fs::write(&completed, format!("[x] Done #{done}\n")).unwrap();

    let result = delete_action(temp.path(), &source, &selector(done, "Done")).unwrap();

    assert!(result.from_completed);
    assert!(read_actions(&completed).unwrap().is_empty());
    assert_eq!(std::fs::read(&source).unwrap(), active_before);
}
