use clearhead_core::domain::update::ActionUpdate;
use clearhead_core::workspace::sidecar::{ActionMeta, CharterMetadata, read_sidecar, sidecar_path};
use clearhead_core::{Action, ActionSelector, ActionState, read_actions};
use clearhead_workspace_fs::sidecar::write_sidecar;
use clearhead_workspace_fs::{delete_action, insert_action, update_action};
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

fn plant(source: &std::path::Path, content: &str) {
    let charters = source.parent().unwrap();
    let tmp = charters.join(".tmp.recover");
    std::fs::write(&tmp, content).unwrap();
    std::fs::write(
        charters.join(".pending"),
        format!("{}\t{}\n", tmp.display(), source.display()),
    )
    .unwrap();
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

#[test]
fn insert_recovers_before_preparing() {
    let (temp, source) = workspace();
    std::fs::write(
        &source,
        "[ ] Existing #019f733d-4600-7000-8000-000000000001\n",
    )
    .unwrap();
    plant(
        &source,
        "[ ] Existing #019f733d-4600-7000-8000-000000000001\n[ ] X #019f733d-4600-7000-8000-000000000002\n",
    );
    insert_action(
        temp.path(),
        &source,
        Action {
            id: Uuid::new_v4(),
            name: "Y".into(),
            state: ActionState::NotStarted,
            ..Default::default()
        },
        None,
    )
    .unwrap();
    assert_eq!(
        read_actions(&source)
            .unwrap()
            .iter()
            .map(|a| a.name.as_str())
            .collect::<Vec<_>>(),
        ["Existing", "X", "Y"]
    );
}

#[test]
fn update_recovers_before_preparing() {
    let (temp, source) = workspace();
    let id: Uuid = "019f733d-4600-7000-8000-000000000001".parse().unwrap();
    std::fs::write(&source, format!("[ ] Task #{id}\n")).unwrap();
    plant(&source, &format!("[ ] Renamed #{id}\n"));
    update_action(
        temp.path(),
        &source,
        &selector(id, "Task"),
        ActionUpdate {
            priority: Some(2),
            ..Default::default()
        },
    )
    .unwrap();
    let actions = read_actions(&source).unwrap();
    assert_eq!(actions[0].name, "Renamed");
    assert_eq!(actions[0].priority, Some(2));
}

#[test]
fn delete_recovers_before_preparing() {
    let (temp, source) = workspace();
    let a: Uuid = "019f733d-4600-7000-8000-000000000001".parse().unwrap();
    let b: Uuid = "019f733d-4600-7000-8000-000000000002".parse().unwrap();
    std::fs::write(&source, format!("[ ] A #{a}\n[ ] B #{b}\n")).unwrap();
    plant(&source, &format!("[ ] B #{b}\n"));
    delete_action(temp.path(), &source, &selector(b, "B")).unwrap();
    assert!(read_actions(&source).unwrap().is_empty());
}
