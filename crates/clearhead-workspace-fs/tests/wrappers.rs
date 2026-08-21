use clearhead_core::workspace::CharterMetadata;
use clearhead_core::{Action, ActionState};

#[test]
fn action_file_roundtrip_uses_core_codec_through_native_wrapper() {
    let root = tempfile::tempdir().unwrap();
    let path = root.path().join("work.actions");
    let action = Action {
        name: "Native wrapper".into(),
        state: ActionState::NotStarted,
        ..Default::default()
    };
    clearhead_workspace_fs::write_actions(std::slice::from_ref(&action), &path).unwrap();
    assert_eq!(
        clearhead_workspace_fs::read_actions(&path).unwrap(),
        vec![action]
    );
}

#[test]
fn template_resolution_preserves_local_then_global_precedence() {
    let root = tempfile::tempdir().unwrap();
    let charter = root.path().join("charters/work");
    let data = root.path().join("data");
    std::fs::create_dir_all(charter.join("templates")).unwrap();
    std::fs::create_dir_all(data.join("templates")).unwrap();
    let local = charter.join("templates/review.actions");
    std::fs::write(&local, "[ ] Local").unwrap();
    std::fs::write(data.join("templates/review.actions"), "[ ] Global").unwrap();

    assert_eq!(
        clearhead_workspace_fs::templates::resolve_template(&charter, &data, "review").unwrap(),
        Some(local)
    );
}

#[test]
fn sidecar_wrapper_applies_core_stamping_policy() {
    let root = tempfile::tempdir().unwrap();
    let actions_path = root.path().join("work.actions");
    let action = Action {
        name: "Stamped".into(),
        ..Default::default()
    };
    clearhead_workspace_fs::sidecar::stamp_sidecar_entries(
        &actions_path,
        std::slice::from_ref(&action),
    )
    .unwrap();
    let path = clearhead_core::workspace::sidecar_path(&actions_path);
    let metadata: CharterMetadata = clearhead_workspace_fs::sidecar::read_sidecar(&path).unwrap();
    assert!(metadata.actions.contains_key(&action.id.to_string()));
}
