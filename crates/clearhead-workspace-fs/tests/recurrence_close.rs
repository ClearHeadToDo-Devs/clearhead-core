use std::fs;

use clearhead_core::{
    ActionSelector, OccurrenceOp, apply_sync, completed_actions_path, load_domain_model, plan_sync,
    read_actions, read_plans_sync_store, read_vtodo_actions, resolve_materialized_occurrence,
};
use clearhead_workspace_fs::close_action_subtree;

fn recurring_plan_workspace() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let charters = dir.path().join(".clearhead/charters");
    let plans = dir.path().join(".clearhead/plans/health");
    fs::create_dir_all(&charters).unwrap();
    fs::create_dir_all(&plans).unwrap();
    fs::write(charters.join("health.actions"), "").unwrap();
    fs::write(
        plans.join("run.ics"),
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:run@example.com\r\nSUMMARY:Run\r\nDTSTART:20260101T080000Z\r\nRRULE:FREQ=DAILY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    )
    .unwrap();
    dir
}

#[test]
fn closing_materialized_occurrence_preserves_completed_sidecar_lineage() {
    let ws = recurring_plan_workspace();
    let root = ws.path();
    let plans_root = root.join(".clearhead/plans");
    let actions_path = root.join(".clearhead/charters/health.actions");
    let now = chrono::Local::now();
    let model = load_domain_model(root).unwrap();
    let store = read_plans_sync_store(root, &plans_root).unwrap();
    let calendar = read_vtodo_actions(&plans_root).unwrap();
    apply_sync(root, None, &plan_sync(&model, &store, &calendar).unwrap()).unwrap();
    let links = read_plans_sync_store(root, &plans_root)
        .unwrap()
        .occurrence_links();
    let (&occ_id, (plan_id, slot_key)) = links.iter().next().unwrap();
    let plan_id = *plan_id;
    let slot_key = slot_key.clone();
    let token = read_actions(&actions_path)
        .unwrap()
        .into_iter()
        .find(|a| a.id == occ_id)
        .unwrap();

    close_action_subtree(
        root,
        &actions_path,
        &ActionSelector::from(&token),
        clearhead_core::ActionState::Completed,
        now,
    )
    .unwrap();
    resolve_materialized_occurrence(root, None, occ_id, &OccurrenceOp::Complete { at: now }, now)
        .unwrap();

    let completed_path = completed_actions_path(&actions_path);
    assert!(
        fs::read_to_string(&completed_path)
            .unwrap()
            .contains(&occ_id.to_string())
    );
    let sidecar: serde_json::Value = serde_json::from_str(
        &fs::read_to_string(completed_path.with_file_name(".health.completed.json")).unwrap(),
    )
    .unwrap();
    let occurrence = &sidecar["actions"][occ_id.to_string()]["occurrence"];
    assert_eq!(occurrence["plan_id"], plan_id.to_string());
    assert_eq!(occurrence["occurrence_key"], slot_key);
    assert_eq!(occurrence["plan_uid"], "run@example.com");
}
