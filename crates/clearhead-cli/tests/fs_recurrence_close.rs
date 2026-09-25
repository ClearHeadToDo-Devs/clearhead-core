use std::collections::HashMap;
use std::fs;
use std::path::Path;

use clearhead_cli::filesystem::read_actions;
use clearhead_cli::filesystem::{
    close_action_subtree, load_domain_model, resolve_materialized_occurrence, sync_calendar,
};
use clearhead_core::workspace::calendar::reconcile::occurrence_links;
use clearhead_core::{ActionSelector, OccurrenceOp, completed_actions_path};
use uuid::Uuid;

/// Live occurrence links as the loaded workspace sees them.
fn live_links(root: &Path) -> HashMap<Uuid, (Uuid, String)> {
    let model = load_domain_model(root).unwrap();
    occurrence_links(model.all_actions())
}

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
    let actions_path = root.join(".clearhead/charters/health.actions");
    let now = chrono::Local::now();
    sync_calendar(root, None).unwrap();
    let links = live_links(root);
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
    resolve_materialized_occurrence(root, occ_id, &OccurrenceOp::Complete { at: now }, now)
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
    let live: serde_json::Value = serde_json::from_str(
        &fs::read_to_string(actions_path.with_file_name(".health.json")).unwrap(),
    )
    .unwrap();
    assert!(
        live["actions"][occ_id.to_string()]["plan"].is_null(),
        "resolution replaces the live link with the frozen snapshot"
    );
}

#[test]
fn resolving_a_materialized_occurrence_writes_the_deviation_and_advances() {
    let ws = recurring_plan_workspace();
    let root = ws.path();
    let plans_root = root.join(".clearhead/plans");
    let now = chrono::Local::now();
    sync_calendar(root, None).unwrap();

    let links = live_links(root);
    let (&occurrence_id, (plan_id, resolved_slot)) = links.iter().next().unwrap();
    let (plan_id, resolved_slot) = (*plan_id, resolved_slot.clone());
    assert!(
        resolve_materialized_occurrence(
            root,
            occurrence_id,
            &OccurrenceOp::Complete { at: now },
            now,
        )
        .unwrap()
    );

    let advanced_links = live_links(root);
    assert!(!advanced_links.contains_key(&occurrence_id));
    assert_eq!(advanced_links.len(), 1);
    let (&next_id, (next_plan, _)) = advanced_links.iter().next().unwrap();
    assert_ne!(next_id, occurrence_id);
    assert_eq!(*next_plan, plan_id);
    let content = fs::read_to_string(plans_root.join("health/run.ics")).unwrap();
    assert!(content.contains("RECURRENCE-ID"));
    assert!(content.contains(&resolved_slot));
}

#[test]
fn materialized_occurrence_link_lives_in_the_sidecar_and_survives_store_loss() {
    let ws = recurring_plan_workspace();
    let root = ws.path();
    sync_calendar(root, None).unwrap();

    let links = live_links(root);
    let (&occurrence_id, (plan_id, slot_key)) = links.iter().next().unwrap();
    let sidecar: serde_json::Value = serde_json::from_str(
        &fs::read_to_string(root.join(".clearhead/charters/.health.json")).unwrap(),
    )
    .unwrap();
    let link = &sidecar["actions"][occurrence_id.to_string()]["plan"];
    assert_eq!(link["uid"], "run@example.com");
    assert_eq!(link["occurrence_key"], slot_key.as_str());

    fs::remove_file(root.join(".clearhead/sync/plans.json")).unwrap();
    let token = load_domain_model(root)
        .unwrap()
        .all_actions()
        .into_iter()
        .find(|action| action.id == occurrence_id)
        .cloned()
        .unwrap();
    assert_eq!(token.plan_id, Some(*plan_id));
    assert_eq!(
        token.external_occurrence_key.as_deref(),
        Some(slot_key.as_str())
    );
}
