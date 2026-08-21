use clearhead_core::{load_workspace, render_occurrences};
use std::fs;
use std::path::Path;
use tempfile::TempDir;

fn render_projection(
    root: &Path,
    now: chrono::DateTime<chrono::Local>,
    window: u32,
) -> Vec<clearhead_core::Action> {
    load_workspace(root)
        .unwrap()
        .iter()
        .flat_map(|charter| charter.plans.iter())
        .flat_map(|plan| render_occurrences(plan, now, window))
        .collect()
}
// Note: occurrences are no longer unioned into the loaded `DomainModel`, so the
// former `recurring_plan_projects_windowed_occurrences_into_model` and
// `sync_reconciles_owned_artifacts_not_projected_occurrences` tests are retired.
// Projection is now a query-only concern (see `render_projection`), and the
// materialized-token sync seal is unit-tested in `reconcile.rs`.

/// An isolated temp workspace with one daily recurring master (never the
/// committed fixture — these tests mutate the plan file).
fn recurring_plan_workspace() -> TempDir {
    let dir = tempfile::tempdir().unwrap();
    let charters = dir.path().join(".clearhead").join("charters");
    let plans = dir.path().join(".clearhead").join("plans").join("health");
    fs::create_dir_all(&charters).unwrap();
    fs::create_dir_all(&plans).unwrap();
    fs::write(charters.join("health.actions"), "").unwrap();
    fs::write(
        plans.join("run.ics"),
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n\
         BEGIN:VTODO\r\nUID:run@example.com\r\nSUMMARY:Run\r\n\
         DTSTART:20260101T080000Z\r\nRRULE:FREQ=DAILY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    )
    .unwrap();
    dir
}

fn fixed_now() -> chrono::DateTime<chrono::Local> {
    chrono::DateTime::parse_from_rfc3339("2026-06-15T00:00:00Z")
        .unwrap()
        .with_timezone(&chrono::Local)
}

/// Grab the first projected occurrence's handle: (id, plan_id, occurrence_key).
fn first_occurrence(
    root: &Path,
    now: chrono::DateTime<chrono::Local>,
) -> (uuid::Uuid, uuid::Uuid, String) {
    let occ = render_projection(root, now, 2)
        .into_iter()
        .find(|a| a.external_occurrence_key.is_some())
        .expect("a projected occurrence");
    (
        occ.id,
        occ.plan_id.unwrap(),
        occ.external_occurrence_key.unwrap(),
    )
}

#[test]
fn occurrence_complete_writes_deviation_that_reprojects() {
    // The whole occurrence-ops loop: resolve a projected occurrence's handle,
    // write a Complete deviation to the master, and prove reprojection reflects
    // it. This exercises the frame fix end to end — the RECURRENCE-ID key must
    // match the occurrence slot key for the override to bind.
    use clearhead_core::{ActionState, OccurrenceOp, apply_occurrence_op};

    let ws = recurring_plan_workspace();
    let root = ws.path();
    let now = fixed_now();

    let (occ_id, plan_id, key) = first_occurrence(root, now);
    apply_occurrence_op(
        root,
        None,
        plan_id,
        &key,
        &OccurrenceOp::Complete { at: now },
    )
    .unwrap();

    let reprojected = render_projection(root, now, 2)
        .into_iter()
        .find(|a| a.id == occ_id)
        .expect("the completed occurrence still projects at its slot");
    assert_eq!(
        reprojected.state,
        ActionState::Completed,
        "completion deviation binds on reprojection (RECURRENCE-ID key == occurrence key)"
    );
}

#[test]
fn occurrence_skip_removes_it_from_the_projection() {
    use clearhead_core::{OccurrenceOp, apply_occurrence_op};

    let ws = recurring_plan_workspace();
    let root = ws.path();
    let now = fixed_now();

    let (occ_id, plan_id, key) = first_occurrence(root, now);
    apply_occurrence_op(root, None, plan_id, &key, &OccurrenceOp::Skip).unwrap();

    assert!(
        render_projection(root, now, 2)
            .iter()
            .all(|a| a.id != occ_id),
        "the EXDATE'd slot no longer projects"
    );
}

#[test]
fn occurrence_reschedule_moves_the_slot_in_the_projection() {
    use clearhead_core::{OccurrenceOp, apply_occurrence_op};

    let ws = recurring_plan_workspace();
    let root = ws.path();
    let now = fixed_now();

    let (occ_id, plan_id, key) = first_occurrence(root, now);
    let moved = now + chrono::Duration::hours(30); // a distinct new time
    apply_occurrence_op(
        root,
        None,
        plan_id,
        &key,
        &OccurrenceOp::Reschedule {
            scheduled_at: Some(moved),
            due_date: None,
        },
    )
    .unwrap();

    // Same occurrence identity (keyed by the immutable slot), new scheduled time.
    let occ = render_projection(root, now, 2)
        .into_iter()
        .find(|a| a.id == occ_id)
        .expect("the rescheduled occurrence keeps its slot identity");
    assert_eq!(
        occ.scheduled_at.map(|t| t.with_timezone(&chrono::Utc)),
        Some(moved.with_timezone(&chrono::Utc)),
        "reschedule moves the value, not the identity"
    );
}

/// Rewrite the master's `DTSTART` in place — simulates a camp-B client (Apple
/// Reminders, etc.) completing an occurrence by advancing the anchor.
fn advance_master(root: &Path, from: &str, to: &str) {
    let ics = root.join(".clearhead/plans/health/run.ics");
    let content = fs::read_to_string(&ics).unwrap();
    let advanced = content.replace(&format!("DTSTART:{from}"), &format!("DTSTART:{to}"));
    assert_ne!(content, advanced, "DTSTART replacement must match");
    fs::write(&ics, advanced).unwrap();
}

#[test]
fn foreign_rollforward_is_ingested_as_completion() {
    use clearhead_core::{ActionState, occurrence_action_id, sync_master_rollforwards};

    let ws = recurring_plan_workspace(); // daily from 2026-01-01T08:00Z, uid run@example.com
    let root = ws.path();
    let ics = root.join(".clearhead/plans/health/run.ics");

    // First sight establishes the origin; nothing is recorded.
    assert_eq!(sync_master_rollforwards(root, None).unwrap(), 0);

    // Camp-B completes the 01-01 occurrence by advancing the anchor one day.
    advance_master(root, "20260101T080000Z", "20260102T080000Z");
    assert_eq!(sync_master_rollforwards(root, None).unwrap(), 1);

    // The anchor is reset to the origin and the 01-01 slot is a completed override.
    let content = fs::read_to_string(&ics).unwrap();
    assert!(
        content.contains("DTSTART:20260101T080000Z"),
        "anchor reset to origin"
    );
    assert!(content.contains("RECURRENCE-ID:20260101T080000Z"));
    assert!(content.contains("STATUS:COMPLETED"));

    // It projects as completed at its slot.
    let now = chrono::DateTime::parse_from_rfc3339("2026-01-01T00:00:00Z")
        .unwrap()
        .with_timezone(&chrono::Local);
    let occ_id = occurrence_action_id("run@example.com", "20260101T080000Z");
    let occ = render_projection(root, now, 2)
        .into_iter()
        .find(|a| a.id == occ_id)
        .expect("the origin slot still projects");
    assert_eq!(
        occ.state,
        ActionState::Completed,
        "roll-forward recorded as completion"
    );

    // A camp-B client that ignores overrides and re-advances records nothing new
    // (idempotent by slot) — only the anchor churns, history is stable.
    advance_master(root, "20260101T080000Z", "20260102T080000Z");
    assert_eq!(sync_master_rollforwards(root, None).unwrap(), 0);
    let content = fs::read_to_string(&ics).unwrap();
    assert_eq!(
        content.matches("RECURRENCE-ID:20260101T080000Z").count(),
        1,
        "the completion override is not duplicated under re-advance"
    );
}

#[test]
fn multi_period_rollforward_records_each_passed_occurrence() {
    use clearhead_core::sync_master_rollforwards;

    let ws = recurring_plan_workspace();
    let root = ws.path();
    let ics = root.join(".clearhead/plans/health/run.ics");

    assert_eq!(sync_master_rollforwards(root, None).unwrap(), 0); // establish origin

    // Sync gap: the client completed three occurrences (01-01, 01-02, 01-03),
    // advancing the anchor to 01-04 before we next sync.
    advance_master(root, "20260101T080000Z", "20260104T080000Z");
    assert_eq!(
        sync_master_rollforwards(root, None).unwrap(),
        3,
        "every passed occurrence is recorded, not just the last"
    );

    let content = fs::read_to_string(&ics).unwrap();
    assert!(
        content.contains("DTSTART:20260101T080000Z"),
        "anchor reset to origin"
    );
    for day in ["20260101T080000Z", "20260102T080000Z", "20260103T080000Z"] {
        assert!(
            content.contains(&format!("RECURRENCE-ID:{day}")),
            "missing completion override for {day}"
        );
    }
}
