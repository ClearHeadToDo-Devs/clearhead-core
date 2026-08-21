mod common;
use chrono::{Local, TimeZone};
use common::TestEnv;
use predicates::prelude::*;
use std::fs;

fn write_plans_sync_store(env: &TestEnv, action_id: &str, scheduled_at: &str) {
    let content = serde_json::json!({
        "version": 1,
        "plans_root": env.data_dir.join("plans"),
        "actions": {
            (action_id): {
                "scheduled_at": scheduled_at
            }
        }
    });
    env.write_text(
        "sync/plans.json",
        &serde_json::to_string_pretty(&content).unwrap(),
    );
}

#[test]
fn fresh_init_charter_plan_sync_stamps_into_the_real_charter() {
    let env = TestEnv::new();

    env.command().arg("init").assert().success();
    assert!(
        env.work_dir
            .join(".clearhead/charters/next.actions")
            .exists(),
        "init must materialize the project root charter"
    );
    assert!(
        env.work_dir.join(".clearhead/charters/.next.json").exists(),
        "the root charter must have persistent identity"
    );

    env.command()
        .args(["add", "charter", "Dogfood Operations", "--alias", "dogfood"])
        .assert()
        .success();

    let anchor = (Local::now() + chrono::Duration::minutes(1)).to_rfc3339();
    env.command()
        .args([
            "add",
            "plan",
            "Dogfood recurring check",
            "--charter",
            "dogfood",
            "--scheduled-at",
            &anchor,
            "--rrule",
            "FREQ=DAILY",
        ])
        .assert()
        .success();

    env.command().args(["sync", "calendar"]).assert().success();

    let actions =
        fs::read_to_string(env.work_dir.join(".clearhead/charters/dogfood.actions")).unwrap();
    assert!(actions.contains("Dogfood recurring check"), "{actions}");

    env.command()
        .arg("doctor")
        .assert()
        .success()
        .stdout(predicate::str::contains("workspace clean"));
}

#[test]
fn sync_calendar_refuses_a_quarantined_action_workspace() {
    let env = TestEnv::new();
    let malformed = concat!(
        "[ ] Read [[docs|https://example.com\n",
        "[ ] Next #019f0000-0000-7000-8000-000000000001\n",
    );
    env.write_text("charters/inbox.actions", malformed);

    env.command()
        .args(["sync", "calendar"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("sync calendar refused"));
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap(),
        malformed,
        "semantic mutation refusal must leave source byte-stable"
    );
}

#[test]
fn test_read_plans_shows_recurring_vtodo() {
    let env = TestEnv::new();
    env.write_plan_ics("inbox", "root.ics", &["My Plan"]);
    env.command()
        .arg("read")
        .arg("plans")
        .assert()
        .success()
        .stdout(predicate::str::contains("BEGIN:VCALENDAR"))
        .stdout(predicate::str::contains("SUMMARY:My Plan"));
}

#[test]
fn test_read_plans_honors_ids_and_jsonld_formats() {
    let env = TestEnv::new();
    env.write_plan_ics("inbox", "root.ics", &["My Plan"]);

    let ids = env
        .command()
        .args(["read", "plans", "--format", "ids"])
        .output()
        .unwrap();
    assert!(ids.status.success());
    let ids = String::from_utf8(ids.stdout).unwrap();
    let id = ids.trim();
    assert_eq!(id.len(), 36, "{ids}");
    assert!(uuid::Uuid::parse_str(id).is_ok(), "{ids}");

    let json = env
        .command()
        .args(["read", "plans", "--format", "json-ld"])
        .output()
        .unwrap();
    assert!(json.status.success());
    let value: serde_json::Value = serde_json::from_slice(&json.stdout).unwrap();
    let graph = value["@graph"].as_array().unwrap();
    assert!(
        graph
            .iter()
            .any(|node| node.get("name").and_then(|v| v.as_str()) == Some("My Plan")),
        "{}",
        String::from_utf8_lossy(&json.stdout)
    );
}

#[test]
fn test_import_plans_splits_multi_vtodo_ics_into_vdir_files() {
    let env = TestEnv::new();
    env.write_actions("bulk-export.actions", "");
    let source = env.data_dir.join("bulk-export.ics");
    fs::write(
        &source,
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:plan-one@example.com\r\nSUMMARY:Plan One\r\nDTSTART:20260428T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nBEGIN:VTODO\r\nUID:plan-two@example.com\r\nSUMMARY:Plan Two\r\nDTSTART:20260429T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    ).unwrap();
    env.command()
        .arg("import")
        .arg("plans")
        .arg(&source)
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "Imported 2 plan(s) into charter 'bulk-export'",
        ));
    let plans_dir = env.data_dir.join("plans").join("bulk-export");
    let first = fs::read_to_string(plans_dir.join("plan-one@example.com.ics")).unwrap();
    assert!(first.contains("BEGIN:VTODO"), "{first}");
    assert!(first.contains("RRULE:FREQ=WEEKLY"), "{first}");
    assert!(plans_dir.join("plan-two@example.com.ics").exists());
    env.command()
        .arg("read")
        .arg("plans")
        .assert()
        .success()
        .stdout(predicate::str::contains("Plan One"));
}

#[test]
fn test_import_plans_honors_explicit_charter_flag() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "");
    let source = env.data_dir.join("calendar.ics");
    fs::write(
        &source,
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:focus@example.com\r\nSUMMARY:Focus Block\r\nDTSTART:20260428T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    ).unwrap();
    env.command()
        .arg("import")
        .arg("plans")
        .arg(&source)
        .arg("--charter")
        .arg("inbox")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "Imported 1 plan(s) into charter 'inbox'",
        ));
    assert!(
        env.data_dir
            .join("plans")
            .join("inbox")
            .join("focus@example.com.ics")
            .exists()
    );
}

#[test]
fn test_import_plans_errors_on_existing_uid_without_overwrite() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "");
    let existing = env
        .data_dir
        .join("plans")
        .join("inbox")
        .join("focus@example.com.ics");
    fs::create_dir_all(existing.parent().unwrap()).unwrap();
    fs::write(&existing, "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:focus@example.com\r\nSUMMARY:Old Focus\r\nDTSTART:20260427T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n").unwrap();
    let source = env.data_dir.join("collision.ics");
    fs::write(&source, "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:focus@example.com\r\nSUMMARY:New Focus\r\nDTSTART:20260428T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n").unwrap();
    env.command()
        .arg("import")
        .arg("plans")
        .arg(&source)
        .arg("--charter")
        .arg("inbox")
        .assert()
        .failure()
        .stderr(predicate::str::contains("re-run with --overwrite"));
    assert!(
        fs::read_to_string(existing)
            .unwrap()
            .contains("SUMMARY:Old Focus")
    );
}

#[test]
fn test_import_plans_overwrites_existing_uid_with_flag() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "");
    let existing = env
        .data_dir
        .join("plans")
        .join("inbox")
        .join("focus@example.com.ics");
    fs::create_dir_all(existing.parent().unwrap()).unwrap();
    fs::write(&existing, "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:focus@example.com\r\nSUMMARY:Old Focus\r\nDTSTART:20260427T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n").unwrap();
    let source = env.data_dir.join("collision.ics");
    fs::write(&source, "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:focus@example.com\r\nSUMMARY:New Focus\r\nDTSTART:20260428T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n").unwrap();
    env.command()
        .arg("import")
        .arg("plans")
        .arg(&source)
        .arg("--charter")
        .arg("inbox")
        .arg("--overwrite")
        .assert()
        .success()
        .stdout(predicate::str::contains("(1 overwritten)"));
    assert!(
        fs::read_to_string(existing)
            .unwrap()
            .contains("SUMMARY:New Focus")
    );
}

#[test]
fn test_error_on_missing_ics_file() {
    let env = TestEnv::new();
    env.command()
        .arg("read")
        .arg("plans")
        .arg("--file")
        .arg("/nonexistent/path.ics")
        .assert()
        .failure();
}

#[test]
fn test_add_command_with_options() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "");
    env.command()
        .arg("add")
        .arg("plan")
        .arg("High Priority Task")
        .arg("--priority")
        .arg("1")
        .arg("--context")
        .arg("work")
        .arg("--context")
        .arg("urgent")
        .arg("--description")
        .arg("Do it now")
        .arg("--rrule")
        .arg("FREQ=WEEKLY;BYDAY=MO")
        .assert()
        .success();
    let plans_dir = env.data_dir.join("plans").join("inbox");
    let written = fs::read_dir(&plans_dir)
        .unwrap()
        .next()
        .unwrap()
        .unwrap()
        .path();
    let content = fs::read_to_string(written).unwrap();
    assert!(content.contains("SUMMARY:High Priority Task"));
    assert!(content.contains("DESCRIPTION:Do it now"));
    assert!(content.contains("RRULE:FREQ=WEEKLY;BYDAY=MO"));
}

#[test]
fn test_add_plan_file_flag_writes_single_todo_file_to_explicit_path() {
    let env = TestEnv::new();
    let output = env
        .data_dir
        .join("plans")
        .join("focus")
        .join("focus-block.ics");
    env.command()
        .arg("add")
        .arg("plan")
        .arg("Focus Block")
        .arg("--file")
        .arg(&output)
        .arg("--rrule")
        .arg("FREQ=WEEKLY;BYDAY=TU")
        .arg("--scheduled-at")
        .arg("2026-04-28T10:00:00Z")
        .assert()
        .success();
    let content = fs::read_to_string(&output).unwrap();
    assert!(content.contains("SUMMARY:Focus Block"));
    assert!(content.contains("BEGIN:VTODO"));
}

#[test]
fn test_add_plan_file_flag_rejects_non_ics_path() {
    let env = TestEnv::new();
    env.command()
        .arg("add")
        .arg("plan")
        .arg("Focus Block")
        .arg("--file")
        .arg(env.data_dir.join("plans").join("focus"))
        .arg("--rrule")
        .arg("FREQ=WEEKLY;BYDAY=TU")
        .assert()
        .failure()
        .stderr(predicate::str::contains("must end with '.ics'"));
}

#[test]
fn test_complete_plan_explains_state_lives_on_acts() {
    let env = TestEnv::new();
    env.write_ics("inbox/plans/scheduled-plan.ics", &["Scheduled Plan"]);
    env.command()
        .arg("complete")
        .arg("plan")
        .arg("Scheduled Plan")
        .assert()
        .failure()
        .stderr(predicate::str::contains("use `complete action`"));
}

#[test]
fn test_archive_plans_explains_externally_owned_schedule_lifecycle() {
    let env = TestEnv::new();
    env.command()
        .arg("archive")
        .arg("plans")
        .assert()
        .failure()
        .stderr(predicate::str::contains("use `delete plan`"));
}

#[test]
fn test_sync_events_command() {
    let env = TestEnv::new();
    let uuid1 = "019baaec-00b6-7991-be34-94b68212619a";
    let uuid2 = "019baaec-00b6-7991-be34-94b68212619b";
    env.write_actions(
        "inbox.actions",
        &format!("[ ] Task 1 #{}\n[ ] Task 2 #{}", uuid1, uuid2),
    );
    env.command()
        .arg("sync")
        .arg("events")
        .assert()
        .success()
        .stdout(predicate::str::contains("2 events backfilled"));
    env.command().arg("sync").arg("events").assert().success();
    let uuid3 = "019baaec-00b6-7991-be34-94b68212619c";
    env.write_actions(
        "inbox.actions",
        &format!(
            "[ ] Task 1 #{}\n[ ] Task 2 #{}\n[ ] Task 3 #{}",
            uuid1, uuid2, uuid3
        ),
    );
    env.command().arg("sync").arg("events").assert().success();
}

#[test]
fn test_sync_calendar_creates_action_mirror_and_stamps_sync_store() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions(
        "inbox.actions",
        &format!("[ ] Sync me @2026-04-28T10:00 #{}", uuid),
    );

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("push action → calendar"))
        .stdout(predicate::str::contains(
            "Sync complete. 1 push, 0 pull, 0 converged, 0 conflict.",
        ));

    let ics_path = env
        .data_dir
        .join("plans")
        .join("inbox")
        .join(format!("{}.ics", uuid));
    let ics = fs::read_to_string(&ics_path).unwrap();
    assert!(ics.contains(&format!("UID:{}", uuid)));
    assert!(ics.contains("SUMMARY:Sync me"));

    let sync_store = fs::read_to_string(env.data_dir.join("sync").join("plans.json")).unwrap();
    assert!(sync_store.contains("scheduled_at"));
    assert!(sync_store.contains(uuid));
}

#[test]
fn test_sync_calendar_pulls_calendar_edit_into_action_file() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions(
        "inbox.actions",
        &format!("[ ] Pull me @2026-04-28T10:00 #{}", uuid),
    );

    let base = Local
        .with_ymd_and_hms(2026, 4, 28, 10, 0, 0)
        .unwrap()
        .to_rfc3339();
    write_plans_sync_store(&env, uuid, &base);
    env.write_text(
        &format!("plans/inbox/{}.ics", uuid),
        &format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:{}\r\nSUMMARY:Pull me\r\nSTATUS:NEEDS-ACTION\r\nDTSTART:20260429T100000\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
            uuid
        ),
    );

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("pull calendar → action"))
        .stdout(predicate::str::contains(
            "Sync complete. 0 push, 1 pull, 0 converged, 0 conflict.",
        ));

    let actions = fs::read_to_string(env.data_dir.join("charters").join("inbox.actions")).unwrap();
    assert!(actions.contains("@2026-04-29T10:00"));

    let sync_store = fs::read_to_string(env.data_dir.join("sync").join("plans.json")).unwrap();
    assert!(sync_store.contains("scheduled_at"));
    assert!(sync_store.contains("2026-04-29T10:00:00"));
}

#[test]
fn test_sync_calendar_pulls_all_owned_vtodo_fields_from_arbitrary_vdir_filename() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions(
        "inbox.actions",
        &format!(
            "[ ] Original $ old notes $ @2026-04-28T10:00 :2026-04-29T17:00 #{}",
            uuid
        ),
    );
    env.command().arg("sync").arg("calendar").assert().success();

    let canonical = env
        .data_dir
        .join("plans")
        .join("inbox")
        .join(format!("{}.ics", uuid));
    let transported = canonical.parent().unwrap().join("server-resource-name.ics");
    fs::rename(&canonical, &transported).unwrap();
    fs::write(
        &transported,
        format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Other Client//EN\r\nBEGIN:VTODO\r\nUID:{}\r\nSUMMARY:Edited title\r\nDESCRIPTION:edited notes\r\nSTATUS:COMPLETED\r\nPRIORITY:9\r\nCATEGORIES:calendar,portable\r\nDTSTART:20260430T140000Z\r\nDUE:20260501T180000Z\r\nCOMPLETED:20260430T150000Z\r\nX-CLIENT-METADATA:keep-me\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
            uuid
        ),
    )
    .unwrap();

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("pull calendar → action"));

    let actions = fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap();
    assert!(actions.contains("[x] Edited title"));
    assert!(actions.contains("edited notes"), "{actions}");
    assert!(actions.contains("%2026-04-30T"));
    assert!(actions.contains("!9"), "{actions}");
    assert!(actions.contains("+calendar,portable"), "{actions}");
    assert!(
        !canonical.exists(),
        "sync must not duplicate a transport-renamed resource"
    );
    let resource = fs::read_to_string(transported).unwrap();
    assert!(resource.contains("X-CLIENT-METADATA:keep-me"));
}

#[test]
fn test_sync_calendar_imports_calendar_created_vtodo_with_arbitrary_uid() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "");
    let uid = "calendar-client-generated@example.test";
    let expected_id = clearhead_core::action_id_from_vtodo_uid(uid);
    env.write_text(
        "plans/inbox/client-resource.ics",
        &format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Calendar Client//EN\r\nBEGIN:VTODO\r\nUID:{}\r\nSUMMARY:Captured in calendar\r\nDESCRIPTION:created from the calendar UI\r\nSTATUS:IN-PROCESS\r\nPRIORITY:8\r\nCATEGORIES:errands,phone\r\nDUE:20260501T180000Z\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
            uid
        ),
    );

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("pull calendar → new action"));

    let actions = fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap();
    assert!(actions.contains("[-] Captured in calendar"), "{actions}");
    assert!(actions.contains("!8"), "{actions}");
    assert!(actions.contains("+errands,phone"), "{actions}");
    assert!(actions.contains(&format!("#{}", expected_id)), "{actions}");

    // Adoption is stable and does not rewrite the client's interoperable UID.
    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("Already in sync."));
    let original_resource = env.data_dir.join("plans/inbox/client-resource.ics");
    let resource = fs::read_to_string(&original_resource).unwrap();
    assert!(resource.contains(&format!("UID:{}", uid)));

    // Resource absence has no lifecycle meaning: recreate it with the original
    // arbitrary UID remembered by the vdir projection store.
    fs::remove_file(original_resource).unwrap();
    env.command().arg("sync").arg("calendar").assert().success();
    let recreated = env
        .data_dir
        .join(format!("plans/inbox/{}.ics", expected_id));
    let resource = fs::read_to_string(recreated).unwrap();
    assert!(resource.contains(&format!("UID:{}", uid)));
    let actions = fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap();
    assert!(actions.contains("[-] Captured in calendar"));
}

#[test]
fn test_sync_calendar_routes_next_collection_to_root_not_nested_primary_file() {
    let env = TestEnv::new();
    env.write_actions(
        "next.actions",
        "[ ] Root sentinel #019baaec-00b6-7991-be34-94b6821261a0",
    );
    env.write_actions(
        "linux/next.actions",
        "[ ] Nested sentinel #019baaec-00b6-7991-be34-94b6821261a1",
    );
    env.write_text(
        "plans/next/client-resource.ics",
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Calendar Client//EN\r\nBEGIN:VTODO\r\nUID:root-capture@example.test\r\nSUMMARY:Captured in root collection\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    );

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("pull calendar → new action"));

    let root = fs::read_to_string(env.data_dir.join("charters/next.actions")).unwrap();
    let nested = fs::read_to_string(env.data_dir.join("charters/linux/next.actions")).unwrap();
    assert!(root.contains("Captured in root collection"), "{root}");
    assert!(!nested.contains("Captured in root collection"), "{nested}");

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("Already in sync."));
}

#[test]
fn test_sync_calendar_refuses_to_invent_a_charter_for_an_unowned_collection() {
    let env = TestEnv::new();
    env.write_text(
        "plans/fresh/client.ics",
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:fresh-task@example.test\r\nSUMMARY:Fresh capture\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    );
    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .failure()
        .stderr(predicate::str::contains("has no owning charter"));
    assert!(!env.data_dir.join("charters/fresh.actions").exists());
}

#[test]
fn test_sync_calendar_recreates_a_missing_projection_without_changing_state() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions(
        "inbox.actions",
        &format!("[-] Keep the action !7 +work #{}", uuid),
    );
    env.command().arg("sync").arg("calendar").assert().success();
    let resource = env.data_dir.join(format!("plans/inbox/{}.ics", uuid));
    fs::remove_file(&resource).unwrap();

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("push action → calendar"));
    assert!(resource.exists());
    let actions = fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap();
    assert!(actions.contains("[-] Keep the action"));
}

#[test]
fn test_sync_calendar_status_cancelled_is_the_explicit_cancellation_signal() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions(
        "inbox.actions",
        &format!("[ ] Cancel from calendar #{}", uuid),
    );
    env.command().arg("sync").arg("calendar").assert().success();
    env.write_text(
        &format!("plans/inbox/{}.ics", uuid),
        &format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{}\r\nSUMMARY:Cancel from calendar\r\nSTATUS:CANCELLED\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
            uuid
        ),
    );

    env.command()
        .arg("sync")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("pull calendar → action"));
    let actions = fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap();
    assert!(actions.contains("[_] Cancel from calendar"), "{actions}");
}

#[test]
fn test_sync_calendar_conflict_can_be_resolved_toward_action() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions(
        "inbox.actions",
        &format!("[ ] Clash @2026-04-29T10:00 #{}", uuid),
    );

    let base = Local
        .with_ymd_and_hms(2026, 4, 28, 10, 0, 0)
        .unwrap()
        .to_rfc3339();
    write_plans_sync_store(&env, uuid, &base);
    env.write_text(
        &format!("plans/inbox/{}.ics", uuid),
        &format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:{}\r\nSUMMARY:Clash\r\nSTATUS:NEEDS-ACTION\r\nDTSTART:20260430T100000\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
            uuid
        ),
    );

    env.command()
        .arg("sync")
        .arg("calendar")
        .arg("--conflict")
        .arg("action")
        .assert()
        .success()
        .stdout(predicate::str::contains("push action → calendar"))
        .stdout(predicate::str::contains(
            "Sync complete. 1 push, 0 pull, 0 converged, 0 conflict.",
        ));

    let ics_path = env
        .data_dir
        .join("plans")
        .join("inbox")
        .join(format!("{}.ics", uuid));
    let actions = clearhead_core::parse_vtodo_actions(&ics_path).unwrap();
    assert_eq!(actions.len(), 1);
    assert_eq!(actions[0].id.to_string(), uuid);
    let dt = actions[0].scheduled_at.unwrap();
    assert_eq!(dt.format("%Y-%m-%dT%H:%M").to_string(), "2026-04-29T10:00");
}

#[test]
fn test_sync_calendar_conflict_can_be_resolved_toward_calendar() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions(
        "inbox.actions",
        &format!("[ ] Clash @2026-04-29T10:00 #{}", uuid),
    );

    let base = Local
        .with_ymd_and_hms(2026, 4, 28, 10, 0, 0)
        .unwrap()
        .to_rfc3339();
    write_plans_sync_store(&env, uuid, &base);
    env.write_text(
        &format!("plans/inbox/{}.ics", uuid),
        &format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//Test//EN\r\nBEGIN:VTODO\r\nUID:{}\r\nSUMMARY:Clash\r\nSTATUS:NEEDS-ACTION\r\nDTSTART:20260430T100000\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
            uuid
        ),
    );

    env.command()
        .arg("sync")
        .arg("calendar")
        .arg("--conflict")
        .arg("calendar")
        .assert()
        .success()
        .stdout(predicate::str::contains("pull calendar → action"))
        .stdout(predicate::str::contains(
            "Sync complete. 0 push, 1 pull, 0 converged, 0 conflict.",
        ));

    let actions = fs::read_to_string(env.data_dir.join("charters").join("inbox.actions")).unwrap();
    assert!(actions.contains("@2026-04-30T10:00"));
}

#[test]
fn test_read_plans_rejects_where_flag() {
    let env = TestEnv::new();
    env.command()
        .arg("read")
        .arg("plans")
        .arg("--where")
        .arg("?s a cco:Plan")
        .assert()
        .failure();
}

#[test]
fn test_read_plans_charter_filter() {
    let env = TestEnv::new();
    env.write_actions("build_clearhead.actions", "");
    env.write_plan_ics("build_clearhead", "top-level-plan.ics", &["Top level plan"]);
    env.write_plan_ics("build_clearhead-subcharter", "sub-plan.ics", &["Sub plan"]);
    env.command()
        .arg("read")
        .arg("plans")
        .arg("--charter")
        .arg("build_clearhead")
        .assert()
        .success()
        .stdout(predicate::str::contains("Top level plan"));
}

#[test]
fn test_recursive_requires_charter() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Task");
    env.command()
        .arg("read")
        .arg("plans")
        .arg("--recursive")
        .assert()
        .failure();
}

#[test]
fn test_export_plans_stdin_recover_mode_warns_and_succeeds() {
    let env = TestEnv::new();
    env.command()
        .arg("export")
        .arg("plans")
        .arg("-")
        .write_stdin("not valid actions syntax !!!\n[ ] Keep export\n")
        .assert()
        .success()
        .stdout(predicate::str::contains("BEGIN:VCALENDAR"))
        .stderr(predicate::str::contains("parsed with"));
}

#[test]
fn test_sync_events_file_recover_mode_warns_and_succeeds() {
    let env = TestEnv::new();
    env.write_text(
        "charters/recover-sync.actions",
        "not valid actions syntax !!!\n[ ] Sync me #019baaec-00b6-7991-be34-94b68212619a\n",
    );
    let path = env.data_dir.join("charters").join("recover-sync.actions");
    env.command()
        .arg("sync")
        .arg("events")
        .arg(&path)
        .assert()
        .success()
        .stdout(predicate::str::contains("1 events backfilled"))
        .stderr(predicate::str::contains("parsed with"));
}
