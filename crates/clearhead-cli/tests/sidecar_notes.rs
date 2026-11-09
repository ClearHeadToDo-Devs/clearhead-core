mod common;

use clearhead_cli::filesystem::sidecar::{append_note, read_sidecar, stamp_charter_id};
use clearhead_cli::filesystem::{apply_doctor_repairs, diagnose_workspace, read_workspace};
use clearhead_core::workspace::sidecar::{Note, parse_sidecar, render_sidecar, sidecar_path};
use serde_json::json;
use std::{fs, path::Path, process::Command};
use uuid::Uuid;

const ENTITY: &str = "019f733d-45b2-7f21-bcad-5610887b7230";
const CHARTER: &str = "019f733d-45c2-7dd2-91dc-8631f33c6b77";

fn note(text: &str, at: &str) -> Note {
    serde_json::from_value(json!({"at": at, "by": "agent:test", "text": text})).unwrap()
}

#[test]
fn sidecar_accepts_date_only_notes_as_midnight_utc() {
    let metadata = parse_sidecar(
        &json!({
            "actions": {ENTITY: {"created": "2026-11-06T12:00:00Z"}},
            "notes": {ENTITY: [
                {"at": "2026-11-07", "text": "migrated", "future": true},
                {"at": "2026-11-08T01:00:00+01:00", "text": "later"}
            ]}
        })
        .to_string(),
    )
    .unwrap();
    assert_eq!(
        metadata.notes[ENTITY][0].at.to_rfc3339(),
        "2026-11-07T00:00:00+00:00"
    );
    assert!(metadata.notes[ENTITY][0].by.is_none());
    assert_eq!(metadata.notes[ENTITY][0].extra["future"], true);
    assert_eq!(metadata.notes[ENTITY].len(), 2);
    assert!(metadata.actions[ENTITY].created.is_some());
    let rendered = render_sidecar(&metadata).unwrap();
    assert_eq!(parse_sidecar(&rendered).unwrap().notes, metadata.notes);
}

#[test]
fn rewrites_preserve_every_unknown_key_and_format_with_newline() {
    let input = json!({
        "future": {"nested": [1, {"x": true}]},
        "charter": {"future": "charter"},
        "actions": {ENTITY: {
            "future": "action", "plan": {"uid": "plan", "future": "link"},
            "occurrence": {"plan_id": CHARTER, "occurrence_key": "slot", "plan_title": "Plan", "future": "snapshot"}
        }},
        "notes": {ENTITY: [{"at": "2026-11-01T00:00:00Z", "text": "legacy", "future": 4}]}
    });
    let env = common::TestEnv::new();
    env.write_text("charters/.work.json", &input.to_string());
    let anchor = env.data_dir.join("charters/work.actions");
    stamp_charter_id(&anchor, CHARTER.parse().unwrap()).unwrap();
    let output = fs::read_to_string(sidecar_path(&anchor)).unwrap();
    assert!(output.ends_with("\n"));
    assert!(output.contains("\n  \"charter\": {\n    \"id\":"));
    let mut value: serde_json::Value = serde_json::from_str(&output).unwrap();
    value.as_object_mut().unwrap().remove("$schema");
    value["charter"].as_object_mut().unwrap().remove("id");
    assert_eq!(value, input);
    assert_eq!(
        render_sidecar(&parse_sidecar(&output).unwrap()).unwrap(),
        output
    );
}

#[test]
fn workspace_unions_sidecars_and_stably_orders_hand_edited_logs() {
    let env = common::TestEnv::new();
    env.write_actions("work.actions", &format!("[x] Done #{ENTITY}\n"));
    env.write_text(
        "charters/.work.json",
        &json!({"notes": {ENTITY: [
            {"at": "2026-11-02T01:00:00+01:00", "text": "tie a"},
            {"at": "2026-11-01T00:00Z", "text": "first"}
        ]}})
        .to_string(),
    );
    env.write_text(
        "charters/.z-old.json",
        &json!({"notes": {ENTITY: [
        {"at": "2026-11-02T00:00:00Z", "text": "tie b"},
        {"at": "2026-11-03T00:00:00Z", "text": "last"}
    ], CHARTER: [{"at": "2026-11-01T00:00:00Z", "text": "charter note"}]}})
        .to_string(),
    );
    env.write_text(
        "charters/.zz-old.json",
        &json!({"notes": {ENTITY: [
            {"at": "2026-11-02T00:00:00Z", "text": "tie b"},
            {"at": "2026-10-31", "text": "earliest"}
        ]}})
        .to_string(),
    );
    let read = read_workspace(&env.data_dir).unwrap();
    assert_eq!(
        read.notes[ENTITY]
            .iter()
            .map(|n| n.text.as_str())
            .collect::<Vec<_>>(),
        ["earliest", "first", "tie a", "tie b", "tie b", "last"]
    );
    assert_eq!(read.notes[CHARTER][0].text, "charter note");
    assert!(read.notes[ENTITY].iter().all(|n| n.by.is_none()));
}

#[test]
fn append_worker() {
    let Some(anchor) = std::env::var_os("CLEARHEAD_NOTE_TEST_ANCHOR") else {
        return;
    };
    // Multiple independently opened file descriptors in independent processes.
    let text = std::env::var("CLEARHEAD_NOTE_TEST_TEXT").unwrap();
    for index in 0..30 {
        append_note(
            Path::new(&anchor),
            ENTITY.parse().unwrap(),
            note(&format!("{text} {index}"), "2026-11-01T00:00:00Z"),
        )
        .unwrap();
    }
}

#[test]
fn concurrent_process_appends_keep_both_logs() {
    let env = common::TestEnv::new();
    env.write_actions("work.completed.actions", &format!("[x] Done #{ENTITY}\n"));
    let anchor = env.data_dir.join("charters/work.completed.actions");
    let exe = std::env::current_exe().unwrap();
    let spawn = |text| {
        Command::new(&exe)
            .args(["--exact", "append_worker"])
            .env("CLEARHEAD_NOTE_TEST_ANCHOR", &anchor)
            .env("CLEARHEAD_NOTE_TEST_TEXT", text)
            .spawn()
            .unwrap()
    };
    let mut a = spawn("a");
    let mut b = spawn("b");
    assert!(a.wait().unwrap().success());
    assert!(b.wait().unwrap().success());
    let metadata = read_sidecar(&sidecar_path(&anchor)).unwrap();
    let texts = metadata.notes[ENTITY]
        .iter()
        .map(|n| n.text.clone())
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(metadata.notes[ENTITY].len(), 60);
    assert_eq!(texts.len(), 60);
}

#[test]
fn append_has_no_entity_kind_or_state_restriction() {
    for path in [
        "charters/work.actions",
        "charters/work.completed.actions",
        "charters/work.habits.actions",
        "archive/charters/work.actions",
    ] {
        let env = common::TestEnv::new();
        env.write_text(path, "");
        let anchor = env.data_dir.join(path);
        for id in [ENTITY, CHARTER] {
            append_note(
                &anchor,
                id.parse().unwrap(),
                note("entry", "2026-11-01T00:00:00Z"),
            )
            .unwrap();
            assert_eq!(
                read_sidecar(&sidecar_path(&anchor)).unwrap().notes[id].len(),
                1
            );
        }
    }
}

#[test]
fn doctor_relocates_logs_for_actions_charters_and_habits_and_keeps_orphans() {
    let env = common::TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Work #{ENTITY}\n"));
    env.write_text(
        "charters/work.md",
        &format!("---\nid: {CHARTER}\nalias: work\nstate: Active\n---\n# Work\n"),
    );
    let habit = "019f733d-4600-7000-8000-000000000001";
    env.write_actions(
        "work.habits.actions",
        &format!("[ ] Daily @2026-11-01 R:FREQ=DAILY #{habit}\n"),
    );
    let orphan = Uuid::new_v4().to_string();
    let mut logs = serde_json::Map::new();
    for id in [ENTITY, CHARTER, habit, &orphan] {
        logs.insert(
            id.to_string(),
            json!([{"at": "2026-11-01T00:00:00Z", "text": id}]),
        );
    }
    env.write_text(
        "charters/.old.json",
        &json!({"future": 42, "notes": logs}).to_string(),
    );
    env.write_text(
        "charters/.work.json",
        &json!({"notes": {ENTITY: [{"at": "2026-10-01T00:00:00Z", "text": "existing"}]}})
            .to_string(),
    );
    let diagnosis = diagnose_workspace(&env.data_dir).unwrap();
    let relocations = diagnosis
        .repairs
        .iter()
        .filter(|r| {
            matches!(
                r,
                clearhead_core::workspace::store::DoctorRepair::RelocateNotes { .. }
            )
        })
        .count();
    assert_eq!(relocations, 3, "{:?}", diagnosis.findings);
    apply_doctor_repairs(&env.data_dir, &diagnosis.repairs).unwrap();
    let destination = read_sidecar(&env.data_dir.join("charters/.work.json")).unwrap();
    assert_eq!(destination.notes[ENTITY].len(), 2);
    assert_eq!(destination.notes[CHARTER].len(), 1);
    assert_eq!(destination.notes[habit].len(), 1);
    let source = read_sidecar(&env.data_dir.join("charters/.old.json")).unwrap();
    assert_eq!(source.notes.len(), 1);
    assert_eq!(source.notes[&orphan][0].text, orphan);
    assert_eq!(source.extra["future"], 42);
    let after = diagnose_workspace(&env.data_dir).unwrap();
    assert!(after.findings.iter().any(|f| f.code == "orphaned-notes"));
    assert!(!after.findings.iter().any(|f| f.code == "misplaced-notes"));
}

#[test]
fn doctor_creates_completed_destination_and_never_removes_orphan_only_or_corrupt_logs() {
    let env = common::TestEnv::new();
    env.write_actions("work.completed.actions", &format!("[x] Done #{ENTITY}\n"));
    env.write_text(
        "charters/.old.json",
        &json!({"notes": {ENTITY: [
            {"at": "2026-11-01T00:00Z", "text": "completed"}
        ]}})
        .to_string(),
    );
    env.write_text(
        "charters/.orphan.json",
        &json!({"notes": {CHARTER: [
            {"at": "2026-11-01T00:00Z", "text": "orphan"}
        ]}})
        .to_string(),
    );
    let corrupt = r#"{"notes": {"bad-id": [{"at": "bad-date", "text": "keep this too"}]}}"#;
    env.write_text("charters/.corrupt.json", corrupt);
    let diagnosis = diagnose_workspace(&env.data_dir).unwrap();
    apply_doctor_repairs(&env.data_dir, &diagnosis.repairs).unwrap();
    assert_eq!(
        read_sidecar(&env.data_dir.join("charters/.work.completed.json"))
            .unwrap()
            .notes[ENTITY][0]
            .text,
        "completed"
    );
    assert_eq!(
        read_sidecar(&env.data_dir.join("charters/.orphan.json"))
            .unwrap()
            .notes[CHARTER][0]
            .text,
        "orphan"
    );
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters/.corrupt.json")).unwrap(),
        corrupt
    );
}

#[test]
fn doctor_relocates_to_archived_entity_without_overwriting_its_existing_log() {
    let env = common::TestEnv::new();
    env.write_text(
        "archive/charters/work.actions",
        &format!("[x] Done #{ENTITY}\n"),
    );
    env.write_text(
        "archive/charters/work.md",
        &format!("---\nid: {CHARTER}\nalias: work\nstate: Closed\n---\n# Work\n"),
    );
    env.write_text(
        "archive/charters/.work.json",
        &json!({"future": 7, "notes": {ENTITY: [
            {"at": "2026-10-01T00:00Z", "text": "existing archive entry"}
        ]}})
        .to_string(),
    );
    env.write_text(
        "charters/.old.json",
        &json!({"notes": {
            ENTITY: [{"at": "2026-11-01T00:00Z", "text": "moved action"}],
            CHARTER: [{"at": "2026-11-01T00:00Z", "text": "moved charter"}]
        }})
        .to_string(),
    );
    let diagnosis = diagnose_workspace(&env.data_dir).unwrap();
    apply_doctor_repairs(&env.data_dir, &diagnosis.repairs).unwrap();
    let dest = read_sidecar(&env.data_dir.join("archive/charters/.work.json")).unwrap();
    assert_eq!(dest.notes[ENTITY].len(), 2);
    assert_eq!(dest.notes[CHARTER][0].text, "moved charter");
    assert_eq!(dest.extra["future"], 7);
    assert!(
        read_sidecar(&env.data_dir.join("charters/.old.json"))
            .unwrap()
            .notes
            .is_empty()
    );
}

#[test]
fn doctor_retry_after_destination_write_does_not_duplicate_notes() {
    let env = common::TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Work #{ENTITY}\n"));
    let shared = json!({"at": "2026-11-01T00:00Z", "text": "copied", "future": 1});
    let distinct = json!({"at": "2026-11-01T00:00Z", "text": "copied", "future": 2});
    env.write_text(
        "charters/.old.json",
        &json!({"notes": {ENTITY: [shared.clone(), shared.clone(), distinct.clone()]}}).to_string(),
    );
    let before = diagnose_workspace(&env.data_dir).unwrap();
    // Model an interrupted repair: the destination gained a note, but the
    // source was not removed. A fresh diagnosis must safely finish the move.
    env.write_text(
        "charters/.work.json",
        &json!({"notes": {ENTITY: [shared.clone()]}}).to_string(),
    );
    assert!(apply_doctor_repairs(&env.data_dir, &before.repairs).is_err());
    let retry = diagnose_workspace(&env.data_dir).unwrap();
    assert!(retry.findings.iter().any(|f| f.code == "misplaced-notes"));
    apply_doctor_repairs(&env.data_dir, &retry.repairs).unwrap();
    let destination = read_sidecar(&env.data_dir.join("charters/.work.json")).unwrap();
    assert_eq!(
        destination.notes[ENTITY],
        vec![
            serde_json::from_value::<Note>(shared.clone()).unwrap(),
            serde_json::from_value::<Note>(shared).unwrap(),
            serde_json::from_value::<Note>(distinct).unwrap()
        ]
    );
    assert!(
        read_sidecar(&env.data_dir.join("charters/.old.json"))
            .unwrap()
            .notes
            .is_empty()
    );
    let after = diagnose_workspace(&env.data_dir).unwrap();
    apply_doctor_repairs(&env.data_dir, &after.repairs).unwrap();
    assert_eq!(
        read_sidecar(&env.data_dir.join("charters/.work.json"))
            .unwrap()
            .notes,
        destination.notes
    );
}

#[test]
fn doctor_refuses_stale_repairs_after_an_append_without_losing_the_new_note() {
    let env = common::TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Work #{ENTITY}\n"));
    env.write_text(
        "charters/.old.json",
        &json!({"notes": {ENTITY: [
            {"at": "2026-11-01T00:00Z", "text": "old"}
        ]}})
        .to_string(),
    );
    let diagnosis = diagnose_workspace(&env.data_dir).unwrap();
    let source = env.data_dir.join("charters/old.actions");
    append_note(
        &source,
        ENTITY.parse().unwrap(),
        note("new", "2026-11-02T00:00:00Z"),
    )
    .unwrap();
    assert!(apply_doctor_repairs(&env.data_dir, &diagnosis.repairs).is_err());
    assert_eq!(
        read_sidecar(&sidecar_path(&source)).unwrap().notes[ENTITY].len(),
        2
    );
    assert!(!env.data_dir.join("charters/.work.json").exists());
}

#[test]
fn new_charter_sidecar_has_two_space_json_and_trailing_newline() {
    let env = common::TestEnv::new();
    env.command()
        .args(["add", "charter", "Fresh", "--alias", "fresh"])
        .assert()
        .success();
    let bytes = fs::read_to_string(env.data_dir.join("charters/.fresh.json")).unwrap();
    assert!(bytes.ends_with('\n'));
    assert!(bytes.contains("\n  \"charter\": {\n    \"id\":"));
}
