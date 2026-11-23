mod common;

use clearhead_cli::filesystem::sidecar::append_note;
use clearhead_core::workspace::sidecar::Note;
use predicates::prelude::*;
use serde_json::{Value, json};

const ACTION: &str = "019f733d-45b2-7f21-bcad-5610887b7230";
const CHARTER: &str = "019f733d-45c2-7dd2-91dc-8631f33c6b77";
const HABIT: &str = "019f733d-4600-7000-8000-000000000003";

fn note(text: &str, at: &str) -> Note {
    serde_json::from_value(json!({"at": at, "by": "agent:test", "text": text})).unwrap()
}

fn jot(env: &common::TestEnv, anchor: &str, id: &str, text: &str, at: &str) {
    append_note(
        &env.data_dir.join(anchor),
        id.parse().unwrap(),
        note(text, at),
    )
    .unwrap();
}

fn notes_json(env: &common::TestEnv, query: &str) -> Value {
    let output = env
        .command()
        .args(["show", "notes", query, "--json"])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).unwrap()
}

#[test]
fn closed_action_notes_print_newest_last_as_text_and_json() {
    let env = common::TestEnv::new();
    env.write_actions("work.completed.actions", &format!("[x] Done #{ACTION}\n"));
    jot(
        &env,
        "charters/work.completed.actions",
        ACTION,
        "second",
        "2026-11-02T00:00:00Z",
    );
    jot(
        &env,
        "charters/work.completed.actions",
        ACTION,
        "first\nmore",
        "2026-11-01T00:00:00Z",
    );

    env.command()
        .args(["show", "notes", &ACTION[..8]])
        .assert()
        .success()
        .stdout(
            "2026-11-01T00:00:00Z  agent:test\n  first\n  more\n\n\
             2026-11-02T00:00:00Z  agent:test\n  second\n",
        );
    assert_eq!(
        notes_json(&env, ACTION),
        json!([
            {"at": "2026-11-01T00:00:00Z", "by": "agent:test", "text": "first\nmore"},
            {"at": "2026-11-02T00:00:00Z", "by": "agent:test", "text": "second"}
        ])
    );
}

#[test]
fn archived_charter_action_and_habit_notes_resolve_by_id_once_each() {
    let env = common::TestEnv::new();
    env.write_text(
        "charters/done.md",
        &format!("---\nid: {CHARTER}\nalias: done\nstate: Closed\n---\n# Done\n"),
    );
    env.write_actions("done.actions", &format!("[x] Finished #{ACTION}\n"));
    env.write_actions(
        "done.habits.actions",
        &format!("[ ] Daily @2026-11-01 R:FREQ=DAILY #{HABIT}\n"),
    );
    for id in [ACTION, CHARTER, HABIT] {
        jot(
            &env,
            "charters/done.actions",
            id,
            id,
            "2026-11-01T00:00:00Z",
        );
    }
    env.command()
        .args(["archive", "charter", "done"])
        .assert()
        .success();

    for id in [ACTION, CHARTER, HABIT] {
        let notes = notes_json(&env, id);
        assert_eq!(notes.as_array().unwrap().len(), 1, "{id}");
        assert_eq!(notes[0]["text"], id);
    }
}

#[test]
fn live_entities_resolve_by_alias_including_habits_and_charters() {
    let env = common::TestEnv::new();
    env.write_text(
        "charters/work.md",
        &format!("---\nid: {CHARTER}\nalias: work\nstate: Active\n---\n# Work\n"),
    );
    env.write_actions("work.actions", &format!("[ ] Quiet =quiet #{ACTION}\n"));
    env.write_actions(
        "work.habits.actions",
        &format!("[ ] Daily =daily @2026-11-01 R:FREQ=DAILY #{HABIT}\n"),
    );
    jot(
        &env,
        "charters/work.actions",
        HABIT,
        "habit note",
        "2026-11-01T00:00:00Z",
    );
    jot(
        &env,
        "charters/work.actions",
        CHARTER,
        "charter note",
        "2026-11-01T00:00:00Z",
    );

    assert_eq!(notes_json(&env, "daily")[0]["text"], "habit note");
    assert_eq!(notes_json(&env, "work")[0]["text"], "charter note");
    assert_eq!(notes_json(&env, "quiet"), json!([]));
    env.command()
        .args(["show", "notes", "quiet"])
        .assert()
        .success()
        .stdout(format!("No notes for {ACTION}\n"));
}

#[test]
fn an_unknown_entity_fails() {
    let env = common::TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Open #{ACTION}\n"));
    env.command()
        .args(["show", "notes", "nothing-here"])
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "No entity found matching 'nothing-here'",
        ));
}

#[test]
fn a_corrupt_sidecar_is_reported_not_silently_skipped() {
    let env = common::TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Open #{ACTION}\n"));
    jot(
        &env,
        "charters/work.actions",
        ACTION,
        "kept",
        "2026-11-01T00:00:00Z",
    );
    env.write_text("charters/.broken.json", "{ not json");
    env.command()
        .args(["show", "notes", ACTION])
        .assert()
        .success()
        .stdout(predicate::str::contains("kept"))
        .stderr(predicate::str::contains("warning: skipped .broken.json"));
}
