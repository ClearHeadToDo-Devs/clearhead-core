//! `jot`: append a note to any entity's sidecar (workspace.md Notes).

mod common;

use predicates::prelude::*;
use serde_json::{Value, json};

const ACTION: &str = "019f733d-45b2-7f21-bcad-5610887b7230";
const CHARTER: &str = "019f733d-45c2-7dd2-91dc-8631f33c6b77";
const HABIT: &str = "019f733d-4600-7000-8000-000000000003";

/// A `clearhead` command with a known login and no inherited name override.
fn clearhead(env: &common::TestEnv) -> assert_cmd::Command {
    let mut cmd = env.command();
    cmd.env_remove("CLEARHEAD_NAME").env("USER", "login");
    cmd
}

fn jot(env: &common::TestEnv, args: &[&str]) {
    clearhead(env).arg("jot").args(args).assert().success();
}

fn notes(env: &common::TestEnv, query: &str) -> Value {
    let output = clearhead(env)
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

fn work(env: &common::TestEnv) {
    env.write_text(
        "charters/work.md",
        &format!("---\nid: {CHARTER}\nalias: work\nstate: Active\n---\n# Work Things\n"),
    );
    env.write_actions("work.actions", &format!("[ ] Quiet =quiet #{ACTION}\n"));
    env.write_actions(
        "work.habits.actions",
        &format!("[ ] Daily =daily @2026-11-01 R:FREQ=DAILY #{HABIT}\n"),
    );
}

#[test]
fn jot_reaches_an_action_by_id_prefix_and_alias() {
    let env = common::TestEnv::new();
    work(&env);
    jot(&env, &[ACTION, "by id"]);
    jot(&env, &[&ACTION[..13], "by prefix"]);
    jot(&env, &["quiet", "by alias"]);

    let texts: Vec<Value> = notes(&env, ACTION)
        .as_array()
        .unwrap()
        .iter()
        .map(|note| note["text"].clone())
        .collect();
    assert_eq!(
        texts,
        vec![json!("by id"), json!("by prefix"), json!("by alias")]
    );
}

#[test]
fn jot_reaches_a_habit_and_a_charter() {
    let env = common::TestEnv::new();
    work(&env);
    jot(&env, &["daily", "habit note"]);
    jot(&env, &["work", "charter note"]);

    assert_eq!(notes(&env, HABIT)[0]["text"], "habit note");
    assert_eq!(notes(&env, CHARTER)[0]["text"], "charter note");
}

#[test]
fn jot_charter_matches_the_title() {
    let env = common::TestEnv::new();
    work(&env);
    jot(&env, &["charter", "work things", "found by title"]);

    assert_eq!(notes(&env, CHARTER)[0]["text"], "found by title");
}

#[test]
fn jot_reaches_closed_and_archived_entities() {
    let env = common::TestEnv::new();
    env.write_text(
        "charters/done.md",
        &format!("---\nid: {CHARTER}\nalias: done\nstate: Closed\n---\n# Done\n"),
    );
    env.write_actions(
        "done.completed.actions",
        &format!("[x] Finished #{ACTION}\n"),
    );
    jot(&env, &[ACTION, "while closed"]);
    clearhead(&env)
        .args(["archive", "charter", "done"])
        .assert()
        .success();
    jot(&env, &[ACTION, "after archive"]);
    jot(&env, &[CHARTER, "charter after archive"]);

    let texts: Vec<Value> = notes(&env, ACTION)
        .as_array()
        .unwrap()
        .iter()
        .map(|note| note["text"].clone())
        .collect();
    assert_eq!(texts, vec![json!("while closed"), json!("after archive")]);
    assert_eq!(notes(&env, CHARTER)[0]["text"], "charter after archive");
}

#[test]
fn jot_never_touches_the_charter_document_or_the_actions_line() {
    let env = common::TestEnv::new();
    work(&env);
    let md = std::fs::read(env.data_dir.join("charters/work.md")).unwrap();
    let actions = std::fs::read(env.data_dir.join("charters/work.actions")).unwrap();
    jot(&env, &["quiet", "note"]);
    jot(&env, &["work", "note"]);

    assert_eq!(
        std::fs::read(env.data_dir.join("charters/work.md")).unwrap(),
        md
    );
    assert_eq!(
        std::fs::read(env.data_dir.join("charters/work.actions")).unwrap(),
        actions
    );
}

#[test]
fn by_resolves_flag_then_env_then_config_then_login() {
    let env = common::TestEnv::new();
    work(&env);
    jot(&env, &["quiet", "login"]);
    env.write_config(r#"{"name":"configured"}"#);
    jot(&env, &["quiet", "config"]);
    clearhead(&env)
        .env("CLEARHEAD_NAME", "from-env")
        .args(["jot", "quiet", "env"])
        .assert()
        .success();
    clearhead(&env)
        .env("CLEARHEAD_NAME", "from-env")
        .args(["jot", "quiet", "flag", "--by", "from-flag"])
        .assert()
        .success();

    let by: Vec<Value> = notes(&env, ACTION)
        .as_array()
        .unwrap()
        .iter()
        .map(|note| note["by"].clone())
        .collect();
    assert_eq!(
        by,
        vec![
            json!("login"),
            json!("configured"),
            json!("from-env"),
            json!("from-flag")
        ]
    );
}

#[test]
fn at_backdates_a_note_and_keeps_the_sidecar_in_time_order() {
    let env = common::TestEnv::new();
    work(&env);
    jot(&env, &["work", "now"]);
    jot(&env, &["work", "a date", "--at", "2026-09-17"]);
    jot(&env, &["work", "a time", "--at", "2026-09-17T23:28-07:00"]);

    let sidecar: Value =
        serde_json::from_slice(&std::fs::read(env.data_dir.join("charters/.work.json")).unwrap())
            .unwrap();
    let stored: Vec<&str> = sidecar["notes"][CHARTER]
        .as_array()
        .unwrap()
        .iter()
        .map(|note| note["at"].as_str().unwrap())
        .collect();
    assert_eq!(
        &stored[..2],
        ["2026-09-17T00:00:00Z", "2026-09-17T23:28:00-07:00"]
    );
    assert_eq!(notes(&env, CHARTER)[2]["text"], "now");
}

#[test]
fn at_refuses_a_time_without_an_offset() {
    let env = common::TestEnv::new();
    work(&env);
    clearhead(&env)
        .args(["jot", "work", "x", "--at", "2026-09-17T23:28"])
        .assert()
        .failure();
}

#[test]
fn no_by_records_no_author() {
    let env = common::TestEnv::new();
    work(&env);
    jot(&env, &["work", "unknown writer", "--no-by"]);
    assert!(notes(&env, CHARTER)[0].get("by").is_none());
}

#[test]
fn an_unknown_reference_fails_and_writes_nothing() {
    let env = common::TestEnv::new();
    work(&env);
    clearhead(&env)
        .args(["jot", "nothing-here", "text"])
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "No entity found matching 'nothing-here'",
        ));
    assert!(!env.data_dir.join("charters/.work.json").exists());
}

#[test]
fn dry_run_writes_nothing() {
    let env = common::TestEnv::new();
    work(&env);
    clearhead(&env)
        .args(["jot", "quiet", "preview me", "--dry-run"])
        .assert()
        .success()
        .stdout(predicate::str::contains("preview me"));
    assert_eq!(notes(&env, ACTION), json!([]));
}

#[test]
fn doctor_finds_every_jotted_note_where_it_belongs() {
    let env = common::TestEnv::new();
    work(&env);
    env.write_actions(
        "work.completed.actions",
        "[x] Closed #019f733d-4700-7000-8000-000000000004\n",
    );
    for reference in [
        "quiet",
        "daily",
        "work",
        "019f733d-4700-7000-8000-000000000004",
    ] {
        jot(&env, &[reference, "note"]);
    }
    clearhead(&env)
        .args(["doctor"])
        .assert()
        .stdout(predicate::str::contains("misplaced-notes").not())
        .stdout(predicate::str::contains("orphaned-notes").not());
}

#[test]
fn jot_refuses_an_entity_without_a_durable_id_and_writes_nothing() {
    let env = common::TestEnv::new();
    env.write_text("charters/loose.md", "---\nalias: loose\n---\n# Loose\n");
    env.write_actions("loose.actions", "[ ] No id yet =unwritten\n");
    let before = std::fs::read(env.data_dir.join("charters/loose.md")).unwrap();

    for args in [
        &["loose", "note"][..],
        &["unwritten", "note"],
        &["charter", "Loose", "note"],
    ] {
        clearhead(&env)
            .arg("jot")
            .args(args)
            .assert()
            .failure()
            .stderr(predicate::str::contains("no durable id"));
    }
    assert_eq!(
        std::fs::read(env.data_dir.join("charters/loose.md")).unwrap(),
        before
    );
    assert!(!env.data_dir.join("charters/.loose.json").exists());
}

#[test]
fn jot_stamps_an_rfc3339_time_with_an_offset() {
    let env = common::TestEnv::new();
    work(&env);
    jot(&env, &["quiet", "stamped"]);

    let at = notes(&env, ACTION)[0]["at"].as_str().unwrap().to_string();
    chrono::DateTime::parse_from_rfc3339(&at).expect("an RFC 3339 instant with an offset");
}
