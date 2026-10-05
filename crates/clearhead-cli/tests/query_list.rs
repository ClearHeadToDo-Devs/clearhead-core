//! `query list` follows the TTY output rule: a JSON array when piped, the
//! `NAME`/`TYPE`/`SOURCE` table at a terminal. These tests only exercise the
//! piped path — assert_cmd always pipes stdout.
//!
//! This test compiles away entirely in the minimal `--no-default-features`
//! build, which has no query engine.

#![cfg(feature = "sparql")]

mod common;
use common::TestEnv;

#[test]
fn query_list_emits_json_array_when_piped() {
    let env = TestEnv::new();

    let assert = env.command().args(["query", "list"]).assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let value: serde_json::Value = serde_json::from_str(&stdout).expect("query list emits JSON");

    let entries = value.as_array().expect("a JSON array of entries");
    assert!(!entries.is_empty());

    let flat = entries
        .iter()
        .find(|e| e["name"] == "open-actions")
        .expect("a flat built-in query is listed");
    assert!(
        flat["type"].is_null(),
        "the flat namespace's type is null, not the table's dash: {flat}"
    );
    assert_eq!(flat["source"], "built-in");

    let index = entries
        .iter()
        .find(|e| e["name"] == "unscheduled")
        .expect("the unscheduled index view is listed");
    assert_eq!(index["type"], "index");
    assert_eq!(index["source"], "built-in");
}

#[test]
fn query_list_json_reflects_project_dropin_shadowing() {
    let env = TestEnv::new();
    env.write_text(
        ".clearhead/queries/open-actions.sparql",
        "PREFIX app: <https://clearhead.us/vocab/app/v1#>\nSELECT ?x WHERE { ?x a app:Action }\n",
    );

    let assert = env.command().args(["query", "list"]).assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let value: serde_json::Value = serde_json::from_str(&stdout).unwrap();

    let entries = value.as_array().unwrap();
    let shadowed = entries
        .iter()
        .find(|e| e["name"] == "open-actions")
        .expect("the shadowed query is still listed");
    assert_eq!(shadowed["source"], "project");
}
