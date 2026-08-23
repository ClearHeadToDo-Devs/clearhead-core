//! Opt-in conformance: the CLI's real index query output validates against the
//! specification's published `index_query_result` schema.
//!
//! When graphd was retired the index family moved into the CLI's in-process
//! `sparql` layer, so schema conformance for those bytes is now proven here, at
//! the new producer boundary. `--format json` emits the flat array of rows the
//! schema constrains (row *shape*, not membership). Runs only with
//! `--features spec-conformance`; a default `cargo test` stays independent of a
//! `specifications` checkout.
#![cfg(feature = "spec-conformance")]

mod common;
use common::TestEnv;
use std::path::PathBuf;

fn spec_dir() -> PathBuf {
    std::env::var("CLEARHEAD_SPEC_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../specifications")
        })
}

fn index_validator() -> jsonschema::JSONSchema {
    let path = spec_dir().join("schemas/index_query_result.schema.json");
    assert!(
        path.exists(),
        "spec schema not found at {path:?}; set CLEARHEAD_SPEC_DIR to a specifications checkout"
    );
    let schema: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
    jsonschema::JSONSchema::compile(&schema).expect("index schema compiles")
}

/// Run `query index <view> --format json` against the seeded workspace and
/// return the parsed JSON array of rows.
fn index_rows(env: &TestEnv, view: &str) -> Vec<serde_json::Value> {
    let output = env
        .std_command()
        .args(["query", "index", view, "--format", "json"])
        .output()
        .expect("failed to run clearhead query index");
    assert!(
        output.status.success(),
        "index {view} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout)
        .unwrap_or_else(|e| panic!("index {view} output is not JSON: {e}"))
}

/// Seed one workspace that exercises every optional column of the contract: an
/// undated parent/child pair (parent, parent_name, priority) surfaced by the
/// unscheduled view, and a past-dated root carrying both a scheduled and a due
/// datetime (scheduled_at, due_date, priority) surfaced by the agenda view.
fn seeded_env() -> TestEnv {
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        r#"{"workspace_id":"00000000-0000-0000-0000-0000000000cc","workspace_name":"testws"}"#,
    );
    env.write_actions(
        "next.actions",
        concat!(
            "[ ] parent thing !2 #01900000-0000-7000-8000-000000000001\n",
            "> [ ] child thing !3 #01900000-0000-7000-8000-000000000002\n",
            "[ ] overdue thing !1 @2000-01-01T00:00 :2000-01-02T00:00 #01900000-0000-7000-8000-000000000003\n",
        ),
    );
    env
}

#[test]
fn index_output_conforms_to_spec_schema() {
    let validator = index_validator();
    let env = seeded_env();

    for view in ["default", "unscheduled", "agenda", "weekly"] {
        let rows = index_rows(&env, view);
        let value = serde_json::Value::Array(rows);
        if let Err(errors) = validator.validate(&value) {
            let joined: Vec<String> = errors
                .map(|e| format!("{} at {}", e, e.instance_path))
                .collect();
            panic!(
                "index {view} output violates the schema:\n{}",
                joined.join("\n")
            );
        }
    }
}

#[test]
fn validator_rejects_a_row_missing_required_id() {
    // Guards against a hollow pass: if the validator silently ignored the
    // contract, this malformed row (no `id`, plus an unknown column) would slip
    // through and the conformance test above would prove nothing.
    let validator = index_validator();
    let bad = serde_json::json!([{
        "name": "no id here",
        "status": "NotStarted",
        "source_file": "x.actions",
        "source_line": 1,
        "charter_root": "/tmp/.clearhead/charters",
        "surprise": true
    }]);
    assert!(
        validator.validate(&bad).is_err(),
        "validator must reject a row missing `id` / carrying unknown fields"
    );
}

#[test]
fn seeded_views_exercise_the_optional_columns() {
    let env = seeded_env();

    // unscheduled surfaces the lowest open child, denormalizing parent + name.
    let unscheduled = index_rows(&env, "unscheduled");
    let child = unscheduled
        .iter()
        .find(|r| r["name"] == "child thing")
        .expect("child thing should appear in unscheduled");
    assert_eq!(child["parent_name"], "parent thing");
    assert!(child.get("parent").is_some(), "parent id denormalized");
    assert!(child.get("priority").is_some(), "priority projected");

    // agenda surfaces the overdue root, projecting both date columns.
    let agenda = index_rows(&env, "agenda");
    let overdue = agenda
        .iter()
        .find(|r| r["name"] == "overdue thing")
        .expect("overdue thing should appear in agenda");
    assert!(
        overdue.get("scheduled_at").is_some(),
        "scheduled_at projected"
    );
    assert!(overdue.get("due_date").is_some(), "due_date projected");
}
