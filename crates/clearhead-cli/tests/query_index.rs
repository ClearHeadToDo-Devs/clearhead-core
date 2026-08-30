//! The `index` family, in-process (`sparql` feature): resolve a built-in or
//! drop-in index view, validate/frame its rows into the `@context` + `@graph`
//! document, and render the destination-aware formats. These assert the CLI's
//! own contract directly (no graphd), so they survive graphd's retirement.

#![cfg(feature = "sparql")]

mod common;
use common::TestEnv;
use serde_json::Value;

const A: &str = "019f733d-4600-7000-8000-0000000000a1";
const B: &str = "019f733d-4600-7000-8000-0000000000b2";
const C: &str = "019f733d-4600-7000-8000-0000000000c3";
const D: &str = "019f733d-4600-7000-8000-0000000000d4";
const WS: &str = "00000000-0000-0000-0000-0000000000cc";

fn seed(actions: &str) -> TestEnv {
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"testws"}}"#),
    );
    env.write_text(
        "charters/work.md",
        "---\nalias: work\nstate: Active\n---\n# Work\n",
    );
    env.write_actions("work.actions", actions);
    env
}

fn run_index(env: &TestEnv, view: &str, fmt: &str) -> Vec<u8> {
    let output = env
        .std_command()
        .args(["query", "index", view, "--format", fmt])
        .output()
        .expect("run clearhead query index");
    assert!(
        output.status.success(),
        "index {view} --format {fmt} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

#[test]
fn default_view_frames_context_and_graph() {
    let env = seed(&format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));
    let doc: Value = serde_json::from_slice(&run_index(&env, "default", "jsonld")).expect("jsonld");

    assert!(
        doc.get("@context").is_some(),
        "index jsonld carries @context"
    );
    let graph = doc["@graph"].as_array().expect("@graph array");
    assert_eq!(graph.len(), 2);
    // Identity is addressable by the plain `id` alias; locator line is numeric.
    assert!(graph[0]["id"].as_str().unwrap().starts_with("urn:uuid:"));
    assert!(graph[0]["source_line"].is_u64());
}

#[test]
fn ids_format_emits_one_urn_per_line() {
    let env = seed(&format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));
    let out = String::from_utf8(run_index(&env, "default", "ids")).unwrap();
    let ids: Vec<&str> = out.lines().collect();
    assert_eq!(ids.len(), 2);
    assert!(ids.iter().all(|id| id.starts_with("urn:uuid:")), "{ids:?}");
}

#[test]
fn top_level_action_has_no_charter_parent() {
    // Core's canonical projection links a top-level action to its charter, but
    // the index views' denormalized `parent` edge means a parent *action*
    // (matching tree/work-map and the "unset for root actions" contract). A
    // flat action must therefore surface with no `parent`/`parent_name`.
    let env = seed(&format!("[ ] Solo task #{A}\n"));
    let doc: Value = serde_json::from_slice(&run_index(&env, "unscheduled", "json")).expect("json");
    let nodes = doc.as_array().expect("index json is the @graph array");
    let solo = nodes
        .iter()
        .find(|n| n["name"] == "Solo task")
        .expect("solo task present in the unscheduled view");
    assert!(
        solo.get("parent").is_none(),
        "charter must not be a parent: {solo}"
    );
    assert!(solo.get("parent_name").is_none(), "{solo}");
}

#[test]
fn unscheduled_includes_in_progress_leaves_and_excludes_containers() {
    let env = seed(&format!(
        "[-] Continuing #{A}\n[ ] Container #{B}\n> [ ] Leaf #{C}\n[=] Blocked #{D}\n"
    ));
    let doc: Value = serde_json::from_slice(&run_index(&env, "unscheduled", "json")).expect("json");
    let nodes = doc.as_array().expect("index json is the row array");
    let names: Vec<&str> = nodes
        .iter()
        .map(|node| node["name"].as_str().expect("name string"))
        .collect();

    assert!(names.contains(&"Continuing"), "InProgress work: {nodes:?}");
    assert!(names.contains(&"Leaf"), "lowest open child: {nodes:?}");
    assert_eq!(
        names.first(),
        Some(&"Continuing"),
        "InProgress work ranks before ready NotStarted work: {nodes:?}"
    );
    assert!(!names.contains(&"Container"), "container leaked: {nodes:?}");
    assert!(
        !names.contains(&"Blocked"),
        "blocked work leaked: {nodes:?}"
    );
}

#[test]
fn engagement_views_require_active_charter_ancestry() {
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"testws"}}"#),
    );
    env.write_text(
        "charters/root.md",
        "---\nalias: root\nstate: New\n---\n# Root\n",
    );
    env.write_actions("root.actions", "");
    env.write_text(
        "charters/child.md",
        "---\nalias: child\nparent: root\nstate: Active\n---\n# Child\n",
    );
    env.write_actions(
        "child.actions",
        &format!("[ ] Stranded undated #{A}\n[ ] Stranded dated @2000-01-01T00:00 #{B}\n"),
    );

    for view in ["unscheduled", "agenda"] {
        let Ok(doc) = serde_json::from_slice::<Value>(&run_index(&env, view, "json")) else {
            panic!("{view} should emit JSON");
        };
        let Some(nodes) = doc.as_array() else {
            panic!("{view} JSON should be the row array");
        };
        assert!(
            nodes.is_empty(),
            "{view} admitted inactive ancestry: {nodes:?}"
        );
    }
}
