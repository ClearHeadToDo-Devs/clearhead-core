//! The `tree` and `graph` families, in-process (`sparql` feature). Both source
//! containment from the application graph's upward `app:partOf`: the tree nests
//! actions under their charter and parent action; the graph keeps the edge and
//! its DOT projection draws it downward as `contains`. Asserted against the CLI's own contract (no graphd),
//! so they survive graphd's retirement.

#![cfg(feature = "sparql")]

mod common;
use common::TestEnv;
use serde_json::Value;

const A: &str = "019f733d-4600-7000-8000-0000000000a1";
const B: &str = "019f733d-4600-7000-8000-0000000000b2";
const WS: &str = "00000000-0000-0000-0000-0000000000cc";

/// A charter "work" with a top-level action containing one sub-action.
fn seed() -> TestEnv {
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"testws"}}"#),
    );
    env.write_actions(
        "work.actions",
        &format!("[ ] Container #{A}\n    >[ ] Child #{B}\n"),
    );
    env
}

fn stdout(env: &TestEnv, args: &[&str]) -> String {
    let output = env
        .std_command()
        .args(args)
        .output()
        .expect("run clearhead");
    assert!(
        output.status.success(),
        "{args:?} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("utf-8 stdout")
}

#[test]
fn tree_nests_actions_under_their_charter() {
    let env = seed();
    let doc: Value = serde_json::from_str(&stdout(
        &env,
        &["query", "tree", "work-map", "--format", "json"],
    ))
    .expect("tree json");
    let roots = doc.as_array().expect("tree is an array of roots");

    // The workspace root heads the tree; the charter nests under it, the
    // top-level action under the charter, and the sub-action under that — the
    // full upward part_of chain.
    let root = roots
        .iter()
        .find(|n| n["kind"] == "charter")
        .expect("root charter present");
    assert_eq!(root["name"], "testws");
    let charter = root["children"]
        .as_array()
        .expect("root children")
        .iter()
        .find(|n| n["name"] == "work")
        .expect("work nests under the workspace root");
    assert_eq!(charter["status"], "New", "omitted source state is explicit");
    let container = &charter["children"][0];
    assert_eq!(container["name"], "Container");
    assert_eq!(container["children"][0]["name"], "Child");
}

#[test]
fn graph_reconstructs_hierarchical_containment() {
    let env = seed();
    // The CONSTRUCT keeps the graph's upward app:partOf: the sub-action is part
    // of its parent action, which is part of the charter — hierarchical, not
    // flat (the child is not directly part of the charter).
    let triples = stdout(
        &env,
        &["query", "graph", "dependencies", "--format", "turtle"],
    );
    let child_in_container =
        format!("<urn:uuid:{B}> <https://clearhead.us/vocab/app/v1#partOf> <urn:uuid:{A}>");
    assert!(triples.contains(&child_in_container), "{triples}");
    assert_eq!(
        triples.matches("#partOf>").count(),
        2,
        "one part-of per part: {triples}"
    );

    // DOT renders those as two distinct `contains` edges (charter->Container,
    // Container->Child), proving the hierarchy rather than a flat fan-out.
    let dot = stdout(&env, &["query", "graph", "dependencies", "--format", "dot"]);
    let contains = dot.matches("label=\"contains\"").count();
    assert_eq!(contains, 2, "two hierarchical containment edges: {dot}");
}
