//! The `tree` and `graph` families, in-process (`sparql` feature). Both source
//! containment from Core's canonical upward `part_of`: the tree nests actions
//! under their charter and parent action; the graph re-expresses that as
//! hierarchical `has_part`. Asserted against the CLI's own contract (no graphd),
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

    // The charter is the single root; the top-level action nests under it, and
    // the sub-action under the top-level action — the full upward part_of chain.
    let charter = roots
        .iter()
        .find(|n| n["kind"] == "charter")
        .expect("charter root present");
    assert_eq!(charter["name"], "work");
    let container = &charter["children"][0];
    assert_eq!(container["name"], "Container");
    assert_eq!(container["children"][0]["name"], "Child");
}

#[test]
fn graph_reconstructs_hierarchical_containment() {
    let env = seed();
    // Turtle CONSTRUCT output re-expresses part_of as has_part (BFO_0000051):
    // the charter directly contains the top-level action, which contains the
    // sub-action — hierarchical, not flat.
    let turtle = stdout(
        &env,
        &["query", "graph", "dependencies", "--format", "turtle"],
    );
    assert!(
        turtle.contains("BFO_0000051"),
        "containment edge present: {turtle}"
    );
    assert!(turtle.contains("Container"), "{turtle}");
    assert!(turtle.contains("Child"), "{turtle}");

    // DOT renders those as two distinct `contains` edges (charter->Container,
    // Container->Child), proving the hierarchy rather than a flat fan-out.
    let dot = stdout(&env, &["query", "graph", "dependencies", "--format", "dot"]);
    let contains = dot.matches("label=\"contains\"").count();
    assert_eq!(contains, 2, "two hierarchical containment edges: {dot}");
}
