mod common;
use common::TestEnv;

const PREDECESSOR: &str = "01900000-0000-7000-8000-000000000201";
const SUCCESSOR: &str = "01900000-0000-7000-8000-000000000202";

fn dependency_workspace() -> TestEnv {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        &format!("[ ] prepare =prepare #{PREDECESSOR}\n[ ] ship <prepare #{SUCCESSOR}\n"),
    );
    env
}

#[test]
fn dependencies_construct_emits_standard_turtle_edges() {
    let env = dependency_workspace();
    let output = env
        .command()
        .args(["query", "graph", "dependencies", "--format", "turtle"])
        .output()
        .expect("run dependency graph");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let turtle = String::from_utf8(output.stdout).expect("utf8 turtle");
    assert!(
        turtle.contains(&format!("urn:uuid:{SUCCESSOR}")),
        "{turtle}"
    );
    assert!(
        turtle.contains(&format!("urn:uuid:{PREDECESSOR}")),
        "{turtle}"
    );
    assert!(
        turtle.contains("ont00001775"),
        "predecessor predicate missing: {turtle}"
    );
}

#[test]
fn dependencies_construct_emits_jsonld_for_machine_default() {
    let env = dependency_workspace();
    let output = env
        .command()
        .args(["query", "graph", "dependencies"])
        .output()
        .expect("run dependency graph");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let json: serde_json::Value = serde_json::from_slice(&output.stdout).expect("valid JSON-LD");
    assert!(
        json.is_array() || json.is_object(),
        "JSON-LD document: {json}"
    );
    assert!(String::from_utf8_lossy(&output.stdout).contains("ont00001775"));
}

#[test]
fn dependencies_construct_emits_dot_with_visual_flow() {
    let env = dependency_workspace();
    let output = env
        .command()
        .args(["query", "graph", "dependencies", "--format", "dot"])
        .output()
        .expect("run dependency graph as DOT");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let dot = String::from_utf8(output.stdout).expect("utf8 DOT");
    assert!(dot.starts_with("digraph"), "{dot}");
    assert!(dot.contains("prepare"), "{dot}");
    assert!(dot.contains("ship"), "{dot}");
    assert!(
        dot.contains("0 -> 1"),
        "dependency must flow prepare -> ship: {dot}"
    );
    assert!(
        dot.contains("penwidth=\"2\""),
        "dependency edge styling missing: {dot}"
    );
}

#[test]
fn graph_family_rejects_select_queries() {
    let env = dependency_workspace();
    env.write_text(
        ".clearhead/queries/graph/not-a-graph.sparql",
        "SELECT ?s WHERE { ?s ?p ?o }",
    );
    env.command()
        .args(["query", "graph", "not-a-graph"])
        .assert()
        .failure()
        .stderr(predicates::str::contains("requires CONSTRUCT"));
}

#[test]
fn dependencies_query_is_listed_and_inspectable() {
    let env = dependency_workspace();
    env.command()
        .args(["query", "list"])
        .assert()
        .success()
        .stdout(predicates::str::contains("dependencies"))
        .stdout(predicates::str::contains("graph"));
    env.command()
        .args(["query", "show", "dependencies"])
        .assert()
        .success()
        .stdout(predicates::str::contains("CONSTRUCT"))
        .stdout(predicates::str::contains("ont00001775"));
}
