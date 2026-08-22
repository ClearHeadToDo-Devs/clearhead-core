//! `clearhead export workspace`: the whole-workspace RDF publication path —
//! always available (no query engine required), deterministic bytes, dataset
//! formats preserving each workspace's stable named graph.
//!
//! These tests run in both build profiles: export is not part of the optional
//! `sparql` feature.

mod common;
use common::TestEnv;

const A: &str = "019f733d-4600-7000-8000-000000000001";
const WS: &str = "00000000-0000-0000-0000-0000000000aa";
const WS_GRAPH: &str = "urn:clearhead:workspace:00000000-0000-0000-0000-0000000000aa";

fn seed() -> TestEnv {
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"testws"}}"#),
    );
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n"));
    env
}

fn export(env: &TestEnv, extra: &[&str]) -> std::process::Output {
    let mut args = vec!["export", "workspace"];
    args.extend_from_slice(extra);
    let output = env
        .std_command()
        .args(&args)
        .output()
        .expect("run clearhead export workspace");
    assert!(
        output.status.success(),
        "export failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

#[test]
fn trig_is_the_default_and_preserves_the_named_graph() {
    let env = seed();
    let output = export(&env, &[]);
    let text = String::from_utf8(output.stdout).expect("utf8 trig");
    assert!(
        text.contains(WS_GRAPH),
        "TriG names the workspace graph: {text}"
    );
    assert!(
        text.contains(&format!("<urn:uuid:{A}>")),
        "the action resource is present: {text}"
    );
    assert!(
        text.contains("@prefix actions:"),
        "vocabulary prefixes are declared: {text}"
    );
    // The workspace-snapshot layer is published too (same dataset the query
    // layer evaluates).
    assert!(
        text.contains("ws:hasSourceLine") || text.contains("hasSourceLine"),
        "snapshot provenance is part of the export: {text}"
    );
}

#[test]
fn nquads_carries_the_graph_on_every_statement() {
    let env = seed();
    let output = export(&env, &["--format", "nquads"]);
    let text = String::from_utf8(output.stdout).expect("utf8 nquads");
    assert!(!text.trim().is_empty(), "dataset is non-empty");
    assert!(
        text.lines()
            .filter(|l| !l.trim().is_empty())
            .all(|l| l.contains(WS_GRAPH)),
        "every N-Quads statement names the workspace graph: {text}"
    );
}

#[test]
fn jsonld_is_a_json_document_referencing_the_named_graph() {
    let env = seed();
    let output = export(&env, &["--format", "jsonld"]);
    let doc: serde_json::Value = serde_json::from_slice(&output.stdout).expect("valid JSON");
    assert!(doc.get("@context").is_some(), "JSON-LD has an @context");
    let text = String::from_utf8(output.stdout.clone()).expect("utf8");
    // The graph survives as JSON-LD @id on the graph object.
    assert!(
        text.contains(WS_GRAPH),
        "JSON-LD references the workspace graph: {text}"
    );
    let _ = doc;
}

#[test]
fn turtle_is_graph_only_by_contract() {
    let env = seed();
    let output = export(&env, &["--format", "turtle"]);
    let text = String::from_utf8(output.stdout).expect("utf8 turtle");
    // Turtle is a graph syntax: no TriG graph blocks headed by the workspace
    // graph label. (The workspace *entity* IRI still appears — as data —
    // because the ws:Workspace node names the same IRI; Core's own tests cover
    // graph-label elision.)
    assert!(
        !text.contains("GRAPH ") && !text.contains(" graph "),
        "Turtle emits no dataset graph blocks: {text}"
    );
    assert!(
        text.contains("a actions:Action"),
        "the triples survive with prefixed vocabulary: {text}"
    );
    assert!(
        text.contains("ws:hasSourceLine"),
        "the ws: prefix compacts snapshot vocabulary: {text}"
    );
}

#[test]
fn export_is_byte_deterministic() {
    let env = seed();
    for format in ["trig", "nquads", "jsonld", "turtle"] {
        let first = export(&env, &["--format", format]);
        let second = export(&env, &["--format", format]);
        assert_eq!(
            first.stdout, second.stdout,
            "{format} export must be byte-deterministic"
        );
    }
}

#[test]
fn output_file_matches_stdout_and_stdout_stays_empty() {
    let env = seed();
    let path = env.work_dir.join("dataset.trig");
    let output = export(&env, &["--output", path.to_str().unwrap()]);
    assert!(
        output.stdout.is_empty(),
        "--output redirects the dataset to the file"
    );
    let file_bytes = std::fs::read(&path).expect("read exported file");
    let stdout_bytes = export(&env, &[]).stdout;
    assert_eq!(
        file_bytes, stdout_bytes,
        "file and stdout carry the same bytes"
    );
}

#[test]
fn empty_workspace_with_identity_exports_a_deterministic_snapshot_node() {
    // Zero charters/actions: the dataset is just the workspace-snapshot node.
    // With durable manifest identity the bytes are stable across runs.
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"empty"}}"#),
    );
    let first = export(&env, &["--format", "nquads"]);
    let second = export(&env, &["--format", "nquads"]);
    assert_eq!(first.stdout, second.stdout, "deterministic bytes");
    let text = String::from_utf8(first.stdout).expect("utf8");
    assert!(
        text.contains(WS_GRAPH),
        "the workspace node is exported under its stable graph: {text}"
    );
    assert!(
        text.contains("vocab/workspace/v1#Workspace"),
        "the ws:Workspace node survives an empty domain: {text}"
    );
    assert!(
        !text.contains("vocab/actions/v4#Action"),
        "no actions, no Action statements: {text}"
    );
}

#[test]
fn a_closed_downstream_pipe_is_not_an_error() {
    use std::process::Stdio;
    let env = seed();
    let mut cmd = env.std_command();
    cmd.args(["export", "workspace"]).stdout(Stdio::piped());
    let mut child = cmd.spawn().expect("spawn export");
    drop(child.stdout.take());
    let status = child.wait().expect("wait");
    assert!(status.success(), "broken pipe must exit cleanly: {status}");
}

#[test]
fn multi_workspace_export_places_each_workspace_in_its_own_graph() {
    let env = seed();
    // A second, additional workspace with its own durable identity.
    let second = env.work_dir.join("second");
    std::fs::create_dir_all(second.join("charters")).unwrap();
    std::fs::write(
        second.join("workspace.json"),
        r#"{"workspace_id":"00000000-0000-0000-0000-0000000000bb","workspace_name":"second"}"#,
    )
    .unwrap();
    std::fs::write(
        second.join("charters").join("other.actions"),
        "[ ] Gamma #019f733d-4600-7000-8000-000000000003\n",
    )
    .unwrap();
    env.write_config(&format!(
        r#"{{"additional_workspaces":["{}"]}}"#,
        second.display()
    ));

    let output = export(&env, &["--format", "nquads"]);
    let text = String::from_utf8(output.stdout).expect("utf8 nquads");
    assert!(text.contains(WS_GRAPH), "primary graph present: {text}");
    assert!(
        text.contains("urn:clearhead:workspace:00000000-0000-0000-0000-0000000000bb"),
        "additional workspace graph present: {text}"
    );

    // --workspace filters the dataset down to one named graph.
    let filtered = export(&env, &["--format", "nquads", "--workspace", "testws"]);
    let text = String::from_utf8(filtered.stdout).expect("utf8 nquads");
    assert!(text.contains(WS_GRAPH), "selected graph present: {text}");
    assert!(
        !text.contains("urn:clearhead:workspace:00000000-0000-0000-0000-0000000000bb"),
        "filtered-out workspace is absent: {text}"
    );
}
