//! The optional in-process SPARQL evaluator (`sparql` feature, on by default):
//! `query raw` and locally-saved `query named` run against an ephemeral store
//! holding exactly the dataset Core publishes, and emit standard SPARQL result
//! / RDF serializations. graphd-era machinery (prefix/parameter injection, the
//! built-in registry, index/tree/graph rendering) is not part of this path.
//!
//! These tests compile away entirely in the minimal `--no-default-features`
//! build, which has no query engine.

#![cfg(feature = "sparql")]

mod common;
use common::TestEnv;
use serde_json::Value;
use std::process::Stdio;

const A: &str = "019f733d-4600-7000-8000-000000000001";
const B: &str = "019f733d-4600-7000-8000-000000000002";
const WS: &str = "00000000-0000-0000-0000-0000000000aa";

/// A user-layout workspace with durable identity and two open actions.
fn seed() -> TestEnv {
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"testws"}}"#),
    );
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));
    env
}

/// Run `clearhead query raw <query>` piped and return the parsed SPARQL
/// Results JSON document.
fn raw_srj(env: &TestEnv, query: &str, extra: &[&str]) -> Value {
    let mut args = vec!["query", "raw", query];
    args.extend_from_slice(extra);
    let output = env
        .std_command()
        .args(&args)
        .output()
        .expect("run clearhead query raw");
    assert!(
        output.status.success(),
        "query failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).unwrap_or_else(|e| {
        panic!(
            "stdout is not SPARQL Results JSON: {e}\n{}",
            String::from_utf8_lossy(&output.stdout)
        )
    })
}

fn binding_values(doc: &Value, var: &str) -> Vec<String> {
    doc["results"]["bindings"]
        .as_array()
        .expect("SRJ has results.bindings")
        .iter()
        .map(|row| row[var]["value"].as_str().unwrap().to_string())
        .collect()
}

#[test]
fn raw_select_emits_sparql_results_json_over_the_canonical_dataset() {
    let env = seed();
    let doc = raw_srj(
        &env,
        "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
         SELECT ?name WHERE { ?action a actions:Action ; rdfs:label ?name . } ORDER BY ?name",
        &["--format", "json"],
    );
    assert_eq!(doc["head"]["vars"], serde_json::json!(["name"]));
    assert_eq!(binding_values(&doc, "name"), vec!["Alpha", "Beta"]);
}

#[test]
fn raw_defaults_to_sparql_results_json_when_piped() {
    let env = seed();
    // No --format: stdout is not a terminal in tests, so the machine default
    // must be the standard bindings format (graphd-era row JSON is gone).
    let doc = raw_srj(
        &env,
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
         SELECT ?name WHERE { ?s rdfs:label ?name . FILTER(?name = \"Alpha\") }",
        &[],
    );
    assert_eq!(binding_values(&doc, "name"), vec!["Alpha"]);
}

#[test]
fn union_default_graph_and_explicit_graph_both_find_workspace_data() {
    let env = seed();
    // Without GRAPH the query already matches (union default graph)…
    let union_doc = raw_srj(
        &env,
        "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
         SELECT (COUNT(?a) AS ?n) WHERE { ?a a actions:Action }",
        &[],
    );
    assert_eq!(binding_values(&union_doc, "n"), vec!["2"]);
    // …and GRAPH ?g enumerates the workspace's stable named graph.
    let graph_doc = raw_srj(
        &env,
        "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
         SELECT DISTINCT ?g WHERE { GRAPH ?g { ?a a actions:Action } }",
        &[],
    );
    assert_eq!(
        binding_values(&graph_doc, "g"),
        vec![format!("urn:clearhead:workspace:{WS}")],
        "the dataset lives in the workspace's stable named graph"
    );
}

#[test]
fn workspace_snapshot_layer_is_published_for_editor_integration() {
    let env = seed();
    let doc = raw_srj(
        &env,
        "PREFIX ws: <https://clearhead.us/vocab/workspace/v1#>\n\
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
         SELECT ?label ?root WHERE { ?ws a ws:Workspace ; rdfs:label ?label ; ws:root ?root . }",
        &[],
    );
    assert_eq!(binding_values(&doc, "label"), vec!["testws"]);
    let root = &binding_values(&doc, "root")[0];
    assert!(
        root.ends_with("data/clearhead"),
        "ws:root is the canonicalized workspace root: {root}"
    );

    // Per-action provenance: quickfix/jump-to-source facts. hasSourceFile is
    // relative to the workspace's ws:charterRoot (the index contract), so a
    // consumer resolves it without machine-specific absolute paths.
    let lines = raw_srj(
        &env,
        "PREFIX ws: <https://clearhead.us/vocab/workspace/v1#>\n\
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
         SELECT ?file ?line WHERE {\n\
           ?a rdfs:label \"Alpha\" ; ws:hasSourceFile ?file ; ws:hasSourceLine ?line .\n\
         }",
        &[],
    );
    assert_eq!(
        binding_values(&lines, "file"),
        vec!["work.actions"],
        "hasSourceFile is the charter-root-relative source path"
    );
    assert_eq!(binding_values(&lines, "line"), vec!["1"]);
}

#[test]
fn raw_where_uses_the_clause() {
    let env = seed();
    let output = env
        .std_command()
        .args([
            "query",
            "raw",
            "--where",
            "?action a actions:Action ; rdfs:label ?name",
        ])
        .output()
        .expect("run clearhead query raw --where");
    assert!(
        output.status.success(),
        "--where failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: Value = serde_json::from_slice(&output.stdout).expect("SRJ");
    let mut names = binding_values(&doc, "name");
    names.sort();
    assert_eq!(names, vec!["Alpha", "Beta"]);
}

#[test]
fn construct_results_serialize_as_turtle() {
    let env = seed();
    let output = env
        .std_command()
        .args([
            "query",
            "raw",
            "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
             CONSTRUCT { ?s ?p ?o } WHERE { ?s a actions:Action . ?s ?p ?o }",
            "--format",
            "turtle",
        ])
        .output()
        .expect("run clearhead query raw (construct)");
    assert!(
        output.status.success(),
        "construct failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let turtle = String::from_utf8(output.stdout).expect("utf8 turtle");
    assert!(
        turtle.contains(&format!("<urn:uuid:{A}>")),
        "CONSTRUCT emits the action resource: {turtle}"
    );
    assert!(
        turtle.contains("a <https://clearhead.us/vocab/actions/v4#Action>"),
        "Turtle uses the `a` keyword for rdf:type: {turtle}"
    );
}

#[test]
fn ask_results_emit_a_boolean() {
    let env = seed();
    for (query, expected) in [
        (
            "ASK { ?s a <https://clearhead.us/vocab/actions/v4#Action> }",
            "true",
        ),
        (
            "ASK { ?s a <https://clearhead.us/vocab/actions/v4#Nonexistent> }",
            "false",
        ),
    ] {
        let output = env
            .std_command()
            .args(["query", "raw", query])
            .output()
            .expect("run clearhead query raw (ask)");
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), expected);
    }
}

#[test]
fn piped_output_is_byte_deterministic() {
    let env = seed();
    let query = "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
                 PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
                 SELECT ?name WHERE { ?a a actions:Action ; rdfs:label ?name . } ORDER BY ?name";
    let first = env
        .std_command()
        .args(["query", "raw", query])
        .output()
        .unwrap();
    let second = env
        .std_command()
        .args(["query", "raw", query])
        .output()
        .unwrap();
    assert!(first.status.success() && second.status.success());
    assert_eq!(first.stdout, second.stdout, "exports must be diffable");
}

#[test]
fn named_runs_a_project_saved_query() {
    // Project layout: everything lives under the workspace's .clearhead/.
    let env = TestEnv::new();
    env.write_text(
        ".clearhead/workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"proj"}}"#),
    );
    env.write_text(
        ".clearhead/charters/work.actions",
        &format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"),
    );
    env.write_text(
        ".clearhead/queries/mine.sparql",
        "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
         SELECT ?name WHERE { ?a a actions:Action ; rdfs:label ?name . } ORDER BY ?name",
    );

    let output = env
        .std_command()
        .args(["query", "named", "mine"])
        .output()
        .expect("run clearhead query named");
    assert!(
        output.status.success(),
        "named failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: Value = serde_json::from_slice(&output.stdout).expect("SRJ");
    assert_eq!(binding_values(&doc, "name"), vec!["Alpha", "Beta"]);
}

#[test]
fn named_runs_a_user_saved_query() {
    let env = seed();
    std::fs::create_dir_all(env.config_dir.join("queries")).unwrap();
    std::fs::write(
        env.config_dir.join("queries/userq.sparql"),
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
         SELECT ?name WHERE { ?s rdfs:label ?name . FILTER(?name = \"Beta\") }",
    )
    .unwrap();

    let output = env
        .std_command()
        .args(["query", "named", "userq"])
        .output()
        .expect("run clearhead query named");
    assert!(
        output.status.success(),
        "named failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: Value = serde_json::from_slice(&output.stdout).expect("SRJ");
    assert_eq!(binding_values(&doc, "name"), vec!["Beta"]);
}

#[test]
fn named_unknown_name_fails() {
    let env = seed();
    let output = env
        .std_command()
        .args(["query", "named", "definitely-not-a-query-xyz"])
        .output()
        .expect("run clearhead query named");
    assert!(
        !output.status.success(),
        "an unknown query name must fail (locally unresolvable, and graphd has no such built-in)"
    );
}

#[test]
fn named_with_status_stays_on_the_graphd_fallback() {
    // --status is graphd-era parameter injection; the in-process evaluator
    // never sees it. Force the fallback to fail so the forwarding is visible.
    let env = seed();
    let output = env
        .std_command()
        .env("CLEARHEAD_GRAPHD", "/nonexistent/clearhead-graphd")
        .args([
            "query",
            "named",
            "whatever",
            "--status",
            "actions:Completed",
        ])
        .output()
        .expect("run clearhead query named --status");
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("clearhead-graphd"),
        "the --status path forwards to graphd: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn empty_workspace_yields_empty_standard_results() {
    let env = TestEnv::new();
    let output = env
        .std_command()
        .args([
            "query",
            "raw",
            "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
             SELECT ?a WHERE { ?a a actions:Action }",
        ])
        .output()
        .expect("run clearhead query raw");
    assert!(
        output.status.success(),
        "empty workspace must still evaluate: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let doc: Value = serde_json::from_slice(&output.stdout).expect("SRJ");
    assert_eq!(
        doc["results"]["bindings"].as_array().unwrap().len(),
        0,
        "no actions, no bindings"
    );
}

#[test]
fn a_closed_downstream_pipe_is_not_an_error() {
    let env = seed();
    let mut cmd = env.std_command();
    cmd.args([
        "query",
        "raw",
        "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
         SELECT ?a WHERE { ?a a actions:Action }",
    ])
    .stdout(Stdio::piped());
    let mut child = cmd.spawn().expect("spawn clearhead query");
    // Simulate `| head -n1` exiting early: no reader ever consumes the pipe.
    drop(child.stdout.take());
    let status = child.wait().expect("wait");
    assert!(status.success(), "broken pipe must exit cleanly: {status}");
}
