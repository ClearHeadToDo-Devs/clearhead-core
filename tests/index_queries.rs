mod common;
use common::TestEnv;

// A past-dated action — always <= END_OF_TODAY regardless of when tests run.
const DATED_ACTION: &str =
    "[ ] past scheduled action @2000-01-01T00:00 #01900000-0000-7000-8000-000000000001\n";

// An undated action — should never appear in agenda results.
const UNDATED_ACTION: &str = "[ ] undated action #01900000-0000-7000-8000-000000000002\n";

/// Parse the index JSON-LD document and return its @graph nodes.
/// Explicit JSON-LD preserves one framed payload even for empty results.
fn parse_graph(output: &[u8]) -> Vec<serde_json::Value> {
    let doc: serde_json::Value = serde_json::from_slice(output).expect("output is not valid JSON");
    assert!(doc.get("@context").is_some(), "missing @context: {doc}");
    doc.get("@graph")
        .and_then(|g| g.as_array())
        .unwrap_or_else(|| panic!("@graph is not an array: {doc}"))
        .clone()
}

#[test]
fn agenda_returns_empty_when_no_dated_actions() {
    let env = TestEnv::new();
    env.with_workspace_identity()
        .write_actions("next.actions", UNDATED_ACTION);

    let output = env
        .command()
        .args(["query", "index", "agenda", "--format", "jsonld"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let rows = parse_graph(&output.stdout);
    assert!(rows.is_empty(), "expected no rows, got {rows:?}");
}

#[test]
fn agenda_returns_past_dated_action() {
    let env = TestEnv::new();
    env.with_workspace_identity()
        .write_actions("next.actions", DATED_ACTION);

    let output = env
        .command()
        .args(["query", "index", "agenda", "--format", "jsonld"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let rows = parse_graph(&output.stdout);
    assert_eq!(rows.len(), 1, "expected 1 row, got {rows:?}");
    assert_eq!(rows[0]["name"], "past scheduled action");
    assert_eq!(rows[0]["status"], "NotStarted");
}

#[test]
fn index_pipe_defaults_to_ndjson() {
    let env = TestEnv::new();
    env.with_workspace_identity()
        .write_actions("next.actions", DATED_ACTION);

    let output = env
        .command()
        .args(["query", "index", "agenda"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let lines: Vec<_> = output
        .stdout
        .split(|byte| *byte == b'\n')
        .filter(|line| !line.is_empty())
        .collect();
    assert_eq!(lines.len(), 1, "expected one NDJSON record");
    let row: serde_json::Value = serde_json::from_slice(lines[0]).expect("valid NDJSON row");
    assert_eq!(row["name"], "past scheduled action");
    assert!(row.get("@context").is_none(), "NDJSON rows are unframed");
}

#[test]
fn index_ids_emit_canonical_references_in_query_order() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] later but urgent !1 @2000-01-02T00:00 #01900000-0000-7000-8000-000000000011\n\
         [ ] earlier but minor !3 @2000-01-01T00:00 #01900000-0000-7000-8000-000000000012\n",
    );

    let output = env
        .command()
        .args(["query", "index", "agenda", "--format", "ids"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        output.stdout,
        b"urn:uuid:01900000-0000-7000-8000-000000000011\n\
          urn:uuid:01900000-0000-7000-8000-000000000012\n"
    );
}

#[test]
fn index_ids_emit_no_bytes_for_an_empty_view() {
    let env = TestEnv::new();
    env.with_workspace_identity()
        .write_actions("next.actions", UNDATED_ACTION);

    let output = env
        .command()
        .args(["query", "index", "agenda", "--format", "ids"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output.stdout.is_empty(), "expected no ID records");
}

#[test]
fn agenda_excludes_undated_actions() {
    let env = TestEnv::new();
    // Both actions present — only the dated one should appear.
    let content = format!("{DATED_ACTION}{UNDATED_ACTION}");
    env.with_workspace_identity()
        .write_actions("next.actions", &content);

    let output = env
        .command()
        .args(["query", "index", "agenda", "--format", "jsonld"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let rows = parse_graph(&output.stdout);
    assert_eq!(rows.len(), 1, "expected only dated action, got {rows:?}");
    assert_eq!(rows[0]["name"], "past scheduled action");
}

#[test]
fn agenda_row_satisfies_index_contract() {
    let env = TestEnv::new();
    env.with_workspace_identity()
        .write_actions("next.actions", DATED_ACTION);

    let output = env
        .command()
        .args(["query", "index", "agenda", "--format", "jsonld"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let rows = parse_graph(&output.stdout);
    assert_eq!(rows.len(), 1);

    let row = &rows[0];
    // id is the canonical node IRI — the address mutation verbs target.
    assert_eq!(row["id"], "urn:uuid:01900000-0000-7000-8000-000000000001");
    assert!(row.get("name").is_some(), "missing: name");
    assert!(row.get("status").is_some(), "missing: status");
    assert!(row.get("source_file").is_some(), "missing: source_file");
    // Locator line is a number, not a stringified literal — clients jump with it.
    assert!(
        row["source_line"].is_u64(),
        "source_line not numeric: {row:?}"
    );
    assert!(row.get("charter_root").is_some(), "missing: charter_root");
    // Sort keys travel as properties so order survives an RDF round-trip.
    assert!(
        row.get("scheduled_at").is_some(),
        "missing sort key: scheduled_at"
    );
}

// ── agenda wisdom: the charter's daily definition ────────────────────────────
// open/in-progress only, no open predecessors, act on the lowest open child,
// sorted by priority then date. The WHERE clause is the product.

fn run_index(env: &TestEnv, name: &str) -> Vec<serde_json::Value> {
    let output = env
        .command()
        .args(["query", "index", name, "--format", "jsonld"])
        .output()
        .expect("failed to run");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    parse_graph(&output.stdout)
}

fn run_agenda(env: &TestEnv) -> Vec<serde_json::Value> {
    run_index(env, "agenda")
}

#[test]
fn agenda_hides_actions_with_open_predecessors() {
    let env = TestEnv::new();
    // beta depends on alpha; only the head of the chain is actionable.
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] alpha task =alpha @2000-01-01T00:00 #01900000-0000-7000-8000-000000000011\n\
         [ ] beta task <alpha @2000-01-02T00:00 #01900000-0000-7000-8000-000000000012\n",
    );
    let rows = run_agenda(&env);
    assert_eq!(rows.len(), 1, "expected only chain head, got {rows:?}");
    assert_eq!(rows[0]["name"], "alpha task");
}

#[test]
fn agenda_surfaces_successor_when_predecessor_completes() {
    let env = TestEnv::new();
    // The promised aliveness: complete alpha, re-run, beta surfaces.
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[x] alpha task =alpha @2000-01-01T00:00 %2000-01-01T01:00 #01900000-0000-7000-8000-000000000011\n\
         [ ] beta task <alpha @2000-01-02T00:00 #01900000-0000-7000-8000-000000000012\n",
    );
    let rows = run_agenda(&env);
    assert_eq!(rows.len(), 1, "expected surfaced successor, got {rows:?}");
    assert_eq!(rows[0]["name"], "beta task");
}

#[test]
fn agenda_shows_lowest_open_child_not_parent() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] parent task @2000-01-01T00:00 #01900000-0000-7000-8000-000000000021\n\
         >[ ] child task @2000-01-02T00:00 #01900000-0000-7000-8000-000000000022\n",
    );
    let rows = run_agenda(&env);
    assert_eq!(rows.len(), 1, "expected only the leaf, got {rows:?}");
    assert_eq!(rows[0]["name"], "child task");
}

#[test]
fn agenda_excludes_blocked_actions() {
    let env = TestEnv::new();
    // daily is open/in-progress; blocked belongs to weekly's horizon view.
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[=] blocked task @2000-01-01T00:00 #01900000-0000-7000-8000-000000000031\n",
    );
    let rows = run_agenda(&env);
    assert!(rows.is_empty(), "blocked must not appear, got {rows:?}");
}

#[test]
fn agenda_hides_child_of_action_blocked_by_predecessor() {
    let env = TestEnv::new();
    // container depends on setup (still open); leaf is container's child.
    // A blocked parent must block its descendants too, not just itself.
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] setup task =setup @2000-01-01T00:00 #01900000-0000-7000-8000-000000000045\n\
         [ ] container task <setup @2000-01-01T00:00 #01900000-0000-7000-8000-000000000046\n\
         >[ ] leaf task @2000-01-01T00:00 #01900000-0000-7000-8000-000000000047\n",
    );
    let rows = run_agenda(&env);
    assert_eq!(rows.len(), 1, "expected only the chain head, got {rows:?}");
    assert_eq!(rows[0]["name"], "setup task");
}

#[test]
fn agenda_sorts_by_priority_before_date() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] later but urgent !1 @2000-01-02T00:00 #01900000-0000-7000-8000-000000000041\n\
         [ ] earlier but minor !3 @2000-01-01T00:00 #01900000-0000-7000-8000-000000000042\n",
    );
    let rows = run_agenda(&env);
    assert_eq!(rows.len(), 2);
    assert_eq!(
        rows[0]["name"], "later but urgent",
        "priority outranks date: {rows:?}"
    );
    // Sort keys travel as properties; priority is numeric like source_line.
    assert!(
        rows[0]["priority"].is_u64(),
        "priority not numeric: {rows:?}"
    );
}

// ── weekly: the horizon view ─────────────────────────────────────────────────
// Wider than daily on every axis: blocked and undated included, no
// dependency filtering — seeing the chains ahead of time is the point.

#[test]
fn weekly_includes_undated_actions() {
    let env = TestEnv::new();
    env.with_workspace_identity()
        .write_actions("next.actions", UNDATED_ACTION);
    let rows = run_index(&env, "weekly");
    assert_eq!(
        rows.len(),
        1,
        "undated open work belongs on the horizon: {rows:?}"
    );
    assert_eq!(rows[0]["name"], "undated action");
}

#[test]
fn weekly_excludes_actions_beyond_the_horizon() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] within week @2000-01-01T00:00 #01900000-0000-7000-8000-000000000051\n\
         [ ] far future @2999-01-01T00:00 #01900000-0000-7000-8000-000000000052\n",
    );
    let rows = run_index(&env, "weekly");
    assert_eq!(
        rows.len(),
        1,
        "expected only the within-horizon action: {rows:?}"
    );
    assert_eq!(rows[0]["name"], "within week");
}

#[test]
fn weekly_includes_blocked_actions() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[=] blocked task @2000-01-01T00:00 #01900000-0000-7000-8000-000000000053\n",
    );
    let rows = run_index(&env, "weekly");
    assert_eq!(rows.len(), 1, "waiting is part of the horizon: {rows:?}");
    assert_eq!(rows[0]["status"], "Blocked");
}

// ── unscheduled: the date-light working view ─────────────────────────────────
// The agenda's complement: actionable now and unplanned. Undated in scope
// means neither the action nor any ancestor carries a scheduled/due date.

#[test]
fn unscheduled_includes_undated_and_excludes_dated() {
    let env = TestEnv::new();
    let content = format!("{DATED_ACTION}{UNDATED_ACTION}");
    env.with_workspace_identity()
        .write_actions("next.actions", &content);
    let rows = run_index(&env, "unscheduled");
    assert_eq!(
        rows.len(),
        1,
        "expected only the undated action, got {rows:?}"
    );
    assert_eq!(rows[0]["name"], "undated action");
}

#[test]
fn unscheduled_excludes_children_of_dated_parents() {
    let env = TestEnv::new();
    // The child carries no date itself, but its parent is planned work —
    // dates scope down the tree.
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] dated parent @2000-01-01T00:00 #01900000-0000-7000-8000-000000000061\n\
         >[ ] undated child #01900000-0000-7000-8000-000000000062\n",
    );
    let rows = run_index(&env, "unscheduled");
    assert!(
        rows.is_empty(),
        "planned work must not appear, got {rows:?}"
    );
}

#[test]
fn unscheduled_hides_actions_with_open_predecessors_and_blocked() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] alpha task =alpha #01900000-0000-7000-8000-000000000063\n\
         [ ] beta task <alpha #01900000-0000-7000-8000-000000000064\n\
         [=] blocked task #01900000-0000-7000-8000-000000000065\n",
    );
    let rows = run_index(&env, "unscheduled");
    assert_eq!(rows.len(), 1, "expected only the chain head, got {rows:?}");
    assert_eq!(rows[0]["name"], "alpha task");
}

#[test]
fn unscheduled_hides_child_of_action_blocked_by_predecessor() {
    let env = TestEnv::new();
    // container depends on setup (still open); leaf is container's child.
    // A blocked parent must block its descendants too, not just itself.
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] setup task =setup #01900000-0000-7000-8000-000000000069\n\
         [ ] container task <setup #01900000-0000-7000-8000-000000000070\n\
         >[ ] leaf task #01900000-0000-7000-8000-000000000071\n",
    );
    let rows = run_index(&env, "unscheduled");
    assert_eq!(rows.len(), 1, "expected only the chain head, got {rows:?}");
    assert_eq!(rows[0]["name"], "setup task");
}

#[test]
fn unscheduled_shows_lowest_open_child_and_sorts_by_priority() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] loose end !3 #01900000-0000-7000-8000-000000000066\n\
         [ ] undated parent #01900000-0000-7000-8000-000000000067\n\
         >[ ] urgent leaf !1 #01900000-0000-7000-8000-000000000068\n",
    );
    let rows = run_index(&env, "unscheduled");
    assert_eq!(rows.len(), 2, "parent is a container, got {rows:?}");
    assert_eq!(
        rows[0]["name"], "urgent leaf",
        "priority orders the list: {rows:?}"
    );
    assert_eq!(rows[1]["name"], "loose end");
}

#[test]
fn project_index_query_shadows_built_in() {
    let env = TestEnv::new();
    // A real project at the pwd: .clearhead/ carries both the charters and
    // the override. (Placing .clearhead/queries/ inside a *user-layout*
    // workspace would flip resolve_workspace_layout to project mode and hide
    // the charters — the override convention is for projects.)
    env.with_workspace_identity();
    let project_data = env.data_dir.join(".clearhead");
    std::fs::create_dir_all(project_data.join("charters")).expect("create project charters");
    std::fs::write(
        project_data.join("charters").join("next.actions"),
        UNDATED_ACTION,
    )
    .expect("write project actions");

    // Built-in agenda excludes undated actions; the override below includes
    // them. If the undated action appears, the override won.
    let override_dir = project_data.join("queries").join("index");
    std::fs::create_dir_all(&override_dir).expect("create override dir");
    std::fs::write(
        override_dir.join("agenda.sparql"),
        r##"PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
PREFIX cco: <https://www.commoncoreontologies.org/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ws: <https://clearhead.us/vocab/workspace/v1#>
SELECT (?action AS ?id) ?name ?status ?source_file ?source_line ?charter_root WHERE {
    GRAPH ?g {
        ?action a actions:Action ;
             rdfs:label ?name ;
             cco:ont00001868 ?raw_state ;
             ws:hasSourceFile ?source_file ;
             ws:hasSourceLine ?source_line .
        ?workspace_node a ws:Workspace ; ws:charterRoot ?charter_root .
    }
    BIND(STRAFTER(STR(?raw_state), "#") AS ?status)
}
ORDER BY ?name"##,
    )
    .expect("write override query");

    let output = env
        .command()
        .args(["query", "index", "agenda", "--format", "jsonld"])
        .output()
        .expect("failed to run");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let rows = parse_graph(&output.stdout);
    assert_eq!(
        rows.len(),
        1,
        "override should include undated action, got {rows:?}"
    );
    assert_eq!(rows[0]["name"], "undated action");
}

#[test]
fn query_show_prints_built_in_sparql() {
    let env = TestEnv::new();
    env.command()
        .args(["query", "show", "agenda"])
        .assert()
        .success()
        .stdout(predicates::str::contains("SELECT"))
        .stdout(predicates::str::contains("END_OF_TODAY"));
}

// ── chain: given an action, everything still blocking it ────────────────────
// Walks cco:ont00001775 (predecessor) transitively from the resolved action.
// Flat, deduped, open-only — completed predecessors are done, not "still
// required."

fn run_chain(env: &TestEnv, target_uuid: &str) -> Vec<serde_json::Value> {
    let output = env
        .command()
        .args([
            "query",
            "index",
            "chain",
            "--target",
            &format!("<urn:uuid:{target_uuid}>"),
            "--format",
            "jsonld",
        ])
        .output()
        .expect("failed to run");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    parse_graph(&output.stdout)
}

#[test]
fn chain_returns_empty_for_action_with_no_predecessors() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] standalone task #01900000-0000-7000-8000-000000000071\n",
    );
    let rows = run_chain(&env, "01900000-0000-7000-8000-000000000071");
    assert!(rows.is_empty(), "expected no rows, got {rows:?}");
}

#[test]
fn chain_returns_direct_predecessor() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] alpha task =alpha #01900000-0000-7000-8000-000000000072\n\
         [ ] beta task <alpha #01900000-0000-7000-8000-000000000073\n",
    );
    let rows = run_chain(&env, "01900000-0000-7000-8000-000000000073");
    assert_eq!(rows.len(), 1, "expected one predecessor, got {rows:?}");
    assert_eq!(rows[0]["name"], "alpha task");
}

#[test]
fn chain_walks_transitive_predecessors() {
    let env = TestEnv::new();
    // gamma <- beta <- alpha: querying gamma should surface both ancestors.
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] alpha task =alpha #01900000-0000-7000-8000-000000000074\n\
         [ ] beta task =beta <alpha #01900000-0000-7000-8000-000000000075\n\
         [ ] gamma task <beta #01900000-0000-7000-8000-000000000076\n",
    );
    let rows = run_chain(&env, "01900000-0000-7000-8000-000000000076");
    let names: Vec<&str> = rows.iter().map(|r| r["name"].as_str().unwrap()).collect();
    assert_eq!(names.len(), 2, "expected both ancestors, got {rows:?}");
    assert!(
        names.contains(&"alpha task"),
        "missing transitive ancestor: {rows:?}"
    );
    assert!(
        names.contains(&"beta task"),
        "missing direct predecessor: {rows:?}"
    );
}

#[test]
fn chain_excludes_completed_predecessors() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[x] alpha task =alpha %2000-01-01T00:00 #01900000-0000-7000-8000-000000000077\n\
         [ ] beta task <alpha #01900000-0000-7000-8000-000000000078\n",
    );
    let rows = run_chain(&env, "01900000-0000-7000-8000-000000000078");
    assert!(
        rows.is_empty(),
        "completed predecessor no longer blocks anything, got {rows:?}"
    );
}

#[test]
fn chain_row_satisfies_index_contract() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] alpha task =alpha #01900000-0000-7000-8000-000000000079\n\
         [ ] beta task <alpha #01900000-0000-7000-8000-000000000080\n",
    );
    let rows = run_chain(&env, "01900000-0000-7000-8000-000000000080");
    assert_eq!(rows.len(), 1);
    let row = &rows[0];
    assert_eq!(row["id"], "urn:uuid:01900000-0000-7000-8000-000000000079");
    assert!(row.get("name").is_some(), "missing: name");
    assert!(row.get("status").is_some(), "missing: status");
    assert!(row.get("source_file").is_some(), "missing: source_file");
    assert!(
        row["source_line"].is_u64(),
        "source_line not numeric: {row:?}"
    );
    assert!(row.get("charter_root").is_some(), "missing: charter_root");
}

#[test]
fn agenda_query_listed_under_index_type() {
    let env = TestEnv::new();
    env.command()
        .args(["query", "list"])
        .assert()
        .success()
        .stdout(predicates::str::contains("agenda"))
        .stdout(predicates::str::contains("index"));
}
