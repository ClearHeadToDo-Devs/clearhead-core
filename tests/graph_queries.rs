use clearhead_core::{
    graph::{create_store, load_domain_model as load_into_store, query_raw, GraphName, TRANSIENT_GRAPH_URI},
    workspace::store::load_domain_model,
    CharterState,
};
use std::path::Path;

const ACTIONS: &str = "https://clearhead.us/vocab/actions/v4#";
const CCO: &str = "https://www.commoncoreontologies.org/";
const BFO: &str = "http://purl.obolibrary.org/obo/";
const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
const XSD: &str = "http://www.w3.org/2001/XMLSchema#";

fn fixture(name: &str) -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/workspace")
        .join(name)
}

/// Build a store that mirrors the production named-graph layout.
///
/// Production always loads workspace data into `urn:clearhead:workspace:<uuid>`,
/// not into `DefaultGraph`.  Using a named graph here ensures the tests cover
/// the actual code path and would have caught the union-default-graph bug.
fn transient_graph() -> GraphName {
    GraphName::NamedNode(
        oxigraph::model::NamedNode::new(TRANSIENT_GRAPH_URI).unwrap(),
    )
}

fn user_flat_store() -> (clearhead_core::DomainModel, oxigraph::store::Store) {
    let model = load_domain_model(&fixture("user-flat")).expect("load domain model");
    let store = create_store().expect("create store");
    // Use a named graph — the same code path as production.
    load_into_store(&store, &model, None, transient_graph()).expect("load into store");
    (model, store)
}

// ============================================================================
// Action inventory
// ============================================================================

#[test]
fn all_actions_visible_in_graph() {
    let (_, store) = user_flat_store();

    let sparql = "
        PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?name WHERE {
            ?action a actions:Action ; rdfs:label ?name .
        } ORDER BY ?name
    ";

    let rows = query_raw(&store, sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("name").map(String::as_str))
        .collect();

    assert!(
        names.contains(&"Write quarterly report"),
        "got: {:?}",
        names
    );
    assert!(names.contains(&"Review team PRs"), "got: {:?}", names);
    assert!(names.contains(&"Backend PRs"), "got: {:?}", names);
    assert!(names.contains(&"Buy groceries"), "got: {:?}", names);
    assert!(names.contains(&"Morning run"), "got: {:?}", names);
}

// ============================================================================
// Flat .actions workspaces now map directly to actions, not synthetic plans
// ============================================================================

#[test]
fn flat_actions_do_not_create_synthetic_plans() {
    let (model, store) = user_flat_store();

    assert!(
        model.all_plans().is_empty(),
        "flat .actions fixture should not create plans"
    );

    let sparql = "
        PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
        PREFIX cco: <https://www.commoncoreontologies.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?name (COUNT(?action) AS ?action_count) WHERE {
            ?plan a cco:ont00000974 ; rdfs:label ?name ;
                  cco:ont00001942 ?action .
            ?action a actions:Action .
        } GROUP BY ?name ORDER BY ?name
    ";

    let rows = query_raw(&store, sparql).expect("query");
    assert!(
        rows.is_empty(),
        "flat .actions fixture should not produce plan->action prescribes edges: {:?}",
        rows
    );
}

// ============================================================================
// Action state from .actions file is reflected in domain model and graph
// ============================================================================

#[test]
fn action_state_from_actions_file_is_reflected_in_graph() {
    let (model, store) = user_flat_store();

    let work = model.charters.iter().find(|c| c.title == "Work").unwrap();
    let report_actions: Vec<_> = work
        .actions
        .iter()
        .filter(|a| a.name == "Write quarterly report")
        .collect();
    assert_eq!(report_actions.len(), 1);
    assert_eq!(report_actions[0].state, clearhead_core::ActionState::InProgress);

    let sparql = "
        PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
        PREFIX cco: <https://www.commoncoreontologies.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?status WHERE {
            ?action a actions:Action ;
                    rdfs:label \"Write quarterly report\" ;
                    cco:ont00001868 ?status .
        }
    ";

    let rows = query_raw(&store, sparql).expect("query");
    assert_eq!(rows.len(), 1);
    assert!(
        rows[0]
            .get("status")
            .map(|s| s.ends_with("InProgress"))
            .unwrap_or(false),
        "expected InProgress, got: {:?}",
        rows[0].get("status")
    );
}

// ============================================================================
// Filter actions by status
// ============================================================================

#[test]
fn in_progress_actions_listed_correctly() {
    let (_, store) = user_flat_store();

    let sparql = "
        PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX cco: <https://www.commoncoreontologies.org/>
        SELECT ?name WHERE {
            ?action a actions:Action ;
                    rdfs:label ?name ;
                    cco:ont00001868 actions:InProgress .
        } ORDER BY ?name
    ";

    let rows = query_raw(&store, sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("name").map(String::as_str))
        .collect();

    assert!(
        names.contains(&"Write quarterly report"),
        "got: {:?}",
        names
    );
    assert!(names.contains(&"Morning run"), "got: {:?}", names);
    assert_eq!(
        names.len(),
        2,
        "expected exactly 2 InProgress actions, got: {:?}",
        names
    );
}

// ============================================================================
// Charter-scoped action listing
// ============================================================================

#[test]
fn actions_scoped_to_charter_by_label() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX bfo: <{BFO}>
        PREFIX rdfs: <{RDFS}>
        SELECT ?name WHERE {{
            ?charter a actions:Charter ; rdfs:label \"Work\" ;
                     bfo:BFO_0000051 ?action .
            ?action a actions:Action ; rdfs:label ?name .
        }} ORDER BY ?name
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("name").map(String::as_str))
        .collect();

    assert_eq!(
        names,
        vec!["Backend PRs", "Review team PRs", "Write quarterly report"],
        "work charter should contain exactly these actions"
    );
}

#[test]
fn actions_scoped_to_charter_by_alias() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX bfo: <{BFO}>
        PREFIX rdfs: <{RDFS}>
        SELECT ?name WHERE {{
            ?charter a actions:Charter ; actions:hasAlias \"personal\" ;
                     bfo:BFO_0000051 ?action .
            ?action a actions:Action ; rdfs:label ?name .
        }} ORDER BY ?name
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("name").map(String::as_str))
        .collect();

    assert!(names.contains(&"Buy groceries"), "got: {:?}", names);
    assert!(names.contains(&"Morning run"), "got: {:?}", names);
    assert_eq!(names.len(), 2);
}

// ============================================================================
// Due date queries
// ============================================================================

#[test]
fn overdue_actions_returned_for_cutoff_date() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX rdfs: <{RDFS}>
        PREFIX xsd: <{XSD}>
        SELECT ?action_name ?due_date WHERE {{
            ?action a actions:Action ;
                    rdfs:label ?action_name ;
                    actions:hasDueDateTime ?due_date ;
                    cco:ont00001868 ?status .
            FILTER(?status != <{ACTIONS}Completed> && ?status != <{ACTIONS}Cancelled>)
            FILTER(?due_date <= \"2026-04-17T23:59:59Z\"^^xsd:dateTime)
        }} ORDER BY ?due_date
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("action_name").map(String::as_str))
        .collect();

    assert_eq!(
        names,
        vec!["Write quarterly report"],
        "only overdue action should appear; got: {:?}",
        names
    );
}

#[test]
fn upcoming_actions_returned_after_cutoff() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX rdfs: <{RDFS}>
        PREFIX xsd: <{XSD}>
        SELECT ?action_name ?due_date WHERE {{
            ?action a actions:Action ;
                    rdfs:label ?action_name ;
                    actions:hasDueDateTime ?due_date ;
                    cco:ont00001868 ?status .
            FILTER(?status != <{ACTIONS}Completed> && ?status != <{ACTIONS}Cancelled>)
            FILTER(?due_date > \"2026-04-17T23:59:59Z\"^^xsd:dateTime)
        }} ORDER BY ?due_date
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("action_name").map(String::as_str))
        .collect();

    assert_eq!(
        names,
        vec!["Buy groceries"],
        "only upcoming action should appear; got: {:?}",
        names
    );
}

// ============================================================================
// Scheduled / agenda query
// ============================================================================

#[test]
fn scheduled_actions_on_or_before_date() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX rdfs: <{RDFS}>
        PREFIX xsd: <{XSD}>
        SELECT ?action_name ?scheduled_at WHERE {{
            ?action a actions:Action ;
                    rdfs:label ?action_name ;
                    actions:hasScheduledDateTime ?scheduled_at ;
                    cco:ont00001868 ?status .
            FILTER(?status != <{ACTIONS}Completed> && ?status != <{ACTIONS}Cancelled>)
            FILTER(?scheduled_at <= \"2026-04-17T23:59:59Z\"^^xsd:dateTime)
        }} ORDER BY ?scheduled_at
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("action_name").map(String::as_str))
        .collect();

    assert_eq!(
        names,
        vec!["Write quarterly report"],
        "only scheduled-today action should appear; got: {:?}",
        names
    );
}

// ============================================================================
// Named query smoke tests
//
// These tests run the actual .sparql files (compiled into the CLI binary via
// include_str!) against a real fixture workspace loaded in a named graph.
// They exist specifically to catch the class of bug where:
//   (a) a named query is written without GRAPH clause and silently returns
//       nothing because the evaluator only searched the default graph, or
//   (b) a named query uses a predicate or class name that drifted from the
//       ontology (e.g. old ont00001868 constant changed).
//
// If a named query legitimately returns nothing against the fixture (e.g. it
// filters by a future date or a status that doesn't exist in the fixture) the
// test should explain why in a comment rather than silently passing.
// ============================================================================

const NEXT_ACTIONS_SPARQL: &str = include_str!(
    "../../clearhead-cli/src/queries/next-actions.sparql"
);
const ACTS_BY_PHASE_SPARQL: &str = include_str!(
    "../../clearhead-cli/src/queries/acts-by-phase.sparql"
);
const OPEN_PLANS_SPARQL: &str = include_str!(
    "../../clearhead-cli/src/queries/open-plans.sparql"
);

fn inject_status(sparql: &str, status_iri: &str) -> String {
    sparql.replace("?STATUS_FILTER", status_iri)
}

#[test]
fn named_query_next_actions_returns_results_against_fixture() {
    let (_, store) = user_flat_store();
    // The fixture has open actions with no unresolved deps — at least one must appear.
    let rows = query_raw(&store, NEXT_ACTIONS_SPARQL).expect("next-actions query");
    assert!(
        !rows.is_empty(),
        "next-actions named query returned nothing — \
         likely a GRAPH-clause / evaluator configuration regression; \
         fixture has open, unblocked actions that should appear here"
    );
}

#[test]
fn named_query_acts_by_phase_returns_not_started_actions() {
    let (_, store) = user_flat_store();
    let sparql = inject_status(
        ACTS_BY_PHASE_SPARQL,
        &format!("<{ACTIONS}NotStarted>"),
    );
    let rows = query_raw(&store, &sparql).expect("acts-by-phase query");
    assert!(
        !rows.is_empty(),
        "acts-by-phase (NotStarted) returned nothing against fixture; \
         fixture has NotStarted actions — check predicate / class drift"
    );
}

#[test]
fn named_query_acts_by_phase_returns_in_progress_actions() {
    let (_, store) = user_flat_store();
    let sparql = inject_status(
        ACTS_BY_PHASE_SPARQL,
        &format!("<{ACTIONS}InProgress>"),
    );
    let rows = query_raw(&store, &sparql).expect("acts-by-phase query");
    // Fixture has 2 InProgress actions.
    assert!(
        !rows.is_empty(),
        "acts-by-phase (InProgress) returned nothing; fixture has InProgress actions"
    );
}

#[test]
fn named_query_open_plans_returns_results_when_ics_plans_present() {
    // The user-flat fixture has no .ics plans, so open-plans is legitimately empty.
    // This test documents that expectation and ensures the query is at least
    // syntactically valid and runs without error against a named-graph store.
    let (_, store) = user_flat_store();
    let result = query_raw(&store, OPEN_PLANS_SPARQL);
    assert!(result.is_ok(), "open-plans query failed to execute: {:?}", result.err());
    // Zero rows is expected here — the fixture has no plans.
}

// ============================================================================
// CharterState round-trip through graph
// ============================================================================

#[test]
fn charter_state_from_md_frontmatter_is_reflected_in_domain_model() {
    // work.md in the fixture declares `state: Active`
    let (model, _) = user_flat_store();
    let work = model.charters.iter().find(|c| c.title == "Work").unwrap();
    assert_eq!(
        work.state,
        Some(CharterState::Active),
        "work charter should carry Active state from work.md frontmatter"
    );
}

#[test]
fn charter_state_is_queryable_from_graph() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX rdfs: <{RDFS}>
        SELECT ?charter_name ?state WHERE {{
            ?charter a actions:Charter ;
                     rdfs:label ?charter_name ;
                     actions:hasCharterState ?state .
        }} ORDER BY ?charter_name
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    assert_eq!(rows.len(), 1, "expected exactly one charter with explicit state; got {:?}", rows);
    assert_eq!(rows[0].get("charter_name").map(String::as_str), Some("Work"));
    assert_eq!(rows[0].get("state").map(String::as_str), Some("Active"));
}

#[test]
fn charter_without_state_has_no_hascharterstate_triple() {
    // personal.actions has no .md companion → state is None → no triple emitted
    let (model, store) = user_flat_store();

    let personal = model.charters.iter().find(|c| c.title == "personal").unwrap();
    assert_eq!(personal.state, None, "personal charter should have no state");

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX rdfs: <{RDFS}>
        SELECT ?charter WHERE {{
            ?charter a actions:Charter ;
                     rdfs:label \"personal\" ;
                     actions:hasCharterState ?state .
        }}
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    assert!(
        rows.is_empty(),
        "charter with no state should not emit hasCharterState triple; got {:?}",
        rows
    );
}
