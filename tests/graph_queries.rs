use clearhead_core::{
    graph::{create_store, load_domain_model as load_into_store, query_raw, GraphName},
    workspace::store::load_domain_model,
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

fn user_flat_store() -> (clearhead_core::DomainModel, oxigraph::store::Store) {
    let model = load_domain_model(&fixture("user-flat")).expect("load domain model");
    let store = create_store().expect("create store");
    load_into_store(&store, &model, None, GraphName::DefaultGraph).expect("load into store");
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

    let work = model.charters.iter().find(|c| c.title == "work").unwrap();
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
            ?charter a actions:Charter ; rdfs:label \"work\" ;
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
