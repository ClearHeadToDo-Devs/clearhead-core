use clearhead_core::{
    domain::ActPhase,
    graph::{create_store, load_domain_model as load_into_store, query_raw},
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
    load_into_store(&store, &model).expect("load into store");
    (model, store)
}

// ============================================================================
// Plan inventory
// ============================================================================

#[test]
fn all_plans_visible_in_graph() {
    let (_, store) = user_flat_store();

    let sparql = "
        PREFIX cco: <https://www.commoncoreontologies.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?name WHERE {
            ?plan a cco:ont00000974 ; rdfs:label ?name .
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
// Every plan has exactly one act (sidecar or synthetic representative)
// ============================================================================

#[test]
fn every_plan_has_exactly_one_act() {
    let (_, store) = user_flat_store();

    let sparql = "
        PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
        PREFIX cco: <https://www.commoncoreontologies.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?name (COUNT(?act) AS ?act_count) WHERE {
            ?plan a cco:ont00000974 ; rdfs:label ?name ;
                  cco:ont00001942 ?act .
            ?act a cco:ont00000228 .
        } GROUP BY ?name ORDER BY ?name
    ";

    let rows = query_raw(&store, sparql).expect("query");
    for row in &rows {
        assert_eq!(
            row.get("act_count").map(String::as_str),
            Some("1"),
            "plan '{}' should have exactly 1 act",
            row.get("name").unwrap_or(&"?".to_string())
        );
    }
    assert_eq!(rows.len(), 5, "expected 5 plans with acts");
}

// ============================================================================
// Act state from .actions file is reflected in domain model and graph
// ============================================================================

#[test]
fn act_state_from_actions_file_is_reflected_in_graph() {
    let (model, store) = user_flat_store();

    // Domain model: act state from .actions file — InProgress ([-]), not default NotStarted
    let work = model.charters.iter().find(|c| c.title == "work").unwrap();
    let report = work
        .plans
        .iter()
        .find(|p| p.name == "Write quarterly report")
        .unwrap();
    let report_acts: Vec<_> = work
        .acts
        .iter()
        .filter(|a| a.plan_id == Some(report.id))
        .collect();
    assert_eq!(report_acts.len(), 1);
    assert_eq!(report_acts[0].phase, ActPhase::InProgress);

    // Graph: query the specific act linked to "Write quarterly report"
    let sparql = "
        PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
        PREFIX cco: <https://www.commoncoreontologies.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?status WHERE {
            ?plan a cco:ont00000974 ; rdfs:label \"Write quarterly report\" ;
                  cco:ont00001942 ?act .
            ?act cco:ont00001868 ?status .
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
// Filter plans by act status — the practical "what's in progress" query
// ============================================================================

#[test]
fn in_progress_acts_listed_correctly() {
    let (_, store) = user_flat_store();

    let sparql = "
        PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
        PREFIX cco: <https://www.commoncoreontologies.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?name WHERE {
            ?plan a cco:ont00000974 ; rdfs:label ?name ;
                  cco:ont00001942 ?act .
            ?act cco:ont00001868 actions:InProgress .
        } ORDER BY ?name
    ";

    let rows = query_raw(&store, sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("name").map(String::as_str))
        .collect();

    // "Morning run" is [-] in personal.actions
    // "Write quarterly report" is [-] in work.actions
    assert!(
        names.contains(&"Write quarterly report"),
        "got: {:?}",
        names
    );
    assert!(names.contains(&"Morning run"), "got: {:?}", names);
    assert_eq!(
        names.len(),
        2,
        "expected exactly 2 InProgress plans, got: {:?}",
        names
    );
}

// ============================================================================
// Charter-scoped plan listing
// ============================================================================

#[test]
fn plans_scoped_to_charter_by_label() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX bfo: <{BFO}>
        PREFIX rdfs: <{RDFS}>
        SELECT ?name WHERE {{
            ?charter a actions:Charter ; rdfs:label \"work\" ;
                     bfo:BFO_0000051 ?plan .
            ?plan a cco:ont00000974 ; rdfs:label ?name .
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
        "work charter should contain exactly these plans"
    );
}

#[test]
fn plans_scoped_to_charter_by_alias() {
    let (_, store) = user_flat_store();

    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX bfo: <{BFO}>
        PREFIX rdfs: <{RDFS}>
        SELECT ?name WHERE {{
            ?charter a actions:Charter ; actions:hasAlias \"personal\" ;
                     bfo:BFO_0000051 ?plan .
            ?plan a cco:ont00000974 ; rdfs:label ?name .
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
fn overdue_acts_returned_for_cutoff_date() {
    let (_, store) = user_flat_store();

    // Cutoff: 2026-04-17 end of day — "Write quarterly report" (due 2026-04-10) is overdue
    // "Buy groceries" (due 2026-04-20) is not
    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX rdfs: <{RDFS}>
        PREFIX xsd: <{XSD}>
        SELECT ?plan_name ?due_date WHERE {{
            ?act a cco:ont00000228 ;
                 actions:hasDueDateTime ?due_date ;
                 cco:ont00001868 ?status .
            FILTER(?status != <{ACTIONS}Completed> && ?status != <{ACTIONS}Cancelled>)
            FILTER(?due_date <= \"2026-04-17T23:59:59Z\"^^xsd:dateTime)
            ?plan cco:ont00001942 ?act ; rdfs:label ?plan_name .
        }} ORDER BY ?due_date
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("plan_name").map(String::as_str))
        .collect();

    assert_eq!(
        names,
        vec!["Write quarterly report"],
        "only overdue plan should appear; got: {:?}",
        names
    );
}

#[test]
fn upcoming_acts_returned_after_cutoff() {
    let (_, store) = user_flat_store();

    // Acts due AFTER the cutoff — "Buy groceries" (due 2026-04-20) should appear
    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX rdfs: <{RDFS}>
        PREFIX xsd: <{XSD}>
        SELECT ?plan_name ?due_date WHERE {{
            ?act a cco:ont00000228 ;
                 actions:hasDueDateTime ?due_date ;
                 cco:ont00001868 ?status .
            FILTER(?status != <{ACTIONS}Completed> && ?status != <{ACTIONS}Cancelled>)
            FILTER(?due_date > \"2026-04-17T23:59:59Z\"^^xsd:dateTime)
            ?plan cco:ont00001942 ?act ; rdfs:label ?plan_name .
        }} ORDER BY ?due_date
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("plan_name").map(String::as_str))
        .collect();

    assert_eq!(
        names,
        vec!["Buy groceries"],
        "only upcoming plan should appear; got: {:?}",
        names
    );
}

// ============================================================================
// Scheduled / agenda query
// ============================================================================

#[test]
fn scheduled_acts_on_or_before_date() {
    let (_, store) = user_flat_store();

    // "Write quarterly report" has hasScheduledDateTime 2026-04-17T09:00:00Z
    let sparql = format!(
        "
        PREFIX actions: <{ACTIONS}>
        PREFIX cco: <{CCO}>
        PREFIX rdfs: <{RDFS}>
        PREFIX xsd: <{XSD}>
        SELECT ?plan_name ?scheduled_at WHERE {{
            ?act a cco:ont00000228 ;
                 actions:hasScheduledDateTime ?scheduled_at ;
                 cco:ont00001868 ?status .
            FILTER(?status != <{ACTIONS}Completed> && ?status != <{ACTIONS}Cancelled>)
            FILTER(?scheduled_at <= \"2026-04-17T23:59:59Z\"^^xsd:dateTime)
            ?plan cco:ont00001942 ?act ; rdfs:label ?plan_name .
        }} ORDER BY ?scheduled_at
    "
    );

    let rows = query_raw(&store, &sparql).expect("query");
    let names: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("plan_name").map(String::as_str))
        .collect();

    assert_eq!(
        names,
        vec!["Write quarterly report"],
        "only scheduled-today plan should appear; got: {:?}",
        names
    );
}
