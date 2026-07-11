//! SPARQL query execution against an Oxigraph store.

use super::{
    ACTIONS_ACTION, ACTIONS_NS, BFO_NS, CCO_IS_SUCCESSOR_OF, CCO_NS, CCO_PLAN,
    CCO_PRESCRIBES, CCO_STATUS_PROP, GraphError, Result, Store,
};
use oxigraph::model::Term;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use std::collections::HashMap;

type Row = HashMap<String, String>;

pub fn query_action_ids(store: &Store, sparql: &str) -> Result<Vec<String>> {
    query_ids(store, sparql, "id")
}

pub fn query_raw(store: &Store, sparql: &str) -> Result<Vec<Row>> {
    execute_select_rows(store, sparql)
}

pub fn build_raw_where_query(where_clause: &str) -> String {
    format!(
        "PREFIX actions: <{ACTIONS_NS}>\nPREFIX cco: <{CCO_NS}>\n\
         PREFIX bfo: <{bfo}>\nPREFIX rdfs: <{rdfs}>\nPREFIX rdf: <{rdf}>\n\
         PREFIX xsd: <{xsd}>\nPREFIX skos: <{skos}>\n\
         SELECT * WHERE {{ GRAPH ?g {{ {where_clause} }} }}",
        bfo = BFO_NS,
        rdfs = "http://www.w3.org/2000/01/rdf-schema#",
        rdf = super::RDF_NS,
        xsd = super::XSD_NS,
        skos = super::SKOS_NS,
    )
}

pub fn build_where_query(where_clause: &str, _select: Option<&str>, _from: Option<&str>) -> String {
    format!(
        "PREFIX actions: <{actions_ns}>\n\
         PREFIX cco: <{cco_ns}>\n\
         PREFIX bfo: <{bfo_ns}>\n\
         PREFIX rdfs: <{rdfs_ns}>\n\
         PREFIX rdf: <{rdf_ns}>\n\
         PREFIX xsd: <{xsd_ns}>\n\
         PREFIX skos: <{skos_ns}>\n\
         SELECT ?id WHERE {{ GRAPH ?g {{ ?s <{actions_ns}hasUUID> ?id . {{ {where_clause} }} }} }}",
        actions_ns = ACTIONS_NS,
        cco_ns = CCO_NS,
        bfo_ns = BFO_NS,
        rdfs_ns = "http://www.w3.org/2000/01/rdf-schema#",
        rdf_ns = super::RDF_NS,
        xsd_ns = super::XSD_NS,
        skos_ns = super::SKOS_NS,
    )
}

pub fn validate_actions_vocabulary(store: &Store) -> Result<Vec<String>> {
    let mut violations = Vec::new();

    // ── Status checks ──────────────────────────────────────────────────────

    let q_missing_status = format!(
        "SELECT ?action WHERE {{ GRAPH ?g {{ \
            ?action a <{actions}{action}> . \
            FILTER NOT EXISTS {{ ?action <{cco}{status_prop}> ?s }} \
        }} }}",
        actions = ACTIONS_NS,
        action = ACTIONS_ACTION,
        cco = CCO_NS,
        status_prop = CCO_STATUS_PROP,
    );
    for uri in query_term_values(store, &q_missing_status, "action")? {
        violations.push(format!(
            "ActionStatusShape: <{uri}> is missing a status (cco:{prop})",
            prop = CCO_STATUS_PROP,
        ));
    }

    let q_invalid_status = format!(
        "SELECT ?action WHERE {{ GRAPH ?g {{ \
            ?action <{cco}{status_prop}> ?s . \
            FILTER (?s NOT IN ( \
                <{ns}NotStarted>, \
                <{ns}InProgress>, \
                <{ns}Completed>, \
                <{ns}Blocked>, \
                <{ns}Cancelled> \
            )) \
        }} }}",
        cco = CCO_NS,
        status_prop = CCO_STATUS_PROP,
        ns = ACTIONS_NS,
    );
    for uri in query_term_values(store, &q_invalid_status, "action")? {
        violations.push(format!(
            "ActionStatusShape (sh:in): <{uri}> has an unrecognized status value",
        ));
    }

    // ── Plan → Action prescribes target type ───────────────────────────────

    let q_prescribes_wrong_target = format!(
        "SELECT ?plan WHERE {{ GRAPH ?g {{ \
            ?plan a <{cco}{plan_cls}> . \
            ?plan <{cco}{prescribes}> ?target . \
            FILTER NOT EXISTS {{ ?target a <{actions}{action}> }} \
        }} }}",
        actions = ACTIONS_NS,
        action = ACTIONS_ACTION,
        cco = CCO_NS,
        plan_cls = CCO_PLAN,
        prescribes = CCO_PRESCRIBES,
    );
    for uri in query_term_values(store, &q_prescribes_wrong_target, "plan")? {
        violations.push(format!(
            "PlanPrescribesShape: <{uri}> has a prescribes target that is not an Action",
        ));
    }

    // ── UUID shape ─────────────────────────────────────────────────────────
    // Every Action and Plan must carry an actions:hasUUID literal whose value
    // matches the UUID in its subject IRI (urn:uuid:<uuid>).

    let q_missing_uuid = format!(
        "SELECT ?node WHERE {{ GRAPH ?g {{ \
            {{ ?node a <{actions}{action}> }} UNION {{ ?node a <{cco}{plan_cls}> }} \
            FILTER NOT EXISTS {{ ?node <{actions}hasUUID> ?u }} \
        }} }}",
        actions = ACTIONS_NS,
        action = ACTIONS_ACTION,
        cco = CCO_NS,
        plan_cls = CCO_PLAN,
    );
    for uri in query_term_values(store, &q_missing_uuid, "node")? {
        violations.push(format!("UUIDShape: <{uri}> is missing an actions:hasUUID literal"));
    }

    // ── Completed actions must have a completion date ──────────────────────

    let q_completed_no_date = format!(
        "SELECT ?action WHERE {{ GRAPH ?g {{ \
            ?action a <{actions}{action}> ; \
                 <{cco}{status}> <{actions}Completed> . \
            FILTER NOT EXISTS {{ ?action <{actions}hasCompletedDateTime> ?d }} \
        }} }}",
        actions = ACTIONS_NS,
        action = ACTIONS_ACTION,
        cco = CCO_NS,
        status = CCO_STATUS_PROP,
    );
    for uri in query_term_values(store, &q_completed_no_date, "action")? {
        violations.push(format!(
            "CompletedDateShape: <{uri}> has status Completed but no hasCompletedDateTime"
        ));
    }

    // ── Recurrence requires a scheduled anchor ────────────────────────────
    // A Plan with hasRecurrenceRule must also have a scheduled anchor
    // (hasScheduledDateTime) so occurrence expansion has a DTSTART.

    let q_recurrence_no_anchor = format!(
        "SELECT ?plan WHERE {{ GRAPH ?g {{ \
            ?plan a <{cco}{plan_cls}> ; \
                  <{actions}hasRecurrenceRule> ?rrule . \
            FILTER NOT EXISTS {{ ?plan <{actions}hasScheduledDateTime> ?dt }} \
        }} }}",
        actions = ACTIONS_NS,
        cco = CCO_NS,
        plan_cls = CCO_PLAN,
    );
    for uri in query_term_values(store, &q_recurrence_no_anchor, "plan")? {
        violations.push(format!(
            "RecurrenceAnchorShape: <{uri}> has hasRecurrenceRule but no hasScheduledDateTime anchor"
        ));
    }

    // ── Successor-cycle detection ─────────────────────────────────────────
    // An Action must not be its own successor (direct self-loop).
    // Transitive cycles are not checked here (would require recursion in SPARQL 1.1).

    let q_self_successor = format!(
        "SELECT ?action WHERE {{ GRAPH ?g {{ \
            ?action <{cco}{successor}> ?action . \
        }} }}",
        cco = CCO_NS,
        successor = CCO_IS_SUCCESSOR_OF,
    );
    for uri in query_term_values(store, &q_self_successor, "action")? {
        violations.push(format!(
            "SuccessorCycleShape: <{uri}> is its own successor (self-loop)"
        ));
    }

    // ── Alias uniqueness within a named graph ────────────────────────────
    // Two different actions in the same graph must not share an alias.

    let q_duplicate_alias = format!(
        "SELECT ?alias WHERE {{ GRAPH ?g {{ \
            ?a1 <{actions}hasAlias> ?alias . \
            ?a2 <{actions}hasAlias> ?alias . \
            FILTER (?a1 != ?a2) \
        }} }}",
        actions = ACTIONS_NS,
    );
    for alias in query_term_values(store, &q_duplicate_alias, "alias")? {
        violations.push(format!(
            "AliasUniquenessShape: alias '{alias}' is shared by more than one action in the same graph"
        ));
    }

    Ok(violations)
}

fn query_ids(store: &Store, sparql: &str, var_name: &str) -> Result<Vec<String>> {
    Ok(execute_select_rows(store, sparql)?
        .into_iter()
        .filter_map(|row| row.get(var_name).cloned())
        .collect())
}

fn query_term_values(store: &Store, sparql: &str, var_name: &str) -> Result<Vec<String>> {
    query_ids(store, sparql, var_name)
}

fn execute_select_rows(store: &Store, sparql: &str) -> Result<Vec<Row>> {
    // When the query has no FROM / FROM NAMED clauses, enable the union default
    // graph so triple patterns without an explicit GRAPH clause match across all
    // named graphs.  Named queries (next-actions, high-priority, …) written
    // without GRAPH work transparently; `GRAPH ?g { … }` still enumerates named
    // graphs for workspace-scoped and cross-workspace identity queries.
    let mut prepared = SparqlEvaluator::new()
        .parse_query(sparql)
        .map_err(|e| GraphError::Query(e.to_string()))?;

    if prepared.dataset().is_default_dataset() {
        prepared.dataset_mut().set_default_graph_as_union();
    }

    let results = prepared
        .on_store(store)
        .execute()
        .map_err(|e| GraphError::Query(e.to_string()))?;

    match results {
        QueryResults::Solutions(solutions) => {
            let var_names: Vec<String> = solutions
                .variables()
                .iter()
                .map(|v| v.as_str().to_string())
                .collect();
            let mut rows = Vec::new();
            for solution in solutions {
                let solution = solution.map_err(|e| GraphError::Query(e.to_string()))?;
                let mut row = HashMap::new();
                for var_name in &var_names {
                    if let Some(term) = solution.get(var_name.as_str()) {
                        row.insert(var_name.clone(), stringify_term(term));
                    }
                }
                rows.push(row);
            }
            Ok(rows)
        }
        QueryResults::Boolean(_) => Err(GraphError::Query(
            "ASK queries not supported; use SELECT".to_string(),
        )),
        QueryResults::Graph(_) => Err(GraphError::Query(
            "CONSTRUCT/DESCRIBE not supported; use SELECT".to_string(),
        )),
    }
}

fn stringify_term(term: &Term) -> String {
    match term {
        Term::NamedNode(nn) => nn.as_str().to_string(),
        Term::Literal(lit) => lit.value().to_string(),
        Term::BlankNode(bn) => format!("_:{}", bn.as_str()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::{create_store, load_turtle_into_graph, GraphName, TRANSIENT_GRAPH_URI};
    use oxigraph::model::NamedNode;

    fn transient_graph() -> GraphName {
        GraphName::NamedNode(NamedNode::new(TRANSIENT_GRAPH_URI).unwrap())
    }

    #[test]
    fn build_raw_where_query_injects_standard_prefixes() {
        let q = build_raw_where_query("?s ?p ?o .");
        assert!(q.contains("PREFIX actions:"));
        assert!(q.contains("PREFIX cco:"));
        assert!(q.contains("SELECT * WHERE"));
    }

    #[test]
    fn query_action_ids_reads_id_literal_bindings() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-1111-7111-8111-111111111111> a cco:{plan} ;\n\
               actions:hasUUID \"019d7100-1111-7111-8111-111111111111\" .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan = CCO_PLAN,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");

        let sparql = format!(
            "SELECT ?id WHERE {{ ?s a <{cco}{plan}> ; <{actions}hasUUID> ?id . }}",
            cco = CCO_NS,
            plan = CCO_PLAN,
            actions = ACTIONS_NS,
        );

        let ids = query_action_ids(&store, &sparql).expect("query ids");
        assert_eq!(ids, vec!["019d7100-1111-7111-8111-111111111111"]);
    }

    #[test]
    fn validate_actions_detects_missing_status() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
              <urn:uuid:019d7100-2222-7222-8222-222222222222> a actions:{action} ;\n\
                actions:hasUUID \"019d7100-2222-7222-8222-222222222222\" ;\n\
                cco:{prescribed_by} <urn:uuid:019d7100-1111-7111-8111-111111111111> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            prescribed_by = super::super::CCO_PRESCRIBED_BY,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(violations.iter().any(|v| v.contains("missing a status")));
    }

    #[test]
    fn raw_query_errors_on_boolean_queries() {
        let store = create_store().expect("store");
        let err = query_raw(&store, "ASK { ?s ?p ?o }").expect_err("expected error");
        assert!(err.to_string().contains("ASK queries not supported"));
    }

    #[test]
    fn validates_prescribes_target_type() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
             <urn:uuid:019d7100-1111-7111-8111-111111111111> a cco:{plan_class} ;\n\
               actions:hasUUID \"019d7100-1111-7111-8111-111111111111\" ;\n\
               rdfs:label \"Plan\" ;\n\
               cco:{prescribes} <urn:uuid:019d7100-9999-7999-8999-999999999999> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan_class = CCO_PLAN,
            prescribes = CCO_PRESCRIBES,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(violations.iter().any(|v| v.contains("PlanPrescribesShape")));
    }

    #[test]
    fn validates_missing_uuid_literal() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-3333-7333-8333-333333333333> a actions:{action} ;\n\
               cco:{status} <{actions}NotStarted> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("UUIDShape")),
            "expected UUIDShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_completed_action_requires_date() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-4444-7444-8444-444444444444> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-4444-7444-8444-444444444444\" ;\n\
               cco:{status} <{actions}Completed> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("CompletedDateShape")),
            "expected CompletedDateShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_recurrence_requires_scheduled_anchor() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-5555-7555-8555-555555555555> a cco:{plan_cls} ;\n\
               actions:hasUUID \"019d7100-5555-7555-8555-555555555555\" ;\n\
               actions:hasRecurrenceRule \"FREQ=WEEKLY\" .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan_cls = CCO_PLAN,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("RecurrenceAnchorShape")),
            "expected RecurrenceAnchorShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_self_successor_cycle() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-6666-7666-8666-666666666666> cco:{successor} \
               <urn:uuid:019d7100-6666-7666-8666-666666666666> .\n",
            cco = CCO_NS,
            successor = CCO_IS_SUCCESSOR_OF,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("SuccessorCycleShape")),
            "expected SuccessorCycleShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_duplicate_alias() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-7777-7777-8777-777777777771> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-7777-7777-8777-777777777771\" ;\n\
               actions:hasAlias \"shared-alias\" ;\n\
               cco:{status} <{actions}NotStarted> .\n\
             <urn:uuid:019d7100-7777-7777-8777-777777777772> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-7777-7777-8777-777777777772\" ;\n\
               actions:hasAlias \"shared-alias\" ;\n\
               cco:{status} <{actions}NotStarted> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("AliasUniquenessShape")),
            "expected AliasUniquenessShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn valid_completed_action_with_date_passes() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\
             <urn:uuid:019d7100-8888-7888-8888-888888888888> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-8888-7888-8888-888888888888\" ;\n\
               cco:{status} <{actions}Completed> ;\n\
               actions:hasCompletedDateTime \"2026-05-01T10:00:00Z\"^^xsd:dateTime .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        // Only UUID shape fires (no hasUUID here — not the point of this test)
        assert!(
            !violations.iter().any(|v| v.contains("CompletedDateShape")),
            "completed action with date should not fire CompletedDateShape"
        );
    }
}

#[cfg(test)]
mod validation_tests {
    use super::*;
    use crate::graph::{create_store, load_turtle_into_graph, GraphName, TRANSIENT_GRAPH_URI};
    use oxigraph::model::NamedNode;

    fn transient_graph() -> GraphName {
        GraphName::NamedNode(NamedNode::new(TRANSIENT_GRAPH_URI).unwrap())
    }

    #[test]
    fn validates_missing_uuid_literal() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-3333-7333-8333-333333333333> a actions:{action} ;\n\
               cco:{status} <{actions}NotStarted> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("UUIDShape")),
            "expected UUIDShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_completed_action_requires_date() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-4444-7444-8444-444444444444> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-4444-7444-8444-444444444444\" ;\n\
               cco:{status} <{actions}Completed> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("CompletedDateShape")),
            "expected CompletedDateShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_recurrence_requires_scheduled_anchor() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-5555-7555-8555-555555555555> a cco:{plan_cls} ;\n\
               actions:hasUUID \"019d7100-5555-7555-8555-555555555555\" ;\n\
               actions:hasRecurrenceRule \"FREQ=WEEKLY\" .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            plan_cls = CCO_PLAN,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("RecurrenceAnchorShape")),
            "expected RecurrenceAnchorShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_self_successor_cycle() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-6666-7666-8666-666666666666> cco:{successor} \
               <urn:uuid:019d7100-6666-7666-8666-666666666666> .\n",
            cco = CCO_NS,
            successor = CCO_IS_SUCCESSOR_OF,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("SuccessorCycleShape")),
            "expected SuccessorCycleShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn validates_duplicate_alias() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             <urn:uuid:019d7100-7777-7777-8777-777777777771> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-7777-7777-8777-777777777771\" ;\n\
               actions:hasAlias \"shared-alias\" ;\n\
               cco:{status} <{actions}NotStarted> .\n\
             <urn:uuid:019d7100-7777-7777-8777-777777777772> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-7777-7777-8777-777777777772\" ;\n\
               actions:hasAlias \"shared-alias\" ;\n\
               cco:{status} <{actions}NotStarted> .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            violations.iter().any(|v| v.contains("AliasUniquenessShape")),
            "expected AliasUniquenessShape violation, got: {violations:?}"
        );
    }

    #[test]
    fn valid_completed_action_with_date_passes() {
        let store = create_store().expect("store");
        let ttl = format!(
            "@prefix actions: <{actions}> .\n\
             @prefix cco: <{cco}> .\n\
             @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\
             <urn:uuid:019d7100-8888-7888-8888-888888888888> a actions:{action} ;\n\
               actions:hasUUID \"019d7100-8888-7888-8888-888888888888\" ;\n\
               cco:{status} <{actions}Completed> ;\n\
               actions:hasCompletedDateTime \"2026-05-01T10:00:00Z\"^^xsd:dateTime .\n",
            actions = ACTIONS_NS,
            cco = CCO_NS,
            action = ACTIONS_ACTION,
            status = CCO_STATUS_PROP,
        );
        load_turtle_into_graph(&store, &ttl, transient_graph()).expect("load turtle");
        let violations = validate_actions_vocabulary(&store).expect("validate");
        assert!(
            !violations.iter().any(|v| v.contains("CompletedDateShape")),
            "completed action with date should not fire CompletedDateShape"
        );
    }
}

#[cfg(test)]
mod oxigraph_graph_var_probe {
    use crate::graph::{create_store, load_turtle_into_graph, workspace_graph_uri, GraphName, query_raw};

    #[test]
    fn union_default_graph_means_no_graph_clause_matches_named_graphs() {
        let store = create_store().unwrap();
        let gn = GraphName::NamedNode(workspace_graph_uri("probe-uuid"));
        let ttl = r#"
@prefix actions: <https://clearhead.us/vocab/actions/v4#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
<urn:uuid:bbbbbbbb-0000-7000-8000-000000000001> a actions:Action ; rdfs:label "Probe" .
"#;
        load_turtle_into_graph(&store, ttl, gn).unwrap();

        let r1 = query_raw(&store, "PREFIX actions: <https://clearhead.us/vocab/actions/v4#> SELECT ?s WHERE { ?s a actions:Action }").unwrap();
        assert_eq!(r1.len(), 1, "union default graph: no-GRAPH clause should find action in named graph; got {:?}", r1);

        let r2 = query_raw(&store, "PREFIX actions: <https://clearhead.us/vocab/actions/v4#> SELECT ?g ?s WHERE { GRAPH ?g { ?s a actions:Action } }").unwrap();
        assert_eq!(r2.len(), 1, "GRAPH ?g should find action in named graph; got {:?}", r2);
        assert_eq!(r2[0]["g"], "urn:clearhead:workspace:probe-uuid");
    }
}
