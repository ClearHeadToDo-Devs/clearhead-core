//! SPARQL queries and domain-object reconstruction from an Oxigraph store.
//!
//! This module owns the "read things out" direction: SPARQL → domain objects.
//! It also includes SHACL-as-SPARQL validation, since the mechanism is the same.

use super::{
    ACTIONS_NS, CCO_NS, CCO_PLAN, CCO_PLANNED_ACT, CCO_PRESCRIBES, CCO_STATUS_PROP, SCHEMA_NS,
    Store, actions_pred, cco_node, schema_pred,
};
use crate::domain::{ActPhase, Plan, PlannedAct};
use chrono::DateTime;
use oxigraph::model::{GraphName, NamedNode, NamedOrBlankNode, Term};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use std::collections::HashMap;
use uuid::Uuid;

// ============================================================================
// Legacy SPARQL utilities (used by lib.rs SQL-over-SPARQL layer)
// ============================================================================

/// Execute a SPARQL `SELECT ?id` query and return the matched ID strings.
pub fn query_actions(store: &Store, sparql: &str) -> Result<Vec<String>, String> {
    let results = SparqlEvaluator::new()
        .parse_query(sparql)
        .map_err(|e| e.to_string())?
        .on_store(store)
        .execute()
        .map_err(|e| e.to_string())?;

    let mut ids = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let s = solution.map_err(|e| e.to_string())?;
            if let Some(term) = s.get("id")
                && let Term::Literal(lit) = term
            {
                ids.push(lit.value().to_string());
            }
        }
    }
    Ok(ids)
}

/// Execute a raw SPARQL SELECT query and return all variable bindings as string maps.
pub fn query_raw(store: &Store, sparql: &str) -> Result<Vec<HashMap<String, String>>, String> {
    let results = SparqlEvaluator::new()
        .parse_query(sparql)
        .map_err(|e| e.to_string())?
        .on_store(store)
        .execute()
        .map_err(|e| e.to_string())?;

    match results {
        QueryResults::Solutions(solutions) => {
            let var_names: Vec<String> = solutions
                .variables()
                .iter()
                .map(|v| v.as_str().to_string())
                .collect();
            let mut rows = Vec::new();
            for solution in solutions {
                let s = solution.map_err(|e| e.to_string())?;
                let mut row = HashMap::new();
                for var_name in &var_names {
                    if let Some(term) = s.get(var_name.as_str()) {
                        let value = match term {
                            Term::NamedNode(nn) => nn.as_str().to_string(),
                            Term::Literal(lit) => lit.value().to_string(),
                            Term::BlankNode(bn) => format!("_:{}", bn.as_str()),
                        };
                        row.insert(var_name.clone(), value);
                    }
                }
                rows.push(row);
            }
            Ok(rows)
        }
        QueryResults::Boolean(_) => Err("ASK queries not supported; use SELECT".to_string()),
        QueryResults::Graph(_) => {
            Err("CONSTRUCT/DESCRIBE not supported; use SELECT".to_string())
        }
    }
}

/// Build a `SELECT *` SPARQL query from a WHERE clause with standard prefix injections.
pub fn build_raw_where_query(where_clause: &str) -> String {
    format!(
        "PREFIX actions: <{ACTIONS_NS}>\nPREFIX cco: <{CCO_NS}>\n\
         PREFIX schema: <{SCHEMA_NS}>\nPREFIX rdf: <{rdf}>\n\
         PREFIX xsd: <{xsd}>\nPREFIX skos: <{skos}>\n\
         SELECT * WHERE {{ {where_clause} }}",
        rdf = super::RDF_NS,
        xsd = super::XSD_NS,
        skos = super::SKOS_NS,
    )
}

/// Build a `SELECT ?id` SPARQL query from a WHERE clause with standard prefix injections.
pub fn build_where_query(where_clause: &str, _select: Option<&str>, _from: Option<&str>) -> String {
    format!(
        "PREFIX actions: <{actions_ns}>\n\
         PREFIX cco: <{cco_ns}>\n\
         PREFIX schema: <{schema_ns}>\n\
         PREFIX rdf: <{rdf_ns}>\n\
         PREFIX xsd: <{xsd_ns}>\n\
         PREFIX skos: <{skos_ns}>\n\
         SELECT ?id WHERE {{    ?s <{actions_ns}id> ?id .    {{ {where_clause} }}
}}",
        actions_ns = ACTIONS_NS,
        cco_ns = CCO_NS,
        schema_ns = SCHEMA_NS,
        rdf_ns = super::RDF_NS,
        xsd_ns = super::XSD_NS,
        skos_ns = super::SKOS_NS,
        where_clause = where_clause
    )
}

// ============================================================================
// Domain-centric queries
// ============================================================================

/// Query `Plan`s from a store using SPARQL.
///
/// The query should return `?id` (plan UUID as a string literal).
/// Each matching plan is reconstructed from the store.
pub fn query_plans(store: &Store, sparql: &str) -> Result<Vec<Plan>, String> {
    let ids = query_ids(store, sparql, "id")?;
    let mut plans = Vec::new();
    for id_str in ids {
        if let Ok(uuid) = Uuid::parse_str(&id_str) {
            if let Ok(plan) = get_plan_by_id(store, uuid) {
                plans.push(plan);
            }
        }
    }
    Ok(plans)
}

/// Query `PlannedAct`s from a store using SPARQL.
///
/// The query should return `?id` (act UUID as a string literal).
/// Each matching act is reconstructed from the store.
pub fn query_acts(store: &Store, sparql: &str) -> Result<Vec<PlannedAct>, String> {
    let ids = query_ids(store, sparql, "id")?;
    let mut acts = Vec::new();
    for id_str in ids {
        if let Ok(uuid) = Uuid::parse_str(&id_str) {
            if let Ok(act) = get_planned_act_by_id(store, uuid) {
                acts.push(act);
            }
        }
    }
    Ok(acts)
}

// ============================================================================
// SHACL-as-SPARQL Validation
// ============================================================================

/// Validate an Oxigraph store against key SHACL constraints from the Actions Vocabulary v4.1.0.
///
/// Oxigraph does not support SHACL natively, so this implements a subset of the
/// SHACL shapes from `shapes.ttl` as SPARQL queries.
///
/// Returns a list of human-readable violation strings. An empty list means the
/// store passes all checked constraints.
///
/// # Shapes validated
///
/// - `PlannedActStatusShape` (minCount 1): every PlannedAct must have a status
/// - `PlannedActStatusShape` (sh:in): status must be a known Event Status value
/// - `PlanPrescribesShape`: Plan `cco:prescribes` targets must be PlannedActs
pub fn validate_actions_vocabulary(store: &Store) -> Result<Vec<String>, String> {
    let mut violations = Vec::new();

    // Shape 1a: PlannedActStatusShape — minCount 1
    let q_missing_status = format!(
        "SELECT ?act WHERE {{ \
            ?act a <{cco}{planned_act}> . \
            FILTER NOT EXISTS {{ ?act <{cco}{status_prop}> ?s }} \
        }}",
        cco = CCO_NS,
        planned_act = CCO_PLANNED_ACT,
        status_prop = CCO_STATUS_PROP,
    );
    for uri in query_term_values(store, &q_missing_status, "act")? {
        violations.push(format!(
            "PlannedActStatusShape: <{uri}> is missing a status (cco:{prop})",
            prop = CCO_STATUS_PROP,
        ));
    }

    // Shape 1b: PlannedActStatusShape — sh:in (only known status values)
    let q_invalid_status = format!(
        "SELECT ?act WHERE {{ \
            ?act <{cco}{status_prop}> ?s . \
            FILTER (?s NOT IN ( \
                <{ns}NotStarted>, \
                <{ns}InProgress>, \
                <{ns}Completed>, \
                <{ns}Blocked>, \
                <{ns}Cancelled> \
            )) \
        }}",
        cco = CCO_NS,
        status_prop = CCO_STATUS_PROP,
        ns = ACTIONS_NS,
    );
    for uri in query_term_values(store, &q_invalid_status, "act")? {
        violations.push(format!(
            "PlannedActStatusShape (sh:in): <{uri}> has an unrecognized status value",
        ));
    }

    // Shape 2: PlanPrescribesShape
    let q_prescribes_wrong_target = format!(
        "SELECT ?plan WHERE {{ \
            ?plan a <{cco}{plan_cls}> . \
            ?plan <{cco}{prescribes}> ?target . \
            FILTER NOT EXISTS {{ ?target a <{cco}{planned_act}> }} \
        }}",
        cco = CCO_NS,
        plan_cls = CCO_PLAN,
        prescribes = CCO_PRESCRIBES,
        planned_act = CCO_PLANNED_ACT,
    );
    for uri in query_term_values(store, &q_prescribes_wrong_target, "plan")? {
        violations.push(format!(
            "PlanPrescribesShape: <{uri}> has a prescribes target that is not a PlannedAct",
        ));
    }

    Ok(violations)
}

// ============================================================================
// Private helpers
// ============================================================================

/// Generic SPARQL query returning string values of a named variable (literals or IRIs).
fn query_ids(store: &Store, sparql: &str, var_name: &str) -> Result<Vec<String>, String> {
    let results = SparqlEvaluator::new()
        .parse_query(sparql)
        .map_err(|e| e.to_string())?
        .on_store(store)
        .execute()
        .map_err(|e| e.to_string())?;

    let mut ids = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let s = solution.map_err(|e| e.to_string())?;
            if let Some(term) = s.get(var_name)
                && let Term::Literal(lit) = term
            {
                ids.push(lit.value().to_string());
            }
        }
    }
    Ok(ids)
}

/// Extract string values (IRI or literal) of a variable from SELECT results.
fn query_term_values(store: &Store, sparql: &str, var_name: &str) -> Result<Vec<String>, String> {
    let results = SparqlEvaluator::new()
        .parse_query(sparql)
        .map_err(|e| e.to_string())?
        .on_store(store)
        .execute()
        .map_err(|e| e.to_string())?;

    let mut values = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let s = solution.map_err(|e| e.to_string())?;
            if let Some(term) = s.get(var_name) {
                match term {
                    Term::NamedNode(nn) => values.push(nn.as_str().to_string()),
                    Term::Literal(lit) => values.push(lit.value().to_string()),
                    _ => {}
                }
            }
        }
    }
    Ok(values)
}

/// Reconstruct a `Plan` from the store by UUID.
fn get_plan_by_id(store: &Store, id: Uuid) -> Result<Plan, String> {
    let subject = NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let find_one = |pred: NamedNode| -> Option<Term> {
        store
            .quads_for_pattern(
                Some(subject.as_ref()),
                Some(pred.as_ref()),
                None,
                Some(graph.as_ref()),
            )
            .next()
            .map(|q| q.unwrap().object)
    };

    let find_many = |pred: NamedNode| -> Vec<Term> {
        store
            .quads_for_pattern(
                Some(subject.as_ref()),
                Some(pred.as_ref()),
                None,
                Some(graph.as_ref()),
            )
            .map(|q| q.unwrap().object)
            .collect()
    };

    let name = match find_one(schema_pred("name")) {
        Some(Term::Literal(l)) => l.value().to_string(),
        _ => return Err(format!("Plan {} missing name", id)),
    };

    let description = find_one(schema_pred("description")).and_then(|t| match t {
        Term::Literal(l) => Some(l.value().to_string()),
        _ => None,
    });

    let priority = find_one(actions_pred("hasPriority")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<u32>().ok(),
        _ => None,
    });

    let contexts: Vec<String> = find_many(actions_pred("hasContext"))
        .into_iter()
        .filter_map(|t| match t {
            Term::Literal(l) => Some(l.value().to_string()),
            _ => None,
        })
        .collect();

    let objective = find_one(actions_pred("hasObjective")).and_then(|t| match t {
        Term::Literal(l) => Some(l.value().to_string()),
        _ => None,
    });

    let parent = find_one(actions_pred("partOf")).and_then(|t| match t {
        Term::NamedNode(nn) => nn
            .as_str()
            .strip_prefix("urn:uuid:")
            .and_then(|s| Uuid::parse_str(s).ok()),
        _ => None,
    });

    let alias = find_one(actions_pred("alias")).and_then(|t| match t {
        Term::Literal(l) => Some(l.value().to_string()),
        _ => None,
    });

    let is_sequential = find_one(actions_pred("isSequential")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<bool>().ok(),
        _ => None,
    });

    let depends_on: Vec<Uuid> = find_many(actions_pred("dependsOn"))
        .into_iter()
        .filter_map(|t| match t {
            Term::NamedNode(nn) => nn
                .as_str()
                .strip_prefix("urn:uuid:")
                .and_then(|s| Uuid::parse_str(s).ok()),
            _ => None,
        })
        .collect();

    Ok(Plan {
        id,
        name,
        description,
        priority,
        contexts: if contexts.is_empty() { None } else { Some(contexts) },
        recurrence: None, // TODO: full recurrence hydration from blank node
        parent,
        objective,
        alias,
        is_sequential,
        depends_on: if depends_on.is_empty() { None } else { Some(depends_on) },
        duration: None, // TODO: duration hydration from graph
        acts: vec![],
    })
}

/// Reconstruct a `PlannedAct` from the store by UUID.
fn get_planned_act_by_id(store: &Store, id: Uuid) -> Result<PlannedAct, String> {
    let subject = NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let find_one = |pred: NamedNode| -> Option<Term> {
        store
            .quads_for_pattern(
                Some(subject.as_ref()),
                Some(pred.as_ref()),
                None,
                Some(graph.as_ref()),
            )
            .next()
            .map(|q| q.unwrap().object)
    };

    let plan_id = match find_one(actions_pred("prescribedBy")) {
        Some(Term::NamedNode(nn)) => nn
            .as_str()
            .strip_prefix("urn:uuid:")
            .and_then(|s| Uuid::parse_str(s).ok())
            .ok_or_else(|| format!("PlannedAct {} has invalid prescribedBy URI", id))?,
        _ => return Err(format!("PlannedAct {} missing prescribedBy", id)),
    };

    let phase = match find_one(cco_node(CCO_STATUS_PROP)) {
        Some(Term::NamedNode(nn)) => match nn.as_str().split('#').next_back() {
            Some("Completed") => ActPhase::Completed,
            Some("InProgress") => ActPhase::InProgress,
            Some("Blocked") => ActPhase::Blocked,
            Some("Cancelled") => ActPhase::Cancelled,
            _ => ActPhase::NotStarted,
        },
        _ => ActPhase::NotStarted,
    };

    let scheduled_at = find_one(actions_pred("hasDoDateTime")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    let duration = find_one(actions_pred("duration")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<u32>().ok(),
        _ => None,
    });

    let completed_at = find_one(actions_pred("hasCompletedDateTime")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    let created_at = find_one(actions_pred("createdAt")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    Ok(PlannedAct {
        id,
        plan_id,
        phase,
        scheduled_at,
        duration,
        completed_at,
        created_at,
    })
}
