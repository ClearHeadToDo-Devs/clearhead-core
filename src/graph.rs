//! RDF graph module for storing and querying actions using Oxigraph
//!
//! This module implements the RDF schema from the Actions Vocabulary v4 ontology.
//!
//! # Architecture
//!
//! Actions are loaded into an in-memory Oxigraph store as RDF triples.
//! The domain model (Plan, PlannedAct) maps to CCO-aligned classes.
//!
//! # Usage
//!
//! ```ignore
//! use clearhead_cli::graph;
//!
//! // Create store and load actions
//! let store = graph::create_store()?;
//! graph::load_actions(&store, &actions)?;
//!
//! // Query for action IDs
//! let ids = graph::query_actions(&store, "SELECT ?id WHERE { ... }")?;
//! ```

use crate::domain::{ActPhase, DomainModel, Plan, PlannedAct};
use chrono::DateTime;
use oxigraph::io::RdfFormat;
use oxigraph::model::{
    BlankNode, GraphName, GraphNameRef, Literal, NamedNode, NamedOrBlankNode, Quad, Term,
};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
pub use oxigraph::store::Store;
use std::collections::HashMap;
use uuid::Uuid;

// Namespace constants
const ACTIONS_NS: &str = "https://clearhead.us/vocab/actions/v4#";
const CCO_NS: &str = "https://www.commoncoreontologies.org/";
const SCHEMA_NS: &str = "http://schema.org/";
const RDF_NS: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";
const SKOS_NS: &str = "http://www.w3.org/2004/02/skos/core#";

// CCO class and property identifiers (opaque OBO IDs from the CCO ontology)
const CCO_PLAN: &str = "ont00000974";
const CCO_PLANNED_ACT: &str = "ont00000228";
const CCO_PRESCRIBES: &str = "ont00001942";
const CCO_STATUS_PROP: &str = "ont00001868"; // is_measured_by_nominal

/// Create an in-memory Oxigraph store
pub fn create_store() -> Result<Store, String> {
    Store::new().map_err(|e| e.to_string())
}

// Legacy alias for compatibility during refactor
pub fn create_database() -> Result<Store, String> {
    create_store()
}

fn ns(base: &str, name: &str) -> NamedNode {
    NamedNode::new(format!("{}{}", base, name)).unwrap()
}

fn actions_pred(name: &str) -> NamedNode {
    ns(ACTIONS_NS, name)
}

fn cco_node(id: &str) -> NamedNode {
    ns(CCO_NS, id)
}

fn schema_pred(name: &str) -> NamedNode {
    ns(SCHEMA_NS, name)
}

fn rdf_type() -> NamedNode {
    ns(RDF_NS, "type")
}

/// Execute a SPARQL query and return matching action IDs
///
/// The query should SELECT the `?id` variable.
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

/// Build a SPARQL query from a WHERE clause
///
/// Wraps the where_clause in `SELECT ?id WHERE { ... }` and injects standard prefixes.
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
        rdf_ns = RDF_NS,
        xsd_ns = XSD_NS,
        skos_ns = SKOS_NS,
        where_clause = where_clause
    )
}

/// Load tag hierarchies into the RDF store
///
/// # Arguments
/// * `store` - The RDF store
/// * `tag_hierarchies` - Map of parent tag to child tags (e.g., "work" -> ["meeting", "coding"])
pub fn load_tag_hierarchies(
    store: &Store,
    tag_hierarchies: &HashMap<String, Vec<String>>,
) -> Result<(), String> {
    for (parent, children) in tag_hierarchies {
        for child in children {
            // child skos:broader parent
            // We use simple literals or URIs for tags?
            // Spec doesn't strictly define tag URIs, so we'll use a local scheme or literals.
            // But skos:broader relates Concepts (Resources).
            // Let's use `urn:tag:name`.

            let parent_uri = NamedNode::new(format!("urn:tag:{}", parent.to_lowercase())).unwrap();
            let child_uri = NamedNode::new(format!("urn:tag:{}", child.to_lowercase())).unwrap();

            store
                .insert(&Quad::new(
                    NamedOrBlankNode::NamedNode(child_uri),
                    NamedNode::new(format!("{}broader", SKOS_NS)).unwrap(),
                    Term::NamedNode(parent_uri),
                    GraphName::DefaultGraph,
                ))
                .map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}

/// Query plans by context with hierarchy expansion
pub fn query_actions_by_context(store: &Store, context: &str) -> Result<Vec<String>, String> {
    let context_tag = context.to_lowercase();

    let sparql = format!(
        "SELECT ?id WHERE {{    ?s <{actions_ns}id> ?id .    ?s <{actions_ns}hasContext> ?tagLiteral .    BIND(IRI(concat(\"urn:tag:\", lcase(?tagLiteral))) AS ?tagURI)    ?tagURI <{skos_ns}broader>* <urn:tag:{target}> .
}}",
        actions_ns = ACTIONS_NS,
        skos_ns = SKOS_NS,
        target = context_tag
    );

    query_actions(store, &sparql)
}

/// Query plans by objective (project)
pub fn query_actions_by_project(store: &Store, project: &str) -> Result<Vec<String>, String> {
    let sparql = format!(
        "SELECT ?id WHERE {{    ?s <{actions_ns}id> ?id .    ?s <{actions_ns}hasObjective> \"{project}\" .
        }}",
        actions_ns = ACTIONS_NS,
        project = project
    );

    query_actions(store, &sparql)
}

// ============================================================================
// Domain Model (CCO-aligned)
// ============================================================================

fn phase_node(phase: &ActPhase) -> NamedNode {
    let name = match phase {
        ActPhase::NotStarted => "NotStarted",
        ActPhase::InProgress => "InProgress",
        ActPhase::Completed => "Completed",
        ActPhase::Blocked => "Blocked",
        ActPhase::Cancelled => "Cancelled",
    };
    actions_pred(name)
}

/// Load a DomainModel into the store using v4 ontology
///
/// This inserts Plans and PlannedActs as separate entities with proper
/// CCO-aligned types and relationships.
pub fn load_domain_model(store: &Store, model: &DomainModel) -> Result<(), String> {
    for plan in model.all_plans() {
        insert_plan(store, plan)?;
    }
    for act in model.all_acts() {
        insert_planned_act(store, act)?;
    }
    Ok(())
}

/// Insert a Plan into the store
///
/// Maps to cco:Plan - information content that defines a task.
fn insert_plan(store: &Store, plan: &Plan) -> Result<(), String> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", plan.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| e.to_string())
    };

    // rdf:type cco:Plan (ont00000974)
    add(rdf_type(), Term::NamedNode(cco_node(CCO_PLAN)))?;

    // actions:id
    add(
        actions_pred("id"),
        Term::Literal(Literal::new_simple_literal(plan.id.to_string())),
    )?;

    // schema:name
    add(
        schema_pred("name"),
        Term::Literal(Literal::new_simple_literal(&plan.name)),
    )?;

    // schema:description
    if let Some(desc) = &plan.description {
        add(
            schema_pred("description"),
            Term::Literal(Literal::new_simple_literal(desc)),
        )?;
    }

    // actions:hasPriority
    if let Some(priority) = plan.priority {
        add(
            actions_pred("hasPriority"),
            Term::Literal(Literal::new_typed_literal(
                priority.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    // actions:hasContext (multiple)
    if let Some(contexts) = &plan.contexts {
        for context in contexts {
            add(
                actions_pred("hasContext"),
                Term::Literal(Literal::new_simple_literal(context)),
            )?;
        }
    }

    // actions:hasObjective (story/project)
    if let Some(objective) = &plan.objective {
        add(
            actions_pred("hasObjective"),
            Term::Literal(Literal::new_simple_literal(objective)),
        )?;
    }

    // actions:partOf (parent plan)
    if let Some(parent_id) = plan.parent {
        let parent_uri = NamedNode::new(format!("urn:uuid:{}", parent_id)).unwrap();
        add(actions_pred("partOf"), Term::NamedNode(parent_uri))?;
    }

    // actions:dependsOn (predecessor plans)
    if let Some(deps) = &plan.depends_on {
        for dep_id in deps {
            let dep_uri = NamedNode::new(format!("urn:uuid:{}", dep_id)).unwrap();
            add(actions_pred("dependsOn"), Term::NamedNode(dep_uri))?;
        }
    }

    // actions:alias
    if let Some(alias) = &plan.alias {
        add(
            actions_pred("alias"),
            Term::Literal(Literal::new_simple_literal(alias)),
        )?;
    }

    // actions:isSequential
    if let Some(true) = plan.is_sequential {
        add(
            actions_pred("isSequential"),
            Term::Literal(Literal::new_typed_literal("true", ns(XSD_NS, "boolean"))),
        )?;
    }

    // Recurrence (as blank node)
    if let Some(recurrence) = &plan.recurrence {
        let bnode = BlankNode::default();
        add(actions_pred("hasRecurrence"), Term::BlankNode(bnode.clone()))?;

        let r_subj = NamedOrBlankNode::BlankNode(bnode);
        let add_r = |pred: NamedNode, term: Term| {
            store
                .insert(&Quad::new(r_subj.clone(), pred, term, graph.clone()))
                .map_err(|e| e.to_string())
        };

        add_r(
            actions_pred("frequency"),
            Term::Literal(Literal::new_simple_literal(&recurrence.frequency)),
        )?;

        if let Some(interval) = recurrence.interval {
            add_r(
                actions_pred("interval"),
                Term::Literal(Literal::new_typed_literal(
                    interval.to_string(),
                    ns(XSD_NS, "integer"),
                )),
            )?;
        }

        if let Some(by_day) = &recurrence.by_day {
            for day in by_day {
                add_r(
                    actions_pred("byDay"),
                    Term::Literal(Literal::new_simple_literal(day)),
                )?;
            }
        }
    }

    // cco:prescribes (ont00001942) — forward link from Plan to each PlannedAct
    for act in &plan.acts {
        let act_uri = NamedNode::new(format!("urn:uuid:{}", act.id)).unwrap();
        add(cco_node(CCO_PRESCRIBES), Term::NamedNode(act_uri))?;
    }

    Ok(())
}

/// Insert a PlannedAct into the store
///
/// Maps to cco:PlannedAct - an occurrence that realizes a Plan.
fn insert_planned_act(store: &Store, act: &PlannedAct) -> Result<(), String> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", act.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| e.to_string())
    };

    // rdf:type cco:PlannedAct (ont00000228)
    add(rdf_type(), Term::NamedNode(cco_node(CCO_PLANNED_ACT)))?;

    // actions:id
    add(
        actions_pred("id"),
        Term::Literal(Literal::new_simple_literal(act.id.to_string())),
    )?;

    // actions:prescribedBy (convenience inverse of cco:prescribes for efficient lookup)
    let plan_uri = NamedNode::new(format!("urn:uuid:{}", act.plan_id)).unwrap();
    add(actions_pred("prescribedBy"), Term::NamedNode(plan_uri))?;

    // cco:is_measured_by_nominal (ont00001868) — status as Event Status Nominal ICE
    add(cco_node(CCO_STATUS_PROP), Term::NamedNode(phase_node(&act.phase)))?;

    // actions:scheduledAt
    if let Some(dt) = &act.scheduled_at {
        add(
            actions_pred("scheduledAt"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    // actions:duration
    if let Some(duration) = act.duration {
        add(
            actions_pred("duration"),
            Term::Literal(Literal::new_typed_literal(
                duration.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    // actions:completedAt
    if let Some(dt) = &act.completed_at {
        add(
            actions_pred("completedAt"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    // actions:createdAt
    if let Some(dt) = &act.created_at {
        add(
            actions_pred("createdAt"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    Ok(())
}

// ============================================================================
// Turtle Serialization
// ============================================================================

/// Serialize all PlannedActs from a DomainModel to Turtle format.
///
/// Loads the full model (Plans + Acts) into a temporary store,
/// then serializes the default graph to Turtle.
pub fn serialize_acts_to_turtle(model: &DomainModel) -> Result<String, String> {
    let store = create_store()?;
    load_domain_model(&store, model)?;
    store_to_turtle(&store)
}

/// Serialize only completed/cancelled acts (and their plans) to Turtle format.
///
/// Useful for generating a "closed acts" archive file.
pub fn serialize_closed_acts_to_turtle(model: &DomainModel) -> Result<String, String> {
    let filtered = filter_model_by_phase(model, |phase| {
        matches!(phase, ActPhase::Completed | ActPhase::Cancelled)
    });
    let store = create_store()?;
    load_domain_model(&store, &filtered)?;
    store_to_turtle(&store)
}

/// Serialize only open (non-completed, non-cancelled) acts to Turtle format.
///
/// Useful for generating an "upcoming acts" file.
pub fn serialize_open_acts_to_turtle(model: &DomainModel) -> Result<String, String> {
    let filtered = filter_model_by_phase(model, |phase| {
        !matches!(phase, ActPhase::Completed | ActPhase::Cancelled)
    });
    let store = create_store()?;
    load_domain_model(&store, &filtered)?;
    store_to_turtle(&store)
}

/// Filter a DomainModel to only include acts matching the predicate,
/// preserving the charter → plan → act hierarchy.
fn filter_model_by_phase(
    model: &DomainModel,
    predicate: impl Fn(&ActPhase) -> bool,
) -> DomainModel {
    let mut filtered_charters = Vec::new();

    for charter in &model.charters {
        let mut filtered_plans = Vec::new();
        for plan in &charter.plans {
            let filtered_acts: Vec<PlannedAct> = plan
                .acts
                .iter()
                .filter(|a| predicate(&a.phase))
                .cloned()
                .collect();
            if !filtered_acts.is_empty() {
                let mut filtered_plan = plan.clone();
                filtered_plan.acts = filtered_acts;
                filtered_plans.push(filtered_plan);
            }
        }
        if !filtered_plans.is_empty() {
            let mut filtered_charter = charter.clone();
            filtered_charter.plans = filtered_plans;
            filtered_charters.push(filtered_charter);
        }
    }

    DomainModel {
        objectives: model.objectives.clone(),
        charters: filtered_charters,
    }
}

/// Serialize an Oxigraph store's default graph to Turtle.
fn store_to_turtle(store: &Store) -> Result<String, String> {
    let mut buffer = Vec::new();
    store
        .dump_graph_to_writer(GraphNameRef::DefaultGraph, RdfFormat::Turtle, &mut buffer)
        .map_err(|e| format!("Failed to serialize to Turtle: {}", e))?;
    String::from_utf8(buffer).map_err(|e| format!("Invalid UTF-8 in Turtle output: {}", e))
}

// ============================================================================
// Domain-centric Query Functions
// ============================================================================

/// Query Plans from a store using SPARQL.
///
/// The query should return `?id` (plan UUID as string literal).
/// Each matching plan is reconstructed from the store.
pub fn query_plans(store: &Store, sparql: &str) -> Result<Vec<Plan>, String> {
    let ids = query_action_ids(store, sparql, "id")?;
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

/// Query PlannedActs from a store using SPARQL.
///
/// The query should return `?id` (act UUID as string literal).
/// Each matching act is reconstructed from the store.
pub fn query_acts(store: &Store, sparql: &str) -> Result<Vec<PlannedAct>, String> {
    let ids = query_action_ids(store, sparql, "id")?;
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

/// Generic SPARQL query that returns string values of a named variable.
fn query_action_ids(store: &Store, sparql: &str, var_name: &str) -> Result<Vec<String>, String> {
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

/// Reconstruct a Plan from the store by UUID.
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
        contexts: if contexts.is_empty() {
            None
        } else {
            Some(contexts)
        },
        recurrence: None, // TODO: Full recurrence hydration from blank node
        parent,
        objective,
        alias,
        is_sequential,
        depends_on: if depends_on.is_empty() {
            None
        } else {
            Some(depends_on)
        },
        duration: None, // TODO: duration hydration from graph
        acts: vec![],
    })
}

/// Reconstruct a PlannedAct from the store by UUID.
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

    let scheduled_at = find_one(actions_pred("scheduledAt")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    let duration = find_one(actions_pred("duration")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<u32>().ok(),
        _ => None,
    });

    let completed_at = find_one(actions_pred("completedAt")).and_then(|t| match t {
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

// ============================================================================
// TTL Loading
// ============================================================================

/// Load RDF Turtle content into the store's default graph.
///
/// This is the reverse of `store_to_turtle` — useful for loading
/// external TTL files or round-trip testing.
pub fn load_turtle(store: &Store, content: &str) -> Result<(), String> {
    store
        .load_from_reader(RdfFormat::Turtle, content.as_bytes())
        .map_err(|e| format!("Failed to load Turtle: {}", e))
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
    // "Planned Act must have exactly one status via is_measured_by_nominal"
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
            uri = uri,
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
            uri = uri,
        ));
    }

    // Shape 2: PlanPrescribesShape
    // "Plan prescribes targets must be CCO Planned Act instances"
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
            uri = uri,
        ));
    }

    Ok(violations)
}

/// Helper: extract the string value of a SPARQL variable from SELECT results.
///
/// Works for both `NamedNode` (returns IRI string) and `Literal` (returns lexical value).
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::{Action, ActionState, convert};

    #[test]
    fn test_load_domain_model() {
        let store = create_store().unwrap();

        let actions = vec![Action::new("Test task")];
        let model = convert::from_actions(&actions);

        load_domain_model(&store, &model).unwrap();

        // Verify Plan was inserted with correct CCO class URI (ont00000974)
        let plan_query = format!(
            "SELECT ?name WHERE {{ ?s a <{}{}> . ?s <{}name> ?name }}",
            CCO_NS, CCO_PLAN, SCHEMA_NS
        );
        let results = SparqlEvaluator::new()
            .parse_query(&plan_query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let names: Vec<_> = solutions
                .filter_map(|s| s.ok())
                .filter_map(|s| s.get("name").cloned())
                .collect();
            assert_eq!(names.len(), 1);
        }
    }

    #[test]
    fn test_serialize_acts_to_turtle() {
        let actions = vec![Action::new("Turtle task")];
        let model = convert::from_actions(&actions);

        let turtle = serialize_acts_to_turtle(&model).unwrap();

        // Should contain CCO OBO IDs for Plan and PlannedAct
        assert!(turtle.contains(CCO_PLAN));
        assert!(turtle.contains(CCO_PLANNED_ACT));
        // Should be valid turtle (non-empty)
        assert!(!turtle.is_empty());
    }

    #[test]
    fn test_serialize_open_closed_split() {
        let action_open = Action::new("Open task");
        let mut action_done = Action::new("Done task");
        action_done.state = ActionState::Completed;

        let model = convert::from_actions(&vec![action_open, action_done]);

        let open_turtle = serialize_open_acts_to_turtle(&model).unwrap();
        let closed_turtle = serialize_closed_acts_to_turtle(&model).unwrap();

        // Open turtle should have NotStarted but not Completed
        assert!(open_turtle.contains("NotStarted"));
        assert!(!open_turtle.contains("Completed"));

        // Closed turtle should have Completed but not NotStarted
        assert!(closed_turtle.contains("Completed"));
        assert!(!closed_turtle.contains("NotStarted"));
    }

    #[test]
    fn test_query_plans_from_store() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Queryable plan")];
        let model = convert::from_actions(&actions);
        load_domain_model(&store, &model).unwrap();

        let sparql = format!(
            "SELECT ?id WHERE {{ ?s a <{}{}> . ?s <{}id> ?id }}",
            CCO_NS, CCO_PLAN, ACTIONS_NS
        );
        let plans = query_plans(&store, &sparql).unwrap();

        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].name, "Queryable plan");
    }

    #[test]
    fn test_query_acts_from_store() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Queryable act")];
        let model = convert::from_actions(&actions);
        load_domain_model(&store, &model).unwrap();

        let sparql = format!(
            "SELECT ?id WHERE {{ ?s a <{}{}> . ?s <{}id> ?id }}",
            CCO_NS, CCO_PLANNED_ACT, ACTIONS_NS
        );
        let acts = query_acts(&store, &sparql).unwrap();

        assert_eq!(acts.len(), 1);
        assert_eq!(acts[0].plan_id, model.all_plans()[0].id);
    }

    #[test]
    fn test_plan_and_act_linked() {
        let store = create_store().unwrap();

        let actions = vec![Action::new("Linked task")];
        let model = convert::from_actions(&actions);
        let plan_id = model.all_plans()[0].id;

        load_domain_model(&store, &model).unwrap();

        // Query for PlannedAct that prescribedBy the Plan (convenience inverse)
        let query = format!(
            "SELECT ?act WHERE {{ ?act <{}prescribedBy> <urn:uuid:{}> }}",
            ACTIONS_NS, plan_id
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let acts: Vec<_> = solutions.filter_map(|s| s.ok()).collect();
            assert_eq!(acts.len(), 1);
        }
    }

    #[test]
    fn test_plan_prescribes_act() {
        let store = create_store().unwrap();

        let actions = vec![Action::new("Prescribed task")];
        let model = convert::from_actions(&actions);
        let plan_id = model.all_plans()[0].id;

        load_domain_model(&store, &model).unwrap();

        // Verify the forward cco:prescribes triple is stored on the Plan
        let query = format!(
            "SELECT ?act WHERE {{ <urn:uuid:{}> <{}{}> ?act }}",
            plan_id, CCO_NS, CCO_PRESCRIBES
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let acts: Vec<_> = solutions.filter_map(|s| s.ok()).collect();
            assert_eq!(acts.len(), 1, "Plan should prescribe exactly one act");
        }
    }

    #[test]
    fn test_act_status_uses_cco_property() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Status task")];
        let model = convert::from_actions(&actions);

        load_domain_model(&store, &model).unwrap();

        // Verify act status uses cco:is_measured_by_nominal (ont00001868), not actions:hasPhase
        let query = format!(
            "SELECT ?status WHERE {{ ?act a <{}{}> . ?act <{}{}> ?status }}",
            CCO_NS, CCO_PLANNED_ACT, CCO_NS, CCO_STATUS_PROP
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let statuses: Vec<_> = solutions.filter_map(|s| s.ok()).collect();
            assert_eq!(statuses.len(), 1, "PlannedAct should have exactly one status");
        }
    }
}
