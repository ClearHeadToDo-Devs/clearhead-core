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

use crate::domain::{ActPhase, DomainModel, Plan, PlannedAct, Recurrence};
use crate::workspace::actions::{Action, ActionList, ActionState};
use chrono::DateTime;
use oxigraph::io::RdfFormat;
use oxigraph::model::{
    BlankNode, GraphName, GraphNameRef, Literal, NamedNode, NamedOrBlankNode, Quad, Term,
};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;
use std::collections::HashMap;
use uuid::Uuid;

// Namespace constants (v3 - legacy)
const ACTIONS_NS: &str = "https://clearhead.us/vocab/actions/v3#";
const SCHEMA_NS: &str = "http://schema.org/";
const RDF_NS: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";
const SKOS_NS: &str = "http://www.w3.org/2004/02/skos/core#";

// Namespace constants (v4 - CCO-aligned)
const ACTIONS_V4_NS: &str = "https://clearhead.us/vocab/actions/v4#";
const CCO_NS: &str = "http://www.ontologyrepository.com/CommonCoreOntologies/";

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

fn action_pred(name: &str) -> NamedNode {
    ns(ACTIONS_NS, name)
}

fn schema_pred(name: &str) -> NamedNode {
    ns(SCHEMA_NS, name)
}

fn rdf_type() -> NamedNode {
    ns(RDF_NS, "type")
}

fn action_class() -> NamedNode {
    action_pred("Action")
}

fn state_node(state: &ActionState) -> NamedNode {
    let name = match state {
        ActionState::NotStarted => "NotStarted",
        ActionState::Completed => "Completed",
        ActionState::InProgress => "InProgress",
        ActionState::BlockedorAwaiting => "Blocked",
        ActionState::Cancelled => "Cancelled",
    };
    action_pred(name)
}

fn insert_action(
    store: &Store,
    action: &Action,
    file_path: Option<&str>,
    project: Option<&str>,
) -> Result<(), String> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", action.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    // Helper to add triple
    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| e.to_string())
    };

    // rdf:type actions:Action
    add(rdf_type(), Term::NamedNode(action_class()))?;

    // Add specific type based on hierarchy (heuristic until depth is explicitly stored)
    let specific_type = if action.parent_id.is_none() {
        action_pred("RootActionPlan")
    } else {
        action_pred("ChildActionPlan")
    };
    add(rdf_type(), Term::NamedNode(specific_type))?;

    // actions:id (as string)
    add(
        action_pred("id"),
        Term::Literal(Literal::new_simple_literal(action.id.to_string())),
    )?;

    // schema:name
    add(
        schema_pred("name"),
        Term::Literal(Literal::new_simple_literal(&action.name)),
    )?;

    // actions:hasState
    add(
        action_pred("hasState"),
        Term::NamedNode(state_node(&action.state)),
    )?;

    // schema:description
    if let Some(desc) = &action.description {
        add(
            schema_pred("description"),
            Term::Literal(Literal::new_simple_literal(desc)),
        )?;
    }

    // actions:parentAction
    if let Some(parent_id) = action.parent_id {
        let parent_uri = NamedNode::new(format!("urn:uuid:{}", parent_id)).unwrap();
        add(action_pred("parentAction"), Term::NamedNode(parent_uri))?;
    }

    // actions:hasPriority
    if let Some(priority) = action.priority {
        add(
            action_pred("hasPriority"),
            Term::Literal(Literal::new_typed_literal(
                priority.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    // actions:hasProject (story)
    if let Some(story) = &action.story {
        add(
            action_pred("hasProject"),
            Term::Literal(Literal::new_simple_literal(story)),
        )?;
    }

    // actions:requiresContext
    if let Some(contexts) = &action.context_list {
        for context in contexts {
            add(
                action_pred("requiresContext"),
                Term::Literal(Literal::new_simple_literal(context)),
            )?;
        }
    }

    // actions:hasDoDateTime
    if let Some(dt) = &action.do_date_time {
        add(
            action_pred("hasDoDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    // actions:hasDoDuration
    if let Some(duration) = action.do_duration {
        add(
            action_pred("hasDoDuration"),
            Term::Literal(Literal::new_typed_literal(
                duration.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    // actions:completedDateTime
    if let Some(dt) = &action.completed_date_time {
        add(
            action_pred("completedDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    // File path and project from source
    if let Some(fp) = file_path {
        add(
            action_pred("filePath"),
            Term::Literal(Literal::new_simple_literal(fp)),
        )?;
    }

    if let Some(p) = project {
        add(
            action_pred("inferredProject"),
            Term::Literal(Literal::new_simple_literal(p)),
        )?;
    }

    // Recurrence
    if let Some(recurrence) = &action.recurrence {
        // Create a blank node for recurrence
        let bnode = BlankNode::default();
        add(action_pred("hasRecurrence"), Term::BlankNode(bnode.clone()))?;

        let r_subj = NamedOrBlankNode::BlankNode(bnode);
        let add_r = |pred: NamedNode, term: Term| {
            store
                .insert(&Quad::new(r_subj.clone(), pred, term, graph.clone()))
                .map_err(|e| e.to_string())
        };

        add_r(
            action_pred("recurrenceFrequency"),
            Term::Literal(Literal::new_simple_literal(&recurrence.frequency)),
        )?;

        if let Some(val) = recurrence.interval {
            add_r(
                action_pred("recurrenceInterval"),
                Term::Literal(Literal::new_typed_literal(
                    val.to_string(),
                    ns(XSD_NS, "integer"),
                )),
            )?;
        }

        if let Some(val) = &recurrence.by_day {
            for day in val {
                add_r(
                    action_pred("recurrenceByDay"),
                    Term::Literal(Literal::new_simple_literal(day)),
                )?;
            }
        }

        // Add other recurrence fields as needed...
    }

    Ok(())
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

/// Get full Action structs from a SPARQL query
///
/// The query should return ?id (as string). The function will then fetch full details for those IDs.
/// Alternatively, the query could be "SELECT ?s WHERE { ... }" returning URIs, but this function
/// currently assumes we first get IDs or URIs and then reconstruct the object.
///
/// Note: To fully reconstruct Actions from SPARQL is complex.
/// A simpler approach is:
/// 1. Run the user's SPARQL query to get a list of matching Action URIs or IDs.
/// 2. For each matching Action, query its properties from the Store to rebuild the struct.
pub fn get_actions_from_query(store: &Store, sparql: &str) -> Result<ActionList, String> {
    let ids = query_actions(store, sparql)?;
    let mut actions = ActionList::new();

    for id_str in ids {
        if let Ok(uuid) = Uuid::parse_str(&id_str)
            && let Ok(action) = get_action_by_id(store, uuid)
        {
            actions.push(action);
        }
    }

    Ok(actions)
}

// Alias for compatibility
pub fn get_actions_from_sql(store: &Store, sql: &str) -> Result<ActionList, String> {
    get_actions_from_query(store, sql)
}

fn get_action_by_id(store: &Store, id: Uuid) -> Result<Action, String> {
    let subject = NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", id)).unwrap());
    let graph = GraphName::DefaultGraph;

    // Helper to find one object
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

    // Helper to find multiple objects
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
        _ => return Err(format!("Action {} missing name", id)),
    };

    let state_term = find_one(action_pred("hasState"));
    let state = if let Some(Term::NamedNode(nn)) = state_term {
        match nn.as_str().split('#').next_back() {
            Some("Completed") => ActionState::Completed,
            Some("InProgress") => ActionState::InProgress,
            Some("Blocked") => ActionState::BlockedorAwaiting,
            Some("Cancelled") => ActionState::Cancelled,
            _ => ActionState::NotStarted,
        }
    } else {
        ActionState::NotStarted
    };

    let description = find_one(schema_pred("description")).map(|t| match t {
        Term::Literal(l) => l.value().to_string(),
        _ => String::new(),
    });

    let priority = find_one(action_pred("hasPriority")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<u32>().ok(),
        _ => None,
    });

    let story = find_one(action_pred("hasProject")).map(|t| match t {
        Term::Literal(l) => l.value().to_string(),
        _ => String::new(),
    });

    let contexts: Vec<String> = find_many(action_pred("requiresContext"))
        .into_iter()
        .map(|t| match t {
            Term::Literal(l) => l.value().to_string(),
            _ => String::new(),
        })
        .collect();
    let context_list = if contexts.is_empty() {
        None
    } else {
        Some(contexts)
    };

    let do_date_time = find_one(action_pred("hasDoDateTime")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    let do_duration = find_one(action_pred("hasDoDuration")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<u32>().ok(),
        _ => None,
    });

    let completed_date_time = find_one(action_pred("completedDateTime")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    let parent_id = find_one(action_pred("parentAction")).and_then(|t| match t {
        Term::NamedNode(nn) => {
            let s = nn.as_str();
            s.strip_prefix("urn:uuid:")
                .and_then(|stripped| Uuid::parse_str(stripped).ok())
        }
        _ => None,
    });

    // Recurrence (Basic implementation)
    let recurrence = find_one(action_pred("hasRecurrence")).map(|_t| {
        // In a real implementation we would traverse the blank node.
        // For now, returning None or basic if needed.
        // We'll skip deep recurrence hydration for this iteration
        // unless explicitly requested, as it requires querying the blank node.
        Recurrence {
            frequency: "daily".to_string(), // Placeholder/Default
            interval: None,
            count: None,
            until: None,
            by_second: None,
            by_minute: None,
            by_hour: None,
            by_day: None,
            by_month_day: None,
            by_year_day: None,
            by_week_no: None,
            by_month: None,
            by_set_pos: None,
            week_start: None,
        }
    });

    Ok(Action {
        id,
        parent_id,
        state,
        name,
        description,
        priority,
        context_list,
        do_date_time,
        do_duration,
        recurrence, // TODO: Full recurrence hydration
        completed_date_time,
        created_date_time: None, // Not in basic schema yet
        predecessors: None,
        story,
        alias: None,
        is_sequential: None,
    })
}

/// Build a SPARQL query from a WHERE clause
///
/// Wraps the where_clause in `SELECT ?id WHERE { ... }`.
/// This assumes the user provides a valid SPARQL Graph Pattern.
/// It also injects `?s <https://clearhead.us/vocab/actions/v3#id> ?id` to ensure ?id is bound.
pub fn build_where_query(where_clause: &str, _select: Option<&str>, _from: Option<&str>) -> String {
    // We assume the user wants ?id back.
    // We bind ?s (subject) to ?id (string id)
    format!(
        "PREFIX actions: <{actions_ns}>\n\
         PREFIX schema: <{schema_ns}>\n\
         PREFIX rdf: <{rdf_ns}>\n\
         PREFIX xsd: <{xsd_ns}>\n\
         PREFIX skos: <{skos_ns}>\n\
         SELECT ?id WHERE {{    ?s <{actions_ns}id> ?id .    {{ {where_clause} }}
}}",
        actions_ns = ACTIONS_NS,
        schema_ns = SCHEMA_NS,
        rdf_ns = RDF_NS,
        xsd_ns = XSD_NS,
        skos_ns = SKOS_NS,
        where_clause = where_clause
    )
}

/// Load tag hierarchies from Config into the store
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

/// Query actions by context with hierarchy expansion
pub fn query_actions_by_context(store: &Store, context: &str) -> Result<Vec<String>, String> {
    let context_tag = context.to_lowercase();

    // SPARQL Property Path:
    // ?action actions:requiresContext ?tagLiteral .
    // ?tagURI skos:broader* ?contextURI .
    // FILTER (str(?tagURI) = concat("urn:tag:", ?tagLiteral))
    //
    // Simplified:
    // We want actions that have a context C, where C is a subclass/narrower of `context`.
    // In our `load_tag_hierarchies`, we stored `urn:tag:child skos:broader urn:tag:parent`.
    // So if we search for "computer", we want "neovim" because neovim -> broader -> terminal -> broader -> computer.
    // So we want ?child skos:broader* <urn:tag:computer>.

    let sparql = format!(
        "SELECT ?id WHERE {{    ?s <{actions_ns}id> ?id .    ?s <{actions_ns}requiresContext> ?tagLiteral .    BIND(IRI(concat(\"urn:tag:\", lcase(?tagLiteral))) AS ?tagURI)    ?tagURI <{skos_ns}broader>* <urn:tag:{target}> .
}}",
        actions_ns = ACTIONS_NS,
        skos_ns = SKOS_NS,
        target = context_tag
    );

    query_actions(store, &sparql)
}

/// Query actions by project
pub fn query_actions_by_project(store: &Store, project: &str) -> Result<Vec<String>, String> {
    // Search for explicit project OR inferred project
    let sparql = format!(
        "SELECT ?id WHERE {{    ?s <{actions_ns}id> ?id .    {{ ?s <{actions_ns}hasProject> \"{project}\" }}    UNION    {{ ?s <{actions_ns}inferredProject> \"{project}\" .
               FILTER NOT EXISTS {{ ?s <{actions_ns}hasProject> ?any }}
            }}
        }}",
        actions_ns = ACTIONS_NS,
        project = project
    );

    query_actions(store, &sparql)
}

// ============================================================================
// V4 Domain Model (CCO-aligned)
// ============================================================================

fn v4_pred(name: &str) -> NamedNode {
    ns(ACTIONS_V4_NS, name)
}

fn cco_class(name: &str) -> NamedNode {
    ns(CCO_NS, name)
}

fn phase_node(phase: &ActPhase) -> NamedNode {
    let name = match phase {
        ActPhase::NotStarted => "NotStarted",
        ActPhase::InProgress => "InProgress",
        ActPhase::Completed => "Completed",
        ActPhase::Blocked => "Blocked",
        ActPhase::Cancelled => "Cancelled",
    };
    v4_pred(name)
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

    // rdf:type cco:Plan
    add(rdf_type(), Term::NamedNode(cco_class("Plan")))?;

    // actions:id
    add(
        v4_pred("id"),
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
            v4_pred("hasPriority"),
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
                v4_pred("hasContext"),
                Term::Literal(Literal::new_simple_literal(context)),
            )?;
        }
    }

    // actions:hasObjective (story/project)
    if let Some(objective) = &plan.objective {
        add(
            v4_pred("hasObjective"),
            Term::Literal(Literal::new_simple_literal(objective)),
        )?;
    }

    // actions:partOf (parent plan)
    if let Some(parent_id) = plan.parent {
        let parent_uri = NamedNode::new(format!("urn:uuid:{}", parent_id)).unwrap();
        add(v4_pred("partOf"), Term::NamedNode(parent_uri))?;
    }

    // actions:dependsOn (predecessor plans)
    if let Some(deps) = &plan.depends_on {
        for dep_id in deps {
            let dep_uri = NamedNode::new(format!("urn:uuid:{}", dep_id)).unwrap();
            add(v4_pred("dependsOn"), Term::NamedNode(dep_uri))?;
        }
    }

    // actions:alias
    if let Some(alias) = &plan.alias {
        add(
            v4_pred("alias"),
            Term::Literal(Literal::new_simple_literal(alias)),
        )?;
    }

    // actions:isSequential
    if let Some(true) = plan.is_sequential {
        add(
            v4_pred("isSequential"),
            Term::Literal(Literal::new_typed_literal("true", ns(XSD_NS, "boolean"))),
        )?;
    }

    // Recurrence (as blank node)
    if let Some(recurrence) = &plan.recurrence {
        let bnode = BlankNode::default();
        add(v4_pred("hasRecurrence"), Term::BlankNode(bnode.clone()))?;

        let r_subj = NamedOrBlankNode::BlankNode(bnode);
        let add_r = |pred: NamedNode, term: Term| {
            store
                .insert(&Quad::new(r_subj.clone(), pred, term, graph.clone()))
                .map_err(|e| e.to_string())
        };

        add_r(
            v4_pred("frequency"),
            Term::Literal(Literal::new_simple_literal(&recurrence.frequency)),
        )?;

        if let Some(interval) = recurrence.interval {
            add_r(
                v4_pred("interval"),
                Term::Literal(Literal::new_typed_literal(
                    interval.to_string(),
                    ns(XSD_NS, "integer"),
                )),
            )?;
        }

        if let Some(by_day) = &recurrence.by_day {
            for day in by_day {
                add_r(
                    v4_pred("byDay"),
                    Term::Literal(Literal::new_simple_literal(day)),
                )?;
            }
        }
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

    // rdf:type cco:PlannedAct
    add(rdf_type(), Term::NamedNode(cco_class("PlannedAct")))?;

    // actions:id
    add(
        v4_pred("id"),
        Term::Literal(Literal::new_simple_literal(act.id.to_string())),
    )?;

    // actions:prescribedBy (link to Plan)
    let plan_uri = NamedNode::new(format!("urn:uuid:{}", act.plan_id)).unwrap();
    add(v4_pred("prescribedBy"), Term::NamedNode(plan_uri))?;

    // actions:hasPhase
    add(v4_pred("hasPhase"), Term::NamedNode(phase_node(&act.phase)))?;

    // actions:scheduledAt
    if let Some(dt) = &act.scheduled_at {
        add(
            v4_pred("scheduledAt"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    // actions:duration
    if let Some(duration) = act.duration {
        add(
            v4_pred("duration"),
            Term::Literal(Literal::new_typed_literal(
                duration.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    // actions:completedAt
    if let Some(dt) = &act.completed_at {
        add(
            v4_pred("completedAt"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    // actions:createdAt
    if let Some(dt) = &act.created_at {
        add(
            v4_pred("createdAt"),
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

    let priority = find_one(v4_pred("hasPriority")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<u32>().ok(),
        _ => None,
    });

    let contexts: Vec<String> = find_many(v4_pred("hasContext"))
        .into_iter()
        .filter_map(|t| match t {
            Term::Literal(l) => Some(l.value().to_string()),
            _ => None,
        })
        .collect();

    let objective = find_one(v4_pred("hasObjective")).and_then(|t| match t {
        Term::Literal(l) => Some(l.value().to_string()),
        _ => None,
    });

    let parent = find_one(v4_pred("partOf")).and_then(|t| match t {
        Term::NamedNode(nn) => nn
            .as_str()
            .strip_prefix("urn:uuid:")
            .and_then(|s| Uuid::parse_str(s).ok()),
        _ => None,
    });

    let alias = find_one(v4_pred("alias")).and_then(|t| match t {
        Term::Literal(l) => Some(l.value().to_string()),
        _ => None,
    });

    let is_sequential = find_one(v4_pred("isSequential")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<bool>().ok(),
        _ => None,
    });

    let depends_on: Vec<Uuid> = find_many(v4_pred("dependsOn"))
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

    let plan_id = match find_one(v4_pred("prescribedBy")) {
        Some(Term::NamedNode(nn)) => nn
            .as_str()
            .strip_prefix("urn:uuid:")
            .and_then(|s| Uuid::parse_str(s).ok())
            .ok_or_else(|| format!("PlannedAct {} has invalid prescribedBy URI", id))?,
        _ => return Err(format!("PlannedAct {} missing prescribedBy", id)),
    };

    let phase = match find_one(v4_pred("hasPhase")) {
        Some(Term::NamedNode(nn)) => match nn.as_str().split('#').next_back() {
            Some("Completed") => ActPhase::Completed,
            Some("InProgress") => ActPhase::InProgress,
            Some("Blocked") => ActPhase::Blocked,
            Some("Cancelled") => ActPhase::Cancelled,
            _ => ActPhase::NotStarted,
        },
        _ => ActPhase::NotStarted,
    };

    let scheduled_at = find_one(v4_pred("scheduledAt")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    let duration = find_one(v4_pred("duration")).and_then(|t| match t {
        Term::Literal(l) => l.value().parse::<u32>().ok(),
        _ => None,
    });

    let completed_at = find_one(v4_pred("completedAt")).and_then(|t| match t {
        Term::Literal(l) => DateTime::parse_from_rfc3339(l.value())
            .ok()
            .map(|dt| dt.with_timezone(&chrono::Local)),
        _ => None,
    });

    let created_at = find_one(v4_pred("createdAt")).and_then(|t| match t {
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

#[cfg(test)]
mod v4_tests {
    use super::*;
    use crate::workspace::actions::{Action, convert};

    #[test]
    fn test_load_domain_model() {
        let store = create_store().unwrap();

        let actions = vec![Action::new("Test task")];
        let model = convert::from_actions(&actions);

        load_domain_model(&store, &model).unwrap();

        // Verify Plan was inserted
        let plan_query = format!(
            "SELECT ?name WHERE {{ ?s a <{}Plan> . ?s <{}name> ?name }}",
            CCO_NS, SCHEMA_NS
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

        // Should contain plan and act data
        assert!(turtle.contains("Plan"));
        assert!(turtle.contains("PlannedAct"));
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
            "SELECT ?id WHERE {{ ?s a <{}Plan> . ?s <{}id> ?id }}",
            CCO_NS, ACTIONS_V4_NS
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
            "SELECT ?id WHERE {{ ?s a <{}PlannedAct> . ?s <{}id> ?id }}",
            CCO_NS, ACTIONS_V4_NS
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

        // Query for PlannedAct that prescribedBy the Plan
        let query = format!(
            "SELECT ?act WHERE {{ ?act <{}prescribedBy> <urn:uuid:{}> }}",
            ACTIONS_V4_NS, plan_id
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
}
