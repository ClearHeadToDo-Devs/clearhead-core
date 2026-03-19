//! Load domain objects into an Oxigraph store.
//!
//! This module owns the "put things in" direction: domain model → RDF triples.

use super::{
    BFO_HAS_PART, CCO_PLAN, CCO_PLANNED_ACT, CCO_PRESCRIBES, CCO_STATUS_PROP, Store, XSD_NS,
    actions_pred, bfo_pred, cco_node, ns, phase_node, rdf_type, schema_pred,
};
use crate::domain::{Charter, DomainModel, Plan, PlannedAct};
use crate::workspace::actions::convert::INBOX_CHARTER_NS;
use oxigraph::io::RdfFormat;
use oxigraph::model::{BlankNode, GraphName, Literal, NamedNode, NamedOrBlankNode, Quad, Term};
use std::collections::HashMap;
use uuid::Uuid;

/// Load a `DomainModel` into the store using the v4 ontology.
///
/// Inserts Charters, Plans, and PlannedActs with CCO/BFO-aligned types and
/// relationships, including Charter → Plan containment via `bfo:has_part`.
pub fn load_domain_model(store: &Store, model: &DomainModel) -> Result<(), String> {
    // Build title → UUID map so hasSubCharter triples use the actual charter UUID
    // rather than re-deriving it (which breaks when explicit .md charters have
    // their own UUID that differs from the INBOX_CHARTER_NS-derived one).
    let charter_id_by_title: HashMap<String, Uuid> = model
        .charters
        .iter()
        .map(|c| (c.title.to_lowercase(), c.id))
        .collect();

    for charter in &model.charters {
        insert_charter(store, charter, &charter_id_by_title)?;
    }
    for act in model.all_acts() {
        insert_planned_act(store, act)?;
    }
    Ok(())
}

/// Insert a slice of `PlannedAct`s directly into the store.
///
/// Used by the archive command to populate a store before serializing to
/// `archive.ttl`.  Quad idempotence means calling this with already-present
/// acts is safe.
pub fn load_acts_into_store(store: &Store, acts: &[PlannedAct]) -> Result<(), String> {
    for act in acts {
        insert_planned_act(store, act)?;
    }
    Ok(())
}

/// Load RDF Turtle content into the store's default graph.
///
/// Reverse of `dump_store_to_turtle` — useful for loading external `.ttl`
/// files or round-trip testing.
pub fn load_turtle(store: &Store, content: &str) -> Result<(), String> {
    store
        .load_from_reader(RdfFormat::Turtle, content.as_bytes())
        .map_err(|e| format!("Failed to load Turtle: {}", e))
}

// ============================================================================
// Private insertion helpers
// ============================================================================

fn insert_charter(
    store: &Store,
    charter: &Charter,
    charter_id_by_title: &HashMap<String, Uuid>,
) -> Result<(), String> {
    let subject = NamedOrBlankNode::NamedNode(
        NamedNode::new(format!("urn:uuid:{}", charter.id)).unwrap(),
    );
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| e.to_string())
    };

    add(rdf_type(), Term::NamedNode(ns(super::ACTIONS_NS, "Charter")))?;
    add(
        schema_pred("name"),
        Term::Literal(Literal::new_simple_literal(&charter.title)),
    )?;
    add(
        actions_pred("id"),
        Term::Literal(Literal::new_simple_literal(charter.id.to_string())),
    )?;

    if let Some(ref parent_title) = charter.parent {
        let parent_uuid = charter_id_by_title
            .get(&parent_title.to_lowercase())
            .copied()
            .unwrap_or_else(|| Uuid::new_v5(&INBOX_CHARTER_NS, parent_title.as_bytes()));
        let parent_uri = NamedOrBlankNode::NamedNode(
            NamedNode::new(format!("urn:uuid:{}", parent_uuid)).unwrap(),
        );
        store
            .insert(&Quad::new(
                parent_uri,
                actions_pred("hasSubCharter"),
                Term::NamedNode(NamedNode::new(format!("urn:uuid:{}", charter.id)).unwrap()),
                GraphName::DefaultGraph,
            ))
            .map_err(|e| e.to_string())?;
    }

    for plan in &charter.plans {
        let plan_uri = NamedNode::new(format!("urn:uuid:{}", plan.id)).unwrap();
        add(bfo_pred(BFO_HAS_PART), Term::NamedNode(plan_uri))?;
        insert_plan(store, plan)?;
    }

    Ok(())
}

fn insert_plan(store: &Store, plan: &Plan) -> Result<(), String> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", plan.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| e.to_string())
    };

    add(rdf_type(), Term::NamedNode(cco_node(CCO_PLAN)))?;
    add(
        actions_pred("id"),
        Term::Literal(Literal::new_simple_literal(plan.id.to_string())),
    )?;
    add(
        schema_pred("name"),
        Term::Literal(Literal::new_simple_literal(&plan.name)),
    )?;

    if let Some(desc) = &plan.description {
        add(
            schema_pred("description"),
            Term::Literal(Literal::new_simple_literal(desc)),
        )?;
    }

    if let Some(priority) = plan.priority {
        add(
            actions_pred("hasPriority"),
            Term::Literal(Literal::new_typed_literal(
                priority.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    if let Some(contexts) = &plan.contexts {
        for context in contexts {
            add(
                actions_pred("hasContext"),
                Term::Literal(Literal::new_simple_literal(context)),
            )?;
        }
    }

    if let Some(objective) = &plan.objective {
        add(
            actions_pred("hasObjective"),
            Term::Literal(Literal::new_simple_literal(objective)),
        )?;
    }

    if let Some(parent_id) = plan.parent {
        let parent_uri = NamedNode::new(format!("urn:uuid:{}", parent_id)).unwrap();
        add(actions_pred("partOf"), Term::NamedNode(parent_uri))?;
    }

    if let Some(deps) = &plan.depends_on {
        for dep_id in deps {
            let dep_uri = NamedNode::new(format!("urn:uuid:{}", dep_id)).unwrap();
            add(actions_pred("dependsOn"), Term::NamedNode(dep_uri))?;
        }
    }

    if let Some(alias) = &plan.alias {
        add(
            actions_pred("alias"),
            Term::Literal(Literal::new_simple_literal(alias)),
        )?;
    }

    if let Some(true) = plan.is_sequential {
        add(
            actions_pred("isSequential"),
            Term::Literal(Literal::new_typed_literal("true", ns(XSD_NS, "boolean"))),
        )?;
    }

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

fn insert_planned_act(store: &Store, act: &PlannedAct) -> Result<(), String> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", act.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| e.to_string())
    };

    add(rdf_type(), Term::NamedNode(cco_node(CCO_PLANNED_ACT)))?;
    add(
        actions_pred("id"),
        Term::Literal(Literal::new_simple_literal(act.id.to_string())),
    )?;

    let plan_uri = NamedNode::new(format!("urn:uuid:{}", act.plan_id)).unwrap();
    add(actions_pred("prescribedBy"), Term::NamedNode(plan_uri))?;

    // cco:is_measured_by_nominal (ont00001868) — status as nominal ICE
    add(
        cco_node(CCO_STATUS_PROP),
        Term::NamedNode(phase_node(&act.phase)),
    )?;

    if let Some(dt) = &act.scheduled_at {
        add(
            actions_pred("hasDoDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    if let Some(duration) = act.duration {
        add(
            actions_pred("duration"),
            Term::Literal(Literal::new_typed_literal(
                duration.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    if let Some(dt) = &act.completed_at {
        add(
            actions_pred("hasCompletedDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

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
