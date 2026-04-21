//! Load domain objects into an Oxigraph store.
//!
//! This module owns the "put things in" direction: domain model → RDF triples.

use super::{
    actions_pred, bfo_pred, cco_node, ns, phase_node, rdf_type, rdfs_pred, GraphError, Result,
    Store, BFO_HAS_PART, BFO_PART_OF, CCO_IS_SUCCESSOR_OF, CCO_PLAN, CCO_PLANNED_ACT,
    CCO_PRESCRIBED_BY, CCO_PRESCRIBES, CCO_STATUS_PROP, RDFS_COMMENT, RDFS_LABEL, XSD_NS,
};
use crate::domain::{Charter, DomainModel, Plan, PlannedAct};
use crate::workspace::actions::convert::INBOX_CHARTER_NS;
use oxigraph::io::RdfFormat;
use oxigraph::model::{GraphName, Literal, NamedNode, NamedOrBlankNode, Quad, Term};
use std::collections::HashMap;
use uuid::Uuid;

/// Load a `DomainModel` into the store using the v4 ontology.
///
/// Inserts Charters, Plans, and PlannedActs with CCO/BFO-aligned types and
/// relationships, including Charter → Plan containment via `bfo:has_part`.
pub fn load_domain_model(store: &Store, model: &DomainModel) -> Result<()> {
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
pub fn load_acts_into_store(store: &Store, acts: &[PlannedAct]) -> Result<()> {
    for act in acts {
        insert_planned_act(store, act)?;
    }
    Ok(())
}

/// Load RDF Turtle content into the store's default graph.
///
/// Reverse of `dump_store_to_turtle` — useful for loading external `.ttl`
/// files or round-trip testing.
pub fn load_turtle(store: &Store, content: &str) -> Result<()> {
    store
        .load_from_reader(RdfFormat::Turtle, content.as_bytes())
        .map_err(|e| GraphError::Syntax(e.to_string()))
}

// ============================================================================
// Private insertion helpers
// ============================================================================

fn insert_charter(
    store: &Store,
    charter: &Charter,
    charter_id_by_title: &HashMap<String, Uuid>,
) -> Result<()> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", charter.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| GraphError::Store(e.to_string()))
    };

    add(
        rdf_type(),
        Term::NamedNode(ns(super::ACTIONS_NS, "Charter")),
    )?;
    add(
        rdfs_pred(RDFS_LABEL),
        Term::Literal(Literal::new_simple_literal(&charter.title)),
    )?;
    add(
        actions_pred("hasUUID"),
        Term::Literal(Literal::new_simple_literal(charter.id.to_string())),
    )?;

    if let Some(description) = &charter.description {
        add(
            rdfs_pred(RDFS_COMMENT),
            Term::Literal(Literal::new_simple_literal(description)),
        )?;
    }

    if let Some(alias) = &charter.alias {
        add(
            actions_pred("hasAlias"),
            Term::Literal(Literal::new_simple_literal(alias)),
        )?;
    }

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
            .map_err(|e| GraphError::Store(e.to_string()))?;
    }

    for plan in &charter.plans {
        let plan_uri = NamedNode::new(format!("urn:uuid:{}", plan.id)).unwrap();
        add(bfo_pred(BFO_HAS_PART), Term::NamedNode(plan_uri))?;
        insert_plan(store, plan, &charter.acts)?;
    }

    Ok(())
}

fn insert_plan(store: &Store, plan: &Plan, charter_acts: &[PlannedAct]) -> Result<()> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", plan.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| GraphError::Store(e.to_string()))
    };

    add(rdf_type(), Term::NamedNode(cco_node(CCO_PLAN)))?;
    add(
        actions_pred("hasUUID"),
        Term::Literal(Literal::new_simple_literal(plan.id.to_string())),
    )?;
    add(
        rdfs_pred(RDFS_LABEL),
        Term::Literal(Literal::new_simple_literal(&plan.name)),
    )?;

    if let Some(desc) = &plan.description {
        add(
            rdfs_pred(RDFS_COMMENT),
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

    if let Some(parent_id) = plan.parent {
        let parent_uri = NamedNode::new(format!("urn:uuid:{}", parent_id)).unwrap();
        add(bfo_pred(BFO_PART_OF), Term::NamedNode(parent_uri))?;
    }

    if let Some(deps) = &plan.depends_on {
        for dep_id in deps {
            let dep_uri = NamedNode::new(format!("urn:uuid:{}", dep_id)).unwrap();
            add(cco_node(CCO_IS_SUCCESSOR_OF), Term::NamedNode(dep_uri))?;
        }
    }

    if let Some(alias) = &plan.alias {
        add(
            actions_pred("hasAlias"),
            Term::Literal(Literal::new_simple_literal(alias)),
        )?;
    }

    if let Some(true) = plan.is_sequential {
        add(
            actions_pred("hasSequentialChildren"),
            Term::Literal(Literal::new_typed_literal("true", ns(XSD_NS, "boolean"))),
        )?;
    }

    if let Some(recurrence) = &plan.recurrence {
        let recurrence_str = recurrence.to_string();
        let clean_recurrence = recurrence_str.strip_prefix("R:").unwrap_or(&recurrence_str);
        add(
            actions_pred("hasRecurrenceRule"),
            Term::Literal(Literal::new_simple_literal(clean_recurrence)),
        )?;
    }

    if let Some(recurrence) = &plan.due_recurrence {
        let recurrence_str = recurrence.to_string();
        let clean = recurrence_str.strip_prefix("R:").unwrap_or(&recurrence_str);
        add(
            actions_pred("hasDueRecurrenceRule"),
            Term::Literal(Literal::new_simple_literal(clean)),
        )?;
    }

    if let Some(ext_id) = &plan.external_id {
        add(
            actions_pred("hasExternalId"),
            Term::Literal(Literal::new_simple_literal(ext_id)),
        )?;
    }

    if let Some(tmpl) = &plan.template_name {
        add(
            actions_pred("hasTemplateName"),
            Term::Literal(Literal::new_simple_literal(tmpl)),
        )?;
    }

    // cco:prescribes (ont00001942) — forward link from Plan to each PlannedAct
    for act in charter_acts.iter().filter(|a| a.plan_id == Some(plan.id)) {
        let act_uri = NamedNode::new(format!("urn:uuid:{}", act.id)).unwrap();
        add(cco_node(CCO_PRESCRIBES), Term::NamedNode(act_uri))?;
    }

    Ok(())
}

fn insert_planned_act(store: &Store, act: &PlannedAct) -> Result<()> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", act.id)).unwrap());
    let graph = GraphName::DefaultGraph;

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| GraphError::Store(e.to_string()))
    };

    add(rdf_type(), Term::NamedNode(cco_node(CCO_PLANNED_ACT)))?;
    add(
        actions_pred("hasUUID"),
        Term::Literal(Literal::new_simple_literal(act.id.to_string())),
    )?;

    if let Some(plan_id) = act.plan_id {
        let plan_uri = NamedNode::new(format!("urn:uuid:{}", plan_id)).unwrap();
        add(cco_node(CCO_PRESCRIBED_BY), Term::NamedNode(plan_uri))?;
    }

    if let Some(external_schedule_id) = &act.external_schedule_id {
        add(
            actions_pred("hasExternalScheduleId"),
            Term::Literal(Literal::new_simple_literal(external_schedule_id)),
        )?;
    }

    if let Some(external_occurrence_key) = &act.external_occurrence_key {
        add(
            actions_pred("hasExternalOccurrenceKey"),
            Term::Literal(Literal::new_simple_literal(external_occurrence_key)),
        )?;
    }

    // cco:is_measured_by_nominal (ont00001868) — status as nominal ICE
    add(
        cco_node(CCO_STATUS_PROP),
        Term::NamedNode(phase_node(&act.phase)),
    )?;

    if let Some(dt) = &act.scheduled_at {
        add(
            actions_pred("hasScheduledDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    if let Some(dt) = &act.due_date {
        add(
            actions_pred("hasDueDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    if let Some(duration) = act.duration {
        add(
            actions_pred("hasDurationMinutes"),
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
            actions_pred("hasCreatedDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dt.to_rfc3339(),
                ns(XSD_NS, "dateTime"),
            )),
        )?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{ActPhase, Charter, DomainModel, Plan, PlannedAct, Recurrence};
    use crate::graph::{self, validate_actions_vocabulary};
    use chrono::TimeZone;
    use oxigraph::model::{GraphNameRef, LiteralRef, NamedNodeRef, TermRef};

    fn sample_model() -> DomainModel {
        let plan_id = Uuid::parse_str("019d7100-1111-7111-8111-111111111111").unwrap();
        let act_id = Uuid::parse_str("019d7100-2222-7222-8222-222222222222").unwrap();
        let charter_id = Uuid::parse_str("019d7100-3333-7333-8333-333333333333").unwrap();

        DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: charter_id,
                title: "Platform".to_string(),
                description: Some("Platform charter".to_string()),
                alias: Some("platform".to_string()),
                plans: vec![Plan {
                    id: plan_id,
                    name: "Write graph tests".to_string(),
                    description: Some("Lock down graph semantics".to_string()),
                    priority: Some(1),
                    recurrence: Some(Recurrence {
                        frequency: "weekly".to_string(),
                        interval: Some(2),
                        by_day: Some(vec!["MO".to_string(), "WE".to_string()]),
                        ..Default::default()
                    }),
                    alias: Some("graph_tests".to_string()),
                    is_sequential: Some(true),
                    depends_on: Some(vec![Uuid::parse_str(
                        "019d7100-4444-7444-8444-444444444444",
                    )
                    .unwrap()]),
                    ..Default::default()
                }],
                acts: vec![PlannedAct {
                    id: act_id,
                    plan_id: Some(plan_id),
                    external_schedule_id: Some("weekly-review@example.com".to_string()),
                    external_occurrence_key: Some("2026-04-09T10:00:00-07:00".to_string()),
                    phase: ActPhase::InProgress,
                    scheduled_at: Some(
                        chrono::Local
                            .with_ymd_and_hms(2026, 4, 9, 10, 0, 0)
                            .unwrap(),
                    ),
                    duration: Some(45),
                    created_at: Some(chrono::Local.with_ymd_and_hms(2026, 4, 9, 9, 0, 0).unwrap()),
                    ..Default::default()
                }],
                ..Default::default()
            }],
        }
    }

    fn has_term(
        store: &Store,
        subject: NamedNodeRef<'_>,
        predicate: NamedNodeRef<'_>,
        object: TermRef<'_>,
    ) -> bool {
        store
            .quads_for_pattern(
                Some(subject.into()),
                Some(predicate.into()),
                Some(object),
                Some(GraphNameRef::DefaultGraph),
            )
            .next()
            .is_some()
    }

    fn has_predicate(
        store: &Store,
        subject: NamedNodeRef<'_>,
        predicate: NamedNodeRef<'_>,
    ) -> bool {
        store
            .quads_for_pattern(
                Some(subject.into()),
                Some(predicate.into()),
                None,
                Some(GraphNameRef::DefaultGraph),
            )
            .next()
            .is_some()
    }

    #[test]
    fn load_domain_model_uses_canonical_v4_terms() {
        let store = graph::create_store().expect("store");
        let model = sample_model();

        load_domain_model(&store, &model).expect("load model into graph");

        let plan = NamedNodeRef::new("urn:uuid:019d7100-1111-7111-8111-111111111111").unwrap();
        let act = NamedNodeRef::new("urn:uuid:019d7100-2222-7222-8222-222222222222").unwrap();
        let charter = NamedNodeRef::new("urn:uuid:019d7100-3333-7333-8333-333333333333").unwrap();
        assert!(has_term(
            &store,
            plan,
            rdfs_pred(RDFS_LABEL).as_ref(),
            LiteralRef::new_simple_literal("Write graph tests").into(),
        ));
        assert!(has_term(
            &store,
            charter,
            rdfs_pred(RDFS_LABEL).as_ref(),
            LiteralRef::new_simple_literal("Platform").into(),
        ));
        assert!(has_term(
            &store,
            plan,
            actions_pred("hasUUID").as_ref(),
            LiteralRef::new_simple_literal("019d7100-1111-7111-8111-111111111111").into(),
        ));
        assert!(has_term(
            &store,
            plan,
            actions_pred("hasAlias").as_ref(),
            LiteralRef::new_simple_literal("graph_tests").into(),
        ));
        assert!(has_term(
            &store,
            plan,
            cco_node(CCO_IS_SUCCESSOR_OF).as_ref(),
            NamedNodeRef::new("urn:uuid:019d7100-4444-7444-8444-444444444444")
                .unwrap()
                .into(),
        ));
        assert!(has_term(
            &store,
            plan,
            actions_pred("hasSequentialChildren").as_ref(),
            LiteralRef::new_typed_literal("true", ns(XSD_NS, "boolean").as_ref()).into(),
        ));
        assert!(has_term(
            &store,
            plan,
            actions_pred("hasRecurrenceRule").as_ref(),
            LiteralRef::new_simple_literal("FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE").into(),
        ));
        assert!(has_term(
            &store,
            act,
            cco_node(CCO_PRESCRIBED_BY).as_ref(),
            plan.into(),
        ));
        assert!(has_term(
            &store,
            act,
            actions_pred("hasDurationMinutes").as_ref(),
            LiteralRef::new_typed_literal("45", ns(XSD_NS, "integer").as_ref()).into(),
        ));
        assert!(has_term(
            &store,
            act,
            actions_pred("hasExternalScheduleId").as_ref(),
            LiteralRef::new_simple_literal("weekly-review@example.com").into(),
        ));
        assert!(has_term(
            &store,
            act,
            actions_pred("hasExternalOccurrenceKey").as_ref(),
            LiteralRef::new_simple_literal("2026-04-09T10:00:00-07:00").into(),
        ));
        assert!(has_predicate(
            &store,
            act,
            actions_pred("hasCreatedDateTime").as_ref(),
        ));

        assert!(!has_predicate(
            &store,
            plan,
            NamedNodeRef::new("http://schema.org/name").unwrap(),
        ));
        assert!(!has_predicate(&store, plan, actions_pred("id").as_ref()));
        assert!(!has_predicate(&store, plan, actions_pred("alias").as_ref()));
        assert!(!has_predicate(
            &store,
            plan,
            actions_pred("dependsOn").as_ref()
        ));
        assert!(!has_predicate(
            &store,
            plan,
            actions_pred("isSequential").as_ref()
        ));
        assert!(!has_predicate(
            &store,
            plan,
            actions_pred("hasRecurrence").as_ref()
        ));
        assert!(!has_predicate(
            &store,
            act,
            actions_pred("duration").as_ref()
        ));
        assert!(!has_predicate(
            &store,
            act,
            actions_pred("createdAt").as_ref()
        ));
        assert!(!has_predicate(
            &store,
            act,
            actions_pred("prescribedBy").as_ref(),
        ));
    }

    #[test]
    fn canonical_graph_passes_validation_subset() {
        let store = graph::create_store().expect("store");
        let model = sample_model();

        load_domain_model(&store, &model).expect("load model into graph");
        let violations = validate_actions_vocabulary(&store).expect("validate graph");

        assert!(violations.is_empty(), "violations: {violations:?}");
    }
}
