//! Load domain objects into an Oxigraph store.
//!
//! This module owns the "put things in" direction: domain model → RDF triples.

use super::{
    ACTIONS_ACTION, BFO_HAS_PART, BFO_PART_OF, CCO_IS_SUCCESSOR_OF, CCO_PLAN, CCO_PRESCRIBED_BY,
    CCO_PRESCRIBES, CCO_STATUS_PROP, GraphError, RDFS_COMMENT, RDFS_LABEL, Result, Store, XSD_NS,
    actions_pred, bfo_pred, cco_node, ns, phase_node, rdf_type, rdfs_pred,
};
use crate::WorkspaceConfig;
use crate::domain::{Action, Charter, CharterState, DomainModel, Plan};
use crate::workspace::actions::convert::INBOX_CHARTER_NS;
use oxigraph::io::RdfFormat;
use oxigraph::model::{GraphName, Literal, NamedNode, NamedOrBlankNode, Quad, Term};
use std::collections::HashMap;
use uuid::Uuid;

/// Load a `DomainModel` into the store using the v4 ontology.
///
/// Inserts Charters, Plans, and Actions with v4-aligned types and
/// relationships, including Charter → Plan containment via `bfo:has_part`.
///
/// If `config` is supplied, `tag_hierarchies` are materialised as
/// `actions:contextBroader` / `actions:contextNarrower` triples so the graph
/// is fully queryable for context hierarchy without runtime expansion.
pub fn load_domain_model(
    store: &Store,
    model: &DomainModel,
    config: Option<&WorkspaceConfig>,
    graph_name: GraphName,
) -> Result<()> {
    // Build title → UUID map so hasSubCharter triples use the actual charter UUID
    // rather than re-deriving it (which breaks when explicit .md charters have
    // their own UUID that differs from the INBOX_CHARTER_NS-derived one).
    let charter_id_by_title: HashMap<String, Uuid> = model
        .charters
        .iter()
        .map(|c| (c.title.to_lowercase(), c.id))
        .collect();

    let workspace_name = config.and_then(|c| c.workspace_name.as_deref());
    for charter in &model.charters {
        insert_charter(store, charter, &charter_id_by_title, &graph_name, workspace_name)?;
    }
    for action in model.all_actions() {
        insert_action(store, action, &graph_name)?;
    }
    if let Some(cfg) = config {
        inject_context_hierarchy(store, cfg, &graph_name)?;
    }
    Ok(())
}

/// Insert a slice of `Action`s directly into the store.
///
/// Used by the archive command to populate a store before serializing to
/// `archive.ttl`.  Quad idempotence means calling this with already-present
/// acts is safe.
pub fn load_acts_into_store(store: &Store, acts: &[Action]) -> Result<()> {
    // Archive serialization stores always use the default graph — they are
    // transient single-use stores written to TTL, not persistent query stores.
    let graph = GraphName::DefaultGraph;
    for act in acts {
        insert_action(store, act, &graph)?;
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

/// Load RDF Turtle content into a specific named graph.
///
/// Parses via a temporary store (DefaultGraph), then re-inserts quads into
/// `graph_name`. Used in tests to seed named-graph stores with hand-crafted TTL.
pub fn load_turtle_into_graph(store: &Store, content: &str, graph_name: GraphName) -> Result<()> {
    use oxigraph::model::GraphNameRef;
    let temp = Store::new().map_err(|e| GraphError::Store(e.to_string()))?;
    temp.load_from_reader(RdfFormat::Turtle, content.as_bytes())
        .map_err(|e| GraphError::Syntax(e.to_string()))?;
    for quad in temp.quads_for_pattern(None, None, None, Some(GraphNameRef::DefaultGraph)) {
        let quad = quad.map_err(|e| GraphError::Store(e.to_string()))?;
        store
            .insert(&oxigraph::model::Quad::new(
                quad.subject,
                quad.predicate,
                quad.object,
                graph_name.clone(),
            ))
            .map_err(|e| GraphError::Store(e.to_string()))?;
    }
    Ok(())
}

// ============================================================================
// Private insertion helpers
// ============================================================================

fn insert_charter(
    store: &Store,
    charter: &Charter,
    charter_id_by_title: &HashMap<String, Uuid>,
    graph_name: &GraphName,
    workspace_name: Option<&str>,
) -> Result<()> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", charter.id)).unwrap());
    let graph = graph_name.clone();

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

    if let Some(name) = workspace_name {
        add(
            actions_pred("hasWorkspaceName"),
            Term::Literal(Literal::new_simple_literal(name)),
        )?;
    }

    if let Some(ref state) = charter.state {
        let state_str = match state {
            CharterState::New => "New",
            CharterState::Active => "Active",
            CharterState::Blocked => "Blocked",
            CharterState::Closed => "Closed",
        };
        add(
            actions_pred("hasCharterState"),
            Term::Literal(Literal::new_simple_literal(state_str)),
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
                graph_name.clone(),
            ))
            .map_err(|e| GraphError::Store(e.to_string()))?;
    }

    for plan in &charter.plans {
        let plan_uri = NamedNode::new(format!("urn:uuid:{}", plan.id)).unwrap();
        add(bfo_pred(BFO_HAS_PART), Term::NamedNode(plan_uri))?;
        insert_plan(store, plan, &charter.actions, graph_name)?;
    }

    for action in charter.actions.iter().filter(|a| a.plan_id.is_none()) {
        let action_uri = NamedNode::new(format!("urn:uuid:{}", action.id)).unwrap();
        add(bfo_pred(BFO_HAS_PART), Term::NamedNode(action_uri))?;
    }

    Ok(())
}

fn insert_plan(store: &Store, plan: &Plan, charter_actions: &[Action], graph_name: &GraphName) -> Result<()> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", plan.id)).unwrap());
    let graph = graph_name.clone();

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
            actions_pred("hasExternalScheduleId"),
            Term::Literal(Literal::new_simple_literal(ext_id)),
        )?;
    }

    if let Some(tmpl) = &plan.template_name {
        add(
            actions_pred("hasTemplateName"),
            Term::Literal(Literal::new_simple_literal(tmpl)),
        )?;
    }

    // cco:prescribes — forward link from recurring Plan to each Action it generated.
    for action in charter_actions.iter().filter(|a| a.plan_id == Some(plan.id)) {
        let action_uri = NamedNode::new(format!("urn:uuid:{}", action.id)).unwrap();
        add(cco_node(CCO_PRESCRIBES), Term::NamedNode(action_uri))?;
    }

    if let Some(dtstart) = plan.dtstart {
        add(
            actions_pred("hasScheduledDateTime"),
            Term::Literal(Literal::new_typed_literal(
                dtstart.to_rfc3339(),
                NamedNode::new(format!("{}dateTime", XSD_NS)).unwrap(),
            )),
        )?;
    }

    Ok(())
}

fn insert_action(store: &Store, act: &Action, graph_name: &GraphName) -> Result<()> {
    let subject =
        NamedOrBlankNode::NamedNode(NamedNode::new(format!("urn:uuid:{}", act.id)).unwrap());
    let graph = graph_name.clone();

    let add = |pred: NamedNode, term: Term| {
        store
            .insert(&Quad::new(subject.clone(), pred, term, graph.clone()))
            .map_err(|e| GraphError::Store(e.to_string()))
    };

    add(rdf_type(), Term::NamedNode(ns(super::ACTIONS_NS, ACTIONS_ACTION)))?;
    add(
        actions_pred("hasUUID"),
        Term::Literal(Literal::new_simple_literal(act.id.to_string())),
    )?;

    add(
        rdfs_pred(RDFS_LABEL),
        Term::Literal(Literal::new_simple_literal(&act.name)),
    )?;

    if let Some(description) = &act.description {
        add(
            rdfs_pred(RDFS_COMMENT),
            Term::Literal(Literal::new_simple_literal(description)),
        )?;
    }

    if let Some(priority) = act.priority {
        add(
            actions_pred("hasPriority"),
            Term::Literal(Literal::new_typed_literal(
                priority.to_string(),
                ns(XSD_NS, "integer"),
            )),
        )?;
    }

    if let Some(contexts) = &act.contexts {
        for context in contexts {
            let ctx_node = insert_context_node(store, context, graph_name)?;
            add(actions_pred("requiresContext"), Term::NamedNode(ctx_node))?;
        }
    }

    if let Some(parent_id) = act.parent_id {
        let parent_uri = NamedNode::new(format!("urn:uuid:{}", parent_id)).unwrap();
        add(bfo_pred(BFO_PART_OF), Term::NamedNode(parent_uri))?;
    }

    let depends_on = act.depends_on();
    if !depends_on.is_empty() {
        for dep_id in depends_on {
            let dep_uri = NamedNode::new(format!("urn:uuid:{}", dep_id)).unwrap();
            add(cco_node(CCO_IS_SUCCESSOR_OF), Term::NamedNode(dep_uri))?;
        }
    }

    if let Some(alias) = &act.alias {
        add(
            actions_pred("hasAlias"),
            Term::Literal(Literal::new_simple_literal(alias)),
        )?;
    }

    if let Some(true) = act.is_sequential {
        add(
            actions_pred("hasSequentialChildren"),
            Term::Literal(Literal::new_typed_literal("true", ns(XSD_NS, "boolean"))),
        )?;
    }

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
        Term::NamedNode(phase_node(&act.state)),
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

/// Ensure an `actions:Context` node exists for `tag` and return its URI.
///
/// Emits `a actions:Context` and `hasContextIdentifier` triples. Idempotent —
/// inserting the same quad twice is a no-op in Oxigraph.
fn insert_context_node(store: &Store, tag: &str, graph_name: &GraphName) -> Result<NamedNode> {
    let clean = tag.trim_start_matches('+');
    let uri = NamedNode::new(format!("urn:context:{}", clean)).unwrap();
    let subject = NamedOrBlankNode::NamedNode(uri.clone());
    let graph = graph_name.clone();

    store
        .insert(&Quad::new(
            subject.clone(),
            rdf_type(),
            Term::NamedNode(ns(super::ACTIONS_NS, "Context")),
            graph.clone(),
        ))
        .map_err(|e| GraphError::Store(e.to_string()))?;
    store
        .insert(&Quad::new(
            subject,
            actions_pred("hasContextIdentifier"),
            Term::Literal(Literal::new_typed_literal(clean, ns(XSD_NS, "string"))),
            graph,
        ))
        .map_err(|e| GraphError::Store(e.to_string()))?;

    Ok(uri)
}

/// Materialise `tag_hierarchies` as `contextBroader`/`contextNarrower` triples.
///
/// Creates Context nodes for every tag mentioned in the hierarchy (whether or
/// not any action currently uses them) and links them with
/// `actions:contextBroader` (child → parent) and `actions:contextNarrower`
/// (parent → child). This makes the full hierarchy SPARQL-queryable.
fn inject_context_hierarchy(store: &Store, config: &WorkspaceConfig, graph_name: &GraphName) -> Result<()> {
    let graph = graph_name.clone();
    for (parent_tag, children) in &config.tag_hierarchies {
        let parent_uri = insert_context_node(store, parent_tag, &graph)?;
        for child_tag in children {
            let child_uri = insert_context_node(store, child_tag, &graph)?;
            // contextBroader: child → parent
            store
                .insert(&Quad::new(
                    NamedOrBlankNode::NamedNode(child_uri.clone()),
                    actions_pred("contextBroader"),
                    Term::NamedNode(parent_uri.clone()),
                    graph.clone(),
                ))
                .map_err(|e| GraphError::Store(e.to_string()))?;
            // contextNarrower: parent → child
            store
                .insert(&Quad::new(
                    NamedOrBlankNode::NamedNode(parent_uri.clone()),
                    actions_pred("contextNarrower"),
                    Term::NamedNode(child_uri),
                    graph.clone(),
                ))
                .map_err(|e| GraphError::Store(e.to_string()))?;
        }
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Action, ActionState, Charter, DomainModel, Plan, Recurrence};
    use crate::graph::{self, validate_actions_vocabulary};
    use chrono::TimeZone;
    use oxigraph::model::{LiteralRef, NamedNodeRef};
    #[allow(unused_imports)]
    use oxigraph::model::TermRef;

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
                    recurrence: Some(Recurrence {
                        frequency: "weekly".to_string(),
                        interval: Some(2),
                        by_day: Some(vec!["MO".to_string(), "WE".to_string()]),
                        ..Default::default()
                    }),
                    dtstart: Some(
                        chrono::Local
                            .with_ymd_and_hms(2026, 4, 7, 10, 0, 0)
                            .unwrap(),
                    ),
                    ..Default::default()
                }],
                actions: vec![Action {
                    id: act_id,
                    name: "Write graph tests".to_string(),
                    description: Some("Lock down graph semantics".to_string()),
                    priority: Some(1),
                    alias: Some("graph_tests".to_string()),
                    is_sequential: Some(true),
                    predecessors: Some(vec![crate::domain::PredecessorRef {
                        raw_ref: "019d7100-4444-7444-8444-444444444444".to_string(),
                        resolved_uuid: Some(Uuid::parse_str("019d7100-4444-7444-8444-444444444444").unwrap()),
                    }]),
                    plan_id: Some(plan_id),
                    external_schedule_id: Some("weekly-review@example.com".to_string()),
                    external_occurrence_key: Some("2026-04-09T10:00:00-07:00".to_string()),
                    state: ActionState::InProgress,
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
                None, // search all graphs — data lives in a named graph in production
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
                None, // search all graphs
            )
            .next()
            .is_some()
    }

    #[test]
    fn load_domain_model_uses_canonical_v4_terms() {
        let store = graph::create_store().expect("store");
        let model = sample_model();

        load_domain_model(&store, &model, None, GraphName::NamedNode(
            oxigraph::model::NamedNode::new(super::super::TRANSIENT_GRAPH_URI).unwrap()
        )).expect("load model into graph");

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
        let g = GraphName::NamedNode(
            oxigraph::model::NamedNode::new(super::super::TRANSIENT_GRAPH_URI).unwrap(),
        );

        load_domain_model(&store, &model, None, g).expect("load model into graph");
        let violations = validate_actions_vocabulary(&store).expect("validate graph");

        assert!(violations.is_empty(), "violations: {violations:?}");
    }

    // ============================================================================
    // Context graph shape
    // ============================================================================

    fn action_with_contexts(contexts: Vec<&str>) -> DomainModel {
        use crate::domain::ActionState;
        let charter_id = Uuid::parse_str("019d7100-cccc-7ccc-8ccc-cccccccccccc").unwrap();
        let act_id = Uuid::parse_str("019d7100-aaaa-7aaa-8aaa-aaaaaaaaaaaa").unwrap();
        DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: charter_id,
                title: "Test".to_string(),
                actions: vec![Action {
                    id: act_id,
                    name: "Tagged action".to_string(),
                    contexts: Some(contexts.into_iter().map(String::from).collect()),
                    state: ActionState::NotStarted,
                    ..Default::default()
                }],
                ..Default::default()
            }],
        }
    }

    #[test]
    fn context_triples_emitted_for_action_with_contexts() {
        let store = graph::create_store().expect("store");
        let model = action_with_contexts(vec!["neovim", "work"]);
        let g = GraphName::NamedNode(
            oxigraph::model::NamedNode::new(super::super::TRANSIENT_GRAPH_URI).unwrap()
        );
        load_domain_model(&store, &model, None, g).expect("load");

        let act = NamedNodeRef::new("urn:uuid:019d7100-aaaa-7aaa-8aaa-aaaaaaaaaaaa").unwrap();
        let neovim = NamedNodeRef::new("urn:context:neovim").unwrap();
        let work = NamedNodeRef::new("urn:context:work").unwrap();

        // Action links to context nodes via requiresContext
        assert!(has_term(&store, act, actions_pred("requiresContext").as_ref(), neovim.into()));
        assert!(has_term(&store, act, actions_pred("requiresContext").as_ref(), work.into()));

        // Each context node has the right type and identifier
        assert!(has_term(
            &store, neovim,
            rdf_type().as_ref(),
            NamedNodeRef::new("https://clearhead.us/vocab/actions/v4#Context").unwrap().into(),
        ));
        assert!(has_term(
            &store, neovim,
            actions_pred("hasContextIdentifier").as_ref(),
            LiteralRef::new_typed_literal("neovim", ns(XSD_NS, "string").as_ref()).into(),
        ));
    }

    #[test]
    fn inject_context_hierarchy_emits_broader_narrower_triples() {
        use std::collections::HashMap;
        use oxigraph::model::GraphNameRef;
        let store = graph::create_store().expect("store");

        let mut tag_hierarchies = HashMap::new();
        tag_hierarchies.insert("computer".to_string(), vec!["terminal".to_string()]);
        tag_hierarchies.insert("terminal".to_string(), vec!["neovim".to_string()]);
        let config = WorkspaceConfig { tag_hierarchies, ..Default::default() };
        let g = GraphName::NamedNode(
            oxigraph::model::NamedNode::new(super::super::TRANSIENT_GRAPH_URI).unwrap(),
        );

        inject_context_hierarchy(&store, &config, &g).expect("inject");

        let computer = NamedNodeRef::new("urn:context:computer").unwrap();
        let terminal = NamedNodeRef::new("urn:context:terminal").unwrap();
        let neovim = NamedNodeRef::new("urn:context:neovim").unwrap();

        // Check presence across any graph (None = any graph)
        let has = |s: NamedNodeRef, p: NamedNodeRef<'_>, o: NamedNodeRef| {
            store.quads_for_pattern(Some(s.into()), Some(p.into()), Some(o.into()), None::<GraphNameRef>)
                .next().is_some()
        };

        // contextBroader: child → parent
        assert!(has(terminal, actions_pred("contextBroader").as_ref(), computer));
        assert!(has(neovim, actions_pred("contextBroader").as_ref(), terminal));
        // contextNarrower: parent → child
        assert!(has(computer, actions_pred("contextNarrower").as_ref(), terminal));
        assert!(has(terminal, actions_pred("contextNarrower").as_ref(), neovim));
    }

    #[test]
    fn sparql_property_path_traverses_context_hierarchy() {
        // An action tagged +neovim should be found by a SPARQL query for +computer
        // when the hierarchy computer → terminal → neovim is injected.
        use std::collections::HashMap;
        let store = graph::create_store().expect("store");
        let model = action_with_contexts(vec!["neovim"]);

        let mut tag_hierarchies = HashMap::new();
        tag_hierarchies.insert("computer".to_string(), vec!["terminal".to_string()]);
        tag_hierarchies.insert("terminal".to_string(), vec!["neovim".to_string()]);
        let config = WorkspaceConfig { tag_hierarchies, ..Default::default() };
        let g = GraphName::NamedNode(
            oxigraph::model::NamedNode::new(super::super::TRANSIENT_GRAPH_URI).unwrap(),
        );

        load_domain_model(&store, &model, Some(&config), g).expect("load");

        // Property-path query within the named graph: action → requiresContext → ctx →(contextBroader)*→ computer
        let sparql = "
            PREFIX actions: <https://clearhead.us/vocab/actions/v4#>
            SELECT ?id WHERE {
                GRAPH ?g {
                    ?action actions:hasUUID ?id .
                    ?action actions:requiresContext ?ctx .
                    ?ctx (actions:contextBroader)* <urn:context:computer> .
                }
            }
        ";

        let ids = graph::query_action_ids(&store, sparql).expect("query");
        assert!(
            ids.contains(&"019d7100-aaaa-7aaa-8aaa-aaaaaaaaaaaa".to_string()),
            "neovim-tagged action not found under computer hierarchy; got: {:?}",
            ids
        );
    }
}
