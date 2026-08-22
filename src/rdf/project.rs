//! Project the domain model into canonical RDF quads.
//!
//! This is the one "put things in" direction: `DomainModel` → `Vec<Quad>`. There
//! is no store — quads are built directly, deduplicated, and sorted into a
//! deterministic canonical order. Serializers in this module turn that one set
//! into TriG / N-Quads / Turtle / JSON-LD.
//!
//! The mapping is the v4 vocabulary, reconciled to the ontology arbiter:
//! descriptions use `dcterms:description`; containment is published upward only
//! (`part_of`, never the derivable `has_part`); a Charter's lifecycle status is a
//! `charterState` individual; and derivable/unmodelled predicates
//! (`prescribed_by`, `templateName`) are not emitted.

use super::{
    ACTIONS_NS, BFO_PART_OF, CCO_IS_SUCCESSOR_OF, CCO_PLAN, CCO_PRESCRIBES, CCO_STATUS_PROP,
    RdfError, Result, actions_pred, bfo_pred, cco_node, charter_state_node, dcterms_pred, ns,
    phase_node, rdf_type, rdfs_pred, simple, typed, uuid_node,
};
use crate::WorkspaceConfig;
use crate::domain::{Action, Charter, DomainModel, Plan};
use crate::workspace::actions::convert::INBOX_CHARTER_NS;
use oxrdf::{GraphName, NamedNode, Quad, Term};
use std::collections::HashMap;
use uuid::Uuid;

/// Project a whole `DomainModel` into the canonical quad set for one workspace
/// named graph.
///
/// The returned quads are deduplicated and sorted into a stable canonical order,
/// so the projection is deterministic for a given input regardless of internal
/// iteration order. `config`, when supplied, materialises the context hierarchy
/// (`contextBroader` / `contextNarrower`) so it is queryable without runtime
/// expansion.
pub fn project_domain(
    model: &DomainModel,
    config: Option<&WorkspaceConfig>,
    graph: GraphName,
) -> Result<Vec<Quad>> {
    // Title/alias → UUID map so hasSubCharter edges use the real charter UUID.
    // Frontmatter parent references are commonly aliases (`parent: platform`).
    let charter_id_by_ref: HashMap<String, Uuid> = model
        .charters
        .iter()
        .flat_map(|charter| {
            std::iter::once((charter.title.to_lowercase(), charter.id)).chain(
                charter
                    .alias
                    .iter()
                    .map(|alias| (alias.to_lowercase(), charter.id)),
            )
        })
        .collect();

    let mut qs = QuadSet::new(graph);
    for charter in &model.charters {
        project_charter(&mut qs, charter, &charter_id_by_ref)?;
    }
    for action in model.all_actions() {
        project_action(&mut qs, action)?;
    }
    for charter in &model.charters {
        project_sequential_chains(&mut qs, charter);
    }
    if let Some(cfg) = config {
        project_context_hierarchy(&mut qs, cfg)?;
    }
    Ok(qs.into_canonical())
}

// ============================================================================
// Quad accumulation
// ============================================================================

/// A growing set of quads bound to one named graph, canonicalized on the way out.
struct QuadSet {
    graph: GraphName,
    quads: Vec<Quad>,
}

impl QuadSet {
    fn new(graph: GraphName) -> Self {
        Self {
            graph,
            quads: Vec::new(),
        }
    }

    fn add(&mut self, subject: &NamedNode, predicate: NamedNode, object: Term) {
        self.quads.push(Quad::new(
            subject.clone(),
            predicate,
            object,
            self.graph.clone(),
        ));
    }

    fn add_node(&mut self, subject: &NamedNode, predicate: NamedNode, object: NamedNode) {
        self.add(subject, predicate, Term::NamedNode(object));
    }

    /// Deduplicate and sort into a deterministic canonical order.
    fn into_canonical(mut self) -> Vec<Quad> {
        super::canonicalize(&mut self.quads);
        self.quads
    }
}

// ============================================================================
// Term helpers
// ============================================================================

/// Strip the `R:` prefix oxrrule prints on `Recurrence::to_string()`.
fn recurrence_rule(recurrence: &impl ToString) -> String {
    let s = recurrence.to_string();
    s.strip_prefix("R:").unwrap_or(&s).to_string()
}

// ============================================================================
// Per-entity projection
// ============================================================================

fn project_charter(
    qs: &mut QuadSet,
    charter: &Charter,
    charter_id_by_ref: &HashMap<String, Uuid>,
) -> Result<()> {
    let subject = uuid_node(charter.id);

    qs.add_node(&subject, rdf_type(), ns(ACTIONS_NS, "Charter"));
    qs.add(&subject, rdfs_pred("label"), simple(&charter.title));
    qs.add(
        &subject,
        actions_pred("hasUUID"),
        simple(charter.id.to_string()),
    );
    if let Some(description) = &charter.description {
        qs.add(&subject, dcterms_pred("description"), simple(description));
    }
    if let Some(alias) = &charter.alias {
        qs.add(&subject, actions_pred("hasAlias"), simple(alias));
    }
    if let Some(state) = &charter.state {
        qs.add_node(
            &subject,
            actions_pred("hasCharterState"),
            charter_state_node(state),
        );
    }

    if let Some(parent_ref) = &charter.parent {
        let parent_uuid = charter_id_by_ref
            .get(&parent_ref.to_lowercase())
            .copied()
            .unwrap_or_else(|| Uuid::new_v5(&INBOX_CHARTER_NS, parent_ref.as_bytes()));
        qs.add_node(
            &uuid_node(parent_uuid),
            actions_pred("hasSubCharter"),
            subject.clone(),
        );
    }

    // Containment is published upward only. A Plan is part_of its Charter; a
    // top-level Action (no parent Action, not plan-generated) is part_of its
    // Charter. Sub-actions are part_of their parent Action (in project_action),
    // and plan-generated actions are reached via `prescribes` — neither is
    // re-attached to the Charter, so no Action carries two part_of edges.
    for plan in &charter.plans {
        qs.add_node(&uuid_node(plan.id), bfo_pred(BFO_PART_OF), subject.clone());
        project_plan(qs, plan, &charter.actions)?;
    }
    for action in charter
        .actions
        .iter()
        .filter(|a| a.parent_id.is_none() && a.plan_id.is_none())
    {
        qs.add_node(
            &uuid_node(action.id),
            bfo_pred(BFO_PART_OF),
            subject.clone(),
        );
    }

    Ok(())
}

fn project_plan(qs: &mut QuadSet, plan: &Plan, charter_actions: &[Action]) -> Result<()> {
    let subject = uuid_node(plan.id);

    qs.add_node(&subject, rdf_type(), cco_node(CCO_PLAN));
    qs.add(
        &subject,
        actions_pred("hasUUID"),
        simple(plan.id.to_string()),
    );
    qs.add(&subject, rdfs_pred("label"), simple(&plan.name));
    if let Some(description) = &plan.description {
        qs.add(&subject, dcterms_pred("description"), simple(description));
    }
    if let Some(recurrence) = &plan.recurrence {
        qs.add(
            &subject,
            actions_pred("hasRecurrenceRule"),
            simple(recurrence_rule(recurrence)),
        );
    }
    if let Some(recurrence) = &plan.due_recurrence {
        qs.add(
            &subject,
            actions_pred("hasDueRecurrenceRule"),
            simple(recurrence_rule(recurrence)),
        );
    }
    if let Some(ext_id) = &plan.external_id {
        qs.add(
            &subject,
            actions_pred("hasExternalScheduleId"),
            simple(ext_id),
        );
    }
    // `template_name` is intentionally not published: a template has no stable
    // identity today (see ONTOLOGY_OUT_CONTRACT note 9).

    // Forward `prescribes` link only; the inverse `prescribed_by` is derivable
    // and not published.
    for action in charter_actions
        .iter()
        .filter(|a| a.plan_id == Some(plan.id))
    {
        qs.add_node(&subject, cco_node(CCO_PRESCRIBES), uuid_node(action.id));
    }

    if let Some(dtstart) = plan.dtstart {
        qs.add(
            &subject,
            actions_pred("hasScheduledDateTime"),
            typed(dtstart.to_rfc3339(), "dateTime"),
        );
    }

    Ok(())
}

fn project_action(qs: &mut QuadSet, action: &Action) -> Result<()> {
    let subject = uuid_node(action.id);

    qs.add_node(&subject, rdf_type(), ns(ACTIONS_NS, "Action"));
    qs.add(
        &subject,
        actions_pred("hasUUID"),
        simple(action.id.to_string()),
    );
    qs.add(&subject, rdfs_pred("label"), simple(&action.name));
    if let Some(description) = &action.description {
        qs.add(&subject, dcterms_pred("description"), simple(description));
    }
    if let Some(priority) = action.priority {
        qs.add(
            &subject,
            actions_pred("hasPriority"),
            typed(priority.to_string(), "integer"),
        );
    }
    if let Some(contexts) = &action.contexts {
        for context in contexts {
            let ctx_node = project_context_node(qs, context)?;
            qs.add_node(&subject, actions_pred("requiresContext"), ctx_node);
        }
    }
    if let Some(parent_id) = action.parent_id {
        qs.add_node(&subject, bfo_pred(BFO_PART_OF), uuid_node(parent_id));
    }
    for dep_id in action.depends_on() {
        qs.add_node(&subject, cco_node(CCO_IS_SUCCESSOR_OF), uuid_node(dep_id));
    }
    if let Some(alias) = &action.alias {
        qs.add(&subject, actions_pred("hasAlias"), simple(alias));
    }
    if action.is_sequential == Some(true) {
        qs.add(
            &subject,
            actions_pred("hasSequentialChildren"),
            typed("true", "boolean"),
        );
    }
    // `plan_id` links this occurrence to its Plan; that fact is published from
    // the Plan side as `prescribes`. The inverse `prescribed_by` is not emitted.
    if let Some(external_occurrence_key) = &action.external_occurrence_key {
        qs.add(
            &subject,
            actions_pred("hasExternalOccurrenceKey"),
            simple(external_occurrence_key),
        );
    }

    qs.add_node(
        &subject,
        cco_node(CCO_STATUS_PROP),
        phase_node(&action.state),
    );

    if let Some(dt) = &action.scheduled_at {
        qs.add(
            &subject,
            actions_pred("hasScheduledDateTime"),
            typed(dt.to_rfc3339(), "dateTime"),
        );
    }
    if let Some(dt) = &action.due_date {
        qs.add(
            &subject,
            actions_pred("hasDueDateTime"),
            typed(dt.to_rfc3339(), "dateTime"),
        );
    }
    if let Some(duration) = action.duration {
        qs.add(
            &subject,
            actions_pred("hasDurationMinutes"),
            typed(duration.to_string(), "integer"),
        );
    }
    if let Some(dt) = &action.completed_at {
        qs.add(
            &subject,
            actions_pred("hasCompletedDateTime"),
            typed(dt.to_rfc3339(), "dateTime"),
        );
    }
    if let Some(dt) = &action.created_at {
        qs.add(
            &subject,
            actions_pred("hasCreatedDateTime"),
            typed(dt.to_rfc3339(), "dateTime"),
        );
    }

    Ok(())
}

/// Ensure an `actions:Context` node exists for `tag` and return its IRI.
fn project_context_node(qs: &mut QuadSet, tag: &str) -> Result<NamedNode> {
    let clean = tag
        .trim_start_matches('+')
        .trim()
        .to_lowercase()
        .replace(' ', "-");
    let uri = NamedNode::new(format!("urn:context:{clean}"))
        .map_err(|e| RdfError::Projection(format!("invalid context IRI for tag '{tag}': {e}")))?;
    qs.add_node(&uri, rdf_type(), ns(ACTIONS_NS, "Context"));
    qs.add(
        &uri,
        actions_pred("hasContextIdentifier"),
        typed(clean, "string"),
    );
    Ok(uri)
}

/// Chain the direct children of every `~` (sequential) parent: child N is
/// `is_successor_of` child N-1 in document order, so a bare `~` is enough for
/// "first in chain" query logic without hand-written predecessor refs.
fn project_sequential_chains(qs: &mut QuadSet, charter: &Charter) {
    let mut children_by_parent: HashMap<Uuid, Vec<&Action>> = HashMap::new();
    for action in &charter.actions {
        if let Some(parent_id) = action.parent_id {
            children_by_parent
                .entry(parent_id)
                .or_default()
                .push(action);
        }
    }

    for parent in &charter.actions {
        if parent.is_sequential != Some(true) {
            continue;
        }
        let Some(children) = children_by_parent.get(&parent.id) else {
            continue;
        };
        for pair in children.windows(2) {
            qs.add_node(
                &uuid_node(pair[1].id),
                cco_node(CCO_IS_SUCCESSOR_OF),
                uuid_node(pair[0].id),
            );
        }
    }
}

/// Materialise `tag_hierarchies` as `contextBroader` (child → parent) and
/// `contextNarrower` (parent → child) edges over Context nodes.
fn project_context_hierarchy(qs: &mut QuadSet, config: &WorkspaceConfig) -> Result<()> {
    for (parent_tag, children) in &config.tag_hierarchies {
        let parent_uri = project_context_node(qs, parent_tag)?;
        for child_tag in children {
            let child_uri = project_context_node(qs, child_tag)?;
            qs.add_node(
                &child_uri,
                actions_pred("contextBroader"),
                parent_uri.clone(),
            );
            qs.add_node(&parent_uri, actions_pred("contextNarrower"), child_uri);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{ActionState, CharterState, Recurrence};
    use crate::rdf::transient_graph_name;
    use chrono::TimeZone;
    use oxrdf::NamedOrBlankNode;

    const CHARTER: &str = "019d7100-3333-7333-8333-333333333333";
    const PLAN: &str = "019d7100-1111-7111-8111-111111111111";
    const ACTION: &str = "019d7100-2222-7222-8222-222222222222";

    const RDFS_LABEL: &str = "http://www.w3.org/2000/01/rdf-schema#label";
    const RDFS_COMMENT: &str = "http://www.w3.org/2000/01/rdf-schema#comment";
    const DCT_DESCRIPTION: &str = "http://purl.org/dc/terms/description";
    const BFO_PART_OF_IRI: &str = "http://purl.obolibrary.org/obo/BFO_0000050";
    const BFO_HAS_PART_IRI: &str = "http://purl.obolibrary.org/obo/BFO_0000051";
    const HAS_CHARTER_STATE: &str = "https://clearhead.us/vocab/actions/v4#hasCharterState";
    const HAS_TEMPLATE_NAME: &str = "https://clearhead.us/vocab/actions/v4#hasTemplateName";
    const PRESCRIBES: &str = "https://www.commoncoreontologies.org/ont00001942";
    const PRESCRIBED_BY: &str = "https://www.commoncoreontologies.org/ont00001920";
    const STATUS: &str = "https://www.commoncoreontologies.org/ont00001868";

    fn sample_model() -> DomainModel {
        DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: Uuid::parse_str(CHARTER).unwrap(),
                title: "Platform".to_string(),
                description: Some("Platform charter".to_string()),
                alias: Some("platform".to_string()),
                state: Some(CharterState::Active),
                plans: vec![Plan {
                    id: Uuid::parse_str(PLAN).unwrap(),
                    name: "Write graph tests".to_string(),
                    description: Some("Lock down graph semantics".to_string()),
                    template_name: Some("weekly-review".to_string()),
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
                    id: Uuid::parse_str(ACTION).unwrap(),
                    name: "Write graph tests".to_string(),
                    description: Some("Lock down graph semantics".to_string()),
                    priority: Some(1),
                    plan_id: Some(Uuid::parse_str(PLAN).unwrap()),
                    state: ActionState::InProgress,
                    duration: Some(45),
                    ..Default::default()
                }],
                ..Default::default()
            }],
        }
    }

    fn project(model: &DomainModel) -> Vec<Quad> {
        project_domain(model, None, transient_graph_name()).expect("project")
    }

    fn subj(q: &Quad) -> &str {
        match &q.subject {
            NamedOrBlankNode::NamedNode(n) => n.as_str(),
            _ => "",
        }
    }

    fn obj(q: &Quad) -> String {
        match &q.object {
            Term::NamedNode(n) => n.as_str().to_string(),
            Term::Literal(l) => l.value().to_string(),
            _ => String::new(),
        }
    }

    /// A quad exists with this subject IRI + predicate IRI (object unchecked).
    fn has(quads: &[Quad], s: &str, p: &str) -> bool {
        quads
            .iter()
            .any(|q| subj(q) == s && q.predicate.as_str() == p)
    }

    /// A quad exists with this subject IRI, predicate IRI, and object lexical.
    fn has_o(quads: &[Quad], s: &str, p: &str, o: &str) -> bool {
        quads
            .iter()
            .any(|q| subj(q) == s && q.predicate.as_str() == p && obj(q) == o)
    }

    #[test]
    fn canonical_terms_are_emitted() {
        let quads = project(&sample_model());
        let charter = format!("urn:uuid:{CHARTER}");
        let plan = format!("urn:uuid:{PLAN}");
        let action = format!("urn:uuid:{ACTION}");

        assert!(has_o(&quads, &charter, RDFS_LABEL, "Platform"));
        assert!(has_o(&quads, &plan, RDFS_LABEL, "Write graph tests"));
        assert!(has_o(&quads, &plan, PRESCRIBES, &action));
        assert!(has_o(
            &quads,
            &action,
            STATUS,
            "https://clearhead.us/vocab/actions/v4#InProgress"
        ));
        assert!(has(
            &quads,
            &action,
            "https://clearhead.us/vocab/actions/v4#hasDurationMinutes"
        ));
    }

    #[test]
    fn descriptions_use_dcterms_not_rdfs_comment() {
        let quads = project(&sample_model());
        let charter = format!("urn:uuid:{CHARTER}");
        assert!(has_o(&quads, &charter, DCT_DESCRIPTION, "Platform charter"));
        assert!(!has(&quads, &charter, RDFS_COMMENT));
    }

    #[test]
    fn containment_is_published_upward_only() {
        let quads = project(&sample_model());
        let charter = format!("urn:uuid:{CHARTER}");
        let plan = format!("urn:uuid:{PLAN}");
        // plan part_of charter (up), never charter has_part plan (down).
        assert!(has_o(&quads, &plan, BFO_PART_OF_IRI, &charter));
        assert!(!has(&quads, &charter, BFO_HAS_PART_IRI));
    }

    #[test]
    fn charter_state_is_a_named_individual() {
        let quads = project(&sample_model());
        let charter = format!("urn:uuid:{CHARTER}");
        assert!(has_o(
            &quads,
            &charter,
            HAS_CHARTER_STATE,
            "https://clearhead.us/vocab/actions/v4#CharterActive"
        ));
        // Not the old bare-string literal.
        assert!(!has_o(&quads, &charter, HAS_CHARTER_STATE, "Active"));
    }

    #[test]
    fn derivable_and_unmodelled_predicates_are_dropped() {
        let quads = project(&sample_model());
        let plan = format!("urn:uuid:{PLAN}");
        let action = format!("urn:uuid:{ACTION}");
        // prescribed_by is the derivable inverse of prescribes — not published.
        assert!(!has(&quads, &action, PRESCRIBED_BY));
        // templateName has no stable identity — not published.
        assert!(!has(&quads, &plan, HAS_TEMPLATE_NAME));
    }

    #[test]
    fn projection_is_deterministic() {
        let model = sample_model();
        assert_eq!(project(&model), project(&model));
    }
}
