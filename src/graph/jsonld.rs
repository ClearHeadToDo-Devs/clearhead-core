//! Canonical graph-derived JSON-LD export.
//!
//! This module intentionally sits at the semantic waist:
//! - input: canonical RDF graph semantics
//! - output: canonical compact JSON-LD document (`@context` + `@graph`)
//!
//! The vendored `actions.context.v4.json` and `actions.schema.v4.json` artifacts
//! in `src/resources/` are used so export behavior and tests remain stable
//! without network dependencies.

use super::{create_store, GraphError, Result, Store};
use crate::domain::{ActPhase, Charter, DomainModel, Plan};
use crate::graph::{load_domain_model, load_domain_model_from_store};
use serde_json::{json, Map, Value};
use std::collections::BTreeMap;

const ACTIONS_CONTEXT_V4: &str = include_str!("../resources/actions.context.v4.json");

/// Serialize a store into canonical compact JSON-LD.
///
/// # Example
///
/// ```
/// use clearhead_core::domain::{ActPhase, Charter, DomainModel, Plan, PlannedAct};
/// use clearhead_core::graph::{create_store, load_domain_model, serialize_store_to_jsonld};
/// use uuid::Uuid;
///
/// let plan_id = Uuid::new_v4();
/// let model = DomainModel {
///     objectives: vec![],
///     charters: vec![Charter {
///         id: Uuid::new_v4(),
///         title: "Demo".to_string(),
///         description: None,
///         alias: None,
///         parent: None,
///         objectives: None,
///         plans: vec![Plan {
///             id: plan_id,
///             name: "Write docs".to_string(),
///             description: None,
///             priority: None,
///             contexts: None,
///             recurrence: None,
///             due_recurrence: None,
///             parent: None,
///             alias: None,
///             is_sequential: None,
///             depends_on: None,
///         }],
///         acts: vec![PlannedAct {
///             id: Uuid::new_v4(),
///             plan_id: Some(plan_id),
///             external_schedule_id: None,
///             external_occurrence_key: None,
///             phase: ActPhase::NotStarted,
///             scheduled_at: None,
///             due_date: None,
///             duration: Some(30),
///             completed_at: None,
///             created_at: None,
///         }],
///     }],
/// };
///
/// let store = create_store().unwrap();
/// load_domain_model(&store, &model).unwrap();
/// let jsonld = serialize_store_to_jsonld(&store).unwrap();
/// assert!(jsonld.contains("\"@context\""));
/// assert!(jsonld.contains("\"@graph\""));
/// ```
pub fn serialize_store_to_jsonld(store: &Store) -> Result<String> {
    let model = load_domain_model_from_store(store)?;
    let document = build_jsonld_document(&model)?;
    serde_json::to_string_pretty(&document).map_err(|e| GraphError::Syntax(e.to_string()))
}

/// Serialize a `DomainModel` into canonical compact JSON-LD.
///
/// This helper is equivalent to `DomainModel -> Store -> JSON-LD`.
pub fn serialize_domain_to_jsonld(model: &DomainModel) -> Result<String> {
    let store = create_store()?;
    load_domain_model(&store, model)?;
    serialize_store_to_jsonld(&store)
}

fn build_jsonld_document(model: &DomainModel) -> Result<Value> {
    let context_value: Value = serde_json::from_str(ACTIONS_CONTEXT_V4)
        .map_err(|e| GraphError::Syntax(format!("Invalid vendored actions context JSON: {e}")))?;
    let context = context_value
        .get("@context")
        .cloned()
        .ok_or_else(|| GraphError::Syntax("Vendored context missing @context".to_string()))?;

    let mut nodes: Vec<Value> = Vec::new();

    let mut plan_charter_id: BTreeMap<String, String> = BTreeMap::new();
    let mut charter_children: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut alias_to_charter_id: BTreeMap<String, String> = BTreeMap::new();
    let mut title_to_charter_id: BTreeMap<String, String> = BTreeMap::new();
    let mut acts_by_plan: BTreeMap<String, Vec<String>> = BTreeMap::new();

    for charter in &model.charters {
        let charter_id = uuid_urn(charter.id.to_string());
        if let Some(alias) = &charter.alias {
            alias_to_charter_id.insert(alias.to_lowercase(), charter_id.clone());
        }
        title_to_charter_id.insert(charter.title.to_lowercase(), charter_id.clone());

        for plan in &charter.plans {
            plan_charter_id.insert(uuid_urn(plan.id.to_string()), charter_id.clone());
        }
        for act in &charter.acts {
            if let Some(plan_id) = act.plan_id {
                acts_by_plan
                    .entry(uuid_urn(plan_id.to_string()))
                    .or_default()
                    .push(uuid_urn(act.id.to_string()));
            }
        }
    }

    for charter in &model.charters {
        if let Some(parent_ref) = &charter.parent {
            let parent_id = alias_to_charter_id
                .get(&parent_ref.to_lowercase())
                .or_else(|| title_to_charter_id.get(&parent_ref.to_lowercase()));
            if let Some(parent_id) = parent_id {
                charter_children
                    .entry(parent_id.clone())
                    .or_default()
                    .push(uuid_urn(charter.id.to_string()));
            }
        }
    }

    let contexts = collect_contexts(model);
    nodes.extend(contexts.into_iter().map(context_to_jsonld));

    for charter in &model.charters {
        nodes.push(charter_to_jsonld(charter, &charter_children));
    }

    for charter in &model.charters {
        for plan in &charter.plans {
            nodes.push(plan_to_jsonld(plan, &plan_charter_id, &acts_by_plan));
        }
    }

    for act in model.all_acts() {
        nodes.push(planned_act_to_jsonld(act));
    }

    nodes.sort_by_key(node_sort_key);

    Ok(json!({
        "@context": context,
        "@graph": nodes,
    }))
}

fn charter_to_jsonld(charter: &Charter, charter_children: &BTreeMap<String, Vec<String>>) -> Value {
    let id = uuid_urn(charter.id.to_string());
    let mut node = ordered_node(id, "Charter");
    insert_str(&mut node, "name", &charter.title);

    if let Some(description) = &charter.description {
        insert_str(&mut node, "description", description);
    }
    if let Some(children) =
        charter_children.get(node.get("id").and_then(Value::as_str).unwrap_or(""))
    {
        insert_id_or_many(&mut node, "subCharters", children.clone());
    }

    Value::Object(node)
}

fn plan_to_jsonld(
    plan: &Plan,
    plan_charter_id: &BTreeMap<String, String>,
    acts_by_plan: &BTreeMap<String, Vec<String>>,
) -> Value {
    let plan_id = uuid_urn(plan.id.to_string());
    let mut node = ordered_node(plan_id.clone(), "Plan");

    insert_str(&mut node, "name", &plan.name);
    if let Some(description) = &plan.description {
        insert_str(&mut node, "description", description);
    }

    if let Some(parent_plan_id) = plan.parent {
        insert_id(&mut node, "partOf", uuid_urn(parent_plan_id.to_string()));
    } else if let Some(charter_id) = plan_charter_id.get(&plan_id) {
        insert_id(&mut node, "partOf", charter_id.clone());
    }

    let planned_acts: Vec<String> = acts_by_plan.get(&plan_id).cloned().unwrap_or_default();
    insert_id_or_many(&mut node, "plannedActs", planned_acts);

    if let Some(depends_on) = &plan.depends_on {
        let deps: Vec<String> = depends_on
            .iter()
            .map(|id| uuid_urn(id.to_string()))
            .collect();
        insert_id_or_many(&mut node, "isSuccessorOf", deps);
    }

    if let Some(contexts) = &plan.contexts {
        let ids: Vec<String> = contexts.iter().map(|c| context_id(c)).collect();
        insert_id_or_many(&mut node, "requiresContext", ids);
    }

    insert_str(&mut node, "uuid", &plan.id.to_string());

    if let Some(alias) = &plan.alias {
        insert_str(&mut node, "alias", alias);
    }
    if let Some(priority) = plan.priority {
        node.insert("priority".to_string(), json!(priority));
    }
    if let Some(is_seq) = plan.is_sequential {
        node.insert("sequentialChildren".to_string(), json!(is_seq));
    }
    if let Some(recurrence) = &plan.recurrence {
        insert_str(&mut node, "recurrence", &recurrence.to_string());
    }
    if let Some(recurrence) = &plan.due_recurrence {
        insert_str(&mut node, "dueRecurrence", &recurrence.to_string());
    }

    Value::Object(node)
}

fn planned_act_to_jsonld(act: &crate::domain::PlannedAct) -> Value {
    let mut node = ordered_node(uuid_urn(act.id.to_string()), "PlannedAct");
    insert_str(&mut node, "status", phase_term(act.phase));

    if let Some(scheduled) = act.scheduled_at {
        insert_str(&mut node, "scheduledAt", &scheduled.to_rfc3339());
    }
    if let Some(due) = act.due_date {
        insert_str(&mut node, "dueDate", &due.to_rfc3339());
    }
    if let Some(completed) = act.completed_at {
        insert_str(&mut node, "completedDate", &completed.to_rfc3339());
    }
    if let Some(duration) = act.duration {
        node.insert("durationMinutes".to_string(), json!(duration));
    }
    if let Some(external_schedule_id) = &act.external_schedule_id {
        insert_str(&mut node, "externalScheduleId", external_schedule_id);
    }
    if let Some(external_occurrence_key) = &act.external_occurrence_key {
        insert_str(&mut node, "externalOccurrenceKey", external_occurrence_key);
    }

    Value::Object(node)
}

fn collect_contexts(model: &DomainModel) -> Vec<String> {
    let mut contexts = BTreeMap::new();
    for plan in model.all_plans() {
        if let Some(values) = &plan.contexts {
            for value in values {
                let id = context_id(value);
                contexts.insert(id, value.clone());
            }
        }
    }
    contexts.values().cloned().collect()
}

fn context_to_jsonld(context: String) -> Value {
    let mut node = ordered_node(context_id(&context), "Context");
    insert_str(&mut node, "name", &context);
    insert_str(&mut node, "contextIdentifier", &context);
    Value::Object(node)
}

fn context_id(context: &str) -> String {
    let normalized = context
        .trim()
        .trim_start_matches('@')
        .to_lowercase()
        .replace(' ', "-");
    format!("urn:context:{}", normalized)
}

fn uuid_urn(id: String) -> String {
    format!("urn:uuid:{id}")
}

fn phase_term(phase: ActPhase) -> &'static str {
    match phase {
        ActPhase::NotStarted => "NotStarted",
        ActPhase::InProgress => "InProgress",
        ActPhase::Completed => "Completed",
        ActPhase::Blocked => "Blocked",
        ActPhase::Cancelled => "Cancelled",
    }
}

fn ordered_node(id: String, type_name: &str) -> Map<String, Value> {
    let mut node = Map::new();
    node.insert("id".to_string(), Value::String(id));
    node.insert("type".to_string(), Value::String(type_name.to_string()));
    node
}

fn insert_str(node: &mut Map<String, Value>, key: &str, value: &str) {
    node.insert(key.to_string(), Value::String(value.to_string()));
}

fn insert_id(node: &mut Map<String, Value>, key: &str, value: String) {
    node.insert(key.to_string(), Value::String(value));
}

fn insert_id_or_many(node: &mut Map<String, Value>, key: &str, values: Vec<String>) {
    if values.is_empty() {
        return;
    }
    if values.len() == 1 {
        node.insert(key.to_string(), Value::String(values[0].clone()));
    } else {
        node.insert(
            key.to_string(),
            Value::Array(values.into_iter().map(Value::String).collect()),
        );
    }
}

fn node_sort_key(node: &Value) -> (u8, String) {
    let type_name = node.get("type").and_then(Value::as_str).unwrap_or_default();
    let type_rank = match type_name {
        "Charter" => 0,
        "Objective" => 1,
        "Context" => 2,
        "Plan" => 3,
        "PlannedAct" => 4,
        "ContextType" => 5,
        _ => 255,
    };
    let id = node
        .get("id")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_string();
    (type_rank, id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Charter, DomainModel, Plan, PlannedAct, Recurrence};
    use crate::graph::create_store;
    use chrono::TimeZone;
    use jsonschema::JSONSchema;
    use serde_json::json;
    use uuid::Uuid;

    const ACTIONS_SCHEMA_V4: &str = include_str!("../resources/actions.schema.v4.json");
    const ONTOLOGY_EXAMPLE_V4: &str = include_str!("../resources/ontology-out.example.v4.jsonld");

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
                    contexts: Some(vec!["@dev".to_string()]),
                    recurrence: Some(Recurrence {
                        frequency: "weekly".to_string(),
                        interval: Some(2),
                        by_day: Some(vec!["MO".to_string(), "WE".to_string()]),
                        ..Default::default()
                    }),
                    alias: Some("graph_tests".to_string()),
                    is_sequential: Some(true),
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

    #[test]
    fn serialize_store_to_jsonld_contains_context_and_graph() {
        let store = create_store().expect("store");
        let model = sample_model();
        load_domain_model(&store, &model).expect("load model");

        let json = serialize_store_to_jsonld(&store).expect("serialize jsonld");
        let doc: Value = serde_json::from_str(&json).expect("valid json");

        assert!(doc.get("@context").is_some());
        let graph = doc
            .get("@graph")
            .and_then(Value::as_array)
            .expect("@graph array");

        assert!(graph
            .iter()
            .any(|n| n.get("type") == Some(&json!("Charter"))));
        assert!(graph.iter().any(|n| n.get("type") == Some(&json!("Plan"))));
        assert!(graph
            .iter()
            .any(|n| n.get("type") == Some(&json!("PlannedAct"))));
    }

    #[test]
    fn jsonld_nodes_are_deterministically_sorted() {
        let store = create_store().expect("store");
        let model = sample_model();
        load_domain_model(&store, &model).expect("load model");

        let json = serialize_store_to_jsonld(&store).expect("serialize jsonld");
        let doc: Value = serde_json::from_str(&json).expect("valid json");
        let graph = doc
            .get("@graph")
            .and_then(Value::as_array)
            .expect("@graph array");

        let types: Vec<String> = graph
            .iter()
            .filter_map(|node| node.get("type").and_then(Value::as_str))
            .map(|s| s.to_string())
            .collect();

        let charter_idx = types.iter().position(|t| t == "Charter").unwrap();
        let context_idx = types.iter().position(|t| t == "Context").unwrap();
        let plan_idx = types.iter().position(|t| t == "Plan").unwrap();
        let act_idx = types.iter().position(|t| t == "PlannedAct").unwrap();

        assert!(charter_idx < context_idx);
        assert!(context_idx < plan_idx);
        assert!(plan_idx < act_idx);
    }

    #[test]
    fn plan_and_act_fields_follow_contract_names() {
        let store = create_store().expect("store");
        let model = sample_model();
        load_domain_model(&store, &model).expect("load model");

        let json = serialize_store_to_jsonld(&store).expect("serialize jsonld");
        let doc: Value = serde_json::from_str(&json).expect("valid json");
        let graph = doc
            .get("@graph")
            .and_then(Value::as_array)
            .expect("@graph array");

        let plan = graph
            .iter()
            .find(|n| n.get("type") == Some(&json!("Plan")))
            .expect("plan node");
        assert!(plan.get("plannedActs").is_some());
        assert!(plan.get("partOf").is_some());
        assert!(plan.get("uuid").is_some());
        assert!(plan.get("sequentialChildren").is_some());
        assert!(plan.get("recurrence").is_some());
        assert!(plan.get("requiresContext").is_some());

        let act = graph
            .iter()
            .find(|n| n.get("type") == Some(&json!("PlannedAct")))
            .expect("act node");
        assert!(act.get("status").is_some());
        assert!(act.get("scheduledAt").is_some());
        assert!(act.get("durationMinutes").is_some());
        assert!(act.get("externalScheduleId").is_some());
        assert!(act.get("externalOccurrenceKey").is_some());
        assert_eq!(act.get("status"), Some(&json!("InProgress")));
    }

    #[test]
    fn exported_jsonld_validates_against_vendored_schema() {
        let store = create_store().expect("store");
        let model = sample_model();
        load_domain_model(&store, &model).expect("load model");

        let output: Value =
            serde_json::from_str(&serialize_store_to_jsonld(&store).expect("serialize jsonld"))
                .expect("json parse");
        let schema: Value = serde_json::from_str(ACTIONS_SCHEMA_V4).expect("schema parse");

        let validator = JSONSchema::compile(&schema).expect("compile schema");
        if let Err(errors) = validator.validate(&output) {
            let lines: Vec<String> = errors.map(|e| e.to_string()).collect();
            panic!("schema validation failed: {}", lines.join("; "));
        }
    }

    #[test]
    fn ontology_example_validates_against_vendored_schema() {
        let schema: Value = serde_json::from_str(ACTIONS_SCHEMA_V4).expect("schema parse");
        let example: Value = serde_json::from_str(ONTOLOGY_EXAMPLE_V4).expect("example parse");

        let validator = JSONSchema::compile(&schema).expect("compile schema");
        if let Err(errors) = validator.validate(&example) {
            let lines: Vec<String> = errors.map(|e| e.to_string()).collect();
            panic!(
                "ontology example failed schema validation: {}",
                lines.join("; ")
            );
        }
    }
}
