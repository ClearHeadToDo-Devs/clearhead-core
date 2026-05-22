//! Canonical JSON-LD export for the clearhead domain model.
//!
//! The vendored `actions.context.v4.json` and `actions.schema.v4.json` artifacts
//! in `src/resources/` are used so export behavior and tests remain stable
//! without network dependencies.
//!
//! ## Context node deferral
//!
//! Context tags (`+tag` in the DSL) are represented as provisional `Context`
//! nodes with `urn:context:<name>` identifiers. These URNs are **not** declared
//! in the v4 ontology — full context semantics (ontology classes, SKOS concept
//! scheme, real IRIs) are deferred until the domain model can express them
//! properly. The `_meta.context_nodes` field in the exported document records
//! this explicitly so consumers know not to rely on context node identity.

use super::{GraphError, Result};
use crate::domain::{ActionState, Action, Charter, DomainModel, Plan};
use serde_json::{Map, Value, json};
use std::collections::BTreeMap;

const ACTIONS_CONTEXT_V4: &str = include_str!("../resources/actions.context.v4.json");

/// Serialize a `DomainModel` into canonical compact JSON-LD.
pub fn serialize_domain_to_jsonld(model: &DomainModel) -> Result<String> {
    let document = build_jsonld_document(model)?;
    serde_json::to_string_pretty(&document).map_err(|e| GraphError::Syntax(e.to_string()))
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
        for act in &charter.actions {
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

    for act in model.all_actions() {
        nodes.push(action_to_jsonld(act));
    }

    nodes.sort_by_key(node_sort_key);

    // Build _meta block — records deferred semantics so consumers have
    // explicit notice rather than silently incomplete data.
    let context_count = collect_contexts(model).len();
    let meta = if context_count > 0 {
        json!({
            "context_nodes": {
                "status": "provisional",
                "count": context_count,
                "note": "Context nodes use urn:context:<name> provisional URNs. \
                          Full ontology support (SKOS scheme, real IRIs) is deferred."
            }
        })
    } else {
        json!({})
    };

    Ok(json!({
        "@context": context,
        "_meta": meta,
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

    if let Some(charter_id) = plan_charter_id.get(&plan_id) {
        insert_id(&mut node, "partOf", charter_id.clone());
    }

    let actions: Vec<String> = acts_by_plan.get(&plan_id).cloned().unwrap_or_default();
    insert_id_or_many(&mut node, "actions", actions);

    insert_str(&mut node, "uuid", &plan.id.to_string());

    if let Some(recurrence) = &plan.recurrence {
        insert_str(&mut node, "recurrence", &recurrence.to_string());
    }
    if let Some(recurrence) = &plan.due_recurrence {
        insert_str(&mut node, "dueRecurrence", &recurrence.to_string());
    }

    Value::Object(node)
}

fn action_to_jsonld(act: &Action) -> Value {
    let mut node = ordered_node(uuid_urn(act.id.to_string()), "Action");
    insert_str(&mut node, "name", &act.name);
    if let Some(description) = &act.description {
        insert_str(&mut node, "description", description);
    }
    if let Some(alias) = &act.alias {
        insert_str(&mut node, "alias", alias);
    }
    if let Some(priority) = act.priority {
        node.insert("priority".to_string(), json!(priority));
    }
    if let Some(contexts) = &act.contexts {
        let ids: Vec<String> = contexts.iter().map(|c| context_id(c)).collect();
        insert_id_or_many(&mut node, "requiresContext", ids);
    }
    if let Some(parent_id) = act.parent_id {
        insert_id(&mut node, "partOf", uuid_urn(parent_id.to_string()));
    }
    let deps_on = act.depends_on();
    if !deps_on.is_empty() {
        let deps: Vec<String> = deps_on.iter().map(|id| uuid_urn(id.to_string())).collect();
        insert_id_or_many(&mut node, "isSuccessorOf", deps);
    }
    insert_str(&mut node, "uuid", &act.id.to_string());
    insert_str(&mut node, "status", phase_term(act.state));

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
    for action in model.all_actions() {
        if let Some(values) = &action.contexts {
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

fn phase_term(phase: ActionState) -> &'static str {
    match phase {
        ActionState::NotStarted => "NotStarted",
        ActionState::InProgress => "InProgress",
        ActionState::Completed => "Completed",
        ActionState::BlockedOrAwaiting => "Blocked",
        ActionState::Cancelled => "Cancelled",
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
        "Action" => 4,
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
    use crate::domain::{Action, Charter, DomainModel, Plan, Recurrence};
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
                    recurrence: Some(Recurrence {
                        frequency: "weekly".to_string(),
                        interval: Some(2),
                        by_day: Some(vec!["MO".to_string(), "WE".to_string()]),
                        ..Default::default()
                    }),
                    ..Default::default()
                }],
                actions: vec![Action {
                    id: act_id,
                    name: "Write graph tests".to_string(),
                    description: Some("Lock down graph semantics".to_string()),
                    priority: Some(1),
                    contexts: Some(vec!["@dev".to_string()]),
                    alias: Some("graph_tests".to_string()),
                    is_sequential: Some(true),
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

    #[test]
    fn serialize_domain_to_jsonld_contains_context_and_graph() {
        let model = sample_model();
        let json = serialize_domain_to_jsonld(&model).expect("serialize jsonld");
        let doc: Value = serde_json::from_str(&json).expect("valid json");

        assert!(doc.get("@context").is_some());
        let graph = doc
            .get("@graph")
            .and_then(Value::as_array)
            .expect("@graph array");

        assert!(
            graph
                .iter()
                .any(|n| n.get("type") == Some(&json!("Charter")))
        );
        assert!(graph.iter().any(|n| n.get("type") == Some(&json!("Plan"))));
        assert!(
            graph
                .iter()
                .any(|n| n.get("type") == Some(&json!("Action")))
        );
    }

    #[test]
    fn jsonld_nodes_are_deterministically_sorted() {
        let model = sample_model();
        let json = serialize_domain_to_jsonld(&model).expect("serialize jsonld");
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
        let act_idx = types.iter().position(|t| t == "Action").unwrap();

        assert!(charter_idx < context_idx);
        assert!(context_idx < plan_idx);
        assert!(plan_idx < act_idx);
    }

    #[test]
    fn plan_and_act_fields_follow_contract_names() {
        let model = sample_model();
        let json = serialize_domain_to_jsonld(&model).expect("serialize jsonld");
        let doc: Value = serde_json::from_str(&json).expect("valid json");
        let graph = doc
            .get("@graph")
            .and_then(Value::as_array)
            .expect("@graph array");

        let plan = graph
            .iter()
            .find(|n| n.get("type") == Some(&json!("Plan")))
            .expect("plan node");
        assert!(plan.get("actions").is_some());
        assert!(plan.get("partOf").is_some());
        assert!(plan.get("uuid").is_some());
        assert!(plan.get("recurrence").is_some());

        let act = graph
            .iter()
            .find(|n| n.get("type") == Some(&json!("Action")))
            .expect("act node");
        assert!(act.get("name").is_some());
        assert!(act.get("status").is_some());
        assert!(act.get("scheduledAt").is_some());
        assert!(act.get("durationMinutes").is_some());
        assert!(act.get("externalScheduleId").is_some());
        assert!(act.get("externalOccurrenceKey").is_some());
        assert_eq!(act.get("status"), Some(&json!("InProgress")));
    }

    #[test]
    fn exported_jsonld_validates_against_vendored_schema() {
        let model = sample_model();
        let output: Value =
            serde_json::from_str(&serialize_domain_to_jsonld(&model).expect("serialize jsonld"))
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
