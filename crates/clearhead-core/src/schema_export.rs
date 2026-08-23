//! Project domain [`Action`]s onto the canonical JSON serialization defined by
//! the specifications repo's `schemas/actions.schema.json`.
//!
//! This is a dedicated *export* representation, deliberately separate from the
//! `Serialize` derive on [`Action`] (which drives the JSON-LD graph path and
//! must not change shape). Field names mirror the v4 actions ontology minus the
//! `has` prefix, so a single grep connects a field here to its ontology property
//! and its `.actions` DSL token.

use crate::{Action, ActionState};
use serde::Serialize;

/// Top-level `{ "actions": [...] }` document matching the schema root.
#[derive(Debug, Clone, Serialize)]
pub struct SchemaDocument {
    pub actions: Vec<SchemaAction>,
}

/// One action in the canonical serialization shape. Absent optionals are
/// omitted (never serialized as `null`) so the output satisfies the schema's
/// typed, `additionalProperties: false` definition.
#[derive(Debug, Clone, Serialize)]
pub struct SchemaAction {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    pub state: ActionState,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub priority: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub contexts: Option<Vec<String>>,
    #[serde(rename = "scheduledDateTime", skip_serializing_if = "Option::is_none")]
    pub scheduled_date_time: Option<String>,
    #[serde(rename = "durationMinutes", skip_serializing_if = "Option::is_none")]
    pub duration_minutes: Option<u32>,
    #[serde(rename = "dueDateTime", skip_serializing_if = "Option::is_none")]
    pub due_date_time: Option<String>,
    #[serde(rename = "completedDateTime", skip_serializing_if = "Option::is_none")]
    pub completed_date_time: Option<String>,
    #[serde(rename = "createdDateTime", skip_serializing_if = "Option::is_none")]
    pub created_date_time: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub charter: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alias: Option<String>,
    #[serde(rename = "sequentialChildren", skip_serializing_if = "Option::is_none")]
    pub sequential_children: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub predecessors: Option<Vec<String>>,
    #[serde(
        rename = "externalOccurrenceKey",
        skip_serializing_if = "Option::is_none"
    )]
    pub external_occurrence_key: Option<String>,
    #[serde(rename = "parentId", skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,
}

impl SchemaAction {
    /// Project a domain [`Action`] onto the canonical schema shape.
    ///
    /// Core stores the hierarchy flat (via `parent_id`), which the schema
    /// permits — `children` nesting is an alternative serialization the schema
    /// also allows but this projection does not emit.
    pub fn from_action(action: &Action) -> Self {
        SchemaAction {
            id: Some(action.id.to_string()),
            state: action.state,
            name: action.name.clone(),
            description: action.description.clone(),
            priority: action.priority,
            contexts: action.contexts.clone(),
            scheduled_date_time: action.scheduled_at.map(|d| d.to_rfc3339()),
            duration_minutes: action.duration,
            due_date_time: action.due_date.map(|d| d.to_rfc3339()),
            completed_date_time: action.completed_at.map(|d| d.to_rfc3339()),
            created_date_time: action.created_at.map(|d| d.to_rfc3339()),
            charter: action.charter.clone(),
            alias: action.alias.clone(),
            sequential_children: action.is_sequential,
            predecessors: action.predecessors.as_ref().map(|refs| {
                refs.iter()
                    .map(|p| {
                        p.resolved_uuid
                            .map(|u| u.to_string())
                            .unwrap_or_else(|| p.raw_ref.clone())
                    })
                    .collect()
            }),
            external_occurrence_key: action.external_occurrence_key.clone(),
            parent_id: action.parent_id.map(|u| u.to_string()),
        }
    }
}

/// Project a flat list of actions onto the canonical schema document.
pub fn to_schema_document(actions: &[Action]) -> SchemaDocument {
    SchemaDocument {
        actions: actions.iter().map(SchemaAction::from_action).collect(),
    }
}
