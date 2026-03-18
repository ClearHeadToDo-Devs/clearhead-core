//! Telemetry trait and domain types for semantic event logging.
//!
//! This module defines the *interface* for telemetry — the domain event types,
//! the envelope record, and the `TelemetryEmitter` trait. It deliberately
//! contains no I/O. Concrete implementations (NDJSON files, RDF triples,
//! network transport) live in downstream crates.
//!
//! # Design
//!
//! - `TelemetryEvent` — the semantic events (what happened in domain terms)
//! - `TelemetryRecord` — envelope: event + timestamp + tool + action UUID
//! - `TelemetryEmitter` — trait any consumer implements to receive events
//! - `NoopEmitter` — zero-cost stub for tests or consumers that don't need telemetry
//!
//! # Why `Arc<dyn TelemetryEmitter>`?
//!
//! The LSP backend clones its state across async handlers. The future sync
//! server will send events across threads. `Arc` makes both work without
//! requiring `Clone` on the emitter itself.

use chrono::{DateTime, Utc};
use serde::Serialize;
use std::sync::Arc;

use crate::workspace::actions::FieldChange;
use crate::ActionState;

// =============================================================================
// Domain Types
// =============================================================================

/// Tool identifier indicating the source of a telemetry event.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Tool {
    /// Command-line interface
    Cli,
    /// Language Server Protocol implementation
    Lsp,
    /// Sync daemon or operation
    Sync,
}

/// Semantic domain events.
///
/// Each variant corresponds to something meaningful that happened — useful for
/// debugging, analytics, audit, and (eventually) cross-node coordination.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "event")]
pub enum TelemetryEvent {
    // =========================================================================
    // Action Lifecycle
    // =========================================================================
    #[serde(rename = "action_created")]
    ActionCreated { name: String, file_path: String },

    #[serde(rename = "action_completed")]
    ActionCompleted { name: String, completed_at: String },

    #[serde(rename = "action_cancelled")]
    ActionCancelled {
        name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
    },

    #[serde(rename = "action_started")]
    ActionStarted { name: String },

    #[serde(rename = "action_blocked")]
    ActionBlocked {
        name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
    },

    #[serde(rename = "action_restarted")]
    ActionRestarted {
        name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
    },

    #[serde(rename = "action_deleted")]
    ActionDeleted { name: String },

    // =========================================================================
    // Property Changes
    // =========================================================================
    #[serde(rename = "priority_changed")]
    PriorityChanged {
        old_priority: Option<u32>,
        new_priority: Option<u32>,
    },

    #[serde(rename = "due_date_changed")]
    DueDateChanged {
        old_date: Option<String>,
        new_date: Option<String>,
    },

    #[serde(rename = "name_changed")]
    NameChanged { old_name: String, new_name: String },

    #[serde(rename = "context_added")]
    ContextAdded { context: String },

    #[serde(rename = "context_removed")]
    ContextRemoved { context: String },

    // =========================================================================
    // Recurring Action Instances
    // =========================================================================
    #[serde(rename = "instance_generated")]
    InstanceGenerated {
        template_uuid: String,
        occurrence_date: String,
    },

    #[serde(rename = "instance_completed")]
    InstanceCompleted {
        template_uuid: String,
        scheduled_date: String,
        completed_date: String,
    },

    #[serde(rename = "instance_skipped")]
    InstanceSkipped {
        template_uuid: String,
        occurrence_date: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
    },

    #[serde(rename = "template_edited")]
    TemplateEdited {
        template_uuid: String,
        fields_changed: Vec<String>,
    },

    // =========================================================================
    // Sync / Coordination
    // =========================================================================
    #[serde(rename = "sync_started")]
    SyncStarted { remote: String, direction: String },

    #[serde(rename = "sync_completed")]
    SyncCompleted {
        remote: String,
        changes_sent: u32,
        changes_received: u32,
    },

    #[serde(rename = "sync_failed")]
    SyncFailed { remote: String, error: String },

    #[serde(rename = "conflict_resolved")]
    ConflictResolved {
        action_uuid: String,
        resolution: String,
    },

    // =========================================================================
    // System
    // =========================================================================
    #[serde(rename = "workspace_opened")]
    WorkspaceOpened { path: String, action_count: usize },

    #[serde(rename = "file_parsed")]
    FileParsed {
        file_path: String,
        action_count: usize,
        parse_time_ms: u64,
    },

    #[serde(rename = "lsp_started")]
    LspStarted {
        #[serde(skip_serializing_if = "Option::is_none")]
        port: Option<u16>,
    },
}

impl TelemetryEvent {
    /// Snake_case event name — matches JSON serialization.
    pub fn name(&self) -> &'static str {
        match self {
            TelemetryEvent::ActionCreated { .. } => "action_created",
            TelemetryEvent::ActionCompleted { .. } => "action_completed",
            TelemetryEvent::ActionCancelled { .. } => "action_cancelled",
            TelemetryEvent::ActionStarted { .. } => "action_started",
            TelemetryEvent::ActionBlocked { .. } => "action_blocked",
            TelemetryEvent::ActionRestarted { .. } => "action_restarted",
            TelemetryEvent::ActionDeleted { .. } => "action_deleted",
            TelemetryEvent::PriorityChanged { .. } => "priority_changed",
            TelemetryEvent::DueDateChanged { .. } => "due_date_changed",
            TelemetryEvent::NameChanged { .. } => "name_changed",
            TelemetryEvent::ContextAdded { .. } => "context_added",
            TelemetryEvent::ContextRemoved { .. } => "context_removed",
            TelemetryEvent::InstanceGenerated { .. } => "instance_generated",
            TelemetryEvent::InstanceCompleted { .. } => "instance_completed",
            TelemetryEvent::InstanceSkipped { .. } => "instance_skipped",
            TelemetryEvent::TemplateEdited { .. } => "template_edited",
            TelemetryEvent::SyncStarted { .. } => "sync_started",
            TelemetryEvent::SyncCompleted { .. } => "sync_completed",
            TelemetryEvent::SyncFailed { .. } => "sync_failed",
            TelemetryEvent::ConflictResolved { .. } => "conflict_resolved",
            TelemetryEvent::WorkspaceOpened { .. } => "workspace_opened",
            TelemetryEvent::FileParsed { .. } => "file_parsed",
            TelemetryEvent::LspStarted { .. } => "lsp_started",
        }
    }
}

/// Envelope wrapping a `TelemetryEvent` with common metadata.
///
/// Serializes as a flat JSON object (event fields merged in via `#[serde(flatten)]`).
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct TelemetryRecord {
    pub timestamp: String,
    pub tool: Tool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub action_uuid: Option<String>,
    #[serde(flatten)]
    pub event: TelemetryEvent,
}

impl TelemetryRecord {
    pub fn new(tool: Tool, action_uuid: Option<String>, event: TelemetryEvent) -> Self {
        Self {
            timestamp: Utc::now().to_rfc3339(),
            tool,
            action_uuid,
            event,
        }
    }

    pub fn with_timestamp(
        timestamp: DateTime<Utc>,
        tool: Tool,
        action_uuid: Option<String>,
        event: TelemetryEvent,
    ) -> Self {
        Self {
            timestamp: timestamp.to_rfc3339(),
            tool,
            action_uuid,
            event,
        }
    }
}

// =============================================================================
// Trait
// =============================================================================

/// Accepts telemetry records. Implement this to send events anywhere.
///
/// Implementations: NDJSON files (CLI), RDF triples (future), network
/// (sync server), no-op (tests). Use `Arc<dyn TelemetryEmitter>` at
/// call sites so the same instance can be shared across threads/handlers.
pub trait TelemetryEmitter: Send + Sync {
    fn emit(&self, record: TelemetryRecord) -> Result<(), String>;

    /// Convenience: construct a record from parts and emit it.
    fn emit_event(
        &self,
        tool: Tool,
        action_uuid: Option<String>,
        event: TelemetryEvent,
    ) -> Result<(), String> {
        self.emit(TelemetryRecord::new(tool, action_uuid, event))
    }
}

/// Zero-cost emitter for tests or consumers that don't need telemetry.
pub struct NoopEmitter;

impl TelemetryEmitter for NoopEmitter {
    fn emit(&self, _record: TelemetryRecord) -> Result<(), String> {
        Ok(())
    }
}

/// Convenience: wrap a `NoopEmitter` in an `Arc` ready for injection.
pub fn noop_emitter() -> Arc<dyn TelemetryEmitter> {
    Arc::new(NoopEmitter)
}

// =============================================================================
// Conversion Helpers
// =============================================================================

/// Map a `FieldChange` to a `TelemetryEvent`, if a semantic mapping exists.
///
/// Returns `None` for changes that have no corresponding event (description,
/// state — state is handled separately by `event_from_state_change`).
pub fn event_from_field_change(change: &FieldChange) -> Option<TelemetryEvent> {
    match change {
        FieldChange::Name { old, new } => Some(TelemetryEvent::NameChanged {
            old_name: old.clone(),
            new_name: new.clone(),
        }),
        FieldChange::Priority { old, new } => Some(TelemetryEvent::PriorityChanged {
            old_priority: *old,
            new_priority: *new,
        }),
        FieldChange::DoDate { old, new } => Some(TelemetryEvent::DueDateChanged {
            old_date: old.clone(),
            new_date: new.clone(),
        }),
        // State transitions handled by event_from_state_change (needs action name)
        FieldChange::State { .. } => None,
        _ => None,
    }
}

/// Map an `ActionState` transition to a `TelemetryEvent`.
///
/// Returns `None` for transitions with no semantic meaning (e.g. same state).
pub fn event_from_state_change(
    old: ActionState,
    new: ActionState,
    name: &str,
) -> Option<TelemetryEvent> {
    match (old, new) {
        (_, ActionState::Completed) => Some(TelemetryEvent::ActionCompleted {
            name: name.to_string(),
            completed_at: Utc::now().to_rfc3339(),
        }),
        (_, ActionState::InProgress) => Some(TelemetryEvent::ActionStarted {
            name: name.to_string(),
        }),
        (_, ActionState::BlockedorAwaiting) => Some(TelemetryEvent::ActionBlocked {
            name: name.to_string(),
            reason: None,
        }),
        (_, ActionState::Cancelled) => Some(TelemetryEvent::ActionCancelled {
            name: name.to_string(),
            reason: None,
        }),
        (ActionState::Completed, ActionState::NotStarted) => {
            Some(TelemetryEvent::ActionRestarted {
                name: name.to_string(),
                reason: None,
            })
        }
        _ => None,
    }
}
