//! LSP telemetry: thin wrappers over the shared `clearhead_core` emitter.
//!
//! The domain types (`Tool`, `TelemetryEvent`, `TelemetryRecord`), the
//! `TelemetryEmitter` trait, and the concrete `NdjsonEmitter` (rotating monthly
//! NDJSON files) all live in `clearhead_core::telemetry`. This module only:
//!
//! - re-exports the types used by protocol handlers
//! - provides a module-level `emit_event` wrapper for call sites that don't yet
//!   inject an emitter via context

// Re-export core telemetry types used by protocol handlers.
pub use clearhead_core::telemetry::ndjson::NdjsonEmitter;
pub use clearhead_core::telemetry::{
    TelemetryEmitter, TelemetryEvent, Tool, event_from_field_change, event_from_state_change,
};

/// Build and emit a record from parts via the LSP's NDJSON emitter.
pub fn emit_event(
    tool: Tool,
    action_uuid: Option<String>,
    event: TelemetryEvent,
) -> Result<(), String> {
    NdjsonEmitter.emit_event(tool, action_uuid, event)
}
