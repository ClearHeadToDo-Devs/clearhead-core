//! CLI telemetry: thin wrappers over the shared `clearhead_core` emitter.
//!
//! The domain types, the `TelemetryEmitter` trait, and the concrete
//! `NdjsonEmitter` (rotating monthly NDJSON files) all live in
//! `clearhead_core::telemetry`. This module only:
//!
//! - re-exports those types so existing `clearhead_cli::telemetry::*` imports
//!   keep working unchanged
//! - provides module-level `emit` / `emit_event` wrappers for call sites that
//!   haven't been migrated to an injected emitter

pub use clearhead_core::telemetry::ndjson::{NdjsonEmitter, get_telemetry_dir};
pub use clearhead_core::telemetry::{
    NoopEmitter, TelemetryEmitter, TelemetryEvent, TelemetryRecord, Tool, event_from_field_change,
    event_from_state_change, noop_emitter,
};

/// Emit a pre-built `TelemetryRecord` via the CLI's NDJSON emitter.
pub fn emit(record: TelemetryRecord) -> Result<(), String> {
    NdjsonEmitter.emit(record)
}

/// Build and emit a record from parts via the CLI's NDJSON emitter.
pub fn emit_event(
    tool: Tool,
    action_uuid: Option<String>,
    event: TelemetryEvent,
) -> Result<(), String> {
    NdjsonEmitter.emit_event(tool, action_uuid, event)
}
