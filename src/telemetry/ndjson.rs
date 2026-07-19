//! NDJSON file emitter: the default concrete [`TelemetryEmitter`].
//!
//! Writes each [`TelemetryRecord`] as one line of JSON to a monthly rotating
//! file under the XDG state directory. Every ClearHead runtime (CLI, LSP, and
//! future services) shares this single implementation so telemetry lands in one
//! place with one format.
//!
//! The [interface module](super) stays I/O-free; this submodule is where the
//! filesystem lives.

use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;

use tracing::info;

use super::{TelemetryEmitter, TelemetryRecord};

/// XDG state directory for ClearHead telemetry files.
pub fn get_telemetry_dir() -> PathBuf {
    let state_dir = dirs::state_dir()
        .or_else(|| dirs::data_local_dir().map(|p| p.join("state")))
        .unwrap_or_else(|| {
            dirs::home_dir()
                .expect("Could not determine home directory")
                .join(".local")
                .join("state")
        });
    state_dir.join("clearhead").join("telemetry")
}

fn get_current_file() -> PathBuf {
    let now = chrono::Utc::now();
    let filename = format!("events-{}.ndjson", now.format("%Y-%m"));
    get_telemetry_dir().join(filename)
}

fn ensure_telemetry_dir() -> Result<(), String> {
    fs::create_dir_all(get_telemetry_dir())
        .map_err(|e| format!("Failed to create telemetry directory: {}", e))
}

/// Writes `TelemetryRecord`s as newline-delimited JSON to monthly rotating files.
///
/// Files land in [`get_telemetry_dir`] as `events-YYYY-MM.ndjson`.
pub struct NdjsonEmitter;

impl TelemetryEmitter for NdjsonEmitter {
    fn emit(&self, record: TelemetryRecord) -> Result<(), String> {
        ensure_telemetry_dir()?;

        let json = serde_json::to_string(&record)
            .map_err(|e| format!("Failed to serialize telemetry event: {}", e))?;

        let file_path = get_current_file();
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&file_path)
            .map_err(|e| format!("Failed to open telemetry file: {}", e))?;

        writeln!(file, "{}", json)
            .map_err(|e| format!("Failed to write telemetry event: {}", e))?;

        info!(
            event = record.event.name(),
            action_uuid = ?record.action_uuid,
            tool = ?record.tool,
            "telemetry"
        );

        Ok(())
    }
}
