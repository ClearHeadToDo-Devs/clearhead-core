//! ClearHead CLI Library
//!
//! This library provides the command-line implementation for the ClearHead framework.
//! It builds on `clearhead_core`'s shared domain, workspace, and configuration services,
//! adding command orchestration, terminal presentation, CLI-specific settings, and process
//! concerns. The editor protocol runtime lives in the separate `clearhead-lsp` crate.

use clearhead_core::WorkspaceConfig;
use tree_sitter::Tree;

// Re-export core library types and functions
pub use clearhead_core::{
    Action, ActionList, ActionState, Charter, DomainModel, IntegrityError, OutputFormat,
    ParseFailure, ParseMode, ParseOutcome, ParsedDocument, Plan, RecoveryReport, SourceMetadata,
    SourceRange, TrustedDocument, format, format_charter, format_trusted_source, implicit_charter,
    parse_actions, parse_actions_with_mode, parse_charter, parse_document, parse_tree,
    parse_trusted_document, patch_action_list,
};

pub use clearhead_core::format::{FormatConfig, FormatStyle, IndentStyle};
pub use clearhead_core::workspace::actions::TableFormatOptions;

// Re-export environment_reader so command handlers can call resolve_workspace_paths
// without reaching back through the CLI binary layer.
pub use environment_reader::resolve_workspace_paths;

pub use clearhead_core::workspace::actions::{
    LintDiagnostic, LintResults, LintSeverity, lint_document,
};

pub mod display;
pub use display::{render_charter_tree, render_domain_tree};

pub mod export;
pub use export::format_as_icalendar;

pub mod mutations;
pub use mutations::{ActionUpdate, apply_updates};

pub mod environment_reader;
pub use environment_reader::{Config, get_config_dir, get_data_dir, load_config};

pub mod graph_backend;
pub use graph_backend::serialize_domain_to_jsonld;

/// Exposes the CLI argument parser so tooling (e.g. `gen-man`) can build the
/// `clap::Command` tree without depending on the binary entry point.
pub mod argparser;

pub mod telemetry;
pub use telemetry::{
    TelemetryEvent, TelemetryRecord, Tool, emit, emit_event, event_from_field_change,
    event_from_state_change, get_telemetry_dir,
};

// CLI wrappers for backward compatibility

/// Parse a .actions file into a structured ActionList
pub fn get_action_list_struct(actions: &str) -> Result<ActionList, String> {
    parse_actions(actions)
}

/// Parse a .actions file into a ParsedDocument (Actions + Source Metadata)
pub fn get_parsed_document(actions: &str) -> Result<ParsedDocument, String> {
    parse_document(actions)
}

/// Parse a .actions file into a tree-sitter Tree
pub fn get_action_list_tree(actions: &str) -> Result<Tree, String> {
    parse_tree(actions)
}

/// Build a [`WorkspaceConfig`] from a tag hierarchy map.
///
/// Maps the semantic fields core understands for graph operations.
/// Expansion config (`expansion_total_instances`, the projection window) is not
/// passed here — it only affects occurrence projection, not graph queries.
pub fn workspace_config_from(
    tag_hierarchies: &std::collections::HashMap<String, Vec<String>>,
) -> WorkspaceConfig {
    WorkspaceConfig {
        tag_hierarchies: tag_hierarchies.clone(),
        ..WorkspaceConfig::default()
    }
}
