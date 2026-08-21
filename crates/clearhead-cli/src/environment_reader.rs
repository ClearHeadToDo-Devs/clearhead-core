use config::ConfigError;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::PathBuf;

// Path resolution and the config source/precedence stack now live in core so
// every tool (CLI, LSP, graphd) resolves configuration identically. Re-exported
// here so existing `clearhead_cli::environment_reader::*` call sites are
// unchanged.
pub use clearhead_core::config::loader::{
    ensure_dir_exists, find_project_data_dir, get_config_dir, get_data_dir, resolve_config_path,
    resolve_file_path, resolve_workspace_paths,
};

/// Configuration loaded from file and environment variables
/// Uses flat structure with cli_ prefix for implementation-specific settings
#[derive(Debug, Deserialize, Clone)]
pub struct Config {
    // Core settings (no prefix)
    #[serde(default = "default_data_dir")]
    pub data_dir: String,

    #[serde(default = "default_config_dir")]
    pub config_dir: String,

    #[serde(default = "default_file")]
    pub default_file: String,

    // Workspace identity (workspace_id, workspace_name, created_at) is NOT config —
    // it does not layer through the precedence chain. It lives in the workspace
    // manifest (.clearhead/workspace.json) and is read by core from the workspace
    // itself. See clearhead_core::workspace::manifest::WorkspaceManifest.

    // Additional workspace directories to include in multi-workspace queries.
    // Each entry may be an absolute path, a path starting with `~/` (expanded to
    // the user's home directory), a path with `$VAR` / `${VAR}` environment
    // variable references, or a path relative to the directory that contains
    // the config.json that declares it.
    // Resolved at `CommandContext` construction time via `resolve_workspace_paths`.
    #[serde(default)]
    pub additional_workspaces: Vec<String>,

    // Bypass project detection entirely and operate on the user workspace
    // only (specifications/configuration.md, Workspace Resolution). The one
    // sanctioned way to ignore an enclosing project.
    #[serde(default)]
    pub default_to_user_scope: bool,

    // Tag hierarchies for implicit inheritance
    // Maps parent tag -> list of child tags
    #[serde(default)]
    pub tag_hierarchies: HashMap<String, Vec<String>>,

    // Expansion: total instances generated per schedule across both files
    #[serde(default = "default_expansion_total_instances")]
    pub expansion_total_instances: u32,

    // Configured plans vdir, laid out as <plan_path>/<charter>/<uid>.ics.
    // Transport and sharing are external. When None, plans live under
    // <data_root>/plans.
    #[serde(default)]
    pub plan_path: Option<String>,

    // CLI-specific settings (cli_ prefix)
    #[serde(default = "default_format")]
    pub cli_format: String,

    #[serde(default = "default_indent_style")]
    pub cli_indent_style: String,

    #[serde(default = "default_indent_width")]
    pub cli_indent_width: usize,
}

// Default functions
// Empty string means "use XDG defaults"
fn default_data_dir() -> String {
    String::new()
}

fn default_config_dir() -> String {
    String::new()
}

fn default_file() -> String {
    "inbox.actions".to_string()
}

fn default_format() -> String {
    "actions".to_string()
}

fn default_indent_style() -> String {
    "spaces".to_string()
}

fn default_indent_width() -> usize {
    4
}

fn default_expansion_total_instances() -> u32 {
    2
}

/// Load configuration with the shared precedence chain (defaults → global →
/// project → project.local → env), deserialized into the CLI's [`Config`],
/// which extends the shared semantic fields with `cli_*` settings.
///
/// The source/precedence stack lives in
/// [`clearhead_core::config::loader::config_sources`]; per-field defaults come
/// from this struct's serde `default` attributes.
pub fn load_config(custom_config_path: Option<PathBuf>) -> Result<Config, ConfigError> {
    clearhead_core::config::loader::config_sources(custom_config_path)
        .build()?
        .try_deserialize()
}
