//! Configuration loading: the shared source-and-precedence stack.
//!
//! This is the drift-prone part every tool needs *identically* — where config
//! files live and in what order they override each other. Core owns it so the
//! CLI, LSP, and graphd all resolve configuration the same way.
//!
//! Each tool deserializes its **own** struct from these sources and extends with
//! tool-specific fields (the CLI adds `cli_*`, etc.); core defines the shared
//! semantic fields in [`WorkspaceConfig`](super::WorkspaceConfig). Field
//! defaults come from each struct's serde `default` attributes, so this module
//! only assembles the source layering.

use config::builder::DefaultState;
use config::{ConfigBuilder, Environment, File, FileFormat};
use dirs::{config_dir, data_dir};
use std::path::{Path, PathBuf};

/// XDG config directory for clearhead (`~/.config/clearhead`).
pub fn get_config_dir() -> PathBuf {
    config_dir()
        .expect("Failed to determine config directory")
        .join("clearhead")
}

/// XDG data directory for clearhead (`~/.local/share/clearhead`).
pub fn get_data_dir() -> PathBuf {
    data_dir()
        .expect("Failed to determine data directory")
        .join("clearhead")
}

/// Walk up from the current directory looking for a `.clearhead/` directory.
/// Returns the first ancestor that contains one, or `None`.
pub fn find_project_data_dir() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok()?;
    loop {
        if dir.join(".clearhead").is_dir() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Assemble the layered config source stack (lowest → highest precedence):
/// global config → project config → project-local config → `CLEARHEAD_` env.
///
/// Callers finish with `.build()?.try_deserialize::<TheirStruct>()`; per-field
/// defaults come from the struct's serde `default` attributes.
///
/// - **global**: `custom_config_path`, else `~/.config/clearhead/config.json`.
/// - **project**: `<project-root>/.clearhead/config.json` — committed shared
///   team settings.
/// - **project-local**: `<project-root>/.clearhead/config.local.json` — a
///   git-ignored personal override that wins over the committed project config.
///
/// Workspace *identity* is not config; it lives in the sibling `workspace.json`
/// manifest and is read from the workspace itself.
pub fn config_sources(custom_config_path: Option<PathBuf>) -> ConfigBuilder<DefaultState> {
    let global_config_path =
        custom_config_path.unwrap_or_else(|| get_config_dir().join("config.json"));

    let project_root = find_project_data_dir();
    let project_config_path = project_root
        .as_ref()
        .map(|root| root.join(".clearhead").join("config.json"));
    let project_local_config_path = project_root
        .as_ref()
        .map(|root| root.join(".clearhead").join("config.local.json"));

    let mut builder = ConfigBuilder::<DefaultState>::default().add_source(
        File::from(global_config_path)
            .format(FileFormat::Json)
            .required(false),
    );

    if let Some(project_cfg) = project_config_path {
        builder = builder.add_source(
            File::from(project_cfg)
                .format(FileFormat::Json)
                .required(false),
        );
    }

    if let Some(project_local_cfg) = project_local_config_path {
        builder = builder.add_source(
            File::from(project_local_cfg)
                .format(FileFormat::Json)
                .required(false),
        );
    }

    builder.add_source(
        Environment::with_prefix("CLEARHEAD")
            .prefix_separator("_")
            .separator("__")
            .try_parsing(true),
    )
}

/// Resolve the config file path that would be loaded for this invocation.
pub fn resolve_config_path(custom_config_path: Option<PathBuf>) -> PathBuf {
    custom_config_path.unwrap_or_else(|| get_config_dir().join("config.json"))
}

/// Expand `~/` and `$VAR` / `${VAR}` references in a path string.
fn expand_path(path: &str) -> PathBuf {
    if let Some(relative) = path.strip_prefix("~/")
        && let Some(home) = dirs::home_dir()
    {
        return home.join(relative);
    }

    let expanded = shellexpand::env(path).unwrap_or_else(|_| path.into());
    PathBuf::from(expanded.as_ref())
}

/// Resolve a list of `additional_workspaces` path strings to absolute paths.
///
/// Each entry is expanded (`~/`, `$VAR`) then, if still relative, joined onto
/// `base` — the directory containing the `config.json` that declared it.
/// Entries whose resolved path does not exist are **included**; callers decide
/// how to handle missing paths so errors surface at the point of use.
pub fn resolve_workspace_paths(paths: &[String], base: &Path) -> Vec<PathBuf> {
    paths
        .iter()
        .map(|p| {
            let expanded = expand_path(p);
            if expanded.is_absolute() {
                expanded
            } else {
                base.join(expanded)
            }
        })
        .collect()
}

/// Ensure a directory exists, creating it (and parents) if necessary.
pub fn ensure_dir_exists(path: &Path) -> std::io::Result<()> {
    if !path.exists() {
        std::fs::create_dir_all(path)?;
    }
    Ok(())
}

/// Resolve a file path, handling `~/` expansion and relative-vs-absolute.
/// An empty `file` returns the `fallback` directory.
pub fn resolve_file_path(file: &str, fallback: &Path) -> PathBuf {
    if file.is_empty() {
        return fallback.to_path_buf();
    }

    let expanded = expand_path(file);

    if expanded.is_absolute() {
        expanded
    } else {
        fallback.join(expanded)
    }
}
