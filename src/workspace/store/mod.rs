//! Workspace storage — loading and saving domain models on disk.
//!
//! # Workspace Layouts
//!
//! ClearHead supports two workspace layouts. See the
//! [naming conventions specification](../../../specifications/naming_conventions.md)
//! for the authoritative description of each scope.
//!
//! ## Project layout
//! The workspace root contains a `.clearhead/` subdirectory. Action files live
//! inside it, and the root directory name becomes the top-level charter — so
//! `my-project/.clearhead/next.actions` belongs to the `my-project` charter.
//! Used for project-local work tracked alongside source code.
//!
//! ## User layout
//! No `.clearhead/` subdirectory. Action files live directly in the root and
//! charter names are inferred purely from filenames and directory structure.
//! Used for personal workspaces not tied to a specific project.
//!
//! `resolve_workspace_layout` detects which layout applies and returns a
//! `WorkspaceLayout` that all load/save functions use for path resolution.

mod discovery;
mod doctor;
mod findings;
pub mod load;
mod manifest;
mod pathing;
mod save;

use std::path::{Path, PathBuf};

pub use doctor::{Diagnosis, diagnose, diagnose_read};
pub use findings::{Finding, FindingSeverity};
pub use load::{
    Workspace, WorkspaceRead, load_domain_model, load_domain_model_with_plans, load_workspace,
    load_workspace_with_plans, load_workspaces, read_workspace, read_workspace_with_plans,
};
pub use manifest::{ManifestSourceType, WorkspaceManifestEntry, collect_workspace_manifest};
pub use pathing::{infer_charter_name, infer_parent_charter_name};
pub use save::save_domain_model;

/// Returns the workspace root directory (`.clearhead/` for project layout).
///
/// Use this for non-charter files: `archive/`, `objectives/`, `templates/`.
pub fn workspace_data_root(root: &Path) -> PathBuf {
    resolve_workspace_layout(root).data_root
}

/// Returns the charter tree root (`<data_root>/charters/`).
///
/// All `.actions` and `.md` charter files live here. Use this for charter/action path resolution.
pub fn charter_root(root: &Path) -> PathBuf {
    resolve_workspace_layout(root).charter_root
}

/// Returns the plans root (`<data_root>/plans/`).
///
/// All vdir `.ics` plan files live here. Each charter gets one subdirectory.
pub fn plans_root(root: &Path) -> PathBuf {
    resolve_workspace_layout(root).plans_root
}

/// Returns absolute paths to all `.actions` files in the workspace.
pub fn list_action_files(root: &Path) -> Result<Vec<PathBuf>, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    discovery::discover_action_files(&layout.charter_root)
}

/// Errors that can occur when interacting with a workspace.
#[derive(thiserror::Error, Debug)]
pub enum WorkspaceError {
    /// An underlying I/O error occurred.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    /// Error parsing a `.actions` file.
    #[error("Parse error: {0}")]
    Parse(String),
    /// Error loading or saving sidecar actions.
    #[error("Actions error: {0}")]
    Actions(String),
    /// A path provided was not within the workspace or was otherwise invalid.
    #[error("Invalid path: {0}")]
    InvalidPath(PathBuf),
}

#[derive(Debug, Clone)]
pub(crate) struct WorkspaceLayout {
    /// `.clearhead/` (or workspace root for user layout) — for non-charter files.
    pub(crate) data_root: PathBuf,
    /// `<data_root>/charters/` — where the charter tree lives.
    pub(crate) charter_root: PathBuf,
    /// `<data_root>/plans/` — where vdir plan files live (parallel to charters/).
    pub(crate) plans_root: PathBuf,
    pub(crate) project_root_charter: Option<String>,
}

/// Detect the workspace layout and return the paths needed for load/save.
///
/// - **Project layout** (`.clearhead/` exists): data root is `.clearhead/`,
///   and the root directory name becomes `project_root_charter` so that
///   `next.actions` at the top level maps to the project charter rather than "next".
/// - **User layout** (no `.clearhead/`): data root is the directory itself,
///   `project_root_charter` is `None`, and all charter names come from filenames.
pub(crate) fn resolve_workspace_layout(root: &Path) -> WorkspaceLayout {
    let project_data = root.join(".clearhead");
    if project_data.is_dir() {
        return WorkspaceLayout {
            charter_root: project_data.join("charters"),
            plans_root: project_data.join("plans"),
            data_root: project_data,
            project_root_charter: root
                .file_name()
                .and_then(|name| name.to_str())
                .map(ToString::to_string),
        };
    }

    WorkspaceLayout {
        charter_root: root.join("charters"),
        plans_root: root.join("plans"),
        data_root: root.to_path_buf(),
        project_root_charter: None,
    }
}
