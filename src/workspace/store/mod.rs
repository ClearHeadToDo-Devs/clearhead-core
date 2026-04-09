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
mod load;
mod pathing;
mod save;

use std::fmt;
use std::path::{Path, PathBuf};

pub use load::load_domain_model;
pub use pathing::{
    infer_charter_name, infer_charter_name_for_workspace, infer_parent_charter_name,
    infer_parent_charter_name_for_workspace,
};
pub use save::save_domain_model;

/// Errors that can occur when interacting with a workspace.
#[derive(Debug)]
pub enum WorkspaceError {
    Io(std::io::Error),
    Parse(String),
    Acts(String),
    InvalidPath(PathBuf),
}

impl From<std::io::Error> for WorkspaceError {
    fn from(e: std::io::Error) -> Self {
        WorkspaceError::Io(e)
    }
}

impl fmt::Display for WorkspaceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WorkspaceError::Io(e) => write!(f, "IO error: {}", e),
            WorkspaceError::Parse(msg) => write!(f, "Parse error: {}", msg),
            WorkspaceError::Acts(msg) => write!(f, "Acts error: {}", msg),
            WorkspaceError::InvalidPath(p) => write!(f, "Invalid path: {}", p.display()),
        }
    }
}

impl std::error::Error for WorkspaceError {}

#[derive(Debug, Clone)]
pub(crate) struct WorkspaceLayout {
    pub(crate) data_root: PathBuf,
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
            data_root: project_data,
            project_root_charter: root
                .file_name()
                .and_then(|name| name.to_str())
                .map(ToString::to_string),
        };
    }

    WorkspaceLayout {
        data_root: root.to_path_buf(),
        project_root_charter: None,
    }
}
