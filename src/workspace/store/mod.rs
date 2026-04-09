//! Workspace storage — loading and saving domain models on disk.

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
