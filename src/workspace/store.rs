//! Workspace storage — loading domain models from `.actions` files on disk.

use crate::domain::DomainModel;
use crate::workspace::actions::convert::from_actions_with_charter;
use std::fmt;
use std::path::{Path, PathBuf};

// ============================================================================
// Core types
// ============================================================================

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

/// The main interface for loading and saving domain models in a workspace.
/// iterates through all .actions files,
/// iterates through all charters from the .md files
/// iterates through all acts through the ttl files
/// pulls all of this out as a domain model for further effort
pub fn load_domain_model(root: &PathBuf) -> Result<DomainModel, WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.clone()));
    } else {
        let mut domain_model = DomainModel::new();
        let files = discover_action_files(root)
            .map_err(|e| WorkspaceError::Io(std::io::Error::new(std::io::ErrorKind::Other, e)))?;

        for file_path in files {
            let relative = file_path
                .strip_prefix(root)
                .unwrap_or(&file_path)
                .to_string_lossy()
                .to_string();

            let charter_name = infer_project_name(Path::new(&relative)).unwrap();
            let mut actions = super::parse_actions(&std::fs::read_to_string(&file_path)?)
                .map_err(|e| WorkspaceError::Parse(e))?;

            for action in &mut actions {
                if action.charter.is_none() {
                    action.charter = Some(charter_name.clone());
                }
            }
            let charter = from_actions_with_charter(&actions, charter_name);
            domain_model.charters.push(charter);
        }
        return Ok(domain_model);
    }
}

/// Strip archive suffixes (`.completed`, `.archived`) from a file stem.
///
/// `health.completed` → `"health"`, `health` → `"health"`.
pub fn strip_archive_suffix(stem: &str) -> &str {
    stem.strip_suffix(".completed")
        .or_else(|| stem.strip_suffix(".archived"))
        .unwrap_or(stem)
}

/// Infer a human-readable project name from a relative file path.
///
/// Rules:
/// - `project.actions` → "project"
/// - `project.completed.actions` → "project"  (archive convention)
/// - `project/next.actions` → "project"  (primary file for that charter)
/// - `project/subcharter.actions` → "subcharter"  (sub-charter)
/// - `project/subdir/next.actions` → "subdir"
/// - `project/logs/2026-01.actions` → "2026-01"
pub fn infer_project_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();

    if components.is_empty() {
        return None;
    }

    let filename = relative_path.file_name()?.to_str()?;

    if components.len() == 1 {
        let stem = relative_path.file_stem()?.to_str()?;
        let base = strip_archive_suffix(stem);
        return Some(base.to_string());
    }

    // Nested: `next.actions` means the parent dir IS the charter name.
    // Anything else means the file stem IS the charter name (sub-charter).
    if filename == "next.actions" {
        if let std::path::Component::Normal(name) = components[components.len() - 2] {
            return name.to_str().map(String::from);
        }
        return None;
    }

    let stem = relative_path.file_stem()?.to_str()?;
    Some(strip_archive_suffix(stem).to_string())
}

/// Infer the parent charter name from a file path.
///
/// Returns `None` if the file's charter has no structural parent.
///
/// - `project.actions` → None
/// - `project/next.actions` → None  (top-level directory charter)
/// - `project/subcharter.actions` → Some("project")
/// - `project/subdir/next.actions` → Some("project")
/// - `project/subdir/sub.actions` → Some("subdir")
pub fn infer_parent_charter_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();
    let filename = relative_path.file_name()?.to_str()?;

    if components.len() <= 1 {
        return None;
    }

    if filename == "next.actions" {
        // project/next.actions → charter "project", no parent
        // project/subdir/next.actions → charter "subdir", parent "project"
        if components.len() == 2 {
            return None;
        }
        if let std::path::Component::Normal(name) = components[components.len() - 3] {
            return name.to_str().map(String::from);
        }
    } else {
        // project/subcharter.actions → parent "project"
        if let std::path::Component::Normal(name) = components[components.len() - 2] {
            return name.to_str().map(String::from);
        }
    }
    None
}

/// Discover all `.actions` files recursively, skipping hidden directories.
fn discover_action_files(dir: &Path) -> Result<Vec<PathBuf>, WorkspaceError> {
    let mut files = Vec::new();
    discover_recursive(dir, "actions".to_string(), &mut files)?;
    files.sort();
    Ok(files)
}

fn discover_recursive(
    dir: &Path,
    ext: String,
    files: &mut Vec<PathBuf>,
) -> Result<(), WorkspaceError> {
    if !dir.is_dir() {
        return Ok(());
    }

    let entries = std::fs::read_dir(dir).map_err(|e| WorkspaceError::Io(e))?;

    for entry in entries {
        let entry = entry.map_err(|e| WorkspaceError::Io(e))?;
        let path = entry.path();

        if path.is_dir() {
            if let Some(name) = path.file_name() {
                if name.to_string_lossy().starts_with('.') {
                    continue;
                }
            }
            discover_recursive(&path, ext.clone(), files)?;
        } else if path.is_file() {
            if let Some(file_ext) = path.extension() {
                if *file_ext == *ext {
                    files.push(path);
                }
            }
        }
    }

    Ok(())
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_infer_project_name_rules() {
        assert_eq!(
            infer_project_name(Path::new("work.actions")),
            Some("work".to_string())
        );
        assert_eq!(
            infer_project_name(Path::new("inbox.actions")),
            Some("inbox".to_string())
        );
        assert_eq!(
            infer_project_name(Path::new("myproject/next.actions")),
            Some("myproject".to_string())
        );
        // sub-charter: file stem is the charter name
        assert_eq!(
            infer_project_name(Path::new("myproject/subcharter.actions")),
            Some("subcharter".to_string())
        );
        // nested next.actions: parent dir is the charter
        assert_eq!(
            infer_project_name(Path::new("myproject/subdir/next.actions")),
            Some("subdir".to_string())
        );
        // nested non-next: file stem is the charter
        assert_eq!(
            infer_project_name(Path::new("myproject/logs/2026-01.actions")),
            Some("2026-01".to_string())
        );
    }

    #[test]
    fn test_infer_parent_charter_name_rules() {
        assert_eq!(infer_parent_charter_name(Path::new("work.actions")), None);
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/next.actions")),
            None
        );
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/subcharter.actions")),
            Some("myproject".to_string())
        );
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/subdir/next.actions")),
            Some("myproject".to_string())
        );
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/subdir/sub.actions")),
            Some("subdir".to_string())
        );
    }
}
