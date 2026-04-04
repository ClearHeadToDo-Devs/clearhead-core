//! Workspace storage — loading domain models from `.actions` files on disk.

use crate::domain::DomainModel;
use crate::workspace::actions::convert::from_actions_with_charter;
use crate::workspace::charter::parse_charter;
use std::collections::HashMap;
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

/// Load a `DomainModel` from a workspace root directory.
///
/// Two-pass process:
/// 1. `.actions` files → implicit charters (deterministic ID from name, plans attached)
/// 2. `.md` files → explicit charters (rich metadata); merged over matching implicit charters
///
/// After merging, structural parent relationships are inferred from the file
/// hierarchy for any charter that doesn't have an explicit `parent` field.
pub fn load_domain_model(root: &PathBuf) -> Result<DomainModel, WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.clone()));
    }

    // charter name → Charter (insertion-ordered for deterministic output)
    let mut charters: HashMap<String, crate::domain::Charter> = HashMap::new();
    // charter name → relative path (for parent inference after merging)
    let mut path_for_name: HashMap<String, PathBuf> = HashMap::new();

    // Pass 1: .actions files → implicit charters with plans
    for file_path in discover_action_files(root)? {
        let relative = file_path
            .strip_prefix(root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name = infer_charter_name(&relative).unwrap();

        let mut actions = super::parse_actions(&std::fs::read_to_string(&file_path)?)
            .map_err(WorkspaceError::Parse)?;

        for action in &mut actions {
            if action.charter.is_none() {
                action.charter = Some(name.clone());
            }
        }

        let charter = from_actions_with_charter(&actions, name.clone());
        charters
            .entry(name.clone())
            .and_modify(|c| c.plans.extend(charter.plans.clone()))
            .or_insert(charter);
        path_for_name.entry(name).or_insert(relative);
    }

    // Pass 2: .md files → enrich charters with explicit metadata
    for file_path in discover_charter_files(root)? {
        let relative = file_path
            .strip_prefix(root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name = infer_charter_name(&relative).unwrap();

        let explicit =
            parse_charter(&std::fs::read_to_string(&file_path)?).map_err(WorkspaceError::Parse)?;

        charters
            .entry(name)
            .and_modify(|implicit| {
                // .md wins on all metadata; plans stay from .actions
                implicit.id = explicit.id;
                implicit.title = explicit.title.clone();
                implicit.description = explicit.description.clone();
                if explicit.alias.is_some() {
                    implicit.alias = explicit.alias.clone();
                }
                if explicit.parent.is_some() {
                    implicit.parent = explicit.parent.clone();
                }
                if explicit.objectives.is_some() {
                    implicit.objectives = explicit.objectives.clone();
                }
            })
            .or_insert(explicit);
    }

    // Pass 3: fill structural parents for charters without an explicit parent
    let parent_hints: Vec<(String, String)> = path_for_name
        .iter()
        .filter_map(|(name, path)| {
            infer_parent_charter_name(path).map(|parent| (name.clone(), parent))
        })
        .collect();

    for (name, parent) in parent_hints {
        if let Some(charter) = charters.get_mut(&name) {
            if charter.parent.is_none() {
                charter.parent = Some(parent);
            }
        }
    }

    Ok(DomainModel {
        objectives: vec![],
        charters: charters.into_values().collect(),
    })
}

/// Strip archive suffixes (`.completed`, `.archived`) from a file stem.
///
/// `health.completed` → `"health"`, `health` → `"health"`.
pub fn strip_archive_suffix(stem: &str) -> &str {
    stem.strip_suffix(".completed")
        .or_else(|| stem.strip_suffix(".archived"))
        .unwrap_or(stem)
}

/// Returns true for filenames where the parent directory is the charter name.
///
/// Both `next.actions` and `README.md` are "primary" files — they represent
/// the charter itself, not a sub-charter.
fn is_primary_filename(filename: &str) -> bool {
    filename == "next.actions" || filename == "README.md"
}

/// Infer the charter name from a relative file path.
///
/// Rules:
/// - `project.actions` → "project"
/// - `project.md` → "project"
/// - `project.completed.actions` → "project"  (archive convention)
/// - `project/next.actions` → "project"  (primary actions file)
/// - `project/README.md` → "project"  (primary charter document)
/// - `project/subcharter.actions` → "subcharter"
/// - `project/subcharter.md` → "subcharter"
/// - `project/subdir/next.actions` → "subdir"
/// - `project/logs/2026-01.actions` → "2026-01"
pub fn infer_charter_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();

    if components.is_empty() {
        return None;
    }

    let filename = relative_path.file_name()?.to_str()?;

    if components.len() == 1 {
        let stem = relative_path.file_stem()?.to_str()?;
        return Some(strip_archive_suffix(stem).to_string());
    }

    // Primary filenames: parent directory is the charter name.
    if is_primary_filename(filename) {
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
/// - `project/README.md` → None
/// - `project/subcharter.actions` → Some("project")
/// - `project/subcharter.md` → Some("project")
/// - `project/subdir/next.actions` → Some("project")
/// - `project/subdir/sub.actions` → Some("subdir")
pub fn infer_parent_charter_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();
    let filename = relative_path.file_name()?.to_str()?;

    if components.len() <= 1 {
        return None;
    }

    if is_primary_filename(filename) {
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
    discover_recursive(dir, "actions", &mut files)?;
    files.sort();
    Ok(files)
}

/// Discover all `.md` files recursively, skipping hidden directories.
fn discover_charter_files(dir: &Path) -> Result<Vec<PathBuf>, WorkspaceError> {
    let mut files = Vec::new();
    discover_recursive(dir, "md", &mut files)?;
    files.sort();
    Ok(files)
}

fn discover_recursive(
    dir: &Path,
    ext: &str,
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
            discover_recursive(&path, ext, files)?;
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
    fn test_infer_charter_name() {
        assert_eq!(
            infer_charter_name(Path::new("work.actions")),
            Some("work".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("inbox.actions")),
            Some("inbox".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("health.md")),
            Some("health".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/next.actions")),
            Some("myproject".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/README.md")),
            Some("myproject".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/subcharter.actions")),
            Some("subcharter".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/subcharter.md")),
            Some("subcharter".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/subdir/next.actions")),
            Some("subdir".to_string())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/logs/2026-01.actions")),
            Some("2026-01".to_string())
        );
    }

    #[test]
    fn test_infer_parent_charter_name() {
        assert_eq!(infer_parent_charter_name(Path::new("work.actions")), None);
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/next.actions")),
            None
        );
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/README.md")),
            None
        );
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/subcharter.actions")),
            Some("myproject".to_string())
        );
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/subcharter.md")),
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
