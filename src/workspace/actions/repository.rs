use super::parser::{Action, ActionList};
use std::path::{Path, PathBuf};

/// Metadata about an action's source file
#[derive(Debug, Clone)]
pub struct ActionSource {
    /// Path to the source file (relative to workspace root)
    pub file_path: PathBuf,
    /// Inferred project name from file/directory structure
    pub project: Option<String>,
}

/// An action with its source metadata
#[derive(Debug, Clone)]
pub struct SourcedAction {
    pub action: Action,
    pub source: ActionSource,
}

/// Collection of actions from multiple files with source tracking.
///
/// This is a lightweight container for aggregating actions from across
/// a workspace, preserving their origin for cross-file queries.
#[derive(Debug, Default, Clone)]
pub struct ActionRepository {
    /// All actions with their source metadata
    pub sourced_actions: Vec<SourcedAction>,
}

impl ActionRepository {
    /// Create a new empty repository
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a flat list of all actions (without source metadata)
    pub fn to_action_list(&self) -> ActionList {
        self.sourced_actions
            .iter()
            .map(|sa| sa.action.clone())
            .collect()
    }

    /// Add actions from a file
    pub fn add_from_file(&mut self, file_path: &Path, actions: ActionList, workspace_root: &Path) {
        let relative_path = file_path
            .strip_prefix(workspace_root)
            .unwrap_or(file_path)
            .to_path_buf();

        let project = infer_project_name(&relative_path);

        for action in actions {
            self.sourced_actions.push(SourcedAction {
                action,
                source: ActionSource {
                    file_path: relative_path.clone(),
                    project: project.clone(),
                },
            });
        }
    }

    /// Get the number of actions
    pub fn len(&self) -> usize {
        self.sourced_actions.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.sourced_actions.is_empty()
    }
}

/// Infer project name from file path
///
/// Rules:
/// - `project.actions` -> project
/// - `project/next.actions` -> project
/// - `project/logs/2026-01.actions` -> project
/// - `inbox.actions` -> None (not a project)
pub fn infer_project_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();

    if components.is_empty() {
        return None;
    }

    // Single file at root: check if it's a project file
    if components.len() == 1 {
        let filename = relative_path.file_stem()?.to_str()?;
        // inbox is special - not a project
        if filename == "inbox" {
            return None;
        }
        return Some(filename.to_string());
    }

    // Nested file: first directory is the project
    let first = components.first()?;
    if let std::path::Component::Normal(name) = first {
        return name.to_str().map(String::from);
    }

    None
}
