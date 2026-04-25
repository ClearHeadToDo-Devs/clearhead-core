//! Workspace manifest — a human-readable summary of what the load pipeline sees.
//!
//! Useful for debugging load order, understanding inferred charter names and parents,
//! and verifying that the workspace layout is being read as intended.

use super::discovery::{discover_action_files, discover_charter_files};
use super::pathing::{infer_charter_name_for_workspace, infer_parent_charter_name_for_workspace};
use super::{WorkspaceError, resolve_workspace_layout};
use crate::workspace::plans::collect_plan_files;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// A single entry in the workspace load manifest.
///
/// Each entry corresponds to one file discovered during workspace loading and
/// captures the path, inferred charter name, inferred parent, and source type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WorkspaceManifestEntry {
    /// Relative path from the workspace data root.
    pub path: String,
    /// Charter name inferred from this file's path.
    pub charter_name: String,
    /// Parent charter name inferred from directory structure (if any).
    pub inferred_parent: Option<String>,
    /// Whether this entry came from `.actions`, `.md`, or both.
    pub source_type: ManifestSourceType,
}

/// The file types that contributed to a charter's manifest entry.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ManifestSourceType {
    Actions,
    Markdown,
    Ics,
    ActionsPlusMarkdown,
    ActionsPlusIcs,
    MarkdownPlusIcs,
    ActionsPlusMarkdownPlusIcs,
}

/// Collect the workspace load manifest for a root directory.
///
/// Returns one entry per discovered file, sorted by path. Pairs of
/// `.actions` + `.md` files for the same charter are merged into a single
/// `ActionsPlusMarkdown` entry so callers can see the combined picture.
pub fn collect_workspace_manifest(
    root: &Path,
) -> Result<Vec<WorkspaceManifestEntry>, WorkspaceError> {
    let layout = resolve_workspace_layout(root);

    let action_files = discover_action_files(&layout.data_root)?;
    let charter_files = discover_charter_files(&layout.data_root)?;
    let plan_files = collect_plan_files(root)?;

    let mut entries_by_charter: HashMap<String, WorkspaceManifestEntry> = HashMap::new();

    // Helper to merge source types
    fn merge_sources(
        current: &ManifestSourceType,
        new_source: ManifestSourceType,
    ) -> ManifestSourceType {
        match (current, new_source) {
            (ManifestSourceType::Actions, ManifestSourceType::Markdown)
            | (ManifestSourceType::Markdown, ManifestSourceType::Actions) => {
                ManifestSourceType::ActionsPlusMarkdown
            }
            (ManifestSourceType::Actions, ManifestSourceType::Ics)
            | (ManifestSourceType::Ics, ManifestSourceType::Actions) => {
                ManifestSourceType::ActionsPlusIcs
            }
            (ManifestSourceType::Markdown, ManifestSourceType::Ics)
            | (ManifestSourceType::Ics, ManifestSourceType::Markdown) => {
                ManifestSourceType::MarkdownPlusIcs
            }
            (ManifestSourceType::ActionsPlusMarkdown, ManifestSourceType::Ics)
            | (ManifestSourceType::Ics, ManifestSourceType::ActionsPlusMarkdown)
            | (ManifestSourceType::ActionsPlusIcs, ManifestSourceType::Markdown)
            | (ManifestSourceType::MarkdownPlusIcs, ManifestSourceType::Actions) => {
                ManifestSourceType::ActionsPlusMarkdownPlusIcs
            }
            _ => current.clone(), // fallback or same
        }
    }

    for file_path in action_files {
        let relative = file_path
            .strip_prefix(&layout.data_root)
            .unwrap_or(&file_path);
        let charter_name =
            infer_charter_name_for_workspace(relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;
        let inferred_parent = infer_parent_charter_name_for_workspace(
            relative,
            layout.project_root_charter.as_deref(),
        );

        entries_by_charter.insert(
            charter_name.clone(),
            WorkspaceManifestEntry {
                path: relative.to_string_lossy().into_owned(),
                charter_name,
                inferred_parent,
                source_type: ManifestSourceType::Actions,
            },
        );
    }

    for file_path in charter_files {
        let relative = file_path
            .strip_prefix(&layout.data_root)
            .unwrap_or(&file_path);
        let charter_name =
            infer_charter_name_for_workspace(relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;
        let inferred_parent = infer_parent_charter_name_for_workspace(
            relative,
            layout.project_root_charter.as_deref(),
        );

        if let Some(entry) = entries_by_charter.get_mut(&charter_name) {
            entry.source_type = merge_sources(&entry.source_type, ManifestSourceType::Markdown);
        } else {
            entries_by_charter.insert(
                charter_name.clone(),
                WorkspaceManifestEntry {
                    path: relative.to_string_lossy().into_owned(),
                    charter_name,
                    inferred_parent,
                    source_type: ManifestSourceType::Markdown,
                },
            );
        }
    }

    for entry in plan_files {
        let charter_name = entry.charter_name.clone();
        if let Some(existing) = entries_by_charter.get_mut(&charter_name) {
            existing.source_type = merge_sources(&existing.source_type, ManifestSourceType::Ics);
        } else {
            entries_by_charter.insert(
                charter_name.clone(),
                WorkspaceManifestEntry {
                    path: entry.relative_path.to_string_lossy().into_owned(),
                    charter_name,
                    inferred_parent: entry.inferred_parent,
                    source_type: ManifestSourceType::Ics,
                },
            );
        }
    }

    let mut entries: Vec<WorkspaceManifestEntry> = entries_by_charter.into_values().collect();
    entries.sort_by(|a, b| a.path.cmp(&b.path));
    Ok(entries)
}
