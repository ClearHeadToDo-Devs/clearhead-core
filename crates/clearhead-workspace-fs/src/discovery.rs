//! Native filesystem discovery — walking the workspace tree to enumerate the
//! `.actions`, `.md`, and `.ics` resources that Core assembles from supplied
//! bytes. This is host I/O: it lives in the adapter, not in the pure core.
//!
//! The workspace *manifest* built here is a human-readable summary of what a
//! load would see (inferred charter names, parents, and source types), used by
//! `clearhead debug`. Charter-name and parent inference remain pure Core policy
//! ([`infer_charter_name_for_workspace`] et al.); only the directory walk and
//! the manifest assembly are native.

use clearhead_core::workspace::calendar::plans::{
    infer_plan_charter_name_for_workspace, infer_plan_parent_for_workspace,
};
use clearhead_core::workspace::store::{
    WorkspaceError, charter_root, infer_charter_name_for_workspace,
    infer_parent_charter_name_for_workspace, plans_root, project_root_charter,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PlanFileEntry {
    pub path: PathBuf,
    pub relative_path: PathBuf,
    pub charter_name: String,
    pub inferred_parent: Option<String>,
}

/// Discover all `.actions` files recursively, skipping hidden directories.
pub(crate) fn discover_action_files(dir: &Path) -> Result<Vec<PathBuf>, WorkspaceError> {
    let mut files = Vec::new();
    discover_recursive(dir, "actions", &mut files)?;
    // Exclude `.completed.actions` (closed-action archives) and `.upcoming.actions`
    // (a retired materialization artifact — recurring occurrences are now projected
    // on read, never filed; any lingering legacy file must not shadow projections).
    files.retain(|p| {
        p.file_name()
            .and_then(|n| n.to_str())
            .map(|n| !n.ends_with(".completed.actions") && !n.ends_with(".upcoming.actions"))
            .unwrap_or(true)
    });
    files.sort();
    Ok(files)
}

/// Discover and classify legacy native `.ics` resources.
pub(crate) fn discover_plan_files(
    plans_root: &Path,
    project_root_charter: Option<&str>,
) -> Result<Vec<PlanFileEntry>, WorkspaceError> {
    let mut files = Vec::new();
    discover_recursive(plans_root, "ics", &mut files)?;
    let mut entries = Vec::new();
    for path in files {
        let relative_path = path
            .strip_prefix(plans_root)
            .map_err(|_| WorkspaceError::InvalidPath(path.clone()))?
            .to_path_buf();
        let Some(charter_name) =
            infer_plan_charter_name_for_workspace(&relative_path, project_root_charter)
        else {
            continue;
        };
        entries.push(PlanFileEntry {
            path,
            inferred_parent: infer_plan_parent_for_workspace(&relative_path, project_root_charter),
            relative_path,
            charter_name,
        });
    }
    entries.sort_by(|left, right| left.relative_path.cmp(&right.relative_path));
    Ok(entries)
}

/// Discover all `.md` files recursively, skipping hidden directories.
pub(crate) fn discover_charter_files(dir: &Path) -> Result<Vec<PathBuf>, WorkspaceError> {
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

    let entries = std::fs::read_dir(dir).map_err(WorkspaceError::Io)?;
    for entry in entries {
        let path = entry.map_err(WorkspaceError::Io)?.path();

        if path.is_dir() {
            if let Some(name) = path.file_name()
                && name.to_string_lossy().starts_with('.')
            {
                continue;
            }
            discover_recursive(&path, ext, files)?;
            continue;
        }

        if path.is_file()
            && let Some(file_ext) = path.extension()
            && *file_ext == *ext
        {
            files.push(path);
        }
    }

    Ok(())
}

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
    /// Only a `.actions` file was found.
    Actions,
    /// Only a `.md` charter file was found.
    Markdown,
    /// Only an `.ics` plan file was found.
    Ics,
    /// Both a `.actions` and a `.md` file were found for the same charter stem.
    ActionsPlusMarkdown,
    /// Both a `.actions` and an `.ics` file were found for the same charter stem.
    ActionsPlusIcs,
    /// Both a `.md` and an `.ics` file were found for the same charter stem.
    MarkdownPlusIcs,
    /// All three file types were found for the same charter stem.
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
    let charter_root = charter_root(root);
    let plans_root = plans_root(root);
    let project_root_charter = project_root_charter(root);

    let action_files = discover_action_files(&charter_root)?;
    let charter_files = discover_charter_files(&charter_root)?;
    let plan_files = discover_plan_files(&plans_root, project_root_charter.as_deref())?;

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
        let relative = file_path.strip_prefix(&charter_root).unwrap_or(&file_path);
        let charter_name =
            infer_charter_name_for_workspace(relative, project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;
        let inferred_parent =
            infer_parent_charter_name_for_workspace(relative, project_root_charter.as_deref());

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
        let relative = file_path.strip_prefix(&charter_root).unwrap_or(&file_path);
        let charter_name =
            infer_charter_name_for_workspace(relative, project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;
        let inferred_parent =
            infer_parent_charter_name_for_workspace(relative, project_root_charter.as_deref());

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
        let plans_dir = entry
            .relative_path
            .parent()
            .map(|path| path.to_string_lossy().into_owned())
            .unwrap_or_else(|| entry.relative_path.to_string_lossy().into_owned());
        if let Some(existing) = entries_by_charter.get_mut(&charter_name) {
            existing.source_type = merge_sources(&existing.source_type, ManifestSourceType::Ics);
        } else {
            entries_by_charter.insert(
                charter_name.clone(),
                WorkspaceManifestEntry {
                    path: plans_dir,
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
