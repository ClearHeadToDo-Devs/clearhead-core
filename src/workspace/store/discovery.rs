use super::WorkspaceError;
use crate::workspace::calendar::plans::{
    infer_plan_charter_name_for_workspace, infer_plan_parent_for_workspace,
};
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

/// Discover and classify legacy native `.ics` resources for the root loader.
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
