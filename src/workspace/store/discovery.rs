use super::WorkspaceError;
use std::path::{Path, PathBuf};

/// Discover all `.actions` files recursively, skipping hidden directories.
pub(crate) fn discover_action_files(dir: &Path) -> Result<Vec<PathBuf>, WorkspaceError> {
    let mut files = Vec::new();
    discover_recursive(dir, "actions", &mut files)?;
    files.sort();
    Ok(files)
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
