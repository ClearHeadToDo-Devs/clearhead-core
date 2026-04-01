//! This module provides functionality to search for the workspace upon which all other work is
//! based on. To do this, it takes the current working directory, and traverses up to see if it can
//! find a directory named `.clearhead`. if it finds one, it returns the path to that directory. If
//! it doesn't find one, it returns None to denote that it is NOT a project-scoped workspace.

/// Core function to check for a workspace by traversing up the directory tree
/// Returns either the path to the workspace (if found) or None (if not found)
///
/// We designate a workspace by the presence of a `.clearhead` directory. This allows us to easily
/// identify the root of a project and manage project-scoped actions and configurations.
///
/// Examples:
///
/// ```
/// let workspace_path = check_for_workspace(std::path::Path::new("/path/to/your/project/src"));
/// assert_eq!(workspace_path, Some(std::path::PathBuf::from("/path/to/your/project")));
/// ```
pub fn check_for_workspace(cwd: &std::path::Path) -> Option<std::path::PathBuf> {
    let mut current_dir = cwd;

    loop {
        let clearhead_dir = current_dir.join(".clearhead");
        if clearhead_dir.is_dir() {
            return Some(current_dir.to_path_buf());
        }

        // If we've reached the root directory, stop searching
        if let Some(parent) = current_dir.parent() {
            current_dir = parent;
        } else {
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    fn setup_workspace_structure() -> (tempfile::TempDir, std::path::PathBuf, std::path::PathBuf) {
        let temp_dir = tempdir().unwrap();
        let workspace_dir = temp_dir.path().join("my_project");
        let clearhead_dir = workspace_dir.as_path().join(".clearhead");
        fs::create_dir(&workspace_dir).unwrap();
        fs::create_dir(&clearhead_dir).unwrap();
        (temp_dir, workspace_dir, clearhead_dir)
    }

    #[test]
    fn test_check_for_workspace() {
        let (_temp_dir, workspace_dir, _clearhead_dir) = setup_workspace_structure();

        // Test that the workspace is found when we are in the workspace directory
        let found_workspace = check_for_workspace(&workspace_dir);
        assert_eq!(found_workspace, Some(workspace_dir.clone()));
    }

    #[test]
    fn test_check_from_subfolder() {
        let (_tempdir, workspace_dir, _clearhead_dir) = setup_workspace_structure();

        let subfolder = workspace_dir.join("subfolder");
        fs::create_dir(&subfolder).unwrap();

        // Test that the workspace is found when we are in a subfolder of the workspace
        let found_workspace = check_for_workspace(&subfolder);
        assert_eq!(found_workspace, Some(workspace_dir.clone()));
    }

    #[test]
    fn test_check_no_workspace() {
        let temp_dir = tempdir().unwrap();
        let found_workspace = check_for_workspace(temp_dir.path());
        assert_eq!(found_workspace, None);
    }
}
