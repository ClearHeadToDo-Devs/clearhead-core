//! Native template discovery using Core's ordered candidate policy.

use std::path::{Path, PathBuf};

use clearhead_core::workspace::WorkspaceError;

pub fn resolve_template(
    charter_dir: &Path,
    data_root: &Path,
    name: &str,
) -> Result<Option<PathBuf>, WorkspaceError> {
    Ok(
        clearhead_core::workspace::templates::template_candidates(charter_dir, data_root, name)
            .into_iter()
            .find(|path| path.is_file()),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn resolve_finds_charter_local_first() {
        let tmp = tempfile::tempdir().unwrap();
        let charter_dir = tmp.path().join("health");
        let data_root = tmp.path().join("root");

        fs::create_dir_all(charter_dir.join("templates")).unwrap();
        fs::create_dir_all(data_root.join("templates")).unwrap();

        let local = charter_dir.join("templates/weekly-review.actions");
        let root = data_root.join("templates/weekly-review.actions");
        fs::write(&local, "[ ] Step one\n").unwrap();
        fs::write(&root, "[ ] Root version\n").unwrap();

        let result = resolve_template(&charter_dir, &data_root, "weekly-review").unwrap();
        assert_eq!(result, Some(local));
    }

    #[test]
    fn resolve_falls_back_to_data_root() {
        let tmp = tempfile::tempdir().unwrap();
        let charter_dir = tmp.path().join("health");
        let data_root = tmp.path().join("root");

        fs::create_dir_all(&charter_dir).unwrap();
        fs::create_dir_all(data_root.join("templates")).unwrap();

        let root = data_root.join("templates/weekly-review.actions");
        fs::write(&root, "[ ] Step one\n").unwrap();

        let result = resolve_template(&charter_dir, &data_root, "weekly-review").unwrap();
        assert_eq!(result, Some(root));
    }

    #[test]
    fn resolve_returns_none_when_missing() {
        let tmp = tempfile::tempdir().unwrap();
        let result =
            resolve_template(&tmp.path().join("a"), &tmp.path().join("b"), "nonexistent").unwrap();
        assert!(result.is_none());
    }
}
