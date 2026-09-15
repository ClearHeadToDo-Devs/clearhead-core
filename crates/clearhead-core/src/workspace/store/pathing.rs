use std::path::{Path, PathBuf};

/// Reserved stem of every primary charter's action anchor and of the root's
/// plan collection (`charters/next.actions`, `plans/next/`).
pub const ROOT_ANCHOR_STEM: &str = "next";
/// A primary charter's action anchor; at the charter root it anchors the workspace root.
pub const PRIMARY_ACTIONS_FILE: &str = "next.actions";
/// A primary charter's prose and identity anchor.
pub const PRIMARY_DOCUMENT_FILE: &str = "README.md";

/// Infer a charter name; the workspace root's primary files name the root charter.
pub fn infer_charter_name_for_workspace(
    relative_path: &Path,
    root_charter: &str,
) -> Option<String> {
    let filename = relative_path.file_name()?.to_str()?;
    let components: Vec<_> = relative_path.components().collect();

    if components.len() == 1 && is_primary_filename(filename) {
        return Some(root_charter.to_string());
    }

    infer_charter_name(relative_path)
}

/// Canonical plans collection owned by the charter anchored at `relative_path`.
///
/// Workspace construction assigns this once for every charter. Consumers then
/// attach calendar resources by exact path instead of reconstructing ownership
/// from aliases, titles, or action-file basenames.
pub fn charter_collection_from_anchor(relative_path: &Path) -> PathBuf {
    let filename = relative_path
        .file_name()
        .and_then(|name| name.to_str())
        .expect("a discovered charter anchor has a UTF-8 filename");
    let components: Vec<_> = relative_path.components().collect();

    if components.len() == 1 && is_primary_filename(filename) {
        return PathBuf::from(ROOT_ANCHOR_STEM);
    }

    let named_owner;
    let owner = if is_primary_filename(filename) {
        relative_path
            .parent()
            .expect("a nested primary charter anchor has a parent")
    } else {
        named_owner = relative_path.with_extension("");
        &named_owner
    };
    let slug = owner
        .components()
        .filter_map(|component| match component {
            std::path::Component::Normal(value) => value.to_str().map(crate::workspace::slugify),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("-");

    assert!(
        !slug.is_empty(),
        "a charter anchor produces a collection path"
    );
    PathBuf::from(slug)
}

/// Infer the charter name from a relative file path.
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

    if is_primary_filename(filename) {
        if let std::path::Component::Normal(name) = components[components.len() - 2] {
            return name.to_str().map(ToString::to_string);
        }
        return None;
    }

    let stem = relative_path.file_stem()?.to_str()?;
    Some(strip_archive_suffix(stem).to_string())
}

/// Infer a parent charter name; flat charters and top-level directories descend
/// from the workspace's single root charter.
pub fn infer_parent_charter_name_for_workspace(
    relative_path: &Path,
    root_charter: &str,
) -> Option<String> {
    let filename = relative_path.file_name()?.to_str()?;
    let components: Vec<_> = relative_path.components().collect();

    if components.len() == 1 {
        if is_primary_filename(filename)
            || infer_charter_name(relative_path).as_deref() == Some(root_charter)
        {
            return None;
        }
        return Some(root_charter.to_string());
    }

    if components.len() == 2 && is_primary_filename(filename) {
        return Some(root_charter.to_string());
    }

    infer_parent_charter_name(relative_path)
}
/// Infer the parent charter name from a file path.
pub fn infer_parent_charter_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();
    let filename = relative_path.file_name()?.to_str()?;

    if components.len() <= 1 {
        return None;
    }

    if is_primary_filename(filename) {
        if components.len() == 2 {
            return None;
        }
        if let std::path::Component::Normal(name) = components[components.len() - 3] {
            return name.to_str().map(ToString::to_string);
        }
    } else if let std::path::Component::Normal(name) = components[components.len() - 2] {
        return name.to_str().map(ToString::to_string);
    }

    None
}
/// Strip archive suffixes (`.completed`, `.archived`) from a file stem.
///
/// `health.completed` → `"health"`, `health` → `"health"`.
pub(crate) fn strip_archive_suffix(stem: &str) -> &str {
    stem.strip_suffix(".completed")
        .or_else(|| stem.strip_suffix(".archived"))
        .unwrap_or(stem)
}

/// Returns true for filenames where the parent directory is the charter name.
///
/// Both `next.actions` and `README.md` are "primary" files — they represent
/// the charter itself, not a sub-charter.
pub(crate) fn is_primary_filename(filename: &str) -> bool {
    filename == PRIMARY_ACTIONS_FILE || filename == PRIMARY_DOCUMENT_FILE
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn primary_actions_file_is_the_root_anchor_stem() {
        assert_eq!(PRIMARY_ACTIONS_FILE, format!("{ROOT_ANCHOR_STEM}.actions"));
    }

    #[test]
    fn infer_charter_names() {
        assert_eq!(
            infer_charter_name(Path::new("work.actions")),
            Some("work".into())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/next.actions")),
            Some("myproject".into())
        );
        assert_eq!(
            infer_charter_name(Path::new("myproject/subcharter.actions")),
            Some("subcharter".into())
        );
    }

    #[test]
    fn infer_parent_charter_names() {
        assert_eq!(infer_parent_charter_name(Path::new("work.actions")), None);
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/subcharter.actions")),
            Some("myproject".into())
        );
        assert_eq!(
            infer_parent_charter_name(Path::new("myproject/subdir/next.actions")),
            Some("myproject".into())
        );
    }

    #[test]
    fn plans_collection_is_derived_from_the_workspace_anchor() {
        assert_eq!(
            charter_collection_from_anchor(Path::new("next.actions")),
            PathBuf::from("next")
        );
        assert_eq!(
            charter_collection_from_anchor(Path::new("linux/next.actions")),
            PathBuf::from("linux")
        );
        assert_eq!(
            charter_collection_from_anchor(Path::new("work/feature/next.actions")),
            PathBuf::from("work-feature")
        );
        assert_eq!(
            charter_collection_from_anchor(Path::new("inbox.actions")),
            PathBuf::from("inbox")
        );
    }

    #[test]
    fn infer_workspace_project_root_rules() {
        assert_eq!(
            infer_charter_name_for_workspace(Path::new("next.actions"), "platform"),
            Some("platform".into())
        );
        assert_eq!(
            infer_parent_charter_name_for_workspace(Path::new("next.actions"), "platform"),
            None
        );
        assert_eq!(
            infer_parent_charter_name_for_workspace(Path::new("observability.actions"), "platform"),
            Some("platform".into())
        );
        assert_eq!(
            infer_parent_charter_name_for_workspace(Path::new("platform.actions"), "platform"),
            None,
            "a legacy named root anchor must not parent the project to itself"
        );
        assert_eq!(
            infer_parent_charter_name_for_workspace(Path::new("platform.md"), "platform"),
            None,
            "the root Charter document must not infer a self-parent"
        );
    }
}
