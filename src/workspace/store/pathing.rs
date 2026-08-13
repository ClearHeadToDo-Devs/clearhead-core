use std::path::{Path, PathBuf};

/// Infer charter name with optional project-root behavior.
pub(crate) fn infer_charter_name_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let filename = relative_path.file_name()?.to_str()?;
    let components: Vec<_> = relative_path.components().collect();

    if components.len() == 1
        && is_primary_filename(filename)
        && let Some(project_name) = project_root_charter
    {
        return Some(project_name.to_string());
    }

    infer_charter_name(relative_path)
}

/// Canonical plans collection owned by the charter anchored at `relative_path`.
///
/// Workspace construction assigns this once for every charter. Consumers then
/// attach calendar resources by exact path instead of reconstructing ownership
/// from aliases, titles, or action-file basenames.
pub(crate) fn infer_plans_collection(relative_path: &Path) -> Option<PathBuf> {
    let filename = relative_path.file_name()?.to_str()?;
    let components: Vec<_> = relative_path.components().collect();

    if components.len() == 1 && is_primary_filename(filename) {
        return Some(PathBuf::from("next"));
    }

    let named_owner;
    let owner = if is_primary_filename(filename) {
        relative_path.parent()?
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

    (!slug.is_empty()).then(|| PathBuf::from(slug))
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

/// Infer parent charter with optional project-root behavior.
pub(crate) fn infer_parent_charter_name_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let filename = relative_path.file_name()?.to_str()?;
    let components: Vec<_> = relative_path.components().collect();

    if let Some(project_name) = project_root_charter {
        if components.len() == 1 {
            if is_primary_filename(filename) {
                return None;
            }
            return Some(project_name.to_string());
        }

        if components.len() == 2 && is_primary_filename(filename) {
            return Some(project_name.to_string());
        }
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
    filename == "next.actions" || filename == "README.md"
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

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
            infer_plans_collection(Path::new("next.actions")),
            Some(PathBuf::from("next"))
        );
        assert_eq!(
            infer_plans_collection(Path::new("linux/next.actions")),
            Some(PathBuf::from("linux"))
        );
        assert_eq!(
            infer_plans_collection(Path::new("work/feature/next.actions")),
            Some(PathBuf::from("work-feature"))
        );
        assert_eq!(
            infer_plans_collection(Path::new("inbox.actions")),
            Some(PathBuf::from("inbox"))
        );
        assert_eq!(
            infer_plans_collection(Path::new("next.actions")),
            Some(PathBuf::from("next"))
        );
    }

    #[test]
    fn infer_workspace_project_root_rules() {
        assert_eq!(
            infer_charter_name_for_workspace(Path::new("next.actions"), Some("platform")),
            Some("platform".into())
        );
        assert_eq!(
            infer_parent_charter_name_for_workspace(Path::new("next.actions"), Some("platform")),
            None
        );
        assert_eq!(
            infer_parent_charter_name_for_workspace(
                Path::new("observability.actions"),
                Some("platform")
            ),
            Some("platform".into())
        );
    }
}
