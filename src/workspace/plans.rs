use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};
use std::path::{Path, PathBuf};

/// A discovered `.ics` file with inferred charter metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlanFileEntry {
    pub path: PathBuf,
    pub relative_path: PathBuf,
    pub charter_name: String,
    pub inferred_parent: Option<String>,
}

/// Discover all `.ics` plan files and infer charter relationships.
///
/// This applies workspace layout rules from the naming-conventions spec:
/// - Project layout: `<project>/.clearhead/next.ics` maps to the project charter.
/// - User layout: filenames map directly to charter names.
/// - Folder `next.ics` files map to their parent folder charter.
pub fn collect_plan_files(root: &Path) -> Result<Vec<PlanFileEntry>, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    let mut files = Vec::new();
    discover_plan_paths(&layout.data_root, &mut files)?;

    let mut entries = Vec::new();
    for path in files {
        let Ok(relative_path) = path.strip_prefix(&layout.data_root) else {
            return Err(WorkspaceError::InvalidPath(path));
        };

        let relative_path = relative_path.to_path_buf();

        let Some(charter_name) = infer_plan_charter_name_for_workspace(
            &relative_path,
            layout.project_root_charter.as_deref(),
        ) else {
            continue;
        };

        let inferred_parent =
            infer_plan_parent_for_workspace(&relative_path, layout.project_root_charter.as_deref());

        entries.push(PlanFileEntry {
            path,
            relative_path,
            charter_name,
            inferred_parent,
        });
    }

    entries.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
    Ok(entries)
}

/// Infer charter name for a `.ics` file path, with optional project-root behavior.
pub fn infer_plan_charter_name_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let filename = relative_path.file_name()?.to_str()?;
    let components: Vec<_> = relative_path.components().collect();

    if components.len() == 1
        && is_primary_plan_filename(filename)
        && let Some(project_name) = project_root_charter
    {
        return Some(project_name.to_string());
    }

    infer_plan_charter_name(relative_path)
}

/// Infer charter name for a `.ics` file path.
pub fn infer_plan_charter_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();
    if components.is_empty() {
        return None;
    }

    let filename = relative_path.file_name()?.to_str()?;
    if components.len() == 1 {
        let stem = relative_path.file_stem()?.to_str()?;
        return Some(stem.to_string());
    }

    if is_primary_plan_filename(filename) {
        if let std::path::Component::Normal(name) = components[components.len() - 2] {
            return name.to_str().map(ToString::to_string);
        }
        return None;
    }

    let stem = relative_path.file_stem()?.to_str()?;
    Some(stem.to_string())
}

/// Infer parent charter for a `.ics` file path, with optional project-root behavior.
pub fn infer_plan_parent_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let filename = relative_path.file_name()?.to_str()?;
    let components: Vec<_> = relative_path.components().collect();

    if let Some(project_name) = project_root_charter {
        if components.len() == 1 {
            if is_primary_plan_filename(filename) {
                return None;
            }
            return Some(project_name.to_string());
        }

        if components.len() == 2 && is_primary_plan_filename(filename) {
            return Some(project_name.to_string());
        }
    }

    infer_plan_parent(relative_path)
}

/// Infer parent charter for a `.ics` file path.
pub fn infer_plan_parent(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();
    let filename = relative_path.file_name()?.to_str()?;

    if components.len() <= 1 {
        return None;
    }

    if is_primary_plan_filename(filename) {
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

fn discover_plan_paths(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), WorkspaceError> {
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
            discover_plan_paths(&path, files)?;
            continue;
        }

        if path.is_file()
            && let Some(ext) = path.extension()
            && *ext == *"ics"
        {
            files.push(path);
        }
    }

    Ok(())
}

fn is_primary_plan_filename(filename: &str) -> bool {
    filename == "next.ics"
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn infer_plan_charter_names() {
        assert_eq!(
            infer_plan_charter_name(Path::new("inbox.ics")),
            Some("inbox".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("new/next.ics")),
            Some("new".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("new/subcharter.ics")),
            Some("subcharter".into())
        );
    }

    #[test]
    fn infer_plan_parent_names() {
        assert_eq!(infer_plan_parent(Path::new("inbox.ics")), None);
        assert_eq!(
            infer_plan_parent(Path::new("new/subcharter.ics")),
            Some("new".into())
        );
        assert_eq!(infer_plan_parent(Path::new("new/next.ics")), None);
        assert_eq!(
            infer_plan_parent(Path::new("new/sub/next.ics")),
            Some("new".into())
        );
    }

    #[test]
    fn infer_workspace_project_root_rules() {
        assert_eq!(
            infer_plan_charter_name_for_workspace(Path::new("next.ics"), Some("platform")),
            Some("platform".into())
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("next.ics"), Some("platform")),
            None
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("work.ics"), Some("platform")),
            Some("platform".into())
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("work/next.ics"), Some("platform")),
            Some("platform".into())
        );
    }

    #[test]
    fn collect_plan_files_uses_layout_and_inference() {
        let outer = tempfile::tempdir().expect("tempdir");
        let project = outer.path().join("my-project");
        let data = project.join(".clearhead");
        fs::create_dir_all(data.join("work")).expect("create dirs");
        fs::create_dir_all(data.join("work").join("ops")).expect("create nested dirs");

        fs::write(data.join("next.ics"), "BEGIN:VCALENDAR\nEND:VCALENDAR\n").expect("write");
        fs::write(data.join("inbox.ics"), "BEGIN:VCALENDAR\nEND:VCALENDAR\n").expect("write");
        fs::write(
            data.join("work").join("next.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::write(
            data.join("work").join("ops.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::write(
            data.join("work").join("ops").join("next.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::create_dir_all(data.join(".hidden")).expect("mkdir hidden");
        fs::write(
            data.join(".hidden").join("ghost.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");

        let entries = collect_plan_files(&project).expect("collect failed");

        let summarized: Vec<(String, String, Option<String>)> = entries
            .into_iter()
            .map(|entry| {
                (
                    entry.relative_path.display().to_string(),
                    entry.charter_name,
                    entry.inferred_parent,
                )
            })
            .collect();

        assert_eq!(
            summarized,
            vec![
                (
                    "inbox.ics".into(),
                    "inbox".into(),
                    Some("my-project".into())
                ),
                ("next.ics".into(), "my-project".into(), None),
                (
                    "work/next.ics".into(),
                    "work".into(),
                    Some("my-project".into())
                ),
                (
                    "work/ops/next.ics".into(),
                    "ops".into(),
                    Some("work".into())
                ),
                ("work/ops.ics".into(), "ops".into(), Some("work".into())),
            ]
        );
    }
}
