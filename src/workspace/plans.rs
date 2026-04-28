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

/// Discover all vdir `.ics` plan files and infer charter relationships.
///
/// This applies workspace layout rules from the naming-conventions spec:
/// - Plans live only in `plans/` directories.
/// - Root charter plans live at `charters/plans/<uid>.ics`.
/// - Sub-charter plans live at `charters/<charter>/plans/<uid>.ics`.
pub fn collect_plan_files(root: &Path) -> Result<Vec<PlanFileEntry>, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    let mut files = Vec::new();
    discover_plan_paths(&layout.charter_root, &mut files)?;

    let mut entries = Vec::new();
    for path in files {
        let Ok(relative_path) = path.strip_prefix(&layout.charter_root) else {
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

/// Infer charter name for a vdir `.ics` file path, with optional project-root behavior.
pub fn infer_plan_charter_name_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let charter_components = charter_components(relative_path)?;

    if charter_components.is_empty() {
        return project_root_charter.map(ToString::to_string);
    }

    charter_components.last().cloned()
}

/// Infer charter name for a vdir `.ics` file path.
pub fn infer_plan_charter_name(relative_path: &Path) -> Option<String> {
    let charter_components = charter_components(relative_path)?;
    charter_components.last().cloned()
}

/// Infer parent charter for a vdir `.ics` file path, with optional project-root behavior.
pub fn infer_plan_parent_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let charter_components = charter_components(relative_path)?;

    if charter_components.is_empty() {
        return None;
    }

    if charter_components.len() == 1 {
        return project_root_charter.map(ToString::to_string);
    }

    charter_components.get(charter_components.len() - 2).cloned()
}

/// Infer parent charter for a vdir `.ics` file path.
pub fn infer_plan_parent(relative_path: &Path) -> Option<String> {
    let charter_components = charter_components(relative_path)?;
    charter_components
        .get(charter_components.len().checked_sub(2)?)
        .cloned()
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

        if path.is_file() && is_vdir_plan_file(&path) {
            files.push(path);
        }
    }

    Ok(())
}

fn is_vdir_plan_file(path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "ics")
        && path
            .parent()
            .and_then(|parent| parent.file_name())
            .and_then(|name| name.to_str())
            .is_some_and(|name| name == "plans" || name.ends_with(".plans"))
}

fn charter_components(relative_path: &Path) -> Option<Vec<String>> {
    let mut components = Vec::new();
    let mut saw_plans = false;

    for component in relative_path.components() {
        let std::path::Component::Normal(name) = component else {
            return None;
        };
        let name = name.to_str()?;
        if name == "plans" {
            saw_plans = true;
            break;
        }
        if let Some(stem) = name.strip_suffix(".plans") {
            components.push(stem.to_string());
            saw_plans = true;
            break;
        }
        components.push(name.to_string());
    }

    if !saw_plans || relative_path.extension().and_then(|ext| ext.to_str()) != Some("ics") {
        return None;
    }

    Some(components)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn infer_plan_charter_names() {
        assert_eq!(
            infer_plan_charter_name(Path::new("inbox/plans/weekly-review.ics")),
            Some("inbox".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("new/plans/weekly-review.ics")),
            Some("new".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("new/subcharter/plans/sprint.ics")),
            Some("subcharter".into())
        );
        // .plans suffix form (flat charter sibling dir)
        assert_eq!(
            infer_plan_charter_name(Path::new("subproject.plans/a1b2.ics")),
            Some("subproject".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("work/sub.plans/a1b2.ics")),
            Some("sub".into())
        );
    }

    #[test]
    fn infer_plan_parent_names() {
        assert_eq!(infer_plan_parent(Path::new("inbox/plans/weekly-review.ics")), None);
        assert_eq!(
            infer_plan_parent(Path::new("new/subcharter/plans/sprint.ics")),
            Some("new".into())
        );
        assert_eq!(infer_plan_parent(Path::new("new/plans/weekly-review.ics")), None);
        assert_eq!(
            infer_plan_parent(Path::new("new/sub/plans/weekly-review.ics")),
            Some("new".into())
        );
        // .plans suffix form
        assert_eq!(infer_plan_parent(Path::new("subproject.plans/a1b2.ics")), None);
        assert_eq!(
            infer_plan_parent(Path::new("work/sub.plans/a1b2.ics")),
            Some("work".into())
        );
    }

    #[test]
    fn infer_workspace_project_root_rules() {
        assert_eq!(
            infer_plan_charter_name_for_workspace(Path::new("plans/root.ics"), Some("platform")),
            Some("platform".into())
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("plans/root.ics"), Some("platform")),
            None
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("work/plans/release.ics"), Some("platform")),
            Some("platform".into())
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("work/ops/plans/deploy.ics"), Some("platform")),
            Some("work".into())
        );
    }

    #[test]
    fn collect_plan_files_uses_layout_and_inference() {
        let outer = tempfile::tempdir().expect("tempdir");
        let project = outer.path().join("my-project");
        let data = project.join(".clearhead").join("charters");
        fs::create_dir_all(data.join("plans")).expect("create root plans");
        fs::create_dir_all(data.join("work").join("plans")).expect("create dirs");
        fs::create_dir_all(data.join("work").join("ops").join("plans")).expect("create nested dirs");

        fs::write(
            data.join("plans").join("root.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::write(
            data.join("work").join("plans").join("release.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::write(
            data.join("work").join("ops").join("plans").join("deploy.ics"),
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
                    "plans/root.ics".into(),
                    "my-project".into(),
                    None
                ),
                (
                    "work/ops/plans/deploy.ics".into(),
                    "ops".into(),
                    Some("work".into())
                ),
                (
                    "work/plans/release.ics".into(),
                    "work".into(),
                    Some("my-project".into())
                ),
            ]
        );
    }

    #[test]
    fn collect_plan_files_dotplans_sibling_dir() {
        let outer = tempfile::tempdir().expect("tempdir");
        let project = outer.path().join("my-project");
        let data = project.join(".clearhead").join("charters");

        // flat charter: subproject.actions + subproject.plans/
        fs::create_dir_all(data.join("subproject.plans")).expect("create .plans dir");
        fs::write(data.join("subproject.actions"), "").expect("write actions");
        fs::write(
            data.join("subproject.plans").join("a1b2c3d4.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write ics");

        // nested: work/feature.plans/
        fs::create_dir_all(data.join("work").join("feature.plans")).expect("create nested .plans dir");
        fs::write(
            data.join("work").join("feature.plans").join("b2c3d4e5.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write nested ics");

        let entries = collect_plan_files(&project).expect("collect failed");
        let summarized: Vec<(String, String, Option<String>)> = entries
            .into_iter()
            .map(|e| (e.relative_path.display().to_string(), e.charter_name, e.inferred_parent))
            .collect();

        assert_eq!(
            summarized,
            vec![
                (
                    "subproject.plans/a1b2c3d4.ics".into(),
                    "subproject".into(),
                    Some("my-project".into()) // root charter is inferred parent
                ),
                (
                    "work/feature.plans/b2c3d4e5.ics".into(),
                    "feature".into(),
                    Some("work".into())
                ),
            ]
        );
    }
}
