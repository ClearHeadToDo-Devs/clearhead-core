use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};
use std::path::{Path, PathBuf};

/// A discovered `.ics` file with inferred charter metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlanFileEntry {
    pub path: PathBuf,
    /// Path relative to `plans_root` — e.g. `inbox/uid.ics` or `work-feature/uid.ics`.
    pub relative_path: PathBuf,
    pub charter_name: String,
    pub inferred_parent: Option<String>,
}

/// Discover all vdir `.ics` plan files and infer charter relationships.
///
/// Plans live in `<data_root>/plans/`, parallel to `charters/`.
/// Each charter gets one flat subdirectory: `plans/<slug>/<uid>.ics`.
/// Sub-charter hierarchy is encoded in the slug using `-` as a separator
/// (e.g. `work-feature` for the `feature` charter under `work`).
/// In project workspaces the root charter uses the reserved slug `next`.
pub fn collect_plan_files(root: &Path) -> Result<Vec<PlanFileEntry>, WorkspaceError> {
    collect_plan_files_with_plans(root, None)
}

/// Like [`collect_plan_files`] but reads `.ics` from `plan_override` when given
/// (the resolved `plan_path` config value), instead of the workspace's own
/// `plans/` directory. Charter-slug inference still comes from `root`'s layout.
pub fn collect_plan_files_with_plans(
    root: &Path,
    plan_override: Option<&Path>,
) -> Result<Vec<PlanFileEntry>, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    let plans_root = plan_override.unwrap_or(&layout.plans_root);
    collect_plan_files_in(plans_root, layout.project_root_charter.as_deref())
}

/// Leaf form of [`collect_plan_files`]: discover `.ics` plan files directly under
/// `plans_root`, trusting the caller to have already resolved any `plan_path`
/// override. `project_root_charter` maps the reserved `next` slug to the project's
/// root charter (`None` for user workspaces).
pub(crate) fn collect_plan_files_in(
    plans_root: &Path,
    project_root_charter: Option<&str>,
) -> Result<Vec<PlanFileEntry>, WorkspaceError> {
    let mut files = Vec::new();
    discover_plan_paths(plans_root, &mut files)?;

    let mut entries = Vec::new();
    for path in files {
        let Ok(relative_path) = path.strip_prefix(plans_root) else {
            return Err(WorkspaceError::InvalidPath(path));
        };

        let relative_path = relative_path.to_path_buf();

        let Some(charter_name) =
            infer_plan_charter_name_for_workspace(&relative_path, project_root_charter)
        else {
            continue;
        };

        let inferred_parent =
            infer_plan_parent_for_workspace(&relative_path, project_root_charter);

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

/// Infer charter name for an `.ics` path relative to `plans_root`, with project-root support.
///
/// The slug `next` maps to `project_root_charter` when in a project workspace.
pub(crate) fn infer_plan_charter_name_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let slug = plan_charter_slug(relative_path)?;
    if slug == "next" {
        // In project workspaces "next" maps to the project root charter name.
        // In user workspaces there is no root charter, so "next" is just "next".
        Some(project_root_charter.unwrap_or("next").to_string())
    } else {
        Some(slug)
    }
}

/// Infer charter name for an `.ics` path relative to `plans_root`.
pub fn infer_plan_charter_name(relative_path: &Path) -> Option<String> {
    plan_charter_slug(relative_path)
}

/// Infer parent charter for an `.ics` path relative to `plans_root`, with project-root support.
///
/// Named charters in a project workspace are children of the root charter.
/// Sub-charter hierarchy (e.g. `work-feature`) is resolved at load time via slug matching.
pub(crate) fn infer_plan_parent_for_workspace(
    relative_path: &Path,
    project_root_charter: Option<&str>,
) -> Option<String> {
    let slug = plan_charter_slug(relative_path)?;
    if slug == "next" {
        None
    } else {
        project_root_charter.map(ToString::to_string)
    }
}

/// Infer parent charter for an `.ics` path relative to `plans_root`.
pub fn infer_plan_parent(relative_path: &Path) -> Option<String> {
    // Without project-root context parent inference is not possible from the path alone.
    // Callers that need hierarchy should use infer_plan_parent_for_workspace.
    let _ = plan_charter_slug(relative_path)?;
    None
}

/// Extract the charter slug from a path relative to `plans_root`.
///
/// Valid forms:
/// - `<slug>/<uid>.ics` → returns `<slug>`
///
/// Returns `None` for paths that don't match the expected depth.
fn plan_charter_slug(relative_path: &Path) -> Option<String> {
    let mut components = relative_path.components();

    let first = components.next()?;
    let std::path::Component::Normal(first_os) = first else {
        return None;
    };
    let first_str = first_os.to_str()?;

    match components.next() {
        None => {
            // Single component — must be a .ics file directly in plans_root (invalid layout)
            None
        }
        Some(second) => {
            let std::path::Component::Normal(second_os) = second else {
                return None;
            };
            let second_str = second_os.to_str()?;
            // Exactly two components and last one is an .ics file
            if !second_str.ends_with(".ics") || components.next().is_some() {
                return None;
            }
            Some(first_str.to_string())
        }
    }
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

        if path.is_file() && path.extension().is_some_and(|ext| ext == "ics") {
            files.push(path);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn infer_plan_charter_names() {
        assert_eq!(
            infer_plan_charter_name(Path::new("inbox/weekly-review.ics")),
            Some("inbox".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("work/sprint.ics")),
            Some("work".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("work-feature/deploy.ics")),
            Some("work-feature".into())
        );
        assert_eq!(
            infer_plan_charter_name(Path::new("subproject/task.ics")),
            Some("subproject".into())
        );
        // Too many components — invalid
        assert_eq!(
            infer_plan_charter_name(Path::new("work/feature/deploy.ics")),
            None
        );
        // Single component without .ics — invalid
        assert_eq!(
            infer_plan_charter_name(Path::new("inbox")),
            None
        );
    }

    #[test]
    fn infer_plan_charter_name_workspace_maps_next_to_project_root() {
        assert_eq!(
            infer_plan_charter_name_for_workspace(Path::new("next/root.ics"), Some("platform")),
            Some("platform".into())
        );
        assert_eq!(
            infer_plan_charter_name_for_workspace(Path::new("inbox/weekly.ics"), Some("platform")),
            Some("inbox".into())
        );
        assert_eq!(
            infer_plan_charter_name_for_workspace(Path::new("next/root.ics"), None),
            Some("next".into())
        );
    }

    #[test]
    fn infer_plan_parent_names() {
        assert_eq!(infer_plan_parent(Path::new("inbox/weekly-review.ics")), None);
        assert_eq!(infer_plan_parent(Path::new("work-feature/deploy.ics")), None);
    }

    #[test]
    fn infer_plan_parent_workspace_uses_project_root() {
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("inbox/weekly.ics"), Some("platform")),
            Some("platform".into())
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("next/root.ics"), Some("platform")),
            None
        );
        assert_eq!(
            infer_plan_parent_for_workspace(Path::new("inbox/weekly.ics"), None),
            None
        );
    }

    #[test]
    fn collect_plan_files_uses_plans_root() {
        let outer = tempfile::tempdir().expect("tempdir");
        let project = outer.path().join("my-project");
        let plans = project.join(".clearhead").join("plans");
        fs::create_dir_all(plans.join("next")).expect("create next");
        fs::create_dir_all(plans.join("work")).expect("create work");
        fs::create_dir_all(plans.join("work-ops")).expect("create work-ops");

        fs::write(
            plans.join("next").join("root.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::write(
            plans.join("work").join("release.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::write(
            plans.join("work-ops").join("deploy.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::create_dir_all(plans.join(".hidden")).expect("mkdir hidden");
        fs::write(
            plans.join(".hidden").join("ghost.ics"),
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
                    "next/root.ics".into(),
                    "my-project".into(),
                    None
                ),
                (
                    "work/release.ics".into(),
                    "work".into(),
                    Some("my-project".into())
                ),
                (
                    "work-ops/deploy.ics".into(),
                    "work-ops".into(),
                    Some("my-project".into())
                ),
            ]
        );
    }

    #[test]
    fn collect_plan_files_user_workspace() {
        let outer = tempfile::tempdir().expect("tempdir");
        let workspace = outer.path().join("clearhead");
        let plans = workspace.join("plans");
        fs::create_dir_all(plans.join("inbox")).expect("create inbox");
        fs::create_dir_all(plans.join("work")).expect("create work");

        fs::write(
            plans.join("inbox").join("a1b2c3d4.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");
        fs::write(
            plans.join("work").join("b2c3d4e5.ics"),
            "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
        )
        .expect("write");

        let entries = collect_plan_files(&workspace).expect("collect failed");
        let summarized: Vec<(String, String, Option<String>)> = entries
            .into_iter()
            .map(|e| (e.relative_path.display().to_string(), e.charter_name, e.inferred_parent))
            .collect();

        assert_eq!(
            summarized,
            vec![
                ("inbox/a1b2c3d4.ics".into(), "inbox".into(), None),
                ("work/b2c3d4e5.ics".into(), "work".into(), None),
            ]
        );
    }
}
