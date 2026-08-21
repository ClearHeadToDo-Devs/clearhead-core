use crate::domain::{Action, Plan};
use crate::workspace::charter::MarkdownCharter;
use std::path::{Path, PathBuf};

/// Slug a user-facing value into the canonical directory/file form used by ClearHead.
///
/// This intentionally preserves the historical policy: lowercase Unicode,
/// replace spaces with `-`, and replace `&` with `and`. It does not perform
/// broad punctuation stripping because existing workspace paths and imported
/// iCalendar UIDs depend on byte-for-byte stability.
pub fn slugify(value: &str) -> String {
    value.to_lowercase().replace(' ', "-").replace('&', "and")
}

/// File name for a plan's `.ics` mirror.
///
/// Uses `Plan.external_id` when present (the stable iCalendar UID), otherwise the
/// plan's own id. Matches the CLI's historical `<uid>.ics` naming.
pub fn plan_file_name(plan: &Plan) -> String {
    let uid = plan
        .external_id
        .clone()
        .unwrap_or_else(|| plan.id.to_string());
    format!("{}.ics", slugify(&uid))
}

/// Relative plans directory for a charter under `plans_root`.
///
/// Return collection ownership established when the charter was constructed.
pub fn charter_plans_dir_relative(charter: &MarkdownCharter) -> PathBuf {
    charter.plans_dir.clone()
}

/// Absolute output path for a plan's `.ics` file under `plans_root`.
pub fn plan_output_path(plans_root: &Path, charter: &MarkdownCharter, plan: &Plan) -> PathBuf {
    plans_root
        .join(charter_plans_dir_relative(charter))
        .join(plan_file_name(plan))
}

/// Absolute output path for a standalone action's mirrored `.ics` file.
///
/// The mirror is keyed by the action's id (`UID == action.id`), one VTODO per
/// file, under the same per-charter directory policy as plans.
pub fn action_mirror_path(
    plans_root: &Path,
    charter: &MarkdownCharter,
    action: &Action,
) -> PathBuf {
    plans_root
        .join(charter_plans_dir_relative(charter))
        .join(format!("{}.ics", slugify(&action.id.to_string())))
}

/// Infer charter name for an `.ics` path relative to `plans_root`, with project-root support.
///
/// The slug `next` maps to `project_root_charter` when in a project workspace.
pub fn infer_plan_charter_name_for_workspace(
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
pub fn infer_plan_parent_for_workspace(
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::charter::implicit_charter;

    #[test]
    fn canonical_slug_policy_is_stable() {
        assert_eq!(slugify("Team & Operations"), "team-and-operations");
        assert_eq!(slugify("Already--Slugged"), "already--slugged");
        assert_eq!(slugify("release@example.com"), "release@example.com");
    }

    #[test]
    fn canonical_plan_paths_cover_uid_and_charter_policy() {
        let plan = Plan {
            external_id: Some("Weekly Review & Notes".to_string()),
            ..Default::default()
        };
        assert_eq!(plan_file_name(&plan), "weekly-review-and-notes.ics");

        let top = MarkdownCharter::from(implicit_charter("Team & Ops"));
        assert_eq!(charter_plans_dir_relative(&top), Path::new("team-and-ops"));

        let mut child_domain = implicit_charter("Release Notes");
        child_domain.parent = Some("Team & Ops".to_string());
        let child = MarkdownCharter::from(child_domain);
        assert_eq!(
            charter_plans_dir_relative(&child),
            Path::new("team-and-ops-release-notes")
        );
        assert_eq!(
            plan_output_path(Path::new("/plans"), &child, &plan),
            Path::new("/plans/team-and-ops-release-notes/weekly-review-and-notes.ics")
        );

        let mut top = top;
        top.plans_dir = PathBuf::from("next");
        assert_eq!(charter_plans_dir_relative(&top), Path::new("next"));
    }

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
        assert_eq!(infer_plan_charter_name(Path::new("inbox")), None);
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
        assert_eq!(
            infer_plan_parent(Path::new("inbox/weekly-review.ics")),
            None
        );
        assert_eq!(
            infer_plan_parent(Path::new("work-feature/deploy.ics")),
            None
        );
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
}
