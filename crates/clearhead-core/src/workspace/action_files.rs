//! Per-charter actions-resource classification and path derivation.
//!
//! - `<charter>.actions` — executable Actions, including persisted occurrences
//! - `<charter>.habits.actions` — native Habit roots and template children
//! - `<charter>.completed.actions` — terminal Action history
//! - `<charter>.upcoming.actions` — retired artifacts, never live work
//!
//! Charter stem derivation mostly uses the file stem. Primary files like
//! `subdir/next.actions` use the directory name, except the root anchor
//! itself (`charters/next.actions`), which keeps the reserved `next` stem
//! regardless of project or user scope — every workspace's root shares it.

use std::path::{Path, PathBuf};

use crate::workspace::actions::ActionList;
use crate::workspace::actions::repository::SourcedAction;

/// The semantic role of an actions resource. Classify before deriving stems.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ActionFileRole {
    Actions,
    Habits,
    Completed,
    Upcoming,
}

/// Classify a known suffix, longest first, and return its full charter stem.
pub fn classify_action_file(path: &Path) -> Option<(&str, ActionFileRole)> {
    let name = path.file_name()?.to_str()?;
    for (suffix, role) in [
        (".habits.actions", ActionFileRole::Habits),
        (".completed.actions", ActionFileRole::Completed),
        (".upcoming.actions", ActionFileRole::Upcoming),
        (".actions", ActionFileRole::Actions),
    ] {
        if let Some(stem) = name.strip_suffix(suffix) {
            return Some((stem, role));
        }
    }
    None
}

/// The ordinary actions anchor sharing this resource's charter and sidecar.
pub fn actions_anchor(path: &Path) -> Option<PathBuf> {
    let (stem, _) = classify_action_file(path)?;
    Some(path.with_file_name(format!("{stem}.actions")))
}

/// Storage role at the workspace data root (never at an arbitrary ancestor).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceRole {
    Ordinary,
    Archive,
    Template,
}

/// Takes a workspace-root-relative path. Nested charter directory names are ordinary.
pub fn resource_role(relative: &Path) -> ResourceRole {
    match relative
        .components()
        .next()
        .and_then(|part| part.as_os_str().to_str())
    {
        Some("archive") => ResourceRole::Archive,
        Some("templates") => ResourceRole::Template,
        _ => ResourceRole::Ordinary,
    }
}

/// Whether a workspace-root-relative resource belongs to a live charter.
pub fn is_charter_action_file(path: &Path) -> bool {
    matches!(
        classify_action_file(path),
        Some((_, ActionFileRole::Actions | ActionFileRole::Habits))
    ) && resource_role(path) == ResourceRole::Ordinary
}

/// Whether a workspace-root-relative path holds executable live Actions.
pub fn is_live_actions(path: &Path) -> bool {
    matches!(
        classify_action_file(path),
        Some((_, ActionFileRole::Actions))
    ) && resource_role(path) == ResourceRole::Ordinary
}

/// An executable Action/history resource, including archives but not templates.
pub fn is_action_history(path: &Path) -> bool {
    matches!(
        classify_action_file(path),
        Some((_, ActionFileRole::Actions | ActionFileRole::Completed))
    ) && resource_role(path) != ResourceRole::Template
}

/// Reject attempts to lower Habit/template data into an executable mutation.
pub fn require_action_resource(path: &Path) -> Result<(), String> {
    if matches!(
        classify_action_file(path),
        Some((_, ActionFileRole::Habits | ActionFileRole::Upcoming))
    ) || resource_role(path) == ResourceRole::Template
    {
        Err("Habit definitions and template data are not executable Actions".into())
    } else {
        Ok(())
    }
}

/// A parsed `.actions` file — the workspace-layer representation of a charter's actions.
///
/// Carries the file path and a [`SourcedAction`] per action, each with its file
/// origin and (when parsed from a live document) LSP source positions.
/// Convert to domain [`Action`]s via `.into_actions()` at the workspace boundary.
#[derive(Debug, Clone)]
pub struct ActionsFile {
    pub path: PathBuf,
    pub actions: Vec<SourcedAction>,
}

impl ActionsFile {
    /// Strip file-layer metadata, yielding plain domain [`Action`]s.
    pub fn into_actions(self) -> ActionList {
        self.actions.into_iter().map(|sa| sa.action).collect()
    }
}

// ============================================================================
// Path derivation
// ============================================================================

/// Derive the charter stem from an actions file path.
///
/// When the filename is a primary file (`next.actions`) inside a subdirectory,
/// uses the directory name as the stem — matching how charter names are
/// inferred. The root anchor itself (`charters/next.actions`) is the one
/// exception: its parent is the literal workspace container `charters/`, not
/// a charter directory, so it keeps the reserved `next` stem regardless of
/// project or user scope.
///
/// - `health.actions`                                → `health`
/// - `next.actions`                                  → `next`
/// - `build_clearhead/next.actions`                  → `build_clearhead`
/// - `/repo/.clearhead/charters/next.actions`        → `next`
/// - `/data/clearhead/charters/next.actions`         → `next`
/// - `build_clearhead/obs.actions`                   → `obs`
pub(crate) fn charter_stem(actions_path: &Path) -> String {
    let filename = actions_path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("");

    // Primary file (next.actions) inside a subdirectory → use parent dir name,
    // except for the root anchor, whose parent is the literal workspace
    // container `charters/`.
    if classify_action_file(actions_path).is_some_and(|(stem, _)| stem == "next")
        && let Some(parent) = actions_path.parent()
        && let Some(dir_name) = parent.file_name().and_then(|s| s.to_str())
        && dir_name != "charters"
        && !dir_name.is_empty()
    {
        return dir_name.to_string();
    }

    classify_action_file(actions_path)
        .map(|(stem, _)| stem)
        .unwrap_or(filename)
        .to_string()
}

/// Derive the completed actions path for a `.actions` file.
///
/// - `health.actions`               → `health.completed.actions`
/// - `next.actions`                 → `next.completed.actions`
/// - `build_clearhead/next.actions` → `build_clearhead/build_clearhead.completed.actions`
/// - `build_clearhead/obs.actions`  → `build_clearhead/obs.completed.actions`
pub fn completed_actions_path(actions_path: &Path) -> PathBuf {
    let stem = charter_stem(actions_path);
    let dir = actions_path.parent().unwrap_or(Path::new(""));
    dir.join(format!("{}.completed.actions", stem))
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Path derivation
    // ========================================================================

    #[test]
    fn test_completed_actions_path() {
        assert_eq!(
            completed_actions_path(Path::new("/data/health.actions")),
            PathBuf::from("/data/health.completed.actions")
        );
        assert_eq!(
            completed_actions_path(Path::new("inbox.actions")),
            PathBuf::from("inbox.completed.actions")
        );
        assert_eq!(
            completed_actions_path(Path::new("build_clearhead/next.actions")),
            PathBuf::from("build_clearhead/build_clearhead.completed.actions")
        );
        assert_eq!(
            completed_actions_path(Path::new("/repo/.clearhead/charters/next.actions")),
            PathBuf::from("/repo/.clearhead/charters/next.completed.actions")
        );
        assert_eq!(
            completed_actions_path(Path::new("/data/clearhead/charters/next.actions")),
            PathBuf::from("/data/clearhead/charters/next.completed.actions")
        );
        assert_eq!(
            completed_actions_path(Path::new("build_clearhead/obs.actions")),
            PathBuf::from("build_clearhead/obs.completed.actions")
        );
    }
}
