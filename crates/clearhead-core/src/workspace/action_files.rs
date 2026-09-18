//! Per-charter `.actions` / `.completed.actions` reads.
//!
//! Each charter's actions are stored across two DSL files:
//! - `<charter>.actions`           — active actions (recurring occurrences are
//!   projected on read, never filed)
//! - `<charter>.completed.actions` — completed/cancelled actions
//!
//! Charter stem derivation mostly uses the file stem. Primary files like
//! `subdir/next.actions` use the directory name, except the root anchor
//! itself (`charters/next.actions`), which keeps the reserved `next` stem
//! regardless of project or user scope — every workspace's root shares it.
//! Unlike charter name inference, `inbox` is NOT skipped — `inbox.actions`
//! is valid.

use std::path::{Path, PathBuf};

use crate::workspace::actions::ActionList;
use crate::workspace::actions::repository::SourcedAction;

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
    if filename == crate::workspace::PRIMARY_ACTIONS_FILE
        && let Some(parent) = actions_path.parent()
        && let Some(dir_name) = parent.file_name().and_then(|s| s.to_str())
        && dir_name != "charters"
        && !dir_name.is_empty()
    {
        return dir_name.to_string();
    }

    actions_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
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
