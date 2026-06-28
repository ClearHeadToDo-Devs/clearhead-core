//! Per-charter `.actions` / `.completed.actions` / `.upcoming.actions` read/write.
//!
//! Each charter's actions are stored across three DSL files:
//! - `<charter>.actions`           — active actions (within primary instance cap)
//! - `<charter>.upcoming.actions`  — future generated instances beyond the primary cap
//! - `<charter>.completed.actions` — completed/cancelled actions
//!
//! Charter stem derivation mostly uses the file stem. Primary files like
//! `subdir/next.actions` use the directory name, and project-root
//! `.clearhead/charters/next.actions` uses the project directory name rather
//! than the literal `charters` container. Unlike charter name inference,
//! `inbox` is NOT skipped — `inbox.actions` is valid.

use std::path::{Path, PathBuf};

use crate::workspace::actions::format::{OutputFormat, format};
use crate::workspace::actions::repository::{ActionSource, SourcedAction};
use crate::workspace::actions::{Action, ActionList, parse_actions};
use crate::workspace::store::{WorkspaceError, infer_charter_name};

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
/// uses the directory name as the stem — matching how charter names are inferred.
/// For project-root paths like `/repo/.clearhead/charters/next.actions`, the stem
/// is the project directory name rather than the literal directory `charters`.
///
/// - `health.actions`                                → `health`
/// - `next.actions`                                  → `next`
/// - `build_clearhead/next.actions`                  → `build_clearhead`
/// - `/repo/.clearhead/charters/next.actions`        → `repo`
/// - `/data/clearhead/charters/next.actions`         → `next`
/// - `build_clearhead/obs.actions`                   → `obs`
pub(crate) fn charter_stem(actions_path: &Path) -> String {
    let filename = actions_path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("");

    // Primary file (next.actions) inside a subdirectory → use parent dir name,
    // except for the root charter in project layout where the parent directory
    // is the literal workspace container `charters/`.
    if filename == "next.actions"
        && let Some(parent) = actions_path.parent()
        && let Some(dir_name) = parent.file_name().and_then(|s| s.to_str())
    {
        if dir_name != "charters" && !dir_name.is_empty() {
            return dir_name.to_string();
        }

        // Project layout: <project>/.clearhead/charters/next.actions → <project>
        let is_project_layout = parent
            .parent()
            .and_then(|p| p.file_name())
            .and_then(|s| s.to_str())
            == Some(".clearhead");
        if is_project_layout
            && let Some(project_name) = parent
                .parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.file_name())
                .and_then(|s| s.to_str())
            && !project_name.is_empty()
        {
            return project_name.to_string();
        }
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

/// Derive the upcoming actions path for a `.actions` file.
///
/// - `health.actions`               → `health.upcoming.actions`
/// - `next.actions`                 → `next.upcoming.actions`
/// - `build_clearhead/next.actions` → `build_clearhead/build_clearhead.upcoming.actions`
/// - `build_clearhead/obs.actions`  → `build_clearhead/obs.upcoming.actions`
pub fn upcoming_actions_path(actions_path: &Path) -> PathBuf {
    let stem = charter_stem(actions_path);
    let dir = actions_path.parent().unwrap_or(Path::new(""));
    dir.join(format!("{}.upcoming.actions", stem))
}

// ============================================================================
// Public API
// ============================================================================

/// Read [`Action`]s from a `.actions` or `.completed.actions` file.
///
/// Returns `Ok(vec![])` if the file is absent. Syntax errors are returned as
/// an error per the parse boundary contract.
pub fn read_actions(path: &Path) -> Result<ActionList, WorkspaceError> {
    if !path.exists() {
        return Ok(ActionList::new());
    }

    let content = std::fs::read_to_string(path).map_err(WorkspaceError::Io)?;
    parse_actions(&content).map_err(|e| WorkspaceError::Acts(e))
}

/// Read a `.actions` file into an [`ActionsFile`], preserving file origin on each action.
///
/// `workspace_root` is used to infer the charter name from the relative path.
/// Returns `Ok(ActionsFile { path, actions: [] })` if the file is absent.
pub fn read_action_file(path: &Path, workspace_root: &Path) -> Result<ActionsFile, WorkspaceError> {
    let actions = read_actions(path)?;
    let relative = path.strip_prefix(workspace_root).unwrap_or(path);
    let project = infer_charter_name(relative);
    let sourced = actions
        .into_iter()
        .map(|action| SourcedAction {
            action,
            source: ActionSource {
                file_path: relative.to_path_buf(),
                project: project.clone(),
            },
            source_metadata: None,
        })
        .collect();
    Ok(ActionsFile { path: path.to_path_buf(), actions: sourced })
}

/// Write [`Action`]s to a `.actions` or `.completed.actions` file.
///
/// Writes atomically: temp file in the same directory, fsync, rename.
pub fn write_actions(actions: &[Action], path: &Path) -> Result<(), WorkspaceError> {
    let list: ActionList = actions.to_vec();
    let content =
        format(&list, OutputFormat::Actions, None, None).map_err(|e| WorkspaceError::Acts(e))?;
    super::durability::atomic_write(path, content.as_bytes()).map_err(WorkspaceError::Io)
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
            PathBuf::from("/repo/.clearhead/charters/repo.completed.actions")
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

    #[test]
    fn test_upcoming_actions_path() {
        assert_eq!(
            upcoming_actions_path(Path::new("/data/health.actions")),
            PathBuf::from("/data/health.upcoming.actions")
        );
        assert_eq!(
            upcoming_actions_path(Path::new("inbox.actions")),
            PathBuf::from("inbox.upcoming.actions")
        );
        assert_eq!(
            upcoming_actions_path(Path::new("build_clearhead/next.actions")),
            PathBuf::from("build_clearhead/build_clearhead.upcoming.actions")
        );
        assert_eq!(
            upcoming_actions_path(Path::new("/repo/.clearhead/charters/next.actions")),
            PathBuf::from("/repo/.clearhead/charters/repo.upcoming.actions")
        );
        assert_eq!(
            upcoming_actions_path(Path::new("/data/clearhead/charters/next.actions")),
            PathBuf::from("/data/clearhead/charters/next.upcoming.actions")
        );
        assert_eq!(
            upcoming_actions_path(Path::new("build_clearhead/obs.actions")),
            PathBuf::from("build_clearhead/obs.upcoming.actions")
        );
    }

    // ========================================================================
    // Round-trip I/O
    // ========================================================================

    #[test]
    fn test_missing_file_returns_empty() {
        let result = read_actions(Path::new("/nonexistent/path.actions"));
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_roundtrip() {
        use crate::workspace::actions::{Action, ActionState};

        let action = Action {
            name: "Write tests".to_string(),
            state: ActionState::NotStarted,
            ..Default::default()
        };

        let acts = vec![action.clone()];
        let tmp = tempfile::tempdir().expect("tempdir");
        let path = tmp.path().join("health.actions");

        write_actions(&acts, &path).expect("write");
        let loaded = read_actions(&path).expect("read");

        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].id, action.id);
        assert_eq!(loaded[0].name, action.name);
        assert_eq!(loaded[0].state, ActionState::NotStarted);
    }

    #[test]
    fn test_completed_roundtrip() {
        use crate::workspace::actions::{Action, ActionState};
        use chrono::Local;

        let action = Action {
            name: "Finished task".to_string(),
            state: ActionState::Completed,
            completed_at: Some(Local::now()),
            ..Default::default()
        };

        let tmp = tempfile::tempdir().expect("tempdir");
        let path = tmp.path().join("health.completed.actions");

        write_actions(&[action.clone()], &path).expect("write");
        let loaded = read_actions(&path).expect("read");

        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].state, ActionState::Completed);
    }
}
