//! Per-charter `.actions` / `.completed.actions` read/write for planned act persistence.
//!
//! Each charter's acts are stored as action DSL files alongside the `.ics` plan file:
//! - `<charter>.actions`           — upcoming/in-progress acts
//! - `<charter>.completed.actions` — completed/cancelled acts
//!
//! Charter stem derivation: `next.actions` uses the parent directory name;
//! all other `.actions` files use the file stem. Unlike charter name inference,
//! `inbox` is NOT skipped — `inbox.actions` is valid.

use std::path::{Path, PathBuf};

use crate::workspace::actions::format::{OutputFormat, format};
use crate::workspace::actions::{Action, ActionList, parse_actions};
use crate::workspace::store::WorkspaceError;

// ============================================================================
// Path derivation
// ============================================================================

pub(crate) fn charter_stem(actions_path: &Path) -> String {
    let filename = actions_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("");

    if filename == "next.actions" {
        if let Some(parent) = actions_path.parent() {
            if let Some(dir_name) = parent.file_name().and_then(|n| n.to_str()) {
                return dir_name.to_string();
            }
        }
    }

    actions_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string()
}

/// Derive the completed acts path for a `.actions` file.
///
/// - `health.actions`               → `health.completed.actions`
/// - `inbox.actions`                → `inbox.completed.actions`
/// - `build_clearhead/next.actions` → `build_clearhead/build_clearhead.completed.actions`
/// - `build_clearhead/obs.actions`  → `build_clearhead/obs.completed.actions`
pub fn completed_acts_path(actions_path: &Path) -> PathBuf {
    let stem = charter_stem(actions_path);
    let dir = actions_path.parent().unwrap_or(Path::new(""));
    dir.join(format!("{}.completed.actions", stem))
}

// ============================================================================
// Public API
// ============================================================================

/// Read [`Action`]s from a `.actions` or `.completed.actions` file.
///
/// Returns `Ok(vec![])` if the file is absent. Syntax errors are returned as
/// an error per the parse boundary contract.
pub fn read_acts(path: &Path) -> Result<ActionList, WorkspaceError> {
    if !path.exists() {
        return Ok(ActionList::new());
    }

    let content = std::fs::read_to_string(path).map_err(WorkspaceError::Io)?;
    parse_actions(&content).map_err(|e| WorkspaceError::Acts(e))
}

/// Write [`Action`]s to a `.actions` or `.completed.actions` file.
///
/// Creates parent directories as needed. Overwrites any existing file.
pub fn write_acts(acts: &[Action], path: &Path) -> Result<(), WorkspaceError> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent).map_err(WorkspaceError::Io)?;
        }
    }

    let list: ActionList = acts.to_vec();
    let content =
        format(&list, OutputFormat::Actions, None, None).map_err(|e| WorkspaceError::Acts(e))?;
    std::fs::write(path, content).map_err(WorkspaceError::Io)
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
    fn test_completed_acts_path() {
        assert_eq!(
            completed_acts_path(Path::new("/data/health.actions")),
            PathBuf::from("/data/health.completed.actions")
        );
        assert_eq!(
            completed_acts_path(Path::new("inbox.actions")),
            PathBuf::from("inbox.completed.actions")
        );
        assert_eq!(
            completed_acts_path(Path::new("build_clearhead/next.actions")),
            PathBuf::from("build_clearhead/build_clearhead.completed.actions")
        );
        assert_eq!(
            completed_acts_path(Path::new("build_clearhead/obs.actions")),
            PathBuf::from("build_clearhead/obs.completed.actions")
        );
    }

    // ========================================================================
    // Round-trip I/O
    // ========================================================================

    #[test]
    fn test_missing_file_returns_empty() {
        let result = read_acts(Path::new("/nonexistent/path.actions"));
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

        write_acts(&acts, &path).expect("write");
        let loaded = read_acts(&path).expect("read");

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
            completed_date_time: Some(Local::now()),
            ..Default::default()
        };

        let tmp = tempfile::tempdir().expect("tempdir");
        let path = tmp.path().join("health.completed.actions");

        write_acts(&[action.clone()], &path).expect("write");
        let loaded = read_acts(&path).expect("read");

        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].state, ActionState::Completed);
    }
}
