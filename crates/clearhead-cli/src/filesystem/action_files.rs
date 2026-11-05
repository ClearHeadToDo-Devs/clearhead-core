//! Native `.actions` file wrappers around Core's pure codec.

use std::path::{Path, PathBuf};

use clearhead_core::workspace::actions::format::{OutputFormat, format};
use clearhead_core::workspace::{Action, ActionList, ActionsFile, SourcedAction, WorkspaceError};

/// Resolve a native path against its nearest workspace data root before Core classification.
/// Standalone files have suffix semantics only; ancestor directory names are not roles.
pub fn role_path(path: &Path) -> PathBuf {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir().unwrap_or_default().join(path)
    };
    for ancestor in absolute.ancestors().skip(1) {
        if ancestor
            .file_name()
            .is_some_and(|name| name == ".clearhead")
            || ancestor.join("workspace.json").is_file()
            || ancestor.join("charters").is_dir()
        {
            return absolute.strip_prefix(ancestor).unwrap().to_path_buf();
        }
    }
    PathBuf::from(path.file_name().unwrap_or_default())
}

pub fn read_actions(path: &Path) -> Result<ActionList, WorkspaceError> {
    clearhead_core::workspace::action_files::require_action_resource(&role_path(path))
        .map_err(WorkspaceError::Actions)?;
    let content = match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(error) => return Err(error.into()),
    };
    clearhead_core::parse_actions(&content).map_err(WorkspaceError::Actions)
}

pub fn read_action_file(path: &Path) -> Result<ActionsFile, WorkspaceError> {
    let actions = read_actions(path)?;
    Ok(ActionsFile {
        path: path.to_path_buf(),
        actions: actions
            .into_iter()
            .map(|action| SourcedAction {
                action,
                source_metadata: None,
            })
            .collect(),
    })
}

pub fn write_actions(actions: &[Action], path: &Path) -> Result<(), WorkspaceError> {
    clearhead_core::workspace::action_files::require_action_resource(&role_path(path))
        .map_err(WorkspaceError::Actions)?;
    let content = format(&actions.to_vec(), OutputFormat::Actions, None, None)
        .map_err(WorkspaceError::Actions)?;
    crate::filesystem::durability::atomic_write(path, content.as_bytes())?;
    Ok(())
}

pub fn completed_actions_path(actions_path: &Path) -> PathBuf {
    clearhead_core::completed_actions_path(actions_path)
}
