//! Native `.actions` file wrappers around Core's pure codec.

use std::path::{Path, PathBuf};

use clearhead_core::workspace::actions::format::{OutputFormat, format};
use clearhead_core::workspace::{Action, ActionList, ActionsFile, SourcedAction, WorkspaceError};

/// Classify relative to the caller's known workspace, never ancestor names.
/// Explicit standalone files outside that workspace have suffix semantics only.
pub fn role_path(root: &Path, path: &Path) -> PathBuf {
    let absolute = |path: &Path| {
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir().unwrap_or_default().join(path)
        }
    };
    absolute(path)
        .strip_prefix(absolute(&super::workspace_data_root(root)))
        .map(Path::to_path_buf)
        .unwrap_or_else(|_| PathBuf::from(path.file_name().unwrap_or_default()))
}

pub fn read_actions_in(root: &Path, path: &Path) -> Result<ActionList, WorkspaceError> {
    clearhead_core::workspace::action_files::require_action_resource(&role_path(root, path))
        .map_err(WorkspaceError::Actions)?;
    read_actions(path)
}

/// Read a standalone file using suffix semantics; workspace callers use read_actions_in.
pub fn read_actions(path: &Path) -> Result<ActionList, WorkspaceError> {
    clearhead_core::workspace::action_files::require_action_resource(Path::new(
        path.file_name().unwrap_or_default(),
    ))
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

pub fn write_actions_in(
    root: &Path,
    actions: &[Action],
    path: &Path,
) -> Result<(), WorkspaceError> {
    clearhead_core::workspace::action_files::require_action_resource(&role_path(root, path))
        .map_err(WorkspaceError::Actions)?;
    write_actions(actions, path)
}

pub fn write_actions(actions: &[Action], path: &Path) -> Result<(), WorkspaceError> {
    clearhead_core::workspace::action_files::require_action_resource(Path::new(
        path.file_name().unwrap_or_default(),
    ))
    .map_err(WorkspaceError::Actions)?;
    let content = format(&actions.to_vec(), OutputFormat::Actions, None, None)
        .map_err(WorkspaceError::Actions)?;
    crate::filesystem::durability::atomic_write(path, content.as_bytes())?;
    Ok(())
}

pub fn completed_actions_path(actions_path: &Path) -> PathBuf {
    clearhead_core::completed_actions_path(actions_path)
}
