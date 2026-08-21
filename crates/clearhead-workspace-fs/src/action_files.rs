//! Native `.actions` file wrappers around Core's pure codec.

use std::path::{Path, PathBuf};

use clearhead_core::workspace::actions::format::{OutputFormat, format};
use clearhead_core::workspace::{Action, ActionList, ActionsFile, SourcedAction, WorkspaceError};

pub fn read_actions(path: &Path) -> Result<ActionList, WorkspaceError> {
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
    let content = format(&actions.to_vec(), OutputFormat::Actions, None, None)
        .map_err(WorkspaceError::Actions)?;
    crate::durability::atomic_write(path, content.as_bytes())?;
    Ok(())
}

pub fn completed_actions_path(actions_path: &Path) -> PathBuf {
    clearhead_core::completed_actions_path(actions_path)
}
