//! Native sidecar reads and writes around Core's pure metadata policy.

use std::path::Path;

use chrono::Local;
use clearhead_core::workspace::{
    Action, CharterMetadata, WorkspaceError, parse_sidecar, record_charter_id, render_sidecar,
    sidecar_path, stamp_metadata_entries,
};

pub fn read_sidecar(path: &Path) -> Result<CharterMetadata, WorkspaceError> {
    let content = match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Ok(CharterMetadata::default());
        }
        Err(error) => return Err(error.into()),
    };
    parse_sidecar(&content)
}

pub fn write_sidecar(path: &Path, metadata: &CharterMetadata) -> Result<(), WorkspaceError> {
    let content = render_sidecar(metadata)?;
    crate::filesystem::durability::atomic_write(path, content.as_bytes())?;
    Ok(())
}

/// Append to the owning entity's sidecar, including completed or archived
/// owners. The caller supplies the same actions anchor used by other sidecar
/// mutations; no entity state is used to gate a log entry.
///
/// Lock a stable companion rather than the replaced JSON inode. Dropping the
/// file releases the OS lock even after an error or process termination.
pub fn append_note(
    actions_path: &Path,
    entity: uuid::Uuid,
    note: clearhead_core::workspace::sidecar::Note,
) -> Result<(), WorkspaceError> {
    let path = sidecar_path(actions_path);
    let _lock = lock_sidecar(&path)?;
    let mut metadata = read_sidecar(&path)?;
    clearhead_core::workspace::sidecar::append_note(&mut metadata, entity, note);
    write_sidecar(&path, &metadata)
}

pub(crate) fn lock_sidecar(path: &Path) -> Result<std::fs::File, WorkspaceError> {
    let lock_path = path.with_file_name(".clearhead.lock");
    let lock = std::fs::OpenOptions::new()
        .create(true)
        .truncate(false)
        .read(true)
        .write(true)
        .open(lock_path)?;
    lock.lock()?;
    Ok(lock)
}

pub fn stamp_sidecar_entries(
    actions_path: &Path,
    actions: &[Action],
) -> Result<(), WorkspaceError> {
    let path = sidecar_path(actions_path);
    let mut metadata = read_sidecar(&path)?;
    stamp_metadata_entries(&mut metadata, actions, Local::now());
    write_sidecar(&path, &metadata)
}

pub fn stamp_charter_id(actions_path: &Path, id: uuid::Uuid) -> Result<(), WorkspaceError> {
    let path = sidecar_path(actions_path);
    let mut metadata = read_sidecar(&path)?;
    if record_charter_id(&mut metadata, id) {
        write_sidecar(&path, &metadata)?;
    }
    Ok(())
}
