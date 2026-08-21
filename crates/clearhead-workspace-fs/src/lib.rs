//! Native filesystem delivery for host-neutral Core workspace mutations.

use std::collections::HashSet;
use std::path::Path;

use chrono::Local;
pub use clearhead_core::TransactionOutcome;
use clearhead_core::charter_root;
use clearhead_core::workspace::durability::{PendingBatch, WorkspaceLock, recover_pending};
use clearhead_core::workspace::resource::{
    DeliveryError, Effect, ExpectedResource, ResourceConflict, ResourceRevision, ResourceSnapshot,
    WorkspacePath,
};
use clearhead_core::workspace::{
    FileState, PreparedTransactionOutcome, TransactionModel, TransactionRequest, WorkspaceError,
    completed_actions_path, list_action_files, normalize_request, parse_actions,
    prepare_transaction, workspace_data_root,
};

/// Execute one ordered transaction while holding the native workspace lock
/// across recovery, snapshot reads, pure preparation, validation, and commit.
pub fn transact(
    workspace_root: &Path,
    request: TransactionRequest,
    dry_run: bool,
) -> Result<TransactionOutcome, WorkspaceError> {
    let operations =
        normalize_request(request).map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let target_ids: HashSet<_> = operations
        .iter()
        .map(|operation| operation.target())
        .collect();
    let data_root = workspace_data_root(workspace_root);
    let journal_dir = charter_root(workspace_root);
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(data_root.clone()))?;
    recover_pending(&journal_dir)?;

    let model = load_target_files(workspace_root, &data_root, &target_ids)?;
    let prepared = prepare_transaction(model, &operations, Local::now(), dry_run)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;

    if !prepared.effects().is_empty() {
        validate_preconditions(&data_root, prepared.effects().preconditions())?;
        execute_effects(&data_root, &journal_dir, prepared.effects().effects())?;
    }

    let applied = prepared
        .adopt::<String>(Ok(()))
        .expect("successful native delivery releases prepared state");
    Ok(map_outcome(&data_root, applied.outcome))
}

fn load_target_files(
    workspace_root: &Path,
    data_root: &Path,
    target_ids: &HashSet<uuid::Uuid>,
) -> Result<TransactionModel, WorkspaceError> {
    let mut files = Vec::new();
    for active_path in list_action_files(workspace_root)? {
        let completed_path = completed_actions_path(&active_path);
        let (active_snapshot, active_expected) = snapshot(data_root, &active_path)?;
        let (completed_snapshot, completed_expected) = snapshot(data_root, &completed_path)?;
        let active = parse_snapshot(&active_snapshot)?;
        let completed = parse_snapshot(&completed_snapshot)?;
        if active
            .iter()
            .chain(completed.iter())
            .any(|action| target_ids.contains(&action.id))
        {
            files.push(FileState::new(
                active_snapshot.path().clone(),
                completed_snapshot.path().clone(),
                active,
                completed,
                active_expected,
                completed_expected,
            ));
        }
    }
    Ok(TransactionModel::new(files))
}

fn snapshot(
    data_root: &Path,
    path: &Path,
) -> Result<(ResourceSnapshot, ExpectedResource), WorkspaceError> {
    let relative = path
        .strip_prefix(data_root)
        .map_err(|_| WorkspaceError::InvalidPath(path.to_path_buf()))?;
    let logical = logical_path(relative)?;
    let (bytes, expected) = match std::fs::read(path) {
        Ok(bytes) => {
            let expected = ExpectedResource::Revision(revision(&bytes));
            (bytes, expected)
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            (Vec::new(), ExpectedResource::Missing)
        }
        Err(error) => return Err(error.into()),
    };
    let snapshot_revision = match &expected {
        ExpectedResource::Revision(revision) => revision.clone(),
        ExpectedResource::Missing => revision(&bytes),
    };
    Ok((
        ResourceSnapshot::new(logical, bytes, snapshot_revision),
        expected,
    ))
}

fn parse_snapshot(
    snapshot: &ResourceSnapshot,
) -> Result<Vec<clearhead_core::Action>, WorkspaceError> {
    if snapshot.bytes().is_empty() {
        return Ok(Vec::new());
    }
    let source = std::str::from_utf8(snapshot.bytes())
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    parse_actions(source).map_err(WorkspaceError::Actions)
}

fn revision(bytes: &[u8]) -> ResourceRevision {
    ResourceRevision::new(blake3::hash(bytes).to_hex().to_string())
}

fn logical_path(path: &Path) -> Result<WorkspacePath, WorkspaceError> {
    let logical = path
        .components()
        .map(|part| part.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| WorkspaceError::InvalidPath(path.to_path_buf()))?
        .join("/");
    WorkspacePath::new(logical).map_err(|_| WorkspaceError::InvalidPath(path.to_path_buf()))
}

fn validate_preconditions(
    data_root: &Path,
    preconditions: &[clearhead_core::workspace::resource::ResourcePrecondition],
) -> Result<(), WorkspaceError> {
    for precondition in preconditions {
        let path = data_root.join(precondition.path.as_str());
        let actual = match std::fs::read(&path) {
            Ok(bytes) => Some(revision(&bytes)),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error.into()),
        };
        let matches = match &precondition.expected {
            ExpectedResource::Missing => actual.is_none(),
            ExpectedResource::Revision(expected) => actual.as_ref() == Some(expected),
        };
        if !matches {
            let conflict = ResourceConflict {
                path: precondition.path.clone(),
                expected: precondition.expected.clone(),
                actual,
            };
            let error: DeliveryError<String> = DeliveryError::Conflict { conflict };
            return Err(WorkspaceError::Actions(error.to_string()));
        }
    }
    Ok(())
}

fn execute_effects(
    data_root: &Path,
    journal_dir: &Path,
    effects: &[Effect],
) -> Result<(), WorkspaceError> {
    let mut batch = PendingBatch::new(journal_dir.to_path_buf());
    for effect in effects {
        match effect {
            Effect::Write { path, bytes } => batch.stage(data_root.join(path.as_str()), bytes)?,
            Effect::Move { .. } | Effect::Remove { .. } => {
                return Err(WorkspaceError::Actions(
                    "transaction preparation emitted an unsupported non-write effect".into(),
                ));
            }
        }
    }
    batch.commit()?;
    Ok(())
}

fn map_outcome(data_root: &Path, outcome: PreparedTransactionOutcome) -> TransactionOutcome {
    let absolute = |files: Vec<WorkspacePath>| {
        files
            .into_iter()
            .map(|path| data_root.join(path.as_str()).display().to_string())
            .collect()
    };
    match outcome {
        PreparedTransactionOutcome::Committed { operations, files } => {
            TransactionOutcome::Committed {
                operations,
                files: absolute(files),
            }
        }
        PreparedTransactionOutcome::DryRun { operations, files } => TransactionOutcome::DryRun {
            operations,
            files: absolute(files),
        },
        PreparedTransactionOutcome::Rejected { operation, error } => {
            TransactionOutcome::Rejected { operation, error }
        }
    }
}
