//! Native filesystem delivery for host-neutral Core workspace mutations.

pub mod action_files;
pub mod archive_charter;
pub mod calendar;
pub mod discovery;
pub mod doctor;
pub mod durability;
pub mod manifest;
pub mod mounts;
pub mod sidecar;
pub mod templates;
pub use action_files::{read_action_file, read_actions, write_actions};
pub use archive_charter::{
    ArchiveCharterError, ArchiveCharterOptions, ArchiveCharterResult, archive_charter,
    archive_terminal_charters, find_charter as find_markdown_charter,
};
pub use calendar::{
    CalendarObservation, CalendarResource, CalendarSyncResult, apply_occurrence_op,
    delete_plan_file, observe_calendar_resources, plans_sync_store_path, read_calendar_resources,
    read_ics_file, read_plans_sync_store, read_vtodo_actions, read_vtodo_file,
    resolve_materialized_occurrence, sync_calendar, sync_master_rollforwards, write_plan_file,
};
pub use discovery::{ManifestSourceType, WorkspaceManifestEntry, collect_workspace_manifest};
pub use doctor::{
    apply_doctor_repairs, diagnose_workspace, diagnose_workspace_read, observe_doctor,
};
pub use manifest::{read_workspace_manifest, workspace_manifest_path, write_workspace_manifest};
pub use mounts::{
    NativeWorkspaceMounts, charter_root, list_action_files, load_domain_model, load_workspace,
    load_workspace_model, plans_root, project_root_charter, read_workspace, workspace_data_root,
};

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::durability::{PendingBatch, WorkspaceLock, recover_pending};
use chrono::Local;
pub use clearhead_core::TransactionOutcome;
use clearhead_core::domain::update::ActionUpdate;
use clearhead_core::workspace::resource::PreparedMutation;
use clearhead_core::workspace::resource::{
    DeliveryError, Effect, ExpectedResource, ResourceConflict, ResourceRevision, ResourceSnapshot,
    WorkspacePath,
};
use clearhead_core::workspace::sidecar::CharterMetadata;
use clearhead_core::workspace::{
    ActionResourceState, FileState, PreparedArchiveOutcome, PreparedCloseOutcome,
    PreparedDeleteOutcome, PreparedInsertOutcome, PreparedTransactionOutcome,
    PreparedUpdateOutcome, SidecarResourceState, TransactionModel, TransactionRequest,
    WorkspaceError, completed_actions_path, normalize_request, parse_actions,
    prepare_action_archive, prepare_action_delete, prepare_action_insert, prepare_action_update,
    prepare_close_action_subtree, prepare_transaction, sidecar_path,
};
use clearhead_core::{Action, ActionSelector};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InsertActionResult {
    pub action_id: uuid::Uuid,
    pub parent_id: Option<uuid::Uuid>,
    pub source_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UpdateActionResult {
    pub action_id: uuid::Uuid,
    pub source_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeleteActionResult {
    pub action_id: uuid::Uuid,
    pub deleted_count: usize,
    pub source_path: PathBuf,
    pub from_completed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionArchiveResult {
    pub archived_count: usize,
    pub source_path: PathBuf,
    pub completed_path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CloseActionResult {
    pub action_id: uuid::Uuid,
    pub closed_count: usize,
    pub source_path: PathBuf,
    pub completed_path: PathBuf,
    pub already_closed: bool,
}

pub fn insert_action(
    workspace_root: &Path,
    source_path: &Path,
    new_action: Action,
    parent: Option<&ActionSelector>,
) -> Result<InsertActionResult, WorkspaceError> {
    let (mounts, journal_dir, _lock) = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let (snapshot, expected) = snapshot(data_root, source_path)?;
    let source = ActionResourceState {
        path: snapshot.path().clone(),
        actions: parse_snapshot(&snapshot)?,
        expected,
    };
    let prepared = prepare_action_insert(source, new_action, parent)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let outcome = deliver(&mounts, &journal_dir, prepared)?;
    Ok(map_insert(data_root, outcome))
}

pub fn update_action(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
    update: ActionUpdate,
) -> Result<UpdateActionResult, WorkspaceError> {
    let (mounts, journal_dir, _lock) = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let (snapshot, expected) = snapshot(data_root, source_path)?;
    let source = ActionResourceState {
        path: snapshot.path().clone(),
        actions: parse_snapshot(&snapshot)?,
        expected,
    };
    let prepared = prepare_action_update(source, selector, update)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let outcome = deliver(&mounts, &journal_dir, prepared)?;
    Ok(map_update(data_root, outcome))
}

pub fn delete_action(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
) -> Result<DeleteActionResult, WorkspaceError> {
    let (mounts, journal_dir, _lock) = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let completed_path = completed_actions_path(source_path);
    let active_sidecar_path = sidecar_path(source_path);
    let completed_sidecar_path = sidecar_path(&completed_path);
    let (active_snapshot, active_expected) = snapshot(data_root, source_path)?;
    let (completed_snapshot, completed_expected) = snapshot(data_root, &completed_path)?;
    let (active_sidecar_snapshot, active_sidecar_expected) =
        snapshot(data_root, &active_sidecar_path)?;
    let (completed_sidecar_snapshot, completed_sidecar_expected) =
        snapshot(data_root, &completed_sidecar_path)?;
    let prepared = prepare_action_delete(
        ActionResourceState {
            path: active_snapshot.path().clone(),
            actions: parse_snapshot(&active_snapshot)?,
            expected: active_expected,
        },
        ActionResourceState {
            path: completed_snapshot.path().clone(),
            actions: parse_snapshot(&completed_snapshot)?,
            expected: completed_expected,
        },
        SidecarResourceState {
            path: active_sidecar_snapshot.path().clone(),
            metadata: parse_sidecar_snapshot(&active_sidecar_snapshot)?,
            expected: active_sidecar_expected,
        },
        SidecarResourceState {
            path: completed_sidecar_snapshot.path().clone(),
            metadata: parse_sidecar_snapshot(&completed_sidecar_snapshot)?,
            expected: completed_sidecar_expected,
        },
        selector,
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let outcome = deliver(&mounts, &journal_dir, prepared)?;
    Ok(map_delete(data_root, outcome))
}

pub fn archive_actions(
    workspace_root: &Path,
    source_path: &Path,
) -> Result<ActionArchiveResult, WorkspaceError> {
    let (mounts, journal_dir, _lock) = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let completed_path = completed_actions_path(source_path);
    let (active_snapshot, active_expected) = snapshot(data_root, source_path)?;
    let (completed_snapshot, completed_expected) = snapshot(data_root, &completed_path)?;
    let prepared = prepare_action_archive(
        action_state(active_snapshot, active_expected)?,
        action_state(completed_snapshot, completed_expected)?,
        Local::now(),
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let outcome = deliver(&mounts, &journal_dir, prepared)?;
    Ok(map_archive(data_root, outcome))
}

pub fn close_action_subtree(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
    closing_state: clearhead_core::ActionState,
    completed_at: chrono::DateTime<Local>,
) -> Result<CloseActionResult, WorkspaceError> {
    let (mounts, journal_dir, _lock) = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let completed_path = completed_actions_path(source_path);
    let (active_snapshot, active_expected) = snapshot(data_root, source_path)?;
    let (completed_snapshot, completed_expected) = snapshot(data_root, &completed_path)?;
    let prepared = prepare_close_action_subtree(
        action_state(active_snapshot, active_expected)?,
        action_state(completed_snapshot, completed_expected)?,
        selector,
        closing_state,
        completed_at,
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let outcome = deliver(&mounts, &journal_dir, prepared)?;
    Ok(map_close(data_root, outcome))
}

fn action_state(
    snapshot: ResourceSnapshot,
    expected: ExpectedResource,
) -> Result<ActionResourceState, WorkspaceError> {
    let actions = parse_snapshot(&snapshot)?;
    Ok(ActionResourceState {
        path: snapshot.path().clone(),
        actions,
        expected,
    })
}

fn begin_mutation(
    workspace_root: &Path,
    source_path: &Path,
) -> Result<(NativeWorkspaceMounts, PathBuf, WorkspaceLock), WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, None);
    let journal_dir = mounts.workspace.join("charters");
    validate_source_path(source_path, &journal_dir)?;
    std::fs::create_dir_all(&journal_dir)?;
    let lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;
    Ok((mounts, journal_dir, lock))
}

fn validate_source_path(source_path: &Path, charter_root: &Path) -> Result<(), WorkspaceError> {
    let valid_location = source_path
        .canonicalize()
        .ok()
        .zip(charter_root.canonicalize().ok())
        .is_some_and(|(source, root)| source.starts_with(root));
    let valid_name = source_path
        .file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with(".actions") && !name.ends_with(".completed.actions"));
    if valid_location && valid_name {
        Ok(())
    } else {
        Err(WorkspaceError::InvalidPath(source_path.to_path_buf()))
    }
}

fn parse_sidecar_snapshot(snapshot: &ResourceSnapshot) -> Result<CharterMetadata, WorkspaceError> {
    if snapshot.bytes().is_empty() {
        return Ok(CharterMetadata::default());
    }
    serde_json::from_slice(snapshot.bytes())
        .map_err(|error| WorkspaceError::Parse(format!("sidecar: {error}")))
}

fn deliver<S, O>(
    mounts: &NativeWorkspaceMounts,
    journal_dir: &Path,
    prepared: PreparedMutation<S, O>,
) -> Result<O, WorkspaceError> {
    validate_preconditions(mounts, prepared.effects().preconditions())?;
    execute_effects(mounts, journal_dir, prepared.effects().effects())?;
    Ok(prepared
        .adopt::<String>(Ok(()))
        .expect("successful native delivery releases prepared state")
        .outcome)
}

fn map_archive(data_root: &Path, outcome: PreparedArchiveOutcome) -> ActionArchiveResult {
    ActionArchiveResult {
        archived_count: outcome.archived_count,
        source_path: data_root.join(outcome.source_path.as_str()),
        completed_path: data_root.join(outcome.completed_path.as_str()),
    }
}

fn map_close(data_root: &Path, outcome: PreparedCloseOutcome) -> CloseActionResult {
    CloseActionResult {
        action_id: outcome.action_id,
        closed_count: outcome.closed_count,
        source_path: data_root.join(outcome.source_path.as_str()),
        completed_path: data_root.join(outcome.completed_path.as_str()),
        already_closed: outcome.already_closed,
    }
}

fn map_insert(data_root: &Path, outcome: PreparedInsertOutcome) -> InsertActionResult {
    InsertActionResult {
        action_id: outcome.action_id,
        parent_id: outcome.parent_id,
        source_path: data_root.join(outcome.source_path.as_str()),
    }
}

fn map_update(data_root: &Path, outcome: PreparedUpdateOutcome) -> UpdateActionResult {
    UpdateActionResult {
        action_id: outcome.action_id,
        source_path: data_root.join(outcome.source_path.as_str()),
    }
}

fn map_delete(data_root: &Path, outcome: PreparedDeleteOutcome) -> DeleteActionResult {
    DeleteActionResult {
        action_id: outcome.action_id,
        deleted_count: outcome.deleted_count,
        source_path: data_root.join(outcome.source_path.as_str()),
        from_completed: outcome.from_completed,
    }
}

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
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, None);
    let data_root = &mounts.workspace;
    let journal_dir = data_root.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(data_root.clone()))?;
    recover_pending(&journal_dir)?;

    let model = load_target_files(workspace_root, data_root, &target_ids)?;
    let prepared = prepare_transaction(model, &operations, Local::now(), dry_run)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;

    if !prepared.effects().is_empty() {
        validate_preconditions(&mounts, prepared.effects().preconditions())?;
        execute_effects(&mounts, &journal_dir, prepared.effects().effects())?;
    }

    let applied = prepared
        .adopt::<String>(Ok(()))
        .expect("successful native delivery releases prepared state");
    Ok(map_outcome(data_root, applied.outcome))
}

fn load_target_files(
    workspace_root: &Path,
    data_root: &Path,
    target_ids: &HashSet<uuid::Uuid>,
) -> Result<TransactionModel, WorkspaceError> {
    let mut files = Vec::new();
    for active_path in crate::list_action_files(workspace_root)? {
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
    mounts::content_revision(bytes)
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
    mounts: &NativeWorkspaceMounts,
    preconditions: &[clearhead_core::workspace::resource::ResourcePrecondition],
) -> Result<(), WorkspaceError> {
    for precondition in preconditions {
        let path = mounts.physical_path(&precondition.path)?;
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
    mounts: &NativeWorkspaceMounts,
    journal_dir: &Path,
    effects: &[Effect],
) -> Result<(), WorkspaceError> {
    let mut batch = PendingBatch::new(journal_dir.to_path_buf());
    for effect in effects {
        match effect {
            Effect::Write { path, bytes } => batch.stage(mounts.physical_path(path)?, bytes)?,
            Effect::Move {
                source,
                destination,
            } => batch.stage_move(
                mounts.physical_path(source)?,
                mounts.physical_path(destination)?,
            )?,
            Effect::Remove { path } => batch.stage_remove(mounts.physical_path(path)?)?,
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

pub mod archive_facts;
pub mod config;
pub mod detection;
pub mod telemetry;
pub use archive_facts::read_archived_action_facts;
pub use detection::check_for_workspace;

#[cfg(test)]
mod mounted_effect_tests {
    use super::*;
    use clearhead_core::workspace::resource::{
        EffectBatch, MountId, ResourceLocation, ResourcePrecondition, WorkspaceScope,
    };

    fn location(mount: MountId, path: &str) -> ResourceLocation {
        ResourceLocation::new(mount, WorkspacePath::new(path).unwrap())
    }

    #[test]
    fn one_pending_batch_delivers_workspace_and_external_plan_writes() {
        let temp = tempfile::tempdir().unwrap();
        let workspace = temp.path().join("workspace");
        let external = temp.path().join("external-plans");
        let journal = workspace.join("charters");
        std::fs::create_dir_all(&journal).unwrap();
        std::fs::create_dir_all(&external).unwrap();
        let mounts = NativeWorkspaceMounts {
            workspace: workspace.clone(),
            external_plans: Some(external.clone()),
            scope: WorkspaceScope::User,
        };
        let workspace_location = location(MountId::Workspace, "sync/plans.json");
        let external_location = location(MountId::ExternalPlans, "inbox/action.ics");
        let batch = EffectBatch::new(
            vec![
                Effect::Write {
                    path: workspace_location.clone(),
                    bytes: b"store".to_vec(),
                },
                Effect::Write {
                    path: external_location.clone(),
                    bytes: b"calendar".to_vec(),
                },
            ],
            vec![
                ResourcePrecondition {
                    path: workspace_location,
                    expected: ExpectedResource::Missing,
                },
                ResourcePrecondition {
                    path: external_location,
                    expected: ExpectedResource::Missing,
                },
            ],
        )
        .unwrap();

        validate_preconditions(&mounts, batch.preconditions()).unwrap();
        execute_effects(&mounts, &journal, batch.effects()).unwrap();

        assert_eq!(
            std::fs::read(workspace.join("sync/plans.json")).unwrap(),
            b"store"
        );
        assert_eq!(
            std::fs::read(external.join("inbox/action.ics")).unwrap(),
            b"calendar"
        );
        assert!(!journal.join(".pending").exists());
    }
}
