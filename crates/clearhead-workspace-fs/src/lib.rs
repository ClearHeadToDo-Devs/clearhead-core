//! Native filesystem delivery for host-neutral Core workspace mutations.

pub mod action_files;
pub mod archive_charter;
pub mod calendar;
pub mod discovery;
pub mod doctor;
pub mod durability;
pub mod init;
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
    COLLECTION_DISPLAYNAME_FILE, CalendarObservation, CalendarResource, CalendarSyncPreview,
    CalendarSyncResult, apply_occurrence_op, delete_plan_file, observe_calendar_resources,
    plans_sync_store_path, preview_calendar_sync_with_component, read_calendar_resources,
    read_ics_file, read_plans_sync_store, resolve_materialized_occurrence, sync_calendar,
    sync_calendar_with_component, sync_master_rollforwards, write_collection_displaynames,
    write_plan_file,
};
pub use discovery::{ManifestSourceType, WorkspaceManifestEntry, collect_workspace_manifest};
pub use doctor::{
    apply_doctor_repairs, diagnose_workspace, diagnose_workspace_read, observe_doctor,
};
pub use init::init_workspace;
pub use manifest::{read_workspace_manifest, workspace_manifest_path, write_workspace_manifest};
pub use mounts::{
    NativeWorkspaceMounts, charter_root, list_action_files, load_domain_model, load_workspace,
    load_workspace_model, plans_root, read_workspace, root_charter_name, workspace_data_root,
};

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::durability::atomic_write;
use chrono::Local;
pub use clearhead_core::TransactionOutcome;
use clearhead_core::domain::update::ActionUpdate;
use clearhead_core::workspace::resource::{
    Effect, EffectBatch, ExpectedResource, ResourceConflict, ResourceLocation,
    ResourcePrecondition, ResourceRevision, ResourceSnapshot, WorkspacePath,
};
use clearhead_core::workspace::sidecar::CharterMetadata;
use clearhead_core::workspace::{
    ActionResourceState, FileState, PreparedArchiveOutcome, PreparedCloseOutcome,
    PreparedDeleteOutcome, PreparedInsertOutcome, PreparedReopenOutcome,
    PreparedTransactionOutcome, PreparedUpdateOutcome, SidecarResourceState, TransactionModel,
    TransactionRequest, WorkspaceError, completed_actions_path, normalize_request, parse_actions,
    prepare_action_archive, prepare_action_delete, prepare_action_insert, prepare_action_update,
    prepare_close_action_subtree, prepare_reopen_action_subtree, prepare_transaction, sidecar_path,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReopenActionResult {
    pub action_id: uuid::Uuid,
    pub reopened_count: usize,
    pub source_path: PathBuf,
    pub completed_path: PathBuf,
    pub already_open: bool,
}

pub fn insert_action(
    workspace_root: &Path,
    source_path: &Path,
    new_action: Action,
    parent: Option<&ActionSelector>,
) -> Result<InsertActionResult, WorkspaceError> {
    let mounts = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let (snapshot, expected) = snapshot(data_root, source_path)?;
    let source = ActionResourceState {
        path: snapshot.path().clone(),
        actions: parse_snapshot(&snapshot)?,
        expected,
    };
    let (batch, outcome) = prepare_action_insert(source, new_action, parent)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    deliver(&mounts, &batch)?;
    Ok(map_insert(data_root, outcome))
}

pub fn update_action(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
    update: ActionUpdate,
) -> Result<UpdateActionResult, WorkspaceError> {
    let mounts = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let (snapshot, expected) = snapshot(data_root, source_path)?;
    let source = ActionResourceState {
        path: snapshot.path().clone(),
        actions: parse_snapshot(&snapshot)?,
        expected,
    };
    let (batch, outcome) = prepare_action_update(source, selector, update)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    deliver(&mounts, &batch)?;
    Ok(map_update(data_root, outcome))
}

pub fn delete_action(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
) -> Result<DeleteActionResult, WorkspaceError> {
    let mounts = begin_mutation(workspace_root, source_path)?;
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
    let (batch, outcome) = prepare_action_delete(
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
    deliver(&mounts, &batch)?;
    Ok(map_delete(data_root, outcome))
}

pub fn archive_actions(
    workspace_root: &Path,
    source_path: &Path,
) -> Result<ActionArchiveResult, WorkspaceError> {
    let mounts = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let completed_path = completed_actions_path(source_path);
    let (active_snapshot, active_expected) = snapshot(data_root, source_path)?;
    let (completed_snapshot, completed_expected) = snapshot(data_root, &completed_path)?;
    let (batch, outcome) = prepare_action_archive(
        action_state(active_snapshot, active_expected)?,
        action_state(completed_snapshot, completed_expected)?,
        Local::now(),
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    deliver(&mounts, &batch)?;
    Ok(map_archive(data_root, outcome))
}

pub fn close_action_subtree(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
    closing_state: clearhead_core::ActionState,
    completed_at: chrono::DateTime<Local>,
) -> Result<CloseActionResult, WorkspaceError> {
    let mounts = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let completed_path = completed_actions_path(source_path);
    let (active_snapshot, active_expected) = snapshot(data_root, source_path)?;
    let (completed_snapshot, completed_expected) = snapshot(data_root, &completed_path)?;
    let (batch, outcome) = prepare_close_action_subtree(
        action_state(active_snapshot, active_expected)?,
        action_state(completed_snapshot, completed_expected)?,
        selector,
        closing_state,
        completed_at,
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    deliver(&mounts, &batch)?;
    Ok(map_close(data_root, outcome))
}

pub fn reopen_action_subtree(
    workspace_root: &Path,
    source_path: &Path,
    selector: &ActionSelector,
) -> Result<ReopenActionResult, WorkspaceError> {
    let mounts = begin_mutation(workspace_root, source_path)?;
    let data_root = &mounts.workspace;
    let completed_path = completed_actions_path(source_path);
    let (active_snapshot, active_expected) = snapshot(data_root, source_path)?;
    let (completed_snapshot, completed_expected) = snapshot(data_root, &completed_path)?;
    let (batch, outcome) = prepare_reopen_action_subtree(
        action_state(active_snapshot, active_expected)?,
        action_state(completed_snapshot, completed_expected)?,
        selector,
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    deliver(&mounts, &batch)?;
    Ok(map_reopen(data_root, outcome))
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
) -> Result<NativeWorkspaceMounts, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, None);
    let charter_root = mounts.workspace.join("charters");
    validate_source_path(source_path, &charter_root)?;
    std::fs::create_dir_all(&charter_root)?;
    Ok(mounts)
}

fn validate_source_path(source_path: &Path, charter_root: &Path) -> Result<(), WorkspaceError> {
    // A charter gaining its first action has no anchor on disk yet, so the
    // path cannot always be canonicalized. An existing target keeps the exact
    // previous check (resolve the file itself); a not-yet-created one is
    // contained iff its deepest existing ancestor is.
    let contained_in = |root: &Path| match source_path.canonicalize() {
        Ok(canonical) => canonical.starts_with(root),
        Err(_) => source_path
            .parent()
            .and_then(|parent| parent.ancestors().find(|ancestor| ancestor.exists()))
            .and_then(|existing| existing.canonicalize().ok())
            .is_some_and(|canonical| canonical.starts_with(root)),
    };
    let valid_location = charter_root
        .canonicalize()
        .map(|root| contained_in(&root))
        .unwrap_or(false);
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

// ============================================================================
// Charter document delivery
// ============================================================================

/// A charter `.md` read under the same revision discipline action files use.
///
/// `jot`, `close` and `update` used to write charter markdown with a bare
/// [`atomic_write`], skipping the precondition check every action mutation
/// gets. Reading through this seam captures the revision at read time and
/// [`write_charter_document`] re-checks it immediately before the write, so a
/// concurrent writer becomes a reported conflict instead of a silent lost
/// update.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CharterDocument {
    path: PathBuf,
    content: Option<String>,
    expected: ExpectedResource,
}

impl CharterDocument {
    /// The document's text, or `None` when it does not exist yet.
    pub fn content(&self) -> Option<&str> {
        self.content.as_deref()
    }

    /// Whether the document was missing when it was read.
    pub fn is_missing(&self) -> bool {
        self.content.is_none()
    }

    /// The physical path the document was read from.
    pub fn path(&self) -> &Path {
        &self.path
    }
}

/// Read a charter `.md` and capture the revision a later write must still see.
pub fn read_charter_document(
    workspace_root: &Path,
    md_path: &Path,
) -> Result<CharterDocument, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, None);
    let data_root = &mounts.workspace;
    charter_document_location(data_root, md_path)?;
    let (snapshot, expected) = snapshot(data_root, md_path)?;
    // An existing *empty* file is a revision, not an absence: only the
    // expected state distinguishes the two.
    let content = match &expected {
        ExpectedResource::Missing => None,
        ExpectedResource::Revision(_) => Some(
            std::str::from_utf8(snapshot.bytes())
                .map_err(|error| {
                    WorkspaceError::Parse(format!("charter markdown is not UTF-8: {error}"))
                })?
                .to_string(),
        ),
    };
    Ok(CharterDocument {
        path: md_path.to_path_buf(),
        content,
        expected,
    })
}

/// Write `content` to the document iff it still matches the revision captured
/// by [`read_charter_document`].
///
pub fn write_charter_document(
    workspace_root: &Path,
    document: &CharterDocument,
    content: &str,
) -> Result<(), WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, None);
    let location = charter_document_location(&mounts.workspace, &document.path)?;
    let batch = EffectBatch::new(
        vec![Effect::Write {
            path: location.clone(),
            bytes: content.as_bytes().to_vec(),
        }],
        vec![ResourcePrecondition {
            path: location,
            expected: document.expected.clone(),
        }],
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    deliver(&mounts, &batch)
}

/// The workspace-relative location of a charter document, rejecting paths that
/// are outside the charter tree or are not markdown — a guard mirroring
/// [`validate_source_path`] so the public seam cannot be pointed at arbitrary
/// workspace files.
fn charter_document_location(
    data_root: &Path,
    md_path: &Path,
) -> Result<ResourceLocation, WorkspaceError> {
    let charter_root = data_root.join("charters");
    let valid_location = md_path.starts_with(&charter_root);
    let valid_name = md_path
        .file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with(".md"));
    if !(valid_location && valid_name) {
        return Err(WorkspaceError::InvalidPath(md_path.to_path_buf()));
    }
    let relative = md_path
        .strip_prefix(data_root)
        .map_err(|_| WorkspaceError::InvalidPath(md_path.to_path_buf()))?;
    Ok(ResourceLocation::workspace(logical_path(relative)?))
}

fn parse_sidecar_snapshot(snapshot: &ResourceSnapshot) -> Result<CharterMetadata, WorkspaceError> {
    if snapshot.bytes().is_empty() {
        return Ok(CharterMetadata::default());
    }
    serde_json::from_slice(snapshot.bytes())
        .map_err(|error| WorkspaceError::Parse(format!("sidecar: {error}")))
}

/// Validate the batch's preconditions against the live workspace, then apply it.
fn deliver(mounts: &NativeWorkspaceMounts, batch: &EffectBatch) -> Result<(), WorkspaceError> {
    validate_preconditions(mounts, batch.preconditions())?;
    execute_effects(mounts, batch.effects())?;
    Ok(())
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

fn map_reopen(data_root: &Path, outcome: PreparedReopenOutcome) -> ReopenActionResult {
    ReopenActionResult {
        action_id: outcome.action_id,
        reopened_count: outcome.reopened_count,
        source_path: data_root.join(outcome.source_path.as_str()),
        completed_path: data_root.join(outcome.completed_path.as_str()),
        already_open: outcome.already_open,
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
    std::fs::create_dir_all(data_root.join("charters"))?;

    let model = load_target_files(workspace_root, data_root, &target_ids)?;
    let (batch, outcome) = prepare_transaction(model, &operations, Local::now(), dry_run)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;

    if !batch.is_empty() {
        deliver(&mounts, &batch)?;
    }

    Ok(map_outcome(data_root, outcome))
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
            return Err(WorkspaceError::Conflict(conflict));
        }
    }
    Ok(())
}

/// Apply a validated effect batch directly to the filesystem.
///
/// There is no write-ahead journal: each write is a single-file POSIX atomic
/// replace ([`atomic_write`] = temp + fsync + rename). Effects are applied in
/// *additive order* — content-producing effects (writes, moves) before removals
/// — so an interrupted multi-file mutation leaves a recoverable duplicate for
/// `doctor` to reconcile rather than a hole. The finer add-before-subtract
/// ordering *between two writes* is Core's responsibility (it emits the
/// destination-gaining write first; see the direct-delivery charter §4), since
/// an `Effect::Write` is opaque bytes here and cannot be classified.
fn execute_effects(
    mounts: &NativeWorkspaceMounts,
    effects: &[Effect],
) -> Result<(), WorkspaceError> {
    // Stable partition preserves Core's emission order within each group.
    let (removals, additions): (Vec<&Effect>, Vec<&Effect>) = effects
        .iter()
        .partition(|effect| matches!(effect, Effect::Remove { .. }));

    for effect in additions {
        match effect {
            Effect::Write { path, bytes } => {
                atomic_write(&mounts.physical_path(path)?, bytes)?;
            }
            Effect::Move {
                source,
                destination,
            } => {
                let destination = mounts.physical_path(destination)?;
                if let Some(parent) = destination.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::rename(mounts.physical_path(source)?, destination)?;
            }
            Effect::Remove { .. } => unreachable!("removals are partitioned out and applied last"),
        }
    }

    for effect in removals {
        if let Effect::Remove { path } = effect {
            match std::fs::remove_file(mounts.physical_path(path)?) {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error.into()),
            }
        }
    }
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
        EffectBatch, MountId, ResourceLocation, ResourcePrecondition,
    };

    fn location(mount: MountId, path: &str) -> ResourceLocation {
        ResourceLocation::new(mount, WorkspacePath::new(path).unwrap())
    }

    #[test]
    fn delivers_workspace_and_external_plan_writes() {
        let temp = tempfile::tempdir().unwrap();
        let workspace = temp.path().join("workspace");
        let external = temp.path().join("external-plans");
        std::fs::create_dir_all(&external).unwrap();
        let mounts = NativeWorkspaceMounts {
            workspace: workspace.clone(),
            external_plans: Some(external.clone()),
            root_charter: "workspace".into(),
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
        execute_effects(&mounts, batch.effects()).unwrap();

        assert_eq!(
            std::fs::read(workspace.join("sync/plans.json")).unwrap(),
            b"store"
        );
        assert_eq!(
            std::fs::read(external.join("inbox/action.ics")).unwrap(),
            b"calendar"
        );
    }

    #[test]
    fn a_source_anchor_that_does_not_exist_yet_still_validates() {
        let temp = tempfile::tempdir().unwrap();
        let charter_root = temp.path().join(".clearhead/charters");
        std::fs::create_dir_all(&charter_root).unwrap();

        // A charter gaining its first action has no anchor on disk.
        validate_source_path(&charter_root.join("notes.actions"), &charter_root).unwrap();
        // ...including one in a directory that does not exist yet (the deepest
        // existing ancestor is the charter root).
        validate_source_path(&charter_root.join("someday/probe.actions"), &charter_root).unwrap();

        // A missing path outside the charter tree is still rejected, as is a
        // missing file that is not an actions anchor.
        assert!(validate_source_path(&temp.path().join("outside.actions"), &charter_root).is_err());
        assert!(validate_source_path(&charter_root.join("notes.md"), &charter_root).is_err());
    }
}

#[cfg(test)]
mod charter_document_tests {
    use super::*;

    fn workspace() -> tempfile::TempDir {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(temp.path().join(".clearhead/charters")).unwrap();
        temp
    }

    fn charter_path(root: &Path, name: &str) -> PathBuf {
        root.join(".clearhead/charters").join(name)
    }

    #[test]
    fn writes_an_existing_document_and_reports_its_content() {
        let temp = workspace();
        let root = temp.path();
        let path = charter_path(root, "support.md");
        std::fs::write(&path, "---\nid: a\n---\n").unwrap();

        let document = read_charter_document(root, &path).unwrap();
        assert_eq!(document.content(), Some("---\nid: a\n---\n"));
        assert!(!document.is_missing());

        write_charter_document(root, &document, "---\nid: a\n---\n\nnew\n").unwrap();
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            "---\nid: a\n---\n\nnew\n"
        );
    }

    #[test]
    fn creates_a_document_that_was_missing() {
        let temp = workspace();
        let root = temp.path();
        let path = charter_path(root, "support.md");

        let document = read_charter_document(root, &path).unwrap();
        assert!(document.is_missing());
        assert_eq!(document.content(), None);

        write_charter_document(root, &document, "---\nid: a\n---\n# Support\n").unwrap();
        assert!(path.exists());
    }

    #[test]
    fn rejects_a_write_when_the_document_changed_since_the_read() {
        let temp = workspace();
        let root = temp.path();
        let path = charter_path(root, "support.md");
        std::fs::write(&path, "original\n").unwrap();

        let document = read_charter_document(root, &path).unwrap();
        std::fs::write(&path, "written by someone else\n").unwrap();

        let error = write_charter_document(root, &document, "clobber\n").unwrap_err();
        assert!(
            matches!(&error, WorkspaceError::Conflict(_)),
            "expected a typed conflict, got: {error:?}"
        );
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            "written by someone else\n",
            "a stale write must not clobber the newer content"
        );
    }

    #[test]
    fn rejects_a_path_outside_the_charter_tree() {
        let temp = workspace();
        let root = temp.path();
        let outside = root.join(".clearhead/notes.md");
        std::fs::write(&outside, "notes\n").unwrap();
        assert!(matches!(
            read_charter_document(root, &outside),
            Err(WorkspaceError::InvalidPath(_))
        ));
    }
}
