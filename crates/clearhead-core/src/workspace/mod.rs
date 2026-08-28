//! Workspace (DSL) projection.
//!
//! This module owns the `.actions` file format and `.md` charter parsing,
//! plus the `WorkspaceStore` trait for discovering and persisting workspace
//! content on disk or in memory.
//!
//! It speaks `DomainModel` at its boundary — callers convert to/from the
//! domain IR, and the workspace module handles the DSL details.

pub mod action_files;
pub mod actions;
pub mod archive_actions;
pub mod archive_charter;
pub mod archive_facts;
pub mod calendar;
pub mod charter;
pub mod manifest;
pub mod mutate_actions;
pub mod resource;
pub mod selector;
pub mod sidecar;
pub mod store;
pub mod templates;
pub mod transaction;

// Re-export key types at workspace level
pub use action_files::{ActionsFile, completed_actions_path};
pub use actions::{
    Action, ActionList, ActionState, Diff, IntegrityError, OutputFormat, ParseFailure, ParseMode,
    ParseOutcome, ParsedDocument, RecoveryReport, SourceMetadata, SourceRange, SourcedAction,
    TrustedDocument, diff_actions, format, format_trusted_source, parse_actions,
    parse_actions_with_mode, parse_document, parse_tree, parse_trusted_document, patch_action_list,
};
pub use archive_actions::{
    ActionArchivePlan, ClosePreparedState, PreparedArchiveOutcome, PreparedCloseOutcome,
    plan_action_archive, prepare_action_archive, prepare_close_action_subtree,
};
pub use archive_charter::{
    ArchivePolicyError, archive_charter_name, archive_charter_subtree,
    has_terminal_archive_ancestor, materialize_archive_parent, resolve_archive_parent_uuid,
    validate_archive_candidate,
};
pub use archive_facts::ArchivedActionFact;
pub use calendar::ics::{
    ICSPlan, OccurrenceOp, OccurrenceOverride, VTodoAction, action_id_from_vtodo_uid,
    action_to_vtodo, actions_to_icalendar, canonical_occurrence_key, occurrence_action_id,
    parse_ics, parse_vtodo_actions_content, plan_to_vevent, plan_to_vtodo, plans_to_icalendar,
    plans_to_icalendar_with_component, render_master_rollforward, render_occurrence_deviation,
    render_plan_resource_with_component,
};
pub use calendar::plans::{
    action_mirror_path, charter_plans_dir_relative, infer_plan_charter_name, infer_plan_parent,
    plan_file_name, plan_output_path, slugify,
};
pub use calendar::reconcile::{
    AppliedSync, CalendarSyncPreparationInput, CalendarSyncState, OutcomeKind, Reconcile,
    SyncActionResourceState, SyncConflictResolution, SyncEntry, SyncField, SyncImport,
    SyncMirrorResourceState, SyncPlanTemplate, SyncReport, SyncTally, VTodoResource, plan_sync,
    prepare_sync, reconcile, sync_import_actions_file,
};
pub use calendar::sync_store::{PlansSyncStore, decode_plans_sync_store, encode_plans_sync_store};
pub use charter::{MarkdownCharter, format_charter, implicit_charter, parse_charter};
pub use manifest::{WorkspaceManifest, parse_workspace_manifest, render_workspace_manifest};
pub use mutate_actions::{
    ActionPrepareError, ActionResourceState, DeletePreparedState, PreparedDeleteOutcome,
    PreparedInsertOutcome, PreparedUpdateOutcome, SidecarResourceState, plan_action_insert,
    prepare_action_delete, prepare_action_insert, prepare_action_update,
};
pub use resource::{
    AppliedMutation, DeliveryError, Effect, EffectBatch, EffectBatchError, ExpectedResource,
    MountId, MountInventory, MountReadEvidence, PreparedMutation, ReadPlan, ResourceConflict,
    ResourceLocation, ResourcePrecondition, ResourceReadFailure, ResourceRevision,
    ResourceSnapshot, SnapshotError, WorkspaceInventory, WorkspaceMounts, WorkspacePath,
    WorkspacePathError, WorkspaceScope, WorkspaceSnapshot, plan_workspace_read,
};
pub use selector::ActionSelector;
pub use sidecar::{
    ActionMeta, ActionPlanLink, CharterMeta, CharterMetadata, OccurrenceSnapshot, hydrate_actions,
    parse_sidecar, record_charter_id, render_sidecar, sidecar_path, stamp_metadata_entries,
};
pub use transaction::{
    ActionUpdateSet, FileState, NormalizedOperation, Operation, PreparedTransactionOutcome,
    TransactionError, TransactionModel, TransactionOutcome, TransactionRequest, apply_operations,
    normalize_request, prepare_transaction,
};

pub use store::{
    Diagnosis, DoctorCollectionEvidence, DoctorDocument, DoctorEvidence, DoctorRepair,
    DoctorSidecarEvidence, DurabilityResidue, DurabilityResidueKind, Finding, FindingSeverity,
    Workspace, WorkspaceAssemblyInput, WorkspaceError, WorkspaceRead, assemble_workspace,
    assembled_domain_model, charter_collection_from_anchor, diagnose, infer_charter_name,
    infer_charter_name_for_workspace, infer_parent_charter_name,
    infer_parent_charter_name_for_workspace,
};
pub use templates::{instantiate_template, template_candidates};
