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
pub mod durability;
pub mod manifest;
pub mod mutate_actions;
pub mod resource;
pub mod selector;
pub mod sidecar;
pub mod store;
pub mod templates;
pub mod transaction;

// Re-export key types at workspace level
pub use action_files::{
    ActionsFile, completed_actions_path, read_action_file, read_actions, write_actions,
};
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
    ArchiveCharterError, ArchiveCharterOptions, ArchiveCharterResult, archive_charter,
    archive_terminal_charters, find_charter as find_markdown_charter,
};
pub use archive_facts::ArchivedActionFact;
pub use calendar::ics::{
    ICSPlan, OccurrenceOp, OccurrenceOverride, VTodoAction, action_id_from_vtodo_uid,
    action_to_vtodo, actions_to_icalendar, canonical_occurrence_key, occurrence_action_id,
    parse_vtodo_actions, plan_to_vtodo, plans_to_icalendar, write_master_rollforward,
    write_occurrence_deviation,
};
pub use calendar::plans::{
    PlanFileEntry, action_mirror_path, apply_occurrence_op, charter_plans_dir_relative,
    collect_plan_files, collect_plan_files_with_plans, infer_plan_charter_name, infer_plan_parent,
    plan_file_name, plan_output_path, slugify,
};
pub use calendar::reconcile::{
    AppliedSync, OutcomeKind, Reconcile, SyncEntry, SyncField, SyncImport, SyncReport, SyncTally,
    VTodoResource, apply_sync, plan_sync, read_ics_dates, read_vtodo_actions, reconcile,
    resolve_materialized_occurrence, sync_master_rollforwards,
};
pub use calendar::sync_store::{PlansSyncStore, plans_sync_store_path, read_plans_sync_store};
pub use charter::{MarkdownCharter, format_charter, implicit_charter, parse_charter};
pub use manifest::WorkspaceManifest;
pub use mutate_actions::{
    ActionPrepareError, ActionResourceState, DeletePreparedState, PreparedDeleteOutcome,
    PreparedInsertOutcome, PreparedUpdateOutcome, SidecarResourceState, plan_action_insert,
    prepare_action_delete, prepare_action_insert, prepare_action_update,
};
pub use resource::{
    AppliedMutation, DeliveryError, Effect, EffectBatch, EffectBatchError, ExpectedResource,
    PreparedMutation, ReadPlan, ResourceConflict, ResourcePrecondition, ResourceRevision,
    ResourceSnapshot, SnapshotError, WorkspaceInventory, WorkspacePath, WorkspacePathError,
    WorkspaceSnapshot,
};
pub use selector::ActionSelector;
pub use sidecar::{
    ActionMeta, CharterMeta, CharterMetadata, OccurrenceSnapshot, hydrate_actions, read_sidecar,
    sidecar_path, write_sidecar,
};
pub use transaction::{
    ActionUpdateSet, FileState, NormalizedOperation, Operation, PreparedTransactionOutcome,
    TransactionError, TransactionModel, TransactionOutcome, TransactionRequest, apply_operations,
    normalize_request, prepare_transaction,
};

pub use store::{
    Diagnosis, Finding, FindingSeverity, ManifestSourceType, Workspace, WorkspaceError,
    WorkspaceManifestEntry, WorkspaceRead, charter_collection_from_anchor, charter_root,
    collect_workspace_manifest, diagnose, diagnose_read, infer_charter_name,
    infer_parent_charter_name, list_action_files, load_domain_model, load_domain_model_with_plans,
    load_workspace, load_workspace_with_plans, load_workspaces, plans_root, read_workspace,
    read_workspace_with_plans, save_domain_model, workspace_data_root,
};
pub use templates::{instantiate_template, resolve_template};
