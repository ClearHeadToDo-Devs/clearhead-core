//! Workspace (DSL) projection.
//!
//! This module owns the `.actions` file format and `.md` charter parsing,
//! plus the `WorkspaceStore` trait for discovering and persisting workspace
//! content on disk or in memory.
//!
//! It speaks `DomainModel` at its boundary — callers convert to/from the
//! domain IR, and the workspace module handles the DSL details.

pub mod actions;
pub mod action_files;
pub mod archive_actions;
pub mod calendar;
pub mod durability;
pub mod archive_charter;
pub mod charter;
pub mod detection;
pub mod manifest;
pub mod sidecar;
pub mod store;
pub mod templates;

// Re-export key types at workspace level
pub use actions::{
    Action, ActionList, ActionState, Diff, OutputFormat,
    ParseFailure, ParseMode, ParseOutcome, ParsedDocument, RecoveryReport, SourceMetadata,
    SourceRange, SourcedAction, diff_actions, format, parse_actions, parse_actions_with_mode,
    parse_document, parse_tree, patch_action_list,
};
pub use action_files::{ActionsFile, completed_actions_path, upcoming_actions_path, read_action_file, read_actions, write_actions};
pub use archive_actions::{
    ActionArchivePlan, ActionArchiveResult, CloseActionResult, CloseActionSelector,
    archive_actions, close_action_subtree, plan_action_archive,
};
pub use sidecar::{ActionMeta, CharterMeta, CharterMetadata, PlanMeta, hydrate_actions, read_sidecar, sidecar_path, write_sidecar};
pub use charter::{MarkdownCharter, format_charter, implicit_charter, parse_charter};
pub use manifest::WorkspaceManifest;
pub use archive_charter::{
    ArchiveCharterError, ArchiveCharterOptions, ArchiveCharterResult,
    archive_charter, archive_terminal_charters, find_charter as find_markdown_charter,
};
pub use calendar::ics::{ICSPlan, action_to_vtodo, actions_to_icalendar, occurrence_action_id};
pub use calendar::plans::{
    PlanFileEntry, action_mirror_path, charter_plans_dir_relative, collect_plan_files,
    collect_plan_files_with_plans, infer_plan_charter_name, infer_plan_parent, plan_file_name,
    plan_output_path,
};
pub use calendar::reconcile::{
    AppliedSync, Reconcile, SyncEntry, SyncReport, SyncTally, apply_sync, plan_sync,
    read_ics_dates, reconcile,
};
pub use calendar::sync_store::{
    PlansSyncStore, plans_sync_store_path, read_plans_sync_store,
};

pub use detection::check_for_workspace;
pub use calendar::expand::expand_plans_into_actions;
pub use store::{
    Diagnosis, diagnose, diagnose_read, Finding, FindingSeverity, Workspace, WorkspaceRead, ManifestSourceType, WorkspaceError,
    WorkspaceManifestEntry, collect_workspace_manifest, infer_charter_name,
    infer_parent_charter_name, list_action_files, load_domain_model,
    load_domain_model_with_plans, load_workspaces, charter_root, plans_root, load_workspace,
    load_workspace_with_plans, read_workspace, read_workspace_with_plans,
    save_domain_model, workspace_data_root,
};
pub use templates::{instantiate_template, resolve_template};
