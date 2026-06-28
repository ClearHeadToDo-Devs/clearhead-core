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
pub mod durability;
pub mod archive_charter;
pub mod charter;
pub mod detection;
pub mod expand;
pub mod ics;
pub mod plans;
pub mod sidecar;
pub mod store;
pub mod templates;

// Re-export key types at workspace level
pub use actions::{
    Action, ActionList, ActionRepository, ActionSource, ActionState, Diff, OutputFormat,
    ParseFailure, ParseMode, ParseOutcome, ParsedDocument, RecoveryReport, SourceMetadata,
    SourceRange, SourcedAction, diff_actions, format, parse_actions, parse_actions_with_mode,
    parse_document, parse_tree, patch_action_list,
};
pub use action_files::{ActionsFile, completed_actions_path, upcoming_actions_path, read_action_file, read_actions, write_actions};
pub use sidecar::{ActMeta, CharterMeta, CharterMetadata, PlanMeta, hydrate_acts, read_sidecar, sidecar_path, write_sidecar};
pub use charter::{MarkdownCharter, format_charter, implicit_charter, parse_charter};
pub use archive_charter::{
    ArchiveCharterError, ArchiveCharterOptions, ArchiveCharterResult,
    archive_charter, archive_closed_charters, find_charter as find_markdown_charter,
};
pub use ics::{ICSPlan, action_to_vevent, actions_to_icalendar, occurrence_act_id};
pub use plans::{PlanFileEntry, collect_plan_files, infer_plan_charter_name, infer_plan_parent};

pub use detection::check_for_workspace;
pub use expand::expand_plans_into_acts;
pub use store::{
    Workspace, ManifestSourceType, WorkspaceError, WorkspaceManifestEntry,
    collect_workspace_manifest, infer_charter_name, infer_parent_charter_name,
    list_action_files, load_domain_model, load_workspaces, charter_root, plans_root, load_workspace,
    save_domain_model, workspace_data_root,
};
pub use templates::{instantiate_template, resolve_template};
