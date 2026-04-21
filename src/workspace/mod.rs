//! Workspace (DSL) projection.
//!
//! This module owns the `.actions` file format and `.md` charter parsing,
//! plus the `WorkspaceStore` trait for discovering and persisting workspace
//! content on disk or in memory.
//!
//! It speaks `DomainModel` at its boundary — callers convert to/from the
//! domain IR, and the workspace module handles the DSL details.

pub mod actions;
pub mod acts;
pub mod charter;
pub mod detection;
pub mod ics;
pub mod plans;
pub mod store;

// Re-export key types at workspace level
pub use actions::{
    Action, ActionList, ActionRepository, ActionSource, ActionState, Diff, OutputFormat,
    ParseFailure, ParseMode, ParseOutcome, ParsedDocument, RecoveryReport, SourceMetadata,
    SourceRange, SourcedAction, diff_actions, format, parse_actions, parse_actions_with_mode,
    parse_document, parse_tree, patch_action_list,
};
pub use acts::{completed_acts_path, read_acts, write_acts};
pub use charter::{format_charter, implicit_charter, parse_charter};
pub use ics::occurrence_act_id;
pub use plans::{
    PlanFileEntry, collect_plan_files, infer_plan_charter_name, infer_plan_charter_name_for_workspace,
    infer_plan_parent, infer_plan_parent_for_workspace,
};

pub use detection::check_for_workspace;
pub use store::{
    ManifestSourceType, WorkspaceError, WorkspaceManifestEntry, collect_workspace_manifest,
    infer_charter_name, infer_charter_name_for_workspace, infer_parent_charter_name,
    infer_parent_charter_name_for_workspace, list_action_files, load_domain_model,
    save_domain_model, workspace_data_root,
};
