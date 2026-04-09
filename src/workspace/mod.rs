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
pub mod store;

// Re-export key types at workspace level
pub use actions::{
    Action, ActionList, ActionRepository, ActionSource, ActionState, Diff, OutputFormat,
    ParsedDocument, SourceMetadata, SourceRange, SourcedAction, diff_actions, format,
    parse_actions, parse_document, parse_tree, patch_action_list,
};
pub use acts::{
    closed_acts_path, merge_acts_into_model, open_acts_path, read_acts, write_acts,
    write_acts_for_plans,
};
pub use charter::{format_charter, implicit_charter, parse_charter};

pub use detection::check_for_workspace;
pub use store::{
    WorkspaceError, infer_charter_name, infer_charter_name_for_workspace,
    infer_parent_charter_name, infer_parent_charter_name_for_workspace, load_domain_model,
    save_domain_model,
};
