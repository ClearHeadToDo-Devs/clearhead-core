//! Workspace (DSL) projection.
//!
//! This module owns the `.actions` file format and `.md` charter parsing,
//! plus the `WorkspaceStore` trait for discovering and persisting workspace
//! content on disk or in memory.
//!
//! It speaks `DomainModel` at its boundary — callers convert to/from the
//! domain IR, and the workspace module handles the DSL details.

use tree_sitter::Tree;

pub mod actions;
pub mod charter;
pub mod store;

use actions::*;

// Re-export key types at workspace level
pub use actions::{
    Action, ActionList, ActionRepository, ActionSource, ActionState, Diff, OutputFormat,
    ParsedDocument, SourcedAction, SourceMetadata, SourceRange, diff_actions, format,
};
pub use charter::{format_charter, implicit_charter, parse_charter};
pub use store::{DiscoveredCharter, FsWorkspaceStore, InMemoryStore, ObjectiveRef, WorkspaceStore};

use crate::domain::DomainModel;

/// Parse a .actions file into a structured ActionList
pub fn parse_actions(input: &str) -> Result<ActionList, String> {
    let parsed_doc = parse_document(input)?;

    // Check for syntax errors
    if !parsed_doc.syntax_errors.is_empty() {
        let err = &parsed_doc.syntax_errors[0];
        return Err(format!(
            "Syntax error at line {}, column {}: {}",
            err.range.start_row + 1,
            err.range.start_col + 1,
            err.message
        ));
    }

    Ok(parsed_doc.actions)
}

/// Parse a .actions file into a ParsedDocument (Actions + Source Metadata)
pub fn parse_document(input: &str) -> Result<ParsedDocument, String> {
    let tree = parse_tree(input)?;
    let tree_wrapper = actions::TreeWrapper {
        tree,
        source: input.to_string(),
    };
    let parsed_doc: ParsedDocument = tree_wrapper.try_into()?;
    Ok(parsed_doc)
}

/// Parse a .actions file into a tree-sitter Tree
pub fn parse_tree(input: &str) -> Result<Tree, String> {
    let mut action_parser = tree_sitter::Parser::new();

    action_parser
        .set_language(&tree_sitter_actions::LANGUAGE.into())
        .expect("Failed to set language for tree-sitter parser");

    action_parser
        .parse(input, None)
        .ok_or("Failed to parse tree".to_string())
}

/// Parse a .actions file into a DomainModel
pub fn parse_domain_model(content: &str) -> Result<DomainModel, String> {
    let actions = parse_actions(content)?;
    Ok(actions::convert::from_actions(&actions))
}

/// Patch a primary ActionList with updates from a secondary list
pub fn patch_action_list(primary: &mut ActionList, secondary: &ActionList) {
    for patch_action in secondary {
        if let Some(original_action) = primary.iter_mut().find(|a| a.id == patch_action.id) {
            // Update existing action
            *original_action = patch_action.clone();
        } else {
            // Add new action
            primary.push(patch_action.clone());
        }
    }
}
