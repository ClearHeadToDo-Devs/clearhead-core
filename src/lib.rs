//! ClearHead Core Library
//!
//! This library provides the core domain model and logic for the ClearHead framework.
//! It is designed to be consumed by multiple frontends (CLI, LSP, web services, etc.)
//! without containing any frontend-specific concerns.
//!
//! # Modules
//!
//! - `entities`: Core domain types (Action, ActionList, ActionState)
//! - `treesitter`: Tree-sitter parsing integration
//! - `format`: Formatting actions to various output formats
//! - `diff`: Diffing action lists to detect changes
//! - `sync`: Semantic comparison and sync decision logic
//! - `document`: Document save pipeline orchestration
//! - `crdt`: CRDT operations for distributed synchronization
//! - `graph`: RDF/SPARQL integration for semantic queries
//! - `domain`: Higher-level domain models (Plan, Act, phases)

use std::collections::HashMap;
use tree_sitter::Tree;
use uuid::Uuid;

pub mod entities;
pub use entities::{Action, ActionList, ActionState};

pub mod treesitter;
use treesitter::{SourceMetadata, SourceRange};

pub mod domain;
pub use domain::{ActPhase, DomainModel, Plan, PlannedAct};

pub mod crdt;

pub mod format;
pub use format::{format, FormatConfig, FormatStyle, IndentStyle, OutputFormat};

pub mod diff;

pub mod sync;

pub mod document;

pub mod graph;

pub mod sync_utils;

pub mod lint;
pub use lint::{lint_document, LintDiagnostic, LintResults, LintSeverity};

#[derive(Debug, Clone)]
pub struct ParsedDocument {
    pub actions: ActionList,
    pub source_map: HashMap<Uuid, SourceMetadata>,
    pub tag_index: HashMap<String, Vec<SourceRange>>,
    pub syntax_errors: Vec<LintDiagnostic>,
}

/// Parse a .actions file into a structured ActionList
pub fn parse_actions(actions: &str) -> Result<ActionList, String> {
    let parsed_doc = parse_document(actions)?;

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
pub fn parse_document(actions: &str) -> Result<ParsedDocument, String> {
    let tree = parse_tree(actions)?;
    let tree_wrapper = treesitter::TreeWrapper {
        tree,
        source: actions.to_string(),
    };
    let parsed_doc: ParsedDocument = tree_wrapper.try_into()?;
    Ok(parsed_doc)
}

/// Parse a .actions file into a tree-sitter Tree
pub fn parse_tree(actions: &str) -> Result<Tree, String> {
    let mut action_parser = tree_sitter::Parser::new();

    action_parser
        .set_language(&tree_sitter_actions::LANGUAGE.into())
        .expect("Failed to set language for tree-sitter parser");

    action_parser
        .parse(actions, None)
        .ok_or("Failed to parse tree".to_string())
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
