//! Action DSL (.actions) parsing and manipulation.
//!
//! This module provides the tools to work with the ClearHead Action DSL:
//! - [`Action`] - The core unit of work in a `.actions` file.
//! - [`ActionList`] - A collection of actions, often representing a full document.
//! - [`ActionRepository`] - High-level abstraction for managing action sources.
//! - Parsing, formatting, and linting logic for the DSL.

pub mod convert;
pub mod diff;
pub mod format;
pub mod lint;
pub mod parser;
pub mod repository;
pub mod source;

pub use crate::workspace::store::infer_charter_name;

// Core action types
pub use parser::{Action, ActionList, ActionState, PredecessorRef, parse_tree};

// Parse pipeline
pub use source::{ParsedDocument, SourceMetadata, SourceRange, get_node_text, parse_actions, parse_document};

// Storage
pub use repository::{ActionRepository, ActionSource, SourcedAction};
pub use convert::patch_action_list;

// Diff
pub use diff::{ActionDiff, Diff, FieldChange, diff_actions};

// Format
pub use format::{FormatConfig, FormatStyle, IndentStyle, OutputFormat, TableFormatOptions, format};

// Lint
pub use lint::{LintDiagnostic, LintResults, LintSeverity, lint_document};
