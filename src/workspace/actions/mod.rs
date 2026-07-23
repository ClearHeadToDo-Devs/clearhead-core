//! Action DSL (.actions) parsing and manipulation.
//!
//! This module provides the tools to work with the ClearHead Action DSL:
//! - [`Action`] - The core unit of work in a `.actions` file.
//! - [`ActionList`] - A collection of actions, often representing a full document.
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
pub use crate::domain::{Action, ActionState, PredecessorRef};
pub use parser::{ActionList, parse_tree};

// Parse pipeline
pub use source::{
    ParseFailure, ParseMode, ParseOutcome, ParsedDocument, RecoveryReport, SourceMetadata,
    SourceRange, get_node_text, parse_actions, parse_actions_with_mode, parse_document,
};

// Storage
pub use convert::patch_action_list;
pub use repository::SourcedAction;

// Diff
pub use diff::{ActionDiff, Diff, FieldChange, diff_actions};

// Format
pub use format::{
    FormatConfig, FormatStyle, IndentStyle, OutputFormat, TableFormatOptions, format,
};

// Lint
pub use lint::{LintDiagnostic, LintResults, LintSeverity, lint_document};
