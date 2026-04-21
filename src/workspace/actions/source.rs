use super::ActionList;
use super::lint::LintDiagnostic;
use super::parser::{parse_action_recursive, parse_tree};
use std::collections::HashMap;
use std::fmt;
use tree_sitter::{Node, Tree};
use uuid::Uuid;

// --- Public types ---

/// The result of parsing a `.actions` file: actions, source locations, tag index, and any errors.
#[derive(Debug, Clone)]
pub struct ParsedDocument {
    pub actions: ActionList,
    pub source_map: HashMap<Uuid, SourceMetadata>,
    pub tag_index: HashMap<String, Vec<SourceRange>>,
    pub syntax_errors: Vec<LintDiagnostic>,
}

/// Parse policy for `.actions` source handling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseMode {
    /// Fail if any syntax error is detected.
    Strict,
    /// Return recoverable actions and diagnostics.
    Recover,
}

/// Structured outcome for parser boundary consumers.
#[derive(Debug, Clone)]
pub struct ParseOutcome {
    pub document: ParsedDocument,
    pub syntax_errors: Vec<LintDiagnostic>,
    pub lint_warnings: Vec<LintDiagnostic>,
    pub recovery: RecoveryReport,
}

/// Recovery metadata for observability and policy decisions.
#[derive(Debug, Clone, Default)]
pub struct RecoveryReport {
    /// Number of actions successfully recovered from source.
    pub recoverable_actions: usize,
}

/// Systemic parse failure used by strict-mode mutation boundaries.
#[derive(Debug, Clone)]
pub struct ParseFailure {
    pub code: &'static str,
    pub message: String,
    pub range: SourceRange,
    pub hint: Option<String>,
}

impl fmt::Display for ParseFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at line {}, column {}: {}",
            self.code,
            self.range.start_row + 1,
            self.range.start_col + 1,
            self.message
        )
    }
}

impl std::error::Error for ParseFailure {}

/// Source location metadata for a single action.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceMetadata {
    pub root: SourceRange,
    pub line_range: SourceRange,
    pub do_date: Option<SourceRange>,
    pub due_date: Option<SourceRange>,
    pub completed_date: Option<SourceRange>,
    pub created_date: Option<SourceRange>,
    pub is_id_generated: bool,
    pub raw_id: Option<String>,
}

/// A row/column span within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SourceRange {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
}

impl SourceRange {
    pub fn from_node(node: &tree_sitter::Node) -> Self {
        let start = node.start_position();
        let end = node.end_position();
        Self {
            start_row: start.row,
            start_col: start.column,
            end_row: end.row,
            end_col: end.column,
        }
    }
}

// --- Parse pipeline (high → low) ---

/// Parse a `.actions` file into a structured `ActionList`, returning an error on syntax problems.
pub fn parse_actions(input: &str) -> Result<ActionList, String> {
    let outcome = parse_actions_with_mode(input, ParseMode::Strict)
        .map_err(|e| e.to_string())?;
    Ok(outcome.document.actions)
}

/// Parse a `.actions` document with explicit policy mode.
pub fn parse_actions_with_mode(input: &str, mode: ParseMode) -> Result<ParseOutcome, ParseFailure> {
    let document = parse_document(input).map_err(|e| ParseFailure {
        code: "parse-io",
        message: e,
        range: SourceRange::default(),
        hint: None,
    })?;

    let syntax_errors = document.syntax_errors.clone();

    if mode == ParseMode::Strict && !syntax_errors.is_empty() {
        let first = &syntax_errors[0];
        return Err(ParseFailure {
            code: "syntax-error",
            message: first.message.clone(),
            range: first.range,
            hint: Some("Fix syntax errors before running mutating commands".to_string()),
        });
    }

    Ok(ParseOutcome {
        recovery: RecoveryReport {
            recoverable_actions: document.actions.len(),
        },
        document,
        syntax_errors,
        lint_warnings: Vec::new(),
    })
}

/// Parse a `.actions` file into a `ParsedDocument` (actions + source metadata + errors).
pub fn parse_document(input: &str) -> Result<ParsedDocument, String> {
    let tree = parse_tree(input)?;
    TreeWrapper {
        tree,
        source: input.to_string(),
    }
    .try_into()
}

impl TryFrom<TreeWrapper> for ParsedDocument {
    type Error = String;
    fn try_from(value: TreeWrapper) -> Result<Self, Self::Error> {
        let root = value.tree.root_node();
        let mut action_list = Vec::new();
        let mut source_map = HashMap::new();
        let mut tag_index = HashMap::new();
        let mut syntax_errors = Vec::new();
        let mut cursor = root.walk();

        if root.has_error() {
            let mut stack = vec![root];
            while let Some(node) = stack.pop() {
                if node.is_error() || node.is_missing() {
                    let start = node.start_position();
                    let end = node.end_position();
                    let message = if node.is_missing() {
                        format!("missing '{}'", node.kind())
                    } else {
                        "unexpected token".to_string()
                    };
                    syntax_errors.push(LintDiagnostic::error(
                        "syntax-error",
                        message,
                        SourceRange {
                            start_row: start.row,
                            start_col: start.column,
                            end_row: end.row,
                            end_col: end.column,
                        },
                    ));
                }
                if !node.is_error() {
                    for child in node.children(&mut cursor) {
                        stack.push(child);
                    }
                }
            }
        }

        for root_action in root.children(&mut cursor) {
            if root_action.kind() == "root_action" {
                let wrapper = create_node_wrapper(root_action, value.source.clone());
                match parse_action_recursive(wrapper, None, &mut source_map, &mut tag_index) {
                    Ok(parsed_actions) => action_list.extend(parsed_actions),
                    Err(err) => {
                        syntax_errors.push(LintDiagnostic::error(
                            "action-parse-error",
                            err.to_string(),
                            SourceRange::from_node(&root_action),
                        ));
                    }
                }
            }
        }

        Ok(ParsedDocument {
            actions: action_list,
            source_map,
            tag_index,
            syntax_errors,
        })
    }
}

impl TryFrom<TreeWrapper> for ActionList {
    type Error = String;
    fn try_from(value: TreeWrapper) -> Result<Self, Self::Error> {
        let parsed: ParsedDocument = value.try_into()?;
        Ok(parsed.actions)
    }
}

// --- Tree-sitter wrappers ---

/// Pairs a parsed tree with its source text, required for any node-level operations.
pub struct TreeWrapper {
    pub tree: Tree,
    pub source: String,
}

/// Pairs a tree-sitter node with its source text so text extraction doesn't need a separate argument.
pub struct NodeWrapper<'a> {
    pub node: Node<'a>,
    pub source: String,
}

impl<'a> NodeWrapper<'a> {
    pub fn require_field(&self, field: &str) -> Result<Node<'a>, &'static str> {
        self.node
            .child_by_field_name(field)
            .ok_or("Missing required field")
    }

    pub fn text(&self) -> String {
        get_node_text(&self.node, &self.source)
    }

    pub fn field_text(&self, field: &str) -> Option<String> {
        get_field_text(&self.node, &self.source, field)
    }

    pub fn prefixed_text(&self, prefix: char) -> Option<String> {
        get_prefixed_text(&self.node, &self.source, prefix)
    }

    pub fn field_children(&self, field: &str) -> impl Iterator<Item = NodeWrapper<'a>> + '_ {
        let mut cursor = self.node.walk();
        self.node
            .children_by_field_name(field, &mut cursor)
            .map(|n| NodeWrapper {
                node: n,
                source: self.source.clone(),
            })
            .collect::<Vec<_>>()
            .into_iter()
    }
}

// --- Low-level utilities ---

pub fn create_tree_wrapper(tree: Tree, source: String) -> TreeWrapper {
    TreeWrapper { tree, source }
}

pub fn create_node_wrapper(node: Node, source: String) -> NodeWrapper {
    NodeWrapper { node, source }
}

pub fn get_node_text(node: &Node, source: &str) -> String {
    source[node.start_byte()..node.end_byte()].to_string()
}

pub fn get_prefixed_text(node: &Node, source: &str, prefix: char) -> Option<String> {
    let text = get_node_text(node, source);
    text.strip_prefix(prefix).map(|s| s.trim().to_string())
}

pub fn get_field_text(node: &Node, source: &str, field: &str) -> Option<String> {
    node.child_by_field_name(field)
        .map(|n| get_node_text(&n, source))
}
