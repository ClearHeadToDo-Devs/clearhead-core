use super::lint::LintDiagnostic;
use super::parser::parse_action_recursive;
use std::collections::HashMap;
use tree_sitter::{Node, Tree};
use uuid::Uuid;

/// Extract the source text for a node
pub fn get_node_text(node: &Node, source: &str) -> String {
    source[node.start_byte()..node.end_byte()].to_string()
}

/// Get text from a node, stripping a prefix character (e.g., '$', '!', '+')
/// Returns None if the text doesn't start with the expected prefix
pub fn get_prefixed_text(node: &Node, source: &str, prefix: char) -> Option<String> {
    let text = get_node_text(node, source);
    text.strip_prefix(prefix).map(|s| s.trim().to_string())
}

/// Get text from a named child field
pub fn get_field_text(node: &Node, source: &str, field: &str) -> Option<String> {
    node.child_by_field_name(field)
        .map(|n| get_node_text(&n, source))
}

pub fn create_tree_wrapper(tree: Tree, source: String) -> TreeWrapper {
    TreeWrapper { tree, source }
}

/// Validate a tree-sitter tree for syntax errors
pub fn validate_tree(tree: &Tree) -> Result<(), String> {
    let root = tree.root_node();
    if root.has_error() {
        // Find the specific error node
        let mut cursor = root.walk();
        let mut error_node = None;

        // Depth-first search for the first ERROR or MISSING node
        let mut stack = vec![root];
        while let Some(node) = stack.pop() {
            if node.is_error() || node.is_missing() {
                error_node = Some(node);
                break;
            }
            for child in node.children(&mut cursor) {
                stack.push(child);
            }
        }

        if let Some(err) = error_node {
            let start = err.start_position();
            return Err(format!(
                "Syntax error at line {}, column {}: {}",
                start.row + 1,
                start.column + 1,
                if err.is_missing() {
                    format!("missing '{}'", err.kind())
                } else {
                    "unexpected token".to_string()
                }
            ));
        }
        return Err("Syntax error in actions file".to_string());
    }
    Ok(())
}

// we need both the tree and the source to do our type conversions properly
pub struct TreeWrapper {
    pub tree: Tree,
    pub source: String,
}

pub fn create_node_wrapper(node: Node, source: String) -> NodeWrapper {
    NodeWrapper { node, source }
}

// same goes for the nodes, infact, we are going to be passing a cloned version of the string so
// everything has what they need early
pub struct NodeWrapper<'a> {
    pub node: Node<'a>,
    pub source: String,
}

impl<'a> NodeWrapper<'a> {
    /// Get a required child field, returning an error if missing
    pub fn require_field(&self, field: &str) -> Result<Node<'a>, &'static str> {
        self.node
            .child_by_field_name(field)
            .ok_or("Missing required field")
    }

    /// Get the text content of this node
    pub fn text(&self) -> String {
        get_node_text(&self.node, &self.source)
    }

    /// Get text from a child field
    pub fn field_text(&self, field: &str) -> Option<String> {
        get_field_text(&self.node, &self.source, field)
    }

    /// Get text with a prefix stripped (e.g., '$' for description, '!' for priority)
    pub fn prefixed_text(&self, prefix: char) -> Option<String> {
        get_prefixed_text(&self.node, &self.source, prefix)
    }

    /// Iterate over children with a given field name
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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceMetadata {
    pub root: SourceRange,
    pub line_range: SourceRange,
    pub do_date: Option<SourceRange>,
    pub completed_date: Option<SourceRange>,
    pub created_date: Option<SourceRange>,
    pub is_id_generated: bool,
    pub raw_id: Option<String>,
}

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

use super::ActionList;

#[derive(Debug, Clone)]
pub struct ParsedDocument {
    pub actions: ActionList,
    pub source_map: HashMap<Uuid, SourceMetadata>,
    pub tag_index: HashMap<String, Vec<SourceRange>>,
    pub syntax_errors: Vec<LintDiagnostic>,
}

impl TryFrom<TreeWrapper> for ActionList {
    type Error = String;
    fn try_from(value: TreeWrapper) -> Result<Self, Self::Error> {
        let parsed: ParsedDocument = value.try_into()?;
        Ok(parsed.actions)
    }
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

        // Collect syntax errors (ERROR and MISSING nodes)
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
                // Don't recurse into errors themselves, just find the top-most ones
                if !node.is_error() {
                    for child in node.children(&mut cursor) {
                        stack.push(child);
                    }
                }
            }
        }

        // Iterate through all root actions
        for root_action in root.children(&mut cursor) {
            if root_action.kind() == "root_action" {
                let wrapper = create_node_wrapper(root_action, value.source.clone());
                action_list.extend(
                    parse_action_recursive(wrapper, None, &mut source_map, &mut tag_index)
                        .map_err(|e| e.to_string())?,
                );
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
