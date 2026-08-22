//! The `tree` family: canonical parent-linked hierarchies.
//!
//! A tree query is a portable SELECT whose flat rows carry `id`, `name`,
//! `kind`, and — for non-roots — a `parent` that identifies another row in the
//! same result. This module validates that contract and nests the rows into
//! JSON (or an indented terminal view). Nesting is an output projection, not
//! query syntax — the query stays a plain SELECT.

use std::collections::HashMap;
use std::io::IsTerminal;

use anyhow::anyhow;
use serde_json::Value;

use super::{Row, build_store, select_rows};
use crate::argparser::QueryFormat;
use crate::commands::CommandContext;
use crate::stdout::{write_stdout, write_stdout_line};

/// Identity and display terms every tree node carries. `parent` is optional
/// only for roots; when present it must identify another row in the result.
const TREE_REQUIRED: &[&str] = &["id", "name", "kind"];

/// Run a named tree view: resolve, execute, validate/nest, render.
pub fn run(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    let name = name.unwrap_or("work-map");
    let sparql = super::registry::resolve_family(ctx, "tree", name, super::registry::BUILT_IN_TREE)
        .ok_or_else(|| {
            anyhow!(
                "No tree query named '{name}'. Save a .sparql file to \
                 <config>/queries/tree/ or <workspace>/.clearhead/queries/tree/"
            )
        })?;

    let store = build_store(ctx)?;
    let rows = select_rows(&store, &sparql)?;
    let tree = frame_tree(&rows)
        .map_err(|e| anyhow!("Query result does not satisfy the tree contract: {e}"))?;

    match format.unwrap_or_else(default_tree_format) {
        QueryFormat::Table => emit_tree(&tree),
        QueryFormat::Json => write_stdout_line(&serde_json::to_string_pretty(&tree)?),
        QueryFormat::Ndjson => {
            anyhow::bail!("--format ndjson is not defined for tree queries; use json")
        }
        QueryFormat::Jsonld => {
            anyhow::bail!("--format jsonld is not defined for tree queries; use json")
        }
        QueryFormat::Ids => anyhow::bail!("--format ids is defined for index queries"),
        QueryFormat::Turtle | QueryFormat::Dot => {
            anyhow::bail!("--format turtle/dot requires a CONSTRUCT graph query")
        }
    }
}

/// Tree rows default to a nested JSON document when piped and an indented
/// terminal view at a terminal.
fn default_tree_format() -> QueryFormat {
    if std::io::stdout().is_terminal() {
        QueryFormat::Table
    } else {
        QueryFormat::Json
    }
}

/// Validate flat, ordered tree bindings and project them into nested JSON. The
/// input stays the direct result of a portable SELECT; nesting is the output
/// projection.
fn frame_tree(rows: &[Row]) -> anyhow::Result<Value> {
    for (i, row) in rows.iter().enumerate() {
        let missing: Vec<&str> = TREE_REQUIRED
            .iter()
            .filter(|term| !row.contains_key(**term))
            .copied()
            .collect();
        if !missing.is_empty() {
            anyhow::bail!(
                "result row {i} is missing required terms: {}",
                missing.join(", ")
            );
        }
    }

    let mut positions = HashMap::new();
    for (index, row) in rows.iter().enumerate() {
        let id = &row["id"];
        if positions.insert(id.clone(), index).is_some() {
            anyhow::bail!("duplicate tree id: {id}");
        }
    }

    let mut children = vec![Vec::new(); rows.len()];
    let mut roots = Vec::new();
    for (index, row) in rows.iter().enumerate() {
        match row.get("parent") {
            None => roots.push(index),
            Some(parent) if parent == &row["id"] => {
                anyhow::bail!("tree node {parent} is its own parent");
            }
            Some(parent) => {
                let parent_index = *positions.get(parent).ok_or_else(|| {
                    anyhow!("tree node {} references missing parent {parent}", row["id"])
                })?;
                children[parent_index].push(index);
            }
        }
    }
    if !rows.is_empty() && roots.is_empty() {
        anyhow::bail!("tree has no root (cycle detected)");
    }

    let mut visiting = vec![false; rows.len()];
    let mut emitted = vec![false; rows.len()];
    let trees = roots
        .into_iter()
        .map(|root| build_tree_node(root, rows, &children, &mut visiting, &mut emitted))
        .collect::<anyhow::Result<Vec<_>>>()?;
    if emitted.iter().any(|seen| !seen) {
        anyhow::bail!("tree contains a disconnected cycle");
    }
    Ok(Value::Array(trees))
}

fn build_tree_node(
    index: usize,
    rows: &[Row],
    children: &[Vec<usize>],
    visiting: &mut [bool],
    emitted: &mut [bool],
) -> anyhow::Result<Value> {
    if visiting[index] {
        anyhow::bail!("tree cycle at {}", rows[index]["id"]);
    }
    visiting[index] = true;
    let mut node = super::frame_row_node(&rows[index])?;
    if !children[index].is_empty() {
        let nested = children[index]
            .iter()
            .map(|child| build_tree_node(*child, rows, children, visiting, emitted))
            .collect::<anyhow::Result<Vec<_>>>()?;
        node.as_object_mut()
            .expect("frame_row_node returns an object")
            .insert("children".into(), Value::Array(nested));
    }
    visiting[index] = false;
    emitted[index] = true;
    Ok(node)
}

/// Indented terminal view: `kind: name [status]`, two spaces per depth.
fn emit_tree(tree: &Value) -> anyhow::Result<()> {
    fn visit(node: &Value, depth: usize, lines: &mut Vec<String>) {
        let name = node["name"].as_str().unwrap_or("?");
        let kind = node["kind"].as_str().unwrap_or("node");
        let suffix = node
            .get("status")
            .and_then(Value::as_str)
            .map(|value| format!(" [{value}]"))
            .unwrap_or_default();
        lines.push(format!(
            "{}{}: {}{}",
            "  ".repeat(depth),
            kind,
            name,
            suffix
        ));
        if let Some(children) = node.get("children").and_then(Value::as_array) {
            for child in children {
                visit(child, depth + 1, lines);
            }
        }
    }

    let mut lines = Vec::new();
    if let Some(roots) = tree.as_array() {
        for root in roots {
            visit(root, 0, &mut lines);
        }
    }
    if lines.is_empty() {
        return Ok(());
    }
    let mut bytes = lines.join("\n").into_bytes();
    bytes.push(b'\n');
    write_stdout(&bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn row(pairs: &[(&str, &str)]) -> Row {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn nests_children_under_canonical_parent() {
        let rows = vec![
            row(&[
                ("id", "urn:uuid:root"),
                ("name", "Charter"),
                ("kind", "charter"),
            ]),
            row(&[
                ("id", "urn:uuid:child"),
                ("parent", "urn:uuid:root"),
                ("name", "Action"),
                ("kind", "action"),
            ]),
        ];
        let tree = frame_tree(&rows).expect("tree");
        assert_eq!(tree[0]["name"], "Charter");
        assert_eq!(tree[0]["children"][0]["id"], "urn:uuid:child");
    }

    #[test]
    fn rejects_missing_parent() {
        let rows = vec![row(&[
            ("id", "urn:uuid:child"),
            ("parent", "urn:uuid:missing"),
            ("name", "Action"),
            ("kind", "action"),
        ])];
        let err = frame_tree(&rows).expect_err("orphan must fail");
        assert!(err.to_string().contains("missing parent"), "{err}");
    }
}
