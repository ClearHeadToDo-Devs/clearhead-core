//! The `index` family: addressable, ordered action views.
//!
//! An index query is a portable SELECT whose rows carry the identity, display,
//! and locator terms a client needs to navigate to and act on each action. This
//! module validates those rows against the index contract, frames them into the
//! single `@context` + `@graph` JSON-LD document every consumer reads
//! (`specifications/query_output.md`), and renders the destination-aware output
//! formats. It is the CLI-side owner of what graphd used to serve — client
//! presentation, so it lives in the delivery shell, not Core.
//!
//! The wire shapes here (the `@context`, the numeric locator framing, the
//! `--format` set) are a stable contract with existing consumers such as
//! clearhead.nvim.

use std::io::IsTerminal;

use anyhow::{Context as _, anyhow};
use serde_json::{Value, json};

use super::{Row, build_store, select_rows};
use crate::argparser::QueryFormat;
use crate::commands::CommandContext;
use crate::stdout::{write_stdout, write_stdout_line};

// The index `@context` IRIs. These are a client-facing wire contract, not Core
// domain semantics, so the presentation layer owns them.
const ACTIONS_NS: &str = "https://clearhead.us/vocab/actions/v4#";
const WORKSPACE_NS: &str = "https://clearhead.us/vocab/workspace/v1#";
const CCO_NS: &str = "https://www.commoncoreontologies.org/";
const BFO_NS: &str = "http://purl.obolibrary.org/obo/";
const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";

/// Identity, display, and locator terms every index entry must carry. Sort keys
/// (`scheduled_at`, `due_date`, …) are emitted when bound but not required — an
/// undated node legitimately lacks them.
const INDEX_REQUIRED: &[&str] = &[
    "id",
    "name",
    "status",
    "source_file",
    "source_line",
    "charter_root",
];

/// Run a named index view: resolve the query, bind `?TARGET_ACTION` when a
/// chain target is supplied, execute, validate/frame, and render.
pub fn run(
    ctx: &CommandContext,
    name: Option<&str>,
    target: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    let name = name.unwrap_or("default");
    let sparql =
        super::registry::resolve_family(ctx, "index", name, super::registry::BUILT_IN_INDEX)
            .ok_or_else(|| {
                anyhow!(
                    "No index query named '{name}'. Save a .sparql file to \
                 <config>/queries/index/ or <workspace>/.clearhead/queries/index/"
                )
            })?;
    // `?TARGET_ACTION` is a canonical `<urn:uuid:…>` the caller resolved and
    // wrapped itself — a controlled, validated term, not free-form input.
    let sparql = match target {
        Some(target) => sparql.replace("?TARGET_ACTION", target),
        None => sparql,
    };

    let store = build_store(ctx)?;
    let rows = select_rows(&store, &sparql)?;
    let doc = frame_index(&rows)
        .map_err(|e| anyhow!("Query result does not satisfy the index contract: {e}"))?;
    let nodes = doc["@graph"]
        .as_array()
        .expect("frame_index always emits an @graph array");

    match format.unwrap_or_else(default_index_format) {
        QueryFormat::Table => emit_table(&rows),
        QueryFormat::Json => write_stdout_line(&serde_json::to_string_pretty(nodes)?),
        QueryFormat::Ndjson => emit_ndjson(nodes),
        QueryFormat::Jsonld => write_stdout_line(&serde_json::to_string_pretty(&doc)?),
        QueryFormat::Ids => emit_ids(nodes),
        QueryFormat::Turtle | QueryFormat::Dot => {
            anyhow::bail!("--format turtle/dot requires a CONSTRUCT graph query")
        }
    }
}

/// Index rows default to NDJSON when piped (one addressable node per line) and
/// a human table at a terminal.
fn default_index_format() -> QueryFormat {
    if std::io::stdout().is_terminal() {
        QueryFormat::Table
    } else {
        QueryFormat::Ndjson
    }
}

/// Frame ordered SELECT bindings into the index JSON-LD document. Empty
/// bindings frame as an empty `@graph` — one payload shape always.
fn frame_index(rows: &[Row]) -> anyhow::Result<Value> {
    validate_contract(rows)?;
    let nodes: Vec<Value> = rows
        .iter()
        .map(super::frame_row_node)
        .collect::<anyhow::Result<_>>()?;
    Ok(json!({ "@context": index_context(), "@graph": nodes }))
}

fn validate_contract(rows: &[Row]) -> anyhow::Result<()> {
    for (i, row) in rows.iter().enumerate() {
        let missing: Vec<&str> = INDEX_REQUIRED
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
    Ok(())
}

/// The index shape's `@context`: exactly the terms the contract emits. `id`
/// aliases `@id` so simple clients never see an `@`-key; `status` values are
/// bare enum terms typed `@vocab`; `charter_root` is deliberately unmapped
/// join-context, dropped on JSON-LD expansion but usable by direct readers.
fn index_context() -> Value {
    json!({
        "@version": 1.1,
        "actions": ACTIONS_NS,
        "bfo": BFO_NS,
        "cco": CCO_NS,
        "ws": WORKSPACE_NS,
        "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
        "xsd": XSD_NS,
        "id": "@id",
        "name": "rdfs:label",
        "status": { "@id": "cco:ont00001868", "@type": "@vocab" },
        "NotStarted": "actions:NotStarted",
        "InProgress": "actions:InProgress",
        "Completed": "actions:Completed",
        "Blocked": "actions:Blocked",
        "Cancelled": "actions:Cancelled",
        "source_file": "ws:hasSourceFile",
        "source_line": { "@id": "ws:hasSourceLine", "@type": "xsd:integer" },
        "priority": { "@id": "actions:hasPriority", "@type": "xsd:integer" },
        "scheduled_at": { "@id": "actions:hasScheduledDateTime", "@type": "xsd:dateTime" },
        "due_date": { "@id": "actions:hasDueDateTime", "@type": "xsd:dateTime" },
        "parent": { "@id": "bfo:BFO_0000050", "@type": "@id" }
    })
}

fn emit_ndjson(nodes: &[Value]) -> anyhow::Result<()> {
    let lines = nodes
        .iter()
        .map(serde_json::to_string)
        .collect::<Result<Vec<_>, _>>()
        .context("serialize index node")?;
    write_lines(&lines)
}

fn emit_ids(nodes: &[Value]) -> anyhow::Result<()> {
    let lines = nodes
        .iter()
        .map(|node| {
            node.get("id")
                .and_then(Value::as_str)
                .map(str::to_owned)
                .ok_or_else(|| anyhow!("index contract produced a non-string id"))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    write_lines(&lines)
}

fn emit_table(rows: &[Row]) -> anyhow::Result<()> {
    use comfy_table::{Cell, Color, ContentArrangement, Table, presets::UTF8_FULL};
    use std::collections::BTreeSet;

    if rows.is_empty() {
        return write_stdout_line("(no results)");
    }
    let columns: Vec<String> = rows
        .iter()
        .flat_map(|r| r.keys().cloned())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();

    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .set_content_arrangement(ContentArrangement::Dynamic);
    table.set_header(
        columns
            .iter()
            .map(|c| Cell::new(c).fg(Color::Cyan))
            .collect::<Vec<_>>(),
    );
    for row in rows {
        table.add_row(
            columns
                .iter()
                .map(|col| Cell::new(row.get(col).map(String::as_str).unwrap_or("")))
                .collect::<Vec<_>>(),
        );
    }
    write_stdout_line(&table.to_string())
}

/// Newline-joined lines with a trailing newline (empty input writes nothing) —
/// graphd's `write_stdout_lines` byte layout.
fn write_lines(lines: &[String]) -> anyhow::Result<()> {
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

    fn sample_row(uuid_suffix: &str, name: &str) -> Row {
        Row::from([
            (
                "id".into(),
                format!("urn:uuid:01900000-0000-7000-8000-{uuid_suffix}"),
            ),
            ("name".into(), name.into()),
            ("status".into(), "NotStarted".into()),
            ("source_file".into(), "charters/demo/next.actions".into()),
            ("source_line".into(), "3".into()),
            ("charter_root".into(), "/workspace/.clearhead".into()),
        ])
    }

    #[test]
    fn frames_rows_into_ordered_graph_document() {
        let mut first = sample_row("000000000001", "first action");
        first.insert("scheduled_at".into(), "2026-07-01T09:00:00Z".into());
        let second = sample_row("000000000002", "second action");

        let doc = frame_index(&[first, second]).expect("frame");
        let graph = doc["@graph"].as_array().expect("@graph array");
        assert_eq!(graph.len(), 2);
        assert_eq!(graph[0]["name"], "first action");
        // Locator line is numeric, not a stringified literal.
        assert!(graph[0]["source_line"].is_u64());
        assert_eq!(graph[0]["scheduled_at"], "2026-07-01T09:00:00Z");
    }

    #[test]
    fn missing_required_term_errors_loudly() {
        let mut row = sample_row("000000000001", "incomplete");
        row.remove("status");
        let err = frame_index(&[row]).expect_err("must fail");
        assert!(err.to_string().contains("status"), "{err}");
    }

    #[test]
    fn empty_rows_frame_as_empty_graph_document() {
        let doc = frame_index(&[]).expect("frame");
        assert!(doc.get("@context").is_some());
        assert_eq!(doc["@graph"], json!([]));
    }

    #[test]
    fn ids_are_the_node_id_one_per_line_semantics() {
        let nodes = vec![json!({ "id": "urn:uuid:a" }), json!({ "id": "urn:uuid:b" })];
        // emit_ids selects the `id` field; a node without one is a contract error.
        assert!(emit_ids(&nodes).is_ok());
        assert!(emit_ids(&[json!({ "name": "no id" })]).is_err());
    }
}
