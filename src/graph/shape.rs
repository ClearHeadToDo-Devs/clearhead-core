//! Response shapes: the contract side of query output.
//!
//! A shape declares the node properties its consumers require and frames raw
//! SELECT bindings into the single JSON-LD payload every consumer reads
//! (specifications/query_output.md). The shape validates the projection
//! against its contract — it never composes, injects, or repairs.
//!
//! Row order is the query's ORDER BY, preserved as `@graph` array position;
//! sort keys ride along as node properties so ordering survives an RDF
//! round-trip. `index` is the first shape: ordered, display-labeled,
//! locator-bearing entries, each addressable by canonical `@id`. Future
//! shapes (`table` for aggregates, `graph` for networks) slot alongside.

use super::{ACTIONS_NS, CCO_NS, GraphError, Result, WORKSPACE_NS, XSD_NS};
use serde_json::{Map, Value, json};
use std::collections::HashMap;

type Row = HashMap<String, String>;

/// Identity, display, and locator terms every index entry must carry.
/// Sort keys (`scheduled_at`, `due_date`, …) are emitted when bound but not
/// required — an undated node legitimately lacks them.
pub const INDEX_REQUIRED: &[&str] =
    &["id", "name", "status", "source_file", "source_line", "charter_root"];

/// Frame ordered SELECT bindings into the index JSON-LD document.
///
/// Empty bindings frame as an empty `@graph` — one payload shape always.
pub fn frame_index(rows: &[Row]) -> Result<Value> {
    validate_contract(rows, INDEX_REQUIRED)?;
    let nodes: Vec<Value> = rows.iter().map(index_node).collect::<Result<_>>()?;
    Ok(json!({ "@context": index_context(), "@graph": nodes }))
}

fn validate_contract(rows: &[Row], required: &[&str]) -> Result<()> {
    for (i, row) in rows.iter().enumerate() {
        let missing: Vec<&str> = required
            .iter()
            .filter(|term| !row.contains_key(**term))
            .copied()
            .collect();
        if !missing.is_empty() {
            return Err(GraphError::Contract(format!(
                "index row {i} is missing required terms: {}",
                missing.join(", ")
            )));
        }
    }
    Ok(())
}

fn index_node(row: &Row) -> Result<Value> {
    let mut node = Map::new();
    for (term, value) in row {
        let framed = if term == "source_line" {
            let line: u64 = value.parse().map_err(|_| {
                GraphError::Contract(format!("source_line is not an integer: {value:?}"))
            })?;
            json!(line)
        } else {
            Value::String(value.clone())
        };
        node.insert(term.clone(), framed);
    }
    Ok(Value::Object(node))
}

/// The index shape's `@context`: exactly the terms the contract emits.
///
/// `id` aliases `@id`, so simple clients never see an `@`-key. `status`
/// values are bare enum terms typed `@vocab`, expanding through the five
/// status term definitions to the ontology individuals. `charter_root` is
/// deliberately unmapped: it is join-context denormalized from the workspace
/// node, not a property of the action — JSON-LD processors drop it on
/// expansion, direct readers use it as a locator.
fn index_context() -> Value {
    json!({
        "@version": 1.1,
        "actions": ACTIONS_NS,
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
        "scheduled_at": { "@id": "actions:hasScheduledDateTime", "@type": "xsd:dateTime" },
        "due_date": { "@id": "actions:hasDueDateTime", "@type": "xsd:dateTime" }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_row(uuid_suffix: &str, name: &str) -> Row {
        HashMap::from([
            ("id".into(), format!("urn:uuid:01900000-0000-7000-8000-{uuid_suffix}")),
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

        assert!(doc.get("@context").is_some());
        let graph = doc["@graph"].as_array().expect("@graph array");
        assert_eq!(graph.len(), 2);
        // ORDER BY survives as array position.
        assert_eq!(graph[0]["name"], "first action");
        assert_eq!(graph[1]["name"], "second action");
        assert_eq!(graph[0]["id"], "urn:uuid:01900000-0000-7000-8000-000000000001");
        // Locator line is numeric, not a stringified literal.
        assert!(graph[0]["source_line"].is_u64());
        // Sort key travels as a node property.
        assert_eq!(graph[0]["scheduled_at"], "2026-07-01T09:00:00Z");
    }

    #[test]
    fn missing_required_term_errors_loudly() {
        let mut row = sample_row("000000000001", "incomplete");
        row.remove("status");
        row.remove("charter_root");

        let err = frame_index(&[row]).expect_err("must fail");
        let msg = err.to_string();
        assert!(msg.contains("charter_root"), "unexpected: {msg}");
        assert!(msg.contains("status"), "unexpected: {msg}");
        assert!(!msg.contains("name"), "unexpected: {msg}");
    }

    #[test]
    fn optional_sort_keys_are_not_required() {
        let undated = sample_row("000000000001", "undated");
        let doc = frame_index(&[undated]).expect("frame");
        let node = &doc["@graph"][0];
        assert!(node.get("scheduled_at").is_none());
        assert!(node.get("due_date").is_none());
    }

    #[test]
    fn empty_rows_frame_as_empty_graph_document() {
        let doc = frame_index(&[]).expect("frame");
        assert!(doc.get("@context").is_some());
        assert_eq!(doc["@graph"], json!([]));
    }

    #[test]
    fn non_integer_source_line_errors_loudly() {
        let mut row = sample_row("000000000001", "bad line");
        row.insert("source_line".into(), "not-a-number".into());
        let err = frame_index(&[row]).expect_err("must fail");
        assert!(err.to_string().contains("source_line"));
    }

    #[test]
    fn context_expands_status_through_vocab_terms() {
        let doc = frame_index(&[sample_row("000000000001", "any")]).expect("frame");
        let ctx = &doc["@context"];
        // Bare status terms only expand through term definitions under @vocab.
        assert_eq!(ctx["status"]["@type"], "@vocab");
        for term in ["NotStarted", "InProgress", "Completed", "Blocked", "Cancelled"] {
            let iri = ctx[term].as_str().expect("status term defined");
            assert_eq!(iri, format!("actions:{term}"));
        }
        // Identity aliasing: consumers address nodes by plain `id`.
        assert_eq!(ctx["id"], "@id");
    }
}
