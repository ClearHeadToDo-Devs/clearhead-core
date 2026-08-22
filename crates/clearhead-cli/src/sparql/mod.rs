//! Optional in-process SPARQL evaluation — the CLI's `sparql` feature.
//!
//! One-shot, read-only evaluation over exactly the dataset Core publishes:
//!
//! ```text
//! plaintext workspace -> DomainModel -> Core RDF quads -> ephemeral store -> SPARQL
//! ```
//!
//! The store is in-memory and dropped with the command: no persistence, no
//! arbitrary-RDF loading, no federation, and no endpoint proxying. Queries run
//! as complete standard SPARQL documents — no graphd-era prefix injection or
//! textual placeholder substitution — so a saved `.sparql` file stays portable
//! and runs unchanged in independent tooling (the `rdf-publication` charter's
//! portability contract).
//!
//! The one ClearHead convention is a small set of **view variables** — `?NOW`,
//! `?CUTOFF_DATE`, `?END_OF_TODAY`, `?END_OF_WEEK`, `?STATUS_FILTER`,
//! `?TARGET_ACTION` — that the built-in views leave free and this module binds
//! at run time ([`bind_time_vars`]). They live in `FILTER`/`BIND` expressions,
//! not the `SELECT` projection, so oxigraph's `substitute_variable` cannot bind
//! them; instead only these fixed placeholder names are replaced, and only with
//! terms this module constructs itself (never caller input) — so a saved query
//! stays a standard document on disk and nothing untrusted is interpolated. A
//! query that mentions no view variable is left exactly as written.
//!
//! This module owns:
//! - dataset assembly: Core's canonical domain projection plus Core's pure
//!   workspace-snapshot projection, fed host filesystem evidence;
//! - the union-default-graph convention (triple patterns without an explicit
//!   `GRAPH` clause match across every workspace named graph — see
//!   `specifications/ontology.md`), applied only when the query does not
//!   declare its own `FROM` / `FROM NAMED` dataset;
//! - standard result serializations: SPARQL Results JSON for SELECT/ASK and
//!   Turtle/JSON-LD for CONSTRUCT/DESCRIBE, plus the human table.

pub mod graph;
pub mod index;
pub mod registry;
pub mod tree;

use std::io::IsTerminal;

use anyhow::{Context as _, anyhow};
use chrono::Utc;
use oxigraph::io::{JsonLdProfileSet, RdfFormat, RdfSerializer};
use oxigraph::model::{Term, Triple};
use oxigraph::sparql::results::{QueryResultsFormat, QueryResultsSerializer};
use oxigraph::sparql::{QueryResults, QuerySolutionIter, QueryTripleIter, SparqlEvaluator};
use oxigraph::store::Store;

use crate::argparser::QueryFormat;
use crate::commands::CommandContext;
use crate::stdout::{write_stdout, write_stdout_line};

// ============================================================================
// Dataset assembly
// ============================================================================

/// Load the canonical workspace dataset (see [`crate::dataset`]) into a fresh
/// in-memory store. The store holds exactly the published quad set — nothing
/// else is ever loaded into it — and is dropped with the command.
pub fn build_store(ctx: &CommandContext) -> anyhow::Result<Store> {
    let store = Store::new().context("create in-memory SPARQL store")?;
    for quad in &crate::dataset::assemble_dataset(ctx)? {
        store
            .insert(quad)
            .context("insert quad into ephemeral store")?;
    }
    Ok(store)
}

// ============================================================================
// View variables
// ============================================================================

/// An `xsd:dateTime` literal in Turtle/SPARQL syntax, datatype spelled as a
/// full IRI so it needs no `PREFIX xsd:` in the query.
fn datetime_literal(value: &str) -> String {
    format!("\"{value}\"^^<http://www.w3.org/2001/XMLSchema#dateTime>")
}

/// Bind ClearHead's time-anchor view variables (`?NOW`, `?CUTOFF_DATE`,
/// `?END_OF_TODAY`, `?END_OF_WEEK`) to the current instant and its derived
/// day/week boundaries.
///
/// These appear in `FILTER`/`BIND` expressions, not the `SELECT` projection, so
/// oxigraph's `substitute_variable` (which only binds projected variables)
/// cannot reach them; we substitute the placeholder text instead. Only these
/// fixed names are touched, and only with literals we format ourselves — never
/// caller input — so a saved query stays a standard document on disk and
/// nothing untrusted is interpolated. `?STATUS_FILTER` / `?TARGET_ACTION` (real
/// IRIs) are bound the same way by the family runners, from validated inputs.
fn bind_time_vars(sparql: &str) -> String {
    let now = Utc::now();
    let instant = datetime_literal(&now.format("%Y-%m-%dT%H:%M:%SZ").to_string());
    let end_of_today = datetime_literal(&format!("{}T23:59:59Z", now.format("%Y-%m-%d")));
    let end_of_week = datetime_literal(&format!(
        "{}T23:59:59Z",
        (now + chrono::Duration::days(7)).format("%Y-%m-%d")
    ));
    sparql
        .replace("?NOW", &instant)
        .replace("?CUTOFF_DATE", &instant)
        .replace("?END_OF_TODAY", &end_of_today)
        .replace("?END_OF_WEEK", &end_of_week)
}

// ============================================================================
// Execution
// ============================================================================

/// Execute one complete standard SPARQL query against the ephemeral store.
///
/// When the query declares no dataset of its own, the default graph is the
/// union of all named graphs (the documented ClearHead evaluator convention),
/// so workspace-agnostic queries work unchanged in single- and multi-workspace
/// stores. Time-anchor view variables are bound ([`bind_time_vars`]); a query
/// that mentions none is unchanged.
///
/// Results stream against the store; keep it alive until [`emit`] returns.
pub fn execute<'a>(store: &'a Store, sparql: &str) -> anyhow::Result<QueryResults<'a>> {
    let sparql = bind_time_vars(sparql);
    let mut prepared = SparqlEvaluator::new()
        .parse_query(&sparql)
        .map_err(|e| anyhow!("SPARQL parse error: {e}"))?;
    if prepared.dataset().is_default_dataset() {
        prepared.dataset_mut().set_default_graph_as_union();
    }
    prepared
        .on_store(store)
        .execute()
        .map_err(|e| anyhow!("SPARQL evaluation error: {e}"))
}

/// A SELECT result row: variable name → the term's bare lexical form.
pub type Row = std::collections::HashMap<String, String>;

/// Execute a SELECT and collect its rows, stringifying each term to the bare
/// form the view families consume (IRI without `<>`, literal value, `_:`
/// blank). Rejects non-SELECT results — the families are row-shaped.
pub fn select_rows(store: &Store, sparql: &str) -> anyhow::Result<Vec<Row>> {
    match execute(store, sparql)? {
        QueryResults::Solutions(solutions) => {
            let vars: Vec<String> = solutions
                .variables()
                .iter()
                .map(|v| v.as_str().to_string())
                .collect();
            let mut rows = Vec::new();
            for solution in solutions {
                let solution = solution.context("evaluate solution")?;
                let mut row = Row::new();
                for var in &vars {
                    if let Some(term) = solution.get(var.as_str()) {
                        row.insert(var.clone(), term_display(term));
                    }
                }
                rows.push(row);
            }
            Ok(rows)
        }
        QueryResults::Boolean(_) => {
            anyhow::bail!("this query family requires a SELECT query, not ASK")
        }
        QueryResults::Graph(_) => {
            anyhow::bail!("this query family requires a SELECT query, not CONSTRUCT/DESCRIBE")
        }
    }
}

/// Terms framed as JSON numbers rather than stringified literals — shared by
/// the `index` and `tree` node projections.
pub(super) const INTEGER_TERMS: &[&str] = &["source_line", "priority"];

/// Project one SELECT row into a JSON-LD node: integer terms become JSON
/// numbers, everything else a string. Shared framing primitive for the
/// row-shaped families.
pub(super) fn frame_row_node(row: &Row) -> anyhow::Result<serde_json::Value> {
    let mut node = serde_json::Map::new();
    for (term, value) in row {
        let framed = if INTEGER_TERMS.contains(&term.as_str()) {
            let n: u64 = value
                .parse()
                .map_err(|_| anyhow!("{term} is not an integer: {value:?}"))?;
            serde_json::json!(n)
        } else {
            serde_json::Value::String(value.clone())
        };
        node.insert(term.clone(), framed);
    }
    Ok(serde_json::Value::Object(node))
}

/// Execute a CONSTRUCT/DESCRIBE and collect its RDF triples — the graph
/// family's semantic result, before any presentation projection. Rejects
/// row-shaped results.
pub fn construct_triples(store: &Store, sparql: &str) -> anyhow::Result<Vec<Triple>> {
    match execute(store, sparql)? {
        QueryResults::Graph(triples) => triples
            .collect::<Result<_, _>>()
            .map_err(|e| anyhow!("evaluate graph result: {e}")),
        QueryResults::Solutions(_) => {
            anyhow::bail!("the graph family requires a CONSTRUCT or DESCRIBE query, not SELECT")
        }
        QueryResults::Boolean(_) => {
            anyhow::bail!("the graph family requires a CONSTRUCT or DESCRIBE query, not ASK")
        }
    }
}

/// Expand a bare WHERE clause into a complete standard SELECT query — CLI
/// sugar whose result is an ordinary query document (`GRAPH ?g` so the clause
/// reaches every workspace named graph).
pub fn expand_where_clause(where_clause: &str) -> String {
    format!(
        "PREFIX actions: <https://clearhead.us/vocab/actions/v4#>\n\
         PREFIX cco: <https://www.commoncoreontologies.org/>\n\
         PREFIX bfo: <http://purl.obolibrary.org/obo/>\n\
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
         PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n\
         PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n\
         PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\n\
         PREFIX ws: <https://clearhead.us/vocab/workspace/v1#>\n\
         SELECT * WHERE {{ GRAPH ?g {{ {where_clause} }} }}"
    )
}

// ============================================================================
// Output
// ============================================================================

/// Emit query results in a standard serialization. Machine defaults prefer
/// standards: SELECT/ASK emit SPARQL Results JSON when piped; CONSTRUCT and
/// DESCRIBE emit RDF (Turtle). A human at a terminal gets a table (SELECT) or
/// plain `true`/`false` (ASK).
pub fn emit(results: QueryResults<'_>, format: Option<QueryFormat>) -> anyhow::Result<()> {
    match results {
        QueryResults::Solutions(solutions) => emit_solutions(solutions, format),
        QueryResults::Graph(triples) => emit_graph(triples, format),
        QueryResults::Boolean(value) => emit_boolean(value, format),
    }
}

fn emit_solutions(solutions: QuerySolutionIter, format: Option<QueryFormat>) -> anyhow::Result<()> {
    let format = format.unwrap_or_else(|| {
        if std::io::stdout().is_terminal() {
            QueryFormat::Table
        } else {
            QueryFormat::Json
        }
    });
    match format {
        QueryFormat::Table => emit_solutions_table(solutions),
        // SPARQL Results JSON — the standard machine-readable binding format.
        QueryFormat::Json => {
            let variables = solutions.variables().to_vec();
            let mut buffer = Vec::new();
            let mut writer = QueryResultsSerializer::from_format(QueryResultsFormat::Json)
                .serialize_solutions_to_writer(&mut buffer, variables)
                .context("start SPARQL Results JSON")?;
            for solution in solutions {
                let solution = solution.context("evaluate solution")?;
                writer
                    .serialize(solution.iter())
                    .context("serialize solution")?;
            }
            writer.finish().context("finish SPARQL Results JSON")?;
            write_stdout(&buffer)
        }
        QueryFormat::Ndjson => anyhow::bail!(
            "--format ndjson is the graphd-era index contract; raw SELECT results use --format json (SPARQL Results JSON)"
        ),
        QueryFormat::Ids => {
            anyhow::bail!("--format ids is defined for index views, not raw SELECT results")
        }
        QueryFormat::Jsonld | QueryFormat::Turtle => {
            anyhow::bail!("RDF formats apply to CONSTRUCT/DESCRIBE results, not SELECT bindings")
        }
        QueryFormat::Dot => {
            anyhow::bail!("--format dot is graph-family rendering, served by `query graph`")
        }
    }
}

fn emit_graph(triples: QueryTripleIter, format: Option<QueryFormat>) -> anyhow::Result<()> {
    let triples: Vec<Triple> = triples
        .collect::<Result<_, _>>()
        .map_err(|e| anyhow!("evaluate graph result: {e}"))?;
    match format.unwrap_or(QueryFormat::Turtle) {
        QueryFormat::Turtle => emit_rdf(&triples, RdfFormat::Turtle),
        QueryFormat::Jsonld => emit_rdf(
            &triples,
            RdfFormat::JsonLd {
                profile: JsonLdProfileSet::empty(),
            },
        ),
        QueryFormat::Table => {
            let subjects: std::collections::HashSet<_> =
                triples.iter().map(|t| t.subject.to_string()).collect();
            let predicates: std::collections::HashSet<_> =
                triples.iter().map(|t| t.predicate.to_string()).collect();
            write_stdout_line(&format!(
                "{} triples, {} subjects, {} predicates",
                triples.len(),
                subjects.len(),
                predicates.len()
            ))
        }
        QueryFormat::Json | QueryFormat::Ndjson => anyhow::bail!(
            "graph results use a standard RDF serialization: --format turtle or --format jsonld"
        ),
        QueryFormat::Ids => {
            anyhow::bail!("--format ids is defined for index views, not graph results")
        }
        QueryFormat::Dot => {
            anyhow::bail!("--format dot is graph-family rendering, served by `query graph`")
        }
    }
}

fn emit_boolean(value: bool, format: Option<QueryFormat>) -> anyhow::Result<()> {
    match format {
        None | Some(QueryFormat::Table) => write_stdout_line(if value { "true" } else { "false" }),
        // SPARQL Results JSON carries ASK answers as a boolean document.
        Some(QueryFormat::Json) => {
            let mut buffer = Vec::new();
            QueryResultsSerializer::from_format(QueryResultsFormat::Json)
                .serialize_boolean_to_writer(&mut buffer, value)
                .context("serialize boolean result")?;
            write_stdout(&buffer)
        }
        Some(other) => anyhow::bail!("--format {other:?} is not defined for ASK results"),
    }
}

fn emit_rdf(triples: &[Triple], format: RdfFormat) -> anyhow::Result<()> {
    let mut serializer = RdfSerializer::from_format(format).for_writer(Vec::new());
    for triple in triples {
        serializer
            .serialize_triple(triple)
            .context("serialize RDF triple")?;
    }
    let bytes = serializer.finish().context("finish RDF serialization")?;
    write_stdout(&bytes)
}

fn emit_solutions_table(solutions: QuerySolutionIter) -> anyhow::Result<()> {
    use comfy_table::{Cell, Color, ContentArrangement, Table, presets::UTF8_FULL};

    let variables: Vec<String> = solutions
        .variables()
        .iter()
        .map(|v| v.as_str().to_string())
        .collect();
    let mut rows: Vec<Vec<String>> = Vec::new();
    for solution in solutions {
        let solution = solution.context("evaluate solution")?;
        rows.push(
            variables
                .iter()
                .map(|v| {
                    solution
                        .get(v.as_str())
                        .map(term_display)
                        .unwrap_or_default()
                })
                .collect(),
        );
    }

    if rows.is_empty() {
        return write_stdout_line("(no results)");
    }
    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .set_content_arrangement(ContentArrangement::Dynamic);
    table.set_header(
        variables
            .iter()
            .map(|v| Cell::new(v).fg(Color::Cyan))
            .collect::<Vec<_>>(),
    );
    for row in rows {
        table.add_row(row.into_iter().map(Cell::new).collect::<Vec<_>>());
    }
    write_stdout_line(&table.to_string())
}

/// Display form of a term in the human table: bare IRI / literal value.
fn term_display(term: &Term) -> String {
    match term {
        Term::NamedNode(node) => node.as_str().to_string(),
        Term::Literal(literal) => literal.value().to_string(),
        Term::BlankNode(node) => format!("_:{}", node.as_str()),
    }
}

// ============================================================================
// Command entry points (called from commands::query)
// ============================================================================

/// Run `query raw` in-process: a complete SPARQL document, or a `--where`
/// clause expanded via [`expand_where_clause`].
pub fn run_raw(
    ctx: &CommandContext,
    sparql: Option<&str>,
    where_clause: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    let full_query = match (sparql, where_clause) {
        (Some(query), None) => query.to_string(),
        (None, Some(clause)) => expand_where_clause(clause),
        (None, None) => anyhow::bail!(
            "Provide a SPARQL query or --where clause.\n\
             Usage: clearhead query raw \"SELECT ?name WHERE {{ ... }}\"\n\
             Usage: clearhead query raw --where \"?s rdfs:label ?name\""
        ),
        (Some(_), Some(_)) => anyhow::bail!("Cannot combine positional query and --where"),
    };
    let store = build_store(ctx)?;
    emit(execute(&store, &full_query)?, format)
}

/// The action status individuals (`cco:ont00001868` objects) a `--status`
/// filter may name.
const STATUS_TERMS: &[&str] = &[
    "NotStarted",
    "InProgress",
    "Completed",
    "Blocked",
    "Cancelled",
];

/// The `?STATUS_FILTER` replacement for a `--status` value: a validated
/// `actions:` status IRI. An `actions:` prefix is optional; anything outside
/// the known set is rejected rather than interpolated, so nothing untrusted
/// reaches the query.
fn status_filter_iri(status: &str) -> anyhow::Result<String> {
    let local = status.strip_prefix("actions:").unwrap_or(status);
    if STATUS_TERMS.contains(&local) {
        Ok(format!("<{ACTIONS_STATUS_NS}{local}>"))
    } else {
        anyhow::bail!(
            "unknown --status '{status}'; expected one of: {}",
            STATUS_TERMS.join(", ")
        )
    }
}

const ACTIONS_STATUS_NS: &str = "https://clearhead.us/vocab/actions/v4#";

/// Run `query named` in-process when the name resolves in the flat registry —
/// a project or user drop-in, or a built-in ([`registry::resolve_flat`]). When
/// `status` is given it is bound to `?STATUS_FILTER` (validated, never raw).
/// Returns `Ok(false)` when nothing matches, so the caller can report an
/// unknown-query error.
pub fn run_saved(
    ctx: &CommandContext,
    name: &str,
    status: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<bool> {
    let Some(mut sparql) = registry::resolve_flat(ctx, name) else {
        return Ok(false);
    };
    if let Some(status) = status {
        sparql = sparql.replace("?STATUS_FILTER", &status_filter_iri(status)?);
    }
    let store = build_store(ctx)?;
    emit(execute(&store, &sparql)?, format)?;
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn where_clause_expands_to_complete_standard_sparql() {
        let expanded = expand_where_clause("?s rdfs:label ?name");
        assert!(expanded.starts_with("PREFIX actions:"), "{expanded}");
        assert!(
            expanded.contains("SELECT * WHERE { GRAPH ?g { ?s rdfs:label ?name } }"),
            "{expanded}"
        );
        // The expansion must parse as a complete query without any injection.
        let _prepared = SparqlEvaluator::new()
            .parse_query(&expanded)
            .expect("expanded --where clause is a complete valid query");
    }
}
