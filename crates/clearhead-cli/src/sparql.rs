//! Optional in-process SPARQL evaluation — the CLI's `sparql` feature.
//!
//! One-shot, read-only evaluation over exactly the dataset Core publishes:
//!
//! ```text
//! plaintext workspace -> DomainModel -> Core RDF quads -> ephemeral store -> SPARQL
//! ```
//!
//! The store is in-memory and dropped with the command: no persistence, no
//! arbitrary-RDF loading, no federation, no endpoint proxying, and no
//! ClearHead-specific query machinery. Queries execute verbatim as standard
//! SPARQL — graphd-era prefix/placeholder injection does not happen here, so a
//! saved `.sparql` file is complete and runs unchanged in independent tooling
//! (the `rdf-publication` charter's portability contract).
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

use std::io::IsTerminal;

use anyhow::{Context as _, anyhow};
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
// Saved queries
// ============================================================================

/// Resolve a saved query by name: the project's `.clearhead/queries/` wins over
/// the user config's `queries/`, matching graphd's registry precedence. Only
/// plain `.sparql` files are considered — a saved query is a complete standard
/// SPARQL document, portable by construction.
///
/// Returns `None` when no local file matches, so the caller can fall back to
/// graphd's built-in registry until `migrate-graph-consumers` moves it.
pub fn saved_query(ctx: &CommandContext, name: &str) -> Option<String> {
    if !is_safe_query_name(name) {
        return None;
    }
    let file = format!("{name}.sparql");
    let project = ctx.data_dir.join(".clearhead").join("queries").join(&file);
    let user = crate::environment_reader::get_config_dir()
        .join("queries")
        .join(&file);
    [project, user]
        .iter()
        .find_map(|path| std::fs::read_to_string(path).ok())
}

/// A saved-query name is a plain file stem: reject anything path-shaped so a
/// command-line name can never escape the queries directories.
fn is_safe_query_name(name: &str) -> bool {
    !name.is_empty()
        && name != "."
        && name != ".."
        && !name.contains(['/', '\\'])
        && !name.contains("..")
}

// ============================================================================
// Execution
// ============================================================================

/// Execute one complete standard SPARQL query against the ephemeral store.
///
/// When the query declares no dataset of its own, the default graph is the
/// union of all named graphs (the documented ClearHead evaluator convention),
/// so workspace-agnostic queries work unchanged in single- and multi-workspace
/// stores. Queries are otherwise verbatim — no prefix or parameter injection.
///
/// Results stream against the store; keep it alive until [`emit`] returns.
pub fn execute<'a>(store: &'a Store, sparql: &str) -> anyhow::Result<QueryResults<'a>> {
    let mut prepared = SparqlEvaluator::new()
        .parse_query(sparql)
        .map_err(|e| anyhow!("SPARQL parse error: {e}"))?;
    if prepared.dataset().is_default_dataset() {
        prepared.dataset_mut().set_default_graph_as_union();
    }
    prepared
        .on_store(store)
        .execute()
        .map_err(|e| anyhow!("SPARQL evaluation error: {e}"))
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

/// Run `query named` in-process when the name resolves to a saved `.sparql`
/// file in the project or user queries directory. Returns `Ok(false)` when no
/// local query matches, so the caller can fall back to graphd's built-in
/// registry until it migrates.
pub fn run_saved(
    ctx: &CommandContext,
    name: &str,
    format: Option<QueryFormat>,
) -> anyhow::Result<bool> {
    let Some(sparql) = saved_query(ctx, name) else {
        return Ok(false);
    };
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

    #[test]
    fn saved_query_names_are_plain_file_stems() {
        for good in ["agenda", "my-query", "weekly_rollup"] {
            assert!(is_safe_query_name(good), "{good:?} should be accepted");
        }
        for bad in ["", ".", "..", "../secrets", "a/b", "a\\b", "..hidden.."] {
            assert!(!is_safe_query_name(bad), "{bad:?} should be rejected");
        }
    }
}
