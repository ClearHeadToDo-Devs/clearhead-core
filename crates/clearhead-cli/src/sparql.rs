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

use std::io::{IsTerminal, Write};

use anyhow::{Context as _, anyhow};
use clearhead_core::rdf::{self, WorkspaceSnapshot};
use clearhead_core::workspace::store::Workspace;
use oxigraph::io::{JsonLdProfileSet, RdfFormat, RdfSerializer};
use oxigraph::model::{Term, Triple};
use oxigraph::sparql::results::{QueryResultsFormat, QueryResultsSerializer};
use oxigraph::sparql::{QueryResults, QuerySolutionIter, QueryTripleIter, SparqlEvaluator};
use oxigraph::store::Store;

use crate::argparser::QueryFormat;
use crate::commands::CommandContext;

// ============================================================================
// Dataset assembly
// ============================================================================

/// Load every selected workspace, project it through Core, and return a fresh
/// in-memory store holding exactly the published dataset — nothing else is
/// ever loaded into it.
///
/// Mirrors the CLI's workspace fan-out: the primary workspace honors
/// `plan_path` and contributes the configured context hierarchy; additional
/// workspaces warn and are skipped on error so one bad workspace never blocks
/// the others. Each workspace lands in its own `urn:clearhead:workspace:<uuid>`
/// named graph.
pub fn build_store(ctx: &CommandContext) -> anyhow::Result<Store> {
    let store = Store::new().context("create in-memory SPARQL store")?;
    let config = ctx.workspace_config();

    for (name, path) in ctx.workspace_dirs() {
        let is_primary = path == ctx.data_dir;
        let loaded = if is_primary {
            clearhead_workspace_fs::load_workspace_model(&path, ctx.plan_override().as_deref())
        } else {
            clearhead_workspace_fs::load_workspace_model(&path, None)
        };
        let workspace = match loaded {
            Ok(workspace) => workspace,
            Err(error) if is_primary => {
                return Err(error).context("Failed to load workspace");
            }
            Err(error) => {
                tracing::warn!("Skipping workspace '{}': {error}", path.display());
                continue;
            }
        };
        let _ = name;

        let graph = rdf::workspace_graph_name(&workspace.effective_id());
        let snapshot = workspace_snapshot(&workspace);
        let model = clearhead_core::DomainModel::from(workspace);
        let mut quads =
            rdf::project_domain(&model, is_primary.then_some(&config), graph.clone())
                .map_err(|e| anyhow!("Failed to project workspace '{}': {e}", path.display()))?;
        quads.extend(
            rdf::project_workspace_snapshot(&snapshot, graph)
                .map_err(|e| anyhow!("Failed to project workspace snapshot: {e}"))?,
        );
        for quad in &quads {
            store
                .insert(quad)
                .context("insert quad into ephemeral store")?;
        }
    }
    Ok(store)
}

/// Assemble the host evidence for Core's pure workspace-snapshot projection:
/// workspace identity plus per-charter / per-action source locations, with
/// paths canonicalized here at the filesystem boundary.
fn workspace_snapshot(workspace: &Workspace) -> WorkspaceSnapshot {
    let root = workspace
        .root
        .canonicalize()
        .unwrap_or_else(|_| workspace.root.clone());
    WorkspaceSnapshot {
        workspace_id: workspace.effective_id(),
        workspace_name: workspace.effective_name(),
        root: root.to_string_lossy().into_owned(),
        charter_root: clearhead_core::charter_root(&root)
            .to_string_lossy()
            .into_owned(),
        charter_files: workspace
            .charters
            .iter()
            .filter_map(|charter| {
                charter
                    .md_file
                    .as_deref()
                    .map(|p| (charter.id, p.to_string_lossy().into_owned()))
            })
            .collect(),
        action_sources: workspace
            .charters
            .iter()
            .flat_map(|charter| {
                let source_file = charter
                    .actions_file
                    .as_deref()
                    .map(|p| p.to_string_lossy().into_owned())
                    .unwrap_or_default();
                charter.actions.iter().filter_map(move |sourced| {
                    sourced.source_metadata.as_ref().map(|meta| {
                        (
                            sourced.action.id,
                            source_file.clone(),
                            // Published lines are 1-based; tree-sitter rows are 0-based.
                            meta.root.start_row as u32 + 1,
                        )
                    })
                })
            })
            .collect(),
    }
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
            write_stdout_raw(&buffer)
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
            write_stdout(&format!(
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
        None | Some(QueryFormat::Table) => write_stdout(if value { "true" } else { "false" }),
        // SPARQL Results JSON carries ASK answers as a boolean document.
        Some(QueryFormat::Json) => {
            let mut buffer = Vec::new();
            QueryResultsSerializer::from_format(QueryResultsFormat::Json)
                .serialize_boolean_to_writer(&mut buffer, value)
                .context("serialize boolean result")?;
            write_stdout_raw(&buffer)
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
    write_stdout_raw(&bytes)
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
        return write_stdout("(no results)");
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
    write_stdout(&table.to_string())
}

/// Display form of a term in the human table: bare IRI / literal value.
fn term_display(term: &Term) -> String {
    match term {
        Term::NamedNode(node) => node.as_str().to_string(),
        Term::Literal(literal) => literal.value().to_string(),
        Term::BlankNode(node) => format!("_:{}", node.as_str()),
    }
}

fn write_stdout(value: &str) -> anyhow::Result<()> {
    let mut bytes = value.as_bytes().to_vec();
    bytes.push(b'\n');
    write_stdout_raw(&bytes)
}

/// Write to stdout, treating a closed downstream pipe as success so
/// `clearhead query … | head -n1` exits cleanly.
fn write_stdout_raw(bytes: &[u8]) -> anyhow::Result<()> {
    match std::io::stdout().lock().write_all(bytes) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::BrokenPipe => Ok(()),
        Err(error) => Err(error).context("write stdout"),
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
