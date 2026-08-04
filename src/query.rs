//! The human/agent-facing query interface — graphd's own command surface,
//! not a private protocol for the CLI. Resolves a query (raw SPARQL, a named
//! query, or a named index view) against a workspace and renders the result.
//!
//! This is the public query surface for direct consumers
//! (`clearhead-graphd --workspace . query index unscheduled`). Context is just
//! the workspace path plus config self-discovered from core; no CLI proxy is
//! involved.

use std::collections::HashMap;
use std::io::{IsTerminal, Write};
use std::path::{Path, PathBuf};

use anyhow::{Context as _, anyhow};
use chrono::Utc;
use clearhead_core::WorkspaceConfig;
use clearhead_core::workspace::store::load::Workspace;
use oxigraph::io::{JsonLdProfileSet, RdfFormat, RdfSerializer};
use tracing::debug;

use crate::graph::{self, GraphName, Store};

/// Everything a query needs about where to look, self-discovered by graphd
/// rather than handed in by the CLI.
pub struct QueryContext {
    /// Primary workspace root (the `--workspace` argument).
    pub workspace: PathBuf,
    /// Semantic config (tag hierarchies, additional workspaces), from core.
    pub config: WorkspaceConfig,
    /// XDG config dir, for user-saved queries under `queries/`.
    pub config_dir: PathBuf,
}

/// Explicit output format. Query families choose their own machine default:
/// ordinary SELECT rows use JSON, while the index family uses NDJSON.
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum Format {
    Table,
    Json,
    Ndjson,
    Jsonld,
    Turtle,
    Dot,
}

fn default_rows_format() -> Format {
    if std::io::stdout().is_terminal() {
        Format::Table
    } else {
        Format::Json
    }
}

fn default_graph_format() -> Format {
    if std::io::stdout().is_terminal() {
        Format::Table
    } else {
        Format::Jsonld
    }
}

fn default_index_format() -> Format {
    if std::io::stdout().is_terminal() {
        Format::Table
    } else {
        Format::Ndjson
    }
}

fn default_tree_format() -> Format {
    if std::io::stdout().is_terminal() {
        Format::Table
    } else {
        Format::Json
    }
}

// =============================================================================
// Store construction
// =============================================================================

/// Load the primary workspace (and any additional workspaces) into a fresh
/// in-memory store ready for SPARQL.
pub fn build_store(workspace: &Path, config: &WorkspaceConfig) -> anyhow::Result<Store> {
    let store = graph::create_store().map_err(|e| anyhow!("Failed to create store: {e}"))?;

    let primary =
        Workspace::load(workspace).map_err(|e| anyhow!("Failed to load workspace: {e}"))?;
    let graph_name = GraphName::NamedNode(graph::workspace_graph_uri(&primary.effective_id()));
    graph::insert_workspace_metadata(&store, &primary, graph_name.clone())
        .map_err(|e| anyhow!("Failed to insert workspace metadata: {e}"))?;
    let model = clearhead_core::DomainModel::from(primary);
    graph::load_domain_model(&store, &model, Some(config), graph_name)
        .map_err(|e| anyhow!("Failed to load domain model: {e}"))?;

    for path_str in &config.additional_workspaces {
        if let Err(e) = load_additional(&store, Path::new(path_str)) {
            tracing::warn!("Skipping additional workspace '{}': {e}", path_str);
        }
    }
    Ok(store)
}

fn load_additional(store: &Store, path: &Path) -> anyhow::Result<()> {
    if !path.exists() {
        anyhow::bail!(
            "Additional workspace path does not exist: {}",
            path.display()
        );
    }
    let workspace = Workspace::load(path)
        .map_err(|e| anyhow!("Failed to load workspace at {}: {e}", path.display()))?;
    let graph_name = GraphName::NamedNode(graph::workspace_graph_uri(&workspace.effective_id()));
    graph::insert_workspace_metadata(store, &workspace, graph_name.clone()).map_err(|e| {
        anyhow!(
            "Failed to insert workspace metadata for {}: {e}",
            path.display()
        )
    })?;
    let model = clearhead_core::DomainModel::from(workspace);
    graph::load_domain_model(store, &model, None, graph_name)
        .map_err(|e| anyhow!("Failed to insert workspace {}: {e}", path.display()))
}

// =============================================================================
// Parameter injection
// =============================================================================

/// Prepend standard PREFIX declarations for any prefix not already declared, so
/// ad-hoc queries can use short names without knowing the full IRIs.
fn inject_prefixes(sparql: &str) -> String {
    let lower = sparql.to_lowercase();
    const STANDARD: &[(&str, &str)] = &[
        ("actions", "https://clearhead.us/vocab/actions/v4#"),
        ("cco", "https://www.commoncoreontologies.org/"),
        ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
        ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        ("bfo", "http://purl.obolibrary.org/obo/"),
        ("xsd", "http://www.w3.org/2001/XMLSchema#"),
    ];
    let missing: String = STANDARD
        .iter()
        .filter(|(p, _)| !lower.contains(&format!("prefix {}:", p)))
        .map(|(p, iri)| format!("PREFIX {}: <{}>\n", p, iri))
        .collect();
    if missing.is_empty() {
        sparql.to_string()
    } else {
        format!("{}{}", missing, sparql)
    }
}

/// Replace well-known placeholders before execution: time cutoffs (?NOW,
/// ?END_OF_TODAY, ?END_OF_WEEK), ?STATUS_FILTER, and ?TARGET_ACTION (resolved
/// by the caller — the CLI turns a fuzzy action query into a canonical IRI).
fn inject_params(sparql: &str, status: Option<&str>, target: Option<&str>) -> String {
    let sparql = inject_prefixes(sparql);
    let now_dt = Utc::now();
    let now = now_dt.format("%Y-%m-%dT%H:%M:%SZ").to_string();
    let datetime = format!("\"{}\"^^xsd:dateTime", now);
    let end_of_today = format!("\"{}T23:59:59Z\"^^xsd:dateTime", now_dt.format("%Y-%m-%d"));
    let end_of_week = format!(
        "\"{}T23:59:59Z\"^^xsd:dateTime",
        (now_dt + chrono::Duration::days(7)).format("%Y-%m-%d")
    );
    let mut out = sparql
        .replace("?NOW", &datetime)
        .replace("?CUTOFF_DATE", &datetime)
        .replace("?END_OF_TODAY", &end_of_today)
        .replace("?END_OF_WEEK", &end_of_week);
    if let Some(iri) = status {
        out = out.replace("?STATUS_FILTER", iri);
    }
    if let Some(iri) = target {
        out = out.replace("?TARGET_ACTION", iri);
    }
    out
}

// =============================================================================
// Named-query registry
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum QuerySource {
    BuiltIn,
    User,
    Project,
}

impl std::fmt::Display for QuerySource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QuerySource::BuiltIn => write!(f, "built-in"),
            QuerySource::User => write!(f, "user"),
            QuerySource::Project => write!(f, "project"),
        }
    }
}

struct NamedQuery {
    sparql: String,
    source: QuerySource,
}

const BUILT_IN_INDEX_QUERIES: &[(&str, &str)] = &[
    ("agenda", include_str!("queries/index/agenda.sparql")),
    ("chain", include_str!("queries/index/chain.sparql")),
    ("default", include_str!("queries/index/default.sparql")),
    (
        "unscheduled",
        include_str!("queries/index/unscheduled.sparql"),
    ),
    ("weekly", include_str!("queries/index/weekly.sparql")),
];

const BUILT_IN_TREE_QUERIES: &[(&str, &str)] =
    &[("work-map", include_str!("queries/tree/work-map.sparql"))];

const BUILT_IN_GRAPH_QUERIES: &[(&str, &str)] = &[(
    "dependencies",
    include_str!("queries/graph/dependencies.sparql"),
)];

const BUILT_IN_QUERIES: &[(&str, &str)] = &[
    (
        "actions-by-phase",
        include_str!("queries/actions-by-phase.sparql"),
    ),
    ("all-plans", include_str!("queries/all-plans.sparql")),
    (
        "all-plans-simple",
        include_str!("queries/all-plans-simple.sparql"),
    ),
    (
        "completion-velocity",
        include_str!("queries/completion-velocity.sparql"),
    ),
    (
        "dependency-chain",
        include_str!("queries/dependency-chain.sparql"),
    ),
    (
        "high-priority",
        include_str!("queries/high-priority.sparql"),
    ),
    ("next-actions", include_str!("queries/next-actions.sparql")),
    (
        "orphaned-actions",
        include_str!("queries/orphaned-actions.sparql"),
    ),
    (
        "overdue-tasks",
        include_str!("queries/overdue-tasks.sparql"),
    ),
    ("open-plans", include_str!("queries/open-plans.sparql")),
    (
        "plans-with-contexts",
        include_str!("queries/plans-with-contexts.sparql"),
    ),
];

fn resolve_named_queries(cx: &QueryContext) -> HashMap<String, NamedQuery> {
    let mut queries: HashMap<String, NamedQuery> = HashMap::new();
    for (name, sparql) in BUILT_IN_QUERIES {
        queries.insert(
            name.to_string(),
            NamedQuery {
                sparql: sparql.to_string(),
                source: QuerySource::BuiltIn,
            },
        );
    }
    scan_query_dir(
        &cx.config_dir.join("queries"),
        QuerySource::User,
        &mut queries,
    );
    let project_dir = cx.workspace.join(".clearhead").join("queries");
    scan_query_dir(&project_dir, QuerySource::Project, &mut queries);
    queries
}

fn scan_query_dir(dir: &Path, source: QuerySource, out: &mut HashMap<String, NamedQuery>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("sparql")
            && let Some(stem) = path.file_stem().and_then(|s| s.to_str())
            && let Ok(sparql) = std::fs::read_to_string(&path)
        {
            out.insert(stem.to_string(), NamedQuery { sparql, source });
        }
    }
}

fn resolve_typed_queries(cx: &QueryContext, type_name: &str) -> HashMap<String, NamedQuery> {
    let mut queries = HashMap::new();
    scan_query_dir(
        &cx.config_dir.join("queries").join(type_name),
        QuerySource::User,
        &mut queries,
    );
    scan_query_dir(
        &cx.workspace
            .join(".clearhead")
            .join("queries")
            .join(type_name),
        QuerySource::Project,
        &mut queries,
    );
    queries
}

fn scan_all_typed_queries(cx: &QueryContext) -> Vec<(String, String, NamedQuery)> {
    let dirs: Vec<(PathBuf, QuerySource)> = vec![
        (cx.config_dir.join("queries"), QuerySource::User),
        (
            cx.workspace.join(".clearhead").join("queries"),
            QuerySource::Project,
        ),
    ];
    let mut result = Vec::new();
    for (base, source) in dirs {
        let Ok(entries) = std::fs::read_dir(&base) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let Some(type_name) = path.file_name().and_then(|n| n.to_str()).map(String::from)
                else {
                    continue;
                };
                let mut typed = HashMap::new();
                scan_query_dir(&path, source, &mut typed);
                for (name, query) in typed {
                    result.push((type_name.clone(), name, query));
                }
            }
        }
    }
    result
}

// =============================================================================
// Runners
// =============================================================================

/// Raw SPARQL (positional) or a `--where` clause.
pub fn run_raw(
    cx: &QueryContext,
    sparql: Option<&str>,
    where_clause: Option<&str>,
    format: Option<Format>,
) -> anyhow::Result<()> {
    let full_query = match (sparql, where_clause) {
        (Some(q), None) => q.to_string(),
        (None, Some(w)) => {
            debug!(where_clause = %w, "Building raw WHERE query");
            graph::build_raw_where_query(w)
        }
        (None, None) => anyhow::bail!(
            "Provide a SPARQL query or --where clause.\n\
             Usage: graphd query \"SELECT ?name WHERE {{ ... }}\"\n\
             Usage: graphd query --where \"?s rdfs:label ?name\""
        ),
        (Some(_), Some(_)) => anyhow::bail!("Cannot combine positional query and --where"),
    };

    let store = build_store(&cx.workspace, &cx.config)?;
    let rows = graph::query_raw(&store, &inject_params(&full_query, None, None))
        .map_err(|e| anyhow!("SPARQL query failed: {e}"))?;
    emit_rows(&rows, format)
}

/// A freeform named query from the registry.
pub fn run_named(
    cx: &QueryContext,
    name: &str,
    status: Option<&str>,
    format: Option<Format>,
) -> anyhow::Result<()> {
    let queries = resolve_named_queries(cx);
    let named = queries.get(name).ok_or_else(|| {
        anyhow!("No query named '{name}'. Use `graphd query list` to see available.")
    })?;
    let store = build_store(&cx.workspace, &cx.config)?;
    let rows = graph::query_raw(&store, &inject_params(&named.sparql, status, None))
        .map_err(|e| anyhow!("SPARQL query failed: {e}"))?;
    emit_rows(&rows, format)
}

/// A named index view. `target` supplies a canonical IRI for ?TARGET_ACTION
/// in chain-style views.
pub fn run_index(
    cx: &QueryContext,
    name: Option<&str>,
    target: Option<&str>,
    format: Option<Format>,
) -> anyhow::Result<()> {
    let name = name.unwrap_or("default");
    let sparql = resolve_typed_queries(cx, "index")
        .remove(name)
        .map(|q| q.sparql)
        .or_else(|| {
            BUILT_IN_INDEX_QUERIES
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, s)| s.to_string())
        })
        .ok_or_else(|| {
            anyhow!(
                "No index query named '{name}'. Save a .sparql file to \
                 <config>/queries/index/ or <workspace>/.clearhead/queries/index/"
            )
        })?;

    let store = build_store(&cx.workspace, &cx.config)?;
    let rows = graph::query_raw(&store, &inject_params(&sparql, None, target))
        .map_err(|e| anyhow!("SPARQL query failed: {e}"))?;

    let doc = graph::frame_index(&rows)
        .map_err(|e| anyhow!("Query result does not satisfy the index contract: {e}"))?;
    let nodes = doc["@graph"]
        .as_array()
        .expect("frame_index always emits an @graph array");

    match format.unwrap_or_else(default_index_format) {
        Format::Table => emit_table(&rows),
        Format::Json => write_stdout(&serde_json::to_string_pretty(nodes).context("serialize")?),
        Format::Ndjson => emit_ndjson(nodes),
        Format::Jsonld => write_stdout(&serde_json::to_string_pretty(&doc).context("serialize")?),
        Format::Turtle | Format::Dot => {
            anyhow::bail!("--format turtle/dot requires a CONSTRUCT graph query")
        }
    }
}

/// A named tree view: portable SELECT bindings validated as canonical-id and
/// parent-linked nodes, then projected as nested JSON or an indented terminal
/// tree.
pub fn run_tree(
    cx: &QueryContext,
    name: Option<&str>,
    format: Option<Format>,
) -> anyhow::Result<()> {
    let name = name.unwrap_or("work-map");
    let sparql = resolve_typed_queries(cx, "tree")
        .remove(name)
        .map(|q| q.sparql)
        .or_else(|| {
            BUILT_IN_TREE_QUERIES
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, sparql)| sparql.to_string())
        })
        .ok_or_else(|| {
            anyhow!(
                "No tree query named '{name}'. Save a .sparql file to \
                 <config>/queries/tree/ or <workspace>/.clearhead/queries/tree/"
            )
        })?;

    let store = build_store(&cx.workspace, &cx.config)?;
    let rows = graph::query_raw(&store, &inject_params(&sparql, None, None))
        .map_err(|e| anyhow!("SPARQL query failed: {e}"))?;
    let tree = graph::frame_tree(&rows)
        .map_err(|e| anyhow!("Query result does not satisfy the tree contract: {e}"))?;

    match format.unwrap_or_else(default_tree_format) {
        Format::Table => emit_tree(&tree),
        Format::Json => write_stdout(&serde_json::to_string_pretty(&tree).context("serialize")?),
        Format::Ndjson => {
            anyhow::bail!("--format ndjson is not defined for tree queries; use json")
        }
        Format::Jsonld => {
            anyhow::bail!("--format jsonld is not defined for tree queries; use json")
        }
        Format::Turtle | Format::Dot => {
            anyhow::bail!("--format turtle/dot requires a CONSTRUCT graph query")
        }
    }
}

/// A named graph view backed by a complete standard CONSTRUCT query.
pub fn run_graph(
    cx: &QueryContext,
    name: Option<&str>,
    format: Option<Format>,
) -> anyhow::Result<()> {
    let name = name.unwrap_or("dependencies");
    let sparql = resolve_typed_queries(cx, "graph")
        .remove(name)
        .map(|q| q.sparql)
        .or_else(|| {
            BUILT_IN_GRAPH_QUERIES
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, sparql)| sparql.to_string())
        })
        .ok_or_else(|| {
            anyhow!(
                "No graph query named '{name}'. Save a .sparql file to \
                 <config>/queries/graph/ or <workspace>/.clearhead/queries/graph/"
            )
        })?;

    let store = build_store(&cx.workspace, &cx.config)?;
    let triples = graph::query_graph(&store, &inject_params(&sparql, None, None))
        .map_err(|e| anyhow!("SPARQL graph query failed: {e}"))?;
    match format.unwrap_or_else(default_graph_format) {
        Format::Table => emit_graph_summary(&triples),
        Format::Jsonld => emit_rdf(
            &triples,
            RdfFormat::JsonLd {
                profile: JsonLdProfileSet::empty(),
            },
        ),
        Format::Turtle => emit_rdf(&triples, RdfFormat::Turtle),
        Format::Dot => write_stdout_raw(graph::frame_dot(&triples).as_bytes()),
        Format::Json | Format::Ndjson => {
            anyhow::bail!("graph queries require an RDF format: use jsonld or turtle")
        }
    }
}

pub fn list(cx: &QueryContext) -> anyhow::Result<()> {
    use comfy_table::{Cell, Color, ContentArrangement, Table, presets::UTF8_FULL};
    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .set_content_arrangement(ContentArrangement::Dynamic);
    table.set_header(vec![
        Cell::new("NAME").fg(Color::Cyan),
        Cell::new("TYPE").fg(Color::Cyan),
        Cell::new("SOURCE").fg(Color::Cyan),
    ]);

    let queries = resolve_named_queries(cx);
    let mut root_names: Vec<&String> = queries.keys().collect();
    root_names.sort();
    for name in root_names {
        table.add_row(vec![
            Cell::new(name),
            Cell::new("—"),
            Cell::new(queries[name].source.to_string()),
        ]);
    }
    for (name, _) in BUILT_IN_INDEX_QUERIES {
        table.add_row(vec![
            Cell::new(name),
            Cell::new("index"),
            Cell::new("built-in"),
        ]);
    }
    for (name, _) in BUILT_IN_TREE_QUERIES {
        table.add_row(vec![
            Cell::new(name),
            Cell::new("tree"),
            Cell::new("built-in"),
        ]);
    }
    for (name, _) in BUILT_IN_GRAPH_QUERIES {
        table.add_row(vec![
            Cell::new(name),
            Cell::new("graph"),
            Cell::new("built-in"),
        ]);
    }
    let mut typed = scan_all_typed_queries(cx);
    typed.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
    for (type_name, name, q) in &typed {
        table.add_row(vec![
            Cell::new(name),
            Cell::new(type_name),
            Cell::new(q.source.to_string()),
        ]);
    }
    write_stdout(&table.to_string())
}

pub fn show(cx: &QueryContext, name: &str) -> anyhow::Result<()> {
    let sparql = resolve_typed_queries(cx, "index")
        .remove(name)
        .map(|q| q.sparql)
        .or_else(|| {
            BUILT_IN_INDEX_QUERIES
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, s)| s.to_string())
        })
        .or_else(|| {
            resolve_typed_queries(cx, "tree")
                .remove(name)
                .map(|q| q.sparql)
        })
        .or_else(|| {
            BUILT_IN_TREE_QUERIES
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, sparql)| sparql.to_string())
        })
        .or_else(|| {
            resolve_typed_queries(cx, "graph")
                .remove(name)
                .map(|q| q.sparql)
        })
        .or_else(|| {
            BUILT_IN_GRAPH_QUERIES
                .iter()
                .find(|(n, _)| *n == name)
                .map(|(_, sparql)| sparql.to_string())
        })
        .or_else(|| resolve_named_queries(cx).remove(name).map(|q| q.sparql))
        .ok_or_else(|| {
            anyhow!("No query named '{name}'. Use `graphd query list` to see available.")
        })?;
    write_stdout_raw(sparql.as_bytes())
}

// =============================================================================
// Output
// =============================================================================

fn emit_rows(rows: &[HashMap<String, String>], format: Option<Format>) -> anyhow::Result<()> {
    match format.unwrap_or_else(default_rows_format) {
        Format::Json => write_stdout(&serde_json::to_string_pretty(rows).context("serialize")?),
        Format::Ndjson => {
            let lines = rows
                .iter()
                .map(serde_json::to_string)
                .collect::<Result<Vec<_>, _>>()
                .context("serialize")?;
            write_stdout_lines(&lines)
        }
        Format::Table => emit_table(rows),
        Format::Jsonld | Format::Turtle | Format::Dot => {
            anyhow::bail!("graph formats require a shaped graph query such as `query graph`")
        }
    }
}

fn emit_graph_summary(triples: &[oxigraph::model::Triple]) -> anyhow::Result<()> {
    use std::collections::HashSet;
    let subjects: HashSet<_> = triples
        .iter()
        .map(|triple| triple.subject.to_string())
        .collect();
    let predicates: HashSet<_> = triples
        .iter()
        .map(|triple| triple.predicate.to_string())
        .collect();
    write_stdout(&format!(
        "{} triples, {} subjects, {} predicates",
        triples.len(),
        subjects.len(),
        predicates.len()
    ))
}

fn emit_rdf(triples: &[oxigraph::model::Triple], format: RdfFormat) -> anyhow::Result<()> {
    let mut serializer = RdfSerializer::from_format(format).for_writer(Vec::new());
    for triple in triples {
        serializer
            .serialize_triple(triple)
            .context("serialize RDF triple")?;
    }
    let bytes = serializer.finish().context("finish RDF serialization")?;
    write_stdout_raw(&bytes)
}

fn emit_tree(tree: &serde_json::Value) -> anyhow::Result<()> {
    fn visit(node: &serde_json::Value, depth: usize, lines: &mut Vec<String>) {
        let name = node["name"].as_str().unwrap_or("?");
        let kind = node["kind"].as_str().unwrap_or("node");
        let status = node.get("status").and_then(|value| value.as_str());
        let suffix = status
            .map(|value| format!(" [{value}]"))
            .unwrap_or_default();
        lines.push(format!(
            "{}{}: {}{}",
            "  ".repeat(depth),
            kind,
            name,
            suffix
        ));
        if let Some(children) = node.get("children").and_then(|value| value.as_array()) {
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
    write_stdout_lines(&lines)
}

fn emit_ndjson(nodes: &[serde_json::Value]) -> anyhow::Result<()> {
    let lines = nodes
        .iter()
        .map(serde_json::to_string)
        .collect::<Result<Vec<_>, _>>()
        .context("serialize")?;
    write_stdout_lines(&lines)
}

fn write_stdout(value: &str) -> anyhow::Result<()> {
    let mut bytes = value.as_bytes().to_vec();
    bytes.push(b'\n');
    write_stdout_raw(&bytes)
}

fn write_stdout_lines(lines: &[String]) -> anyhow::Result<()> {
    let mut output = lines.join("\n").into_bytes();
    if !lines.is_empty() {
        output.push(b'\n');
    }
    write_stdout_raw(&output)
}

fn write_stdout_raw(bytes: &[u8]) -> anyhow::Result<()> {
    match std::io::stdout().lock().write_all(bytes) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::BrokenPipe => Ok(()),
        Err(error) => Err(error).context("write stdout"),
    }
}

fn emit_table(rows: &[HashMap<String, String>]) -> anyhow::Result<()> {
    use comfy_table::{Cell, Color, ContentArrangement, Table, presets::UTF8_FULL};
    use std::collections::BTreeSet;

    if rows.is_empty() {
        return write_stdout("(no results)");
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
                .map(|col| Cell::new(row.get(col).map(|s| s.as_str()).unwrap_or("")))
                .collect::<Vec<_>>(),
        );
    }
    write_stdout(&table.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn end_of_today_injects_end_of_day() {
        let result = inject_params("FILTER(?x <= ?END_OF_TODAY)", None, None);
        let today = Utc::now().format("%Y-%m-%d").to_string();
        assert!(
            result.contains(&format!("\"{today}T23:59:59Z\"^^xsd:dateTime")),
            "{result}"
        );
    }

    #[test]
    fn status_filter_replaced_when_provided() {
        let result = inject_params(
            "FILTER(?s = ?STATUS_FILTER)",
            Some("<actions:InProgress>"),
            None,
        );
        assert!(result.contains("<actions:InProgress>"), "{result}");
        assert!(!result.contains("?STATUS_FILTER"), "{result}");
    }

    #[test]
    fn target_action_replaced_when_provided() {
        let result = inject_params("?TARGET_ACTION", None, Some("<urn:uuid:abc>"));
        assert!(result.contains("<urn:uuid:abc>"), "{result}");
        assert!(!result.contains("?TARGET_ACTION"), "{result}");
    }

    #[test]
    fn prefixes_injected_only_when_missing() {
        let already = "PREFIX actions: <x>\nSELECT * WHERE {}";
        assert_eq!(
            inject_prefixes(already).matches("PREFIX actions:").count(),
            1
        );
    }
}
