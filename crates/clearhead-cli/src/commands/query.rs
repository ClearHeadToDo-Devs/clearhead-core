//! Query commands: in-process SPARQL evaluation plus the transitional graphd
//! forwarding shim.
//!
//! With the default `sparql` feature, `query raw` and locally-saved
//! `query named` execute in-process over Core's canonical dataset (see
//! [`crate::sparql`]): an ephemeral in-memory store, standard SPARQL,
//! standard result serializations. Queries run verbatim — graphd-era prefix
//! and `?STATUS_FILTER`-style parameter injection does not happen in-process,
//! so saved `.sparql` files stay portable across independent SPARQL tooling.
//!
//! Still forwarded to graphd (until `migrate-graph-consumers` /
//! `retire-graphd` land): the client-presentation families `index`, `tree`,
//! `graph`, and `chain`, the built-in registry (`named` fallback, `list`,
//! `show`), and `--status` parameter injection. In a minimal
//! `--no-default-features` build there is no query engine: `query raw` errors
//! cleanly while the unmoved registry/families keep forwarding.
//!
//! `chain` resolves a fuzzy action query to a canonical IRI here (an
//! actions-domain concern), then forwards `index chain --target <iri>`.

use std::ffi::OsString;

use crate::argparser::QueryFormat;
use crate::commands::CommandContext;
use crate::commands::verb_result::canonical_id;

impl QueryFormat {
    fn as_arg(self) -> &'static str {
        match self {
            QueryFormat::Table => "table",
            QueryFormat::Json => "json",
            QueryFormat::Ndjson => "ndjson",
            QueryFormat::Jsonld => "jsonld",
            QueryFormat::Ids => "ids",
            QueryFormat::Turtle => "turtle",
            QueryFormat::Dot => "dot",
        }
    }
}

fn push_format(args: &mut Vec<OsString>, format: Option<QueryFormat>) {
    if let Some(format) = format {
        args.push("--format".into());
        args.push(format.as_arg().into());
    }
}

/// Exec `graphd --workspace <ws> query <args…>` with inherited stdio, then
/// propagate its exit status so scripts see graphd's own result.
fn forward(ctx: &CommandContext, args: Vec<OsString>) -> anyhow::Result<()> {
    let status = clearhead_cli::graph_backend::graphd_command()
        .arg("--workspace")
        .arg(&ctx.data_dir)
        .arg("query")
        .args(&args)
        .status()
        .map_err(|e| {
            anyhow::anyhow!(
                "Failed to run clearhead-graphd: {e}. Install it or set CLEARHEAD_GRAPHD"
            )
        })?;
    if !status.success() {
        std::process::exit(status.code().unwrap_or(1));
    }
    Ok(())
}

/// Forward a simple `<subcommand> [name] [--format …]` view (index/tree/graph).
fn forward_named_view(
    ctx: &CommandContext,
    subcommand: &str,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    let mut args: Vec<OsString> = vec![subcommand.into()];
    if let Some(name) = name {
        args.push(name.into());
    }
    push_format(&mut args, format);
    forward(ctx, args)
}

pub fn raw(
    ctx: &CommandContext,
    sparql: Option<&str>,
    where_clause: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::sparql::run_raw(ctx, sparql, where_clause, format)
    }
    #[cfg(not(feature = "sparql"))]
    {
        let _ = (ctx, sparql, where_clause, format);
        anyhow::bail!(
            "this clearhead build has no query engine (compiled without the `sparql` \
             feature); use a default-features build for local SPARQL, or evaluate \
             the exported RDF dataset with any external SPARQL tool"
        )
    }
}

pub fn named(
    ctx: &CommandContext,
    name: &str,
    status: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    // In-process path (sparql feature): a locally saved query with no
    // graphd-era `--status` injection runs against the ephemeral store. The
    // built-in registry and `--status` remain on the forwarding path until
    // migrate-graph-consumers moves them.
    #[cfg(feature = "sparql")]
    if status.is_none() && crate::sparql::run_saved(ctx, name, format)? {
        return Ok(());
    }
    let mut args: Vec<OsString> = vec!["named".into(), name.into()];
    if let Some(status) = status {
        args.push("--status".into());
        args.push(status.into());
    }
    push_format(&mut args, format);
    forward(ctx, args)
}

pub fn index(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    forward_named_view(ctx, "index", name, format)
}

pub fn tree(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    forward_named_view(ctx, "tree", name, format)
}

pub fn graph(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    forward_named_view(ctx, "graph", name, format)
}

pub fn chain(ctx: &CommandContext, query: &str, format: Option<QueryFormat>) -> anyhow::Result<()> {
    let id = super::action::resolve_action_id(ctx, query)?;
    let target = format!("<{}>", canonical_id(id));

    let mut args: Vec<OsString> = vec![
        "index".into(),
        "chain".into(),
        "--target".into(),
        target.into(),
    ];
    push_format(&mut args, format);
    forward(ctx, args)
}

pub fn show(ctx: &CommandContext, name: &str) -> anyhow::Result<()> {
    forward(ctx, vec!["show".into(), name.into()])
}

pub fn list(ctx: &CommandContext) -> anyhow::Result<()> {
    forward(ctx, vec!["list".into()])
}
