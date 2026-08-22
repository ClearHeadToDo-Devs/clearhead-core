//! Query commands: in-process SPARQL evaluation, with a shrinking graphd
//! forwarding shim.
//!
//! With the default `sparql` feature, every query family runs in-process over
//! Core's canonical dataset (see [`crate::sparql`]): an ephemeral in-memory
//! store, standard SPARQL, standard result serializations. `raw` and `named`
//! evaluate directly; `index`/`tree`/`graph` add their client-presentation
//! framing; `list`/`show` read the in-process registry. The built-in views'
//! view variables (`?NOW`, `?STATUS_FILTER`, `?TARGET_ACTION`, …) are bound at
//! run time from values this crate constructs — validated terms, never raw
//! input — so the saved `.sparql` files stay standard, portable documents.
//!
//! The only thing still forwarded to graphd is a `named` invocation whose name
//! resolves nowhere in the registry; `retire-graphd` removes that last shim. In
//! a minimal `--no-default-features` build there is no query engine: `raw`
//! errors cleanly and the families forward.
//!
//! `chain` resolves a fuzzy action query to a canonical IRI here (an
//! actions-domain concern), then runs the `index chain` view with it bound to
//! `?TARGET_ACTION`.

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
/// Only the minimal `--no-default-features` build still forwards these — the
/// `sparql` build runs every family in-process.
#[cfg(not(feature = "sparql"))]
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
    // In-process path (sparql feature): a query that resolves in the registry —
    // drop-in or built-in — runs against the ephemeral store, with `--status`
    // bound as a validated `?STATUS_FILTER`. An unresolved name still forwards
    // to graphd until it is retired.
    #[cfg(feature = "sparql")]
    if crate::sparql::run_saved(ctx, name, status, format)? {
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
    #[cfg(feature = "sparql")]
    {
        crate::sparql::index::run(ctx, name, None, format)
    }
    #[cfg(not(feature = "sparql"))]
    {
        forward_named_view(ctx, "index", name, format)
    }
}

pub fn tree(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::sparql::tree::run(ctx, name, format)
    }
    #[cfg(not(feature = "sparql"))]
    {
        forward_named_view(ctx, "tree", name, format)
    }
}

pub fn graph(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::sparql::graph::run(ctx, name, format)
    }
    #[cfg(not(feature = "sparql"))]
    {
        forward_named_view(ctx, "graph", name, format)
    }
}

pub fn chain(ctx: &CommandContext, query: &str, format: Option<QueryFormat>) -> anyhow::Result<()> {
    // Resolve the fuzzy action query to a canonical IRI here (an actions-domain
    // concern), then run the `chain` index view with it bound to ?TARGET_ACTION.
    let id = super::action::resolve_action_id(ctx, query)?;
    let target = format!("<{}>", canonical_id(id));

    #[cfg(feature = "sparql")]
    {
        crate::sparql::index::run(ctx, Some("chain"), Some(&target), format)
    }
    #[cfg(not(feature = "sparql"))]
    {
        let mut args: Vec<OsString> = vec![
            "index".into(),
            "chain".into(),
            "--target".into(),
            target.into(),
        ];
        push_format(&mut args, format);
        forward(ctx, args)
    }
}

pub fn show(ctx: &CommandContext, name: &str) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::sparql::registry::show(ctx, name)
    }
    #[cfg(not(feature = "sparql"))]
    {
        forward(ctx, vec!["show".into(), name.into()])
    }
}

pub fn list(ctx: &CommandContext) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::sparql::registry::list(ctx)
    }
    #[cfg(not(feature = "sparql"))]
    {
        forward(ctx, vec!["list".into()])
    }
}
