//! Query commands forward to graphd's own `query` interface.
//!
//! graphd owns query execution, the named-query registry, parameter injection,
//! and rendering. The CLI maps its arguments onto `graphd query …` and execs it
//! with **inherited stdio**, so graphd's terminal-vs-pipe detection sees the
//! real stream and there is exactly one renderer. The CLI adds nothing to the
//! output — it is a pure projection.
//!
//! `chain` is the sole exception: resolving a fuzzy action query to a canonical
//! IRI is an actions-domain concern, so the CLI does that here, then forwards
//! `index chain --target <iri>`.

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
    let mut args: Vec<OsString> = vec!["raw".into()];
    if let Some(sparql) = sparql {
        args.push(sparql.into());
    }
    if let Some(where_clause) = where_clause {
        args.push("--where".into());
        args.push(where_clause.into());
    }
    push_format(&mut args, format);
    forward(ctx, args)
}

pub fn named(
    ctx: &CommandContext,
    name: &str,
    status: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
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
