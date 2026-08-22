//! Query commands: in-process SPARQL evaluation over Core's canonical dataset.
//!
//! With the default `sparql` feature every family evaluates in an ephemeral
//! in-memory store (see [`crate::sparql`]): `raw` and `named` run directly,
//! `index`/`tree`/`graph` add their client-presentation framing, and
//! `list`/`show` read the in-process registry. The built-in views' view
//! variables (`?NOW`, `?STATUS_FILTER`, `?TARGET_ACTION`, …) are bound at run
//! time from validated terms this crate constructs — never raw input — so the
//! saved `.sparql` files stay standard, portable documents.
//!
//! A minimal `--no-default-features` build has no query engine: every command
//! reports that cleanly rather than producing partial output.
//!
//! `chain` resolves a fuzzy action query to a canonical IRI here (an
//! actions-domain concern), then runs the `index chain` view with it bound to
//! `?TARGET_ACTION`.

use crate::argparser::QueryFormat;
use crate::commands::CommandContext;

/// The error a query command returns when this build has no evaluator.
#[cfg(not(feature = "sparql"))]
fn no_query_engine() -> anyhow::Error {
    anyhow::anyhow!(
        "this clearhead build has no query engine (compiled without the `sparql` \
         feature); rebuild with default features for local SPARQL, or evaluate \
         the exported RDF dataset with any external SPARQL tool"
    )
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
        Err(no_query_engine())
    }
}

pub fn named(
    ctx: &CommandContext,
    name: &str,
    status: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        if crate::sparql::run_saved(ctx, name, status, format)? {
            return Ok(());
        }
        anyhow::bail!("No query named '{name}'. Use `clearhead query list` to see available.")
    }
    #[cfg(not(feature = "sparql"))]
    {
        let _ = (ctx, name, status, format);
        Err(no_query_engine())
    }
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
        let _ = (ctx, name, format);
        Err(no_query_engine())
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
        let _ = (ctx, name, format);
        Err(no_query_engine())
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
        let _ = (ctx, name, format);
        Err(no_query_engine())
    }
}

pub fn chain(ctx: &CommandContext, query: &str, format: Option<QueryFormat>) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        use crate::commands::verb_result::canonical_id;
        let id = super::action::resolve_action_id(ctx, query)?;
        let target = format!("<{}>", canonical_id(id));
        crate::sparql::index::run(ctx, Some("chain"), Some(&target), format)
    }
    #[cfg(not(feature = "sparql"))]
    {
        let _ = (ctx, query, format);
        Err(no_query_engine())
    }
}

pub fn show(ctx: &CommandContext, name: &str) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::sparql::registry::show(ctx, name)
    }
    #[cfg(not(feature = "sparql"))]
    {
        let _ = (ctx, name);
        Err(no_query_engine())
    }
}

pub fn list(ctx: &CommandContext) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::sparql::registry::list(ctx)
    }
    #[cfg(not(feature = "sparql"))]
    {
        let _ = ctx;
        Err(no_query_engine())
    }
}
