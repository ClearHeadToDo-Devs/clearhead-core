//! Query commands: in-process SPARQL evaluation over Core's canonical dataset.
//!
//! With the default `sparql` feature every family evaluates in an ephemeral
//! in-memory store (see [`crate::query::sparql`]): `raw` and `named` run directly,
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
use crate::cli::CommandContext;

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
        crate::query::sparql::run_raw(ctx, sparql, where_clause, format)
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
        if crate::query::sparql::run_saved(ctx, name, status, format)? {
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
    charter: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        let charter_target = charter
            .map(|query| resolve_charter_target(ctx, query))
            .transpose()?;
        crate::query::sparql::index::run(ctx, name, None, charter_target.as_deref(), format)
    }
    #[cfg(not(feature = "sparql"))]
    {
        let _ = (ctx, name, charter, format);
        Err(no_query_engine())
    }
}

/// Resolve a `--charter` reference to the `<urn:uuid:…>` term an index view's
/// `#CHARTER_FILTER#` marker binds against. Only a charter with a
/// document-declared id is a durable join key — see
/// `charter-document-without-id` in `clearhead doctor` — so one loaded with
/// any other [`clearhead_core::workspace::CharterIdSource`] is a clear error
/// naming the same fix `doctor` does, not a silently ephemeral filter.
#[cfg(feature = "sparql")]
fn resolve_charter_target(ctx: &CommandContext, query: &str) -> anyhow::Result<String> {
    use crate::cli::verb_result::canonical_id;
    use clearhead_core::workspace::CharterIdSource;

    let (charter, _ws_root) = super::action::resolve_charter_across_workspaces(ctx, query)?;
    if charter.id_source != CharterIdSource::Document {
        let subject = charter.alias.as_deref().unwrap_or(&charter.title);
        anyhow::bail!(
            "charter '{subject}' declares no id, so `--charter` cannot address it durably; \
             run `clearhead normalize file <charter.md> --write` to stamp one"
        );
    }
    Ok(format!("<{}>", canonical_id(charter.id)))
}

pub fn tree(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    #[cfg(feature = "sparql")]
    {
        crate::query::sparql::tree::run(ctx, name, format)
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
        crate::query::sparql::graph::run(ctx, name, format)
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
        use crate::cli::verb_result::canonical_id;
        let id = super::action::resolve_action_id(ctx, query)?;
        let target = format!("<{}>", canonical_id(id));
        crate::query::sparql::index::run(ctx, Some("chain"), Some(&target), None, format)
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
        crate::query::sparql::registry::show(ctx, name)
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
        crate::query::sparql::registry::list(ctx)
    }
    #[cfg(not(feature = "sparql"))]
    {
        let _ = ctx;
        Err(no_query_engine())
    }
}
