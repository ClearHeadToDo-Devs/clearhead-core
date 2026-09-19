//! In-process query engine: the canonical workspace dataset assembly plus the
//! optional SPARQL layer.
//!
//! This is one of the crate's two runtime modules (the other is
//! [`crate::filesystem`]). Frontends ([`crate::cli`], [`crate::lsp`], and the
//! future `mcp`) depend on it, never on each other (I8).

pub mod dataset;

/// Optional in-process SPARQL query layer (`sparql` feature, on by default).
/// Absent from the minimal `--no-default-features` build, which compiles no
/// query engine at all.
#[cfg(feature = "sparql")]
pub mod sparql;
