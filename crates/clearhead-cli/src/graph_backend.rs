//! Transitional out-of-process bridge to `clearhead-graphd`.
//!
//! RDF/JSON-LD publication left graphd for Core's `rdf` module first; ad-hoc
//! and saved-query execution followed with the CLI's optional in-process
//! SPARQL evaluator (the default `sparql` feature, [`crate`]'s `query raw` /
//! locally-resolved `query named`). What still crosses this bridge is graphd's
//! remaining registry and client-presentation machinery — the `index`, `tree`,
//! `graph`, and `chain` families, the built-in named queries, `list`/`show`,
//! and `--status` parameter injection — until `migrate-graph-consumers` moves
//! or retires each piece and `retire-graphd` removes this shim entirely.
//!
//! The bridge spawns graphd with inherited stdio so its terminal detection and
//! bytes reach the user unmodified.

use std::process::Command;

const GRAPHD_ENV: &str = "CLEARHEAD_GRAPHD";

/// Locate the graphd executable — the remaining forwarded `query` families
/// spawn it and inherit stdio.
pub fn graphd_command() -> Command {
    let executable = std::env::var_os(GRAPHD_ENV).unwrap_or_else(|| "clearhead-graphd".into());
    Command::new(executable)
}
