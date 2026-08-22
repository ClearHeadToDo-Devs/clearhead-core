//! Out-of-process bridge to `clearhead-graphd` for the `query` facade.
//!
//! The CLI forwards `clearhead query …` to graphd — still the read/query tool —
//! by spawning it with inherited stdio. RDF and JSON-LD export no longer go
//! through graphd: Core's `rdf` module serializes the canonical dataset in
//! process. This shim is the last graphd coupling and exists only until the
//! optional local SPARQL evaluator replaces the query forwarding too.

use std::process::Command;

const GRAPHD_ENV: &str = "CLEARHEAD_GRAPHD";

/// Locate the graphd executable — the `query` facade spawns it and forwards the
/// subcommand with inherited stdio.
pub fn graphd_command() -> Command {
    let executable = std::env::var_os(GRAPHD_ENV).unwrap_or_else(|| "clearhead-graphd".into());
    Command::new(executable)
}
