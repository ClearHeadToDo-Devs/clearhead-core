//! `clearhead transact` — apply an ordered batch of action operations atomically.
//!
//! Reads a `transaction_request` JSON document (from a file or stdin), hands it
//! to core's `transact` executor against the primary workspace, and emits the
//! `transaction_result` JSON to stdout. The whole batch commits together or not
//! at all; `--dry-run` reports what would happen and writes nothing.
//!
//! This is a machine surface: the result is always JSON (the schema is the
//! contract), never prose. A rejected transaction still prints its result and
//! exits non-zero so shell composition (`clearhead transact … && next`) is safe.

use std::io::Read;
use std::path::PathBuf;

use anyhow::Context;

use super::CommandContext;

pub fn run(ctx: &CommandContext, file: &Option<PathBuf>, dry_run: bool) -> anyhow::Result<()> {
    let raw = read_request(file)?;
    let request: clearhead_core::TransactionRequest =
        serde_json::from_str(&raw).context("parsing transaction request JSON")?;

    let outcome = clearhead_core::transact(&ctx.data_dir, request, dry_run)?;

    println!(
        "{}",
        serde_json::to_string(&outcome).expect("transaction outcome serializes")
    );

    if matches!(outcome, clearhead_core::TransactionOutcome::Rejected { .. }) {
        // A well-formed request whose operations could not apply: the result is
        // on stdout for the caller to branch on; the exit code marks the failure.
        std::process::exit(1);
    }
    Ok(())
}

/// Read the request document from `file`, or from stdin when no file is given.
fn read_request(file: &Option<PathBuf>) -> anyhow::Result<String> {
    match file {
        Some(path) => std::fs::read_to_string(path)
            .with_context(|| format!("reading transaction request from {}", path.display())),
        None => {
            let mut buffer = String::new();
            std::io::stdin()
                .read_to_string(&mut buffer)
                .context("reading transaction request from stdin")?;
            Ok(buffer)
        }
    }
}
