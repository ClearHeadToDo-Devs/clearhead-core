//! CLI presentation for the shared verb-result vocabulary.
//!
//! The data — [`VerbOutcome`], [`VerbError`], and canonical-id spelling — lives
//! in `clearhead_core::verb_result` so the CLI verbs and the core `transact`
//! executor produce one taxonomy. This module keeps only the *presentation*
//! that belongs to a terminal client: JSON when piped, prose at a terminal.

pub use clearhead_core::verb_result::{VerbError, VerbOutcome, bare, canonical_id};

use std::io::IsTerminal;

fn prose(outcome: &VerbOutcome) -> String {
    match outcome {
        VerbOutcome::Completed { id, children } => {
            format!("Completed action {} (+{} children)", bare(id), children)
        }
        VerbOutcome::Cancelled { id, children } => {
            format!("Cancelled action {} (+{} children)", bare(id), children)
        }
        VerbOutcome::Updated { id } => format!("Updated action {}", bare(id)),
    }
}

/// Print a verb outcome: JSON when stdout is piped, prose at a terminal.
pub fn emit(outcome: &VerbOutcome) {
    if std::io::stdout().is_terminal() {
        println!("{}", prose(outcome));
    } else {
        println!(
            "{}",
            serde_json::to_string(outcome).expect("verb outcome serializes")
        );
    }
}
