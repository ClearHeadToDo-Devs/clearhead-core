//! Structured results for action-mutation verbs — the shared outcome/error
//! vocabulary (query_output.md, "Errors as data").
//!
//! A verb result — success or failure — is data a caller can branch on, not
//! prose it has to parse. The taxonomy lives in core so every producer (the CLI
//! verbs, the `transact` batch executor, and future clients) speaks one spelling
//! of identity and one set of failure kinds. Presentation — deciding between
//! JSON and human prose, writing to a terminal — belongs to the client and
//! stays out of core.
//!
//! `id` is canonical identity exactly as the query contract exports it
//! (`urn:uuid:…`), so the read and write halves of the system agree.
//!
//! `conflict` joins the taxonomy when the write path gains compare-and-swap;
//! today no conflicting interleave is observable, so it is not modeled.

use serde::Serialize;
use uuid::Uuid;

/// Canonical identity as the query contract exports it.
pub fn canonical_id(id: Uuid) -> String {
    format!("urn:uuid:{id}")
}

/// Strip the `urn:uuid:` prefix for human-facing rendering.
pub fn bare(id: &str) -> &str {
    id.trim_start_matches("urn:uuid:")
}

/// A mutation verb that applied.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum VerbOutcome {
    Completed { id: String, children: usize },
    Cancelled { id: String, children: usize },
    Updated { id: String },
}

/// A mutation verb that could not apply.
///
/// Carried through `anyhow` by the CLI and downcast at the boundary, which emits
/// it as JSON when stdout is piped.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum VerbError {
    /// Nothing open or closed matches the query.
    NotFound { query: String },
    /// More than one action matched the strongest canonical reference tier.
    Ambiguous {
        query: String,
        candidates: Vec<String>,
    },
    /// The query resolves, but to an action already in a completed archive —
    /// an idempotent loop can branch on this as effectively-done.
    AlreadyClosed {
        id: String,
        state: String,
        query: String,
    },
}

impl std::fmt::Display for VerbError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerbError::NotFound { query } => {
                write!(f, "No open action found matching '{query}'")
            }
            VerbError::Ambiguous { query, candidates } => write!(
                f,
                "Ambiguous action reference '{query}'; candidates: {}",
                candidates
                    .iter()
                    .map(|id| bare(id))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            VerbError::AlreadyClosed { id, state, .. } => {
                write!(f, "Action {} is already closed ({state})", bare(id))
            }
        }
    }
}

impl std::error::Error for VerbError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn outcome_serializes_with_kind_tag_and_canonical_id() {
        let id = Uuid::parse_str("01951111-0000-7000-8000-000000000001").unwrap();
        let json = serde_json::to_string(&VerbOutcome::Completed {
            id: canonical_id(id),
            children: 2,
        })
        .unwrap();
        assert_eq!(
            json,
            r#"{"kind":"completed","id":"urn:uuid:01951111-0000-7000-8000-000000000001","children":2}"#
        );
    }

    #[test]
    fn errors_serialize_branchable_kinds() {
        let not_found = serde_json::to_string(&VerbError::NotFound { query: "x".into() }).unwrap();
        assert_eq!(not_found, r#"{"kind":"not-found","query":"x"}"#);

        let ambiguous = serde_json::to_string(&VerbError::Ambiguous {
            query: "dead".into(),
            candidates: vec![
                "urn:uuid:dead0000-0000-7000-8000-000000000001".into(),
                "urn:uuid:deadffff-0000-7000-8000-000000000002".into(),
            ],
        })
        .unwrap();
        assert!(ambiguous.starts_with(r#"{"kind":"ambiguous""#));

        let closed = serde_json::to_string(&VerbError::AlreadyClosed {
            id: "urn:uuid:01951111-0000-7000-8000-000000000001".into(),
            state: "Completed".into(),
            query: "x".into(),
        })
        .unwrap();
        assert!(
            closed.starts_with(r#"{"kind":"already-closed""#),
            "got: {closed}"
        );
    }
}
