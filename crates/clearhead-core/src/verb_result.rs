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
//! `conflict` joined the taxonomy with the delivery precondition seam: every
//! resource a mutation batch touches carries the revision it was read at, and a
//! lost compare-and-swap is reported as a branchable kind rather than prose.

use serde::Serialize;
use uuid::Uuid;

use crate::workspace::WorkspaceError;

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
    Reopened { id: String, children: usize },
    Updated { id: String },
    Added { id: String },
    Deleted { id: String, children: usize },
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
    /// The query resolves to an action that is already open (not in any
    /// completed archive) — the mirror of `AlreadyClosed` for `reopen`, so an
    /// idempotent loop can branch on "already in the desired live state".
    AlreadyOpen {
        id: String,
        state: String,
        query: String,
    },
    /// The write path's compare-and-swap lost: the resource the verb read to
    /// decide its write changed before the batch was delivered, so nothing was
    /// applied. A retry-on-fresh-read loop branches on this.
    Conflict {
        path: String,
        expected: String,
        actual: Option<String>,
    },
}

impl VerbError {
    /// Project a native-adapter failure into the verb taxonomy.
    ///
    /// Only a delivery conflict has a structured kind here; every other
    /// workspace failure stays a workspace error, since the verb layer has no
    /// finer thing to say about it. The mapping lives in Core so the CLI and
    /// any future client report a conflict identically.
    pub fn from_workspace_error(error: &WorkspaceError) -> Option<Self> {
        match error {
            WorkspaceError::Conflict(conflict) => Some(Self::Conflict {
                path: conflict.path.to_string(),
                expected: conflict.expected.to_string(),
                actual: conflict
                    .actual
                    .as_ref()
                    .map(|revision| revision.to_string()),
            }),
            WorkspaceError::Io(_)
            | WorkspaceError::Parse(_)
            | WorkspaceError::Actions(_)
            | WorkspaceError::InvalidPath(_) => None,
        }
    }
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
            VerbError::AlreadyOpen { id, state, .. } => {
                write!(f, "Action {} is already open ({state})", bare(id))
            }
            VerbError::Conflict {
                path,
                expected,
                actual,
            } => match actual {
                Some(actual) => write!(
                    f,
                    "Resource {path} changed underneath the write (expected {expected}, found {actual})"
                ),
                None => write!(
                    f,
                    "Resource {path} was removed underneath the write (expected {expected})"
                ),
            },
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
    fn added_and_deleted_serialize_with_kind_tag() {
        let id = Uuid::parse_str("01951111-0000-7000-8000-000000000001").unwrap();
        let added = serde_json::to_string(&VerbOutcome::Added {
            id: canonical_id(id),
        })
        .unwrap();
        assert_eq!(
            added,
            r#"{"kind":"added","id":"urn:uuid:01951111-0000-7000-8000-000000000001"}"#
        );

        let deleted = serde_json::to_string(&VerbOutcome::Deleted {
            id: canonical_id(id),
            children: 1,
        })
        .unwrap();
        assert_eq!(
            deleted,
            r#"{"kind":"deleted","id":"urn:uuid:01951111-0000-7000-8000-000000000001","children":1}"#
        );
    }

    #[test]
    fn errors_serialize_branchable_kinds() {
        let not_found = serde_json::to_string(&VerbError::NotFound { query: "x".into() }).unwrap();
        assert_eq!(not_found, r#"{"kind":"not-found","query":"x"}"#);

        let conflict = serde_json::to_string(&VerbError::Conflict {
            path: "workspace:charters/support.md".into(),
            expected: "sha256:aaaa".into(),
            actual: Some("sha256:bbbb".into()),
        })
        .unwrap();
        assert_eq!(
            conflict,
            r#"{"kind":"conflict","path":"workspace:charters/support.md","expected":"sha256:aaaa","actual":"sha256:bbbb"}"#
        );

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

    #[test]
    fn a_conflict_is_projected_from_the_workspace_error_that_carries_it() {
        use crate::workspace::WorkspaceError;
        use crate::workspace::resource::{
            ExpectedResource, ResourceConflict, ResourceLocation, ResourceRevision, WorkspacePath,
        };

        let conflict = ResourceConflict {
            path: ResourceLocation::workspace(WorkspacePath::new("charters/support.md").unwrap()),
            expected: ExpectedResource::Revision(ResourceRevision::new("sha256:aaaa")),
            actual: Some(ResourceRevision::new("sha256:bbbb")),
        };
        let projected = VerbError::from_workspace_error(&WorkspaceError::Conflict(conflict))
            .expect("a conflict maps to a verb error");
        assert_eq!(
            projected,
            VerbError::Conflict {
                path: "workspace:charters/support.md".into(),
                expected: "sha256:aaaa".into(),
                actual: Some("sha256:bbbb".into()),
            }
        );

        // A deletion underneath the write is the same kind, with no actual revision.
        let deleted = ResourceConflict {
            path: ResourceLocation::workspace(WorkspacePath::new("charters/support.md").unwrap()),
            expected: ExpectedResource::Revision(ResourceRevision::new("sha256:aaaa")),
            actual: None,
        };
        assert_eq!(
            VerbError::from_workspace_error(&WorkspaceError::Conflict(deleted)),
            Some(VerbError::Conflict {
                path: "workspace:charters/support.md".into(),
                expected: "sha256:aaaa".into(),
                actual: None,
            })
        );

        // Everything else keeps its own error type.
        assert!(VerbError::from_workspace_error(&WorkspaceError::Parse("x".into())).is_none());
    }
}
