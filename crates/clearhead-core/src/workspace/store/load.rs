//! The pure workspace envelope and read result.
//!
//! Loading — walking the filesystem, reading bytes, replaying pending journals —
//! is host I/O and lives in the native adapter (`clearhead-workspace-fs`). What
//! remains here is pure: the [`Workspace`] envelope a host fills from assembled
//! charters plus host-supplied identity, the [`WorkspaceRead`] result of a pure
//! assembly, and the shared syntax-error summary. Assembly itself is
//! [`assemble_workspace`](super::assemble_workspace).

use super::findings::Finding;
use crate::domain::{Charter, DomainModel};
use crate::workspace::charter::MarkdownCharter;
use std::path::PathBuf;
use uuid::Uuid;

/// The complete filesystem representation of a workspace.
///
/// Holds all file-layer types ([`MarkdownCharter`] → [`ICSPlan`] / [`SourcedAction`]).
/// Convert to a pure [`DomainModel`] via `From` at the workspace boundary —
/// all file paths and source metadata are stripped in that conversion.
///
/// [`ICSPlan`]: crate::workspace::calendar::ics::ICSPlan
/// [`SourcedAction`]: crate::workspace::actions::repository::SourcedAction
pub struct Workspace {
    pub root: PathBuf,
    /// Durable UUID for this workspace's RDF named graph, read from the
    /// workspace manifest by the host. `None` for a workspace with no persisted
    /// identity.
    pub id: Option<String>,
    /// Display name — used to scope output in multi-workspace contexts.
    pub name: Option<String>,
    /// A random UUID minted once per load, used as the graph identity only when
    /// `id` is absent. Ephemeral by design: distinct per load, never persisted,
    /// and never derived from the root path — a workspace without a durable id
    /// stays queryable, but its graph URI is not stable across sessions.
    ephemeral_id: String,
    pub charters: Vec<MarkdownCharter>,
}

impl Workspace {
    /// Construct the host-facing workspace envelope from already assembled
    /// charter data and host-supplied identity.
    pub fn from_parts(
        root: PathBuf,
        id: Option<String>,
        name: Option<String>,
        charters: Vec<MarkdownCharter>,
    ) -> Self {
        Self {
            root,
            id,
            name,
            ephemeral_id: Uuid::now_v7().to_string(),
            charters,
        }
    }

    /// The workspace's graph id: its durable [`id`](Self::id) when persisted,
    /// otherwise the per-load [`ephemeral_id`](Self::ephemeral_id). Never
    /// derived from the root path — see the field docs for why.
    pub fn effective_id(&self) -> String {
        self.id.clone().unwrap_or_else(|| self.ephemeral_id.clone())
    }

    /// The workspace's display name, falling back to its directory name.
    ///
    /// Pure: derives the basename from the supplied root without touching the
    /// filesystem (no canonicalization).
    pub fn effective_name(&self) -> String {
        self.name.clone().unwrap_or_else(|| {
            self.root
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_else(|| "workspace".to_string())
        })
    }
}

impl From<Workspace> for DomainModel {
    fn from(ws: Workspace) -> DomainModel {
        // Occurrences are never projected into the action list. The present due
        // occurrence is *materialized* on the write path (a real `.actions` line,
        // indistinguishable from a dated action); the future is a read-only
        // calendar concern, rendered elsewhere from the recurrence engine — not
        // unioned in here. So the lowering is a straight per-charter flatten.
        let charters = ws.charters.into_iter().map(Charter::from).collect();
        DomainModel {
            objectives: vec![],
            charters,
        }
    }
}

/// What a pure read of the workspace produced: everything that loaded, plus
/// a [`Finding`] for everything that didn't (or loaded with issues).
pub struct WorkspaceRead {
    pub charters: Vec<MarkdownCharter>,
    pub findings: Vec<Finding>,
}

/// One human-readable summary of a document's recoverable syntax issues,
/// detailing the first few diagnostics. Shared with `doctor`, which makes the
/// same observation about completed archives (outside the loader's scope).
pub(crate) fn syntax_error_summary(doc: &crate::workspace::actions::ParsedDocument) -> String {
    let mut msg = format!(
        "parsed with {} issue(s); loaded {} recoverable action(s)",
        doc.syntax_errors.len(),
        doc.actions.len()
    );
    for diagnostic in doc.syntax_errors.iter().take(5) {
        msg.push_str(&format!(
            "\n  - line {}, col {}: {}",
            diagnostic.range.start_row + 1,
            diagnostic.range.start_col + 1,
            diagnostic.message
        ));
    }
    let remaining = doc.syntax_errors.len().saturating_sub(5);
    if remaining > 0 {
        msg.push_str(&format!("\n  - ... and {} more issue(s)", remaining));
    }
    msg
}
