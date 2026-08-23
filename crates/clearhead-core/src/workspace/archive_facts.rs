//! Typed read model for archived action facts.
//!
//! Archive storage is deliberately plaintext (`*.completed.actions` plus sidecar
//! JSON), but consumers should not need to learn those file/layout rules. This
//! module is the core-owned adapter from archive files into data.

use std::path::PathBuf;

use crate::domain::Action;
use crate::workspace::sidecar::OccurrenceSnapshot;

/// One terminal action loaded from archive storage.
#[derive(Debug, Clone, PartialEq)]
pub struct ArchivedActionFact {
    /// The archived terminal action exactly as represented in plaintext history.
    pub action: Action,
    /// Workspace-root-relative path of the completed actions file that carried it.
    pub source_path: PathBuf,
    /// Frozen recurring occurrence lineage, when this action is the root of a
    /// materialized recurring occurrence archived at crystallization time.
    pub occurrence: Option<OccurrenceSnapshot>,
}
