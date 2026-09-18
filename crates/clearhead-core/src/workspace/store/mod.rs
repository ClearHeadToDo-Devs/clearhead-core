//! Host-neutral workspace assembly.
//!
//! Core assembles a [`DomainModel`](crate::domain::DomainModel) only from
//! snapshots and the persisted root charter name
//! supplied by a host. Native layout detection and physical path
//! construction belong to `clearhead-workspace-fs`.

mod assembly;
mod doctor;
mod findings;
pub mod load;
mod pathing;

use std::path::PathBuf;

use crate::workspace::resource::ResourceConflict;

pub use assembly::{WorkspaceAssemblyInput, assemble_workspace, assembled_domain_model};
pub use doctor::{
    Diagnosis, DoctorCollectionEvidence, DoctorDocument, DoctorEvidence, DoctorRepair,
    DoctorSidecarEvidence, DurabilityResidue, DurabilityResidueKind, diagnose,
    state_coherence_findings,
};
pub use findings::{Finding, FindingSeverity};
pub use load::{Workspace, WorkspaceRead};
pub use pathing::{
    PRIMARY_ACTIONS_FILE, PRIMARY_DOCUMENT_FILE, ROOT_ANCHOR_STEM, charter_collection_from_anchor,
    infer_charter_name, infer_charter_name_for_workspace, infer_parent_charter_name,
    infer_parent_charter_name_for_workspace,
};

/// Errors that can occur when interacting with a workspace.
#[derive(thiserror::Error, Debug)]
pub enum WorkspaceError {
    /// An underlying I/O error occurred.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    /// Error parsing a `.actions` file.
    #[error("Parse error: {0}")]
    Parse(String),
    /// Error loading or saving sidecar actions.
    #[error("Actions error: {0}")]
    Actions(String),
    /// A path provided was not within the workspace or was otherwise invalid.
    #[error("Invalid path: {0}")]
    InvalidPath(PathBuf),
    /// A precondition compare-and-swap failed: the resource observed while the
    /// mutation was prepared is no longer the resource on disk, so the batch
    /// was not applied. Carries the conflict as data rather than prose so
    /// clients can report a branchable kind.
    #[error("resource conflict: {0}")]
    Conflict(ResourceConflict),
}
