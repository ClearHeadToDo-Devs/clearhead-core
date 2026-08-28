//! Host-neutral workspace assembly.
//!
//! Core assembles a [`DomainModel`](crate::domain::DomainModel) only from
//! snapshots and explicit [`WorkspaceScope`](crate::workspace::WorkspaceScope)
//! evidence supplied by a host. Native layout detection and physical path
//! construction belong to `clearhead-workspace-fs`.

mod assembly;
mod doctor;
mod findings;
pub mod load;
mod pathing;

use std::path::PathBuf;

pub use assembly::{WorkspaceAssemblyInput, assemble_workspace, assembled_domain_model};
pub use doctor::{
    Diagnosis, DoctorCollectionEvidence, DoctorDocument, DoctorEvidence, DoctorRepair,
    DoctorSidecarEvidence, DurabilityResidue, DurabilityResidueKind, diagnose,
};
pub use findings::{Finding, FindingSeverity};
pub use load::{Workspace, WorkspaceRead};
pub use pathing::{
    charter_collection_from_anchor, infer_charter_name, infer_charter_name_for_workspace,
    infer_parent_charter_name, infer_parent_charter_name_for_workspace,
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
    /// Another process currently owns the workspace mutation lock.
    #[error("Workspace is locked by another writer: {0}")]
    WorkspaceLocked(PathBuf),
}
