//! CRDT operations for distributed synchronization.
//!
//! This module manages the Automerge CRDT document that serves as the
//! source of truth for workspace state. It speaks only `DomainModel`
//! at its public boundary — no ActionList, no formatting, no parsing.
//!
//! Filesystem projection (writing .actions files) is handled by
//! [`WorkspaceStore`](crate::store::WorkspaceStore) implementations.
//! File import (seeding CRDT from disk) is the caller's responsibility.

pub mod convert;
pub mod sync_utils;
pub mod types;

use crate::domain::DomainModel;
use automerge::AutoCommit;
use autosurgeon::{hydrate, reconcile};
use std::path::{Path, PathBuf};
use types::{SyncModel, SyncWorkspaceState};

/// Name of the CRDT file within a workspace
const CRDT_FILENAME: &str = "workspace.crdt";

/// Bump this constant any time the Hydrate/Reconcile-derived shape of
/// the Sync* types changes. The domain model can now evolve independently.
pub const CRDT_SCHEMA_VERSION: u32 = 2;

// ============================================================================
// Workspace — directory configuration
// ============================================================================

/// Represents the user-level workspace configuration.
///
/// Holds paths for CRDT storage (hub) and action files (spokes).
#[derive(Debug, Clone)]
pub struct Workspace {
    /// Hub: Where the CRDT lives (XDG_STATE_HOME/clearhead)
    pub state_dir: PathBuf,
    /// Spokes: Where the action files live (XDG_DATA_HOME/clearhead)
    pub data_dir: PathBuf,
}

impl Workspace {
    /// Create a new workspace with explicit paths.
    pub fn new(state_dir: PathBuf, data_dir: PathBuf) -> Self {
        Workspace {
            state_dir,
            data_dir,
        }
    }

    /// Validates that a file is within the workspace data directory.
    pub fn validate_file(&self, file_path: &Path) -> Result<(), String> {
        let canonical_file = file_path
            .canonicalize()
            .map_err(|e| format!("Cannot resolve file path '{}': {}", file_path.display(), e))?;
        let canonical_data = self
            .data_dir
            .canonicalize()
            .unwrap_or_else(|_| self.data_dir.clone());

        if !canonical_file.starts_with(&canonical_data) {
            return Err(format!(
                "File outside managed workspace (file: {}, workspace: {})",
                canonical_file.display(),
                canonical_data.display()
            ));
        }

        Ok(())
    }

    pub fn crdt_path(&self) -> PathBuf {
        self.state_dir.join(CRDT_FILENAME)
    }

    pub fn crdt_dir(&self) -> PathBuf {
        self.state_dir.clone()
    }

    /// Get the logical root for file paths in this workspace.
    pub fn root_dir(&self) -> PathBuf {
        self.data_dir.clone()
    }

    pub fn ensure_dir(&self) -> Result<(), String> {
        let dir = self.crdt_dir();
        std::fs::create_dir_all(&dir).map_err(|e| {
            format!(
                "Failed to create workspace directory '{}': {}",
                dir.display(),
                e
            )
        })
    }

    /// Create a test workspace with custom paths (bypasses boundary validation).
    #[cfg(test)]
    pub fn test_workspace(dir: PathBuf) -> Self {
        Workspace {
            state_dir: dir.clone(),
            data_dir: dir,
        }
    }
}

// ============================================================================
// CrdtStorage — file-level I/O for the Automerge document
// ============================================================================

#[derive(Debug, Clone)]
pub struct CrdtStorage {
    workspace: Workspace,
}

impl CrdtStorage {
    pub fn new(workspace: Workspace) -> Result<Self, String> {
        workspace.ensure_dir()?;
        Ok(CrdtStorage { workspace })
    }

    /// Create storage for a specific file within a workspace.
    ///
    /// Validates that the file is within the workspace data directory.
    pub fn for_file(workspace: &Workspace, file_path: &Path) -> Result<Self, String> {
        workspace.validate_file(file_path)?;
        Self::new(workspace.clone())
    }

    pub fn load(&self) -> Result<WorkspaceDoc, String> {
        let crdt_path = self.workspace.crdt_path();

        if !crdt_path.exists() {
            return Err(format!("CRDT file does not exist: {}", crdt_path.display()));
        }

        let bytes =
            std::fs::read(&crdt_path).map_err(|e| format!("Failed to read CRDT file: {}", e))?;

        let doc = AutoCommit::load(bytes.as_slice())
            .map_err(|e| format!("Failed to load AutoCommit document: {}", e))?;

        // Check whether the stored schema version matches the current one.
        // If hydration fails or the version doesn't match, the CRDT was written
        // by an older (or newer) struct layout — delete and bootstrap fresh.
        let hydrate_result: Result<SyncWorkspaceState, _> = hydrate(&doc);
        let needs_rebuild = match hydrate_result {
            Ok(state) => state.version != CRDT_SCHEMA_VERSION,
            Err(_) => true,
        };

        if needs_rebuild {
            eprintln!(
                "CRDT schema version mismatch — rebuilding from .actions files on next access"
            );
            let _ = std::fs::remove_file(&crdt_path);
            return WorkspaceDoc::new();
        }

        Ok(WorkspaceDoc { doc })
    }

    pub fn save(&self, doc: &mut WorkspaceDoc) -> Result<(), String> {
        let crdt_path = self.workspace.crdt_path();
        if let Some(parent) = crdt_path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create CRDT directory: {}", e))?;
        }
        let bytes = doc.doc.save();
        std::fs::write(&crdt_path, &bytes)
            .map_err(|e| format!("Failed to write CRDT file: {}", e))?;
        Ok(())
    }

    pub fn exists(&self) -> bool {
        self.workspace.crdt_path().exists()
    }

    pub fn workspace(&self) -> &Workspace {
        &self.workspace
    }

    // Test helper
    pub fn with_dir(storage_dir: PathBuf) -> Result<Self, String> {
        std::fs::create_dir_all(&storage_dir).map_err(|e| format!("Failed: {}", e))?;
        Ok(CrdtStorage {
            workspace: Workspace {
                state_dir: storage_dir.clone(),
                data_dir: storage_dir,
            },
        })
    }
}

// ============================================================================
// WorkspaceDoc — in-memory Automerge document
// ============================================================================

/// Wrapper around an Automerge document containing the workspace state.
///
/// Speaks `DomainModel` at its public boundary. Internally hydrates/reconciles
/// through the `Sync*` mirror types.
#[derive(Debug)]
pub struct WorkspaceDoc {
    doc: AutoCommit,
}

impl WorkspaceDoc {
    /// Initialize a new empty workspace doc.
    pub fn new() -> Result<Self, String> {
        let mut doc = AutoCommit::new();
        let empty_state = SyncWorkspaceState {
            version: CRDT_SCHEMA_VERSION,
            files: std::collections::HashMap::new(),
        };
        reconcile(&mut doc, &empty_state).map_err(|e| format!("Failed to init CRDT: {}", e))?;
        Ok(WorkspaceDoc { doc })
    }

    /// Hydrate the workspace state and verify the schema version matches.
    fn hydrate_checked(&self) -> Result<SyncWorkspaceState, String> {
        let state: SyncWorkspaceState = hydrate(&self.doc).map_err(|e| {
            format!(
                "Failed to hydrate workspace (CRDT may need rebuilding): {}",
                e
            )
        })?;
        if state.version != CRDT_SCHEMA_VERSION {
            return Err(format!(
                "CRDT schema version mismatch (file: {}, expected: {}). \
                 Delete the workspace.crdt file to rebuild from .actions files.",
                state.version, CRDT_SCHEMA_VERSION
            ));
        }
        Ok(state)
    }

    /// Get the DomainModel for a specific file key.
    ///
    /// Hydrates CRDT sync types and converts to domain types at the boundary.
    pub fn get_model(&self, key: &str) -> Result<DomainModel, String> {
        let state = self.hydrate_checked()?;

        if let Some(sync_model) = state.files.get(key) {
            Ok(DomainModel::from(sync_model))
        } else {
            Ok(DomainModel::new())
        }
    }

    /// Save a DomainModel for a specific file key.
    ///
    /// Converts domain types to CRDT sync types before reconciling.
    pub fn save_model(&mut self, key: &str, model: &DomainModel) -> Result<(), String> {
        let mut state = self.hydrate_checked()?;

        let sync_model = SyncModel::from(model);
        state.files.insert(key.to_string(), sync_model);

        reconcile(&mut self.doc, &state)
            .map_err(|e| format!("Failed to reconcile workspace update: {}", e))?;

        Ok(())
    }
}

// ============================================================================
// SyncRepo — per-file handle to the workspace CRDT
// ============================================================================

/// Per-file handle to the workspace CRDT.
///
/// Manages the lifecycle of a single file's DomainModel within the
/// global workspace CRDT document. Speaks only `DomainModel` — callers
/// are responsible for any ActionList conversion or formatting.
pub struct SyncRepo {
    storage: CrdtStorage,
    file_path: PathBuf,
    /// The relative path used as the key in the CRDT map
    file_key: String,
    doc: WorkspaceDoc,
}

impl SyncRepo {
    /// Open (or bootstrap) a sync repo for a file within a workspace.
    ///
    /// Does NOT import from disk — if the CRDT is empty for this key,
    /// callers should seed it via a `WorkspaceStore` if desired.
    pub fn load(workspace: Workspace, file_path: PathBuf) -> Result<Self, String> {
        let storage = CrdtStorage::for_file(&workspace, &file_path)?;

        // Calculate stable relative key
        let root = workspace.root_dir();
        let file_abs = if file_path.is_absolute() {
            file_path.clone()
        } else {
            std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .join(&file_path)
        };

        let file_key = file_abs
            .strip_prefix(&root)
            .unwrap_or(file_abs.as_path())
            .to_string_lossy()
            .to_string();

        // Load or bootstrap CRDT
        let doc = if storage.exists() {
            storage.load()?
        } else {
            WorkspaceDoc::new()?
        };

        Ok(Self {
            storage,
            file_path,
            file_key,
            doc,
        })
    }

    /// Get the DomainModel from the CRDT.
    pub fn get_model(&self) -> Result<DomainModel, String> {
        self.doc.get_model(&self.file_key)
    }

    /// Save a DomainModel to the CRDT.
    pub fn save_model(&mut self, model: &DomainModel) -> Result<(), String> {
        self.doc.save_model(&self.file_key, model)?;
        self.storage.save(&mut self.doc)?;
        Ok(())
    }

    /// The file path this repo manages.
    pub fn file_path(&self) -> &Path {
        &self.file_path
    }

    /// The CRDT file key for this repo.
    pub fn file_key(&self) -> &str {
        &self.file_key
    }

    /// Create a SyncRepo for testing with a custom workspace directory.
    ///
    /// Bypasses workspace boundary validation.
    #[doc(hidden)]
    pub fn test_repo(file_path: PathBuf, workspace_dir: PathBuf) -> Result<Self, String> {
        let workspace = Workspace {
            state_dir: workspace_dir.clone(),
            data_dir: workspace_dir,
        };
        let storage = CrdtStorage::new(workspace.clone())?;

        let file_key = file_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("test.actions")
            .to_string();

        let doc = if storage.exists() {
            storage.load()?
        } else {
            WorkspaceDoc::new()?
        };

        Ok(Self {
            storage,
            file_path,
            file_key,
            doc,
        })
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::{Action, convert};
    use tempfile::TempDir;

    #[test]
    fn test_workspace_separation() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path().to_path_buf();
        let workspace = Workspace {
            state_dir: root.clone(),
            data_dir: root.clone(),
        };
        let storage = CrdtStorage::new(workspace.clone()).unwrap();

        let mut doc = WorkspaceDoc::new().unwrap();

        let model_a = convert::from_actions(&vec![Action::new("Task A")]);
        doc.save_model("file_a.actions", &model_a).unwrap();

        let model_b = convert::from_actions(&vec![Action::new("Task B")]);
        doc.save_model("file_b.actions", &model_b).unwrap();

        storage.save(&mut doc).unwrap();

        let loaded_doc = storage.load().unwrap();

        let loaded_a = loaded_doc.get_model("file_a.actions").unwrap();
        assert_eq!(loaded_a.all_plans().len(), 1);
        assert_eq!(loaded_a.all_plans()[0].name, "Task A");

        let loaded_b = loaded_doc.get_model("file_b.actions").unwrap();
        assert_eq!(loaded_b.all_plans().len(), 1);
        assert_eq!(loaded_b.all_plans()[0].name, "Task B");
    }

    #[test]
    fn test_domain_model_roundtrip_through_crdt() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path().to_path_buf();
        let workspace = Workspace {
            state_dir: root.clone(),
            data_dir: root.clone(),
        };
        let storage = CrdtStorage::new(workspace.clone()).unwrap();

        let mut doc = WorkspaceDoc::new().unwrap();

        let mut action = Action::new("Round-trip Task");
        action.priority = Some(3);
        action.description = Some("Test description".to_string());
        let original_model = convert::from_actions(&vec![action]);

        doc.save_model("test.actions", &original_model).unwrap();
        storage.save(&mut doc).unwrap();

        let loaded_doc = storage.load().unwrap();
        let loaded_model = loaded_doc.get_model("test.actions").unwrap();

        assert_eq!(
            loaded_model.all_plans().len(),
            original_model.all_plans().len()
        );
        assert_eq!(
            loaded_model.all_acts().len(),
            original_model.all_acts().len()
        );

        let original_plans = original_model.all_plans();
        let loaded_plans = loaded_model.all_plans();
        for (orig, loaded) in original_plans.iter().zip(loaded_plans.iter()) {
            assert_eq!(loaded.name, orig.name);
            assert_eq!(loaded.priority, orig.priority);
            assert_eq!(loaded.description, orig.description);
        }
    }
}
