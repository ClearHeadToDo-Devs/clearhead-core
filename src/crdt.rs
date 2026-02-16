//! CRDT operations for distributed synchronization.
//!
//! This module manages the Automerge CRDT document that serves as the
//! source of truth for workspace state. It is intentionally limited to
//! CRDT concerns: loading, saving, hydrating, and reconciling the
//! Automerge document.
//!
//! Filesystem projection (writing .actions files) is handled by
//! [`WorkspaceStore`](crate::store::WorkspaceStore) implementations.
//! File import (seeding CRDT from disk) is the caller's responsibility.

use crate::actions::ActionList;
use crate::domain::DomainModel;
use automerge::AutoCommit;
use autosurgeon::{Hydrate, Reconcile, hydrate, reconcile};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Name of the CRDT file within a workspace
const CRDT_FILENAME: &str = "workspace.crdt";

/// Bump this constant any time the Hydrate/Reconcile-derived shape of
/// WorkspaceState, DomainModel, Plan, PlannedAct, or Recurrence changes.
pub const CRDT_SCHEMA_VERSION: u32 = 2;

/// The root state of the CRDT, representing the entire workspace.
///
/// Implements the "Hub-and-Spoke" model:
/// - Hub: The `files` map containing the source of truth for all files.
/// - Spokes: Each entry in `files` corresponds to an "Anchored View" (a .actions file).
#[derive(Debug, Clone, Reconcile, Hydrate)]
struct WorkspaceState {
    /// Schema version — used to detect stale CRDT files after struct changes
    version: u32,
    /// Map of file path (relative to workspace root) -> Domain Model
    files: HashMap<String, DomainModel>,
}

/// Wrapper around an Automerge document containing the WorkspaceState
#[derive(Debug)]
pub struct WorkspaceDoc {
    doc: AutoCommit,
}

/// Represents the user-level workspace configuration
#[derive(Debug, Clone)]
pub struct Workspace {
    /// Hub: Where the CRDT lives (XDG_STATE_HOME/clearhead)
    pub state_dir: PathBuf,
    /// Spokes: Where the action files live (XDG_DATA_HOME/clearhead)
    pub data_dir: PathBuf,
}

impl Workspace {
    /// Create a new workspace with explicit paths
    ///
    /// # Arguments
    /// * `state_dir` - Directory where CRDT file lives (hub)
    /// * `data_dir` - Directory where action files live (spokes)
    pub fn new(state_dir: PathBuf, data_dir: PathBuf) -> Self {
        Workspace {
            state_dir,
            data_dir,
        }
    }

    /// Validates that a file is within the workspace data directory
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

    /// Create a test workspace with custom paths (bypasses boundary validation)
    #[cfg(test)]
    pub fn test_workspace(dir: PathBuf) -> Self {
        Workspace {
            state_dir: dir.clone(),
            data_dir: dir,
        }
    }

    pub fn crdt_path(&self) -> PathBuf {
        self.state_dir.join(CRDT_FILENAME)
    }

    pub fn crdt_dir(&self) -> PathBuf {
        self.state_dir.clone()
    }

    /// Get the logical root for file paths in this workspace
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
}

#[derive(Debug, Clone)]
pub struct CrdtStorage {
    workspace: Workspace,
}

impl CrdtStorage {
    pub fn new(workspace: Workspace) -> Result<Self, String> {
        workspace.ensure_dir()?;
        Ok(CrdtStorage { workspace })
    }

    /// Create storage for a specific file within a workspace
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
        let hydrate_result: Result<WorkspaceState, _> = hydrate(&doc);
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

impl WorkspaceDoc {
    /// Initialize a new empty workspace doc
    pub fn new() -> Result<Self, String> {
        let mut doc = AutoCommit::new();
        let empty_state = WorkspaceState {
            version: CRDT_SCHEMA_VERSION,
            files: HashMap::new(),
        };
        reconcile(&mut doc, &empty_state).map_err(|e| format!("Failed to init CRDT: {}", e))?;
        Ok(WorkspaceDoc { doc })
    }

    /// Hydrate the workspace state and verify the schema version matches.
    fn hydrate_checked(&self) -> Result<WorkspaceState, String> {
        let state: WorkspaceState = hydrate(&self.doc).map_err(|e| {
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
    pub fn get_domain_model(&self, key: &str) -> Result<DomainModel, String> {
        let state = self.hydrate_checked()?;

        if let Some(domain) = state.files.get(key) {
            Ok(domain.clone())
        } else {
            Ok(DomainModel::new())
        }
    }

    /// Update the DomainModel for a specific file key directly.
    pub fn update_file_model(&mut self, key: &str, model: &DomainModel) -> Result<(), String> {
        let mut state = self.hydrate_checked()?;

        state.files.insert(key.to_string(), model.clone());

        reconcile(&mut self.doc, &state)
            .map_err(|e| format!("Failed to reconcile workspace update: {}", e))?;

        Ok(())
    }

    /// Get the ActionList for a specific file key.
    ///
    /// Convenience wrapper — delegates to `get_domain_model()`.
    pub fn get_actions_for_file(&self, key: &str) -> Result<ActionList, String> {
        let model = self.get_domain_model(key)?;
        Ok(model.to_action_list())
    }

    /// Update the DomainModel for a specific file key from an ActionList.
    ///
    /// Convenience wrapper — converts to DomainModel, then delegates to `update_file_model()`.
    pub fn update_file(&mut self, key: &str, actions: &ActionList) -> Result<(), String> {
        let model = DomainModel::from_actions(actions);
        self.update_file_model(key, &model)
    }
}

/// Repository pattern for managing the lifecycle of Actions for a SINGLE file
/// backed by the Global Workspace CRDT.
///
/// This is a pure CRDT layer — it does not read or write `.actions` files.
/// Callers who want to seed the CRDT from disk should load via a
/// [`WorkspaceStore`](crate::store::WorkspaceStore) and call
/// [`save_model`](ActionRepository::save_model) explicitly.
pub struct ActionRepository {
    storage: CrdtStorage,
    file_path: PathBuf,
    /// The relative path used as the key in the CRDT map
    file_key: String,
    doc: WorkspaceDoc,
}

impl ActionRepository {
    /// Load an action repository for a file within a workspace.
    ///
    /// Opens (or bootstraps) the CRDT document. Does NOT import from disk —
    /// if the CRDT is empty for this key, callers should seed it via a
    /// `WorkspaceStore` if desired.
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
    pub fn get_domain_model(&self) -> Result<DomainModel, String> {
        self.doc.get_domain_model(&self.file_key)
    }

    /// Save a DomainModel to the CRDT and return formatted content.
    pub fn save_model(&mut self, model: &DomainModel) -> Result<String, String> {
        self.doc.update_file_model(&self.file_key, model)?;
        self.storage.save(&mut self.doc)?;

        use crate::{OutputFormat, format};
        let actions = model.to_action_list();
        let formatted = format(&actions, OutputFormat::Actions, None, None)?;

        Ok(formatted)
    }

    /// Get actions from the CRDT.
    pub fn get_actions(&self) -> Result<ActionList, String> {
        self.doc.get_actions_for_file(&self.file_key)
    }

    /// Save actions to CRDT and return formatted content.
    pub fn save(&mut self, actions: &ActionList) -> Result<String, String> {
        let model = DomainModel::from_actions(actions);
        self.save_model(&model)
    }

    /// The file path this repository manages.
    pub fn file_path(&self) -> &Path {
        &self.file_path
    }

    /// The CRDT file key for this repository.
    pub fn file_key(&self) -> &str {
        &self.file_key
    }

    /// Create an ActionRepository for testing with a custom workspace directory.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::Action;
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

        let action_a = Action::new("Task A");
        doc.update_file("file_a.actions", &vec![action_a]).unwrap();

        let action_b = Action::new("Task B");
        doc.update_file("file_b.actions", &vec![action_b]).unwrap();

        storage.save(&mut doc).unwrap();

        let loaded_doc = storage.load().unwrap();

        let actions_a = loaded_doc.get_actions_for_file("file_a.actions").unwrap();
        assert_eq!(actions_a.len(), 1);
        assert_eq!(actions_a[0].name, "Task A");

        let actions_b = loaded_doc.get_actions_for_file("file_b.actions").unwrap();
        assert_eq!(actions_b.len(), 1);
        assert_eq!(actions_b[0].name, "Task B");
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
        let original_model = DomainModel::from_actions(&vec![action]);

        doc.update_file_model("test.actions", &original_model)
            .unwrap();
        storage.save(&mut doc).unwrap();

        let loaded_doc = storage.load().unwrap();
        let loaded_model = loaded_doc.get_domain_model("test.actions").unwrap();

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
