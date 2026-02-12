use crate::domain::DomainModel;
use crate::actions::ActionList;
use automerge::AutoCommit;
use autosurgeon::{Hydrate, Reconcile, hydrate, reconcile};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Name of the CRDT file within a workspace
const CRDT_FILENAME: &str = "workspace.crdt";

/// Bump this constant any time the Hydrate/Reconcile-derived shape of
/// WorkspaceState, DomainModel, Plan, PlannedAct, or Recurrence changes.
pub const CRDT_SCHEMA_VERSION: u32 = 1;

/// Directory for shadow files (used in 3-way merge)
const SHADOW_DIR: &str = "/tmp/clearhead-shadow";

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
    /// The workspace configuration (paths, etc.)
    workspace: Workspace,
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
    ///
    /// # Arguments
    /// * `file_path` - Path to the file to validate
    ///
    /// # Returns
    /// Ok(()) if file is within workspace, Err if outside
    pub fn validate_file(&self, file_path: &Path) -> Result<(), String> {
        // Canonicalize paths to resolve symlinks and get absolute paths
        let canonical_file = file_path
            .canonicalize()
            .map_err(|e| format!("Cannot resolve file path '{}': {}", file_path.display(), e))?;
        let canonical_data = self
            .data_dir
            .canonicalize()
            .unwrap_or_else(|_| self.data_dir.clone()); // data_dir might not exist yet

        // Validate file is within managed workspace
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
    /// Only use this in tests!
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

        Ok(WorkspaceDoc {
            doc,
            workspace: self.workspace.clone(),
        })
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
        // For tests, assume data dir is same as state dir unless specified
        Ok(CrdtStorage {
            workspace: Workspace {
                state_dir: storage_dir.clone(),
                data_dir: storage_dir,
            },
        })
    }
}

impl WorkspaceDoc {
    /// Initialize a new workspace doc
    pub fn new(workspace: Workspace) -> Result<Self, String> {
        let mut doc = AutoCommit::new();
        let empty_state = WorkspaceState {
            version: CRDT_SCHEMA_VERSION,
            files: HashMap::new(),
        };
        reconcile(&mut doc, &empty_state).map_err(|e| format!("Failed to init CRDT: {}", e))?;
        Ok(WorkspaceDoc { doc, workspace })
    }

    /// Hydrate the workspace state and verify the schema version matches.
    fn hydrate_checked(&self) -> Result<WorkspaceState, String> {
        let state: WorkspaceState =
            hydrate(&self.doc).map_err(|e| {
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
    /// This is the primary method — returns the domain model directly.
    pub fn get_domain_model(&self, key: &str) -> Result<DomainModel, String> {
        let state = self.hydrate_checked()?;

        if let Some(domain) = state.files.get(key) {
            Ok(domain.clone())
        } else {
            Ok(DomainModel::new())
        }
    }

    /// Update the DomainModel for a specific file key directly.
    ///
    /// This is the primary method — accepts a DomainModel without conversion.
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

    /// Project all files in the CRDT back to the filesystem
    pub fn project_all(&self) -> Result<(), String> {
        let state = self.hydrate_checked()?;

        let root = self.workspace.root_dir();

        for (relative_path, domain) in state.files {
            let full_path = root.join(relative_path);

            // Ensure parent directory exists
            if let Some(parent) = full_path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    format!("Failed to create directory {}: {}", parent.display(), e)
                })?;
            }

            let actions = domain.to_action_list();

            use crate::{OutputFormat, format};
            let formatted = format(&actions, OutputFormat::Actions, None, None)?;

            std::fs::write(&full_path, formatted)
                .map_err(|e| format!("Failed to write file {}: {}", full_path.display(), e))?;
        }

        Ok(())
    }
}

/// Repository pattern for managing the lifecycle of Actions for a SINGLE file
/// backed by the Global Workspace CRDT.
pub struct ActionRepository {
    storage: CrdtStorage,
    file_path: PathBuf,
    /// The relative path used as the key in the CRDT map
    file_key: String,
    doc: WorkspaceDoc,
}

impl ActionRepository {
    /// Load an action repository for a file within a workspace
    ///
    /// # Arguments
    /// * `workspace` - The workspace configuration (provided by CLI)
    /// * `file_path` - Path to the .actions file
    pub fn load(workspace: Workspace, file_path: PathBuf) -> Result<Self, String> {
        let storage = CrdtStorage::for_file(&workspace, &file_path)?;

        // Calculate stable relative key
        let root = workspace.root_dir();
        // Canonicalize if possible to ensure matching
        let file_abs = if file_path.is_absolute() {
            file_path.clone()
        } else {
            // Best effort for relative paths passed in CLI
            std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .join(&file_path)
        };

        // Try to make relative to root
        let file_key = file_abs
            .strip_prefix(&root)
            .unwrap_or({
                // Fallback: use filename if outside root (shouldn't happen in normal usage)
                // or just use the full path string if we can't strip.
                // For safety in this MVP, let's use the full string if strip fails,
                // but this implies "External File" support.
                file_abs.as_path()
            })
            .to_string_lossy()
            .to_string();

        // Load CRDT
        let mut doc = if storage.exists() {
            storage.load()?
        } else {
            // Bootstrap empty
            WorkspaceDoc::new(workspace.clone())?
        };

        // If file exists on disk, we should "Import" it if not in CRDT?
        // Or if CRDT is empty for this key?
        // CRDT-First means we trust CRDT. But if CRDT is empty/missing key, and file exists,
        // we assume it's a new file import.
        if file_path.exists() {
            let stored_actions = doc.get_actions_for_file(&file_key)?;
            if stored_actions.is_empty() {
                // Potentially import from file
                let content = std::fs::read_to_string(&file_path)
                    .map_err(|e| format!("Failed to read file: {}", e))?;

                // Avoid parsing empty files as errors
                if !content.trim().is_empty() {
                    let file_actions = crate::parse_actions(&content)?;
                    doc.update_file(&file_key, &file_actions)?;
                }
            }
        }

        Ok(Self {
            storage,
            file_path,
            file_key,
            doc,
        })
    }

    /// Get the DomainModel from the CRDT.
    ///
    /// This is the primary method — returns the domain model directly.
    pub fn get_domain_model(&self) -> Result<DomainModel, String> {
        self.doc.get_domain_model(&self.file_key)
    }

    /// Save a DomainModel to the CRDT and return formatted content.
    ///
    /// This is the primary method — accepts a DomainModel without conversion.
    pub fn save_model(&mut self, model: &DomainModel) -> Result<String, String> {
        // 1. Update CRDT
        self.doc.update_file_model(&self.file_key, model)?;

        // 2. Persist CRDT
        self.storage.save(&mut self.doc)?;

        // 3. Format actions with UUIDs (but don't write to file)
        use crate::{OutputFormat, format};
        let actions = model.to_action_list();
        let formatted = format(&actions, OutputFormat::Actions, None, None)?;

        Ok(formatted)
    }

    /// Get actions from the CRDT.
    ///
    /// Convenience wrapper — delegates to `get_domain_model()`.
    pub fn get_actions(&self) -> Result<ActionList, String> {
        self.doc.get_actions_for_file(&self.file_key)
    }

    /// Save actions to CRDT and return formatted content.
    ///
    /// Convenience wrapper — converts to DomainModel, then delegates to `save_model()`.
    pub fn save(&mut self, actions: &ActionList) -> Result<String, String> {
        let model = DomainModel::from_actions(actions);
        self.save_model(&model)
    }

    /// Project actions to file (used by CLI commands, not LSP).
    ///
    /// This method writes the formatted actions directly to the file system.
    /// The LSP should NOT call this - instead use save() and apply the result
    /// via workspace/applyEdit to avoid editor sync issues.
    pub fn project_to_file(&self, actions: &ActionList) -> Result<(), String> {
        if self.file_path.exists() {
            write_shadow_file(&self.file_path)?;
        }

        use crate::{OutputFormat, format};
        let formatted = format(actions, OutputFormat::Actions, None, None)?;

        std::fs::write(&self.file_path, formatted)
            .map_err(|e| format!("Failed to write to file: {}", e))?;

        Ok(())
    }

    /// Create an ActionRepository for testing with a custom workspace directory.
    /// This bypasses workspace boundary validation.
    ///
    /// **WARNING:** This is only for tests! Do not use in production code.
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

        let mut doc = if storage.exists() {
            storage.load()?
        } else {
            WorkspaceDoc::new(workspace.clone())?
        };

        // If file exists, import it
        if file_path.exists() {
            let stored_actions = doc.get_actions_for_file(&file_key)?;
            if stored_actions.is_empty() {
                let content = std::fs::read_to_string(&file_path)
                    .map_err(|e| format!("Failed to read file: {}", e))?;

                if !content.trim().is_empty() {
                    let file_actions = crate::parse_actions(&content)?;
                    doc.update_file(&file_key, &file_actions)?;
                }
            }
        }

        Ok(Self {
            storage,
            file_path,
            file_key,
            doc,
        })
    }
}

fn write_shadow_file(file_path: &Path) -> Result<(), String> {
    let shadow_dir = PathBuf::from(SHADOW_DIR);
    std::fs::create_dir_all(&shadow_dir)
        .map_err(|e| format!("Failed to create shadow dir: {}", e))?;

    let filename = file_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");
    let shadow_path = shadow_dir.join(format!("{}.base", filename));

    std::fs::copy(file_path, &shadow_path)
        .map_err(|e| format!("Failed to write shadow file: {}", e))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::Action;
    use tempfile::TempDir;

    #[test]
    fn test_workspace_separation() {
        // Test that two files in the same workspace do not overwrite each other
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path().to_path_buf();
        let workspace = Workspace {
            state_dir: root.clone(),
            data_dir: root.clone(),
        };
        let storage = CrdtStorage::new(workspace.clone()).unwrap();

        let mut doc = WorkspaceDoc::new(workspace).unwrap();

        // Update File A
        let action_a = Action::new("Task A");
        doc.update_file("file_a.actions", &vec![action_a]).unwrap();

        // Update File B
        let action_b = Action::new("Task B");
        doc.update_file("file_b.actions", &vec![action_b]).unwrap();

        storage.save(&mut doc).unwrap();

        // Reload and verify
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

        let mut doc = WorkspaceDoc::new(workspace).unwrap();

        // Create a model with specific data
        let mut action = Action::new("Round-trip Task");
        action.priority = Some(3);
        action.description = Some("Test description".to_string());
        let original_model = DomainModel::from_actions(&vec![action]);

        // Save via DomainModel API
        doc.update_file_model("test.actions", &original_model)
            .unwrap();
        storage.save(&mut doc).unwrap();

        // Reload and verify
        let loaded_doc = storage.load().unwrap();
        let loaded_model = loaded_doc.get_domain_model("test.actions").unwrap();

        assert_eq!(loaded_model.plans.len(), original_model.plans.len());
        assert_eq!(loaded_model.acts.len(), original_model.acts.len());

        for (key, plan) in &original_model.plans {
            let loaded_plan = loaded_model.plans.get(key).expect("Plan should exist");
            assert_eq!(loaded_plan.name, plan.name);
            assert_eq!(loaded_plan.priority, plan.priority);
            assert_eq!(loaded_plan.description, plan.description);
        }
    }
}
