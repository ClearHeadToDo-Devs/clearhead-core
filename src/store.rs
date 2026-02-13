//! Workspace storage abstraction.
//!
//! This module defines the [`WorkspaceStore`] trait — the interface for loading
//! and saving domain objects (plans, charters) regardless of storage backend.
//!
//! # Design
//!
//! The trait abstracts *where* domain objects live. Implementations decide:
//! - **Filesystem:** `.actions` files + `.md` charters in XDG directories
//! - **Database:** SQLite, PostgreSQL, etc.
//! - **In-memory:** For testing and ephemeral use
//!
//! The CRDT sync layer sits *above* this trait. A sync server uses a
//! `WorkspaceStore` to project CRDT state outward, but the store itself
//! has no knowledge of CRDTs or synchronization.
//!
//! # Example
//!
//! ```rust
//! use clearhead_core::store::{InMemoryStore, WorkspaceStore, ObjectiveRef};
//! use clearhead_core::DomainModel;
//!
//! let mut store = InMemoryStore::new();
//!
//! let objective = ObjectiveRef::new("inbox");
//! let model = DomainModel::new();
//! store.save_domain_model(&objective, &model).unwrap();
//!
//! let loaded = store.load_domain_model(&objective).unwrap();
//! assert_eq!(loaded.all_plans().len(), 0);
//! ```

use crate::domain::{Charter, DomainModel};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::path::{Path, PathBuf};

// ============================================================================
// Core types
// ============================================================================

/// Identifies an objective (project/file) in the workspace.
///
/// The key is storage-agnostic: for filesystem stores it's a relative path
/// (e.g., `"inbox.actions"`), for databases it could be a row ID or slug.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectiveRef {
    /// Storage-level key for this objective.
    pub key: String,
    /// Human-readable name (inferred from key or metadata).
    pub name: Option<String>,
}

impl ObjectiveRef {
    /// Create an ObjectiveRef from a key string.
    ///
    /// The name defaults to the key itself.
    pub fn new(key: &str) -> Self {
        Self {
            key: key.to_string(),
            name: Some(key.to_string()),
        }
    }

    /// Create an ObjectiveRef with an explicit name.
    pub fn with_name(key: &str, name: &str) -> Self {
        Self {
            key: key.to_string(),
            name: Some(name.to_string()),
        }
    }
}

/// A charter with metadata about how it was discovered.
#[derive(Debug, Clone)]
pub struct DiscoveredCharter {
    /// The parsed charter.
    pub charter: Charter,
    /// Storage-level key (e.g., file path, row ID).
    pub source_key: String,
    /// Whether this charter was explicitly defined (e.g., from a `.md` file)
    /// vs inferred from context (e.g., from a `.actions` filename or directory).
    pub is_explicit: bool,
}

// ============================================================================
// Trait definition
// ============================================================================

/// Trait for loading and saving workspace content.
///
/// Implementations decide the storage backend. The trait covers the core
/// operations needed by the CLI, LSP, and sync server:
///
/// - **Objectives:** List what's in the workspace
/// - **Domain models:** Load/save plans and their planned acts
/// - **Charters:** Load/save/discover project charters
///
/// # Mutability
///
/// `save_*` methods take `&mut self` to allow implementations that need
/// interior state changes (file handles, transaction state, etc.).
/// Implementations that don't need mutation can use interior mutability
/// or simply ignore the `&mut`.
pub trait WorkspaceStore {
    /// The error type returned by this store's operations.
    type Error: fmt::Display;

    /// List all objectives in the workspace.
    fn list_objectives(&self) -> Result<Vec<ObjectiveRef>, Self::Error>;

    /// Load the domain model for a specific objective.
    ///
    /// Returns an empty `DomainModel` if the objective exists but has no plans.
    /// Returns an error if the objective doesn't exist or can't be read.
    fn load_domain_model(&self, objective: &ObjectiveRef) -> Result<DomainModel, Self::Error>;

    /// Save a domain model for a specific objective.
    ///
    /// Creates the objective if it doesn't exist.
    fn save_domain_model(
        &mut self,
        objective: &ObjectiveRef,
        model: &DomainModel,
    ) -> Result<(), Self::Error>;

    /// Load a charter for a specific objective.
    ///
    /// Returns `None` if no charter exists for this objective.
    fn load_charter(&self, objective: &ObjectiveRef) -> Result<Option<Charter>, Self::Error>;

    /// Save a charter for a specific objective.
    fn save_charter(
        &mut self,
        objective: &ObjectiveRef,
        charter: &Charter,
    ) -> Result<(), Self::Error>;

    /// Discover all charters in the workspace.
    ///
    /// This includes both explicitly defined charters and those inferred
    /// from workspace structure (e.g., directory names, filenames).
    fn discover_charters(&self) -> Result<Vec<DiscoveredCharter>, Self::Error>;
}

// ============================================================================
// InMemoryStore — always-public for consumer testing
// ============================================================================

/// In-memory workspace store.
///
/// Useful for testing and for consumers who want to verify their code against
/// the `WorkspaceStore` trait without touching the filesystem.
///
/// # Example
///
/// ```rust
/// use clearhead_core::store::{InMemoryStore, WorkspaceStore, ObjectiveRef};
/// use clearhead_core::DomainModel;
///
/// let mut store = InMemoryStore::new();
/// let obj = ObjectiveRef::new("test");
/// store.save_domain_model(&obj, &DomainModel::new()).unwrap();
/// assert_eq!(store.list_objectives().unwrap().len(), 1);
/// ```
#[derive(Debug, Default, Clone)]
pub struct InMemoryStore {
    /// Domain models keyed by objective key.
    pub models: HashMap<String, DomainModel>,
    /// Charters keyed by objective key.
    pub charters: HashMap<String, Charter>,
}

/// Error type for the in-memory store.
///
/// Operations on InMemoryStore rarely fail, but the trait requires
/// an error type for consistency.
#[derive(Debug, Clone)]
pub struct InMemoryError(pub String);

impl fmt::Display for InMemoryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl InMemoryStore {
    /// Create a new empty in-memory store.
    pub fn new() -> Self {
        Self::default()
    }
}

impl WorkspaceStore for InMemoryStore {
    type Error = InMemoryError;

    fn list_objectives(&self) -> Result<Vec<ObjectiveRef>, Self::Error> {
        let mut keys: Vec<_> = self
            .models
            .keys()
            .chain(self.charters.keys())
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .map(|k| ObjectiveRef::new(k))
            .collect();
        keys.sort_by(|a, b| a.key.cmp(&b.key));
        Ok(keys)
    }

    fn load_domain_model(&self, objective: &ObjectiveRef) -> Result<DomainModel, Self::Error> {
        Ok(self
            .models
            .get(&objective.key)
            .cloned()
            .unwrap_or_default())
    }

    fn save_domain_model(
        &mut self,
        objective: &ObjectiveRef,
        model: &DomainModel,
    ) -> Result<(), Self::Error> {
        self.models.insert(objective.key.clone(), model.clone());
        Ok(())
    }

    fn load_charter(&self, objective: &ObjectiveRef) -> Result<Option<Charter>, Self::Error> {
        Ok(self.charters.get(&objective.key).cloned())
    }

    fn save_charter(
        &mut self,
        objective: &ObjectiveRef,
        charter: &Charter,
    ) -> Result<(), Self::Error> {
        self.charters.insert(objective.key.clone(), charter.clone());
        Ok(())
    }

    fn discover_charters(&self) -> Result<Vec<DiscoveredCharter>, Self::Error> {
        let mut discovered: Vec<_> = self
            .charters
            .iter()
            .map(|(key, charter)| DiscoveredCharter {
                charter: charter.clone(),
                source_key: key.clone(),
                is_explicit: true,
            })
            .collect();
        discovered.sort_by(|a, b| a.source_key.cmp(&b.source_key));
        Ok(discovered)
    }
}

// ============================================================================
// FsWorkspaceStore — filesystem-backed implementation
// ============================================================================

/// Filesystem-backed workspace store.
///
/// Discovers and manages `.actions` files and `.md` charter files
/// in a root directory following the workspace naming conventions:
///
/// - Root level: `<project>.actions` (single-file projects)
/// - Directories: `<project>/next.actions` (multi-file projects)
/// - Charters: `<project>.md` or `<project>/README.md`
/// - Archives: `<project>/logs/<YYYY-MM>.actions`
#[derive(Debug, Clone)]
pub struct FsWorkspaceStore {
    /// Root directory for .actions files and charters
    root: PathBuf,
}

/// Error type for filesystem store operations.
#[derive(Debug, Clone)]
pub struct FsError(pub String);

impl fmt::Display for FsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FsWorkspaceStore {
    /// Create a new filesystem store rooted at the given directory.
    pub fn new(root: &Path) -> Self {
        Self {
            root: root.to_path_buf(),
        }
    }

    /// The root directory of this store.
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Resolve an objective key to a filesystem path.
    ///
    /// The key is a relative path (e.g., `"inbox.actions"`, `"work/next.actions"`).
    fn resolve_path(&self, key: &str) -> PathBuf {
        self.root.join(key)
    }

    /// Resolve the charter file path for an objective key.
    ///
    /// Checks for `<stem>.md` at root level first, then `<dir>/README.md`.
    fn resolve_charter_path(&self, key: &str) -> Option<PathBuf> {
        // If key is a .actions file, check for matching .md
        let path = Path::new(key);
        if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
            // Root-level: health.actions → health.md
            let md_path = self.root.join(format!("{}.md", stem));
            if md_path.is_file() {
                return Some(md_path);
            }
        }

        // Directory-level: work/next.actions → work/README.md
        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty() {
                let readme = self.root.join(parent).join("README.md");
                if readme.is_file() {
                    return Some(readme);
                }
            }
        }

        // Check if key itself is a directory name
        let readme = self.root.join(key).join("README.md");
        if readme.is_file() {
            return Some(readme);
        }

        let md_path = self.root.join(format!("{}.md", key));
        if md_path.is_file() {
            return Some(md_path);
        }

        None
    }
}

/// Infer a human-readable project name from a relative file path.
///
/// Rules:
/// - `project.actions` → "project"
/// - `project/next.actions` → "project"
/// - `project/logs/2026-01.actions` → "project"
/// - `inbox.actions` → None (inbox is special, not a project)
pub fn infer_project_name(relative_path: &Path) -> Option<String> {
    let components: Vec<_> = relative_path.components().collect();

    if components.is_empty() {
        return None;
    }

    if components.len() == 1 {
        let filename = relative_path.file_stem()?.to_str()?;
        if filename == "inbox" {
            return None;
        }
        return Some(filename.to_string());
    }

    let first = components.first()?;
    if let std::path::Component::Normal(name) = first {
        return name.to_str().map(String::from);
    }

    None
}

/// Discover all `.actions` files recursively, skipping hidden directories.
fn discover_action_files(dir: &Path) -> Result<Vec<PathBuf>, String> {
    let mut files = Vec::new();
    discover_recursive(dir, &mut files)?;
    files.sort();
    Ok(files)
}

fn discover_recursive(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    if !dir.is_dir() {
        return Ok(());
    }

    let entries = std::fs::read_dir(dir)
        .map_err(|e| format!("Failed to read directory '{}': {}", dir.display(), e))?;

    for entry in entries {
        let entry = entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
        let path = entry.path();

        if path.is_dir() {
            if let Some(name) = path.file_name() {
                if name.to_string_lossy().starts_with('.') {
                    continue;
                }
            }
            discover_recursive(&path, files)?;
        } else if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext == "actions" {
                    files.push(path);
                }
            }
        }
    }

    Ok(())
}

impl WorkspaceStore for FsWorkspaceStore {
    type Error = FsError;

    fn list_objectives(&self) -> Result<Vec<ObjectiveRef>, Self::Error> {
        let files = discover_action_files(&self.root).map_err(|e| FsError(e))?;

        let mut objectives = Vec::new();
        for file_path in files {
            let relative = file_path
                .strip_prefix(&self.root)
                .unwrap_or(&file_path)
                .to_string_lossy()
                .to_string();
            let name = infer_project_name(Path::new(&relative));
            objectives.push(ObjectiveRef {
                key: relative,
                name,
            });
        }

        Ok(objectives)
    }

    fn load_domain_model(&self, objective: &ObjectiveRef) -> Result<DomainModel, Self::Error> {
        let path = self.resolve_path(&objective.key);

        if !path.exists() {
            return Ok(DomainModel::new());
        }

        let content = std::fs::read_to_string(&path)
            .map_err(|e| FsError(format!("Failed to read '{}': {}", path.display(), e)))?;

        if content.trim().is_empty() {
            return Ok(DomainModel::new());
        }

        let actions = crate::parse_actions(&content).map_err(|e| FsError(e))?;
        Ok(DomainModel::from_actions(&actions))
    }

    fn save_domain_model(
        &mut self,
        objective: &ObjectiveRef,
        model: &DomainModel,
    ) -> Result<(), Self::Error> {
        let path = self.resolve_path(&objective.key);

        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                FsError(format!(
                    "Failed to create directory '{}': {}",
                    parent.display(),
                    e
                ))
            })?;
        }

        let actions = model.to_action_list();
        let formatted = crate::format::format(
            &actions,
            crate::format::OutputFormat::Actions,
            None,
            None,
        )
        .map_err(|e| FsError(e))?;

        std::fs::write(&path, formatted)
            .map_err(|e| FsError(format!("Failed to write '{}': {}", path.display(), e)))?;

        Ok(())
    }

    fn load_charter(&self, objective: &ObjectiveRef) -> Result<Option<Charter>, Self::Error> {
        let charter_path = match self.resolve_charter_path(&objective.key) {
            Some(p) => p,
            None => return Ok(None),
        };

        let content = std::fs::read_to_string(&charter_path)
            .map_err(|e| FsError(format!("Failed to read '{}': {}", charter_path.display(), e)))?;

        match crate::charter::parse_charter(&content) {
            Ok(charter) => Ok(Some(charter)),
            Err(_) => Ok(None),
        }
    }

    fn save_charter(
        &mut self,
        objective: &ObjectiveRef,
        charter: &Charter,
    ) -> Result<(), Self::Error> {
        // Determine charter path: use <key>.md if key has no extension,
        // or replace .actions with .md
        let key_path = Path::new(&objective.key);
        let charter_filename = if key_path.extension().is_some() {
            let stem = key_path.file_stem().unwrap_or_default().to_string_lossy();
            format!("{}.md", stem)
        } else {
            format!("{}.md", objective.key)
        };

        let path = self.root.join(&charter_filename);

        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                FsError(format!(
                    "Failed to create directory '{}': {}",
                    parent.display(),
                    e
                ))
            })?;
        }

        let formatted = crate::charter::format_charter(charter);
        std::fs::write(&path, formatted)
            .map_err(|e| FsError(format!("Failed to write '{}': {}", path.display(), e)))?;

        Ok(())
    }

    fn discover_charters(&self) -> Result<Vec<DiscoveredCharter>, Self::Error> {
        let mut charters = Vec::new();
        let mut seen_names: HashSet<String> = HashSet::new();

        if !self.root.is_dir() {
            return Ok(charters);
        }

        let entries = std::fs::read_dir(&self.root)
            .map_err(|e| FsError(format!("Failed to read '{}': {}", self.root.display(), e)))?;

        let mut root_md_files = Vec::new();
        let mut root_actions_files = Vec::new();
        let mut directories = Vec::new();

        for entry in entries {
            let entry =
                entry.map_err(|e| FsError(format!("Failed to read directory entry: {}", e)))?;
            let path = entry.path();

            if path.is_dir() {
                if let Some(name) = path.file_name() {
                    if !name.to_string_lossy().starts_with('.') {
                        directories.push(path);
                    }
                }
            } else if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "md" {
                        root_md_files.push(path);
                    } else if ext == "actions" {
                        root_actions_files.push(path);
                    }
                }
            }
        }

        // Phase 1: Explicit charters from .md files at root
        for md_path in &root_md_files {
            let content = match std::fs::read_to_string(md_path) {
                Ok(c) => c,
                Err(_) => continue,
            };
            match crate::charter::parse_charter(&content) {
                Ok(charter) => {
                    let name = charter.title.to_lowercase();
                    seen_names.insert(name);
                    if let Some(ref alias) = charter.alias {
                        seen_names.insert(alias.to_lowercase());
                    }
                    if let Some(stem) = md_path.file_stem().and_then(|s| s.to_str()) {
                        seen_names.insert(stem.to_lowercase());
                    }
                    let relative = md_path
                        .strip_prefix(&self.root)
                        .unwrap_or(md_path)
                        .to_string_lossy()
                        .to_string();
                    charters.push(DiscoveredCharter {
                        charter,
                        source_key: relative,
                        is_explicit: true,
                    });
                }
                Err(_) => continue,
            }
        }

        // Phase 1b: Explicit charters from README.md in project directories
        for dir_path in &directories {
            let readme = dir_path.join("README.md");
            if readme.is_file() {
                if let Ok(content) = std::fs::read_to_string(&readme) {
                    if let Ok(charter) = crate::charter::parse_charter(&content) {
                        let dir_name = dir_path
                            .file_name()
                            .and_then(|n| n.to_str())
                            .unwrap_or("");
                        seen_names.insert(charter.title.to_lowercase());
                        seen_names.insert(dir_name.to_lowercase());
                        if let Some(ref alias) = charter.alias {
                            seen_names.insert(alias.to_lowercase());
                        }
                        let relative = readme
                            .strip_prefix(&self.root)
                            .unwrap_or(&readme)
                            .to_string_lossy()
                            .to_string();
                        charters.push(DiscoveredCharter {
                            charter,
                            source_key: relative,
                            is_explicit: true,
                        });
                    }
                }
            }

            // Check for sub-directory charters
            if let Ok(sub_entries) = std::fs::read_dir(dir_path) {
                let parent_name = dir_path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("")
                    .to_string();

                for sub_entry in sub_entries.flatten() {
                    let sub_path = sub_entry.path();
                    if sub_path.is_dir() {
                        if let Some(name) = sub_path.file_name() {
                            if name.to_string_lossy().starts_with('.') {
                                continue;
                            }
                        }
                        let has_actions = sub_path.join("next.actions").is_file()
                            || std::fs::read_dir(&sub_path)
                                .ok()
                                .map(|entries| {
                                    entries.flatten().any(|e| {
                                        e.path().extension().is_some_and(|ext| ext == "actions")
                                    })
                                })
                                .unwrap_or(false);

                        if has_actions {
                            let sub_name = sub_path
                                .file_name()
                                .and_then(|n| n.to_str())
                                .unwrap_or("")
                                .to_string();
                            if !seen_names.contains(&sub_name.to_lowercase()) {
                                let mut sub_charter =
                                    crate::charter::implicit_charter(&sub_name);
                                sub_charter.parent = Some(parent_name.clone());
                                seen_names.insert(sub_name.to_lowercase());
                                let relative = sub_path
                                    .strip_prefix(&self.root)
                                    .unwrap_or(&sub_path)
                                    .to_string_lossy()
                                    .to_string();
                                charters.push(DiscoveredCharter {
                                    charter: sub_charter,
                                    source_key: relative,
                                    is_explicit: false,
                                });
                            }
                        }
                    }
                }
            }
        }

        // Phase 2: Implicit charters from .actions files
        for actions_path in &root_actions_files {
            if let Some(stem) = actions_path.file_stem().and_then(|s| s.to_str()) {
                if stem == "inbox" {
                    continue;
                }
                if !seen_names.contains(&stem.to_lowercase()) {
                    seen_names.insert(stem.to_lowercase());
                    let relative = actions_path
                        .strip_prefix(&self.root)
                        .unwrap_or(actions_path)
                        .to_string_lossy()
                        .to_string();
                    charters.push(DiscoveredCharter {
                        charter: crate::charter::implicit_charter(stem),
                        source_key: relative,
                        is_explicit: false,
                    });
                }
            }
        }

        // Phase 2b: Implicit charters from directories
        for dir_path in &directories {
            if let Some(dir_name) = dir_path.file_name().and_then(|n| n.to_str()) {
                if !seen_names.contains(&dir_name.to_lowercase()) {
                    let has_actions = discover_action_files(dir_path)
                        .map(|f| !f.is_empty())
                        .unwrap_or(false);
                    if has_actions {
                        seen_names.insert(dir_name.to_lowercase());
                        let relative = dir_path
                            .strip_prefix(&self.root)
                            .unwrap_or(dir_path)
                            .to_string_lossy()
                            .to_string();
                        charters.push(DiscoveredCharter {
                            charter: crate::charter::implicit_charter(dir_name),
                            source_key: relative,
                            is_explicit: false,
                        });
                    }
                }
            }
        }

        charters.sort_by(|a, b| a.charter.title.cmp(&b.charter.title));
        Ok(charters)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::Action;
    use std::fs;

    #[test]
    fn test_in_memory_store_roundtrip() {
        let mut store = InMemoryStore::new();
        let obj = ObjectiveRef::new("inbox");

        // Initially empty
        let model = store.load_domain_model(&obj).unwrap();
        assert!(model.all_plans().is_empty());

        // Save a model with one action
        let action = Action::new("Test task");
        let model = DomainModel::from_actions(&vec![action]);
        store.save_domain_model(&obj, &model).unwrap();

        // Load it back
        let loaded = store.load_domain_model(&obj).unwrap();
        assert_eq!(loaded.all_plans().len(), 1);
    }

    #[test]
    fn test_in_memory_store_list_objectives() {
        let mut store = InMemoryStore::new();

        let inbox = ObjectiveRef::new("inbox");
        let work = ObjectiveRef::new("work");

        store
            .save_domain_model(&inbox, &DomainModel::new())
            .unwrap();
        store
            .save_domain_model(&work, &DomainModel::new())
            .unwrap();

        let objectives = store.list_objectives().unwrap();
        assert_eq!(objectives.len(), 2);
        assert_eq!(objectives[0].key, "inbox");
        assert_eq!(objectives[1].key, "work");
    }

    #[test]
    fn test_in_memory_store_charters() {
        let mut store = InMemoryStore::new();
        let obj = ObjectiveRef::new("health");

        // No charter initially
        assert!(store.load_charter(&obj).unwrap().is_none());

        // Save a charter
        let charter = Charter {
            id: uuid::Uuid::new_v4(),
            title: "Health & Fitness".to_string(),
            description: Some("Stay healthy.".to_string()),
            alias: Some("health".to_string()),
            parent: None,
            objectives: None,
            plans: vec![],
        };
        store.save_charter(&obj, &charter).unwrap();

        // Load it back
        let loaded = store.load_charter(&obj).unwrap().unwrap();
        assert_eq!(loaded.title, "Health & Fitness");
        assert_eq!(loaded.alias, Some("health".to_string()));
    }

    #[test]
    fn test_in_memory_store_discover_charters() {
        let mut store = InMemoryStore::new();

        let health = ObjectiveRef::new("health");
        let work = ObjectiveRef::new("work");

        let charter_a = Charter {
            id: uuid::Uuid::new_v4(),
            title: "Health".to_string(),
            description: None,
            alias: None,
            parent: None,
            objectives: None,
            plans: vec![],
        };
        let charter_b = Charter {
            id: uuid::Uuid::new_v4(),
            title: "Work".to_string(),
            description: None,
            alias: None,
            parent: None,
            objectives: None,
            plans: vec![],
        };

        store.save_charter(&health, &charter_a).unwrap();
        store.save_charter(&work, &charter_b).unwrap();

        let discovered = store.discover_charters().unwrap();
        assert_eq!(discovered.len(), 2);
        assert!(discovered.iter().all(|d| d.is_explicit));
    }

    #[test]
    fn test_objectives_union_of_models_and_charters() {
        let mut store = InMemoryStore::new();

        // One objective has only a model
        let inbox = ObjectiveRef::new("inbox");
        store
            .save_domain_model(&inbox, &DomainModel::new())
            .unwrap();

        // Another has only a charter
        let health = ObjectiveRef::new("health");
        let charter = Charter {
            id: uuid::Uuid::new_v4(),
            title: "Health".to_string(),
            description: None,
            alias: None,
            parent: None,
            objectives: None,
            plans: vec![],
        };
        store.save_charter(&health, &charter).unwrap();

        // list_objectives returns the union
        let objectives = store.list_objectives().unwrap();
        assert_eq!(objectives.len(), 2);
    }

    // ========================================================================
    // FsWorkspaceStore tests
    // ========================================================================

    #[test]
    fn test_fs_store_list_objectives() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        fs::write(root.join("inbox.actions"), "[ ] Task 1").unwrap();
        fs::write(root.join("work.actions"), "[ ] Task 2").unwrap();

        let project_dir = root.join("project1");
        fs::create_dir(&project_dir).unwrap();
        fs::write(project_dir.join("next.actions"), "[ ] Task 3").unwrap();

        let store = FsWorkspaceStore::new(root);
        let objectives = store.list_objectives().unwrap();

        assert_eq!(objectives.len(), 3);
        // inbox has no project name
        let inbox = objectives.iter().find(|o| o.key == "inbox.actions").unwrap();
        assert!(inbox.name.is_none());

        // work has project name
        let work = objectives.iter().find(|o| o.key == "work.actions").unwrap();
        assert_eq!(work.name, Some("work".to_string()));
    }

    #[test]
    fn test_fs_store_load_save_domain_model() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        let mut store = FsWorkspaceStore::new(root);
        let obj = ObjectiveRef::new("test.actions");

        // Save a model
        let action = Action::new("Test task");
        let model = DomainModel::from_actions(&vec![action]);
        store.save_domain_model(&obj, &model).unwrap();

        // File should exist
        assert!(root.join("test.actions").exists());

        // Load it back
        let loaded = store.load_domain_model(&obj).unwrap();
        assert_eq!(loaded.all_plans().len(), 1);
    }

    #[test]
    fn test_fs_store_load_nonexistent_returns_empty() {
        let temp = tempfile::TempDir::new().unwrap();
        let store = FsWorkspaceStore::new(temp.path());
        let obj = ObjectiveRef::new("nonexistent.actions");

        let model = store.load_domain_model(&obj).unwrap();
        assert!(model.all_plans().is_empty());
    }

    #[test]
    fn test_fs_store_discover_charters_implicit() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        fs::write(root.join("health.actions"), "[ ] Exercise").unwrap();
        fs::write(root.join("inbox.actions"), "[ ] Random").unwrap();

        let store = FsWorkspaceStore::new(root);
        let charters = store.discover_charters().unwrap();

        assert_eq!(charters.len(), 1);
        assert_eq!(charters[0].charter.title, "health");
        assert!(!charters[0].is_explicit);
    }

    #[test]
    fn test_fs_store_discover_charters_explicit_wins() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        fs::write(root.join("health.actions"), "[ ] Exercise").unwrap();
        fs::write(root.join("health.md"), "# Health & Fitness\n\nStay fit.\n").unwrap();

        let store = FsWorkspaceStore::new(root);
        let charters = store.discover_charters().unwrap();

        assert_eq!(charters.len(), 1);
        assert_eq!(charters[0].charter.title, "Health & Fitness");
        assert!(charters[0].is_explicit);
    }

    #[test]
    fn test_fs_store_discover_charters_directory() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        let project_dir = root.join("build_clearhead");
        fs::create_dir(&project_dir).unwrap();
        fs::write(project_dir.join("next.actions"), "[ ] Build").unwrap();

        let store = FsWorkspaceStore::new(root);
        let charters = store.discover_charters().unwrap();

        assert_eq!(charters.len(), 1);
        assert_eq!(charters[0].charter.title, "build_clearhead");
        assert!(!charters[0].is_explicit);
    }

    #[test]
    fn test_fs_store_save_and_load_charter() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        let mut store = FsWorkspaceStore::new(root);
        let obj = ObjectiveRef::new("health");

        let charter = Charter {
            id: uuid::Uuid::new_v4(),
            title: "Health & Fitness".to_string(),
            description: Some("Stay healthy.".to_string()),
            alias: Some("health".to_string()),
            parent: None,
            objectives: None,
            plans: vec![],
        };
        store.save_charter(&obj, &charter).unwrap();

        // File should exist
        assert!(root.join("health.md").exists());

        // Load it back via the charter path
        let loaded = store.load_charter(&obj).unwrap().unwrap();
        assert_eq!(loaded.title, "Health & Fitness");
    }

    #[test]
    fn test_fs_store_skips_hidden_dirs() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        fs::write(root.join("inbox.actions"), "[ ] Task").unwrap();
        let hidden = root.join(".clearhead");
        fs::create_dir(&hidden).unwrap();
        fs::write(hidden.join("internal.actions"), "[ ] Hidden").unwrap();

        let store = FsWorkspaceStore::new(root);
        let objectives = store.list_objectives().unwrap();

        assert_eq!(objectives.len(), 1);
        assert_eq!(objectives[0].key, "inbox.actions");
    }

    #[test]
    fn test_infer_project_name_rules() {
        assert_eq!(
            infer_project_name(Path::new("work.actions")),
            Some("work".to_string())
        );
        assert_eq!(infer_project_name(Path::new("inbox.actions")), None);
        assert_eq!(
            infer_project_name(Path::new("myproject/next.actions")),
            Some("myproject".to_string())
        );
        assert_eq!(
            infer_project_name(Path::new("myproject/logs/2026-01.actions")),
            Some("myproject".to_string())
        );
    }
}
