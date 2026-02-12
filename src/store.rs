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
//! assert_eq!(loaded.plans.len(), 0);
//! ```

use crate::domain::{Charter, DomainModel};
use std::collections::HashMap;
use std::fmt;

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
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::Action;

    #[test]
    fn test_in_memory_store_roundtrip() {
        let mut store = InMemoryStore::new();
        let obj = ObjectiveRef::new("inbox");

        // Initially empty
        let model = store.load_domain_model(&obj).unwrap();
        assert!(model.plans.is_empty());

        // Save a model with one action
        let action = Action::new("Test task");
        let model = DomainModel::from_actions(&vec![action]);
        store.save_domain_model(&obj, &model).unwrap();

        // Load it back
        let loaded = store.load_domain_model(&obj).unwrap();
        assert_eq!(loaded.plans.len(), 1);
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
        };
        let charter_b = Charter {
            id: uuid::Uuid::new_v4(),
            title: "Work".to_string(),
            description: None,
            alias: None,
            parent: None,
            objectives: None,
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
        };
        store.save_charter(&health, &charter).unwrap();

        // list_objectives returns the union
        let objectives = store.list_objectives().unwrap();
        assert_eq!(objectives.len(), 2);
    }
}
