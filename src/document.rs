//! Document processing and save orchestration
//!
//! This module coordinates the parse → sync → format pipeline for document saves.
//! It delegates to specialized modules for parsing, CRDT operations, and formatting.

use crate::Diff;
use crate::crdt::ActionRepository;
use crate::domain::{DomainDiff, DomainModel};
use crate::parse_document;
use crate::sync::{DomainSyncDecision, SyncDecision, should_sync, should_sync_model};

/// Result of processing a document save
#[derive(Debug)]
pub enum SaveResult {
    /// No semantic changes detected - preserve user's formatting
    NoChange,
    /// Semantic changes detected and synced - new content available
    Changed {
        /// The new formatted content from CRDT
        new_content: String,
        /// Detailed changes that were made
        changes: Diff,
    },
}

/// Process a document save event
///
/// This function orchestrates the complete save pipeline:
/// 1. Parse current buffer content
/// 2. Compare with CRDT state semantically
/// 3. If no semantic changes, return NoChange to preserve formatting
/// 4. If semantic changes detected, sync to CRDT and return new content
///
/// # Arguments
/// * `current_content` - The current buffer content as a string
/// * `file_path` - Path to the file (for CRDT repository)
/// * `crdt_repo` - Mutable reference to the CRDT repository
///
/// # Returns
/// * `Ok(SaveResult::NoChange)` - No semantic changes, preserve user formatting
/// * `Ok(SaveResult::Changed)` - Semantic changes synced, new content available
/// * `Err(String)` - Error during processing
pub fn process_save(
    current_content: &str,
    _file_path: &str,
    crdt_repo: &mut ActionRepository,
) -> Result<SaveResult, String> {
    // 1. Parse current content
    let parsed = parse_document(current_content)?;

    // 2. Get CRDT state
    let crdt_state = crdt_repo
        .get_actions()
        .map_err(|e| format!("Failed to get CRDT state: {}", e))?;

    // 3. Check if sync is needed (semantic comparison)
    match should_sync(&parsed.actions, &crdt_state) {
        SyncDecision::NoChange => {
            // No semantic changes - preserve user's formatting
            Ok(SaveResult::NoChange)
        }
        SyncDecision::SyncNeeded { changes } => {
            // 4. Sync to CRDT (returns canonical formatted content)
            let canonical = crdt_repo
                .save(&parsed.actions)
                .map_err(|e| format!("Failed to save to CRDT: {}", e))?;

            Ok(SaveResult::Changed {
                new_content: canonical,
                changes,
            })
        }
    }
}

// ============================================================================
// Domain-centric save processing
// ============================================================================

/// Result of processing a domain-centric document save.
#[derive(Debug)]
pub enum DomainSaveResult {
    /// No semantic changes detected - preserve user's formatting
    NoChange,
    /// Semantic changes detected and synced - new content available
    Changed {
        /// The new formatted content from CRDT
        new_content: String,
        /// Detailed domain-level changes that were made
        changes: DomainDiff,
    },
}

/// Process a document save using domain model comparison.
///
/// Like `process_save()`, but compares at the DomainModel level rather than
/// the ActionList level, giving more precise change tracking (plan vs act changes).
pub fn process_save_model(
    current_content: &str,
    _file_path: &str,
    crdt_repo: &mut ActionRepository,
) -> Result<DomainSaveResult, String> {
    // 1. Parse current content → DomainModel
    let parsed = parse_document(current_content)?;
    let current_model = DomainModel::from_actions(&parsed.actions);

    // 2. Get CRDT state as DomainModel
    let crdt_model = crdt_repo
        .get_domain_model()
        .map_err(|e| format!("Failed to get CRDT state: {}", e))?;

    // 3. Check if sync is needed (domain-level comparison)
    match should_sync_model(&current_model, &crdt_model) {
        DomainSyncDecision::NoChange => Ok(DomainSaveResult::NoChange),
        DomainSyncDecision::SyncNeeded { changes } => {
            // 4. Sync to CRDT
            let canonical = crdt_repo
                .save_model(&current_model)
                .map_err(|e| format!("Failed to save to CRDT: {}", e))?;

            Ok(DomainSaveResult::Changed {
                new_content: canonical,
                changes,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn setup_test_repo() -> (TempDir, ActionRepository) {
        let temp_dir = TempDir::new().unwrap();
        let workspace_dir = temp_dir.path().to_path_buf();
        let file_path = workspace_dir.join("test.actions");

        // Create the file so it exists
        std::fs::write(&file_path, "").unwrap();

        let repo = ActionRepository::test_repo(file_path, workspace_dir).unwrap();
        (temp_dir, repo)
    }

    #[test]
    fn test_process_save_initial_save() {
        let (_temp, mut repo) = setup_test_repo();
        let content = "[ ] Task 1\n[ ] Task 2\n";

        let result = process_save(content, "test.actions", &mut repo).unwrap();

        // First save should sync (CRDT starts empty)
        match result {
            SaveResult::Changed { new_content, .. } => {
                assert!(!new_content.is_empty());
            }
            SaveResult::NoChange => {
                panic!("Expected Changed on initial save");
            }
        }
    }

    #[test]
    fn test_process_save_model_initial_save() {
        let (_temp, mut repo) = setup_test_repo();
        let content = "[ ] Task 1\n[ ] Task 2\n";

        let result = process_save_model(content, "test.actions", &mut repo).unwrap();

        match result {
            DomainSaveResult::Changed {
                new_content,
                changes,
            } => {
                assert!(!new_content.is_empty());
                // Should have 2 plans and 2 acts added
                assert_eq!(changes.plans_added.len(), 2);
                assert_eq!(changes.acts_added.len(), 2);
            }
            DomainSaveResult::NoChange => {
                panic!("Expected Changed on initial save");
            }
        }
    }

    #[test]
    fn test_process_save_model_no_semantic_change() {
        let (_temp, mut repo) = setup_test_repo();
        let content = "[ ] Task 1\n";

        // Initial save
        let first_result = process_save_model(content, "test.actions", &mut repo).unwrap();
        let content_with_id = match first_result {
            DomainSaveResult::Changed { new_content, .. } => new_content,
            _ => panic!("Expected Changed on first save"),
        };

        // Save again — should be NoChange
        let result = process_save_model(&content_with_id, "test.actions", &mut repo).unwrap();
        assert!(matches!(result, DomainSaveResult::NoChange));
    }

    #[test]
    fn test_process_save_no_semantic_change() {
        let (_temp, mut repo) = setup_test_repo();
        let content = "[ ] Task 1\n";

        // Initial save - creates ID
        let first_result = process_save(content, "test.actions", &mut repo).unwrap();

        // Get the formatted content with ID
        let content_with_id = match first_result {
            SaveResult::Changed { new_content, .. } => new_content,
            _ => panic!("Expected Changed on first save"),
        };

        // Save again with same semantic content but maybe different formatting
        // (simulating user running `=` which preserves IDs but changes whitespace)
        let result = process_save(&content_with_id, "test.actions", &mut repo).unwrap();

        match result {
            SaveResult::NoChange => {
                // Expected - no semantic changes
            }
            SaveResult::Changed { .. } => {
                panic!("Should not sync when no semantic changes");
            }
        }
    }
}
