//! Synchronization logic for comparing and deciding when to sync actions
//!
//! This module provides pure functions for semantic comparison of ActionLists
//! and determining whether CRDT synchronization is needed.

use crate::domain::diff::{DomainDiff, diff_domain_models};
use crate::domain::DomainModel;
use crate::workspace::actions::{ActionList, Diff, diff_actions};

/// Compare two action lists semantically (ignoring formatting/whitespace)
///
/// Returns true if the actions are semantically identical, even if they differ
/// in whitespace, ordering of metadata, or other non-semantic aspects.
pub fn semantically_equal(a: &ActionList, b: &ActionList) -> bool {
    diff_actions(a, b).is_empty()
}

/// Determine if sync is needed based on semantic comparison
///
/// Compares the current state with the CRDT state and returns a decision
/// indicating whether synchronization should occur and what changed.
pub fn should_sync(current: &ActionList, crdt_state: &ActionList) -> SyncDecision {
    if semantically_equal(current, crdt_state) {
        SyncDecision::NoChange
    } else {
        SyncDecision::SyncNeeded {
            changes: diff_actions(crdt_state, current),
        }
    }
}

/// Decision about whether to sync based on semantic comparison
#[derive(Debug, Clone)]
pub enum SyncDecision {
    /// No semantic changes detected - preserve current formatting
    NoChange,
    /// Semantic changes detected - sync is needed
    SyncNeeded { changes: Diff },
}

impl SyncDecision {
    /// Returns true if sync is needed
    pub fn needs_sync(&self) -> bool {
        matches!(self, SyncDecision::SyncNeeded { .. })
    }
}

// ============================================================================
// Domain Model Sync
// ============================================================================

/// Compare two DomainModels semantically.
///
/// Returns true if the models are semantically identical.
pub fn domain_semantically_equal(a: &DomainModel, b: &DomainModel) -> bool {
    diff_domain_models(a, b).is_empty()
}

/// Determine if sync is needed based on domain model comparison.
pub fn should_sync_model(current: &DomainModel, crdt_state: &DomainModel) -> DomainSyncDecision {
    if domain_semantically_equal(current, crdt_state) {
        DomainSyncDecision::NoChange
    } else {
        DomainSyncDecision::SyncNeeded {
            changes: diff_domain_models(crdt_state, current),
        }
    }
}

/// Decision about whether to sync based on domain model comparison.
#[derive(Debug, Clone)]
pub enum DomainSyncDecision {
    /// No semantic changes detected
    NoChange,
    /// Semantic changes detected - sync is needed
    SyncNeeded { changes: DomainDiff },
}

impl DomainSyncDecision {
    pub fn needs_sync(&self) -> bool {
        matches!(self, DomainSyncDecision::SyncNeeded { .. })
    }
}
