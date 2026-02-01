//! Synchronization logic for comparing and deciding when to sync actions
//!
//! This module provides pure functions for semantic comparison of ActionLists
//! and determining whether CRDT synchronization is needed.

use crate::diff::{diff_actions, Diff};
use crate::entities::ActionList;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::{Action, ActionState};
    use uuid::Uuid;

    fn make_action(state: ActionState, name: &str) -> Action {
        Action {
            id: Uuid::new_v4(),
            parent_id: None,
            state,
            name: name.to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: None,
            do_duration: None,
            recurrence: None,
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        }
    }

    #[test]
    fn test_semantically_equal_identical_actions() {
        let actions1: ActionList = vec![make_action(ActionState::NotStarted, "Task 1")];
        let actions2: ActionList = vec![make_action(ActionState::NotStarted, "Task 1")];

        // Different UUIDs so they won't be equal
        // The real test is when actions have same ID but different formatting
        assert!(!semantically_equal(&actions1, &actions2));
    }

    #[test]
    fn test_semantically_equal_same_instance() {
        let list: ActionList = vec![make_action(ActionState::NotStarted, "Task 1")];

        assert!(semantically_equal(&list, &list));
    }

    #[test]
    fn test_should_sync_no_change() {
        let list: ActionList = vec![make_action(ActionState::NotStarted, "Task 1")];

        let decision = should_sync(&list, &list);
        assert!(!decision.needs_sync());
        assert!(matches!(decision, SyncDecision::NoChange));
    }

    #[test]
    fn test_should_sync_detects_change() {
        let id = Uuid::new_v4();

        let mut action1 = make_action(ActionState::NotStarted, "Task 1");
        action1.id = id;

        let mut action2 = make_action(ActionState::Completed, "Task 1");
        action2.id = id;

        let list1: ActionList = vec![action1];
        let list2: ActionList = vec![action2];

        let decision = should_sync(&list2, &list1);
        assert!(decision.needs_sync());

        if let SyncDecision::SyncNeeded { changes } = decision {
            assert!(!changes.modified.is_empty());
        } else {
            panic!("Expected SyncNeeded");
        }
    }
}
