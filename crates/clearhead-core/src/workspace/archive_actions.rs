//! Pure partitioning for root-tree archival. Closure is a separate transition.
use super::actions::{Action, ActionList};
use super::resource::WorkspacePath;
use crate::domain::collect_subtree_ids;
use chrono::{DateTime, Local};
use std::collections::HashSet;
use uuid::Uuid;

#[derive(Debug, Clone)]
pub struct ActionArchivePlan {
    pub active_actions: ActionList,
    pub completed_actions: ActionList,
    pub archived_count: usize,
}

/// The eligible root trees, preserving factual states, dates and parent links.
pub(crate) fn eligible_archive_ids(active: &[Action]) -> HashSet<Uuid> {
    active
        .iter()
        .filter(|a| a.parent_id.is_none())
        .flat_map(|root| {
            let ids = collect_subtree_ids(active, root.id);
            if active
                .iter()
                .any(|a| ids.contains(&a.id) && !a.state.is_terminal())
            {
                Vec::new()
            } else {
                ids
            }
        })
        .collect()
}

/// Partition whole fully closed root trees. Storage never invents a closure date.
/// The time argument is retained for source compatibility; it does not affect facts.
pub fn plan_action_archive(
    active: &[Action],
    existing_completed: &[Action],
    _archived_at: DateTime<Local>,
) -> ActionArchivePlan {
    let ids = eligible_archive_ids(active);
    let mut completed_actions = existing_completed.to_vec();
    completed_actions.extend(active.iter().filter(|a| ids.contains(&a.id)).cloned());
    ActionArchivePlan {
        active_actions: active
            .iter()
            .filter(|a| !ids.contains(&a.id))
            .cloned()
            .collect(),
        completed_actions,
        archived_count: ids.len(),
    }
}

#[derive(Debug, Clone)]
pub struct PreparedArchiveOutcome {
    pub archived_count: usize,
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
}
