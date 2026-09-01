//! Pure preparation for action close and archival mutations.

use std::collections::HashSet;

use chrono::{DateTime, Local};
use uuid::Uuid;

use crate::domain::{close_subtree, collect_subtree_ids};
use crate::workspace::actions::{Action, ActionList, ActionState, OutputFormat, format};
use crate::workspace::mutate_actions::{ActionPrepareError, ActionResourceState};
use crate::workspace::resource::{
    Effect, EffectBatch, PreparedMutation, ResourceLocation, ResourcePrecondition, WorkspacePath,
};
use crate::workspace::selector::{ActionSelector, unique_selector_match};

/// Pure result of partitioning an active action file for archival.
#[derive(Debug, Clone)]
pub struct ActionArchivePlan {
    /// Actions that remain in the primary `.actions` file.
    pub active_actions: ActionList,
    /// Full post-operation contents of the `.completed.actions` file.
    pub completed_actions: ActionList,
    /// Number of actions moved by this plan.
    pub archived_count: usize,
}

/// Build an action-archive plan without reading or writing the filesystem.
///
/// Only complete terminal trees are archived. A completed/cancelled root with
/// an open descendant remains active, preserving the structural parent chain.
/// Archived roots are detached from external parents, while descendants keep
/// parent links inside the archived subtree. Completed files are history, but
/// the hierarchy is part of the fact (especially for templated recurring
/// occurrences). Any archived action without a completion date is stamped at
/// plan construction time; an existing completion date is preserved.
pub fn plan_action_archive(active: &[Action], existing_completed: &[Action]) -> ActionArchivePlan {
    plan_action_archive_at(active, existing_completed, Local::now())
}

fn plan_action_archive_at(
    active: &[Action],
    existing_completed: &[Action],
    archived_at: DateTime<Local>,
) -> ActionArchivePlan {
    let archive_root_ids: HashSet<_> = active
        .iter()
        .filter(|action| action.parent_id.is_none() && is_terminal(action))
        .filter(|root| descendants(active, root.id).into_iter().all(is_terminal))
        .map(|action| action.id)
        .collect();

    let mut archive_ids = HashSet::new();
    for root_id in archive_root_ids {
        archive_ids.insert(root_id);
        let mut frontier = vec![root_id];
        while let Some(parent_id) = frontier.pop() {
            for child in active
                .iter()
                .filter(|action| action.parent_id == Some(parent_id))
            {
                if archive_ids.insert(child.id) {
                    frontier.push(child.id);
                }
            }
        }
    }

    let mut active_actions = Vec::new();
    let mut archived_actions = Vec::new();
    for action in active {
        if archive_ids.contains(&action.id) {
            let mut archived = action.clone();
            if !archived
                .parent_id
                .is_some_and(|parent| archive_ids.contains(&parent))
            {
                archived.parent_id = None;
            }
            if archived.completed_at.is_none() {
                archived.completed_at = Some(archived_at);
            }
            archived_actions.push(archived);
        } else {
            active_actions.push(action.clone());
        }
    }

    let archived_count = archived_actions.len();
    let mut completed_actions = existing_completed.to_vec();
    completed_actions.extend(archived_actions);

    ActionArchivePlan {
        active_actions,
        completed_actions,
        archived_count,
    }
}

#[derive(Debug, Clone)]
pub struct PreparedArchiveOutcome {
    pub archived_count: usize,
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
}

#[derive(Debug, Clone)]
pub struct PreparedCloseOutcome {
    pub action_id: Uuid,
    pub closed_count: usize,
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
    pub already_closed: bool,
}

#[derive(Debug, Clone)]
pub struct ClosePreparedState {
    pub active: ActionList,
    pub completed: ActionList,
}

#[derive(Debug, Clone)]
pub struct PreparedReopenOutcome {
    pub action_id: Uuid,
    pub reopened_count: usize,
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
    pub already_open: bool,
}

pub fn prepare_action_archive(
    active: ActionResourceState,
    completed: ActionResourceState,
    archived_at: DateTime<Local>,
) -> Result<PreparedMutation<ClosePreparedState, PreparedArchiveOutcome>, ActionPrepareError> {
    let plan = plan_action_archive_at(&active.actions, &completed.actions, archived_at);
    let mut effects = Vec::new();
    if plan.archived_count > 0 {
        effects.push(write_effect(&active.path, &plan.active_actions)?);
        effects.push(write_effect(&completed.path, &plan.completed_actions)?);
    }
    let batch = EffectBatch::new(
        effects,
        vec![
            ResourcePrecondition {
                path: ResourceLocation::workspace(active.path.clone()),
                expected: active.expected,
            },
            ResourcePrecondition {
                path: ResourceLocation::workspace(completed.path.clone()),
                expected: completed.expected,
            },
        ],
    )
    .map_err(|error| ActionPrepareError::Domain(error.to_string()))?;
    Ok(PreparedMutation::with_outcome(
        ClosePreparedState {
            active: plan.active_actions,
            completed: plan.completed_actions,
        },
        batch,
        PreparedArchiveOutcome {
            archived_count: plan.archived_count,
            source_path: active.path,
            completed_path: completed.path,
        },
    ))
}

pub fn prepare_close_action_subtree(
    active: ActionResourceState,
    completed: ActionResourceState,
    selector: &ActionSelector,
    closing_state: ActionState,
    completed_at: DateTime<Local>,
) -> Result<PreparedMutation<ClosePreparedState, PreparedCloseOutcome>, ActionPrepareError> {
    if !matches!(
        closing_state,
        ActionState::Completed | ActionState::Cancelled
    ) {
        return Err(ActionPrepareError::Domain(
            "an action subtree can only be closed as Completed or Cancelled".into(),
        ));
    }
    let mut active_actions = active.actions;
    let mut completed_actions = completed.actions;
    let action_id = unique_selector_match(&active_actions, selector)
        .map_err(|error| ActionPrepareError::Domain(error.to_string()))?;
    let Some(action_id) = action_id else {
        if let Some(completed_id) = unique_selector_match(&completed_actions, selector)
            .map_err(|error| ActionPrepareError::Domain(error.to_string()))?
        {
            let batch = EffectBatch::new(
                Vec::new(),
                vec![
                    ResourcePrecondition {
                        path: ResourceLocation::workspace(active.path.clone()),
                        expected: active.expected,
                    },
                    ResourcePrecondition {
                        path: ResourceLocation::workspace(completed.path.clone()),
                        expected: completed.expected,
                    },
                ],
            )
            .map_err(|error| ActionPrepareError::Domain(error.to_string()))?;
            return Ok(PreparedMutation::with_outcome(
                ClosePreparedState {
                    active: active_actions,
                    completed: completed_actions,
                },
                batch,
                PreparedCloseOutcome {
                    action_id: completed_id,
                    closed_count: 0,
                    source_path: active.path,
                    completed_path: completed.path,
                    already_closed: true,
                },
            ));
        }
        return Err(ActionPrepareError::Domain(format!(
            "open action not found in source file: {}",
            selector.id
        )));
    };
    let target = active_actions
        .iter()
        .find(|action| action.id == action_id)
        .expect("selected action came from active list");
    if matches!(
        target.state,
        ActionState::Completed | ActionState::Cancelled
    ) {
        return Err(ActionPrepareError::Domain(format!(
            "action is already terminal in the active file: {action_id}"
        )));
    }
    let subtree_ids = collect_subtree_ids(&active_actions, action_id);
    if completed_actions
        .iter()
        .any(|action| subtree_ids.contains(&action.id))
    {
        return Err(ActionPrepareError::Domain(format!(
            "completed history already contains part of subtree: {action_id}"
        )));
    }
    let mut closed = close_subtree(&active_actions, action_id, closing_state, completed_at);
    active_actions.retain(|action| !subtree_ids.contains(&action.id));
    let closed_count = closed.len();
    completed_actions.append(&mut closed);
    let batch = EffectBatch::new(
        vec![
            write_effect(&active.path, &active_actions)?,
            write_effect(&completed.path, &completed_actions)?,
        ],
        vec![
            ResourcePrecondition {
                path: ResourceLocation::workspace(active.path.clone()),
                expected: active.expected,
            },
            ResourcePrecondition {
                path: ResourceLocation::workspace(completed.path.clone()),
                expected: completed.expected,
            },
        ],
    )
    .map_err(|error| ActionPrepareError::Domain(error.to_string()))?;
    Ok(PreparedMutation::with_outcome(
        ClosePreparedState {
            active: active_actions,
            completed: completed_actions,
        },
        batch,
        PreparedCloseOutcome {
            action_id,
            closed_count,
            source_path: active.path,
            completed_path: completed.path,
            already_closed: false,
        },
    ))
}

/// Pure preparation for reopening a completed subtree — the inverse of
/// [`prepare_close_action_subtree`]. The target is resolved in the *completed*
/// file, its subtree is driven back to `NotStarted` (see
/// [`reopen_subtree`](crate::domain::reopen_subtree)), removed from the
/// completed history, and appended to the active file.
///
/// When the selector matches nothing in the completed file but an open action
/// in the active file, this is a no-op with `already_open == true`, mirroring
/// how closing an already-terminal action reports `already_closed`.
pub fn prepare_reopen_action_subtree(
    active: ActionResourceState,
    completed: ActionResourceState,
    selector: &ActionSelector,
) -> Result<PreparedMutation<ClosePreparedState, PreparedReopenOutcome>, ActionPrepareError> {
    let mut active_actions = active.actions;
    let mut completed_actions = completed.actions;
    let completed_id = unique_selector_match(&completed_actions, selector)
        .map_err(|error| ActionPrepareError::Domain(error.to_string()))?;
    let Some(action_id) = completed_id else {
        // Nothing to reopen in history. If it's already an open line, treat the
        // request as satisfied rather than an error — the desired end state
        // (action live in the active file) already holds.
        if let Some(open_id) = unique_selector_match(&active_actions, selector)
            .map_err(|error| ActionPrepareError::Domain(error.to_string()))?
        {
            let batch = read_set_batch(
                &active.path,
                active.expected,
                &completed.path,
                completed.expected,
            )?;
            return Ok(PreparedMutation::with_outcome(
                ClosePreparedState {
                    active: active_actions,
                    completed: completed_actions,
                },
                batch,
                PreparedReopenOutcome {
                    action_id: open_id,
                    reopened_count: 0,
                    source_path: active.path,
                    completed_path: completed.path,
                    already_open: true,
                },
            ));
        }
        return Err(ActionPrepareError::Domain(format!(
            "completed action not found in history: {}",
            selector.id
        )));
    };
    let subtree_ids = collect_subtree_ids(&completed_actions, action_id);
    if active_actions
        .iter()
        .any(|action| subtree_ids.contains(&action.id))
    {
        return Err(ActionPrepareError::Domain(format!(
            "active file already contains part of subtree: {action_id}"
        )));
    }
    let mut reopened = crate::domain::reopen_subtree(&completed_actions, action_id);
    completed_actions.retain(|action| !subtree_ids.contains(&action.id));
    let reopened_count = reopened.len();
    active_actions.append(&mut reopened);
    let batch = EffectBatch::new(
        vec![
            write_effect(&active.path, &active_actions)?,
            write_effect(&completed.path, &completed_actions)?,
        ],
        vec![
            ResourcePrecondition {
                path: ResourceLocation::workspace(active.path.clone()),
                expected: active.expected,
            },
            ResourcePrecondition {
                path: ResourceLocation::workspace(completed.path.clone()),
                expected: completed.expected,
            },
        ],
    )
    .map_err(|error| ActionPrepareError::Domain(error.to_string()))?;
    Ok(PreparedMutation::with_outcome(
        ClosePreparedState {
            active: active_actions,
            completed: completed_actions,
        },
        batch,
        PreparedReopenOutcome {
            action_id,
            reopened_count,
            source_path: active.path,
            completed_path: completed.path,
            already_open: false,
        },
    ))
}

/// The read-set-only precondition batch (no effects) shared by no-op mutation
/// paths: nothing changes, but the resources consulted are still pinned so a
/// concurrent write is detected.
fn read_set_batch(
    active_path: &WorkspacePath,
    active_expected: crate::workspace::resource::ExpectedResource,
    completed_path: &WorkspacePath,
    completed_expected: crate::workspace::resource::ExpectedResource,
) -> Result<EffectBatch, ActionPrepareError> {
    EffectBatch::new(
        Vec::new(),
        vec![
            ResourcePrecondition {
                path: ResourceLocation::workspace(active_path.clone()),
                expected: active_expected,
            },
            ResourcePrecondition {
                path: ResourceLocation::workspace(completed_path.clone()),
                expected: completed_expected,
            },
        ],
    )
    .map_err(|error| ActionPrepareError::Domain(error.to_string()))
}

fn write_effect(path: &WorkspacePath, actions: &[Action]) -> Result<Effect, ActionPrepareError> {
    let bytes = format(&actions.to_vec(), OutputFormat::Actions, None, None)
        .map_err(ActionPrepareError::Domain)?
        .into_bytes();
    Ok(Effect::Write {
        path: ResourceLocation::workspace(path.clone()),
        bytes,
    })
}

fn is_terminal(action: &Action) -> bool {
    matches!(
        action.state,
        ActionState::Completed | ActionState::Cancelled
    )
}

fn descendants(actions: &[Action], root_id: Uuid) -> Vec<&Action> {
    let mut descendants = Vec::new();
    let mut seen = HashSet::from([root_id]);
    let mut frontier = vec![root_id];
    while let Some(parent_id) = frontier.pop() {
        for child in actions
            .iter()
            .filter(|action| action.parent_id == Some(parent_id))
        {
            if seen.insert(child.id) {
                descendants.push(child);
                frontier.push(child.id);
            }
        }
    }
    descendants
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    fn action(name: &str, state: ActionState, parent_id: Option<Uuid>) -> Action {
        Action {
            name: name.to_string(),
            state,
            parent_id,
            ..Default::default()
        }
    }

    #[test]
    fn plan_archives_complete_terminal_trees_and_preserves_existing_history() {
        let root = action("done root", ActionState::Completed, None);
        let mut child = action("cancelled child", ActionState::Cancelled, Some(root.id));
        let existing_date = Local.with_ymd_and_hms(2026, 7, 1, 9, 0, 0).unwrap();
        child.completed_at = Some(existing_date);
        let open = action("still open", ActionState::NotStarted, None);
        let existing = action("older", ActionState::Completed, None);
        let archived_at = Local.with_ymd_and_hms(2026, 7, 31, 10, 30, 0).unwrap();
        let plan = plan_action_archive_at(
            &[root.clone(), child, open.clone()],
            &[existing],
            archived_at,
        );
        assert_eq!(plan.archived_count, 2);
        assert_eq!(plan.active_actions[0].id, open.id);
        assert_eq!(plan.completed_actions[1].completed_at, Some(archived_at));
        assert_eq!(plan.completed_actions[2].completed_at, Some(existing_date));
    }

    #[test]
    fn plan_keeps_terminal_root_when_a_descendant_is_open() {
        let root = action("done root", ActionState::Completed, None);
        let child = action("open child", ActionState::NotStarted, Some(root.id));
        let plan = plan_action_archive(&[root, child], &[]);
        assert_eq!(plan.archived_count, 0);
        assert_eq!(plan.active_actions.len(), 2);
    }

    #[cfg(feature = "formatting")]
    fn resource(name: &str, actions: Vec<Action>) -> ActionResourceState {
        ActionResourceState {
            path: WorkspacePath::new(name).unwrap(),
            actions,
            expected: crate::workspace::resource::ExpectedResource::Missing,
        }
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn close_preparation_emits_active_and_completed_writes() {
        let target = action("target", ActionState::NotStarted, None);
        let prepared = prepare_close_action_subtree(
            resource("charters/work.actions", vec![target.clone()]),
            resource("charters/work.completed.actions", vec![]),
            &ActionSelector::from(&target),
            ActionState::Completed,
            Local::now(),
        )
        .unwrap();
        assert_eq!(prepared.effects().effects().len(), 2);
        assert_eq!(prepared.effects().preconditions().len(), 2);
        assert_eq!(prepared.outcome().closed_count, 1);
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn no_op_archive_retains_read_set_preconditions_without_effects() {
        let prepared = prepare_action_archive(
            resource(
                "charters/work.actions",
                vec![action("open", ActionState::NotStarted, None)],
            ),
            resource("charters/work.completed.actions", vec![]),
            Local::now(),
        )
        .unwrap();
        assert!(prepared.effects().is_empty());
        assert_eq!(prepared.effects().preconditions().len(), 2);
    }

    #[test]
    fn reopen_resets_whole_subtree_to_not_started_and_clears_completion() {
        let root = {
            let mut r = action("done root", ActionState::Completed, None);
            r.completed_at = Some(Local::now());
            r
        };
        let child = {
            let mut c = action("done child", ActionState::Cancelled, Some(root.id));
            c.completed_at = Some(Local::now());
            c
        };
        let reopened = crate::domain::reopen_subtree(&[root.clone(), child.clone()], root.id);
        assert_eq!(reopened.len(), 2);
        assert!(
            reopened
                .iter()
                .all(|a| a.state == ActionState::NotStarted && a.completed_at.is_none())
        );
        // The intra-subtree parent link survives; the root stays detached.
        assert_eq!(reopened[0].parent_id, None);
        assert_eq!(reopened[1].parent_id, Some(root.id));
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn reopen_preparation_moves_subtree_from_completed_to_active() {
        let root = {
            let mut r = action("done", ActionState::Completed, None);
            r.completed_at = Some(Local::now());
            r
        };
        let prepared = prepare_reopen_action_subtree(
            resource("charters/work.actions", vec![]),
            resource("charters/work.completed.actions", vec![root.clone()]),
            &ActionSelector::from(&root),
        )
        .unwrap();
        assert_eq!(prepared.effects().effects().len(), 2);
        assert_eq!(prepared.outcome().reopened_count, 1);
        assert!(!prepared.outcome().already_open);
        assert_eq!(prepared.outcome().action_id, root.id);
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn reopen_of_an_already_open_action_is_a_no_op() {
        let open = action("still open", ActionState::NotStarted, None);
        let prepared = prepare_reopen_action_subtree(
            resource("charters/work.actions", vec![open.clone()]),
            resource("charters/work.completed.actions", vec![]),
            &ActionSelector::from(&open),
        )
        .unwrap();
        assert!(prepared.effects().is_empty());
        assert!(prepared.outcome().already_open);
        assert_eq!(prepared.outcome().reopened_count, 0);
    }
}
