//! State transitions and root-tree storage moves are separate operations.
use chrono::{DateTime, Local};
use uuid::Uuid;

use super::actions::{Action, OutputFormat, format};
use super::archive_actions::{PreparedArchiveOutcome, PreparedCloseOutcome, PreparedReopenOutcome};
use super::mutate_actions::{ActionPrepareError, ActionResourceState, SidecarResourceState};
use super::resource::{Effect, EffectBatch, ResourceLocation, ResourcePrecondition};
use super::selector::{ActionSelector, unique_selector_match};
use super::sidecar::render_sidecar;
use super::transaction::{
    FileState, NormalizedOperation, TransactionModel, prepare_transaction, tree_root,
};
use crate::ActionState;

fn error(e: impl std::fmt::Display) -> ActionPrepareError {
    ActionPrepareError::Domain(e.to_string())
}

pub fn prepare_close_action_subtree(
    active: ActionResourceState,
    completed: ActionResourceState,
    selector: &ActionSelector,
    state: ActionState,
    now: DateTime<Local>,
) -> Result<(EffectBatch, PreparedCloseOutcome), ActionPrepareError> {
    if !state.is_terminal() {
        return Err(error("close requires a terminal state"));
    }
    let id = select(&active.actions, &completed.actions, selector)?;
    let source_path = active.path.clone();
    let completed_path = completed.path.clone();
    let model = model(active, completed);
    let op = if state == ActionState::Cancelled {
        NormalizedOperation::Cancel { target: id }
    } else {
        NormalizedOperation::Complete { target: id }
    };
    let (batch, result) = prepare_transaction(model, &[op], now, false).map_err(error)?;
    if let super::transaction::PreparedTransactionOutcome::Rejected { error: e, .. } = result {
        return Err(error(e));
    }
    Ok((
        batch,
        PreparedCloseOutcome {
            action_id: id,
            closed_count: 1,
            source_path,
            completed_path,
            already_closed: false,
        },
    ))
}

pub fn prepare_reopen_action_subtree(
    active: ActionResourceState,
    completed: ActionResourceState,
    selector: &ActionSelector,
    now: DateTime<Local>,
) -> Result<(EffectBatch, PreparedReopenOutcome), ActionPrepareError> {
    let id = select(&active.actions, &completed.actions, selector)?;
    let source_path = active.path.clone();
    let completed_path = completed.path.clone();
    let (batch, result) = prepare_transaction(
        model(active, completed),
        &[NormalizedOperation::Reopen { target: id }],
        now,
        false,
    )
    .map_err(error)?;
    if let super::transaction::PreparedTransactionOutcome::Rejected { error: e, .. } = result {
        return Err(error(e));
    }
    Ok((
        batch,
        PreparedReopenOutcome {
            action_id: id,
            reopened_count: 1,
            source_path,
            completed_path,
            already_open: false,
        },
    ))
}

fn select(
    active: &[Action],
    completed: &[Action],
    selector: &ActionSelector,
) -> Result<Uuid, ActionPrepareError> {
    unique_selector_match(active, selector)
        .map_err(error)?
        .or(unique_selector_match(completed, selector).map_err(error)?)
        .ok_or_else(|| error(format!("action not found: {}", selector.id)))
}

fn model(active: ActionResourceState, completed: ActionResourceState) -> TransactionModel {
    TransactionModel::new(vec![FileState::new(
        active.path,
        completed.path,
        active.actions,
        completed.actions,
        active.expected,
        completed.expected,
    )])
}

/// Move eligible roots, or exactly one selected root. State and dates are facts
/// and are not changed by storage moves. Destination writes precede removals.
#[derive(Debug, Clone, Copy, Default)]
pub struct MoveMode {
    pub unarchive: bool,
    pub reopen: bool,
    pub cascade: bool,
}

pub fn prepare_action_move(
    mut active: ActionResourceState,
    mut completed: ActionResourceState,
    mut active_meta: SidecarResourceState,
    mut completed_meta: SidecarResourceState,
    selection: Option<Uuid>,
    mode: MoveMode,
) -> Result<(EffectBatch, PreparedArchiveOutcome), ActionPrepareError> {
    let MoveMode {
        unarchive,
        reopen,
        cascade,
    } = mode;
    let (source, destination, source_meta, destination_meta) = if unarchive {
        (
            &mut completed,
            &mut active,
            &mut completed_meta,
            &mut active_meta,
        )
    } else {
        (
            &mut active,
            &mut completed,
            &mut active_meta,
            &mut completed_meta,
        )
    };
    let roots: Vec<_> = if let Some(id) = selection {
        let target = source.actions.iter().find(|a| a.id == id).ok_or_else(|| {
            error(if unarchive {
                "action is already active or not found"
            } else {
                "action is archived or not found"
            })
        })?;
        let root = tree_root(&source.actions, target.id);
        if root != id {
            return Err(error(format!(
                "action {id} is not a root; use {} action {root}",
                if unarchive { "unarchive" } else { "archive" }
            )));
        }
        vec![id]
    } else {
        source
            .actions
            .iter()
            .filter(|a| a.parent_id.is_none())
            .map(|a| a.id)
            .collect()
    };
    let eligible = super::archive_actions::eligible_archive_ids(&source.actions);
    let mut ids = std::collections::HashSet::new();
    for root in roots {
        let tree = crate::collect_subtree_ids(&source.actions, root);
        if !unarchive && !eligible.contains(&root) {
            if selection.is_some() {
                return Err(error(format!(
                    "root {root} has open actions; close the whole tree before archive"
                )));
            }
            continue;
        }
        ids.extend(tree);
    }
    if destination.actions.iter().any(|a| ids.contains(&a.id)) {
        return Err(error("destination already contains part of the tree"));
    }
    let count = ids.len();
    let mut moved: Vec<_> = source
        .actions
        .iter()
        .filter(|a| ids.contains(&a.id))
        .cloned()
        .collect();
    if reopen {
        for action in &mut moved {
            if action.state.is_terminal() && (cascade || Some(action.id) == selection) {
                action.state = ActionState::NotStarted;
                action.completed_at = None;
            }
        }
    }
    source.actions.retain(|a| !ids.contains(&a.id));
    destination.actions.extend(moved);
    for id in &ids {
        let key = id.to_string();
        if let Some(entry) = source_meta.metadata.actions.remove(&key) {
            destination_meta.metadata.actions.insert(key, entry);
        }
    }
    let mut effects = Vec::new();
    if count > 0 {
        effects.push(Effect::Write {
            path: ResourceLocation::workspace(destination.path.clone()),
            bytes: format(&destination.actions, OutputFormat::Actions, None, None)
                .map_err(error)?
                .into_bytes(),
        });
        effects.push(Effect::Write {
            path: ResourceLocation::workspace(destination_meta.path.clone()),
            bytes: render_sidecar(&destination_meta.metadata)
                .map_err(error)?
                .into_bytes(),
        });
        effects.push(Effect::Write {
            path: ResourceLocation::workspace(source.path.clone()),
            bytes: format(&source.actions, OutputFormat::Actions, None, None)
                .map_err(error)?
                .into_bytes(),
        });
        effects.push(Effect::Write {
            path: ResourceLocation::workspace(source_meta.path.clone()),
            bytes: render_sidecar(&source_meta.metadata)
                .map_err(error)?
                .into_bytes(),
        });
    }
    let preconditions = vec![
        ResourcePrecondition {
            path: ResourceLocation::workspace(active.path.clone()),
            expected: active.expected,
        },
        ResourcePrecondition {
            path: ResourceLocation::workspace(completed.path.clone()),
            expected: completed.expected,
        },
        ResourcePrecondition {
            path: ResourceLocation::workspace(active_meta.path),
            expected: active_meta.expected,
        },
        ResourcePrecondition {
            path: ResourceLocation::workspace(completed_meta.path),
            expected: completed_meta.expected,
        },
    ];
    Ok((
        EffectBatch::new(effects, preconditions).map_err(error)?,
        PreparedArchiveOutcome {
            archived_count: count,
            source_path: active.path,
            completed_path: completed.path,
        },
    ))
}
