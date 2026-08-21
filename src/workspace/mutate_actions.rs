//! Pure preparation for action-file insertion, update, and deletion.

use thiserror::Error;
use uuid::Uuid;

use crate::domain::collect_subtree_ids;
use crate::domain::update::{ActionUpdate, apply_updates, disallowed_terminal_update};
use crate::workspace::actions::{Action, ActionList, OutputFormat, format};
use crate::workspace::resource::{
    Effect, EffectBatch, ExpectedResource, PreparedMutation, ResourcePrecondition, WorkspacePath,
};
use crate::workspace::selector::{ActionSelector, unique_selector_match};
use crate::workspace::sidecar::{CharterMetadata, render_sidecar};

/// A domain or codec failure while preparing an action mutation.
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum ActionPrepareError {
    #[error("{0}")]
    Domain(String),
}

/// Already-loaded logical action resource used by pure preparation.
#[derive(Debug, Clone)]
pub struct ActionResourceState {
    pub path: WorkspacePath,
    pub actions: ActionList,
    pub expected: ExpectedResource,
}

/// Already-loaded sidecar resource used by deletion preparation.
#[derive(Debug, Clone)]
pub struct SidecarResourceState {
    pub path: WorkspacePath,
    pub metadata: CharterMetadata,
    pub expected: ExpectedResource,
}

#[derive(Debug, Clone)]
pub struct PreparedInsertOutcome {
    pub action_id: Uuid,
    pub parent_id: Option<Uuid>,
    pub source_path: WorkspacePath,
}

#[derive(Debug, Clone)]
pub struct PreparedUpdateOutcome {
    pub action_id: Uuid,
    pub source_path: WorkspacePath,
}

#[derive(Debug, Clone)]
pub struct PreparedDeleteOutcome {
    pub action_id: Uuid,
    pub deleted_count: usize,
    pub source_path: WorkspacePath,
    pub from_completed: bool,
}

#[derive(Debug, Clone)]
pub struct DeletePreparedState {
    pub active: ActionList,
    pub completed: ActionList,
    pub active_sidecar: CharterMetadata,
    pub completed_sidecar: CharterMetadata,
}

/// Insert one action into an active list without touching the filesystem.
pub fn plan_action_insert(
    active: &[Action],
    new_action: Action,
    parent_id: Option<Uuid>,
) -> ActionList {
    let mut list = active.to_vec();
    let mut action = new_action;
    action.parent_id = parent_id;
    let index = parent_id
        .map(|id| index_after_subtree(&list, id))
        .unwrap_or(list.len());
    list.insert(index, action);
    list
}

fn index_after_subtree(actions: &[Action], parent_id: Uuid) -> usize {
    let subtree = collect_subtree_ids(actions, parent_id);
    actions
        .iter()
        .enumerate()
        .filter(|(_, action)| subtree.contains(&action.id))
        .map(|(index, _)| index + 1)
        .max()
        .unwrap_or(actions.len())
}

pub fn prepare_action_insert(
    source: ActionResourceState,
    new_action: Action,
    parent: Option<&ActionSelector>,
) -> Result<PreparedMutation<ActionList, PreparedInsertOutcome>, ActionPrepareError> {
    let parent_id = match parent {
        Some(selector) => Some(
            unique_selector_match(&source.actions, selector)
                .map_err(|error| ActionPrepareError::Domain(error.to_string()))?
                .ok_or_else(|| {
                    ActionPrepareError::Domain(format!(
                        "parent action not found in source file: {}",
                        selector.name
                    ))
                })?,
        ),
        None => None,
    };
    let action_id = new_action.id;
    let next = plan_action_insert(&source.actions, new_action, parent_id);
    let effects = write_batch(&source.path, &next, source.expected)?;
    Ok(PreparedMutation::with_outcome(
        next,
        effects,
        PreparedInsertOutcome {
            action_id,
            parent_id,
            source_path: source.path,
        },
    ))
}

pub fn prepare_action_update(
    source: ActionResourceState,
    selector: &ActionSelector,
    update: ActionUpdate,
) -> Result<PreparedMutation<ActionList, PreparedUpdateOutcome>, ActionPrepareError> {
    if let Some(state) = disallowed_terminal_update(&update) {
        return Err(ActionPrepareError::Domain(format!(
            "cannot set state to {state:?} via update; use complete/cancel, which cascade to the subtree and archive it"
        )));
    }
    let mut next = source.actions;
    let action_id = unique_selector_match(&next, selector)
        .map_err(|error| ActionPrepareError::Domain(error.to_string()))?
        .ok_or_else(|| {
            ActionPrepareError::Domain(format!(
                "open action not found in source file: {}",
                selector.name
            ))
        })?;
    let target = next
        .iter_mut()
        .find(|action| action.id == action_id)
        .expect("selected action came from active list");
    apply_updates(target, update);
    let effects = write_batch(&source.path, &next, source.expected)?;
    Ok(PreparedMutation::with_outcome(
        next,
        effects,
        PreparedUpdateOutcome {
            action_id,
            source_path: source.path,
        },
    ))
}

pub fn prepare_action_delete(
    active: ActionResourceState,
    completed: ActionResourceState,
    active_sidecar: SidecarResourceState,
    completed_sidecar: SidecarResourceState,
    selector: &ActionSelector,
) -> Result<PreparedMutation<DeletePreparedState, PreparedDeleteOutcome>, ActionPrepareError> {
    let mut active_actions = active.actions;
    let mut completed_actions = completed.actions;
    let (from_completed, action_id) = match unique_selector_match(&active_actions, selector)
        .map_err(|error| ActionPrepareError::Domain(error.to_string()))?
    {
        Some(id) => (false, id),
        None => match unique_selector_match(&completed_actions, selector)
            .map_err(|error| ActionPrepareError::Domain(error.to_string()))?
        {
            Some(id) => (true, id),
            None => {
                return Err(ActionPrepareError::Domain(format!(
                    "action not found in active or completed file: {}",
                    selector.name
                )));
            }
        },
    };
    let list = if from_completed {
        &mut completed_actions
    } else {
        &mut active_actions
    };
    let subtree_ids = collect_subtree_ids(list, action_id);
    list.retain(|action| !subtree_ids.contains(&action.id));
    let deleted_count = subtree_ids.len();

    let mut active_meta = active_sidecar.metadata;
    let mut completed_meta = completed_sidecar.metadata;
    let (file, meta_path, meta) = if from_completed {
        (
            &completed.path,
            &completed_sidecar.path,
            &mut completed_meta,
        )
    } else {
        (&active.path, &active_sidecar.path, &mut active_meta)
    };
    let before = meta.actions.len();
    meta.actions.retain(|id_str, _| {
        id_str
            .parse::<Uuid>()
            .map(|id| !subtree_ids.contains(&id))
            .unwrap_or(true)
    });

    let mut effects = vec![Effect::Write {
        path: file.clone(),
        bytes: render_actions(list)?.into_bytes(),
    }];
    let preconditions = vec![
        ResourcePrecondition {
            path: active.path.clone(),
            expected: active.expected,
        },
        ResourcePrecondition {
            path: completed.path.clone(),
            expected: completed.expected,
        },
        ResourcePrecondition {
            path: active_sidecar.path.clone(),
            expected: active_sidecar.expected,
        },
        ResourcePrecondition {
            path: completed_sidecar.path.clone(),
            expected: completed_sidecar.expected,
        },
    ];
    if meta.actions.len() != before {
        effects.push(Effect::Write {
            path: meta_path.clone(),
            bytes: render_sidecar(meta)
                .map_err(|error| ActionPrepareError::Domain(error.to_string()))?
                .into_bytes(),
        });
    }
    let batch = EffectBatch::new(effects, preconditions)
        .map_err(|error| ActionPrepareError::Domain(error.to_string()))?;
    Ok(PreparedMutation::with_outcome(
        DeletePreparedState {
            active: active_actions,
            completed: completed_actions,
            active_sidecar: active_meta,
            completed_sidecar: completed_meta,
        },
        batch,
        PreparedDeleteOutcome {
            action_id,
            deleted_count,
            source_path: file.clone(),
            from_completed,
        },
    ))
}

fn write_batch(
    path: &WorkspacePath,
    actions: &[Action],
    expected: ExpectedResource,
) -> Result<EffectBatch, ActionPrepareError> {
    EffectBatch::new(
        vec![Effect::Write {
            path: path.clone(),
            bytes: render_actions(actions)?.into_bytes(),
        }],
        vec![ResourcePrecondition {
            path: path.clone(),
            expected,
        }],
    )
    .map_err(|error| ActionPrepareError::Domain(error.to_string()))
}

fn render_actions(actions: &[Action]) -> Result<String, ActionPrepareError> {
    format(&actions.to_vec(), OutputFormat::Actions, None, None).map_err(ActionPrepareError::Domain)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::ActionState;

    fn action(name: &str, parent_id: Option<Uuid>) -> Action {
        Action {
            id: Uuid::new_v4(),
            name: name.to_string(),
            state: ActionState::NotStarted,
            parent_id,
            ..Default::default()
        }
    }

    #[test]
    fn plan_appends_a_parentless_action() {
        let active = vec![action("first", None), action("second", None)];
        let planned = plan_action_insert(&active, action("third", None), None);
        assert_eq!(planned[2].name, "third");
        assert_eq!(planned[2].parent_id, None);
    }

    #[test]
    fn plan_places_a_child_after_the_parents_subtree() {
        let parent = action("parent", None);
        let child = action("existing child", Some(parent.id));
        let sibling = action("later root", None);
        let planned = plan_action_insert(
            &[parent.clone(), child, sibling],
            action("new child", None),
            Some(parent.id),
        );
        assert_eq!(
            planned.iter().map(|a| a.name.as_str()).collect::<Vec<_>>(),
            ["parent", "existing child", "new child", "later root"]
        );
        assert_eq!(planned[2].parent_id, Some(parent.id));
    }

    fn resource(name: &str, actions: Vec<Action>) -> ActionResourceState {
        ActionResourceState {
            path: WorkspacePath::new(name).unwrap(),
            actions,
            expected: ExpectedResource::Missing,
        }
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn insert_preparation_emits_one_logical_write() {
        let inserted = action("inserted", None);
        let prepared = prepare_action_insert(
            resource("charters/work.actions", vec![]),
            inserted.clone(),
            None,
        )
        .unwrap();
        assert_eq!(prepared.effects().effects().len(), 1);
        assert_eq!(prepared.outcome().action_id, inserted.id);
        assert_eq!(
            prepared.outcome().source_path.as_str(),
            "charters/work.actions"
        );
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn delete_preparation_prunes_matching_sidecar_in_same_batch() {
        let parent = action("parent", None);
        let child = action("child", Some(parent.id));
        let mut metadata = CharterMetadata::default();
        metadata
            .actions
            .insert(parent.id.to_string(), Default::default());
        metadata
            .actions
            .insert(child.id.to_string(), Default::default());
        let prepared = prepare_action_delete(
            resource("charters/work.actions", vec![parent.clone(), child]),
            resource("charters/work.completed.actions", vec![]),
            SidecarResourceState {
                path: WorkspacePath::new("charters/.work.json").unwrap(),
                metadata,
                expected: ExpectedResource::Missing,
            },
            SidecarResourceState {
                path: WorkspacePath::new("charters/.work.completed.json").unwrap(),
                metadata: CharterMetadata::default(),
                expected: ExpectedResource::Missing,
            },
            &ActionSelector::from(&parent),
        )
        .unwrap();
        assert_eq!(prepared.effects().effects().len(), 2);
        assert_eq!(prepared.effects().preconditions().len(), 4);
        assert!(prepared.next_state().active_sidecar.actions.is_empty());
    }
}
