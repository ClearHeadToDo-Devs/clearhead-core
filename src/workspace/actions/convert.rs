use super::{Action, ActionList, ActionState, PredecessorRef};
use crate::domain::{ActPhase, Charter, DomainModel, Plan, PlannedAct};
use uuid::Uuid;

/// Namespace UUID for the synthetic inbox charter.
pub const INBOX_CHARTER_NS: Uuid = Uuid::from_bytes([
    0x69, 0x6e, 0x62, 0x6f, 0x78, 0x2d, 0x63, 0x68, 0x61, 0x72, 0x74, 0x65, 0x72, 0x2d, 0x6e, 0x73,
]);
/// Convert an ActionList into a Charter with a deterministic ID derived from the name.
pub fn from_actions_with_charter(actions: &ActionList, charter_name: String) -> Charter {
    let mut charter = crate::workspace::charter::implicit_charter(&charter_name);
    charter.plans = actions.iter().map(|a| a.into()).collect();
    charter
}

/// Split an Action into its Plan and PlannedAct components.
///
/// The Action.id becomes the Plan.id (it identifies the task definition).
/// The PlannedAct gets a new UUID (it's a specific occurrence).
impl From<&Action> for Plan {
    fn from(action: &Action) -> Self {
        let plan_id = action.id;
        let act_id = Uuid::new_v5(&plan_id, b"act-0");

        let act = PlannedAct {
            id: act_id,
            plan_id,
            phase: match action.state {
                ActionState::NotStarted => ActPhase::NotStarted,
                ActionState::InProgress => ActPhase::InProgress,
                ActionState::Completed => ActPhase::Completed,
                ActionState::BlockedorAwaiting => ActPhase::Blocked,
                ActionState::Cancelled => ActPhase::Cancelled,
            },
            scheduled_at: action.do_date_time,
            due_date: action.due_date_time,
            duration: action.do_duration,
            completed_at: action.completed_date_time,
            created_at: action.created_date_time,
        };

        let plan = Plan {
            id: plan_id,
            name: action.name.clone(),
            description: action.description.clone(),
            priority: action.priority,
            contexts: action.context_list.clone(),
            recurrence: action.recurrence.clone(),
            due_recurrence: action.due_recurrence.clone(),
            parent: action.parent_id,
            alias: action.alias.clone(),
            is_sequential: action.is_sequential,
            depends_on: action
                .predecessors
                .as_ref()
                .map(|preds| preds.iter().filter_map(|p| p.resolved_uuid).collect()),
            acts: vec![act],
        };

        plan
    }
}

/// Merge a Plan and PlannedAct into a single Action.
pub fn merge_to_action(plan: &Plan, act: &PlannedAct, target_id: Uuid) -> Action {
    Action {
        id: target_id,
        parent_id: plan.parent,
        state: match act.phase {
            ActPhase::NotStarted => ActionState::NotStarted,
            ActPhase::InProgress => ActionState::InProgress,
            ActPhase::Completed => ActionState::Completed,
            ActPhase::Blocked => ActionState::BlockedorAwaiting,
            ActPhase::Cancelled => ActionState::Cancelled,
        },
        name: plan.name.clone(),
        description: plan.description.clone(),
        priority: plan.priority,
        context_list: plan.contexts.clone(),
        do_date_time: act.scheduled_at,
        do_duration: act.duration,
        recurrence: plan.recurrence.clone(),
        due_date_time: act.due_date,
        due_recurrence: plan.due_recurrence.clone(),
        completed_date_time: act.completed_at,
        created_date_time: act.created_at,
        predecessors: plan.depends_on.as_ref().map(|uuids| {
            uuids
                .iter()
                .map(|u| PredecessorRef {
                    raw_ref: u.to_string(),
                    resolved_uuid: Some(*u),
                })
                .collect()
        }),
        charter: None,
        alias: plan.alias.clone(),
        is_sequential: plan.is_sequential,
    }
}

/// Patch a primary ActionList with updates from a secondary list.
///
/// Updates existing actions by ID, appends new ones.
pub fn patch_action_list(primary: &mut ActionList, secondary: &ActionList) {
    for patch_action in secondary {
        if let Some(original) = primary.iter_mut().find(|a| a.id == patch_action.id) {
            *original = patch_action.clone();
        } else {
            primary.push(patch_action.clone());
        }
    }
}

/// Convert a DomainModel back to an ActionList.
///
/// Walks the charter → plan → act hierarchy.
pub fn to_action_list(model: &DomainModel) -> ActionList {
    let plan_ids: Vec<String> = model.all_plans().iter().map(|p| p.id.to_string()).collect();
    to_action_list_ordered(model, &plan_ids)
}

/// Convert a DomainModel back to an ActionList in a specific order.
///
/// Only includes plans whose ID appears in `plan_order`.
pub fn to_action_list_ordered(model: &DomainModel, plan_order: &[String]) -> ActionList {
    let mut actions = Vec::new();

    // Build a lookup from plan ID string → &Plan
    let plan_map: std::collections::HashMap<String, &Plan> = model
        .all_plans()
        .into_iter()
        .map(|p| (p.id.to_string(), p))
        .collect();

    for plan_id_str in plan_order {
        if let Some(plan) = plan_map.get(plan_id_str) {
            if plan.acts.is_empty() {
                // Placeholder act for plans with no acts
                let dummy_act = PlannedAct {
                    id: Uuid::new_v5(&plan.id, b"act-0"),
                    plan_id: plan.id,
                    phase: ActPhase::NotStarted,
                    scheduled_at: None,
                    due_date: None,
                    duration: None,
                    completed_at: None,
                    created_at: None,
                };
                actions.push(merge_to_action(plan, &dummy_act, plan.id));
            } else {
                for act in &plan.acts {
                    actions.push(merge_to_action(plan, act, plan.id));
                }
            }
        }
    }

    actions
}
