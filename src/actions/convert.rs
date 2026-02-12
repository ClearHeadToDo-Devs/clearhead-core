use crate::actions::{Action, ActionList, ActionState, PredecessorRef};
use crate::domain::{ActPhase, DomainModel, Plan, PlannedAct};
use uuid::Uuid;

/// Split an Action into its Plan and PlannedAct components.
///
/// The Action.id becomes the Plan.id (it identifies the task definition).
/// The PlannedAct gets a new UUID (it's a specific occurrence).
pub fn split_action(action: &Action) -> (Plan, PlannedAct) {
    let plan_id = action.id;
    let act_id = Uuid::new_v5(&plan_id, b"act-0");

    let plan = Plan {
        id: plan_id,
        name: action.name.clone(),
        description: action.description.clone(),
        priority: action.priority,
        contexts: action.context_list.clone(),
        recurrence: action.recurrence.clone(),
        parent: action.parent_id,
        objective: action.story.clone(),
        alias: action.alias.clone(),
        is_sequential: action.is_sequential,
        duration: action.do_duration,
        depends_on: action
            .predecessors
            .as_ref()
            .map(|preds| preds.iter().filter_map(|p| p.resolved_uuid).collect()),
        charter: None, // charter doesn't round-trip through Action
    };

    let act = PlannedAct {
        id: act_id,
        plan_id,
        phase: action.state.into(),
        scheduled_at: action.do_date_time,
        duration: action.do_duration,
        completed_at: action.completed_date_time,
        created_at: action.created_date_time,
    };

    (plan, act)
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
        do_duration: act.duration.or(plan.duration),
        recurrence: plan.recurrence.clone(),
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
        story: plan.objective.clone(),
        alias: plan.alias.clone(),
        is_sequential: plan.is_sequential,
    }
}

/// Convert an ActionList into a DomainModel.
pub fn from_actions(actions: &ActionList) -> DomainModel {
    let mut model = DomainModel::new();

    for action in actions {
        let (plan, act) = split_action(action);
        model.plans.insert(plan.id.to_string(), plan);
        model.acts.insert(act.id.to_string(), act);
    }

    model
}

/// Convert a DomainModel back to an ActionList.
pub fn to_action_list(model: &DomainModel) -> ActionList {
    let plan_ids: Vec<String> = model.plans.keys().cloned().collect();
    to_action_list_ordered(model, &plan_ids)
}

/// Convert a DomainModel back to an ActionList in a specific order.
pub fn to_action_list_ordered(model: &DomainModel, plan_order: &[String]) -> ActionList {
    let mut actions = Vec::new();

    // Group acts by plan_id (as string)
    let mut acts_by_plan: std::collections::HashMap<String, Vec<&PlannedAct>> =
        std::collections::HashMap::new();
    for act in model.acts.values() {
        acts_by_plan
            .entry(act.plan_id.to_string())
            .or_default()
            .push(act);
    }

    // Iterate over requested plans
    for plan_id_str in plan_order {
        if let Some(plan) = model.plans.get(plan_id_str) {
            if let Some(acts) = acts_by_plan.get(&plan.id.to_string()) {
                for act in acts {
                    actions.push(merge_to_action(plan, act, plan.id));
                }
            } else {
                // Placeholder act
                let dummy_act = PlannedAct {
                    id: Uuid::new_v5(&plan.id, b"act-0"),
                    plan_id: plan.id,
                    phase: ActPhase::NotStarted,
                    scheduled_at: None,
                    duration: None,
                    completed_at: None,
                    created_at: None,
                };
                actions.push(merge_to_action(plan, &dummy_act, plan.id));
            }
        }
    }

    actions
}
