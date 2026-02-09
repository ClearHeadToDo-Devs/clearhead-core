//! Domain model aligned with the Actions Vocabulary v4 ontology.
//!
//! This module provides structs that map to the CCO-aligned ontology:
//! - `Plan` (cco:Plan) - task definition / template (information content)
//! - `PlannedAct` (cco:PlannedAct) - actual execution (occurrence)
//! - `ActPhase` - lifecycle state (custom BFO Quality)
//!
//! The key insight from BFO: information vs occurrence.
//! - A Plan is a *continuant* — persists and can be realized multiple times
//! - A PlannedAct is an *occurrent* — unfolds through time
//!
//! # Example
//!
//! "Do laundry weekly" is one Plan. Each week's laundry is a separate PlannedAct.
//! For non-recurring tasks, there's still one Plan and one PlannedAct.

use autosurgeon::{Hydrate, Reconcile};
use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::entities::{Action, ActionList, ActionState, Recurrence};
use crate::sync_utils::{hydrate_date, reconcile_date};

/// Lifecycle phase of a PlannedAct.
///
/// Maps to `actions:ActPhase` (subclass of bfo:Quality).
/// The phase inheres in the PlannedAct, not the Plan.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, Reconcile, Hydrate,
)]
pub enum ActPhase {
    #[default]
    NotStarted,
    InProgress,
    Completed,
    Blocked,
    Cancelled,
}

impl From<ActionState> for ActPhase {
    fn from(state: ActionState) -> Self {
        match state {
            ActionState::NotStarted => ActPhase::NotStarted,
            ActionState::InProgress => ActPhase::InProgress,
            ActionState::Completed => ActPhase::Completed,
            ActionState::BlockedorAwaiting => ActPhase::Blocked,
            ActionState::Cancelled => ActPhase::Cancelled,
        }
    }
}

/// Task definition / template.
///
/// Maps to `cco:Plan` - information content that persists and can be
/// realized multiple times (for recurring tasks) or once (for one-off tasks).
///
/// Plans hold the "what" - name, description, priority, contexts, recurrence rules.
/// They don't hold execution state (that's on PlannedAct).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct Plan {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub priority: Option<u32>,
    pub contexts: Option<Vec<String>>,
    pub recurrence: Option<Recurrence>,
    /// Parent plan (partOf relationship for hierarchy)
    pub parent: Option<Uuid>,
    /// Story/project reference (hasObjective relationship)
    pub objective: Option<String>,
    /// Stable alias for references
    pub alias: Option<String>,
    /// Whether children execute sequentially
    pub is_sequential: Option<bool>,
    /// Plans this plan depends on (predecessor relationships)
    pub depends_on: Option<Vec<Uuid>>,
    /// Charter reference (resolved at workspace layer)
    pub charter: Option<String>,
}

/// A charter — a directive that organizes plans under a shared purpose.
///
/// Maps to `actions:Charter` (subclass of cco:DirectiveInformationContentEntity).
/// Charters are the highest-level organizational unit. Plans reference charters
/// via the `charter` field.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Charter {
    pub id: Uuid,
    pub title: String,
    pub description: Option<String>,
    pub alias: Option<String>,
    /// Reference string for parent charter, resolved at workspace layer
    pub parent: Option<String>,
    /// References to objectives, resolved at workspace layer
    pub objectives: Option<Vec<String>>,
}

/// Actual execution / occurrence of a Plan.
///
/// Maps to `cco:PlannedAct` - something that unfolds through time.
/// Each realization of a Plan creates a PlannedAct.
///
/// PlannedActs hold the "when" and "status" - scheduled time, completion, phase.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct PlannedAct {
    pub id: Uuid,
    /// The Plan this act realizes (prescribes relationship, inverse)
    pub plan_id: Uuid,
    /// Current lifecycle phase
    pub phase: ActPhase,
    /// Scheduled date/time for this occurrence
    #[autosurgeon(reconcile = "reconcile_date", hydrate = "hydrate_date")]
    pub scheduled_at: Option<DateTime<Local>>,
    /// Duration in minutes
    pub duration: Option<u32>,
    /// When this act was completed
    #[autosurgeon(reconcile = "reconcile_date", hydrate = "hydrate_date")]
    pub completed_at: Option<DateTime<Local>>,
    /// When this act was created/scheduled
    #[autosurgeon(reconcile = "reconcile_date", hydrate = "hydrate_date")]
    pub created_at: Option<DateTime<Local>>,
}

/// A Plan together with its PlannedAct(s).
///
/// For non-recurring plans: one Plan, one PlannedAct.
/// For recurring plans: one Plan, potentially multiple PlannedActs.
#[derive(Debug, Clone, PartialEq)]
pub struct PlanWithActs {
    pub plan: Plan,
    pub acts: Vec<PlannedAct>,
}

/// Domain model containing all Plans and PlannedActs.
///
/// Uses String keys (UUID strings) for CRDT compatibility.
/// autosurgeon requires HashMap keys to implement AsRef<str> + From<String>.
#[derive(Debug, Clone, Default, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct DomainModel {
    pub plans: std::collections::HashMap<String, Plan>,
    pub acts: std::collections::HashMap<String, PlannedAct>,
}

impl DomainModel {
    pub fn new() -> Self {
        Self::default()
    }

    /// Convert an ActionList into the domain model.
    ///
    /// Each Action becomes one Plan and one PlannedAct.
    /// For recurring actions, additional PlannedActs can be expanded later.
    pub fn from_actions(actions: &ActionList) -> Self {
        let mut model = DomainModel::new();

        for action in actions {
            let (plan, act) = split_action(action);
            model.plans.insert(plan.id.to_string(), plan);
            model.acts.insert(act.id.to_string(), act);
        }

        model
    }

    /// Convert the domain model back to an ActionList.
    ///
    /// This reconstructs Actions from Plans and their PlannedActs.
    pub fn to_action_list(&self) -> ActionList {
        // Default to arbitrary order if no specific order is requested
        let plan_ids: Vec<String> = self.plans.keys().cloned().collect();
        self.to_action_list_ordered(&plan_ids)
    }

    /// Convert the domain model back to an ActionList in a specific order.
    pub fn to_action_list_ordered(&self, plan_order: &[String]) -> ActionList {
        let mut actions = Vec::new();

        // Group acts by plan_id (as string)
        let mut acts_by_plan: std::collections::HashMap<String, Vec<&PlannedAct>> =
            std::collections::HashMap::new();
        for act in self.acts.values() {
            acts_by_plan
                .entry(act.plan_id.to_string())
                .or_default()
                .push(act);
        }

        // Iterate over requested plans
        for plan_id_str in plan_order {
            if let Some(plan) = self.plans.get(plan_id_str) {
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

    /// Find a Plan by ID
    pub fn plan(&self, id: Uuid) -> Option<&Plan> {
        self.plans.get(&id.to_string())
    }

    /// Find all PlannedActs for a given Plan
    pub fn acts_for_plan(&self, plan_id: Uuid) -> Vec<&PlannedAct> {
        self.acts
            .values()
            .filter(|a| a.plan_id == plan_id)
            .collect()
    }

    /// Get all incomplete acts
    pub fn incomplete_acts(&self) -> Vec<&PlannedAct> {
        self.acts
            .values()
            .filter(|a| !matches!(a.phase, ActPhase::Completed | ActPhase::Cancelled))
            .collect()
    }
}

/// Merge a Plan and PlannedAct into a single Action.
fn merge_to_action(plan: &Plan, act: &PlannedAct, target_id: Uuid) -> Action {
    use crate::entities::PredecessorRef;

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

/// Split an Action into its Plan and PlannedAct components.
///
/// The Action.id becomes the Plan.id (it identifies the task definition).
/// The PlannedAct gets a new UUID (it's a specific occurrence).
fn split_action(action: &Action) -> (Plan, PlannedAct) {
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

// ============================================================================
// Domain Diff Types
// ============================================================================

/// A change to a single field on a Plan.
#[derive(Debug, Clone, PartialEq)]
pub enum PlanFieldChange {
    Name { old: String, new: String },
    Description { old: Option<String>, new: Option<String> },
    Priority { old: Option<u32>, new: Option<u32> },
    Contexts { old: Option<Vec<String>>, new: Option<Vec<String>> },
    Parent { old: Option<Uuid>, new: Option<Uuid> },
    Objective { old: Option<String>, new: Option<String> },
    Alias { old: Option<String>, new: Option<String> },
    IsSequential { old: Option<bool>, new: Option<bool> },
    Recurrence { old: Option<Recurrence>, new: Option<Recurrence> },
    DependsOn { old: Option<Vec<Uuid>>, new: Option<Vec<Uuid>> },
    Charter { old: Option<String>, new: Option<String> },
}

/// A change to a single field on a PlannedAct.
#[derive(Debug, Clone, PartialEq)]
pub enum ActFieldChange {
    Phase { old: ActPhase, new: ActPhase },
    ScheduledAt { old: Option<DateTime<Local>>, new: Option<DateTime<Local>> },
    Duration { old: Option<u32>, new: Option<u32> },
    CompletedAt { old: Option<DateTime<Local>>, new: Option<DateTime<Local>> },
    CreatedAt { old: Option<DateTime<Local>>, new: Option<DateTime<Local>> },
}

/// Changes detected for a single Plan.
#[derive(Debug, Clone, PartialEq)]
pub struct PlanDiff {
    pub id: Uuid,
    pub changes: Vec<PlanFieldChange>,
}

/// Changes detected for a single PlannedAct.
#[derive(Debug, Clone, PartialEq)]
pub struct ActDiff {
    pub id: Uuid,
    pub plan_id: Uuid,
    pub changes: Vec<ActFieldChange>,
}

/// Complete diff between two DomainModels.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct DomainDiff {
    pub plans_added: Vec<Plan>,
    pub plans_removed: Vec<Plan>,
    pub plans_modified: Vec<PlanDiff>,
    pub acts_added: Vec<PlannedAct>,
    pub acts_removed: Vec<PlannedAct>,
    pub acts_modified: Vec<ActDiff>,
}

impl DomainDiff {
    pub fn is_empty(&self) -> bool {
        self.plans_added.is_empty()
            && self.plans_removed.is_empty()
            && self.plans_modified.is_empty()
            && self.acts_added.is_empty()
            && self.acts_removed.is_empty()
            && self.acts_modified.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::Action;

    #[test]
    fn test_split_simple_action() {
        let action = Action {
            id: Uuid::new_v4(),
            name: "Buy milk".to_string(),
            state: ActionState::NotStarted,
            ..Default::default()
        };

        let (plan, act) = split_action(&action);

        assert_eq!(plan.id, action.id);
        assert_eq!(plan.name, "Buy milk");
        assert_eq!(act.plan_id, plan.id);
        assert_eq!(act.phase, ActPhase::NotStarted);
    }

    #[test]
    fn test_split_completed_action() {
        let action = Action {
            id: Uuid::new_v4(),
            name: "Done task".to_string(),
            state: ActionState::Completed,
            completed_date_time: Some(Local::now()),
            ..Default::default()
        };

        let (_, act) = split_action(&action);

        assert_eq!(act.phase, ActPhase::Completed);
        assert!(act.completed_at.is_some());
    }

    #[test]
    fn test_domain_model_from_actions() {
        let actions = vec![Action::new("Task 1"), Action::new("Task 2")];

        let model = DomainModel::from_actions(&actions);

        assert_eq!(model.plans.len(), 2);
        assert_eq!(model.acts.len(), 2);
    }

    #[test]
    fn test_acts_for_plan() {
        let action = Action::new("Test task");
        let model = DomainModel::from_actions(&vec![action.clone()]);

        let acts = model.acts_for_plan(action.id);
        assert_eq!(acts.len(), 1);
    }

    #[test]
    fn test_deterministic_act_ids() {
        let actions = vec![Action::new("Task 1"), Action::new("Task 2")];

        let model1 = DomainModel::from_actions(&actions);
        let model2 = DomainModel::from_actions(&actions);

        // Same ActionList should produce same act IDs every time
        let mut ids1: Vec<String> = model1.acts.keys().cloned().collect();
        let mut ids2: Vec<String> = model2.acts.keys().cloned().collect();
        ids1.sort();
        ids2.sort();
        assert_eq!(ids1, ids2);
    }

    #[test]
    fn test_act_id_derived_from_plan_id() {
        let action = Action::new("Test");
        let (plan, act) = split_action(&action);

        // Act ID should be deterministic v5 UUID based on plan ID
        let expected_act_id = Uuid::new_v5(&plan.id, b"act-0");
        assert_eq!(act.id, expected_act_id);
    }
}
