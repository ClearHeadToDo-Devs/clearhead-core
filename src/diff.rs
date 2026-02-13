use crate::domain::{
    ActDiff, ActFieldChange, DomainDiff, DomainModel, Plan, PlanDiff, PlanFieldChange, PlannedAct,
};
use crate::actions::{Action, ActionList, ActionState};
use serde_json::{json, Value};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, PartialEq, Clone)]
pub struct ActionDiff {
    pub id: Uuid,
    pub changes: Vec<FieldChange>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldChange {
    State {
        old: ActionState,
        new: ActionState,
    },
    Name {
        old: String,
        new: String,
    },
    Description {
        old: Option<String>,
        new: Option<String>,
    },
    Priority {
        old: Option<u32>,
        new: Option<u32>,
    },
    DoDate {
        old: Option<String>,
        new: Option<String>,
    },
    CompletedDate {
        old: Option<String>,
        new: Option<String>,
    },
    // We can add more fields as needed
    Generic {
        field: String,
        old: Value,
        new: Value,
    },
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Diff {
    pub added: Vec<Action>,
    pub removed: Vec<Action>,
    pub modified: Vec<ActionDiff>,
}

impl Diff {
    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty() && self.modified.is_empty()
    }
}

pub fn diff_actions(old_actions: &ActionList, new_actions: &ActionList) -> Diff {
    let mut diff = Diff::default();
    let old_map: HashMap<Uuid, &Action> = old_actions.iter().map(|a| (a.id, a)).collect();
    let new_map: HashMap<Uuid, &Action> = new_actions.iter().map(|a| (a.id, a)).collect();

    // Check for added and modified
    for new_action in new_actions {
        if let Some(old_action) = old_map.get(&new_action.id) {
            let changes = compare_action(old_action, new_action);
            if !changes.is_empty() {
                diff.modified.push(ActionDiff {
                    id: new_action.id,
                    changes,
                });
            }
        } else {
            diff.added.push(new_action.clone());
        }
    }

    // Check for removed
    for old_action in old_actions {
        if !new_map.contains_key(&old_action.id) {
            diff.removed.push(old_action.clone());
        }
    }

    diff
}

fn compare_action(old: &Action, new: &Action) -> Vec<FieldChange> {
    let mut changes = Vec::new();

    if old.state != new.state {
        changes.push(FieldChange::State {
            old: old.state,
            new: new.state,
        });
    }

    if old.name != new.name {
        changes.push(FieldChange::Name {
            old: old.name.clone(),
            new: new.name.clone(),
        });
    }

    if old.description != new.description {
        changes.push(FieldChange::Description {
            old: old.description.clone(),
            new: new.description.clone(),
        });
    }

    if old.priority != new.priority {
        changes.push(FieldChange::Priority {
            old: old.priority,
            new: new.priority,
        });
    }

    if old.do_date_time != new.do_date_time {
        changes.push(FieldChange::DoDate {
            old: old.do_date_time.map(|d| d.to_rfc3339()),
            new: new.do_date_time.map(|d| d.to_rfc3339()),
        });
    }

    if old.completed_date_time != new.completed_date_time {
        changes.push(FieldChange::CompletedDate {
            old: old.completed_date_time.map(|d| d.to_rfc3339()),
            new: new.completed_date_time.map(|d| d.to_rfc3339()),
        });
    }

    // Compare contexts (as sets for robust comparison, but list equality for now)
    if old.context_list != new.context_list {
        changes.push(FieldChange::Generic {
            field: "contexts".to_string(),
            old: json!(old.context_list),
            new: json!(new.context_list),
        });
    }

    if old.story != new.story {
        changes.push(FieldChange::Generic {
            field: "story".to_string(),
            old: json!(old.story),
            new: json!(new.story),
        });
    }

    changes
}

// ============================================================================
// Domain Model Diffing
// ============================================================================

/// Diff two DomainModels, producing a DomainDiff.
///
/// Plans are matched by `plan.id`. Acts are matched by `act.id`.
/// Uses the `all_plans()` / `all_acts()` helpers to flatten the hierarchy.
pub fn diff_domain_models(old: &DomainModel, new: &DomainModel) -> DomainDiff {
    let mut diff = DomainDiff::default();

    // Build lookup maps from flattened views
    let old_plans: HashMap<Uuid, &Plan> = old.all_plans().into_iter().map(|p| (p.id, p)).collect();
    let new_plans: HashMap<Uuid, &Plan> = new.all_plans().into_iter().map(|p| (p.id, p)).collect();
    let old_acts: HashMap<Uuid, &PlannedAct> =
        old.all_acts().into_iter().map(|a| (a.id, a)).collect();
    let new_acts: HashMap<Uuid, &PlannedAct> =
        new.all_acts().into_iter().map(|a| (a.id, a)).collect();

    // Plans: added, removed, modified
    for (id, new_plan) in &new_plans {
        if let Some(old_plan) = old_plans.get(id) {
            let changes = compare_plans(old_plan, new_plan);
            if !changes.is_empty() {
                diff.plans_modified.push(PlanDiff {
                    id: new_plan.id,
                    changes,
                });
            }
        } else {
            diff.plans_added.push((*new_plan).clone());
        }
    }
    for (id, old_plan) in &old_plans {
        if !new_plans.contains_key(id) {
            diff.plans_removed.push((*old_plan).clone());
        }
    }

    // Acts: added, removed, modified
    for (id, new_act) in &new_acts {
        if let Some(old_act) = old_acts.get(id) {
            let changes = compare_acts(old_act, new_act);
            if !changes.is_empty() {
                diff.acts_modified.push(ActDiff {
                    id: new_act.id,
                    plan_id: new_act.plan_id,
                    changes,
                });
            }
        } else {
            diff.acts_added.push((*new_act).clone());
        }
    }
    for (id, old_act) in &old_acts {
        if !new_acts.contains_key(id) {
            diff.acts_removed.push((*old_act).clone());
        }
    }

    diff
}

fn compare_plans(old: &Plan, new: &Plan) -> Vec<PlanFieldChange> {
    let mut changes = Vec::new();

    if old.name != new.name {
        changes.push(PlanFieldChange::Name {
            old: old.name.clone(),
            new: new.name.clone(),
        });
    }
    if old.description != new.description {
        changes.push(PlanFieldChange::Description {
            old: old.description.clone(),
            new: new.description.clone(),
        });
    }
    if old.priority != new.priority {
        changes.push(PlanFieldChange::Priority {
            old: old.priority,
            new: new.priority,
        });
    }
    if old.contexts != new.contexts {
        changes.push(PlanFieldChange::Contexts {
            old: old.contexts.clone(),
            new: new.contexts.clone(),
        });
    }
    if old.parent != new.parent {
        changes.push(PlanFieldChange::Parent {
            old: old.parent,
            new: new.parent,
        });
    }
    if old.objective != new.objective {
        changes.push(PlanFieldChange::Objective {
            old: old.objective.clone(),
            new: new.objective.clone(),
        });
    }
    if old.alias != new.alias {
        changes.push(PlanFieldChange::Alias {
            old: old.alias.clone(),
            new: new.alias.clone(),
        });
    }
    if old.is_sequential != new.is_sequential {
        changes.push(PlanFieldChange::IsSequential {
            old: old.is_sequential,
            new: new.is_sequential,
        });
    }
    if old.recurrence != new.recurrence {
        changes.push(PlanFieldChange::Recurrence {
            old: old.recurrence.clone(),
            new: new.recurrence.clone(),
        });
    }
    if old.depends_on != new.depends_on {
        changes.push(PlanFieldChange::DependsOn {
            old: old.depends_on.clone(),
            new: new.depends_on.clone(),
        });
    }

    changes
}

/// Compare two optional DateTimes at minute precision.
///
/// The .actions text format uses `%Y-%m-%dT%H:%M` (minute precision), so
/// timestamps that differ only in seconds or sub-seconds are semantically
/// identical after a format/parse round-trip.
fn dates_equal(
    a: &Option<chrono::DateTime<chrono::Local>>,
    b: &Option<chrono::DateTime<chrono::Local>>,
) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => {
            // Truncate to minute precision for comparison
            (a.timestamp() / 60) == (b.timestamp() / 60)
        }
        _ => false,
    }
}

fn compare_acts(old: &PlannedAct, new: &PlannedAct) -> Vec<ActFieldChange> {
    let mut changes = Vec::new();

    if old.phase != new.phase {
        changes.push(ActFieldChange::Phase {
            old: old.phase,
            new: new.phase,
        });
    }
    if !dates_equal(&old.scheduled_at, &new.scheduled_at) {
        changes.push(ActFieldChange::ScheduledAt {
            old: old.scheduled_at,
            new: new.scheduled_at,
        });
    }
    if old.duration != new.duration {
        changes.push(ActFieldChange::Duration {
            old: old.duration,
            new: new.duration,
        });
    }
    if !dates_equal(&old.completed_at, &new.completed_at) {
        changes.push(ActFieldChange::CompletedAt {
            old: old.completed_at,
            new: new.completed_at,
        });
    }
    if !dates_equal(&old.created_at, &new.created_at) {
        changes.push(ActFieldChange::CreatedAt {
            old: old.created_at,
            new: new.created_at,
        });
    }

    changes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::ActPhase;
    use crate::actions::ActionState;

    fn make_action(id: &str, name: &str) -> Action {
        Action {
            id: Uuid::parse_str(id).unwrap(),
            parent_id: None,
            state: ActionState::NotStarted,
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
    fn test_diff_added() {
        let old = vec![];
        let new = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "New Action",
        )];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.added.len(), 1);
        assert_eq!(diff.removed.len(), 0);
        assert_eq!(diff.modified.len(), 0);
        assert_eq!(diff.added[0].name, "New Action");
    }

    #[test]
    fn test_diff_removed() {
        let old = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "Old Action",
        )];
        let new = vec![];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.added.len(), 0);
        assert_eq!(diff.removed.len(), 1);
        assert_eq!(diff.modified.len(), 0);
        assert_eq!(diff.removed[0].name, "Old Action");
    }

    #[test]
    fn test_diff_modified_state() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let mut old_action = make_action(id, "Task");
        old_action.state = ActionState::NotStarted;

        let mut new_action = make_action(id, "Task");
        new_action.state = ActionState::Completed;

        let old = vec![old_action];
        let new = vec![new_action];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.modified.len(), 1);
        let modification = &diff.modified[0];
        assert_eq!(modification.id.to_string(), id);
        assert_eq!(modification.changes.len(), 1);

        match &modification.changes[0] {
            FieldChange::State { old, new } => {
                assert_eq!(*old, ActionState::NotStarted);
                assert_eq!(*new, ActionState::Completed);
            }
            _ => panic!("Expected state change"),
        }
    }

    #[test]
    fn test_diff_modified_multiple_fields() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let mut old_action = make_action(id, "Task");
        old_action.priority = Some(1);

        let mut new_action = make_action(id, "Updated Task"); // Name change
        new_action.priority = Some(2); // Priority change

        let diff = diff_actions(&vec![old_action], &vec![new_action]);

        assert_eq!(diff.modified.len(), 1);
        let changes = &diff.modified[0].changes;
        assert_eq!(changes.len(), 2);

        // Sort changes isn't guaranteed, so check existence
        let has_name_change = changes
            .iter()
            .any(|c| matches!(c, FieldChange::Name { .. }));
        let has_prio_change = changes
            .iter()
            .any(|c| matches!(c, FieldChange::Priority { .. }));

        assert!(has_name_change);
        assert!(has_prio_change);
    }

    // ====================================================================
    // Domain diff tests
    // ====================================================================

    #[test]
    fn test_domain_diff_plan_added() {
        let old = DomainModel::new();
        let actions = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "New Plan",
        )];
        let new = DomainModel::from_actions(&actions);

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.plans_added.len(), 1);
        assert_eq!(diff.plans_added[0].name, "New Plan");
        assert_eq!(diff.acts_added.len(), 1);
        assert!(diff.plans_removed.is_empty());
    }

    #[test]
    fn test_domain_diff_plan_removed() {
        let actions = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "Old Plan",
        )];
        let old = DomainModel::from_actions(&actions);
        let new = DomainModel::new();

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.plans_removed.len(), 1);
        assert_eq!(diff.acts_removed.len(), 1);
        assert!(diff.plans_added.is_empty());
    }

    #[test]
    fn test_domain_diff_plan_name_changed() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let old = DomainModel::from_actions(&vec![make_action(id, "Old Name")]);

        let mut new = old.clone();
        let plan = new.charters[0].plans.first_mut().unwrap();
        plan.name = "New Name".to_string();

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.plans_modified.len(), 1);
        assert!(diff.plans_modified[0]
            .changes
            .iter()
            .any(|c| matches!(c, PlanFieldChange::Name { .. })));
    }

    #[test]
    fn test_domain_diff_act_phase_changed() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let old = DomainModel::from_actions(&vec![make_action(id, "Task")]);

        let mut new = old.clone();
        let act = new.charters[0].plans[0].acts.first_mut().unwrap();
        act.phase = ActPhase::Completed;

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.acts_modified.len(), 1);
        assert!(diff.acts_modified[0]
            .changes
            .iter()
            .any(|c| matches!(c, ActFieldChange::Phase { .. })));
    }

    #[test]
    fn test_domain_diff_no_changes() {
        let actions = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "Task",
        )];
        let model = DomainModel::from_actions(&actions);

        let diff = diff_domain_models(&model, &model);
        assert!(diff.is_empty());
    }
}
