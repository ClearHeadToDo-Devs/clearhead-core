// ============================================================================
// Domain Model Diffing
// ============================================================================

use crate::ActDiff;
use crate::ActFieldChange;
use crate::DomainDiff;
use crate::PlanDiff;
use crate::PlanFieldChange;
use crate::domain::{DomainModel, Plan, PlannedAct};
use std::collections::HashMap;
use uuid::Uuid;
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

#[cfg(test)]
mod tests {

    use super::*;
    use crate::domain::{ActPhase, Charter, PlannedAct};
    // ====================================================================
    // Domain diff tests
    // ====================================================================

    /// Build a full DomainModel from a single plan name + id.
    ///
    /// Creates the complete hierarchy: Charter → Plan → PlannedAct,
    /// matching the domain invariant that every plan has at least one act.
    fn make_model(id: &str, name: &str) -> DomainModel {
        let plan_id = Uuid::parse_str(id).unwrap();
        let act_id = Uuid::new_v5(&plan_id, b"act-0");

        DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: Uuid::new_v5(&plan_id, b"charter"),
                title: "Test Charter".to_string(),
                description: None,
                alias: None,
                parent: None,
                objectives: None,
                plans: vec![Plan {
                    id: plan_id,
                    name: name.to_string(),
                    description: None,
                    priority: None,
                    contexts: None,
                    parent: None,
                    objective: None,
                    alias: None,
                    is_sequential: None,
                    recurrence: None,
                    depends_on: None,
                    duration: None,
                    acts: vec![PlannedAct {
                        id: act_id,
                        plan_id,
                        phase: ActPhase::NotStarted,
                        scheduled_at: None,
                        duration: None,
                        completed_at: None,
                        created_at: None,
                    }],
                }],
            }],
        }
    }

    #[test]
    fn test_domain_diff_plan_added() {
        let old = DomainModel::new();
        let new = make_model("019baaec-00b6-7991-be34-94b68212619a", "New Plan");

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.plans_added.len(), 1);
        assert_eq!(diff.plans_added[0].name, "New Plan");
        assert_eq!(diff.acts_added.len(), 1);
        assert!(diff.plans_removed.is_empty());
    }

    #[test]
    fn test_domain_diff_plan_removed() {
        let old = make_model("019baaec-00b6-7991-be34-94b68212619a", "Old Plan");
        let new = DomainModel::new();

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.plans_removed.len(), 1);
        assert_eq!(diff.acts_removed.len(), 1);
        assert!(diff.plans_added.is_empty());
    }

    #[test]
    fn test_domain_diff_plan_name_changed() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let old = make_model(id, "Old Name");

        let mut new = old.clone();
        new.charters[0].plans[0].name = "New Name".to_string();

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.plans_modified.len(), 1);
        assert!(
            diff.plans_modified[0]
                .changes
                .iter()
                .any(|c| matches!(c, PlanFieldChange::Name { .. }))
        );
    }

    #[test]
    fn test_domain_diff_act_phase_changed() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let old = make_model(id, "Task");

        let mut new = old.clone();
        new.charters[0].plans[0].acts[0].phase = ActPhase::Completed;

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.acts_modified.len(), 1);
        assert!(
            diff.acts_modified[0]
                .changes
                .iter()
                .any(|c| matches!(c, ActFieldChange::Phase { .. }))
        );
    }

    #[test]
    fn test_domain_diff_no_changes() {
        let model = make_model("019baaec-00b6-7991-be34-94b68212619a", "Task");

        let diff = diff_domain_models(&model, &model);
        assert!(diff.is_empty());
    }
}
