//! Hierarchical diff between two DomainModels.
//!
//! The diff mirrors the domain hierarchy: charter-level changes contain
//! plan-level changes, which contain act-level changes. This preserves
//! charter context throughout — no post-hoc lookups needed.

use super::{ActPhase, Charter, DomainModel, Plan, PlannedAct, Recurrence};
use chrono::{DateTime, Local};
use std::collections::HashMap;
use uuid::Uuid;

// ============================================================================
// Field-level change types
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

/// A change to a single field on a Charter.
#[derive(Debug, Clone, PartialEq)]
pub enum CharterFieldChange {
    Title { old: String, new: String },
    Description { old: Option<String>, new: Option<String> },
    Alias { old: Option<String>, new: Option<String> },
    Parent { old: Option<String>, new: Option<String> },
}

// ============================================================================
// Hierarchical diff structs
// ============================================================================

/// Changes detected for a single PlannedAct.
#[derive(Debug, Clone, PartialEq)]
pub struct ActDiff {
    pub id: Uuid,
    pub plan_id: Uuid,
    pub changes: Vec<ActFieldChange>,
}

/// Changes detected for a single Plan and its acts.
#[derive(Debug, Clone, PartialEq)]
pub struct PlanDiff {
    pub id: Uuid,
    pub changes: Vec<PlanFieldChange>,
    pub acts_added: Vec<PlannedAct>,
    pub acts_removed: Vec<PlannedAct>,
    pub acts_modified: Vec<ActDiff>,
}

impl PlanDiff {
    pub fn is_empty(&self) -> bool {
        self.changes.is_empty()
            && self.acts_added.is_empty()
            && self.acts_removed.is_empty()
            && self.acts_modified.is_empty()
    }
}

/// Changes detected for a single Charter and its plans.
#[derive(Debug, Clone, PartialEq)]
pub struct CharterDiff {
    pub id: Uuid,
    pub field_changes: Vec<CharterFieldChange>,
    pub plans_added: Vec<Plan>,
    pub plans_removed: Vec<Plan>,
    pub plans_modified: Vec<PlanDiff>,
}

impl CharterDiff {
    pub fn is_empty(&self) -> bool {
        self.field_changes.is_empty()
            && self.plans_added.is_empty()
            && self.plans_removed.is_empty()
            && self.plans_modified.is_empty()
    }
}

/// Complete hierarchical diff between two DomainModels.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct DomainDiff {
    pub charters_added: Vec<Charter>,
    pub charters_removed: Vec<Charter>,
    pub charters_modified: Vec<CharterDiff>,
}

impl DomainDiff {
    pub fn is_empty(&self) -> bool {
        self.charters_added.is_empty()
            && self.charters_removed.is_empty()
            && self.charters_modified.is_empty()
    }

    /// IDs of charters that have any change (added, removed, or modified).
    pub fn affected_charter_ids(&self) -> Vec<Uuid> {
        let mut ids: Vec<Uuid> = self
            .charters_added.iter().map(|c| c.id)
            .chain(self.charters_removed.iter().map(|c| c.id))
            .chain(self.charters_modified.iter().map(|cd| cd.id))
            .collect();
        ids.sort();
        ids.dedup();
        ids
    }
}

// ============================================================================
// Diff engine
// ============================================================================

/// Diff two DomainModels, producing a hierarchical DomainDiff.
///
/// Charters are matched by `charter.id`. Plans within a charter are matched
/// by `plan.id`. Acts within a plan are matched by `act.id`.
pub fn diff_domain_models(old: &DomainModel, new: &DomainModel) -> DomainDiff {
    let old_charters: HashMap<Uuid, &Charter> =
        old.charters.iter().map(|c| (c.id, c)).collect();
    let new_charters: HashMap<Uuid, &Charter> =
        new.charters.iter().map(|c| (c.id, c)).collect();

    let mut diff = DomainDiff::default();

    for (id, new_charter) in &new_charters {
        if let Some(old_charter) = old_charters.get(id) {
            let charter_diff = diff_charters(old_charter, new_charter);
            if !charter_diff.is_empty() {
                diff.charters_modified.push(charter_diff);
            }
        } else {
            diff.charters_added.push((*new_charter).clone());
        }
    }

    for (id, old_charter) in &old_charters {
        if !new_charters.contains_key(id) {
            diff.charters_removed.push((*old_charter).clone());
        }
    }

    diff
}

fn diff_charters(old: &Charter, new: &Charter) -> CharterDiff {
    let field_changes = compare_charters(old, new);

    let old_plans: HashMap<Uuid, &Plan> = old.plans.iter().map(|p| (p.id, p)).collect();
    let new_plans: HashMap<Uuid, &Plan> = new.plans.iter().map(|p| (p.id, p)).collect();

    let mut plans_added = Vec::new();
    let mut plans_removed = Vec::new();
    let mut plans_modified = Vec::new();

    for (id, new_plan) in &new_plans {
        if let Some(old_plan) = old_plans.get(id) {
            let plan_diff = diff_plans(old_plan, new_plan);
            if !plan_diff.is_empty() {
                plans_modified.push(plan_diff);
            }
        } else {
            plans_added.push((*new_plan).clone());
        }
    }

    for (id, old_plan) in &old_plans {
        if !new_plans.contains_key(id) {
            plans_removed.push((*old_plan).clone());
        }
    }

    CharterDiff {
        id: new.id,
        field_changes,
        plans_added,
        plans_removed,
        plans_modified,
    }
}

fn diff_plans(old: &Plan, new: &Plan) -> PlanDiff {
    let changes = compare_plans(old, new);

    let old_acts: HashMap<Uuid, &PlannedAct> = old.acts.iter().map(|a| (a.id, a)).collect();
    let new_acts: HashMap<Uuid, &PlannedAct> = new.acts.iter().map(|a| (a.id, a)).collect();

    let mut acts_added = Vec::new();
    let mut acts_removed = Vec::new();
    let mut acts_modified = Vec::new();

    for (id, new_act) in &new_acts {
        if let Some(old_act) = old_acts.get(id) {
            let act_changes = compare_acts(old_act, new_act);
            if !act_changes.is_empty() {
                acts_modified.push(ActDiff {
                    id: new_act.id,
                    plan_id: new_act.plan_id,
                    changes: act_changes,
                });
            }
        } else {
            acts_added.push((*new_act).clone());
        }
    }

    for (id, old_act) in &old_acts {
        if !new_acts.contains_key(id) {
            acts_removed.push((*old_act).clone());
        }
    }

    PlanDiff {
        id: new.id,
        changes,
        acts_added,
        acts_removed,
        acts_modified,
    }
}

fn compare_charters(old: &Charter, new: &Charter) -> Vec<CharterFieldChange> {
    let mut changes = Vec::new();
    if old.title != new.title {
        changes.push(CharterFieldChange::Title { old: old.title.clone(), new: new.title.clone() });
    }
    if old.description != new.description {
        changes.push(CharterFieldChange::Description { old: old.description.clone(), new: new.description.clone() });
    }
    if old.alias != new.alias {
        changes.push(CharterFieldChange::Alias { old: old.alias.clone(), new: new.alias.clone() });
    }
    if old.parent != new.parent {
        changes.push(CharterFieldChange::Parent { old: old.parent.clone(), new: new.parent.clone() });
    }
    changes
}

fn compare_plans(old: &Plan, new: &Plan) -> Vec<PlanFieldChange> {
    let mut changes = Vec::new();
    if old.name != new.name {
        changes.push(PlanFieldChange::Name { old: old.name.clone(), new: new.name.clone() });
    }
    if old.description != new.description {
        changes.push(PlanFieldChange::Description { old: old.description.clone(), new: new.description.clone() });
    }
    if old.priority != new.priority {
        changes.push(PlanFieldChange::Priority { old: old.priority, new: new.priority });
    }
    if old.contexts != new.contexts {
        changes.push(PlanFieldChange::Contexts { old: old.contexts.clone(), new: new.contexts.clone() });
    }
    if old.parent != new.parent {
        changes.push(PlanFieldChange::Parent { old: old.parent, new: new.parent });
    }
    if old.objective != new.objective {
        changes.push(PlanFieldChange::Objective { old: old.objective.clone(), new: new.objective.clone() });
    }
    if old.alias != new.alias {
        changes.push(PlanFieldChange::Alias { old: old.alias.clone(), new: new.alias.clone() });
    }
    if old.is_sequential != new.is_sequential {
        changes.push(PlanFieldChange::IsSequential { old: old.is_sequential, new: new.is_sequential });
    }
    if old.recurrence != new.recurrence {
        changes.push(PlanFieldChange::Recurrence { old: old.recurrence.clone(), new: new.recurrence.clone() });
    }
    if old.depends_on != new.depends_on {
        changes.push(PlanFieldChange::DependsOn { old: old.depends_on.clone(), new: new.depends_on.clone() });
    }
    changes
}

fn compare_acts(old: &PlannedAct, new: &PlannedAct) -> Vec<ActFieldChange> {
    let mut changes = Vec::new();
    if old.phase != new.phase {
        changes.push(ActFieldChange::Phase { old: old.phase, new: new.phase });
    }
    if !dates_equal(&old.scheduled_at, &new.scheduled_at) {
        changes.push(ActFieldChange::ScheduledAt { old: old.scheduled_at, new: new.scheduled_at });
    }
    if old.duration != new.duration {
        changes.push(ActFieldChange::Duration { old: old.duration, new: new.duration });
    }
    if !dates_equal(&old.completed_at, &new.completed_at) {
        changes.push(ActFieldChange::CompletedAt { old: old.completed_at, new: new.completed_at });
    }
    if !dates_equal(&old.created_at, &new.created_at) {
        changes.push(ActFieldChange::CreatedAt { old: old.created_at, new: new.created_at });
    }
    changes
}

/// Compare two optional DateTimes at minute precision.
///
/// The .actions text format uses `%Y-%m-%dT%H:%M` (minute precision), so
/// timestamps that differ only in seconds or sub-seconds are semantically
/// identical after a format/parse round-trip.
fn dates_equal(
    a: &Option<DateTime<Local>>,
    b: &Option<DateTime<Local>>,
) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => (a.timestamp() / 60) == (b.timestamp() / 60),
        _ => false,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{ActPhase, Charter, PlannedAct};

    fn make_model(id: &str, name: &str) -> DomainModel {
        let plan_id = Uuid::parse_str(id).unwrap();
        let act_id = Uuid::new_v5(&plan_id, b"act-0");
        let charter_id = Uuid::new_v5(&plan_id, b"charter");

        DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: charter_id,
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
    fn test_no_changes() {
        let model = make_model("019baaec-00b6-7991-be34-94b68212619a", "Task");
        assert!(diff_domain_models(&model, &model).is_empty());
    }

    #[test]
    fn test_plan_added() {
        let old = DomainModel::new();
        let new = make_model("019baaec-00b6-7991-be34-94b68212619a", "New Plan");

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.charters_added.len(), 1);
        assert_eq!(diff.charters_added[0].plans.len(), 1);
        assert_eq!(diff.charters_added[0].plans[0].name, "New Plan");
        assert!(diff.charters_modified.is_empty());
    }

    #[test]
    fn test_plan_removed() {
        let old = make_model("019baaec-00b6-7991-be34-94b68212619a", "Old Plan");
        let new = DomainModel::new();

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.charters_removed.len(), 1);
        assert!(diff.charters_modified.is_empty());
    }

    #[test]
    fn test_plan_name_changed() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let old = make_model(id, "Old Name");
        let mut new = old.clone();
        new.charters[0].plans[0].name = "New Name".to_string();

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.charters_modified.len(), 1);
        let charter_diff = &diff.charters_modified[0];
        assert_eq!(charter_diff.plans_modified.len(), 1);
        assert!(
            charter_diff.plans_modified[0]
                .changes
                .iter()
                .any(|c| matches!(c, PlanFieldChange::Name { .. }))
        );
    }

    #[test]
    fn test_act_phase_changed() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let old = make_model(id, "Task");
        let mut new = old.clone();
        new.charters[0].plans[0].acts[0].phase = ActPhase::Completed;

        let diff = diff_domain_models(&old, &new);

        assert_eq!(diff.charters_modified.len(), 1);
        let charter_diff = &diff.charters_modified[0];
        assert_eq!(charter_diff.plans_modified.len(), 1);
        assert_eq!(charter_diff.plans_modified[0].acts_modified.len(), 1);
        assert!(
            charter_diff.plans_modified[0].acts_modified[0]
                .changes
                .iter()
                .any(|c| matches!(c, ActFieldChange::Phase { .. }))
        );
    }

    #[test]
    fn test_affected_charter_ids() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let old = make_model(id, "Task");
        let mut new = old.clone();
        new.charters[0].plans[0].name = "Updated".to_string();

        let diff = diff_domain_models(&old, &new);
        let affected = diff.affected_charter_ids();

        assert_eq!(affected.len(), 1);
        assert_eq!(affected[0], old.charters[0].id);
    }
}
