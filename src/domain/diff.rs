//! Hierarchical diff between two DomainModels.
//!
//! The diff mirrors the domain hierarchy: charter-level changes contain
//! plan-level changes, which contain act-level changes. This preserves
//! charter context throughout — no post-hoc lookups needed.

use super::{ActionState, Action, Charter, DomainModel, Plan, Recurrence};
use chrono::{DateTime, Local};
use std::collections::HashMap;
use uuid::Uuid;

// ============================================================================
// Field-level change types
// ============================================================================

/// A change to a single field on a Plan.
#[derive(Debug, Clone, PartialEq)]
pub enum PlanFieldChange {
    Name {
        old: String,
        new: String,
    },
    Description {
        old: Option<String>,
        new: Option<String>,
    },
    Recurrence {
        old: Option<Recurrence>,
        new: Option<Recurrence>,
    },
    DueRecurrence {
        old: Option<Recurrence>,
        new: Option<Recurrence>,
    },
    ExternalId {
        old: Option<String>,
        new: Option<String>,
    },
    TemplateName {
        old: Option<String>,
        new: Option<String>,
    },
    DtStart {
        old: Option<chrono::DateTime<chrono::Local>>,
        new: Option<chrono::DateTime<chrono::Local>>,
    },
}

/// A change to a single field on an Action.
#[derive(Debug, Clone, PartialEq)]
pub enum ActFieldChange {
    Phase {
        old: ActionState,
        new: ActionState,
    },
    ScheduledAt {
        old: Option<DateTime<Local>>,
        new: Option<DateTime<Local>>,
    },
    Duration {
        old: Option<u32>,
        new: Option<u32>,
    },
    CompletedAt {
        old: Option<DateTime<Local>>,
        new: Option<DateTime<Local>>,
    },
    CreatedAt {
        old: Option<DateTime<Local>>,
        new: Option<DateTime<Local>>,
    },
    DueDate {
        old: Option<DateTime<Local>>,
        new: Option<DateTime<Local>>,
    },
}

/// A change to a single field on a Charter.
#[derive(Debug, Clone, PartialEq)]
pub enum CharterFieldChange {
    Title {
        old: String,
        new: String,
    },
    Description {
        old: Option<String>,
        new: Option<String>,
    },
    Alias {
        old: Option<String>,
        new: Option<String>,
    },
    Parent {
        old: Option<String>,
        new: Option<String>,
    },
}

// ============================================================================
// Hierarchical diff structs
// ============================================================================

/// Changes detected for a single Action.
#[derive(Debug, Clone, PartialEq)]
pub struct ActDiff {
    pub id: Uuid,
    pub plan_id: Option<Uuid>,
    pub changes: Vec<ActFieldChange>,
}

/// Changes detected for a single Plan and its acts.
#[derive(Debug, Clone, PartialEq)]
pub struct PlanDiff {
    pub id: Uuid,
    pub changes: Vec<PlanFieldChange>,
    pub acts_added: Vec<Action>,
    pub acts_removed: Vec<Action>,
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

/// Complete hierarchical diff between two [`DomainModel`]s.
///
/// Use [`diff_domain_models`] to generate this struct.
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

    /// IDs of [`Charter`]s that have any change (added, removed, or modified).
    pub fn affected_charter_ids(&self) -> Vec<Uuid> {
        let mut ids: Vec<Uuid> = self
            .charters_added
            .iter()
            .map(|c| c.id)
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

/// Diff two [`DomainModel`]s, producing a hierarchical [`DomainDiff`].
///
/// [`Charter`]s are matched by `charter.id`. [`Plan`]s within a charter are matched
/// by `plan.id`. [`Action`]s within a plan are matched by `act.id`.
pub fn diff_domain_models(old: &DomainModel, new: &DomainModel) -> DomainDiff {
    let old_charters: HashMap<Uuid, &Charter> = old.charters.iter().map(|c| (c.id, c)).collect();
    let new_charters: HashMap<Uuid, &Charter> = new.charters.iter().map(|c| (c.id, c)).collect();

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
            let plan_diff = diff_plans(old_plan, new_plan, &old.actions, &new.actions);
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

fn diff_plans(
    old: &Plan,
    new: &Plan,
    old_charter_acts: &[Action],
    new_charter_acts: &[Action],
) -> PlanDiff {
    let changes = compare_plans(old, new);

    let old_acts: HashMap<Uuid, &Action> = old_charter_acts
        .iter()
        .filter(|a| a.plan_id == Some(old.id))
        .map(|a| (a.id, a))
        .collect();
    let new_acts: HashMap<Uuid, &Action> = new_charter_acts
        .iter()
        .filter(|a| a.plan_id == Some(new.id))
        .map(|a| (a.id, a))
        .collect();

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
        changes.push(CharterFieldChange::Title {
            old: old.title.clone(),
            new: new.title.clone(),
        });
    }
    if old.description != new.description {
        changes.push(CharterFieldChange::Description {
            old: old.description.clone(),
            new: new.description.clone(),
        });
    }
    if old.alias != new.alias {
        changes.push(CharterFieldChange::Alias {
            old: old.alias.clone(),
            new: new.alias.clone(),
        });
    }
    if old.parent != new.parent {
        changes.push(CharterFieldChange::Parent {
            old: old.parent.clone(),
            new: new.parent.clone(),
        });
    }
    changes
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
    if old.recurrence != new.recurrence {
        changes.push(PlanFieldChange::Recurrence {
            old: old.recurrence.clone(),
            new: new.recurrence.clone(),
        });
    }
    if old.due_recurrence != new.due_recurrence {
        changes.push(PlanFieldChange::DueRecurrence {
            old: old.due_recurrence.clone(),
            new: new.due_recurrence.clone(),
        });
    }
    if old.external_id != new.external_id {
        changes.push(PlanFieldChange::ExternalId {
            old: old.external_id.clone(),
            new: new.external_id.clone(),
        });
    }
    if old.template_name != new.template_name {
        changes.push(PlanFieldChange::TemplateName {
            old: old.template_name.clone(),
            new: new.template_name.clone(),
        });
    }
    if !dates_equal(&old.dtstart, &new.dtstart) {
        changes.push(PlanFieldChange::DtStart {
            old: old.dtstart,
            new: new.dtstart,
        });
    }
    changes
}

fn compare_acts(old: &Action, new: &Action) -> Vec<ActFieldChange> {
    let mut changes = Vec::new();
    if old.state != new.state {
        changes.push(ActFieldChange::Phase {
            old: old.state,
            new: new.state,
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
    if !dates_equal(&old.due_date, &new.due_date) {
        changes.push(ActFieldChange::DueDate {
            old: old.due_date,
            new: new.due_date,
        });
    }
    changes
}

/// Compare two optional DateTimes at minute precision.
///
/// The .actions text format uses `%Y-%m-%dT%H:%M` (minute precision), so
/// timestamps that differ only in seconds or sub-seconds are semantically
/// identical after a format/parse round-trip.
fn dates_equal(a: &Option<DateTime<Local>>, b: &Option<DateTime<Local>>) -> bool {
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
    use crate::domain::{ActionState, Action, Charter};

    fn make_model(id: &str, name: &str) -> DomainModel {
        let plan_id = Uuid::parse_str(id).unwrap();
        let act_id = Uuid::new_v5(&plan_id, b"act-0");
        let charter_id = Uuid::new_v5(&plan_id, b"charter");

        DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: charter_id,
                title: "Test Charter".to_string(),
                plans: vec![Plan {
                    id: plan_id,
                    name: name.to_string(),
                    ..Default::default()
                }],
                actions: vec![Action {
                    id: act_id,
                    name: name.to_string(),
                    plan_id: Some(plan_id),
                    ..Default::default()
                }],
                ..Default::default()
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
        new.charters[0].actions[0].state = ActionState::Completed;

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
