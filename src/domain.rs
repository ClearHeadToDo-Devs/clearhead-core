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
use std::fmt;
use uuid::Uuid;

use crate::actions::{ActionList, ActionState};
use crate::sync_utils::{hydrate_date, reconcile_date};

/// A measurable indicator tied to an objective.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct Metric {
    pub name: String,
    pub description: Option<String>,
    pub target: Option<String>,
    pub review_date: Option<String>,
}

/// A high-level goal that organizes charters.
///
/// Maps to `actions:Objective` — the topmost organizational layer.
/// Objectives sit above charters in the hierarchy:
/// Objectives → Charters → Plans → Acts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct Objective {
    pub id: Uuid,
    pub title: Option<String>,
    pub description: Option<String>,
    pub alias: Option<String>,
    pub parent: Option<String>,
    pub metrics: Option<Vec<Metric>>,
}

/// Recurrence rule per RFC 5545 RRULE specification
#[derive(PartialEq, Debug, Clone, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct Recurrence {
    pub frequency: String, // FREQ: secondly, minutely, hourly, daily, weekly, monthly, yearly
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interval: Option<u32>, // INTERVAL: default 1
    #[serde(skip_serializing_if = "Option::is_none")]
    pub count: Option<u32>, // COUNT: max occurrences
    #[serde(skip_serializing_if = "Option::is_none")]
    pub until: Option<String>, // UNTIL: end date/time in ISO 8601
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_second: Option<Vec<u32>>, // BYSECOND: 0-59
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_minute: Option<Vec<u32>>, // BYMINUTE: 0-59
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_hour: Option<Vec<u32>>, // BYHOUR: 0-23
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_day: Option<Vec<String>>, // BYDAY: MO,TU,WE,TH,FR,SA,SU (with optional modifiers like 1MO, -1FR)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_month_day: Option<Vec<i32>>, // BYMONTHDAY: 1-31 or -1 to -31
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_year_day: Option<Vec<i32>>, // BYYEARDAY: 1-366 or -1 to -366
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_week_no: Option<Vec<i32>>, // BYWEEKNO: 1-53 or -1 to -53
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_month: Option<Vec<u32>>, // BYMONTH: 1-12
    #[serde(skip_serializing_if = "Option::is_none")]
    pub by_set_pos: Option<Vec<i32>>, // BYSETPOS: limits to nth occurrence
    #[serde(skip_serializing_if = "Option::is_none")]
    pub week_start: Option<String>, // WKST: MO,TU,WE,TH,FR,SA,SU (default MO)
}

impl fmt::Display for Recurrence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "R:FREQ={}", self.frequency.to_uppercase())?;

        if let Some(interval) = self.interval {
            write!(f, ";INTERVAL={}", interval)?;
        }
        if let Some(count) = self.count {
            write!(f, ";COUNT={}", count)?;
        }
        if let Some(until) = &self.until {
            write!(f, ";UNTIL={}", until)?;
        }
        if let Some(by_second) = &self.by_second {
            write!(
                f,
                ";BYSECOND={}",
                by_second
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(by_minute) = &self.by_minute {
            write!(
                f,
                ";BYMINUTE={}",
                by_minute
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(by_hour) = &self.by_hour {
            write!(
                f,
                ";BYHOUR={}",
                by_hour
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(by_day) = &self.by_day {
            write!(f, ";BYDAY={}", by_day.join(","))?;
        }
        if let Some(by_month_day) = &self.by_month_day {
            write!(
                f,
                ";BYMONTHDAY={}",
                by_month_day
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(by_year_day) = &self.by_year_day {
            write!(
                f,
                ";BYYEARDAY={}",
                by_year_day
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(by_week_no) = &self.by_week_no {
            write!(
                f,
                ";BYWEEKNO={}",
                by_week_no
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(by_month) = &self.by_month {
            write!(
                f,
                ";BYMONTH={}",
                by_month
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(by_set_pos) = &self.by_set_pos {
            write!(
                f,
                ";BYSETPOS={}",
                by_set_pos
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )?;
        }
        if let Some(week_start) = &self.week_start {
            write!(f, ";WKST={}", week_start)?;
        }

        Ok(())
    }
}

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
    /// Duration in minutes (template for acts)
    pub duration: Option<u32>,
    /// Plans this plan depends on (predecessor relationships)
    pub depends_on: Option<Vec<Uuid>>,
    /// PlannedActs that realize this plan (nested hierarchy)
    pub acts: Vec<PlannedAct>,
}

impl Plan {
    /// Expand recurrence rule into a list of occurrence dates.
    ///
    /// Uses the provided `dtstart` as DTSTART and `self.recurrence` as RRULE.
    /// Returns an empty vector if no recurrence is present or if parsing fails.
    ///
    /// # Arguments
    /// * `dtstart` - The start date/time for the recurrence series
    /// * `limit` - Maximum number of occurrences to generate
    pub fn expand_occurrences(
        &self,
        dtstart: DateTime<Local>,
        limit: u16,
    ) -> Vec<DateTime<rrule::Tz>> {
        let recurrence = match &self.recurrence {
            Some(r) => r,
            None => return Vec::new(),
        };

        let recurrence_str = recurrence.to_string();
        let clean_recurrence = recurrence_str.strip_prefix("R:").unwrap_or(&recurrence_str);

        let rrule_str = format!(
            "DTSTART:{}\nRRULE:{}",
            dtstart.format("%Y%m%dT%H%M%S"),
            clean_recurrence
        );

        match rrule_str.parse::<rrule::RRuleSet>() {
            Ok(rrule_set) => rrule_set.all(limit).dates,
            Err(e) => {
                eprintln!("Failed to parse recurrence rule: {}", e);
                Vec::new()
            }
        }
    }
}

/// A charter — a directive that organizes plans under a shared purpose.
///
/// Maps to `actions:Charter` (subclass of cco:DirectiveInformationContentEntity).
/// Charters are the highest-level organizational unit. Plans reference charters
/// via the `charter` field.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct Charter {
    pub id: Uuid,
    pub title: String,
    pub description: Option<String>,
    pub alias: Option<String>,
    /// Reference string for parent charter, resolved at workspace layer
    pub parent: Option<String>,
    /// References to objectives, resolved at workspace layer
    pub objectives: Option<Vec<String>>,
    /// Plans organized under this charter (nested hierarchy)
    pub plans: Vec<Plan>,
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

/// Hierarchical domain model: Objectives → Charters → Plans → Acts.
///
/// Each charter contains its plans, and each plan contains its acts.
/// This structure reflects the ontology hierarchy and eliminates the
/// need for separate flat HashMaps.
#[derive(Debug, Clone, Default, Serialize, Deserialize, Reconcile, Hydrate)]
pub struct DomainModel {
    pub objectives: Vec<Objective>,
    pub charters: Vec<Charter>,
}

impl DomainModel {
    pub fn new() -> Self {
        Self::default()
    }

    /// Convert an ActionList into the domain model.
    ///
    /// Each Action becomes one Plan and one PlannedAct, wrapped in a
    /// synthetic inbox charter.
    pub fn from_actions(actions: &ActionList) -> Self {
        crate::actions::convert::from_actions(actions)
    }

    /// Convert the domain model back to an ActionList.
    ///
    /// This reconstructs Actions from Plans and their PlannedActs,
    /// walking the charter → plan → act hierarchy.
    pub fn to_action_list(&self) -> ActionList {
        crate::actions::convert::to_action_list(self)
    }

    /// Convert the domain model back to an ActionList in a specific order.
    pub fn to_action_list_ordered(&self, plan_order: &[String]) -> ActionList {
        crate::actions::convert::to_action_list_ordered(self, plan_order)
    }

    /// Flatten all plans across all charters.
    pub fn all_plans(&self) -> Vec<&Plan> {
        self.charters.iter().flat_map(|c| c.plans.iter()).collect()
    }

    /// Flatten all acts across all charters and plans.
    pub fn all_acts(&self) -> Vec<&PlannedAct> {
        self.charters
            .iter()
            .flat_map(|c| c.plans.iter())
            .flat_map(|p| p.acts.iter())
            .collect()
    }

    /// Find a Plan by ID, searching across the hierarchy.
    pub fn plan(&self, id: Uuid) -> Option<&Plan> {
        self.all_plans().into_iter().find(|p| p.id == id)
    }

    /// Find the charter that contains a given plan.
    pub fn charter_for_plan(&self, plan_id: Uuid) -> Option<&Charter> {
        self.charters
            .iter()
            .find(|c| c.plans.iter().any(|p| p.id == plan_id))
    }

    /// Find all PlannedActs for a given Plan
    pub fn acts_for_plan(&self, plan_id: Uuid) -> Vec<&PlannedAct> {
        self.all_plans()
            .into_iter()
            .find(|p| p.id == plan_id)
            .map(|p| p.acts.iter().collect())
            .unwrap_or_default()
    }

    /// Get all incomplete acts across the hierarchy.
    pub fn incomplete_acts(&self) -> Vec<&PlannedAct> {
        self.all_acts()
            .into_iter()
            .filter(|a| !matches!(a.phase, ActPhase::Completed | ActPhase::Cancelled))
            .collect()
    }

    /// Expand recurring plans into multiple PlannedActs up to a limit of days.
    pub fn expand_recurring_plans(&mut self, days: u32) {
        use chrono::Duration;

        let now = Local::now();
        let end_date = now + Duration::days(days as i64);

        for charter in &mut self.charters {
            for plan in &mut charter.plans {
                if plan.recurrence.is_some() {
                    let dtstart = plan.acts.first().and_then(|a| a.scheduled_at);
                    let dtstart = match dtstart {
                        Some(dt) => dt,
                        None => continue,
                    };

                    let existing_ids: std::collections::HashSet<Uuid> =
                        plan.acts.iter().map(|a| a.id).collect();

                    let occurrences = plan.expand_occurrences(dtstart, 1000);
                    for (i, occ) in occurrences.iter().enumerate() {
                        let occ_local = occ.with_timezone(&Local);
                        if occ_local > end_date {
                            break;
                        }

                        let act_id = Uuid::new_v5(&plan.id, format!("act-{}", i).as_bytes());

                        if !existing_ids.contains(&act_id) {
                            plan.acts.push(PlannedAct {
                                id: act_id,
                                plan_id: plan.id,
                                phase: ActPhase::NotStarted,
                                scheduled_at: Some(occ_local),
                                duration: plan.duration,
                                completed_at: None,
                                created_at: Some(now),
                            });
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Domain Diff Types
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
    Priority {
        old: Option<u32>,
        new: Option<u32>,
    },
    Contexts {
        old: Option<Vec<String>>,
        new: Option<Vec<String>>,
    },
    Parent {
        old: Option<Uuid>,
        new: Option<Uuid>,
    },
    Objective {
        old: Option<String>,
        new: Option<String>,
    },
    Alias {
        old: Option<String>,
        new: Option<String>,
    },
    IsSequential {
        old: Option<bool>,
        new: Option<bool>,
    },
    Recurrence {
        old: Option<Recurrence>,
        new: Option<Recurrence>,
    },
    DependsOn {
        old: Option<Vec<Uuid>>,
        new: Option<Vec<Uuid>>,
    },
}

/// A change to a single field on a PlannedAct.
#[derive(Debug, Clone, PartialEq)]
pub enum ActFieldChange {
    Phase {
        old: ActPhase,
        new: ActPhase,
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
    use crate::actions::Action;

    #[test]
    fn test_split_simple_action() {
        let action = Action {
            id: Uuid::new_v4(),
            name: "Buy milk".to_string(),
            state: ActionState::NotStarted,
            ..Default::default()
        };

        let (plan, act) = crate::actions::convert::split_action(&action);

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

        let (_, act) = crate::actions::convert::split_action(&action);

        assert_eq!(act.phase, ActPhase::Completed);
        assert!(act.completed_at.is_some());
    }

    #[test]
    fn test_domain_model_from_actions() {
        let actions = vec![Action::new("Task 1"), Action::new("Task 2")];

        let model = DomainModel::from_actions(&actions);

        assert_eq!(model.all_plans().len(), 2);
        assert_eq!(model.all_acts().len(), 2);
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
        let mut ids1: Vec<String> = model1.all_acts().iter().map(|a| a.id.to_string()).collect();
        let mut ids2: Vec<String> = model2.all_acts().iter().map(|a| a.id.to_string()).collect();
        ids1.sort();
        ids2.sort();
        assert_eq!(ids1, ids2);
    }

    #[test]
    fn test_act_id_derived_from_plan_id() {
        let action = Action::new("Test");
        let (plan, act) = crate::actions::convert::split_action(&action);

        // Act ID should be deterministic v5 UUID based on plan ID
        let expected_act_id = Uuid::new_v5(&plan.id, b"act-0");
        assert_eq!(act.id, expected_act_id);
    }

    #[test]
    fn test_expand_occurrences() {
        use chrono::TimeZone;

        let dt_start = Local.with_ymd_and_hms(2025, 1, 1, 9, 0, 0).unwrap();

        let recurrence = Recurrence {
            frequency: "daily".to_string(),
            interval: Some(1),
            count: Some(3),
            until: None,
            by_second: None,
            by_minute: None,
            by_hour: None,
            by_day: None,
            by_month_day: None,
            by_year_day: None,
            by_week_no: None,
            by_month: None,
            by_set_pos: None,
            week_start: None,
        };

        let plan = Plan {
            id: Uuid::new_v4(),
            name: "Daily Standup".to_string(),
            description: None,
            priority: None,
            contexts: None,
            recurrence: Some(recurrence),
            parent: None,
            objective: None,
            alias: None,
            is_sequential: None,
            duration: None,
            depends_on: None,
            acts: vec![],
        };

        let occurrences = plan.expand_occurrences(dt_start, 10);

        assert_eq!(occurrences.len(), 3);
        assert_eq!(occurrences[0].format("%Y-%m-%d").to_string(), "2025-01-01");
        assert_eq!(occurrences[1].format("%Y-%m-%d").to_string(), "2025-01-02");
        assert_eq!(occurrences[2].format("%Y-%m-%d").to_string(), "2025-01-03");
    }
}
