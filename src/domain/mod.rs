//! Domain model aligned with the Actions Vocabulary v4 ontology.
//!
//! This module provides structs that map to the CCO-aligned ontology:
//! - [`Plan`] (cco:Plan) - task definition / template (information content)
//! - [`PlannedAct`] (cco:PlannedAct) - actual execution (occurrence)
//! - [`ActPhase`] - lifecycle state (custom BFO Quality)
//!
//! The key insight from BFO: information vs occurrence.
//! - A [`Plan`] is a *continuant* — persists and can be realized multiple times
//! - A [`PlannedAct`] is an *occurrent* — unfolds through time
//!
//! # Example
//!
//! "Do laundry weekly" is one [`Plan`]. Each week's laundry is a separate [`PlannedAct`].
//! For non-recurring tasks, there's still one [`Plan`] and one [`PlannedAct`].

pub mod diff;

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Reference {
    UUID(Uuid),
    Prefix(String),
    Name(String),
    Alias(String),
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reference::UUID(id) => write!(f, "{}", id),
            Reference::Prefix(p) => write!(f, "{}", p),
            Reference::Name(n) => write!(f, "{}", n),
            Reference::Alias(a) => write!(f, "{}", a),
        }
    }
}

/// A measurable indicator tied to an [`Objective`].
///
/// Metrics provide a way to track progress toward a high-level goal.
///
/// # Examples
///
/// ```
/// use clearhead_core::domain::Metric;
///
/// let metric = Metric {
///     name: "Uptime".to_string(),
///     description: Some("Percentage of time the service is available".to_string()),
///     target: Some("99.9%".to_string()),
///     review_date: Some("2026-06-01".to_string()),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Metric {
    pub name: String,
    pub description: Option<String>,
    pub target: Option<String>,
    pub review_date: Option<String>,
}

/// A high-level goal that organizes [`Charter`]s.
///
/// Maps to `actions:Objective` — the topmost organizational layer.
/// Objectives sit above charters in the hierarchy:
/// [`Objective`] → [`Charter`] → [`Plan`] → [`PlannedAct`]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Objective {
    pub id: Uuid,
    pub title: Option<String>,
    pub description: Option<String>,
    pub alias: Option<String>,
    pub parent: Option<String>,
    pub metrics: Option<Vec<Metric>>,
}

/// Recurrence rule per RFC 5545 RRULE specification.
///
/// Used by [`Plan`] to prescribe multiple [`PlannedAct`]s.
///
/// # Examples
///
/// ```
/// use clearhead_core::domain::Recurrence;
///
/// let r = Recurrence {
///     frequency: "weekly".to_string(),
///     interval: Some(2),
///     count: Some(5),
///     until: None,
///     by_second: None,
///     by_minute: None,
///     by_hour: None,
///     by_day: Some(vec!["MO".to_string(), "WE".to_string()]),
///     by_month_day: None,
///     by_year_day: None,
///     by_week_no: None,
///     by_month: None,
///     by_set_pos: None,
///     week_start: None,
/// };
///
/// assert_eq!(r.to_string(), "R:FREQ=WEEKLY;INTERVAL=2;COUNT=5;BYDAY=MO,WE");
/// ```
#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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

impl Default for Recurrence {
    fn default() -> Self {
        Self {
            frequency: String::new(),
            interval: None,
            count: None,
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
        }
    }
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

impl Recurrence {
    /// Parse an RFC 5545 RRULE string (without the "RRULE:" prefix) into a `Recurrence`.
    pub fn from_rrule_str(s: &str) -> Option<Self> {
        let s = s.strip_prefix("R:").unwrap_or(s);
        let s = s.strip_prefix("RRULE:").unwrap_or(s);
        let mut r = Recurrence::default();
        for part in s.split(';') {
            let (key, value) = part.split_once('=')?;
            match key {
                "FREQ" => r.frequency = value.to_lowercase(),
                "INTERVAL" => r.interval = value.parse().ok(),
                "COUNT" => r.count = value.parse().ok(),
                "UNTIL" => r.until = Some(value.to_string()),
                "BYSECOND" => r.by_second = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "BYMINUTE" => r.by_minute = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "BYHOUR" => r.by_hour = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "BYDAY" => r.by_day = Some(value.split(',').map(|v| v.to_string()).collect()),
                "BYMONTHDAY" => r.by_month_day = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "BYYEARDAY" => r.by_year_day = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "BYWEEKNO" => r.by_week_no = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "BYMONTH" => r.by_month = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "BYSETPOS" => r.by_set_pos = Some(value.split(',').filter_map(|v| v.parse().ok()).collect()),
                "WKST" => r.week_start = Some(value.to_string()),
                _ => {}
            }
        }
        if r.frequency.is_empty() {
            return None;
        }
        Some(r)
    }
}

/// Lifecycle phase of a [`PlannedAct`].
///
/// Maps to `actions:ActPhase` (subclass of bfo:Quality).
/// The phase inheres in the [`PlannedAct`], not the [`Plan`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ActPhase {
    /// Act is defined but not yet ready for execution.
    #[default]
    NotStarted,
    /// Act is currently being executed.
    InProgress,
    /// Act has been successfully completed.
    Completed,
    /// Act is unable to proceed due to a dependency or obstacle.
    Blocked,
    /// Act has been abandoned and will not be completed.
    Cancelled,
}

/// Task definition or template for execution.
///
/// Maps to `cco:Plan` — information content that persists and can be
/// realized multiple times (for recurring tasks) or once (for one-off tasks).
///
/// Plans hold the "what" — name, description, priority, contexts, recurrence rules.
/// They do not hold execution state; that is the responsibility of [`PlannedAct`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Plan {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub priority: Option<u32>,
    pub contexts: Option<Vec<String>>,
    pub recurrence: Option<Recurrence>,
    /// Independent due-date recurrence rule (R: following :-syntax)
    pub due_recurrence: Option<Recurrence>,
    /// Parent plan (partOf relationship for hierarchy)
    pub parent: Option<Uuid>,
    /// Stable alias for references
    pub alias: Option<String>,
    /// Whether children execute sequentially
    pub is_sequential: Option<bool>,
    /// Plans this plan depends on (predecessor relationships)
    pub depends_on: Option<Vec<Uuid>>,
    /// Raw VEVENT UID when this plan was loaded from an ICS file (null for .actions-sourced plans)
    pub external_id: Option<String>,
    /// X-CLEARHEAD-TEMPLATE value from VEVENT (null for .actions-sourced plans)
    pub template_name: Option<String>,
    /// Recurrence anchor (DTSTART from VEVENT); None for .actions-sourced plans
    pub dtstart: Option<DateTime<Local>>,
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
    ///
    /// # Examples
    ///
    /// ```
    /// use clearhead_core::domain::{Plan, Recurrence};
    /// use chrono::{TimeZone, Local};
    /// use uuid::Uuid;
    ///
    /// let dt_start = Local.with_ymd_and_hms(2025, 1, 1, 9, 0, 0).unwrap();
    /// let plan = Plan {
    ///     id: Uuid::new_v4(),
    ///     name: "Daily".to_string(),
    ///     recurrence: Some(Recurrence { frequency: "daily".to_string(), count: Some(2), ..Default::default() }),
    ///     description: None, priority: None, contexts: None, due_recurrence: None, parent: None, alias: None, is_sequential: None, depends_on: None, external_id: None, template_name: None, dtstart: None,
    /// };
    ///
    /// let occurrences = plan.expand_occurrences(dt_start, 10);
    /// assert_eq!(occurrences.len(), 2);
    /// ```
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

impl Default for Plan {
    fn default() -> Self {
        Self {
            id: Uuid::nil(),
            name: String::new(),
            description: None,
            priority: None,
            contexts: None,
            recurrence: None,
            due_recurrence: None,
            parent: None,
            alias: None,
            is_sequential: None,
            depends_on: None,
            external_id: None,
            template_name: None,
            dtstart: None,
        }
    }
}

impl Plan {
    /// Construct a new plan with a generated UUIDv7 and all optional fields unset.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::now_v7(),
            name: name.into(),
            ..Default::default()
        }
    }
}

/// A charter — a directive that organizes plans under a shared purpose.
///
/// Maps to `actions:Charter` (subclass of cco:DirectiveInformationContentEntity).
/// Charters are the highest-level organizational unit for [`Plan`]s.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Charter {
    pub id: Uuid,
    pub title: String,
    pub description: Option<String>,
    pub alias: Option<String>,
    /// Reference string for parent charter, resolved at workspace layer
    pub parent: Option<String>,
    /// References to [`Objective`]s, resolved at workspace layer
    pub objectives: Option<Vec<String>>,
    /// [`Plan`]s organized under this charter (nested hierarchy)
    pub plans: Vec<Plan>,
    /// [`PlannedAct`]s scoped to this charter; each may optionally reference a plan.
    pub acts: Vec<PlannedAct>,
}

pub fn charter_from_plans_and_name(name: String, plans: Vec<Plan>) -> Charter {
    Charter {
        id: Uuid::new_v4(),
        title: name.clone(),
        description: None,
        alias: Some(name.clone()),
        parent: None,
        objectives: None,
        plans,
        acts: vec![],
    }
}

/// Actual execution or occurrence of a [`Plan`].
///
/// Maps to `cco:PlannedAct` — something that unfolds through time.
/// Each realization of a [`Plan`] creates a [`PlannedAct`].
///
/// [`PlannedAct`]s hold the "when" and "status" — scheduled time, completion, phase.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PlannedAct {
    pub id: Uuid,
    /// The [`Plan`] this act realizes (prescribes relationship, inverse)
    pub plan_id: Option<Uuid>,
    /// Optional external schedule-series identifier (integration profile bridge)
    pub external_schedule_id: Option<String>,
    /// Optional external occurrence identifier within a schedule series
    pub external_occurrence_key: Option<String>,
    /// Current lifecycle phase
    pub phase: ActPhase,
    /// Scheduled date/time for this occurrence (@-syntax)
    pub scheduled_at: Option<DateTime<Local>>,
    /// Deadline for this occurrence (:-syntax)
    pub due_date: Option<DateTime<Local>>,
    /// Duration in minutes
    pub duration: Option<u32>,
    /// When this act was completed
    pub completed_at: Option<DateTime<Local>>,
    /// When this act was created/scheduled
    pub created_at: Option<DateTime<Local>>,
}

impl Default for PlannedAct {
    fn default() -> Self {
        Self {
            id: Uuid::nil(),
            plan_id: None,
            external_schedule_id: None,
            external_occurrence_key: None,
            phase: ActPhase::NotStarted,
            scheduled_at: None,
            due_date: None,
            duration: None,
            completed_at: None,
            created_at: None,
        }
    }
}

/// Hierarchical domain model: Objectives → Charters → Plans → Acts.
///
/// Each charter contains its plans, and each plan contains its acts.
/// This structure reflects the ontology hierarchy and eliminates the
/// need for separate flat HashMaps.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DomainModel {
    pub objectives: Vec<Objective>,
    pub charters: Vec<Charter>,
}

impl DomainModel {
    pub fn new() -> Self {
        Self::default()
    }

    /// Flatten all plans across all charters.
    pub fn all_plans(&self) -> Vec<&Plan> {
        self.charters.iter().flat_map(|c| c.plans.iter()).collect()
    }

    /// Flatten all acts across all charters and plans.
    pub fn all_acts(&self) -> Vec<&PlannedAct> {
        self.charters.iter().flat_map(|c| c.acts.iter()).collect()
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
        self.all_acts()
            .into_iter()
            .filter(|a| a.plan_id == Some(plan_id))
            .collect()
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
                    let dtstart = charter
                        .acts
                        .iter()
                        .find(|a| a.plan_id == Some(plan.id))
                        .and_then(|a| a.scheduled_at);
                    let dtstart = match dtstart {
                        Some(dt) => dt,
                        None => continue,
                    };

                    let existing_ids: std::collections::HashSet<Uuid> =
                        charter.acts.iter().map(|a| a.id).collect();

                    let occurrences = plan.expand_occurrences(dtstart, 1000);
                    let template_duration = charter
                        .acts
                        .iter()
                        .find(|a| a.plan_id == Some(plan.id))
                        .and_then(|a| a.duration);
                    for (i, occ) in occurrences.iter().enumerate() {
                        let occ_local = occ.with_timezone(&Local);
                        if occ_local > end_date {
                            break;
                        }

                        let act_id = Uuid::new_v5(&plan.id, format!("act-{}", i).as_bytes());

                        if !existing_ids.contains(&act_id) {
                            charter.acts.push(PlannedAct {
                                id: act_id,
                                plan_id: Some(plan.id),
                                scheduled_at: Some(occ_local),
                                duration: template_duration,
                                created_at: Some(now),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            recurrence: Some(recurrence),
            ..Default::default()
        };

        let occurrences = plan.expand_occurrences(dt_start, 10);

        assert_eq!(occurrences.len(), 3);
        assert_eq!(occurrences[0].format("%Y-%m-%d").to_string(), "2025-01-01");
        assert_eq!(occurrences[1].format("%Y-%m-%d").to_string(), "2025-01-02");
        assert_eq!(occurrences[2].format("%Y-%m-%d").to_string(), "2025-01-03");
    }

    #[test]
    fn test_expand_acts_running_twice_should_not_change_structure() {
        let mut model = DomainModel::new();

        let plan_id = Uuid::new_v4();

        let charter = Charter {
            id: Uuid::new_v4(),
            title: "Test Charter".to_string(),
            plans: vec![Plan {
                id: plan_id,
                name: "Test Recurring".to_string(),
                recurrence: Some(Recurrence {
                    frequency: "daily".to_string(),
                    interval: Some(1),
                    count: Some(2),
                    ..Default::default()
                }),
                ..Default::default()
            }],
            acts: vec![PlannedAct {
                id: Uuid::new_v4(),
                plan_id: Some(plan_id),
                scheduled_at: Some(Local::now()),
                duration: Some(30),
                created_at: Some(Local::now()),
                ..Default::default()
            }],
            ..Default::default()
        };

        model.charters.push(charter);

        model.expand_recurring_plans(7);
        assert_eq!(model.all_acts().len(), 3);
        model.expand_recurring_plans(7);
        assert_eq!(model.all_acts().len(), 3); // No duplicates should be created
    }
}
