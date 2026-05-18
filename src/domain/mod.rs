//! Domain model aligned with the Actions Vocabulary v4 ontology.
//!
//! This module provides structs that map to the ClearHead domain model:
//! - [`Plan`] - schedule definition for recurring work
//! - [`Action`] - actionable work item, optionally prescribed by a plan
//! - [`ActPhase`] - lifecycle state for actions
//!
//! The key distinction is schedule vs actionable work.
//! - A [`Plan`] holds recurrence/schedule semantics
//! - An [`Action`] holds the task/execution-facing data users manipulate directly
//!
//! # Example
//!
//! "Do laundry weekly" is one [`Plan`]. Each week's laundry can become a separate [`Action`].
//! Non-recurring work can exist directly as an [`Action`] without any [`Plan`].

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
/// [`Objective`] → [`Charter`] → [`Plan`] / [`Action`]
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
/// Used by [`Plan`] to prescribe multiple [`Action`]s.
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
                "BYSECOND" => {
                    r.by_second = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
                "BYMINUTE" => {
                    r.by_minute = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
                "BYHOUR" => {
                    r.by_hour = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
                "BYDAY" => r.by_day = Some(value.split(',').map(|v| v.to_string()).collect()),
                "BYMONTHDAY" => {
                    r.by_month_day = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
                "BYYEARDAY" => {
                    r.by_year_day = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
                "BYWEEKNO" => {
                    r.by_week_no = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
                "BYMONTH" => {
                    r.by_month = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
                "BYSETPOS" => {
                    r.by_set_pos = Some(value.split(',').filter_map(|v| v.parse().ok()).collect())
                }
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

/// Lifecycle state of a [`Charter`].
///
/// Charters move through a simple workflow. `Closed` is the precondition
/// for archival — the charter and all its artifacts are swept into
/// `archive.ttl` only when this state is set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CharterState {
    /// Newly created, not yet active.
    New,
    /// Actively being worked on.
    Active,
    /// Blocked on an external dependency.
    Blocked,
    /// All work is done; ready to be archived.
    Closed,
}

impl std::fmt::Display for CharterState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CharterState::New => write!(f, "New"),
            CharterState::Active => write!(f, "Active"),
            CharterState::Blocked => write!(f, "Blocked"),
            CharterState::Closed => write!(f, "Closed"),
        }
    }
}

/// Lifecycle state of an [`Action`].
///
/// Maps to `actions:ActionState`. The state inheres in the [`Action`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ActionState {
    /// Action is defined but not yet started.
    #[default]
    NotStarted,
    /// Action is currently being worked on.
    InProgress,
    /// Action has been successfully completed.
    Completed,
    /// Action is blocked or awaiting something external.
    #[serde(rename = "blocked")]
    BlockedOrAwaiting,
    /// Action has been abandoned and will not be completed.
    Cancelled,
}


/// Reference to a predecessor action.
///
/// Carries both the raw text from the DSL (`raw_ref`) and the resolved UUID
/// after workspace loading. The raw form is required for file round-trips;
/// the graph and CRDT layers use only `resolved_uuid`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PredecessorRef {
    /// The raw reference text from the source (e.g., "build core" or a UUID).
    pub raw_ref: String,
    /// The resolved UUID if resolution was successful.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolved_uuid: Option<Uuid>,
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
    pub recurrence: Option<Recurrence>,
    /// Independent due-date recurrence rule (R: following :-syntax)
    pub due_recurrence: Option<Recurrence>,
    /// Raw VEVENT UID when this plan was loaded from an ICS file (null for .actions-sourced plans)
    pub external_id: Option<String>,
    /// Template name extracted from VEVENT DESCRIPTION (null for .actions-sourced plans)
    pub template_name: Option<String>,
    /// Per-schedule override for how many instances land in the primary `.actions` file.
    /// Sourced from the `upcoming:` directive in VEVENT DESCRIPTION.
    /// When absent, the workspace `expansion_primary_instances` config value applies.
    pub primary_instances: Option<u32>,
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
    ///     description: None, due_recurrence: None, external_id: None, template_name: None, primary_instances: None, dtstart: None,
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
            recurrence: None,
            due_recurrence: None,
            external_id: None,
            template_name: None,
            primary_instances: None,
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
    /// Lifecycle state of this charter. `None` is treated as [`CharterState::New`].
    /// Set to [`CharterState::Closed`] before archiving.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<CharterState>,
    /// [`Plan`]s organized under this charter (nested hierarchy)
    pub plans: Vec<Plan>,
    /// [`Action`]s scoped to this charter; each may optionally reference a plan.
    pub actions: Vec<Action>,
}

pub fn charter_from_plans_and_name(name: String, plans: Vec<Plan>) -> Charter {
    Charter {
        id: Uuid::new_v4(),
        title: name.clone(),
        description: None,
        alias: Some(name.clone()),
        parent: None,
        objectives: None,
        state: None,
        plans,
        actions: vec![],
    }
}

/// The single unified action type across file I/O, graph, and CRDT layers.
///
/// Parsed directly from `.actions` files by the workspace parser and used
/// unchanged by the graph, CRDT, and display layers. No conversion needed.
///
/// Fields set to `None` by the parser (no DSL syntax exists for them) are
/// populated from the JSON sidecar or by the expansion workflow:
/// - `plan_id`, `external_schedule_id`, `external_occurrence_key`
///
/// `predecessors` carries raw DSL references for file round-trips;
/// call [`Action::depends_on`] to get resolved UUIDs for graph/CRDT work.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Action {
    pub id: Uuid,
    /// Parent action ID for hierarchical nesting (`>` depth marker in DSL).
    pub parent_id: Option<Uuid>,
    /// Current lifecycle state.
    pub state: ActionState,
    pub name: String,
    pub description: Option<String>,
    pub priority: Option<u32>,
    /// Associated context tags (`+tag` in DSL).
    pub contexts: Option<Vec<String>>,
    /// Scheduled do-date/time (`@datetime` in DSL).
    pub scheduled_at: Option<DateTime<Local>>,
    /// Expected duration in minutes (`D30` in DSL).
    pub duration: Option<u32>,
    /// Deadline or due date (`:datetime` in DSL).
    pub due_date: Option<DateTime<Local>>,
    /// Completion timestamp (`%datetime` in DSL).
    pub completed_at: Option<DateTime<Local>>,
    /// Creation timestamp (`^datetime` in DSL).
    pub created_at: Option<DateTime<Local>>,
    /// Predecessor references (`<ref` in DSL). Raw text + resolved UUID.
    /// Use [`Action::depends_on`] for resolved UUIDs.
    pub predecessors: Option<Vec<PredecessorRef>>,
    /// Charter hint (`*charter` in DSL). Informational; charter membership
    /// is canonical in [`Charter::actions`].
    pub charter: Option<String>,
    pub alias: Option<String>,
    pub is_sequential: Option<bool>,
    /// The [`Plan`] that prescribed this action (populated from sidecar, not DSL).
    pub plan_id: Option<Uuid>,
    /// External schedule-series identifier (from ICS VEVENT.UID via sidecar).
    pub external_schedule_id: Option<String>,
    /// External occurrence identifier within a schedule series (from sidecar).
    pub external_occurrence_key: Option<String>,
}

impl Default for Action {
    fn default() -> Self {
        Self {
            id: Uuid::now_v7(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: String::new(),
            description: None,
            priority: None,
            contexts: None,
            scheduled_at: None,
            duration: None,
            due_date: None,
            completed_at: None,
            created_at: None,
            predecessors: None,
            charter: None,
            alias: None,
            is_sequential: None,
            plan_id: None,
            external_schedule_id: None,
            external_occurrence_key: None,
        }
    }
}

impl Action {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into(), ..Default::default() }
    }

    /// Resolved predecessor UUIDs. Used by graph and CRDT layers.
    /// Unresolved references (name-only) are silently skipped.
    pub fn depends_on(&self) -> Vec<Uuid> {
        self.predecessors
            .as_ref()
            .map(|preds| preds.iter().filter_map(|p| p.resolved_uuid).collect())
            .unwrap_or_default()
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

    /// Flatten all actions across all charters.
    pub fn all_actions(&self) -> Vec<&Action> {
        self.charters.iter().flat_map(|c| c.actions.iter()).collect()
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

    /// Find all Actions for a given Plan.
    pub fn actions_for_plan(&self, plan_id: Uuid) -> Vec<&Action> {
        self.all_actions()
            .into_iter()
            .filter(|a| a.plan_id == Some(plan_id))
            .collect()
    }

    /// Get all incomplete acts across the hierarchy.
    pub fn incomplete_actions(&self) -> Vec<&Action> {
        self.all_actions()
            .into_iter()
            .filter(|a| !matches!(a.state, ActionState::Completed | ActionState::Cancelled))
            .collect()
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
}
