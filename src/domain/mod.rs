//! Domain model aligned with the Actions Vocabulary v4 ontology.
//!
//! The hierarchy is: [`Objective`] → [`Charter`] → [`Plan`] / [`Action`]
//!
//! - [`Plan`] — schedule definition (RRULE + DTSTART). Produces [`Action`]s via expansion.
//! - [`Action`] — atomic executable work item. Carries all execution state.
//! - [`ActionState`] — lifecycle enum that inheres in an [`Action`].
//! - [`CharterState`] — lifecycle enum for the archival workflow on [`Charter`]s.
//!
//! "Do laundry weekly" is one [`Plan`]; each week's laundry becomes a separate [`Action`].
//! Non-recurring work can exist directly as an [`Action`] without any [`Plan`].

pub mod diff;
pub mod filter;

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

/// String-based reference to an action, charter, or plan.
///
/// Resolution order: [`UUID`](Reference::UUID) → [`Prefix`](Reference::Prefix)
/// (short UUID) → [`Alias`](Reference::Alias) → [`Name`](Reference::Name).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Reference {
    /// Full UUID.
    UUID(Uuid),
    /// Short UUID prefix (first 8 hex chars).
    Prefix(String),
    /// Human-readable name (case-insensitive substring match).
    Name(String),
    /// Exact alias match.
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
/// Charters move through a simple workflow. `Closed` and `Cancelled` are
/// both terminal — either is a precondition for archival, and the charter
/// and all its artifacts are swept into `archive.ttl` once one of them is
/// set (see [`CharterState::is_terminal`]).
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
    /// Abandoned without completion; ready to be archived.
    Cancelled,
}

impl CharterState {
    /// Whether this state is a precondition for archival (`Closed` or `Cancelled`).
    pub fn is_terminal(&self) -> bool {
        matches!(self, CharterState::Closed | CharterState::Cancelled)
    }
}

impl std::fmt::Display for CharterState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CharterState::New => write!(f, "New"),
            CharterState::Active => write!(f, "Active"),
            CharterState::Blocked => write!(f, "Blocked"),
            CharterState::Closed => write!(f, "Closed"),
            CharterState::Cancelled => write!(f, "Cancelled"),
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
    /// Stable identifier. Deterministic v5 UUID for implicit charters; v7 for created ones.
    pub id: Uuid,
    /// Human-readable name. Used in headings, CLI output, and SPARQL `rdfs:label`.
    pub title: String,
    pub description: Option<String>,
    /// Short identifier for CLI queries and DSL references (`*alias`).
    pub alias: Option<String>,
    /// Reference to the parent charter — its alias or UUID (never its title),
    /// per the reference-syntax spec. Usually derived from directory placement
    /// at workspace load time. `None` or empty means this is a root charter.
    pub parent: Option<String>,
    /// Titles of associated [`Objective`]s. Resolved at workspace load time.
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

impl Charter {
    /// Whether this charter is a hierarchy root — it declares no parent, or an
    /// empty one. Combine with an iterator to collect roots:
    /// `model.charters.iter().filter(|c| c.is_root())`.
    pub fn is_root(&self) -> bool {
        self.parent.as_deref().is_none_or(|p| p.trim().is_empty())
    }

    /// Whether this charter is a direct child of `parent`.
    ///
    /// Resolves this charter's `parent` reference against the candidate's
    /// alias, full UUID, or 8-char short prefix — case-insensitively, per the
    /// reference-syntax spec. Titles are never matched. Combine with a filter
    /// to collect a parent's children:
    /// `model.charters.iter().filter(|c| c.is_child_of(parent))`.
    pub fn is_child_of(&self, parent: &Charter) -> bool {
        let parent_ref = match self.parent.as_deref() {
            Some(v) => v.trim().to_lowercase(),
            None => return false,
        };
        if parent_ref.is_empty() {
            return false;
        }
        if let Some(alias) = &parent.alias {
            if parent_ref == alias.to_lowercase() {
                return true;
            }
        }
        // UUID reference: a full match, or a hex prefix of at least 4 chars per
        // the reference-syntax spec. Prefixes may run past 8 to disambiguate
        // IDs minted together (v7 timestamp prefixes, v5 namespacing).
        let parent_id = parent.id.to_string();
        if parent_ref == parent_id {
            return true;
        }
        parent_ref.len() >= 4
            && parent_ref.bytes().all(|b| b.is_ascii_hexdigit())
            && parent_id.replace('-', "").starts_with(&parent_ref)
    }
}

/// A [`Charter`] annotated with its source workspace.
///
/// Used only at the query/display layer when multiple workspaces are loaded
/// together. `DomainModel` stays with plain `Charter`; this type is constructed
/// by the caller once it has provenance context.
#[derive(Debug, Clone, PartialEq)]
pub struct WorkspaceCharter {
    /// The underlying charter.
    pub inner: Charter,
    /// Name of the workspace this charter was loaded from (`None` for the primary workspace).
    pub workspace: Option<String>,
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
    /// Merge-base copy of `scheduled_at` at last reconcile — the **B** column of
    /// the three-way sync (decision 31). Sidecar-populated, never from DSL;
    /// `None` means "not yet synced". Only the reconcile engine may move it.
    pub scheduled_at_sync: Option<DateTime<Local>>,
    /// Merge-base copy of `due_date` at last reconcile — the **B** column for the
    /// deadline. Sidecar-populated; see `scheduled_at_sync`.
    pub due_date_sync: Option<DateTime<Local>>,
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
            scheduled_at_sync: None,
            due_date_sync: None,
        }
    }
}

impl Action {
    /// Construct a new action with a generated UUIDv7, `NotStarted` state, and all optional fields unset.
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

/// Collect the IDs of `root_id` and all its recursive descendants in `actions`.
pub fn collect_subtree_ids(actions: &[Action], root_id: Uuid) -> Vec<Uuid> {
    let mut ids = vec![root_id];
    let mut i = 0;
    while i < ids.len() {
        let parent = ids[i];
        for a in actions {
            if a.parent_id == Some(parent) && !ids.contains(&a.id) {
                ids.push(a.id);
            }
        }
        i += 1;
    }
    ids
}

/// Close `root_id` and its full subtree in `actions`: stamp `closing_state` and
/// `completed_at`, and detach each from its parent. Returns the closed subtree
/// (root first, then descendants in discovery order) for the caller to append
/// to the `.completed.actions` file — `actions` itself is untouched; removing
/// the closed ids from the open list is the caller's job.
///
/// `completed_at` is stamped for `closing_state == Cancelled` too: there is no
/// separate `cancelled_at` field yet (tracked by the action-lifecycle charter,
/// which this was originally sequenced after). Revisit when that lands.
pub fn close_subtree(
    actions: &[Action],
    root_id: Uuid,
    closing_state: ActionState,
    now: DateTime<Local>,
) -> Vec<Action> {
    let subtree_ids = collect_subtree_ids(actions, root_id);
    actions
        .iter()
        .filter(|a| subtree_ids.contains(&a.id))
        .map(|a| {
            let mut closed = a.clone();
            closed.state = closing_state;
            closed.completed_at = Some(now);
            closed.parent_id = None;
            closed
        })
        .collect()
}

/// Hierarchical domain model: Objectives → Charters → Plans → Actions.
///
/// Each charter contains its plans, and each plan contains its actions.
/// This structure reflects the ontology hierarchy and eliminates the
/// need for separate flat HashMaps.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DomainModel {
    /// Top-level objectives (rarely populated; most workspaces use charters directly).
    pub objectives: Vec<Objective>,
    /// All charters in the loaded workspace, each containing its plans and actions.
    pub charters: Vec<Charter>,
}

impl DomainModel {
    /// Construct an empty model.
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

    /// Get all incomplete actions across the hierarchy.
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

    fn charter_with(alias: Option<&str>, parent: Option<&str>) -> Charter {
        Charter {
            id: Uuid::now_v7(),
            title: "Some Title".to_string(),
            alias: alias.map(String::from),
            parent: parent.map(String::from),
            ..Default::default()
        }
    }

    #[test]
    fn is_root_when_parent_absent_or_blank() {
        assert!(charter_with(None, None).is_root());
        assert!(charter_with(None, Some("   ")).is_root());
        assert!(!charter_with(None, Some("clearhead-cli")).is_root());
    }

    #[test]
    fn is_child_of_matches_parent_alias_case_insensitively() {
        let parent = charter_with(Some("clearhead-cli"), None);
        let child = charter_with(None, Some("CLEARHEAD-CLI"));
        assert!(child.is_child_of(&parent));
    }

    #[test]
    fn is_child_of_matches_parent_uuid_at_variable_prefix_lengths() {
        let parent = charter_with(Some("root"), None);
        let hex = parent.id.to_string().replace('-', "");
        // Full, and hex prefixes from the 4-char floor up past 8 (disambiguation).
        for candidate in [parent.id.to_string(), hex[..4].into(), hex[..8].into(), hex[..12].into()] {
            let child = charter_with(None, Some(&candidate));
            assert!(child.is_child_of(&parent), "should match ref {candidate}");
        }
        // Below the floor does not match.
        assert!(!charter_with(None, Some(&hex[..3])).is_child_of(&parent));
    }

    #[test]
    fn is_child_of_never_matches_title() {
        // Spec: titles are not used for reference resolution.
        let parent = charter_with(Some("root"), None); // title == "Some Title"
        let child = charter_with(None, Some("Some Title"));
        assert!(!child.is_child_of(&parent));
    }

    #[test]
    fn is_child_of_is_false_without_parent() {
        let parent = charter_with(Some("root"), None);
        assert!(!charter_with(None, None).is_child_of(&parent));
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
            recurrence: Some(recurrence),
            ..Default::default()
        };

        let occurrences = plan.expand_occurrences(dt_start, 10);

        assert_eq!(occurrences.len(), 3);
        assert_eq!(occurrences[0].format("%Y-%m-%d").to_string(), "2025-01-01");
        assert_eq!(occurrences[1].format("%Y-%m-%d").to_string(), "2025-01-02");
        assert_eq!(occurrences[2].format("%Y-%m-%d").to_string(), "2025-01-03");
    }

    fn action_with(name: &str, parent_id: Option<Uuid>) -> Action {
        Action { parent_id, ..Action::new(name) }
    }

    #[test]
    fn collect_subtree_ids_includes_root_and_descendants() {
        let root = action_with("root", None);
        let child = action_with("child", Some(root.id));
        let grandchild = action_with("grandchild", Some(child.id));
        let unrelated = action_with("unrelated", None);
        let actions = vec![root.clone(), child.clone(), grandchild.clone(), unrelated];

        let ids = collect_subtree_ids(&actions, root.id);

        assert_eq!(ids.len(), 3);
        assert!(ids.contains(&root.id));
        assert!(ids.contains(&child.id));
        assert!(ids.contains(&grandchild.id));
    }

    #[test]
    fn collect_subtree_ids_leaf_is_just_itself() {
        let leaf = action_with("leaf", None);
        let actions = vec![leaf.clone()];
        assert_eq!(collect_subtree_ids(&actions, leaf.id), vec![leaf.id]);
    }

    #[test]
    fn close_subtree_stamps_state_and_detaches_parent() {
        let root = action_with("root", None);
        let child = action_with("child", Some(root.id));
        let unrelated = action_with("unrelated", None);
        let actions = vec![root.clone(), child.clone(), unrelated.clone()];
        let now = Local::now();

        let closed = close_subtree(&actions, root.id, ActionState::Completed, now);

        assert_eq!(closed.len(), 2);
        assert!(closed.iter().all(|a| a.state == ActionState::Completed));
        assert!(closed.iter().all(|a| a.completed_at == Some(now)));
        assert!(closed.iter().all(|a| a.parent_id.is_none()));
        assert!(closed.iter().any(|a| a.id == root.id));
        assert!(closed.iter().any(|a| a.id == child.id));
        assert!(!closed.iter().any(|a| a.id == unrelated.id));
    }

    #[test]
    fn close_subtree_stamps_completed_at_for_cancelled_too() {
        // No separate cancelled_at field yet (action-lifecycle charter, not landed) —
        // Cancelled reuses completed_at, matching the CLI's prior inline behavior.
        let root = action_with("root", None);
        let actions = vec![root.clone()];
        let now = Local::now();

        let closed = close_subtree(&actions, root.id, ActionState::Cancelled, now);

        assert_eq!(closed[0].state, ActionState::Cancelled);
        assert_eq!(closed[0].completed_at, Some(now));
    }

    #[test]
    fn close_subtree_leaves_source_actions_untouched() {
        let root = action_with("root", None);
        let actions = vec![root.clone()];
        let _ = close_subtree(&actions, root.id, ActionState::Completed, Local::now());
        assert_eq!(actions[0].state, ActionState::NotStarted);
        assert_eq!(actions[0].parent_id, None);
    }
}
