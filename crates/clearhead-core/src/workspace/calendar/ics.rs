//! ICS schedule file parser and exporter.
//!
//! **Parse direction** (`ics → domain`): non-override VEVENT and VTODO
//! components become one-off or recurring [`Plan`]s. `RRULE` changes Plan
//! cardinality; it does not classify the component as a different domain type.
//!
//! **Export direction** (`domain → ics`): converts [`Plan`]s through the
//! configured VEVENT or VTODO codec. Legacy [`Action`] VTODO helpers remain
//! temporarily available only to the explicit migration/sync transition.

use crate::config::PlanComponentKind;
use crate::domain::{Action, ActionState, Plan, Recurrence};
use crate::workspace::store::WorkspaceError;
use chrono::{DateTime, Local, NaiveDate, NaiveDateTime, TimeZone, Utc};
use icalendar::{
    Calendar, CalendarComponent, CalendarDateTime, Component, DatePerhapsTime, Event, EventLike,
    Property, Todo, TodoStatus,
};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use uuid::{Uuid, uuid};

/// A parsed `.ics` file entry — the workspace-layer representation of a scheduled plan.
///
/// Carries the source file path and the RFC 5545 deviations (`EXDATE`,
/// `RECURRENCE-ID` overrides) alongside the domain [`Plan`]. Deviations are an
/// iCalendar *projection* concern and deliberately live here, not on the pure
/// domain [`Plan`]: occurrences are rendered as `master + deviations` at the
/// workspace boundary, and `.plan` alone stays iCalendar-ignorant.
#[derive(Debug, Clone)]
pub struct ICSPlan {
    pub path: PathBuf,
    pub plan: Plan,
    /// Observed wire codec. Mutation follows the resource being patched rather
    /// than assuming the workspace's current configured codec during migration.
    pub component_kind: PlanComponentKind,
    /// Canonical keys of occurrence slots excluded from the series (`EXDATE`).
    pub exdates: BTreeSet<String>,
    /// Per-occurrence overrides (`RECURRENCE-ID` VTODOs), keyed by the same
    /// canonical occurrence key as [`exdates`](Self::exdates) and the
    /// occurrence UUIDv5 — see [`canonical_occurrence_key`].
    pub overrides: BTreeMap<String, OccurrenceOverride>,
}

/// One materialized deviation of a recurring [`Plan`] occurrence: the renderable
/// fields carried by a `RECURRENCE-ID` VTODO that replaces the occurrence at a
/// single slot. `None` fields inherit from the master when the occurrence is
/// rendered. Predecessors, hierarchy, and other ClearHead-only structure have no
/// per-occurrence meaning and are intentionally absent.
#[derive(Debug, Clone, PartialEq)]
pub struct OccurrenceOverride {
    pub scheduled_at: Option<DateTime<Local>>,
    pub due_date: Option<DateTime<Local>>,
    pub state: ActionState,
    pub completed_at: Option<DateTime<Local>>,
    pub title: Option<String>,
    pub description: Option<String>,
}

/// Namespace UUID for deriving deterministic Plan and occurrence identities.
const ICS_NAMESPACE: Uuid = uuid!("6ba7b810-9dad-11d1-80b4-00c04fd430c8");
/// Separate namespace for adopting calendar-created standalone VTODOs whose
/// RFC 5545 UID is valid text but not itself a UUID.
const VTODO_ACTION_NAMESPACE: Uuid = uuid!("87ca0d84-1793-5b27-91f4-607bf8d38f87");

/// Derive the stable domain Plan ID for an iCalendar UID.
pub fn plan_id_from_ics_uid(uid: &str) -> uuid::Uuid {
    Uuid::new_v5(&ICS_NAMESPACE, uid.as_bytes())
}

/// Derive a deterministic UUID for a generated action from its schedule identity and occurrence key.
///
/// Per the ICS schedule spec: UUID v5 from `(externalScheduleId, externalOccurrenceKey)`.
/// Running expansion multiple times with the same inputs always yields the same UUID.
pub fn occurrence_action_id(plan_uid: &str, occurrence_key: &str) -> uuid::Uuid {
    let key = format!("{}:{}", plan_uid, occurrence_key);
    uuid::Uuid::new_v5(&ICS_NAMESPACE, key.as_bytes())
}

/// Canonical, peer-stable key for one recurrence occurrence slot.
///
/// This is the single seam that occurrence identity ([`occurrence_action_id`])
/// and every deviation lookup ([`ICSPlan::exdates`] / [`ICSPlan::overrides`])
/// share, so a slot maps to one handle no matter which peer formed it. RFC 5545
/// lets peers emit the same slot as UTC, TZID-local, or floating; collapsing to
/// a single absolute-instant frame (UTC) makes the UTC and TZID forms agree.
/// Floating values — which name no fixed instant — are the acknowledged edge.
///
/// Hashing the immutable *slot* (never the occurrence's mutable DUE) is the
/// invariant: a reschedule must not mint a new identity.
pub fn canonical_occurrence_key(slot: DateTime<Local>) -> String {
    slot.with_timezone(&Utc)
        .format("%Y%m%dT%H%M%SZ")
        .to_string()
}

/// Resolve a standalone VTODO UID to its Action identity. ClearHead-authored
/// UUID UIDs remain unchanged; arbitrary client-generated UIDs get a stable
/// UUIDv5 without rewriting the interoperable UID.
pub fn action_id_from_vtodo_uid(uid: &str) -> Uuid {
    Uuid::parse_str(uid).unwrap_or_else(|_| Uuid::new_v5(&VTODO_ACTION_NAMESPACE, uid.as_bytes()))
}

/// Parse Plan resources from bytes already supplied by a host.
///
/// Every non-override VEVENT or VTODO is a Plan, whether one-off or recurring.
/// A UID may have exactly one master and all of its overrides must use that
/// master's component kind; ambiguous mixed resources are rejected before any
/// caller can select a traversal-order winner.
///
/// `logical_path` is provenance only; parsing never reads it.
pub fn parse_ics(content: &str, logical_path: &Path) -> Result<Vec<ICSPlan>, WorkspaceError> {
    let calendar: Calendar = content
        .parse()
        .map_err(|e: String| WorkspaceError::Parse(e))?;

    validate_plan_component_identities(&calendar, logical_path)?;

    let events: Vec<&Event> = calendar
        .components
        .iter()
        .filter_map(|component| match component {
            CalendarComponent::Event(event) => Some(event),
            _ => None,
        })
        .collect();
    let todos: Vec<&Todo> = calendar
        .components
        .iter()
        .filter_map(|component| match component {
            CalendarComponent::Todo(todo) => Some(todo),
            _ => None,
        })
        .collect();

    let mut plans = Vec::new();
    for event in events
        .iter()
        .copied()
        .filter(|event| event.property_value("RECURRENCE-ID").is_none())
    {
        let Some(mut ics_plan) = component_to_plan(event, logical_path, PlanComponentKind::VEvent)
        else {
            continue;
        };
        let master_uid = event.get_uid();
        ics_plan.exdates = parse_exdates(event);
        ics_plan.overrides = events
            .iter()
            .copied()
            .filter(|other| {
                other.property_value("RECURRENCE-ID").is_some() && other.get_uid() == master_uid
            })
            .filter_map(override_from_event)
            .collect();
        plans.push(ics_plan);
    }

    for todo in todos
        .iter()
        .copied()
        .filter(|todo| todo.property_value("RECURRENCE-ID").is_none())
    {
        let Some(mut ics_plan) = component_to_plan(todo, logical_path, PlanComponentKind::VTodo)
        else {
            continue;
        };
        let master_uid = todo.get_uid();
        ics_plan.exdates = parse_exdates(todo);
        ics_plan.overrides = todos
            .iter()
            .copied()
            .filter(|other| {
                other.property_value("RECURRENCE-ID").is_some() && other.get_uid() == master_uid
            })
            .filter_map(override_from_todo)
            .collect();
        plans.push(ics_plan);
    }

    Ok(plans)
}

fn validate_plan_component_identities(
    calendar: &Calendar,
    logical_path: &Path,
) -> Result<(), WorkspaceError> {
    let mut masters: BTreeMap<String, Vec<PlanComponentKind>> = BTreeMap::new();
    let mut overrides: Vec<(String, PlanComponentKind)> = Vec::new();

    for component in &calendar.components {
        let (uid, kind, recurrence_id) = match component {
            CalendarComponent::Event(event) => (
                event.get_uid(),
                PlanComponentKind::VEvent,
                event.property_value("RECURRENCE-ID"),
            ),
            CalendarComponent::Todo(todo) => (
                todo.get_uid(),
                PlanComponentKind::VTodo,
                todo.property_value("RECURRENCE-ID"),
            ),
            _ => continue,
        };
        let Some(uid) = uid else { continue };
        if recurrence_id.is_some() {
            overrides.push((uid.to_string(), kind));
        } else {
            masters.entry(uid.to_string()).or_default().push(kind);
        }
    }

    for (uid, kinds) in &masters {
        if kinds.len() > 1 {
            let observed = kinds
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ");
            return Err(WorkspaceError::Parse(format!(
                "{} contains duplicate Plan masters for UID {uid}: {observed}",
                logical_path.display()
            )));
        }
    }
    for (uid, override_kind) in overrides {
        if let Some(master_kinds) = masters.get(&uid)
            && master_kinds.first() != Some(&override_kind)
        {
            return Err(WorkspaceError::Parse(format!(
                "{} mixes {} override with {} master for Plan UID {uid}",
                logical_path.display(),
                override_kind,
                master_kinds[0]
            )));
        }
    }
    Ok(())
}

/// Build an [`ICSPlan`] from a Plan component's shared fields.
/// Returns `None` if required Plan identity, label, or DTSTART is missing.
fn component_to_plan<T: Component>(
    component: &T,
    path: &Path,
    component_kind: PlanComponentKind,
) -> Option<ICSPlan> {
    let uid = component.get_uid()?;
    let summary = component.get_summary()?;

    let plan_id = plan_id_from_ics_uid(uid);
    let dtstart = parse_dtstart(component)?;
    let recurrence = component
        .property_value("RRULE")
        .and_then(Recurrence::from_rrule_str);

    let (template_name, description) = component
        .get_description()
        .map(parse_description_directives)
        .unwrap_or((None, None));

    Some(ICSPlan {
        path: path.to_path_buf(),
        component_kind,
        exdates: BTreeSet::new(),
        overrides: BTreeMap::new(),
        plan: Plan {
            id: plan_id,
            name: summary.to_string(),
            description,
            recurrence,
            dtstart: Some(dtstart),
            external_id: Some(uid.to_string()),
            template_name,
            ..Default::default()
        },
    })
}

/// Parse directives and description body from a Plan component DESCRIPTION.
///
/// Leading `key: value` lines are consumed as directives until a blank line
/// or a non-matching line is encountered. The remainder is the description body.
///
/// Supported directives:
/// - `template: <name>` — binds a template for structural instantiation
///
/// Returns `(template_name, description)`.
fn parse_description_directives(desc: &str) -> (Option<String>, Option<String>) {
    let mut template: Option<String> = None;
    let mut body_start = 0usize;

    for line in desc.lines() {
        if line.trim().is_empty() {
            body_start += line.len() + 1;
            break;
        }
        if let Some(val) = line.strip_prefix("template: ") {
            let val = val.trim();
            if !val.is_empty() {
                template = Some(val.to_string());
            }
            body_start += line.len() + 1;
        } else {
            // Non-directive line — everything from here is description body
            break;
        }
    }

    let rest = if body_start < desc.len() {
        let s = desc[body_start..].trim();
        if s.is_empty() {
            None
        } else {
            Some(s.to_string())
        }
    } else {
        None
    };

    (template, rest)
}

fn parse_dtstart<T: Component>(component: &T) -> Option<DateTime<Local>> {
    date_perhaps_time_to_local(component.get_start()?)
}

/// Convert every RFC 5545 date form accepted by the parser into ClearHead's
/// local-time domain representation. UTC instants remain exact; floating and
/// all-day values intentionally use the machine's local zone; IANA TZIDs are
/// resolved in their declared zone before conversion. Unknown/custom TZIDs
/// are rejected rather than silently interpreted in the wrong zone.
fn date_perhaps_time_to_local(value: DatePerhapsTime) -> Option<DateTime<Local>> {
    match value {
        DatePerhapsTime::DateTime(CalendarDateTime::Floating(naive)) => {
            Local.from_local_datetime(&naive).earliest()
        }
        DatePerhapsTime::DateTime(CalendarDateTime::Utc(utc)) => Some(utc.with_timezone(&Local)),
        DatePerhapsTime::DateTime(value @ CalendarDateTime::WithTimezone { .. }) => {
            value.try_into_utc().map(|utc| utc.with_timezone(&Local))
        }
        DatePerhapsTime::Date(naive_date) => Local
            .from_local_datetime(&naive_date.and_hms_opt(0, 0, 0)?)
            .earliest(),
    }
}

/// Parse one raw RFC 5545 date-time token (as found in an `EXDATE` or
/// `RECURRENCE-ID` value) into local time. Handles UTC (`…Z`), floating
/// date-time, and all-day `DATE` forms; a `TZID` parameter on the property is
/// not yet resolved here (our own emit uses UTC) and is left for the
/// interoperability-hardening pass.
fn parse_ics_datetime_token(token: &str) -> Option<DateTime<Local>> {
    let token = token.trim();
    if let Some(utc) = token.strip_suffix('Z') {
        return NaiveDateTime::parse_from_str(utc, "%Y%m%dT%H%M%S")
            .ok()
            .map(|naive| Utc.from_utc_datetime(&naive).with_timezone(&Local));
    }
    if let Ok(naive) = NaiveDateTime::parse_from_str(token, "%Y%m%dT%H%M%S") {
        return Local.from_local_datetime(&naive).earliest();
    }
    let date = NaiveDate::parse_from_str(token, "%Y%m%d").ok()?;
    Local
        .from_local_datetime(&date.and_hms_opt(0, 0, 0)?)
        .earliest()
}

/// Read a master's `EXDATE` slots as canonical occurrence keys. `EXDATE` may be
/// a single comma-joined property or repeated; both are unioned.
fn parse_exdates<T: Component>(component: &T) -> BTreeSet<String> {
    let single = component.properties().get("EXDATE").into_iter();
    let repeated = component
        .multi_properties()
        .get("EXDATE")
        .into_iter()
        .flatten();
    single
        .chain(repeated)
        .flat_map(|property| property.value().split(','))
        .filter_map(parse_ics_datetime_token)
        .map(canonical_occurrence_key)
        .collect()
}

/// Build one schedule-only occurrence override from a `RECURRENCE-ID` VEVENT.
fn override_from_event(event: &Event) -> Option<(String, OccurrenceOverride)> {
    let slot = parse_ics_datetime_token(event.property_value("RECURRENCE-ID")?)?;
    let over = OccurrenceOverride {
        scheduled_at: event.get_start().and_then(date_perhaps_time_to_local),
        due_date: event.get_end().and_then(date_perhaps_time_to_local),
        state: ActionState::NotStarted,
        completed_at: None,
        title: event.get_summary().map(str::to_string),
        description: event
            .get_description()
            .filter(|value| !value.is_empty())
            .map(str::to_string),
    };
    Some((canonical_occurrence_key(slot), over))
}

/// Build one occurrence override from a `RECURRENCE-ID` VTODO. Returns `None`
/// when the component carries no parseable `RECURRENCE-ID` — without a slot it
/// cannot be keyed to an occurrence.
fn override_from_todo(todo: &Todo) -> Option<(String, OccurrenceOverride)> {
    let slot = parse_ics_datetime_token(todo.property_value("RECURRENCE-ID")?)?;
    let over = OccurrenceOverride {
        scheduled_at: todo.get_start().and_then(date_perhaps_time_to_local),
        due_date: todo.get_due().and_then(date_perhaps_time_to_local),
        state: vtodo_state(todo),
        completed_at: todo
            .get_completed()
            .map(|value| value.with_timezone(&Local)),
        title: todo.get_summary().map(str::to_string),
        description: todo
            .get_description()
            .filter(|value| !value.is_empty())
            .map(str::to_string),
    };
    Some((canonical_occurrence_key(slot), over))
}

/// Derive [`ActionState`] from a VTODO's status, honoring ClearHead's
/// `X-CLEARHEAD-STATUS:blocked` extension and the percent-complete / `COMPLETED`
/// fallbacks. Shared by standalone-action and occurrence-override parsing.
fn vtodo_state(todo: &Todo) -> ActionState {
    let standard_status = todo.get_status();
    let blocked = matches!(standard_status, Some(TodoStatus::NeedsAction) | None)
        && todo
            .property_value("X-CLEARHEAD-STATUS")
            .is_some_and(|value| value.eq_ignore_ascii_case("blocked"));
    if blocked {
        return ActionState::BlockedOrAwaiting;
    }
    match standard_status {
        Some(TodoStatus::InProcess) => ActionState::InProgress,
        Some(TodoStatus::Completed) => ActionState::Completed,
        Some(TodoStatus::Cancelled) => ActionState::Cancelled,
        Some(TodoStatus::NeedsAction) | None => {
            if todo.get_percent_complete() == Some(100) || todo.get_completed().is_some() {
                ActionState::Completed
            } else {
                ActionState::NotStarted
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Deviation write path (the mirror of parse_exdates / override_from_todo)
// ---------------------------------------------------------------------------

/// An operation recorded against one occurrence slot of a recurring master, as
/// an RFC 5545 deviation. This is how a *projected* occurrence — which has no
/// `.actions` line to edit — is acted on: the change lands in the single master
/// resource, never as a materialized instance.
#[derive(Debug, Clone, PartialEq)]
pub enum OccurrenceOp {
    /// Drop the slot from the series (`EXDATE`). It stops rendering entirely.
    Skip,
    /// Mark the slot completed at `at`. VTODO projects this as a completed
    /// `RECURRENCE-ID` override for task-client compatibility; VEVENT leaves
    /// completion entirely to the Action/archive lifecycle.
    Complete { at: DateTime<Local> },
    /// Move the slot to new times (a `RECURRENCE-ID` override). `None` clears
    /// that field on the override, inheriting nothing further from the master.
    Reschedule {
        scheduled_at: Option<DateTime<Local>>,
        due_date: Option<DateTime<Local>>,
    },
}

/// Render one occurrence deviation from host-supplied calendar bytes.
///
/// Mutation follows the observed master codec so an alternate-codec resource
/// remains safely editable during migration. Completion is Action-owned:
/// VTODO may project it for task-client compatibility, while VEVENT bytes stay
/// unchanged.
pub fn render_occurrence_deviation(
    content: &str,
    master_uid: &str,
    occurrence_key: &str,
    op: &OccurrenceOp,
) -> Result<String, WorkspaceError> {
    let mut calendar: Calendar = content.parse().map_err(WorkspaceError::Parse)?;
    validate_plan_component_identities(&calendar, Path::new("calendar resource"))?;
    let (_, component_kind) = recurring_master(&calendar, master_uid)?;

    match op {
        OccurrenceOp::Skip => add_exdate(&mut calendar, master_uid, occurrence_key)?,
        OccurrenceOp::Complete { at } => {
            if component_kind == PlanComponentKind::VEvent {
                return Ok(content.to_string());
            }
            let at = *at;
            upsert_todo_override(&mut calendar, master_uid, occurrence_key, |todo| {
                todo.status(TodoStatus::Completed);
                todo.completed(at.with_timezone(&Utc));
            })?;
        }
        OccurrenceOp::Reschedule {
            scheduled_at,
            due_date,
        } => {
            let (scheduled_at, due_date) = (*scheduled_at, *due_date);
            match component_kind {
                PlanComponentKind::VEvent => {
                    upsert_event_override(&mut calendar, master_uid, occurrence_key, |event| {
                        event.remove_starts();
                        if let Some(value) = scheduled_at {
                            event.starts(value.with_timezone(&Utc));
                        }
                        event.remove_ends();
                        if let Some(value) = due_date {
                            event.ends(value.with_timezone(&Utc));
                        }
                    })?;
                }
                PlanComponentKind::VTodo => {
                    upsert_todo_override(&mut calendar, master_uid, occurrence_key, |todo| {
                        todo.remove_starts();
                        if let Some(value) = scheduled_at {
                            todo.starts(value.with_timezone(&Utc));
                        }
                        todo.remove_due();
                        if let Some(value) = due_date {
                            todo.due(value.with_timezone(&Utc));
                        }
                    })?;
                }
            }
        }
    }

    Ok(calendar.to_string())
}

/// Render the VTODO-specific foreign roll-forward compatibility shape.
pub fn render_master_rollforward(
    content: &str,
    master_uid: &str,
    base_dtstart: DateTime<Local>,
    completed_slots: &[(String, DateTime<Local>)],
) -> Result<String, WorkspaceError> {
    let mut calendar: Calendar = content.parse().map_err(WorkspaceError::Parse)?;
    validate_plan_component_identities(&calendar, Path::new("calendar resource"))?;

    let (index, component_kind) = recurring_master(&calendar, master_uid)?;
    if component_kind != PlanComponentKind::VTodo {
        return Err(WorkspaceError::Parse(format!(
            "master roll-forward compatibility requires VTODO Plan {master_uid}"
        )));
    }
    let CalendarComponent::Todo(master) = &mut calendar.components[index] else {
        unreachable!("recurring_master returned the observed component kind")
    };
    master.remove_starts();
    master.starts(base_dtstart.with_timezone(&Utc));

    // Record each passed slot as completed, skipping any slot that already carries
    // an override so re-detecting the same advance records nothing new.
    for (key, completed_at) in completed_slots {
        if override_index(&calendar, master_uid, key, PlanComponentKind::VTodo).is_some() {
            continue;
        }
        let completed_at = *completed_at;
        upsert_todo_override(&mut calendar, master_uid, key, |todo| {
            todo.status(TodoStatus::Completed);
            todo.completed(completed_at.with_timezone(&Utc));
        })?;
    }

    Ok(calendar.to_string())
}

/// Locate exactly one recurring Plan master and report its observed codec.
fn recurring_master(
    calendar: &Calendar,
    uid: &str,
) -> Result<(usize, PlanComponentKind), WorkspaceError> {
    let matches = calendar
        .components
        .iter()
        .enumerate()
        .filter_map(|(index, component)| match component {
            CalendarComponent::Event(event)
                if event.get_uid() == Some(uid)
                    && event.property_value("RRULE").is_some()
                    && event.property_value("RECURRENCE-ID").is_none() =>
            {
                Some((index, PlanComponentKind::VEvent))
            }
            CalendarComponent::Todo(todo)
                if todo.get_uid() == Some(uid)
                    && todo.property_value("RRULE").is_some()
                    && todo.property_value("RECURRENCE-ID").is_none() =>
            {
                Some((index, PlanComponentKind::VTodo))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    match matches.as_slice() {
        [found] => Ok(*found),
        [] => Err(WorkspaceError::Parse(format!(
            "recurring Plan master {uid} not found"
        ))),
        _ => Err(WorkspaceError::Parse(format!(
            "recurring Plan master {uid} is ambiguous"
        ))),
    }
}

fn add_exdate(calendar: &mut Calendar, uid: &str, key: &str) -> Result<(), WorkspaceError> {
    let (index, component_kind) = recurring_master(calendar, uid)?;
    match (&mut calendar.components[index], component_kind) {
        (CalendarComponent::Event(master), PlanComponentKind::VEvent) => {
            if !parse_exdates(master).contains(key) {
                master.add_multi_property("EXDATE", key);
            }
        }
        (CalendarComponent::Todo(master), PlanComponentKind::VTodo) => {
            if !parse_exdates(master).contains(key) {
                master.add_multi_property("EXDATE", key);
            }
        }
        _ => unreachable!("recurring_master returned the observed component kind"),
    }
    Ok(())
}

fn override_index(
    calendar: &Calendar,
    uid: &str,
    key: &str,
    component_kind: PlanComponentKind,
) -> Option<usize> {
    calendar
        .components
        .iter()
        .position(|component| match (component, component_kind) {
            (CalendarComponent::Event(event), PlanComponentKind::VEvent) => {
                event.get_uid() == Some(uid)
                    && event
                        .property_value("RECURRENCE-ID")
                        .and_then(parse_ics_datetime_token)
                        .map(canonical_occurrence_key)
                        .as_deref()
                        == Some(key)
            }
            (CalendarComponent::Todo(todo), PlanComponentKind::VTodo) => {
                todo.get_uid() == Some(uid)
                    && todo
                        .property_value("RECURRENCE-ID")
                        .and_then(parse_ics_datetime_token)
                        .map(canonical_occurrence_key)
                        .as_deref()
                        == Some(key)
            }
            _ => false,
        })
}

fn master_summary(
    calendar: &Calendar,
    uid: &str,
    component_kind: PlanComponentKind,
) -> Result<Option<String>, WorkspaceError> {
    let (index, observed_kind) = recurring_master(calendar, uid)?;
    if observed_kind != component_kind {
        return Err(WorkspaceError::Parse(format!(
            "Plan {uid} is encoded as {observed_kind}, not {component_kind}"
        )));
    }
    Ok(match &calendar.components[index] {
        CalendarComponent::Event(master) => master.get_summary().map(str::to_string),
        CalendarComponent::Todo(master) => master.get_summary().map(str::to_string),
        _ => unreachable!("recurring_master only returns Plan components"),
    })
}

fn upsert_event_override(
    calendar: &mut Calendar,
    uid: &str,
    key: &str,
    patch: impl FnOnce(&mut Event),
) -> Result<(), WorkspaceError> {
    let summary = master_summary(calendar, uid, PlanComponentKind::VEvent)?;
    if let Some(index) = override_index(calendar, uid, key, PlanComponentKind::VEvent) {
        let CalendarComponent::Event(event) = &mut calendar.components[index] else {
            unreachable!()
        };
        patch(event);
    } else {
        let slot = parse_ics_datetime_token(key)
            .ok_or_else(|| WorkspaceError::Parse(format!("invalid occurrence key {key}")))?;
        let mut event = Event::new();
        event.uid(uid);
        event.add_property("RECURRENCE-ID", key);
        event.starts(slot.with_timezone(&Utc));
        if let Some(summary) = summary {
            event.summary(&summary);
        }
        patch(&mut event);
        calendar.push(event);
    }
    Ok(())
}

fn upsert_todo_override(
    calendar: &mut Calendar,
    uid: &str,
    key: &str,
    patch: impl FnOnce(&mut Todo),
) -> Result<(), WorkspaceError> {
    let summary = master_summary(calendar, uid, PlanComponentKind::VTodo)?;
    if let Some(index) = override_index(calendar, uid, key, PlanComponentKind::VTodo) {
        let CalendarComponent::Todo(todo) = &mut calendar.components[index] else {
            unreachable!()
        };
        patch(todo);
    } else {
        let slot = parse_ics_datetime_token(key)
            .ok_or_else(|| WorkspaceError::Parse(format!("invalid occurrence key {key}")))?;
        let mut todo = Todo::new();
        todo.uid(uid);
        todo.add_property("RECURRENCE-ID", key);
        todo.starts(slot.with_timezone(&Utc));
        if let Some(summary) = summary {
            todo.summary(&summary);
        }
        patch(&mut todo);
        calendar.push(todo);
    }
    Ok(())
}

/// The interoperable fields ClearHead owns in a standalone VTODO projection.
/// Transport metadata, alarms, and vendor extensions deliberately stay out of
/// this value and are preserved when an existing file is updated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VTodoAction {
    /// ClearHead Action identity, equal to `uid` when the UID is a UUID and
    /// deterministically derived otherwise.
    pub id: Uuid,
    /// Original RFC 5545 identity. Never rewritten merely to fit Action's UUID.
    pub uid: String,
    pub scheduled_at: Option<DateTime<Local>>,
    pub due_date: Option<DateTime<Local>>,
    pub state: ActionState,
    pub title: String,
    pub description: Option<String>,
    pub priority: Option<u32>,
    pub contexts: Option<Vec<String>>,
    pub completed_at: Option<DateTime<Local>>,
}

/// Parse standalone VTODO projections from host-supplied calendar bytes.
pub fn parse_vtodo_actions_content(content: &str) -> Result<Vec<VTodoAction>, WorkspaceError> {
    let calendar: Calendar = content
        .parse()
        .map_err(|e: String| WorkspaceError::Parse(e))?;
    let mut actions = Vec::new();

    for component in calendar.components {
        let CalendarComponent::Todo(todo) = component else {
            continue;
        };
        // A recurring master (RRULE) is a Plan, and a RECURRENCE-ID component is an
        // occurrence override *of* its master — neither is a standalone Action.
        if todo.property_value("RRULE").is_some() || todo.property_value("RECURRENCE-ID").is_some()
        {
            continue;
        }
        let Some(uid) = todo.get_uid() else {
            continue;
        };
        let id = action_id_from_vtodo_uid(uid);
        let Some(title) = todo.get_summary() else {
            continue;
        };

        actions.push(VTodoAction {
            id,
            uid: uid.to_string(),
            scheduled_at: todo.get_start().and_then(date_perhaps_time_to_local),
            due_date: todo.get_due().and_then(date_perhaps_time_to_local),
            state: vtodo_state(&todo),
            title: title.to_string(),
            description: todo
                .get_description()
                .filter(|value| !value.is_empty())
                .map(str::to_string),
            priority: todo.get_priority().filter(|value| (1..=9).contains(value)),
            contexts: parse_categories(&todo),
            completed_at: todo
                .get_completed()
                .map(|value| value.with_timezone(&Local)),
        });
    }

    Ok(actions)
}

fn parse_categories(todo: &Todo) -> Option<Vec<String>> {
    let single = todo.properties().get("CATEGORIES").into_iter();
    let multiple = todo
        .multi_properties()
        .get("CATEGORIES")
        .into_iter()
        .flatten();
    let mut categories = single
        .chain(multiple)
        .flat_map(|property| split_text_list(property.value()))
        .filter(|value| !value.is_empty())
        .collect::<Vec<_>>();
    categories.sort();
    categories.dedup();
    (!categories.is_empty()).then_some(categories)
}

/// Split an RFC 5545 comma-separated TEXT list and decode standard TEXT
/// escapes. ClearHead context names use commas as separators and therefore do
/// not represent a literal comma inside one category.
fn split_text_list(value: &str) -> Vec<String> {
    let mut values = vec![String::new()];
    let mut chars = value.chars();
    while let Some(ch) = chars.next() {
        match ch {
            '\\' => match chars.next() {
                Some('n' | 'N') => values.last_mut().unwrap().push('\n'),
                Some(next) => values.last_mut().unwrap().push(next),
                None => values.last_mut().unwrap().push('\\'),
            },
            ',' => values.push(String::new()),
            other => values.last_mut().unwrap().push(other),
        }
    }
    values
        .into_iter()
        .map(|value| value.trim().to_string())
        .collect()
}

// ============================================================================
// Export direction: domain → iCalendar string
// ============================================================================

/// Convert one recurring [`Plan`] to its canonical VTODO master.
/// Populate the fields shared by both Plan component codecs.
fn populate_plan_component<T: Component + EventLike>(component: &mut T, plan: &Plan) {
    component.remove_property("SUMMARY").summary(&plan.name);
    component.remove_starts();
    if let Some(dtstart) = plan.dtstart {
        component.starts(dtstart.with_timezone(&Utc));
    }
    component.remove_property("RRULE");
    if let Some(recurrence) = &plan.recurrence {
        let recurrence = recurrence.to_string();
        component.add_property(
            "RRULE",
            recurrence.strip_prefix("R:").unwrap_or(&recurrence),
        );
    }
    component.remove_description();
    let mut description = Vec::new();
    if let Some(template) = &plan.template_name {
        description.push(format!("template: {template}"));
    }
    if let Some(text) = &plan.description {
        description.push(text.clone());
    }
    if !description.is_empty() {
        component.description(&description.join("\n"));
    }
}

/// Convert one [`Plan`] to the VTODO Plan codec.
pub fn plan_to_vtodo(plan: &Plan) -> Todo {
    let mut todo = Todo::new();
    let uid = plan
        .external_id
        .clone()
        .unwrap_or_else(|| plan.id.to_string());
    todo.uid(&uid);
    populate_plan_component(&mut todo, plan);
    todo.done()
}

/// Convert one [`Plan`] to the default VEVENT Plan codec.
pub fn plan_to_vevent(plan: &Plan) -> Event {
    let mut event = Event::new();
    let uid = plan
        .external_id
        .clone()
        .unwrap_or_else(|| plan.id.to_string());
    event.uid(&uid);
    populate_plan_component(&mut event, plan);
    event.done()
}

/// Convert [`Plan`]s to an iCalendar document using the selected component.
pub fn plans_to_icalendar_with_component(
    plans: &[Plan],
    component_kind: PlanComponentKind,
) -> String {
    let mut calendar = Calendar::new()
        .name("ClearHead Plans")
        .description("Schedules managed by ClearHead")
        .done();

    for plan in plans {
        match component_kind {
            PlanComponentKind::VEvent => calendar.push(plan_to_vevent(plan)),
            PlanComponentKind::VTodo => calendar.push(plan_to_vtodo(plan)),
        };
    }

    calendar.to_string()
}

/// Convert Plans to the legacy VTODO representation.
///
/// New call sites should use [`plans_to_icalendar_with_component`].
pub fn plans_to_icalendar(plans: &[Plan]) -> String {
    plans_to_icalendar_with_component(plans, PlanComponentKind::VTodo)
}

/// Render one Plan resource while preserving unowned calendar content.
pub fn render_plan_resource_with_component(
    existing: Option<&str>,
    plan: &Plan,
    component_kind: PlanComponentKind,
) -> Result<String, WorkspaceError> {
    let Some(existing) = existing else {
        return Ok(plans_to_icalendar_with_component(
            std::slice::from_ref(plan),
            component_kind,
        ));
    };
    let mut calendar: Calendar = existing.parse().map_err(WorkspaceError::Parse)?;
    validate_plan_component_identities(&calendar, Path::new("calendar resource"))?;
    let uid = plan
        .external_id
        .clone()
        .unwrap_or_else(|| plan.id.to_string());
    let observed_kind = calendar
        .components
        .iter()
        .find_map(|component| match component {
            CalendarComponent::Event(event)
                if event.get_uid() == Some(uid.as_str())
                    && event.property_value("RECURRENCE-ID").is_none() =>
            {
                Some(PlanComponentKind::VEvent)
            }
            CalendarComponent::Todo(todo)
                if todo.get_uid() == Some(uid.as_str())
                    && todo.property_value("RECURRENCE-ID").is_none() =>
            {
                Some(PlanComponentKind::VTodo)
            }
            _ => None,
        });
    let Some(observed_kind) = observed_kind else {
        return Err(WorkspaceError::Parse(format!(
            "Plan {uid} not found in calendar resource"
        )));
    };

    match (observed_kind, component_kind) {
        (PlanComponentKind::VEvent, PlanComponentKind::VEvent) => {
            for component in &mut calendar.components {
                if let CalendarComponent::Event(event) = component
                    && event.get_uid() == Some(uid.as_str())
                    && event.property_value("RECURRENCE-ID").is_none()
                {
                    populate_plan_component(event, plan);
                }
            }
        }
        (PlanComponentKind::VTodo, PlanComponentKind::VTodo) => {
            for component in &mut calendar.components {
                if let CalendarComponent::Todo(todo) = component
                    && todo.get_uid() == Some(uid.as_str())
                    && todo.property_value("RECURRENCE-ID").is_none()
                {
                    populate_plan_component(todo, plan);
                }
            }
        }
        (PlanComponentKind::VTodo, PlanComponentKind::VEvent) => {
            for component in &mut calendar.components {
                let replacement = match component {
                    CalendarComponent::Todo(todo) if todo.get_uid() == Some(uid.as_str()) => {
                        let is_master = todo.property_value("RECURRENCE-ID").is_none();
                        let mut event = todo_as_event(todo);
                        if is_master {
                            populate_plan_component(&mut event, plan);
                        }
                        Some(CalendarComponent::Event(event))
                    }
                    _ => None,
                };
                if let Some(replacement) = replacement {
                    *component = replacement;
                }
            }
        }
        (PlanComponentKind::VEvent, PlanComponentKind::VTodo) => {
            for component in &mut calendar.components {
                let replacement = match component {
                    CalendarComponent::Event(event) if event.get_uid() == Some(uid.as_str()) => {
                        let is_master = event.property_value("RECURRENCE-ID").is_none();
                        let mut todo = event_as_todo(event);
                        if is_master {
                            populate_plan_component(&mut todo, plan);
                        }
                        Some(CalendarComponent::Todo(todo))
                    }
                    _ => None,
                };
                if let Some(replacement) = replacement {
                    *component = replacement;
                }
            }
        }
    }
    Ok(calendar.to_string())
}

fn copy_component(
    source: &impl Component,
    target: &mut impl Component,
    omitted_properties: &[&str],
) {
    for property in source.properties().values() {
        if !omitted_properties.contains(&property.key()) {
            target.append_property(property.clone());
        }
    }
    for property in source.multi_properties().values().flatten() {
        if !omitted_properties.contains(&property.key()) {
            target.append_multi_property(property.clone());
        }
    }
    for child in source.components() {
        target.append_component(child.clone());
    }
}

fn renamed_property(source: &Property, key: &str) -> Property {
    let mut target = Property::new(key, source.value());
    for parameter in source.params().values() {
        target.add_parameter(parameter.key(), parameter.value());
    }
    target
}

fn todo_as_event(source: &Todo) -> Event {
    let mut target = Event::new();
    copy_component(
        source,
        &mut target,
        &["DUE", "STATUS", "COMPLETED", "PERCENT-COMPLETE"],
    );
    if let Some(due) = source.properties().get("DUE") {
        target.append_property(renamed_property(due, "DTEND"));
    }
    target
}

fn event_as_todo(source: &Event) -> Todo {
    let mut target = Todo::new();
    copy_component(source, &mut target, &["DTEND"]);
    if let Some(end) = source.properties().get("DTEND") {
        target.append_property(renamed_property(end, "DUE"));
    }
    target
}

/// Render a Plan through the legacy VTODO codec.
///
/// New call sites should use [`render_plan_resource_with_component`].
pub fn render_plan_resource(existing: Option<&str>, plan: &Plan) -> Result<String, WorkspaceError> {
    render_plan_resource_with_component(existing, plan, PlanComponentKind::VTodo)
}

/// Map [`ActionState`] to the closest standard iCalendar [`TodoStatus`].
///
/// RFC 5545 has no blocked state. We expose blocked actions as actionable to
/// generic clients and preserve the exact state in `X-CLEARHEAD-STATUS`.
fn action_state_to_todo_status(state: ActionState) -> TodoStatus {
    match state {
        ActionState::NotStarted | ActionState::BlockedOrAwaiting => TodoStatus::NeedsAction,
        ActionState::InProgress => TodoStatus::InProcess,
        ActionState::Completed => TodoStatus::Completed,
        ActionState::Cancelled => TodoStatus::Cancelled,
    }
}

/// Convert one [`Action`] to a standalone VTODO projection.
///
/// The Action UUID is the VTODO UID. VTODO needs no DTSTART, so unscheduled and
/// due-only actions retain a complete calendar representation.
/// Recurrence remains exclusively a [`Plan`] concern and is never emitted here.
pub fn action_to_vtodo(action: &Action) -> Todo {
    let mut todo = Todo::new();
    todo.uid(&action.id.to_string());
    todo.summary(&action.name);
    todo.status(action_state_to_todo_status(action.state));

    if action.state == ActionState::BlockedOrAwaiting {
        todo.add_property("X-CLEARHEAD-STATUS", "blocked");
    }
    if let Some(scheduled_at) = action.scheduled_at {
        todo.starts(scheduled_at.with_timezone(&Utc));
    }
    if let Some(due_date) = action.due_date {
        todo.due(due_date.with_timezone(&Utc));
    }
    if let Some(desc) = &action.description {
        todo.description(desc);
    }
    if action.state == ActionState::Completed
        && let Some(completed_at) = action.completed_at
    {
        todo.completed(completed_at.with_timezone(&Utc));
    }
    if let Some(priority) = action.priority {
        todo.priority(priority);
    }
    if let Some(contexts) = &action.contexts {
        for context in contexts {
            todo.add_multi_property("CATEGORIES", context);
        }
    }

    todo.done()
}

/// Convert a slice of [`Action`]s to an iCalendar string.
///
/// Every action produces a VTODO, including unscheduled actions. Pass
/// `open_only = true` to exclude `Completed` and `Cancelled` actions.
pub fn actions_to_icalendar(actions: &[Action], open_only: bool) -> String {
    let mut calendar = Calendar::new()
        .name("ClearHead Actions")
        .description("Actions exported from ClearHead")
        .done();

    for action in actions {
        if open_only
            && matches!(
                action.state,
                ActionState::Completed | ActionState::Cancelled
            )
        {
            continue;
        }
        calendar.push(action_to_vtodo(action));
    }

    calendar.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn write_ics(content: &str) -> NamedTempFile {
        let mut f = NamedTempFile::new().unwrap();
        f.write_all(content.as_bytes()).unwrap();
        f
    }

    fn parse_ics_file(path: &Path) -> Result<Vec<ICSPlan>, WorkspaceError> {
        parse_ics(&std::fs::read_to_string(path)?, path)
    }

    fn parse_vtodo_actions(path: &Path) -> Result<Vec<VTodoAction>, WorkspaceError> {
        parse_vtodo_actions_content(&std::fs::read_to_string(path)?)
    }

    fn write_occurrence_deviation(
        path: &Path,
        uid: &str,
        key: &str,
        operation: &OccurrenceOp,
    ) -> Result<(), WorkspaceError> {
        let rendered =
            render_occurrence_deviation(&std::fs::read_to_string(path)?, uid, key, operation)?;
        std::fs::write(path, rendered)?;
        Ok(())
    }

    #[test]
    fn parse_description_directives_cases() {
        // template only
        let (tpl, desc) = parse_description_directives("template: weekly-review\nSome notes");
        assert_eq!(tpl.as_deref(), Some("weekly-review"));
        assert_eq!(desc.as_deref(), Some("Some notes"));

        // template only, no body
        let (tpl, desc) = parse_description_directives("template: weekly-review");
        assert_eq!(tpl.as_deref(), Some("weekly-review"));
        assert!(desc.is_none());

        // a retired `upcoming:` directive is no longer special — it degrades to body text
        let (tpl, desc) = parse_description_directives("upcoming: 3");
        assert!(tpl.is_none());
        assert_eq!(desc.as_deref(), Some("upcoming: 3"));

        // no directives — whole string is body
        let (tpl, desc) = parse_description_directives("No template here");
        assert!(tpl.is_none());
        assert_eq!(desc.as_deref(), Some("No template here"));

        // empty template value falls through to body
        let (tpl, desc) = parse_description_directives("template: ");
        assert!(tpl.is_none());
        // "template: " line is consumed as a directive attempt but template is None;
        // nothing left for body
        assert!(desc.is_none());
    }

    #[test]
    fn occurrence_action_id_is_deterministic() {
        let uid = "weekly-review@example.com";
        let occ = "2026-04-27T10:00:00+00:00";
        let id1 = occurrence_action_id(uid, occ);
        let id2 = occurrence_action_id(uid, occ);
        assert_eq!(id1, id2, "same inputs must yield same UUID");

        let other = occurrence_action_id(uid, "2026-05-04T10:00:00+00:00");
        assert_ne!(id1, other, "different occurrence must yield different UUID");
    }

    #[test]
    fn canonical_occurrence_key_collapses_to_one_utc_frame() {
        // A UTC-emitted slot round-trips to its own compact UTC form: the frame
        // every peer agrees on, independent of how the slot was written.
        let slot = parse_ics_datetime_token("20260427T140000Z").unwrap();
        assert_eq!(canonical_occurrence_key(slot), "20260427T140000Z");
        // Two DateTime<Local> naming the same instant hash to the same key.
        let same_instant = slot + chrono::Duration::hours(1) - chrono::Duration::hours(1);
        assert_eq!(canonical_occurrence_key(same_instant), "20260427T140000Z");
    }

    #[test]
    fn parse_ics_file_attaches_exdate_and_recurrence_overrides() {
        let uid = "standup@example.com";
        let f = write_ics(&format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n\
             BEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Standup\r\n\
             DTSTART:20260504T090000Z\r\nRRULE:FREQ=DAILY\r\n\
             EXDATE:20260505T090000Z\r\nEND:VTODO\r\n\
             BEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Standup (moved)\r\n\
             RECURRENCE-ID:20260506T090000Z\r\nDTSTART:20260506T110000Z\r\n\
             STATUS:COMPLETED\r\nCOMPLETED:20260506T113000Z\r\nEND:VTODO\r\n\
             END:VCALENDAR\r\n"
        ));

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(
            plans.len(),
            1,
            "the RECURRENCE-ID VTODO is a deviation, not a second plan"
        );
        let plan = &plans[0];

        assert!(
            plan.exdates.contains("20260505T090000Z"),
            "EXDATE slot is stored as its canonical key: {:?}",
            plan.exdates
        );

        let over = plan
            .overrides
            .get("20260506T090000Z")
            .expect("override keyed by the canonical slot it replaces");
        assert_eq!(over.state, ActionState::Completed);
        assert_eq!(over.title.as_deref(), Some("Standup (moved)"));
        // The override's own DTSTART (11:00Z) is the moved time, distinct from
        // the 09:00Z slot key — proving we hash the slot, not the new value.
        assert_eq!(over.scheduled_at.unwrap().with_timezone(&Utc).hour(), 11);
        assert!(over.completed_at.is_some());
    }

    // -------------------------------------------------------------------------
    // Deviation write path — round-trips through the readers above
    // -------------------------------------------------------------------------

    /// A bare recurring master with no deviations, for the write tests to mutate.
    fn master_ics(uid: &str) -> NamedTempFile {
        write_ics(&format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n\
             BEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Standup\r\n\
             X-APPLE-SORT-ORDER:7\r\n\
             DTSTART:20260504T090000Z\r\nRRULE:FREQ=DAILY\r\nEND:VTODO\r\n\
             END:VCALENDAR\r\n"
        ))
    }

    #[test]
    fn skip_writes_deduplicated_exdate() {
        let uid = "standup@example.com";
        let key = "20260505T090000Z";
        let f = master_ics(uid);

        write_occurrence_deviation(f.path(), uid, key, &OccurrenceOp::Skip).unwrap();
        write_occurrence_deviation(f.path(), uid, key, &OccurrenceOp::Skip).unwrap();

        let plan = &parse_ics_file(f.path()).unwrap()[0];
        assert!(
            plan.exdates.contains(key),
            "EXDATE round-trips into the read model"
        );
        assert_eq!(
            plan.exdates.len(),
            1,
            "writing the same skip twice is idempotent"
        );
        // Unrelated properties survive the mutation.
        let raw = std::fs::read_to_string(f.path()).unwrap();
        assert!(raw.contains("X-APPLE-SORT-ORDER:7"));
    }

    #[test]
    fn complete_writes_recurrence_id_override() {
        let uid = "standup@example.com";
        let key = "20260505T090000Z";
        let at = Utc
            .with_ymd_and_hms(2026, 5, 5, 9, 30, 0)
            .unwrap()
            .with_timezone(&Local);
        let f = master_ics(uid);

        write_occurrence_deviation(f.path(), uid, key, &OccurrenceOp::Complete { at }).unwrap();

        let plan = &parse_ics_file(f.path()).unwrap()[0];
        let over = plan
            .overrides
            .get(key)
            .expect("override keyed by the completed slot");
        assert_eq!(over.state, ActionState::Completed);
        assert!(over.completed_at.is_some());
    }

    #[test]
    fn reschedule_then_complete_updates_one_override() {
        let uid = "standup@example.com";
        let key = "20260505T090000Z";
        let moved = Utc
            .with_ymd_and_hms(2026, 5, 5, 14, 0, 0)
            .unwrap()
            .with_timezone(&Local);
        let at = Utc
            .with_ymd_and_hms(2026, 5, 5, 14, 15, 0)
            .unwrap()
            .with_timezone(&Local);
        let f = master_ics(uid);

        write_occurrence_deviation(
            f.path(),
            uid,
            key,
            &OccurrenceOp::Reschedule {
                scheduled_at: Some(moved),
                due_date: None,
            },
        )
        .unwrap();
        write_occurrence_deviation(f.path(), uid, key, &OccurrenceOp::Complete { at }).unwrap();

        let plan = &parse_ics_file(f.path()).unwrap()[0];
        assert_eq!(
            plan.overrides.len(),
            1,
            "second op updates the same override, not a new one"
        );
        let over = plan.overrides.get(key).unwrap();
        // The reschedule (moved time) and the completion coexist on one override.
        assert_eq!(over.scheduled_at.unwrap().with_timezone(&Utc).hour(), 14);
        assert_eq!(over.state, ActionState::Completed);
    }

    fn vevent_master_ics(uid: &str) -> NamedTempFile {
        write_ics(&format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n\
             BEGIN:VEVENT\r\nUID:{uid}\r\nSUMMARY:Standup\r\n\
             X-APPLE-SORT-ORDER:7\r\n\
             DTSTART:20260504T090000Z\r\nDTEND:20260504T100000Z\r\n\
             RRULE:FREQ=DAILY\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
        ))
    }

    #[test]
    fn vevent_skip_and_reschedule_use_vevent_deviations() {
        let uid = "standup@example.com";
        let key = "20260505T090000Z";
        let moved = Utc
            .with_ymd_and_hms(2026, 5, 5, 14, 0, 0)
            .unwrap()
            .with_timezone(&Local);
        let end = Utc
            .with_ymd_and_hms(2026, 5, 5, 15, 0, 0)
            .unwrap()
            .with_timezone(&Local);
        let f = vevent_master_ics(uid);

        write_occurrence_deviation(f.path(), uid, key, &OccurrenceOp::Skip).unwrap();
        write_occurrence_deviation(
            f.path(),
            uid,
            key,
            &OccurrenceOp::Reschedule {
                scheduled_at: Some(moved),
                due_date: Some(end),
            },
        )
        .unwrap();

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].component_kind, PlanComponentKind::VEvent);
        assert!(plans[0].exdates.contains(key));
        let override_ = plans[0].overrides.get(key).unwrap();
        assert_eq!(override_.scheduled_at, Some(moved));
        assert_eq!(override_.due_date, Some(end));
        let raw = std::fs::read_to_string(f.path()).unwrap();
        assert_eq!(raw.matches("BEGIN:VEVENT").count(), 2);
        assert!(!raw.contains("BEGIN:VTODO"));
        assert!(raw.contains("X-APPLE-SORT-ORDER:7"));
    }

    #[test]
    fn vevent_completion_is_action_owned_and_does_not_mutate_calendar() {
        let uid = "standup@example.com";
        let f = vevent_master_ics(uid);
        let before = std::fs::read_to_string(f.path()).unwrap();
        let at = Utc
            .with_ymd_and_hms(2026, 5, 5, 9, 30, 0)
            .unwrap()
            .with_timezone(&Local);

        write_occurrence_deviation(
            f.path(),
            uid,
            "20260505T090000Z",
            &OccurrenceOp::Complete { at },
        )
        .unwrap();

        assert_eq!(std::fs::read_to_string(f.path()).unwrap(), before);
    }

    #[test]
    fn parses_one_off_vevent_as_plan() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\n\
             UID:focus@example.com\r\nSUMMARY:Focus block\r\n\
             DTSTART:20260504T090000Z\r\nDTEND:20260504T100000Z\r\n\
             END:VEVENT\r\nEND:VCALENDAR\r\n",
        );
        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].plan.name, "Focus block");
        assert!(plans[0].plan.recurrence.is_none());
        assert_eq!(
            plans[0].plan.external_id.as_deref(),
            Some("focus@example.com")
        );
    }

    #[test]
    fn parses_one_off_vtodo_as_plan() {
        let plans = parse_ics(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\n\
             UID:focus@example.com\r\nSUMMARY:Focus block\r\n\
             DTSTART:20260504T090000Z\r\nDUE:20260504T100000Z\r\n\
             END:VTODO\r\nEND:VCALENDAR\r\n",
            Path::new("focus.ics"),
        )
        .unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].component_kind, PlanComponentKind::VTodo);
        assert!(plans[0].plan.recurrence.is_none());
    }

    #[test]
    fn duplicate_and_mixed_plan_components_are_diagnosed() {
        let duplicate = parse_ics(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n\
             BEGIN:VEVENT\r\nUID:focus@example.com\r\nSUMMARY:One\r\n\
             DTSTART:20260504T090000Z\r\nEND:VEVENT\r\n\
             BEGIN:VTODO\r\nUID:focus@example.com\r\nSUMMARY:Two\r\n\
             DTSTART:20260504T090000Z\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
            Path::new("mixed.ics"),
        )
        .unwrap_err();
        let message = duplicate.to_string();
        assert!(message.contains("mixed.ics"));
        assert!(message.contains("focus@example.com"));
        assert!(message.contains("vevent"));
        assert!(message.contains("vtodo"));

        let mismatched_override = parse_ics(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n\
             BEGIN:VEVENT\r\nUID:focus@example.com\r\nSUMMARY:One\r\n\
             DTSTART:20260504T090000Z\r\nRRULE:FREQ=DAILY\r\nEND:VEVENT\r\n\
             BEGIN:VTODO\r\nUID:focus@example.com\r\nSUMMARY:One\r\n\
             RECURRENCE-ID:20260505T090000Z\r\nDTSTART:20260505T100000Z\r\n\
             END:VTODO\r\nEND:VCALENDAR\r\n",
            Path::new("mixed-override.ics"),
        )
        .unwrap_err();
        assert!(
            mismatched_override
                .to_string()
                .contains("mixes vtodo override")
        );
    }

    #[test]
    fn parses_recurring_vevent_with_rescheduled_override() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\n\
             UID:focus@example.com\r\nSUMMARY:Focus block\r\n\
             DTSTART:20260504T090000Z\r\nRRULE:FREQ=DAILY\r\nEND:VEVENT\r\n\
             BEGIN:VEVENT\r\nUID:focus@example.com\r\nSUMMARY:Focus block\r\n\
             RECURRENCE-ID:20260505T090000Z\r\nDTSTART:20260505T110000Z\r\n\
             DTEND:20260505T120000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n",
        );
        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        let override_ = plans[0].overrides.get("20260505T090000Z").unwrap();
        assert_eq!(
            override_.scheduled_at.unwrap().with_timezone(&Utc).hour(),
            11
        );
        assert_eq!(override_.due_date.unwrap().with_timezone(&Utc).hour(), 12);
        assert_eq!(override_.state, ActionState::NotStarted);
    }

    #[test]
    fn standalone_parse_ignores_masters_and_overrides() {
        // A recurring master and its RECURRENCE-ID override are Plan/deviation, not
        // standalone Actions — the standalone reader must skip both, or the sync
        // path would pull an override VTODO in as a spurious new action.
        let uid = "standup@example.com";
        let f = write_ics(&format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n\
             BEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Standup\r\n\
             DTSTART:20260504T090000Z\r\nRRULE:FREQ=DAILY\r\nEND:VTODO\r\n\
             BEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Standup\r\n\
             RECURRENCE-ID:20260505T090000Z\r\nDTSTART:20260505T090000Z\r\n\
             STATUS:COMPLETED\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        ));
        assert!(
            parse_vtodo_actions(f.path()).unwrap().is_empty(),
            "neither the master nor its override is a standalone action"
        );
    }

    #[test]
    fn write_to_absent_master_errors() {
        let f = master_ics("standup@example.com");
        let result = write_occurrence_deviation(
            f.path(),
            "nonexistent@example.com",
            "20260505T090000Z",
            &OccurrenceOp::Skip,
        );
        assert!(
            result.is_err(),
            "a missing master is an error, not a silent no-op"
        );
    }

    // -------------------------------------------------------------------------
    // Export direction
    // -------------------------------------------------------------------------

    use crate::domain::{Action, ActionState};
    use chrono::Timelike;

    fn scheduled_action(name: &str, state: ActionState) -> Action {
        Action {
            id: Uuid::new_v4(),
            name: name.to_string(),
            state,
            scheduled_at: Some(Local.with_ymd_and_hms(2026, 6, 1, 9, 0, 0).unwrap()),
            ..Default::default()
        }
    }

    #[test]
    fn parse_standalone_vtodo_fields_and_iana_timezone() {
        let id = Uuid::new_v4();
        let f = write_ics(&format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{id}\r\nSUMMARY:Edited elsewhere\r\nDESCRIPTION:portable task\r\nSTATUS:COMPLETED\r\nPRIORITY:8\r\nCATEGORIES:home,deep\\,focus\r\nCATEGORIES:errands\r\nDTSTART;TZID=America/New_York:20260427T100000\r\nDUE;VALUE=DATE:20260428\r\nCOMPLETED:20260427T150000Z\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        ));
        let actions = parse_vtodo_actions(f.path()).unwrap();
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].id, id);
        assert_eq!(actions[0].state, ActionState::Completed);
        assert_eq!(actions[0].description.as_deref(), Some("portable task"));
        assert_eq!(actions[0].priority, Some(8));
        assert_eq!(
            actions[0].contexts.as_deref(),
            Some(
                [
                    "deep".into(),
                    "errands".into(),
                    "focus".into(),
                    "home".into()
                ]
                .as_slice()
            )
        );
        assert_eq!(
            actions[0].scheduled_at.unwrap().with_timezone(&Utc).hour(),
            14
        );
        assert!(actions[0].due_date.is_some());
        assert!(actions[0].completed_at.is_some());

        // The compatibility reader still exposes migration input as an Action,
        // while normal Plan discovery now classifies the same one-off VTODO by
        // domain meaning rather than by the presence of RRULE.
        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert!(plans[0].plan.recurrence.is_none());
        assert_eq!(plans[0].component_kind, PlanComponentKind::VTodo);
    }

    #[test]
    fn arbitrary_vtodo_uid_derives_stable_action_uuid() {
        let uid = "client-generated-uid@example.test";
        let first = action_id_from_vtodo_uid(uid);
        assert_eq!(first, action_id_from_vtodo_uid(uid));
        assert_ne!(first, action_id_from_vtodo_uid("other@example.test"));
        let uuid = Uuid::new_v4();
        assert_eq!(action_id_from_vtodo_uid(&uuid.to_string()), uuid);
    }

    #[test]
    fn recurring_vtodo_is_a_plan_not_an_action() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:weekly\r\nSUMMARY:Weekly\r\nDTSTART:20260427T100000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
        );
        assert!(parse_vtodo_actions(f.path()).unwrap().is_empty());
        assert_eq!(parse_ics_file(f.path()).unwrap().len(), 1);
    }

    #[test]
    fn action_to_vtodo_represents_unscheduled_action() {
        let action = Action {
            id: Uuid::new_v4(),
            name: "Unscheduled".to_string(),
            ..Default::default()
        };
        let todo = action_to_vtodo(&action).to_string();
        assert!(todo.contains("BEGIN:VTODO"));
        assert!(!todo.contains("DTSTART"));
        assert!(!todo.contains("DUE"));
    }

    #[test]
    fn action_to_vtodo_maps_identity_and_content() {
        let due = Local.with_ymd_and_hms(2026, 6, 2, 17, 0, 0).unwrap();
        let mut action = scheduled_action("Write spec", ActionState::InProgress);
        action.description = Some("Describe the simpler projection".into());
        action.due_date = Some(due);
        action.priority = Some(2);
        action.contexts = Some(vec!["work".into(), "writing".into()]);

        let todo = action_to_vtodo(&action).to_string();
        assert!(todo.contains(&format!("UID:{}", action.id)));
        assert!(todo.contains("SUMMARY:Write spec"));
        assert!(todo.contains("DESCRIPTION:Describe the simpler projection"));
        assert!(todo.contains("STATUS:IN-PROCESS"));
        assert!(todo.contains("DTSTART"));
        assert!(todo.contains("DUE"));
        assert!(todo.contains("PRIORITY:2"));
        assert!(todo.contains("CATEGORIES:work"));
        assert!(todo.contains("CATEGORIES:writing"));
        assert!(!todo.contains("RRULE"));
    }

    #[test]
    fn action_to_vtodo_preserves_blocked_state_extension() {
        let action = scheduled_action("Waiting", ActionState::BlockedOrAwaiting);
        let todo = action_to_vtodo(&action).to_string();
        assert!(todo.contains("STATUS:NEEDS-ACTION"));
        assert!(todo.contains("X-CLEARHEAD-STATUS:blocked"));
    }

    #[test]
    fn action_to_vtodo_maps_completion() {
        let completed_at = Local.with_ymd_and_hms(2026, 6, 1, 10, 0, 0).unwrap();
        let mut action = scheduled_action("Done", ActionState::Completed);
        action.completed_at = Some(completed_at);
        let todo = action_to_vtodo(&action).to_string();
        assert!(todo.contains("STATUS:COMPLETED"));
        assert!(todo.contains("COMPLETED:"));
    }

    #[test]
    fn plan_serialization_preserves_uid_rrule_directives_and_round_trips() {
        let dtstart = Utc
            .with_ymd_and_hms(2026, 8, 10, 14, 30, 0)
            .unwrap()
            .with_timezone(&Local);
        let plan = Plan {
            id: plan_id_from_ics_uid("weekly@example.com"),
            name: "Weekly Review".to_string(),
            description: Some("Review open commitments".to_string()),
            recurrence: Recurrence::from_rrule_str("FREQ=WEEKLY;COUNT=3"),
            external_id: Some("weekly@example.com".to_string()),
            template_name: Some("weekly-review".to_string()),
            dtstart: Some(dtstart),
            ..Default::default()
        };

        let component = plan_to_vtodo(&plan).to_string();
        assert!(component.contains("UID:weekly@example.com"));
        assert!(component.contains("SUMMARY:Weekly Review"));
        assert!(component.contains("RRULE:FREQ=WEEKLY;COUNT=3"));
        assert!(!component.contains("RRULE:R:"));

        let calendar = plans_to_icalendar(std::slice::from_ref(&plan));
        assert!(calendar.contains("X-WR-CALNAME:ClearHead Plans"));
        let file = write_ics(&calendar);
        let parsed = parse_ics_file(file.path()).unwrap();
        assert_eq!(parsed.len(), 1);
        let round_trip = &parsed[0].plan;
        assert_eq!(round_trip.id, plan.id);
        assert_eq!(round_trip.external_id, plan.external_id);
        assert_eq!(round_trip.name, plan.name);
        assert_eq!(round_trip.description, plan.description);
        assert_eq!(round_trip.template_name, plan.template_name);
        assert_eq!(round_trip.recurrence, plan.recurrence);
        assert_eq!(
            round_trip.dtstart.map(|value| value.timestamp()),
            plan.dtstart.map(|value| value.timestamp())
        );
    }

    #[test]
    fn vevent_plan_serialization_is_the_configurable_default_shape() {
        let plan = Plan {
            id: plan_id_from_ics_uid("focus@example.com"),
            name: "Focus block".to_string(),
            recurrence: Recurrence::from_rrule_str("FREQ=WEEKLY"),
            external_id: Some("focus@example.com".to_string()),
            dtstart: Some(
                Utc.with_ymd_and_hms(2026, 8, 10, 14, 30, 0)
                    .unwrap()
                    .with_timezone(&Local),
            ),
            ..Default::default()
        };

        let calendar = plans_to_icalendar_with_component(
            std::slice::from_ref(&plan),
            PlanComponentKind::VEvent,
        );
        assert!(calendar.contains("BEGIN:VEVENT"));
        assert!(!calendar.contains("BEGIN:VTODO"));
        let parsed = parse_ics(&calendar, Path::new("focus.ics")).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].plan.external_id, plan.external_id);
        assert_eq!(parsed[0].plan.recurrence, plan.recurrence);
    }

    #[test]
    fn vevent_plan_patch_preserves_vendor_properties_and_alarms() {
        let source = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nX-WR-CALNAME:Foreign\r\nBEGIN:VEVENT\r\nUID:focus@example.com\r\nSUMMARY:Old\r\nDTSTART:20260810T143000Z\r\nRRULE:FREQ=WEEKLY\r\nX-VENDOR-KEEP:yes\r\nBEGIN:VALARM\r\nACTION:DISPLAY\r\nTRIGGER:-PT15M\r\nDESCRIPTION:Reminder\r\nEND:VALARM\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n";
        let mut plan = parse_ics(source, Path::new("focus.ics")).unwrap()[0]
            .plan
            .clone();
        plan.name = "Updated".into();

        let rendered =
            render_plan_resource_with_component(Some(source), &plan, PlanComponentKind::VEvent)
                .unwrap();

        assert!(rendered.contains("SUMMARY:Updated"));
        assert!(rendered.contains("X-VENDOR-KEEP:yes"));
        assert!(rendered.contains("BEGIN:VALARM"));
        assert!(rendered.contains("TRIGGER:-PT15M"));
        assert!(rendered.contains("X-WR-CALNAME:Foreign"));
    }

    #[test]
    fn plan_resource_patch_converts_legacy_vtodo_to_configured_vevent() {
        let source = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nX-WR-CALNAME:Foreign\r\nBEGIN:VTODO\r\nUID:weekly@example.com\r\nSUMMARY:Old\r\nDTSTART:20260810T143000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n";
        let mut plan = parse_ics(source, Path::new("weekly.ics")).unwrap()[0]
            .plan
            .clone();
        plan.name = "Updated".into();

        let rendered =
            render_plan_resource_with_component(Some(source), &plan, PlanComponentKind::VEvent)
                .unwrap();

        assert!(rendered.contains("BEGIN:VEVENT"));
        assert!(!rendered.contains("BEGIN:VTODO"));
        assert!(rendered.contains("SUMMARY:Updated"));
        assert!(rendered.contains("X-WR-CALNAME:Foreign"));
    }

    #[test]
    fn codec_conversion_moves_master_and_overrides_as_one_identity() {
        let source = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:weekly@example.com\r\nSUMMARY:Old\r\nDTSTART:20260810T143000Z\r\nDUE:20260810T153000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nBEGIN:VTODO\r\nUID:weekly@example.com\r\nSUMMARY:Old\r\nRECURRENCE-ID:20260817T143000Z\r\nDTSTART:20260817T163000Z\r\nDUE:20260817T173000Z\r\nSTATUS:COMPLETED\r\nX-VENDOR-KEEP:yes\r\nBEGIN:VALARM\r\nACTION:DISPLAY\r\nTRIGGER:-PT15M\r\nDESCRIPTION:Reminder\r\nEND:VALARM\r\nEND:VTODO\r\nEND:VCALENDAR\r\n";
        let plan = parse_ics(source, Path::new("weekly.ics")).unwrap()[0]
            .plan
            .clone();

        let rendered =
            render_plan_resource_with_component(Some(source), &plan, PlanComponentKind::VEvent)
                .unwrap();

        assert_eq!(rendered.matches("BEGIN:VEVENT").count(), 2);
        assert!(!rendered.contains("BEGIN:VTODO"));
        assert!(rendered.contains("RECURRENCE-ID:20260817T143000Z"));
        assert!(rendered.contains("DTEND:20260817T173000Z"));
        assert!(!rendered.contains("STATUS:COMPLETED"));
        assert!(rendered.contains("X-VENDOR-KEEP:yes"));
        assert!(rendered.contains("BEGIN:VALARM"));
        let parsed = parse_ics(&rendered, Path::new("weekly.ics")).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].component_kind, PlanComponentKind::VEvent);
        assert!(parsed[0].overrides.contains_key("20260817T143000Z"));
    }

    #[test]
    fn plan_resource_patch_preserves_vendor_properties_and_alarms() {
        let source = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nX-WR-CALNAME:Foreign\r\nBEGIN:VTODO\r\nUID:weekly@example.com\r\nSUMMARY:Old\r\nDTSTART:20260810T143000Z\r\nRRULE:FREQ=WEEKLY\r\nX-VENDOR-KEEP:yes\r\nBEGIN:VALARM\r\nACTION:DISPLAY\r\nTRIGGER:-PT15M\r\nDESCRIPTION:Reminder\r\nEND:VALARM\r\nEND:VTODO\r\nEND:VCALENDAR\r\n";
        let mut plan = parse_ics(source, Path::new("weekly.ics")).unwrap()[0]
            .plan
            .clone();
        plan.name = "Updated".into();
        plan.description = Some("Owned description".into());

        let rendered = render_plan_resource(Some(source), &plan).unwrap();

        assert!(rendered.contains("SUMMARY:Updated"));
        assert!(rendered.contains("DESCRIPTION:Owned description"));
        assert!(rendered.contains("X-VENDOR-KEEP:yes"));
        assert!(rendered.contains("BEGIN:VALARM"));
        assert!(rendered.contains("TRIGGER:-PT15M"));
        assert!(rendered.contains("X-WR-CALNAME:Foreign"));
    }

    #[test]
    fn plan_without_external_uid_serializes_its_domain_uuid() {
        let plan = Plan {
            name: "Fallback identity".to_string(),
            recurrence: Recurrence::from_rrule_str("FREQ=DAILY"),
            ..Default::default()
        };
        let calendar = plans_to_icalendar(std::slice::from_ref(&plan));
        assert!(calendar.contains(&format!("UID:{}", plan.id)));
    }

    #[test]
    fn actions_to_icalendar_empty_slice_produces_valid_vcalendar() {
        let ics = actions_to_icalendar(&[], false);
        assert!(ics.contains("BEGIN:VCALENDAR"));
        assert!(!ics.contains("BEGIN:VTODO"));
    }

    #[test]
    fn actions_to_icalendar_includes_unscheduled_actions() {
        let unscheduled = Action {
            id: Uuid::new_v4(),
            name: "No date".to_string(),
            ..Default::default()
        };
        let ics = actions_to_icalendar(&[unscheduled], false);
        assert!(ics.contains("BEGIN:VTODO"));
    }

    #[test]
    fn actions_to_icalendar_includes_each_action() {
        let a = scheduled_action("First", ActionState::NotStarted);
        let b = scheduled_action("Second", ActionState::InProgress);
        let ics = actions_to_icalendar(&[a, b], false);
        assert_eq!(ics.matches("BEGIN:VTODO").count(), 2);
    }

    #[test]
    fn actions_to_icalendar_open_only_excludes_terminal_states() {
        let open = scheduled_action("Open task", ActionState::NotStarted);
        let completed = scheduled_action("Done", ActionState::Completed);
        let cancelled = scheduled_action("Dropped", ActionState::Cancelled);
        let ics = actions_to_icalendar(&[open, completed, cancelled], true);
        assert_eq!(ics.matches("BEGIN:VTODO").count(), 1);
        assert!(ics.contains("Open task"));
    }
}
