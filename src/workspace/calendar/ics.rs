//! ICS schedule file parser and exporter.
//!
//! **Parse direction** (`ics → domain`): recurring VTODO components become
//! [`Plan`]s; standalone VTODOs become [`VTodoAction`] projections.
//! Component kind and RRULE semantics, rather than server-specific metadata or
//! filenames, determine which domain projection is read.
//!
//! **Export direction** (`domain → ics`): converts [`Plan`]s and [`Action`]s
//! into iCalendar. Recurring Plans become VTODO masters carrying RRULE; every
//! standalone Action becomes one VTODO whose DTSTART and DUE remain optional.

use crate::domain::{Action, ActionState, Plan, Recurrence};
use crate::workspace::durability::atomic_write;
use crate::workspace::store::WorkspaceError;
use chrono::{DateTime, Local, NaiveDate, NaiveDateTime, TimeZone, Utc};
use icalendar::{
    Calendar, CalendarComponent, CalendarDateTime, Component, DatePerhapsTime, EventLike, Todo,
    TodoStatus,
};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
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

/// Parse recurring VTODOs in an `.ics` file into [`Plan`] structs.
/// Standalone VTODOs are read by [`parse_vtodo_actions`] instead.
///
/// Each accepted component becomes one Plan:
/// - `Plan.id` — UUID v5 from the component's UID (deterministic across reloads)
/// - `Plan.name` — SUMMARY
/// - `Plan.recurrence` — parsed from RRULE
/// - `Plan.dtstart` — DTSTART as local time (recurrence expansion anchor)
/// - `Plan.external_id` — raw UID string
/// - `Plan.template_name` — extracted from DESCRIPTION if it starts with `template: <name>`
pub fn parse_ics_file(path: &Path) -> Result<Vec<ICSPlan>, WorkspaceError> {
    let content = fs::read_to_string(path).map_err(WorkspaceError::Io)?;

    let calendar: Calendar = content
        .parse()
        .map_err(|e: String| WorkspaceError::Parse(e))?;

    let mut plans = Vec::new();

    // Collect todos once: a recurring master and its `RECURRENCE-ID` overrides
    // are separate components that share one UID within the file.
    let todos: Vec<&Todo> = calendar
        .components
        .iter()
        .filter_map(|component| match component {
            CalendarComponent::Todo(todo) => Some(todo),
            _ => None,
        })
        .collect();

    for todo in todos.iter().copied() {
        // A non-recurring VTODO is an Action projection, not a Plan; a
        // `RECURRENCE-ID` VTODO is an override of its master, attached below.
        if todo.property_value("RRULE").is_none() {
            continue;
        }
        let Some(mut ics_plan) = component_to_plan(todo, path) else {
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

/// Build an [`ICSPlan`] from a recurring VTODO's shared component fields.
/// Returns `None` if UID or SUMMARY is missing.
fn component_to_plan<T: Component>(component: &T, path: &Path) -> Option<ICSPlan> {
    let uid = component.get_uid()?;
    let summary = component.get_summary()?;

    let plan_id = plan_id_from_ics_uid(uid);
    let dtstart = parse_dtstart(component);
    let recurrence = component
        .property_value("RRULE")
        .and_then(Recurrence::from_rrule_str);

    let (template_name, description) = component
        .get_description()
        .map(parse_description_directives)
        .unwrap_or((None, None));

    Some(ICSPlan {
        path: path.to_path_buf(),
        exdates: BTreeSet::new(),
        overrides: BTreeMap::new(),
        plan: Plan {
            id: plan_id,
            name: summary.to_string(),
            description,
            recurrence,
            dtstart,
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
fn parse_exdates(todo: &Todo) -> BTreeSet<String> {
    let single = todo.properties().get("EXDATE").into_iter();
    let repeated = todo.multi_properties().get("EXDATE").into_iter().flatten();
    single
        .chain(repeated)
        .flat_map(|property| property.value().split(','))
        .filter_map(parse_ics_datetime_token)
        .map(canonical_occurrence_key)
        .collect()
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
    /// Mark the slot completed at `at` (a `RECURRENCE-ID` override carrying
    /// `STATUS:COMPLETED` + `COMPLETED`).
    Complete { at: DateTime<Local> },
    /// Move the slot to new times (a `RECURRENCE-ID` override). `None` clears
    /// that field on the override, inheriting nothing further from the master.
    Reschedule {
        scheduled_at: Option<DateTime<Local>>,
        due_date: Option<DateTime<Local>>,
    },
}

/// Record `op` against the `occurrence_key` slot of recurring master
/// `master_uid`, mutating `master_path` in place.
///
/// `Skip` adds a deduplicated `EXDATE`; `Complete`/`Reschedule` add or update
/// the slot's `RECURRENCE-ID` override VTODO. Every other component and property
/// in the file is preserved, and the write is atomic. The produced deviation
/// round-trips through [`parse_exdates`] / [`override_from_todo`], so the next
/// projection reflects it. `occurrence_key` must be a [`canonical_occurrence_key`].
pub fn write_occurrence_deviation(
    master_path: &Path,
    master_uid: &str,
    occurrence_key: &str,
    op: &OccurrenceOp,
) -> Result<(), WorkspaceError> {
    let content = fs::read_to_string(master_path).map_err(WorkspaceError::Io)?;
    let mut calendar: Calendar = content.parse().map_err(WorkspaceError::Parse)?;

    match op {
        OccurrenceOp::Skip => add_exdate(&mut calendar, master_uid, occurrence_key)?,
        OccurrenceOp::Complete { at } => {
            let at = *at;
            upsert_override(&mut calendar, master_uid, occurrence_key, |todo| {
                todo.status(TodoStatus::Completed);
                todo.completed(at.with_timezone(&Utc));
            })?
        }
        OccurrenceOp::Reschedule {
            scheduled_at,
            due_date,
        } => {
            let (scheduled_at, due_date) = (*scheduled_at, *due_date);
            upsert_override(&mut calendar, master_uid, occurrence_key, |todo| {
                todo.remove_starts();
                if let Some(value) = scheduled_at {
                    todo.starts(value.with_timezone(&Utc));
                }
                todo.remove_due();
                if let Some(value) = due_date {
                    todo.due(value.with_timezone(&Utc));
                }
            })?
        }
    }

    atomic_write(master_path, calendar.to_string().as_bytes()).map_err(WorkspaceError::Io)
}

/// Ingest a foreign roll-forward: reset the recurring master `master_uid` back to
/// its canonical origin `base_dtstart`, and record each passed slot as a completed
/// occurrence (a `RECURRENCE-ID` override with `STATUS:COMPLETED` + `COMPLETED`).
///
/// This translates a camp-B "advance the master to complete an occurrence"
/// mutation into ClearHead's canonical fixed-anchor + deviation form (per RFC 5545
/// a `RECURRENCE-ID` is only valid when its slot is on the series grid, so the
/// anchor must sit at/before every override). Recording is **idempotent**: a slot
/// that already carries any override is left untouched, so a client that re-advances
/// forever churns only the anchor value, never the completion history. Preserves
/// all other components/properties; atomic. Each tuple is `(canonical key, completed-at)`.
pub fn write_master_rollforward(
    master_path: &Path,
    master_uid: &str,
    base_dtstart: DateTime<Local>,
    completed_slots: &[(String, DateTime<Local>)],
) -> Result<(), WorkspaceError> {
    let content = fs::read_to_string(master_path).map_err(WorkspaceError::Io)?;
    let mut calendar: Calendar = content.parse().map_err(WorkspaceError::Parse)?;

    // Reset the master anchor to the canonical origin.
    let index = master_index(&calendar, master_uid).ok_or_else(|| {
        WorkspaceError::Parse(format!("recurring master VTODO {master_uid} not found"))
    })?;
    if let CalendarComponent::Todo(master) = &mut calendar.components[index] {
        master.remove_starts();
        master.starts(base_dtstart.with_timezone(&Utc));
    }

    // Record each passed slot as completed, skipping any slot that already carries
    // an override so re-detecting the same advance records nothing new.
    for (key, completed_at) in completed_slots {
        let already = calendar.components.iter().any(|component| {
            matches!(component, CalendarComponent::Todo(todo)
                if todo.get_uid() == Some(master_uid)
                    && todo
                        .property_value("RECURRENCE-ID")
                        .and_then(parse_ics_datetime_token)
                        .map(canonical_occurrence_key)
                        .as_deref()
                        == Some(key.as_str()))
        });
        if already {
            continue;
        }
        let completed_at = *completed_at;
        upsert_override(&mut calendar, master_uid, key, |todo| {
            todo.status(TodoStatus::Completed);
            todo.completed(completed_at.with_timezone(&Utc));
        })?;
    }

    atomic_write(master_path, calendar.to_string().as_bytes()).map_err(WorkspaceError::Io)
}

/// Index of the recurring master VTODO for `uid` — the one carrying `RRULE` and
/// no `RECURRENCE-ID` (which would make it an override, not the master).
fn master_index(calendar: &Calendar, uid: &str) -> Option<usize> {
    calendar.components.iter().position(|component| {
        matches!(component, CalendarComponent::Todo(todo)
            if todo.get_uid() == Some(uid)
                && todo.property_value("RRULE").is_some()
                && todo.property_value("RECURRENCE-ID").is_none())
    })
}

fn add_exdate(calendar: &mut Calendar, uid: &str, key: &str) -> Result<(), WorkspaceError> {
    let index = master_index(calendar, uid)
        .ok_or_else(|| WorkspaceError::Parse(format!("recurring master VTODO {uid} not found")))?;
    let CalendarComponent::Todo(master) = &mut calendar.components[index] else {
        unreachable!("master_index only matches Todo components")
    };
    if parse_exdates(master).contains(key) {
        return Ok(()); // already excluded — writing is idempotent
    }
    master.add_multi_property("EXDATE", key);
    Ok(())
}

/// Find the `RECURRENCE-ID` override for `key` and patch it, or create one seeded
/// from the master (UID, the slot as DTSTART, the master SUMMARY) and patch that.
fn upsert_override(
    calendar: &mut Calendar,
    uid: &str,
    key: &str,
    patch: impl FnOnce(&mut Todo),
) -> Result<(), WorkspaceError> {
    let master_summary = {
        let index = master_index(calendar, uid).ok_or_else(|| {
            WorkspaceError::Parse(format!("recurring master VTODO {uid} not found"))
        })?;
        let CalendarComponent::Todo(master) = &calendar.components[index] else {
            unreachable!("master_index only matches Todo components")
        };
        master.get_summary().map(str::to_string)
    };

    let existing = calendar.components.iter().position(|component| {
        let CalendarComponent::Todo(todo) = component else {
            return false;
        };
        todo.get_uid() == Some(uid)
            && todo
                .property_value("RECURRENCE-ID")
                .and_then(parse_ics_datetime_token)
                .map(canonical_occurrence_key)
                .as_deref()
                == Some(key)
    });

    if let Some(index) = existing {
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
        if let Some(summary) = master_summary {
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

/// Read standalone (non-RRULE) VTODOs from one vdir resource.
///
/// Recurring VTODO masters are not Action projections. Components without UID
/// or SUMMARY are ignored because they cannot form a stable Action.
pub fn parse_vtodo_actions(path: &Path) -> Result<Vec<VTodoAction>, WorkspaceError> {
    let content = fs::read_to_string(path).map_err(WorkspaceError::Io)?;
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
///
/// `external_id` is the interoperable RFC 5545 UID when present; locally
/// authored Plans fall back to their domain UUID. Plan names map to SUMMARY,
/// recurrence to RRULE, and the optional template directive leads DESCRIPTION.
pub fn plan_to_vtodo(plan: &Plan) -> Todo {
    let mut todo = Todo::new();
    let uid = plan
        .external_id
        .clone()
        .unwrap_or_else(|| plan.id.to_string());
    todo.uid(&uid);
    todo.summary(&plan.name);

    if let Some(dtstart) = plan.dtstart {
        todo.starts(dtstart.with_timezone(&Utc));
    }
    if let Some(recurrence) = &plan.recurrence {
        let rrule = recurrence.to_string();
        todo.add_property("RRULE", rrule.strip_prefix("R:").unwrap_or(&rrule));
    }

    let mut description = Vec::new();
    if let Some(template) = &plan.template_name {
        description.push(format!("template: {template}"));
    }
    if let Some(text) = &plan.description {
        description.push(text.clone());
    }
    if !description.is_empty() {
        todo.description(&description.join("\n"));
    }

    todo.done()
}

/// Convert recurring [`Plan`]s to the canonical ClearHead Plan calendar.
pub fn plans_to_icalendar(plans: &[Plan]) -> String {
    let mut calendar = Calendar::new()
        .name("ClearHead Plans")
        .description("Schedules managed by ClearHead")
        .done();

    for plan in plans {
        calendar.push(plan_to_vtodo(plan));
    }

    calendar.to_string()
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
        assert!(parse_ics_file(f.path()).unwrap().is_empty());
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
