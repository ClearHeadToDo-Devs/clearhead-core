//! ICS schedule file parser and exporter.
//!
//! **Parse direction** (`ics → domain`): recurring VEVENT/VTODO components
//! become [`Plan`]s; standalone VTODOs become [`VTodoAction`] projections.
//! Component kind and RRULE semantics, rather than server-specific metadata or
//! filenames, determine which domain projection is read.
//!
//! **Export direction** (`domain → ics`): converts a slice of [`Action`]s into
//! an iCalendar string. Every standalone action becomes one VTODO; DTSTART and
//! DUE remain optional, and action projections never carry RRULE.

use crate::domain::{Action, ActionState, Plan, Recurrence};
use crate::workspace::store::WorkspaceError;
use chrono::{DateTime, Local, TimeZone, Utc};
use icalendar::{
    Calendar, CalendarComponent, CalendarDateTime, Component, DatePerhapsTime, EventLike, Todo,
    TodoStatus,
};
use std::fs;
use std::path::{Path, PathBuf};
use uuid::{Uuid, uuid};

/// A parsed `.ics` file entry — the workspace-layer representation of a scheduled plan.
///
/// Carries the source file path alongside the domain [`Plan`].
/// Convert to a plain [`Plan`] via `.plan` at the workspace boundary.
#[derive(Debug, Clone)]
pub struct ICSPlan {
    pub path: PathBuf,
    pub plan: Plan,
}

/// Namespace UUID for deriving deterministic Plan IDs from VEVENT UIDs.
const ICS_NAMESPACE: Uuid = uuid!("6ba7b810-9dad-11d1-80b4-00c04fd430c8");

/// Derive the stable domain Plan ID for a VEVENT UID.
pub fn plan_id_from_ics_uid(uid: &str) -> uuid::Uuid {
    Uuid::new_v5(&ICS_NAMESPACE, uid.as_bytes())
}

/// Derive a deterministic UUID for a generated action from its schedule identity and occurrence key.
///
/// Per the ICS schedule spec: UUID v5 from `(externalScheduleId, externalOccurrenceKey)`.
/// Running expansion multiple times with the same inputs always yields the same UUID.
pub fn occurrence_action_id(vevent_uid: &str, occurrence_rfc3339: &str) -> uuid::Uuid {
    let key = format!("{}:{}", vevent_uid, occurrence_rfc3339);
    uuid::Uuid::new_v5(&ICS_NAMESPACE, key.as_bytes())
}

/// Parse VEVENTs and recurring VTODOs in an `.ics` file into [`Plan`] structs.
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

    for component in calendar.components {
        let plan = match component {
            CalendarComponent::Event(event) => component_to_plan(&event, path),
            // A non-recurring VTODO is an Action projection, not a Plan.
            CalendarComponent::Todo(todo) if todo.property_value("RRULE").is_some() => {
                component_to_plan(&todo, path)
            }
            _ => None,
        };
        if let Some(plan) = plan {
            plans.push(plan);
        }
    }

    Ok(plans)
}

/// Build an [`ICSPlan`] from any component that carries the fields a plan
/// needs (UID, SUMMARY, DTSTART, RRULE, DESCRIPTION) — VEVENT and VTODO both
/// qualify via the shared [`Component`] trait. Returns `None` if UID or
/// SUMMARY is missing.
fn component_to_plan<T: Component>(component: &T, path: &Path) -> Option<ICSPlan> {
    let uid = component.get_uid()?;
    let summary = component.get_summary()?;

    let plan_id = plan_id_from_ics_uid(uid);
    let dtstart = parse_dtstart(component);
    let recurrence = component
        .property_value("RRULE")
        .and_then(Recurrence::from_rrule_str);

    let (template_name, primary_instances, description) = component
        .get_description()
        .map(parse_description_directives)
        .unwrap_or((None, None, None));

    Some(ICSPlan {
        path: path.to_path_buf(),
        plan: Plan {
            id: plan_id,
            name: summary.to_string(),
            description,
            recurrence,
            dtstart,
            external_id: Some(uid.to_string()),
            template_name,
            primary_instances,
            ..Default::default()
        },
    })
}

/// Parse directives and description body from a VEVENT DESCRIPTION.
///
/// Leading `key: value` lines are consumed as directives until a blank line
/// or a non-matching line is encountered. The remainder is the description body.
///
/// Supported directives:
/// - `template: <name>` — binds a template for structural instantiation
/// - `upcoming: <n>`   — per-schedule override for how many instances land in
///                       the primary `.actions` file
///
/// Returns `(template_name, primary_instances, description)`.
fn parse_description_directives(desc: &str) -> (Option<String>, Option<u32>, Option<String>) {
    let mut template: Option<String> = None;
    let mut primary_instances: Option<u32> = None;
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
        } else if let Some(val) = line.strip_prefix("upcoming: ") {
            if let Ok(n) = val.trim().parse::<u32>() {
                primary_instances = Some(n);
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

    (template, primary_instances, rest)
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

/// The interoperable fields ClearHead owns in a standalone VTODO projection.
/// Transport metadata, alarms, and vendor extensions deliberately stay out of
/// this value and are preserved when an existing file is updated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VTodoAction {
    pub id: Uuid,
    pub scheduled_at: Option<DateTime<Local>>,
    pub due_date: Option<DateTime<Local>>,
    pub state: ActionState,
    pub title: String,
    pub description: Option<String>,
    pub completed_at: Option<DateTime<Local>>,
}

/// Read standalone (non-RRULE) VTODOs from one vdir resource.
///
/// VEVENTs and recurring VTODO masters are not Action projections. Components
/// without a UUID UID or SUMMARY are ignored: ClearHead cannot safely attach
/// them to an existing Action.
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
        if todo.property_value("RRULE").is_some() {
            continue;
        }
        let Some(id) = todo.get_uid().and_then(|uid| Uuid::parse_str(uid).ok()) else {
            continue;
        };
        let Some(title) = todo.get_summary() else {
            continue;
        };

        let standard_status = todo.get_status();
        let blocked = matches!(standard_status, Some(TodoStatus::NeedsAction) | None)
            && todo
                .property_value("X-CLEARHEAD-STATUS")
                .is_some_and(|value| value.eq_ignore_ascii_case("blocked"));
        let state = if blocked {
            ActionState::BlockedOrAwaiting
        } else {
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
        };

        actions.push(VTodoAction {
            id,
            scheduled_at: todo.get_start().and_then(date_perhaps_time_to_local),
            due_date: todo.get_due().and_then(date_perhaps_time_to_local),
            state,
            title: title.to_string(),
            description: todo
                .get_description()
                .filter(|value| !value.is_empty())
                .map(str::to_string),
            completed_at: todo
                .get_completed()
                .map(|value| value.with_timezone(&Local)),
        });
    }

    Ok(actions)
}

// ============================================================================
// Export direction: Action slice → iCalendar string
// ============================================================================

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

/// Map ClearHead priority (1–5) to iCalendar PRIORITY (1–9).
fn map_priority(p: u32) -> u32 {
    match p {
        1 => 1,
        2 => 3,
        3 => 5,
        4 => 7,
        _ => 5,
    }
}

/// Convert one [`Action`] to a standalone VTODO projection.
///
/// The Action UUID is the VTODO UID. Unlike VEVENT, VTODO needs no DTSTART, so
/// unscheduled and due-only actions retain a complete calendar representation.
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
    if let Some(p) = action.priority {
        todo.priority(map_priority(p));
    }
    if let Some(contexts) = &action.contexts {
        todo.add_property("CATEGORIES", contexts.join(","));
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
    fn parse_minimal_vevent() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:test-uid-001@example.com\r\n\
             SUMMARY:Weekly Review\r\n\
             DTSTART:20260427T100000\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].plan.name, "Weekly Review");
        assert_eq!(
            plans[0].plan.external_id.as_deref(),
            Some("test-uid-001@example.com")
        );
        assert!(plans[0].plan.dtstart.is_some());
        assert!(plans[0].plan.recurrence.is_none());
        assert!(plans[0].plan.template_name.is_none());
        // ID is deterministic
        let expected_id = Uuid::new_v5(&ICS_NAMESPACE, b"test-uid-001@example.com");
        assert_eq!(plans[0].plan.id, expected_id);
    }

    #[test]
    fn parse_vevent_with_rrule() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:daily-standup@example.com\r\n\
             SUMMARY:Daily Standup\r\n\
             DTSTART:20260420T090000\r\n\
             RRULE:FREQ=DAILY;BYDAY=MO,TU,WE,TH,FR\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        let r = plans[0].plan.recurrence.as_ref().unwrap();
        assert_eq!(r.frequency, "daily");
        let expected: Vec<String> = ["MO", "TU", "WE", "TH", "FR"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        assert_eq!(r.by_day.as_deref(), Some(expected.as_slice()));
    }

    #[test]
    fn parse_vevent_with_template_in_description() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:weekly-review@example.com\r\n\
             SUMMARY:Weekly Review\r\n\
             DTSTART:20260427T100000\r\n\
             RRULE:FREQ=WEEKLY;BYDAY=SU\r\n\
             DESCRIPTION:template: weekly-review\\nReflect on the past week\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(
            plans[0].plan.template_name.as_deref(),
            Some("weekly-review")
        );
        assert_eq!(
            plans[0].plan.description.as_deref(),
            Some("Reflect on the past week")
        );
        assert!(plans[0].plan.recurrence.is_some());
        assert!(plans[0].plan.primary_instances.is_none());
    }

    #[test]
    fn parse_vevent_with_upcoming_directive() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:quarterly-review@example.com\r\n\
             SUMMARY:Quarterly Review\r\n\
             DTSTART:20260427T100000\r\n\
             RRULE:FREQ=MONTHLY;INTERVAL=3\r\n\
             DESCRIPTION:upcoming: 2\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].plan.primary_instances, Some(2));
        assert!(plans[0].plan.template_name.is_none());
    }

    #[test]
    fn parse_vevent_with_template_and_upcoming_directives() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:weekly-review-2@example.com\r\n\
             SUMMARY:Weekly Review\r\n\
             DTSTART:20260427T100000\r\n\
             RRULE:FREQ=WEEKLY;BYDAY=SU\r\n\
             DESCRIPTION:template: weekly-review\\nupcoming: 3\\nSome notes\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(
            plans[0].plan.template_name.as_deref(),
            Some("weekly-review")
        );
        assert_eq!(plans[0].plan.primary_instances, Some(3));
        assert_eq!(plans[0].plan.description.as_deref(), Some("Some notes"));
    }

    #[test]
    fn template_only_description_has_no_remaining_text() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:tpl-only@example.com\r\n\
             SUMMARY:Just Template\r\n\
             DTSTART:20260427T100000\r\n\
             DESCRIPTION:template: release-checklist\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(
            plans[0].plan.template_name.as_deref(),
            Some("release-checklist")
        );
        assert!(plans[0].plan.description.is_none());
    }

    #[test]
    fn description_without_template_prefix_is_plain_description() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:no-tpl@example.com\r\n\
             SUMMARY:Normal Event\r\n\
             DTSTART:20260427T100000\r\n\
             DESCRIPTION:Just a regular event description\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert!(plans[0].plan.template_name.is_none());
        assert_eq!(
            plans[0].plan.description.as_deref(),
            Some("Just a regular event description")
        );
    }

    #[test]
    fn parse_multiple_vevents() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:event-one@example.com\r\n\
             SUMMARY:Event One\r\n\
             DTSTART:20260420T090000\r\n\
             END:VEVENT\r\n\
             BEGIN:VEVENT\r\n\
             UID:event-two@example.com\r\n\
             SUMMARY:Event Two\r\n\
             DTSTART:20260421T100000\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 2);
        let names: Vec<&str> = plans.iter().map(|p| p.plan.name.as_str()).collect();
        assert!(names.contains(&"Event One"));
        assert!(names.contains(&"Event Two"));
    }

    #[test]
    fn vevent_without_uid_is_skipped() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             SUMMARY:No UID Event\r\n\
             DTSTART:20260420T090000\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 0);
    }

    #[test]
    fn parse_description_directives_cases() {
        // template only
        let (tpl, upcoming, desc) =
            parse_description_directives("template: weekly-review\nSome notes");
        assert_eq!(tpl.as_deref(), Some("weekly-review"));
        assert!(upcoming.is_none());
        assert_eq!(desc.as_deref(), Some("Some notes"));

        // template only, no body
        let (tpl, upcoming, desc) = parse_description_directives("template: weekly-review");
        assert_eq!(tpl.as_deref(), Some("weekly-review"));
        assert!(upcoming.is_none());
        assert!(desc.is_none());

        // upcoming only
        let (tpl, upcoming, desc) = parse_description_directives("upcoming: 3");
        assert!(tpl.is_none());
        assert_eq!(upcoming, Some(3));
        assert!(desc.is_none());

        // both directives with body
        let (tpl, upcoming, desc) =
            parse_description_directives("template: weekly-review\nupcoming: 2\nSome notes");
        assert_eq!(tpl.as_deref(), Some("weekly-review"));
        assert_eq!(upcoming, Some(2));
        assert_eq!(desc.as_deref(), Some("Some notes"));

        // no directives — whole string is body
        let (tpl, upcoming, desc) = parse_description_directives("No template here");
        assert!(tpl.is_none());
        assert!(upcoming.is_none());
        assert_eq!(desc.as_deref(), Some("No template here"));

        // empty template value falls through to body
        let (tpl, upcoming, desc) = parse_description_directives("template: ");
        assert!(tpl.is_none());
        assert!(upcoming.is_none());
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
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{id}\r\nSUMMARY:Edited elsewhere\r\nDESCRIPTION:portable task\r\nSTATUS:COMPLETED\r\nDTSTART;TZID=America/New_York:20260427T100000\r\nDUE;VALUE=DATE:20260428\r\nCOMPLETED:20260427T150000Z\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        ));
        let actions = parse_vtodo_actions(f.path()).unwrap();
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].id, id);
        assert_eq!(actions[0].state, ActionState::Completed);
        assert_eq!(actions[0].description.as_deref(), Some("portable task"));
        assert_eq!(
            actions[0].scheduled_at.unwrap().with_timezone(&Utc).hour(),
            14
        );
        assert!(actions[0].due_date.is_some());
        assert!(actions[0].completed_at.is_some());
        assert!(parse_ics_file(f.path()).unwrap().is_empty());
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
        assert!(todo.contains("PRIORITY:3"));
        assert!(todo.contains("CATEGORIES:work\\,writing"));
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
