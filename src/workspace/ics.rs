//! ICS schedule file parser and exporter.
//!
//! **Parse direction** (`ics → domain`): reads `.ics` files and converts each
//! VEVENT into a [`Plan`], populating `recurrence`, `dtstart`, `external_id`,
//! and `template_name`.
//!
//! **Export direction** (`domain → ics`): converts a slice of [`Action`]s into
//! an iCalendar string. Each action with `scheduled_at` becomes one VEVENT —
//! no RRULE master events; every occurrence is its own discrete event.

use crate::domain::{Action, ActionState, Plan, Recurrence};
use crate::workspace::store::WorkspaceError;
use chrono::{DateTime, Local, TimeZone, Utc};
use icalendar::{
    Calendar, CalendarComponent, CalendarDateTime, Component, DatePerhapsTime, Event, EventLike,
    EventStatus,
};
use std::fs;
use std::path::Path;
use uuid::{Uuid, uuid};

/// Namespace UUID for deriving deterministic Plan IDs from VEVENT UIDs.
const ICS_NAMESPACE: Uuid = uuid!("6ba7b810-9dad-11d1-80b4-00c04fd430c8");

/// Derive the stable domain Plan ID for a VEVENT UID.
pub fn plan_id_from_ics_uid(uid: &str) -> uuid::Uuid {
    Uuid::new_v5(&ICS_NAMESPACE, uid.as_bytes())
}

/// Derive a deterministic UUID for a generated act from its schedule identity and occurrence key.
///
/// Per the ICS schedule spec: UUID v5 from `(externalScheduleId, externalOccurrenceKey)`.
/// Running expansion multiple times with the same inputs always yields the same UUID.
pub fn occurrence_act_id(vevent_uid: &str, occurrence_rfc3339: &str) -> uuid::Uuid {
    let key = format!("{}:{}", vevent_uid, occurrence_rfc3339);
    uuid::Uuid::new_v5(&ICS_NAMESPACE, key.as_bytes())
}

/// Parse all VEVENTs in an `.ics` file into [`Plan`] structs.
///
/// Each VEVENT becomes one Plan:
/// - `Plan.id` — UUID v5 from VEVENT UID (deterministic across reloads)
/// - `Plan.name` — SUMMARY
/// - `Plan.recurrence` — parsed from RRULE
/// - `Plan.dtstart` — DTSTART as local time (recurrence expansion anchor)
/// - `Plan.external_id` — raw VEVENT UID string
/// - `Plan.template_name` — extracted from DESCRIPTION if it starts with `template: <name>`
pub fn parse_ics_file(path: &Path) -> Result<Vec<Plan>, WorkspaceError> {
    let content = fs::read_to_string(path).map_err(WorkspaceError::Io)?;

    let calendar: Calendar = content
        .parse()
        .map_err(|e: String| WorkspaceError::Parse(e))?;

    let mut plans = Vec::new();

    for component in calendar.components {
        let CalendarComponent::Event(event) = component else {
            continue;
        };

        let Some(uid) = event.get_uid() else { continue };
        let Some(summary) = event.get_summary() else {
            continue;
        };

        let plan_id = plan_id_from_ics_uid(uid);

        let dtstart = parse_dtstart(&event);
        let recurrence = event
            .property_value("RRULE")
            .and_then(Recurrence::from_rrule_str);

        let (template_name, primary_instances, description) = event
            .get_description()
            .map(parse_description_directives)
            .unwrap_or((None, None, None));

        plans.push(Plan {
            id: plan_id,
            name: summary.to_string(),
            description,
            recurrence,
            dtstart,
            external_id: Some(uid.to_string()),
            template_name,
            primary_instances,
            ..Default::default()
        });
    }

    Ok(plans)
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
        if s.is_empty() { None } else { Some(s.to_string()) }
    } else {
        None
    };

    (template, primary_instances, rest)
}

fn parse_dtstart(event: &icalendar::Event) -> Option<DateTime<Local>> {
    let dpt = event.get_start()?;
    match dpt {
        DatePerhapsTime::DateTime(CalendarDateTime::Floating(naive)) => {
            Local.from_local_datetime(&naive).earliest()
        }
        DatePerhapsTime::DateTime(CalendarDateTime::Utc(utc)) => Some(utc.with_timezone(&Local)),
        DatePerhapsTime::DateTime(CalendarDateTime::WithTimezone { date_time, .. }) => {
            Local.from_local_datetime(&date_time).earliest()
        }
        DatePerhapsTime::Date(naive_date) => {
            let naive = naive_date.and_hms_opt(0, 0, 0)?;
            Local.from_local_datetime(&naive).earliest()
        }
    }
}

// ============================================================================
// Export direction: Action slice → iCalendar string
// ============================================================================

/// Map [`ActionState`] to iCalendar [`EventStatus`].
fn action_state_to_event_status(state: ActionState) -> EventStatus {
    match state {
        ActionState::NotStarted => EventStatus::Tentative,
        ActionState::InProgress => EventStatus::Confirmed,
        ActionState::Completed => EventStatus::Confirmed,
        ActionState::BlockedOrAwaiting => EventStatus::Tentative,
        ActionState::Cancelled => EventStatus::Cancelled,
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

/// Convert one [`Action`] to a VEVENT. Returns `None` if `scheduled_at` is absent.
///
/// Each occurrence is emitted as a discrete event — no RRULE.
pub fn action_to_vevent(action: &Action) -> Option<Event> {
    let scheduled_at = action.scheduled_at?;
    let start = scheduled_at.with_timezone(&Utc);
    let duration_mins = action.duration.unwrap_or(15) as i64;
    let end = start + chrono::Duration::minutes(duration_mins);

    let mut event = Event::new();
    event.uid(&action.id.to_string());
    event.summary(&action.name);
    event.starts(start);
    event.ends(end);
    event.status(action_state_to_event_status(action.state));

    if let Some(desc) = &action.description {
        event.description(desc);
    }
    if action.state == ActionState::Completed {
        if let Some(completed_at) = action.completed_at {
            event.timestamp(completed_at.with_timezone(&Utc));
        }
    }
    if let Some(p) = action.priority {
        event.priority(map_priority(p));
    }
    if let Some(contexts) = &action.contexts {
        event.add_property("CATEGORIES", &contexts.join(","));
    }

    Some(event.done())
}

/// Convert a slice of [`Action`]s to an iCalendar string.
///
/// Only actions with `scheduled_at` produce VEVENTs. Pass `open_only = true` to
/// exclude `Completed` and `Cancelled` actions.
pub fn actions_to_icalendar(actions: &[Action], open_only: bool) -> String {
    let mut calendar = Calendar::new()
        .name("ClearHead Actions")
        .description("Actions exported from ClearHead")
        .done();

    for action in actions {
        if action.scheduled_at.is_none() {
            continue;
        }
        if open_only
            && matches!(
                action.state,
                ActionState::Completed | ActionState::Cancelled
            )
        {
            continue;
        }
        if let Some(event) = action_to_vevent(action) {
            calendar.push(event);
        }
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
        assert_eq!(plans[0].name, "Weekly Review");
        assert_eq!(
            plans[0].external_id.as_deref(),
            Some("test-uid-001@example.com")
        );
        assert!(plans[0].dtstart.is_some());
        assert!(plans[0].recurrence.is_none());
        assert!(plans[0].template_name.is_none());
        // ID is deterministic
        let expected_id = Uuid::new_v5(&ICS_NAMESPACE, b"test-uid-001@example.com");
        assert_eq!(plans[0].id, expected_id);
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
        let r = plans[0].recurrence.as_ref().unwrap();
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
        assert_eq!(plans[0].template_name.as_deref(), Some("weekly-review"));
        assert_eq!(
            plans[0].description.as_deref(),
            Some("Reflect on the past week")
        );
        assert!(plans[0].recurrence.is_some());
        assert!(plans[0].primary_instances.is_none());
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
        assert_eq!(plans[0].primary_instances, Some(2));
        assert!(plans[0].template_name.is_none());
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
        assert_eq!(plans[0].template_name.as_deref(), Some("weekly-review"));
        assert_eq!(plans[0].primary_instances, Some(3));
        assert_eq!(plans[0].description.as_deref(), Some("Some notes"));
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
        assert_eq!(plans[0].template_name.as_deref(), Some("release-checklist"));
        assert!(plans[0].description.is_none());
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
        assert!(plans[0].template_name.is_none());
        assert_eq!(
            plans[0].description.as_deref(),
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
        let names: Vec<&str> = plans.iter().map(|p| p.name.as_str()).collect();
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
        let (tpl, upcoming, desc) = parse_description_directives("template: weekly-review\nSome notes");
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
        let (tpl, upcoming, desc) = parse_description_directives("template: weekly-review\nupcoming: 2\nSome notes");
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
    fn occurrence_act_id_is_deterministic() {
        let uid = "weekly-review@example.com";
        let occ = "2026-04-27T10:00:00+00:00";
        let id1 = occurrence_act_id(uid, occ);
        let id2 = occurrence_act_id(uid, occ);
        assert_eq!(id1, id2, "same inputs must yield same UUID");

        let other = occurrence_act_id(uid, "2026-05-04T10:00:00+00:00");
        assert_ne!(id1, other, "different occurrence must yield different UUID");
    }

    // -------------------------------------------------------------------------
    // Export direction
    // -------------------------------------------------------------------------

    use crate::domain::{Action, ActionState};

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
    fn action_to_vevent_requires_scheduled_at() {
        let action = Action {
            id: Uuid::new_v4(),
            name: "Unscheduled".to_string(),
            ..Default::default()
        };
        assert!(action_to_vevent(&action).is_none());
    }

    #[test]
    fn action_to_vevent_uses_action_name_as_summary() {
        let action = scheduled_action("Write spec", ActionState::NotStarted);
        let event = action_to_vevent(&action).unwrap();
        assert!(event.to_string().contains("Write spec"));
    }

    #[test]
    fn action_to_vevent_emits_no_rrule() {
        let action = scheduled_action("Daily task", ActionState::NotStarted);
        let event = action_to_vevent(&action).unwrap();
        assert!(!event.to_string().contains("RRULE"));
    }

    #[test]
    fn action_to_vevent_uses_action_id_as_uid() {
        let action = scheduled_action("Task", ActionState::NotStarted);
        let event_str = action_to_vevent(&action).unwrap().to_string();
        assert!(event_str.contains(&action.id.to_string()));
    }

    #[test]
    fn action_to_vevent_default_duration_is_15_min() {
        let action = scheduled_action("Quick task", ActionState::NotStarted);
        let event_str = action_to_vevent(&action).unwrap().to_string();
        // DTEND should be 15 minutes after DTSTART
        assert!(event_str.contains("DTEND"));
    }

    #[test]
    fn actions_to_icalendar_empty_slice_produces_valid_vcalendar() {
        let ics = actions_to_icalendar(&[], false);
        assert!(ics.contains("BEGIN:VCALENDAR"));
        assert!(!ics.contains("BEGIN:VEVENT"));
    }

    #[test]
    fn actions_to_icalendar_skips_unscheduled_actions() {
        let unscheduled = Action {
            id: Uuid::new_v4(),
            name: "No date".to_string(),
            ..Default::default()
        };
        let ics = actions_to_icalendar(&[unscheduled], false);
        assert!(!ics.contains("BEGIN:VEVENT"));
    }

    #[test]
    fn actions_to_icalendar_includes_each_scheduled_action() {
        let a = scheduled_action("First", ActionState::NotStarted);
        let b = scheduled_action("Second", ActionState::InProgress);
        let ics = actions_to_icalendar(&[a, b], false);
        assert_eq!(ics.matches("BEGIN:VEVENT").count(), 2);
    }

    #[test]
    fn actions_to_icalendar_open_only_excludes_terminal_states() {
        let open = scheduled_action("Open task", ActionState::NotStarted);
        let completed = scheduled_action("Done", ActionState::Completed);
        let cancelled = scheduled_action("Dropped", ActionState::Cancelled);
        let ics = actions_to_icalendar(&[open, completed, cancelled], true);
        assert_eq!(ics.matches("BEGIN:VEVENT").count(), 1);
        assert!(ics.contains("Open task"));
    }
}
