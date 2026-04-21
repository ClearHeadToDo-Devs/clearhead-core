//! ICS schedule file parser.
//!
//! Reads `.ics` (iCalendar) files and converts each VEVENT into a [`Plan`],
//! populating `recurrence`, `dtstart`, `external_id`, and `template_name` from
//! the VEVENT properties. These Plans are loaded into `Charter.plans` alongside
//! Plans sourced from `.actions` files.

use crate::domain::{Plan, Recurrence};
use crate::workspace::store::WorkspaceError;
use chrono::{DateTime, Local, TimeZone};
use icalendar::{Calendar, CalendarComponent, CalendarDateTime, Component, DatePerhapsTime};
use std::fs;
use std::path::Path;
use uuid::{Uuid, uuid};

/// Namespace UUID for deriving deterministic Plan IDs from VEVENT UIDs.
const ICS_NAMESPACE: Uuid = uuid!("6ba7b810-9dad-11d1-80b4-00c04fd430c8");

/// Parse all VEVENTs in an `.ics` file into [`Plan`] structs.
///
/// Each VEVENT becomes one Plan:
/// - `Plan.id` — UUID v5 from VEVENT UID (deterministic across reloads)
/// - `Plan.name` — SUMMARY
/// - `Plan.recurrence` — parsed from RRULE
/// - `Plan.dtstart` — DTSTART as local time (recurrence expansion anchor)
/// - `Plan.external_id` — raw VEVENT UID string
/// - `Plan.template_name` — X-CLEARHEAD-TEMPLATE custom property
pub fn parse_ics_file(path: &Path) -> Result<Vec<Plan>, WorkspaceError> {
    let content = fs::read_to_string(path)
        .map_err(WorkspaceError::Io)?;

    let calendar: Calendar = content
        .parse()
        .map_err(|e: String| WorkspaceError::Parse(e))?;

    let mut plans = Vec::new();

    for component in calendar.components {
        let CalendarComponent::Event(event) = component else {
            continue;
        };

        let Some(uid) = event.get_uid() else { continue };
        let Some(summary) = event.get_summary() else { continue };

        let plan_id = Uuid::new_v5(&ICS_NAMESPACE, uid.as_bytes());

        let dtstart = parse_dtstart(&event);
        let recurrence = event
            .property_value("RRULE")
            .and_then(Recurrence::from_rrule_str);
        let template_name = event
            .property_value("X-CLEARHEAD-TEMPLATE")
            .map(str::to_string);

        plans.push(Plan {
            id: plan_id,
            name: summary.to_string(),
            description: event.get_description().map(str::to_string),
            recurrence,
            dtstart,
            external_id: Some(uid.to_string()),
            template_name,
            ..Default::default()
        });
    }

    Ok(plans)
}

fn parse_dtstart(event: &icalendar::Event) -> Option<DateTime<Local>> {
    let dpt = event.get_start()?;
    match dpt {
        DatePerhapsTime::DateTime(CalendarDateTime::Floating(naive)) => {
            Local.from_local_datetime(&naive).earliest()
        }
        DatePerhapsTime::DateTime(CalendarDateTime::Utc(utc)) => {
            Some(utc.with_timezone(&Local))
        }
        DatePerhapsTime::DateTime(CalendarDateTime::WithTimezone { date_time, .. }) => {
            Local.from_local_datetime(&date_time).earliest()
        }
        DatePerhapsTime::Date(naive_date) => {
            let naive = naive_date.and_hms_opt(0, 0, 0)?;
            Local.from_local_datetime(&naive).earliest()
        }
    }
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
        assert_eq!(plans[0].external_id.as_deref(), Some("test-uid-001@example.com"));
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
        let expected: Vec<String> = ["MO", "TU", "WE", "TH", "FR"].iter().map(|s| s.to_string()).collect();
        assert_eq!(r.by_day.as_deref(), Some(expected.as_slice()));
    }

    #[test]
    fn parse_vevent_with_template() {
        let f = write_ics(
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VEVENT\r\n\
             UID:weekly-review@example.com\r\n\
             SUMMARY:Weekly Review\r\n\
             DTSTART:20260427T100000\r\n\
             RRULE:FREQ=WEEKLY;BYDAY=SU\r\n\
             X-CLEARHEAD-TEMPLATE:weekly-review\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        );

        let plans = parse_ics_file(f.path()).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].template_name.as_deref(), Some("weekly-review"));
        assert!(plans[0].recurrence.is_some());
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
}
