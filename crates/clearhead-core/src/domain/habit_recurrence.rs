//! Recurrence enumeration for anchors written as DATE, floating, or fixed offset.

use chrono::{DateTime, FixedOffset, NaiveDate, NaiveDateTime, NaiveTime, SecondsFormat, TimeZone};
use rrule::{RRule, RRuleSet, Tz, Unvalidated};

use super::habit_slots::{NativeSlot, SlotFrame, canonical_slot};
use super::time::{Bound, Precision};

/// Expand in series wall time. UTC is a neutral coordinate for the recurrence
/// library, not a conversion of DATE or floating values to an instant.
pub(crate) fn text_recurrence(anchor: Bound, rule: &str) -> Result<RRuleSet, String> {
    if rule.trim().is_empty() {
        return Err("Habit requires a recurrence rule".into());
    }
    let rule = rule.trim().to_ascii_uppercase();
    let parts: Vec<_> = rule.split(';').collect();
    if parts.iter().any(|part| part.starts_with("COUNT="))
        && parts.iter().any(|part| part.starts_with("UNTIL="))
    {
        return Err("Habit recurrence cannot combine COUNT and UNTIL".into());
    }
    let rule = parts
        .iter()
        .map(|part| match part.strip_prefix("UNTIL=") {
            Some(until) => wall_until(anchor, until),
            None => Ok((*part).to_owned()),
        })
        .collect::<Result<Vec<_>, _>>()?
        .join(";");
    let start = Tz::UTC.from_utc_datetime(&anchor.local);
    rule.parse::<RRule<Unvalidated>>()
        .and_then(|rule| rule.build(start))
        .map_err(|error| format!("invalid Habit recurrence: {error}"))
}

fn wall_until(anchor: Bound, until: &str) -> Result<String, String> {
    let error = || {
        format!("invalid Habit recurrence: UNTIL {until} does not match the anchor's time frame")
    };
    let wall = match (anchor.precision, anchor.offset) {
        (Precision::Day, _) => NaiveDate::parse_from_str(until, "%Y%m%d")
            .map_err(|_| error())?
            .and_time(NaiveTime::MIN),
        (_, Some(offset)) => NaiveDateTime::parse_from_str(until, "%Y%m%dT%H%M%SZ")
            .ok()
            .and_then(|utc| utc.checked_add_offset(offset))
            .ok_or_else(error)?,
        (_, None) => NaiveDateTime::parse_from_str(until, "%Y%m%dT%H%M%S").map_err(|_| error())?,
    };
    // The library parses a non-Z UNTIL in the process timezone. Mark this
    // synthetic wall coordinate Z to keep it in the same frame as DTSTART.
    Ok(format!("UNTIL={}", wall.format("%Y%m%dT%H%M%SZ")))
}

pub(crate) fn arrived_text_slots(
    anchor: Bound,
    rule: &str,
    now: &DateTime<FixedOffset>,
) -> Result<impl Iterator<Item = Result<NativeSlot, String>> + use<>, String> {
    let recurrence = text_recurrence(anchor, rule)?;
    let label = move |wall: NaiveDateTime| match anchor.offset {
        Some(offset) => offset
            .from_local_datetime(&wall)
            .single()
            .unwrap()
            .to_rfc3339_opts(SecondsFormat::Secs, true),
        None if anchor.precision == Precision::Day => wall.date().to_string(),
        None => wall.format("%Y-%m-%dT%H:%M:%S").to_string(),
    };
    let (frame, cutoff) = match (anchor.precision, anchor.offset) {
        (Precision::Day, _) => (SlotFrame::Date, now.date_naive().and_time(NaiveTime::MIN)),
        (_, Some(offset)) => (
            SlotFrame::UtcOffset,
            now.with_timezone(&offset).naive_local(),
        ),
        (_, None) => (SlotFrame::Floating, now.naive_local()),
    };
    Ok(recurrence
        .into_iter()
        .map(|at| at.naive_utc())
        .take_while(move |at| *at <= cutoff)
        .map(move |at| canonical_slot(frame.clone(), &label(at))))
}
