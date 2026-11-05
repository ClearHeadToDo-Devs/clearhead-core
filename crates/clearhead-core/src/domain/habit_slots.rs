//! Native Habit slot identity. This does not replace the legacy calendar
//! occurrence key or its event-derived ids; those remain stable until migration.
use chrono::{DateTime, LocalResult, NaiveDate, NaiveDateTime, Offset, TimeZone};
use chrono_tz::{GapInfo, Tz};
use regex::Regex;
use std::{str::FromStr, sync::OnceLock};
use uuid::Uuid;

/// The series frame is retained independently of the canonical identity key.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SlotFrame {
    Date,
    Floating,
    UtcOffset,
    Tzid(Tz),
}

/// A validated canonical D:/F:/U: key, never a transport UID.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SlotKey(String);

impl SlotKey {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl FromStr for SlotKey {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let (prefix, source) = value.split_once(':').ok_or("missing slot prefix")?;
        let frame = match prefix {
            "D" => SlotFrame::Date,
            "F" => SlotFrame::Floating,
            "U" => SlotFrame::UtcOffset,
            _ => return Err("invalid slot prefix".into()),
        };
        let slot = canonical_slot(frame, source)?;
        if slot.key.as_str() != value {
            return Err("noncanonical slot key".into());
        }
        Ok(slot.key)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeSlot {
    pub frame: SlotFrame,
    pub key: SlotKey,
}

/// Canonicalize an original slot (RECURRENCE-ID for a moved occurrence), not
/// its current DTSTART. Input may use basic ICS or extended ISO notation, with
/// minute or second precision. TZID values are local times in the supplied
/// series zone. Fractions retain arbitrary decimal precision without rounding.
pub fn canonical_slot(frame: SlotFrame, original_slot: &str) -> Result<NativeSlot, String> {
    let key = if frame == SlotFrame::Date {
        let date = parse_date(original_slot)?;
        format!("D:{}", date.format("%Y-%m-%d"))
    } else {
        let (local, fraction, offset) = parse_datetime(original_slot)?;
        let (prefix, time, suffix) = match &frame {
            SlotFrame::Floating if offset.is_empty() => ("F:", local, ""),
            SlotFrame::UtcOffset if !offset.is_empty() => {
                let text = format!("{}{offset}", local.format("%Y-%m-%dT%H:%M:%S"));
                let utc = DateTime::parse_from_rfc3339(&text)
                    .map_err(|e| e.to_string())?
                    .naive_utc();
                ("U:", utc, "Z")
            }
            SlotFrame::Tzid(zone) if offset.is_empty() => {
                let utc = match zone.from_local_datetime(&local) {
                    LocalResult::Single(time) => time.naive_utc(),
                    // RFC 5545: the first occurrence of an ambiguous wall time.
                    LocalResult::Ambiguous(a, b) => a.min(b).naive_utc(),
                    LocalResult::None => {
                        // RFC 5545: use the offset before the gap, without
                        // shifting the source wall time or probing nearby dates.
                        let (_, offset) = GapInfo::new(&local, zone)
                            .and_then(|gap| gap.begin)
                            .ok_or("cannot resolve series-zone gap")?;
                        local
                            .checked_sub_offset(offset.fix())
                            .ok_or("slot outside supported date range")?
                    }
                };
                ("U:", utc, "Z")
            }
            _ => return Err("slot value does not match its series frame".into()),
        };
        // Keys have four-digit positive years, even after offset conversion.
        let date = time.format("%Y-%m-%dT%H:%M:%S").to_string();
        if date.len() != 19 || !date.as_bytes()[0].is_ascii_digit() {
            return Err("slot outside supported date range".into());
        }
        format!("{prefix}{date}{fraction}{suffix}")
    };
    Ok(NativeSlot {
        frame,
        key: SlotKey(key),
    })
}

fn parse_date(value: &str) -> Result<NaiveDate, String> {
    let format = match value.len() {
        8 if value.bytes().all(|b| b.is_ascii_digit()) => "%Y%m%d",
        10 if value.bytes().enumerate().all(|(i, b)| {
            if i == 4 || i == 7 {
                b == b'-'
            } else {
                b.is_ascii_digit()
            }
        }) =>
        {
            "%Y-%m-%d"
        }
        _ => return Err("invalid slot date".into()),
    };
    NaiveDate::parse_from_str(value, format).map_err(|e| e.to_string())
}

fn parse_datetime(value: &str) -> Result<(NaiveDateTime, String, String), String> {
    static PATTERN: OnceLock<Regex> = OnceLock::new();
    let captures = PATTERN.get_or_init(|| Regex::new(
        r"\A([0-9]{8}|[0-9]{4}-[0-9]{2}-[0-9]{2})T([0-9]{4}(?:[0-9]{2})?|[0-9]{2}:[0-9]{2}(?::[0-9]{2})?)(\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})?\z"
    ).expect("constant slot grammar")).captures(value).ok_or("invalid slot datetime")?;
    let date = parse_date(&captures[1])?;
    let clock = captures[2].replace(':', "");
    let fraction = captures.get(3).map_or("", |m| m.as_str());
    if clock.len() == 4 && !fraction.is_empty() {
        return Err("fraction requires seconds".into());
    }
    let hour = clock[0..2].parse().map_err(|_| "invalid hour")?;
    let minute = clock[2..4].parse().map_err(|_| "invalid minute")?;
    let second = if clock.len() == 6 {
        clock[4..6].parse().map_err(|_| "invalid second")?
    } else {
        0
    };
    let local = date
        .and_hms_opt(hour, minute, second)
        .ok_or("invalid slot time")?;
    let fraction = fraction
        .trim_end_matches('0')
        .trim_end_matches('.')
        .to_string();
    let offset = captures.get(4).map_or("", |m| m.as_str()).to_string();
    Ok((local, fraction, offset))
}

/// Native identity uses only the persisted Habit UUID and canonical original slot.
pub fn occurrence_id(habit_id: Uuid, key: &SlotKey) -> Uuid {
    Uuid::new_v5(&habit_id, format!("occurrence/{}", key.as_str()).as_bytes())
}

/// Template-step identity is persisted before stamping; names/order are not inputs.
pub fn step_id(occurrence_id: Uuid, template_step_id: Uuid) -> Uuid {
    Uuid::new_v5(
        &occurrence_id,
        format!("step/{template_step_id}").as_bytes(),
    )
}
