//! Written time bounds (Decisions 47 and 48).
//!
//! A bound keeps the precision it was written at: a date covers its day, a
//! minute its minute, a second its second. The due window (`:`) is a deadline,
//! optionally with a lower bound, and is half-open: it opens at the first
//! instant of its start and is late from the instant after its end.

use chrono::{DateTime, Duration, Local, NaiveTime, TimeZone};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;
use std::str::FromStr;

/// Parse ISO 8601 datetime string to DateTime<Local>
/// Supports formats: YYYY-MM-DD, YYYY-MM-DDTHH:MM, YYYY-MM-DDTHH:MM:SS
/// with optional timezone (Z or +/-HH:MM)
pub fn parse_iso8601_datetime(datetime_str: &str) -> Option<DateTime<Local>> {
    use chrono::{NaiveDate, NaiveDateTime, NaiveTime, TimeZone};

    let trimmed = datetime_str.trim();

    // Try parsing with timezone first
    if let Ok(dt) = DateTime::parse_from_rfc3339(trimmed) {
        return Some(dt.with_timezone(&Local));
    }

    // Try YYYY-MM-DDTHH:MM:SS format (without timezone)
    if let Ok(naive_dt) = NaiveDateTime::parse_from_str(trimmed, "%Y-%m-%dT%H:%M:%S") {
        return Local.from_local_datetime(&naive_dt).earliest();
    }

    // Try YYYY-MM-DDTHH:MM format (without timezone, no seconds)
    if let Ok(naive_dt) = NaiveDateTime::parse_from_str(trimmed, "%Y-%m-%dT%H:%M") {
        return Local.from_local_datetime(&naive_dt).earliest();
    }

    // Try YYYY-MM-DD format (date only, default to start of day)
    if let Ok(naive_date) = NaiveDate::parse_from_str(trimmed, "%Y-%m-%d") {
        let naive_dt = naive_date.and_time(NaiveTime::from_hms_opt(0, 0, 0)?);
        return Local.from_local_datetime(&naive_dt).earliest();
    }

    None
}

/// The unit a bound was written to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Precision {
    Day,
    Minute,
    Second,
}

/// A date or date-time as written, covering its whole unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bound {
    /// The first instant the bound covers.
    pub at: DateTime<Local>,
    pub precision: Precision,
}

impl Bound {
    pub fn day(at: DateTime<Local>) -> Self {
        Self {
            at,
            precision: Precision::Day,
        }
    }

    pub fn minute(at: DateTime<Local>) -> Self {
        Self {
            at,
            precision: Precision::Minute,
        }
    }

    /// The first instant after the bound's unit.
    pub fn next_instant(&self) -> DateTime<Local> {
        match self.precision {
            Precision::Day => self
                .at
                .date_naive()
                .succ_opt()
                .and_then(|d| {
                    Local
                        .from_local_datetime(&d.and_time(NaiveTime::MIN))
                        .earliest()
                })
                .unwrap_or(self.at + Duration::days(1)),
            Precision::Minute => self.at + Duration::minutes(1),
            Precision::Second => self.at + Duration::seconds(1),
        }
    }
}

/// Precision from the written text: no time is a day; otherwise count the
/// colons in the clock part, ignoring any offset.
fn written_precision(s: &str) -> Precision {
    match s.split_once('T') {
        None => Precision::Day,
        Some((_, time)) => {
            let clock = time.split(['Z', '+', '-']).next().unwrap_or(time);
            if clock.matches(':').count() >= 2 {
                Precision::Second
            } else {
                Precision::Minute
            }
        }
    }
}

impl FromStr for Bound {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let at = parse_iso8601_datetime(s).ok_or_else(|| format!("invalid date/time: {s}"))?;
        Ok(Self {
            at,
            precision: written_precision(s),
        })
    }
}

impl fmt::Display for Bound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pattern = match self.precision {
            Precision::Day => "%Y-%m-%d",
            Precision::Minute => "%Y-%m-%dT%H:%M",
            Precision::Second => "%Y-%m-%dT%H:%M:%S",
        };
        write!(f, "{}", self.at.format(pattern))
    }
}

/// The due window (`:` in the DSL): a deadline, optionally with a lower bound.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Due {
    pub start: Option<Bound>,
    pub end: Bound,
}

impl Due {
    /// A deadline with no lower bound.
    pub fn by(end: Bound) -> Self {
        Self { start: None, end }
    }

    /// The first instant the action may be worked, if the window has a start.
    pub fn not_before(&self) -> Option<DateTime<Local>> {
        self.start.map(|b| b.at)
    }

    /// The first instant the action is late.
    pub fn late_from(&self) -> DateTime<Local> {
        self.end.next_instant()
    }

    /// Whether the window can never be met: it opens at or after it closes.
    pub fn is_empty(&self) -> bool {
        self.not_before()
            .is_some_and(|open| open >= self.late_from())
    }
}

/// The deadline's first instant, as calendars see a due window: its start is
/// a constraint and is never placed (Decision 48).
pub fn deadline(due: Option<&Due>) -> Option<DateTime<Local>> {
    due.map(|d| d.end.at)
}

/// Set the deadline from a calendar instant, keeping the window's start, and
/// the written precision when the instant is unchanged or still a whole day.
pub fn with_deadline(due: Option<Due>, at: Option<DateTime<Local>>) -> Option<Due> {
    let at = at?;
    let start = due.and_then(|d| d.start);
    let end = match due.map(|d| d.end) {
        Some(end) if end.at == at => end,
        Some(end) if end.precision == Precision::Day && at.time() == NaiveTime::MIN => {
            Bound::day(at)
        }
        _ => Bound::minute(at),
    };
    Some(Due { start, end })
}

impl FromStr for Due {
    type Err = String;

    /// `end` or the ISO 8601 interval `start/end`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim().split_once('/') {
            Some((start, end)) => Ok(Self {
                start: Some(start.parse()?),
                end: end.parse()?,
            }),
            None => Ok(Self::by(s.parse()?)),
        }
    }
}

impl fmt::Display for Due {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.start {
            Some(start) => write!(f, "{start}/{}", self.end),
            None => write!(f, "{}", self.end),
        }
    }
}

impl Serialize for Due {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for Due {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        String::deserialize(deserializer)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn due(s: &str) -> Due {
        s.parse().unwrap()
    }

    #[test]
    fn a_bound_keeps_its_written_precision() {
        for written in ["2026-10-05", "2026-10-05T17:00", "2026-10-05T17:00:00"] {
            assert_eq!(due(written).to_string(), written);
        }
        assert_eq!(
            due("2026-11-01/2026-12-15").to_string(),
            "2026-11-01/2026-12-15"
        );
    }

    #[test]
    fn late_from_is_the_start_of_the_next_unit() {
        let at = |s: &str| parse_iso8601_datetime(s).unwrap();
        assert_eq!(due("2026-10-05").late_from(), at("2026-10-06"));
        assert_eq!(due("2026-10-05T17:00").late_from(), at("2026-10-05T17:01"));
        assert_eq!(
            due("2026-10-05T17:00:00").late_from(),
            at("2026-10-05T17:00:01")
        );
    }

    #[test]
    fn a_window_opens_at_its_start() {
        let window = due("2026-11-01/2026-12-15");
        assert_eq!(window.not_before(), parse_iso8601_datetime("2026-11-01"));
        assert_eq!(due("2026-12-15").not_before(), None);
    }

    #[test]
    fn a_window_that_opens_after_it_closes_is_empty() {
        assert!(due("2026-12-15/2026-11-01").is_empty());
        assert!(!due("2026-10-05/2026-10-05").is_empty());
    }

    #[test]
    fn precision_ignores_the_offset() {
        assert_eq!(
            written_precision("2026-10-05T17:00+02:00"),
            Precision::Minute
        );
        assert_eq!(
            written_precision("2026-10-05T17:00:00-05:00"),
            Precision::Second
        );
    }
}
