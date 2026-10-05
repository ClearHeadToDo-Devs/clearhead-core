//! Written time bounds and the ranges built from them (Decisions 47, 48, 51).
//!
//! A date covers its day; a date and time is an instant. A range is half-open:
//! it begins at its start's first instant and ends where its end does, a
//! date's following midnight or a time itself. The due window (`:`) and the
//! planned block (`@`) read their bounds the same way; they differ only in
//! which side a single value is: the deadline for `:`, the start for `@`.

use chrono::{
    DateTime, Duration, FixedOffset, Local, LocalResult, NaiveDate, NaiveDateTime, NaiveTime,
    Offset, TimeZone, Timelike,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;
use std::str::FromStr;

/// Parse an ISO 8601 date or date-time, with an optional offset, to its
/// instant in the local zone (see [`Bound`] for the forms and how a local
/// time resolves).
pub fn parse_iso8601_datetime(datetime_str: &str) -> Option<DateTime<Local>> {
    datetime_str.parse::<Bound>().ok().map(|bound| bound.at())
}

/// Resolve a local date and time in `zone` as RFC 5545 §3.3.5 does
/// (Decision 52): a time that occurs twice is its first occurrence, and one
/// that does not occur is read with the offset in effect before the gap.
pub fn resolve_local<Tz: TimeZone>(local: NaiveDateTime, zone: &Tz) -> DateTime<Tz> {
    match zone.from_local_datetime(&local) {
        LocalResult::Single(at) | LocalResult::Ambiguous(at, _) => at,
        LocalResult::None => {
            // A day earlier is safely before the gap, whatever the zone.
            let before = zone
                .offset_from_utc_datetime(&(local - Duration::days(1)))
                .fix();
            let utc = local - Duration::seconds(before.local_minus_utc().into());
            zone.from_utc_datetime(&utc)
        }
    }
}

/// The unit a bound was written to. Only a day changes what a bound means;
/// minutes and seconds are kept so values are written back as they were.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Precision {
    Day,
    Minute,
    Second,
}

/// A date or date-time as written (Decision 52): the text is the truth, and
/// an instant is resolved from it only when asked, in the viewer's zone.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bound {
    /// The written date and time; a date's is its midnight.
    pub local: NaiveDateTime,
    /// The offset the file wrote, if any. A date never has one.
    pub offset: Option<FixedOffset>,
    pub precision: Precision,
}

impl Bound {
    /// The local date of `at`, written as a date.
    pub fn day(at: DateTime<Local>) -> Self {
        Self {
            local: at.date_naive().and_time(NaiveTime::MIN),
            offset: None,
            precision: Precision::Day,
        }
    }

    /// The local time of `at` to the minute, written without an offset.
    pub fn minute(at: DateTime<Local>) -> Self {
        let local = at.naive_local();
        Self {
            local: local
                .with_second(0)
                .and_then(|t| t.with_nanosecond(0))
                .unwrap_or(local),
            offset: None,
            precision: Precision::Minute,
        }
    }

    /// The bound's first instant in the local zone: a date's midnight, or the
    /// time itself.
    pub fn at(&self) -> DateTime<Local> {
        self.at_in(&Local)
    }

    /// The bound's first instant in `zone`.
    pub fn at_in<Tz: TimeZone>(&self, zone: &Tz) -> DateTime<Tz> {
        match self.offset {
            Some(offset) => (self.local - Duration::seconds(offset.local_minus_utc().into()))
                .and_utc()
                .with_timezone(zone),
            None => resolve_local(self.local, zone),
        }
    }

    /// Where a range ending at this bound ends: a date's following midnight,
    /// or the time itself.
    pub fn end_instant(&self) -> DateTime<Local> {
        self.end_instant_in(&Local)
    }

    /// [`end_instant`](Self::end_instant) in `zone`.
    pub fn end_instant_in<Tz: TimeZone>(&self, zone: &Tz) -> DateTime<Tz> {
        match self.precision {
            Precision::Day => resolve_local(self.local + Duration::days(1), zone),
            Precision::Minute | Precision::Second => self.at_in(zone),
        }
    }

    /// The bound as an XSD literal, as written (ontology.md, Time): a date is
    /// an `xsd:date`; a time an `xsd:dateTime` with seconds, and its offset
    /// only if one was written. Returns the lexical form and the XSD type.
    pub fn xsd(&self) -> (String, &'static str) {
        match self.precision {
            Precision::Day => (self.local.format("%Y-%m-%d").to_string(), "date"),
            Precision::Minute | Precision::Second => {
                let offset = match self.offset {
                    Some(offset) if offset.local_minus_utc() == 0 => "Z".to_string(),
                    Some(offset) => offset.to_string(),
                    None => String::new(),
                };
                let time = self.local.format("%Y-%m-%dT%H:%M:%S");
                (format!("{time}{offset}"), "dateTime")
            }
        }
    }

    /// The same bound moved by `delta` of elapsed time, written in its own
    /// frame and precision.
    fn shifted(self, delta: Duration) -> Self {
        let local = match self.offset {
            Some(_) => self.local + delta,
            None => (self.at() + delta).naive_local(),
        };
        Self { local, ..self }
    }
}

/// Split `start/end`, the only ISO 8601 interval form the DSL admits.
fn split_range(s: &str) -> Result<(Bound, Option<Bound>), String> {
    match s.trim().split_once('/') {
        Some((start, end)) => Ok((start.parse()?, Some(end.parse()?))),
        None => Ok((s.parse()?, None)),
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

/// Split a written offset (`Z`, `+hh:mm`, `-hh:mm`) from a date-time.
fn split_offset(s: &str) -> Result<(&str, Option<FixedOffset>), String> {
    let Some((_, time)) = s.split_once('T') else {
        return Ok((s, None));
    };
    if let Some(local) = s.strip_suffix('Z') {
        return Ok((
            local,
            Some(FixedOffset::east_opt(0).expect("zero is a valid offset")),
        ));
    }
    match time.rfind(['+', '-']) {
        Some(at) => {
            let split = s.len() - time.len() + at;
            let offset = s[split..]
                .parse()
                .map_err(|_| format!("invalid offset: {}", &s[split..]))?;
            Ok((&s[..split], Some(offset)))
        }
        None => Ok((s, None)),
    }
}

fn parse_local(s: &str) -> Option<NaiveDateTime> {
    ["%Y-%m-%dT%H:%M:%S%.f", "%Y-%m-%dT%H:%M"]
        .iter()
        .find_map(|pattern| NaiveDateTime::parse_from_str(s, pattern).ok())
        .and_then(|local| local.with_nanosecond(0))
        .or_else(|| {
            NaiveDate::parse_from_str(s, "%Y-%m-%d")
                .ok()
                .map(|date| date.and_time(NaiveTime::MIN))
        })
}

impl FromStr for Bound {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let (local, offset) = split_offset(s)?;
        Ok(Self {
            local: parse_local(local).ok_or_else(|| format!("invalid date/time: {s}"))?,
            offset,
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
        write!(f, "{}", self.local.format(pattern))?;
        match self.offset {
            Some(offset) if offset.local_minus_utc() == 0 => write!(f, "Z"),
            Some(offset) => write!(f, "{offset}"),
            None => Ok(()),
        }
    }
}

impl Serialize for Bound {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for Bound {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        String::deserialize(deserializer)?
            .parse()
            .map_err(serde::de::Error::custom)
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
        self.start.map(|b| b.at())
    }

    /// The first instant the action is late.
    pub fn late_from(&self) -> DateTime<Local> {
        self.end.end_instant()
    }

    /// Whether the window can never be met: it opens at or after it closes.
    pub fn is_empty(&self) -> bool {
        self.not_before()
            .is_some_and(|open| open >= self.late_from())
    }
}

impl FromStr for Due {
    type Err = String;

    /// `end` or the ISO 8601 interval `start/end`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match split_range(s)? {
            (start, Some(end)) => Self {
                start: Some(start),
                end,
            },
            (end, None) => Self::by(end),
        })
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

/// The planned block (`@` in the DSL, Decision 51): when the action is
/// planned to be worked. A start alone is a point, or a whole day for a date.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Planned {
    pub start: Bound,
    pub end: Option<Bound>,
}

impl Planned {
    /// A planned start with no end.
    pub fn at(start: Bound) -> Self {
        Self { start, end: None }
    }

    /// The block a retired `D<minutes>` described: ending that many minutes
    /// after a timed start, at the start's precision. A date has no such
    /// block, so the duration is dropped and the whole day is planned.
    pub fn from_minutes(start: Bound, minutes: u32) -> Self {
        let end = (start.precision != Precision::Day)
            .then(|| start.shifted(Duration::minutes(minutes.into())));
        Self { start, end }
    }

    /// The block's length; a start alone has none.
    pub fn duration(&self) -> Option<Duration> {
        self.duration_in(&Local)
    }

    /// [`duration`](Self::duration) in `zone`, where a block that crosses a
    /// clock change has its elapsed length.
    pub fn duration_in<Tz: TimeZone>(&self, zone: &Tz) -> Option<Duration> {
        self.end
            .map(|end| end.end_instant_in(zone) - self.start.at_in(zone))
    }

    /// Whether the block ends at or before it starts.
    pub fn is_empty(&self) -> bool {
        self.duration().is_some_and(|d| d <= Duration::zero())
    }
}

/// The planned start's instant, as calendars place it.
pub fn planned_start(planned: Option<&Planned>) -> Option<DateTime<Local>> {
    planned.map(|p| p.start.at())
}

/// Move the planned start to a calendar instant, keeping the block's length
/// and the written precision when the instant is unchanged or still a whole
/// day.
pub fn with_planned_start(
    planned: Option<Planned>,
    at: Option<DateTime<Local>>,
) -> Option<Planned> {
    let at = at?;
    let Some(planned) = planned else {
        return Some(Planned::at(Bound::minute(at)));
    };
    let start = match planned.start {
        start if start.at() == at => start,
        start if start.precision == Precision::Day && at.time() == NaiveTime::MIN => Bound::day(at),
        _ => Bound::minute(at),
    };
    let end = planned.end.map(|end| end.shifted(at - planned.start.at()));
    Some(Planned { start, end })
}

/// Where the planned block ends, as calendars place it: a date end's next
/// midnight (an all-day `DTEND`), or the time itself.
pub fn planned_end(planned: Option<&Planned>) -> Option<DateTime<Local>> {
    planned.and_then(|p| p.end).map(|end| end.end_instant())
}

/// Set the planned block's end from a calendar instant, keeping the written
/// precision when the instant is unchanged. A midnight ending a block that
/// starts on a date is that date range's exclusive end, so the day before is
/// written. An end needs a start; without one nothing is planned.
pub fn with_planned_end(planned: Option<Planned>, at: Option<DateTime<Local>>) -> Option<Planned> {
    let planned = planned?;
    let end = at.map(|at| match planned.end {
        Some(end) if end.end_instant() == at => end,
        _ if planned.start.precision == Precision::Day && at.time() == NaiveTime::MIN => at
            .date_naive()
            .pred_opt()
            .and_then(|day| {
                Local
                    .from_local_datetime(&day.and_time(NaiveTime::MIN))
                    .earliest()
            })
            .map_or(Bound::minute(at), Bound::day),
        _ => Bound::minute(at),
    });
    Some(Planned { end, ..planned })
}

impl FromStr for Planned {
    type Err = String;

    /// `start` or the ISO 8601 interval `start/end`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (start, end) = split_range(s)?;
        Ok(Self { start, end })
    }
}

impl fmt::Display for Planned {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.end {
            Some(end) => write!(f, "{}/{end}", self.start),
            None => write!(f, "{}", self.start),
        }
    }
}

impl Serialize for Planned {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for Planned {
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

    fn planned(s: &str) -> Planned {
        s.parse().unwrap()
    }

    fn at(s: &str) -> DateTime<Local> {
        parse_iso8601_datetime(s).unwrap()
    }

    #[test]
    fn a_date_deadline_covers_its_day_and_a_time_is_an_instant() {
        assert_eq!(due("2026-10-05").late_from(), at("2026-10-06"));
        assert_eq!(due("2026-10-05T17:00").late_from(), at("2026-10-05T17:00"));
        assert_eq!(
            due("2026-10-05T17:00:00").late_from(),
            at("2026-10-05T17:00")
        );
    }

    #[test]
    fn a_single_planned_value_is_the_start() {
        let p = planned("2026-10-03T09:00");
        assert_eq!(p.start.at(), at("2026-10-03T09:00"));
        assert_eq!(p.end, None);
        assert_eq!(p.duration(), None);
    }

    #[test]
    fn a_half_hour_block_is_thirty_minutes() {
        let p = planned("2026-10-03T09:00/2026-10-03T09:30");
        assert_eq!(p.duration(), Some(Duration::minutes(30)));
        assert_eq!(p.to_string(), "2026-10-03T09:00/2026-10-03T09:30");
    }

    #[test]
    fn a_date_block_covers_its_last_day() {
        assert_eq!(
            planned("2026-10-03/2026-10-05").duration(),
            Some(at("2026-10-06") - at("2026-10-03"))
        );
    }

    #[test]
    fn a_block_that_ends_at_or_before_it_starts_is_empty() {
        assert!(planned("2026-10-03T09:30/2026-10-03T09:00").is_empty());
        assert!(planned("2026-10-03T09:00/2026-10-03T09:00").is_empty());
        assert!(!planned("2026-10-03/2026-10-03").is_empty());
    }

    #[test]
    fn a_retired_duration_becomes_its_block() {
        let start: Bound = "2026-10-03T09:00".parse().unwrap();
        assert_eq!(
            Planned::from_minutes(start, 15).to_string(),
            "2026-10-03T09:00/2026-10-03T09:15"
        );
        let day: Bound = "2026-10-03".parse().unwrap();
        assert_eq!(Planned::from_minutes(day, 60).to_string(), "2026-10-03");
    }

    #[test]
    fn a_calendar_end_keeps_the_written_form() {
        let block = Some(planned("2026-10-03/2026-10-05"));
        assert_eq!(planned_end(block.as_ref()), Some(at("2026-10-06")));
        assert_eq!(
            with_planned_end(block, Some(at("2026-10-07")))
                .unwrap()
                .to_string(),
            "2026-10-03/2026-10-06"
        );
        let timed = Some(planned("2026-10-03T09:00"));
        assert_eq!(
            with_planned_end(timed, Some(at("2026-10-03T09:30")))
                .unwrap()
                .to_string(),
            "2026-10-03T09:00/2026-10-03T09:30"
        );
        assert_eq!(with_planned_end(None, Some(at("2026-10-03T09:30"))), None);
    }

    #[test]
    fn moving_the_start_keeps_the_block_length() {
        let moved = with_planned_start(
            Some(planned("2026-10-03T09:00/2026-10-03T09:30")),
            Some(at("2026-10-03T14:00")),
        );
        assert_eq!(
            moved.unwrap().to_string(),
            "2026-10-03T14:00/2026-10-03T14:30"
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
    fn a_written_offset_is_kept_and_names_its_instant() {
        for written in [
            "2026-10-05T17:00+02:00",
            "2026-10-05T17:00:00-05:00",
            "2026-10-05T17:00Z",
        ] {
            assert_eq!(due(written).to_string(), written);
        }
        assert_eq!(
            due("2026-10-05T17:00+02:00").late_from(),
            at("2026-10-05T15:00:00Z")
        );
    }

    #[test]
    fn a_date_has_no_offset() {
        assert!("2026-10-05+02:00".parse::<Bound>().is_err());
    }

    #[test]
    fn a_local_time_round_trips_even_when_it_occurs_twice() {
        // 01:30 on 2025-11-02 occurs twice in US zones; the text is the truth.
        let written = "2025-11-01T12:00/2025-11-02T01:30";
        assert_eq!(planned(written).to_string(), written);
        assert_eq!(planned(written), planned(&planned(written).to_string()));
    }

    use chrono_tz::America::Los_Angeles as LA;

    fn local(s: &str) -> NaiveDateTime {
        NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M").unwrap()
    }

    #[test]
    fn a_time_that_occurs_twice_is_its_first_occurrence() {
        let at = resolve_local(local("2025-11-02T01:30"), &LA);
        assert_eq!(at.to_rfc3339(), "2025-11-02T01:30:00-07:00");
    }

    #[test]
    fn a_time_that_does_not_occur_takes_the_offset_before_the_gap() {
        let at = resolve_local(local("2026-03-08T02:30"), &LA);
        assert_eq!(at.to_rfc3339(), "2026-03-08T03:30:00-07:00");
    }

    #[test]
    fn a_floating_bound_resolves_in_the_viewers_zone() {
        let bound: Bound = "2026-10-05T17:00".parse().unwrap();
        assert_eq!(bound.at_in(&LA).to_rfc3339(), "2026-10-05T17:00:00-07:00");
        assert_eq!(
            bound.at_in(&chrono_tz::Europe::Berlin).to_rfc3339(),
            "2026-10-05T17:00:00+02:00"
        );
        let fixed: Bound = "2026-10-05T17:00+02:00".parse().unwrap();
        assert_eq!(fixed.at_in(&LA).to_rfc3339(), "2026-10-05T08:00:00-07:00");
    }

    #[test]
    fn a_bound_is_written_to_xsd_as_written() {
        let xsd = |s: &str| s.parse::<Bound>().unwrap().xsd();
        assert_eq!(xsd("2026-10-03"), ("2026-10-03".into(), "date"));
        assert_eq!(
            xsd("2026-10-03T09:00"),
            ("2026-10-03T09:00:00".into(), "dateTime")
        );
        assert_eq!(
            xsd("2026-10-03T09:00+02:00"),
            ("2026-10-03T09:00:00+02:00".into(), "dateTime")
        );
        assert_eq!(
            xsd("2026-10-03T09:00Z"),
            ("2026-10-03T09:00:00Z".into(), "dateTime")
        );
    }

    #[test]
    fn a_block_across_the_clock_change_has_its_elapsed_length() {
        let night = planned("2025-11-01T23:00/2025-11-02T03:00");
        assert_eq!(night.duration_in(&LA), Some(Duration::hours(5)));
        assert_eq!(night.duration_in(&chrono::Utc), Some(Duration::hours(4)));
        assert_eq!(
            due("2026-10-04")
                .end
                .end_instant_in(&chrono::Utc)
                .to_rfc3339(),
            "2026-10-05T00:00:00+00:00"
        );
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
