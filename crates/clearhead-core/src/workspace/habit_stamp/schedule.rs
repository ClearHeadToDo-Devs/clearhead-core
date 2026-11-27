use chrono::{Duration, FixedOffset, NaiveDateTime, NaiveTime, TimeZone};

use crate::domain::Action;
use crate::domain::habit_slots::{NativeSlot, SlotFrame, series_offset};
use crate::domain::time::{Bound, Due, Planned, Precision};

/// Offsets are measured in the series frame, not the viewer's zone. DATE
/// bounds retain their day precision even when attached to a timed series.
pub(super) struct SlotShift {
    frame: SlotFrame,
    wall_delta: Duration,
    day_delta: Duration,
    offset: FixedOffset,
}

impl SlotShift {
    pub(super) fn new(anchor: Bound, slot: &NativeSlot) -> Result<Self, &'static str> {
        if (anchor.precision == Precision::Day) != (slot.frame == SlotFrame::Date)
            || (slot.frame == SlotFrame::Floating && anchor.offset.is_some())
        {
            return Err("slot frame does not match the Habit anchor");
        }
        let offset = anchor
            .offset
            .unwrap_or_else(|| FixedOffset::east_opt(0).unwrap());
        let from = wall_time(anchor, &slot.frame, offset);
        let to = slot
            .position_in(offset)
            .ok_or("slot outside supported date range")?;
        Ok(Self {
            frame: slot.frame.clone(),
            wall_delta: to - from,
            day_delta: to.date() - from.date(),
            offset,
        })
    }

    pub(super) fn apply(&self, action: &mut Action) -> Result<(), &'static str> {
        let shift = |bound| self.bound(bound);
        if let Some(planned) = action.planned {
            action.planned = Some(Planned {
                start: shift(planned.start)?,
                end: planned.end.map(shift).transpose()?,
            });
        }
        if let Some(due) = action.due_date {
            action.due_date = Some(Due {
                start: due.start.map(shift).transpose()?,
                end: shift(due.end)?,
            });
        }
        Ok(())
    }

    fn bound(&self, bound: Bound) -> Result<Bound, &'static str> {
        let (local, offset) = if bound.precision == Precision::Day {
            let date = bound
                .local
                .date()
                .checked_add_signed(self.day_delta)
                .ok_or("shifted date outside supported range")?;
            (date.and_time(NaiveTime::MIN), None)
        } else {
            let local = wall_time(bound, &self.frame, self.offset)
                .checked_add_signed(self.wall_delta)
                .ok_or("shifted time outside supported range")?;
            let offset = match self.frame {
                SlotFrame::Tzid(zone) => Some(series_offset(zone, local)?),
                SlotFrame::UtcOffset => Some(self.offset),
                SlotFrame::Date | SlotFrame::Floating => bound.offset,
            };
            (local, offset)
        };
        Ok(Bound {
            local,
            offset,
            ..bound
        })
    }
}

fn wall_time(bound: Bound, frame: &SlotFrame, offset: FixedOffset) -> NaiveDateTime {
    if bound.offset.is_none() {
        return bound.local;
    }
    let utc = bound.at_in(&chrono::Utc).naive_utc();
    match frame {
        SlotFrame::Tzid(zone) => zone.from_utc_datetime(&utc).naive_local(),
        SlotFrame::UtcOffset => offset.from_utc_datetime(&utc).naive_local(),
        SlotFrame::Date | SlotFrame::Floating => bound.local,
    }
}
