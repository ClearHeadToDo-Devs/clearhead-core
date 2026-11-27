use std::collections::HashSet;

use chrono::{DateTime, FixedOffset};
use uuid::Uuid;

use crate::domain::habit_recurrence::text_slots;
use crate::domain::habit_slots::{NativeSlot, SlotFrame, SlotKey, canonical_slot, occurrence_id};
use crate::domain::time::{Bound, Precision};
use crate::workspace::{WorkspaceError, predecessors::PredecessorScope};

use super::{PreparedStamp, StampSource, StampedOccurrence, prepare_stamp};

pub struct CatchUpInput<'a> {
    pub source: StampSource<'a>,
    pub habit_id: Uuid,
    pub named_template: Option<StampSource<'a>>,
    pub scope: &'a PredecessorScope,
    pub charter_id: Option<Uuid>,
    /// IDs observed in both live Actions and archive history. The host can
    /// also derive occurrence IDs from recorded native lineage.
    pub occupied: &'a HashSet<Uuid>,
    pub excluded: &'a HashSet<SlotKey>,
    /// Maximum number of complete occurrence trees to stage.
    pub budget: usize,
}

impl<'a> CatchUpInput<'a> {
    fn prepare(&self) -> Result<Option<PreparedStamp<'a>>, WorkspaceError> {
        prepare_stamp(
            self.source,
            self.habit_id,
            self.named_template,
            self.scope,
            self.charter_id,
        )
    }
}

#[derive(Debug, Default)]
pub struct CatchUpBatch {
    pub occurrences: Vec<StampedOccurrence>,
    /// At least one additional eligible slot was observed beyond the budget.
    pub has_backlog: bool,
}

/// Plan a bounded batch from an ordered enumeration of arrived original slots.
/// The recurrence iterator supplies the time cutoff; this function applies
/// exclusions and identity checks without touching storage. Resume by replaying
/// the same enumeration against newly observed IDs, not a mutable cursor.
pub fn plan_catch_up(
    input: CatchUpInput<'_>,
    arrived_slots: impl IntoIterator<Item = Result<NativeSlot, WorkspaceError>>,
) -> Result<CatchUpBatch, WorkspaceError> {
    let Some(stamp) = input.prepare()? else {
        return Ok(CatchUpBatch::default());
    };
    select_batch(&input, &stamp, arrived_slots)
}

/// Compute arrivals from the Habit line's rule and written time frame. The
/// explicit clock's wall date/time is used only for DATE and floating cutoffs.
/// Calendar-backed sets with RDATE or a TZID use [`plan_catch_up`] instead.
pub fn plan_text_catch_up(
    input: CatchUpInput<'_>,
    now: &DateTime<FixedOffset>,
) -> Result<CatchUpBatch, WorkspaceError> {
    let Some(stamp) = input.prepare()? else {
        return Ok(CatchUpBatch::default());
    };
    let anchor = stamp.tree[0].planned.expect("validated Habit").start;
    let from_source = |error: String| {
        WorkspaceError::Actions(stamp.source.message(
            stamp.source.range(input.habit_id),
            "slot-time",
            &error,
        ))
    };
    let slots = text_slots(anchor, &stamp.recurrence_rule, now)
        .map_err(from_source)?
        .take_while(|(arrived, _)| *arrived)
        .map(|(_, slot)| slot.map_err(from_source));
    select_batch(&input, &stamp, slots)
}

/// Interpret the text path's EXDATE properties in the anchor's frame. Richer
/// calendar sets must go through the calendar-backed planner, not silently
/// fall back to the text rule and publish the wrong original slots.
pub fn text_exclusions(
    anchor: Bound,
    habits: Option<&serde_json::Value>,
    habit_id: Uuid,
) -> Result<HashSet<SlotKey>, String> {
    #[derive(serde::Deserialize)]
    struct CalendarFacts {
        frame: Option<String>,
        #[serde(default)]
        exdate: Vec<String>,
        #[serde(default)]
        rdate: Vec<String>,
        #[serde(default)]
        pending_overrides: std::collections::BTreeMap<String, String>,
    }
    let Some(habits) = habits else {
        return Ok(HashSet::new());
    };
    let Some(facts) = habits
        .as_object()
        .ok_or("invalid habits sidecar: expected an object")?
        .get(&habit_id.to_string())
    else {
        return Ok(HashSet::new());
    };
    let facts: CalendarFacts = serde_json::from_value(facts.clone())
        .map_err(|error| format!("invalid Habit sidecar: {error}"))?;
    let (frame, frame_name, value_type) = match (anchor.precision, anchor.offset) {
        (Precision::Day, _) => (SlotFrame::Date, "DATE", "VALUE=DATE"),
        (_, None) => (SlotFrame::Floating, "floating", "VALUE=DATE-TIME"),
        (_, Some(_)) => (SlotFrame::UtcOffset, "UTC", "VALUE=DATE-TIME"),
    };
    if !facts.rdate.is_empty()
        || !facts.pending_overrides.is_empty()
        || facts
            .frame
            .is_some_and(|name| !name.eq_ignore_ascii_case(frame_name))
    {
        return Err("calendar-backed Habit dates, overrides or time frame are not supported by text catch-up".into());
    }
    let mut excluded = HashSet::new();
    for line in facts.exdate {
        let (property, values) = line
            .split_once(':')
            .ok_or_else(|| format!("invalid EXDATE '{line}'"))?;
        let valid_property = property.eq_ignore_ascii_case("EXDATE")
            || property.split_once(';').is_some_and(|(name, value)| {
                name.eq_ignore_ascii_case("EXDATE") && value.eq_ignore_ascii_case(value_type)
            });
        if !valid_property {
            return Err(format!(
                "EXDATE '{line}' does not match the text anchor's frame"
            ));
        }
        for value in values.split(',') {
            let slot = canonical_slot(frame.clone(), value)
                .map_err(|error| format!("invalid EXDATE '{line}': {error}"))?;
            excluded.insert(slot.key);
        }
    }
    Ok(excluded)
}

fn select_batch(
    input: &CatchUpInput<'_>,
    stamp: &PreparedStamp<'_>,
    arrived_slots: impl IntoIterator<Item = Result<NativeSlot, WorkspaceError>>,
) -> Result<CatchUpBatch, WorkspaceError> {
    let mut batch = CatchUpBatch::default();
    let mut staged = HashSet::new();
    let mut selected = HashSet::new();
    for slot in arrived_slots {
        let slot = slot?;
        let root = occurrence_id(input.habit_id, &slot.key);
        if input.excluded.contains(&slot.key)
            || input.occupied.contains(&root)
            || selected.contains(&root)
        {
            continue;
        }
        if batch.occurrences.len() == input.budget {
            batch.has_backlog = true;
            break;
        }
        let occurrence = stamp.at(&slot)?;
        occurrence.require_absent(|id| input.occupied.contains(&id) || staged.contains(&id))?;
        selected.insert(root);
        staged.extend(occurrence.actions().iter().map(|action| action.id));
        batch.occurrences.push(occurrence);
    }
    Ok(batch)
}
