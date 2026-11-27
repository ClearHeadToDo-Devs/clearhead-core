use std::collections::HashSet;

use chrono::{DateTime, FixedOffset};
use uuid::Uuid;

use crate::domain::habit_recurrence::arrived_text_slots;
use crate::domain::habit_slots::{NativeSlot, SlotKey, occurrence_id};
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
    let slots = arrived_text_slots(anchor, &stamp.recurrence_rule, now)
        .map_err(from_source)?
        .map(|slot| slot.map_err(from_source));
    select_batch(&input, &stamp, slots)
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
