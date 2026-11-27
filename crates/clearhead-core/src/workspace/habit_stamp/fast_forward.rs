use std::collections::HashSet;

use chrono::{DateTime, FixedOffset};
use serde_json::{Value, json};
use uuid::Uuid;

use crate::domain::habit_recurrence::text_slots;
use crate::domain::habit_slots::{SlotKey, occurrence_id};
use crate::workspace::WorkspaceError;
use crate::workspace::sidecar::CharterMetadata;

use super::{StampSource, habit_from_root, text_exclusions};

#[derive(Debug)]
pub struct FastForward {
    pub habit_name: String,
    pub skipped: usize,
    /// First eligible original slot still in the future; never an Action.
    pub next_slot: Option<SlotKey>,
}

/// Record exclusions for one Habit, leaving its anchor, rule, templates and
/// materialized history alone. A newly introduced slot under a later rule is
/// not hidden by a cutoff. The caller decides whether to persist the sidecar.
pub fn fast_forward_text_habit(
    source: StampSource<'_>,
    habit_id: Uuid,
    now: &DateTime<FixedOffset>,
    occupied: &HashSet<Uuid>,
    metadata: &mut CharterMetadata,
) -> Result<FastForward, WorkspaceError> {
    let root = source.root(habit_id)?;
    let refuse = |code, message: &str| {
        WorkspaceError::Actions(source.message(source.range(habit_id), code, message))
    };
    if root.state.is_terminal() {
        return Err(refuse(
            "habit-closed",
            "a closed Habit cannot be fast-forwarded",
        ));
    }
    if !source.has_written_id(habit_id) {
        return Err(refuse(
            "habit-id-missing",
            "persist the Habit id before fast-forwarding",
        ));
    }
    let habit = habit_from_root(root, &source.doc.actions);
    habit.validate().map_err(|error| refuse("E002", &error))?;
    let anchor = root.planned.expect("validated Habit").start;
    let mut excluded = text_exclusions(anchor, metadata.extra.get("habits"), habit_id)
        .map_err(|error| refuse("habit-exclusions", &error))?;
    let slots = text_slots(anchor, &habit.recurrence_rule, now)
        .map_err(|error| refuse("slot-time", &error))?;
    let mut additions = Vec::new();
    let mut next_slot = None;
    for (arrived, slot) in slots {
        let key = slot.map_err(|error| refuse("slot-time", &error))?.key;
        if occupied.contains(&occurrence_id(habit_id, &key)) || !excluded.insert(key.clone()) {
            continue;
        }
        if !arrived {
            next_slot = Some(key);
            break;
        }
        additions.push(Value::String(exdate(&key)));
    }
    let skipped = additions.len();
    if skipped != 0 {
        // Shapes have already passed text_exclusions. Edit only EXDATE to
        // preserve existing properties, notes and extension fields.
        metadata
            .extra
            .entry("habits".into())
            .or_insert_with(|| json!({}))
            .as_object_mut()
            .expect("validated habits metadata")
            .entry(habit_id.to_string())
            .or_insert_with(|| json!({}))
            .as_object_mut()
            .expect("validated Habit metadata")
            .entry("exdate")
            .or_insert_with(|| json!([]))
            .as_array_mut()
            .expect("validated EXDATE metadata")
            .extend(additions);
    }
    Ok(FastForward {
        habit_name: root.name.clone(),
        skipped,
        next_slot,
    })
}

fn exdate(key: &SlotKey) -> String {
    let (frame, value) = key.as_str().split_once(':').expect("canonical slot key");
    let property = if frame == "D" {
        "EXDATE;VALUE=DATE"
    } else {
        "EXDATE"
    };
    format!("{property}:{}", value.replace(['-', ':'], ""))
}
