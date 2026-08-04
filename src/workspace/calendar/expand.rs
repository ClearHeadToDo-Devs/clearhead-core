use chrono::{DateTime, Local};

use super::ics::{ICSPlan, OccurrenceOverride, canonical_occurrence_key, occurrence_action_id};
use crate::workspace::actions::{Action, ActionState};

// ============================================================================
// Occurrence projection (render, don't file)
// ============================================================================

/// Render a recurring (or one-shot) Plan's next `window` future occurrences as
/// [`Action`]s, applying its deviations.
///
/// Pure and filesystem-free — this is the projection that *replaces* materialized
/// expansion for the client surface: occurrences exist only as returned Actions,
/// never as `.actions` lines. `EXDATE` slots are skipped (and do not consume a
/// window slot); a `RECURRENCE-ID` override overlays the occurrence at its slot.
/// Each occurrence's identity is the deterministic [`occurrence_action_id`] over
/// the [`canonical_occurrence_key`], so it is stable across reloads and peers,
/// and its `plan_id` links it back to the master in memory.
pub fn render_occurrences(ics_plan: &ICSPlan, now: DateTime<Local>, window: u32) -> Vec<Action> {
    let Some(plan_uid) = ics_plan.plan.external_id.as_deref() else {
        return Vec::new();
    };
    let Some(dtstart) = ics_plan.plan.dtstart else {
        return Vec::new();
    };
    if window == 0 {
        return Vec::new();
    }

    let slots: Vec<DateTime<Local>> = if ics_plan.plan.recurrence.is_some() {
        ics_plan
            .plan
            .expand_occurrences(dtstart, 1000)
            .into_iter()
            .map(|dt| dt.with_timezone(&Local))
            .filter(|&dt| dt >= now)
            .collect()
    } else if dtstart >= now {
        vec![dtstart]
    } else {
        vec![]
    };

    let mut out = Vec::new();
    for slot in slots {
        if out.len() as u32 >= window {
            break;
        }
        if ics_plan.exdates.contains(&canonical_occurrence_key(slot)) {
            continue; // excluded slot — does not occupy a window position
        }
        out.push(render_occurrence(ics_plan, plan_uid, slot));
    }
    out
}

/// Build one occurrence [`Action`] for `slot`, applying any `RECURRENCE-ID`
/// override.
///
/// The single definition of occurrence field-mapping, shared by the projection
/// window ([`render_occurrences`]) and the single-token stamper, so a materialized
/// occurrence and its (soon-retired) projection are byte-identical where they
/// overlap. The caller is responsible for EXDATE filtering.
pub(crate) fn render_occurrence(
    ics_plan: &ICSPlan,
    plan_uid: &str,
    slot: DateTime<Local>,
) -> Action {
    let key = canonical_occurrence_key(slot);
    let mut action = Action {
        id: occurrence_action_id(plan_uid, &key),
        state: ActionState::NotStarted,
        name: ics_plan.plan.name.clone(),
        scheduled_at: Some(slot),
        plan_id: Some(ics_plan.plan.id),
        // The occurrence handle: `plan_id` locates the master (and its UID and
        // file), and this canonical slot key names the slot a deviation write
        // targets. Carry the key rather than re-deriving it from `scheduled_at`,
        // which an override may move.
        external_occurrence_key: Some(key.clone()),
        ..Default::default()
    };
    if let Some(over) = ics_plan.overrides.get(&key) {
        apply_override(&mut action, over);
    }
    action
}

/// Overlay a `RECURRENCE-ID` override onto its rendered occurrence. `None`
/// fields inherit the value already rendered from the master.
fn apply_override(action: &mut Action, over: &OccurrenceOverride) {
    action.state = over.state;
    if over.scheduled_at.is_some() {
        action.scheduled_at = over.scheduled_at;
    }
    if over.due_date.is_some() {
        action.due_date = over.due_date;
    }
    action.completed_at = over.completed_at;
    if let Some(title) = &over.title {
        action.name = title.clone();
    }
    if over.description.is_some() {
        action.description = over.description.clone();
    }
}

// ============================================================================
// Single-token advancement (materialize the present, jump forward)
// ============================================================================

/// The slot the plan's single active occurrence should occupy.
///
/// A recurring plan carries **one** active token at a time. This computes where
/// that token belongs given an optional `after_exclusive` floor and `now`:
///
/// - `after_exclusive == None` → the **initial** slot: the first non-EXDATE'd
///   occurrence `>= now` (next upcoming). A plan whose start is long past does not
///   backlog — its first token is simply the next occurrence.
/// - `after_exclusive == Some(resolved)` → the **advance** after resolving that
///   slot: the first non-EXDATE'd occurrence strictly after `resolved` **and**
///   `>= now`. This is **jump-forward** — resolving a long-overdue occurrence lands
///   on the next *upcoming* slot, never replaying the missed ones (they are
///   never-weres, not skips).
///
/// `None` when the series yields no such slot (no UID/DTSTART, or exhausted within
/// the expansion cap). A UID is required: without stable occurrence identity there
/// is nothing to stamp or key a deviation on.
pub fn next_active_slot(
    ics_plan: &ICSPlan,
    after_exclusive: Option<DateTime<Local>>,
    now: DateTime<Local>,
) -> Option<DateTime<Local>> {
    ics_plan.plan.external_id.as_ref()?;
    let dtstart = ics_plan.plan.dtstart?;

    // Cap mirrors `render_occurrences`; a multi-year gap on a *daily* series is the
    // one shape this under-expands (deferred, same edge as the projection window).
    let slots: Vec<DateTime<Local>> = if ics_plan.plan.recurrence.is_some() {
        ics_plan
            .plan
            .expand_occurrences(dtstart, 1000)
            .into_iter()
            .map(|dt| dt.with_timezone(&Local))
            .collect()
    } else {
        vec![dtstart]
    };

    slots
        .into_iter()
        .filter(|&slot| after_exclusive.is_none_or(|a| slot > a))
        .filter(|&slot| slot >= now)
        .find(|slot| !ics_plan.exdates.contains(&canonical_occurrence_key(*slot)))
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Plan, Recurrence};
    use chrono::TimeZone;
    use uuid::Uuid;

    fn make_plan(name: &str, uid: &str, dtstart: DateTime<Local>, rrule: Option<&str>) -> Plan {
        Plan {
            name: name.to_string(),
            external_id: Some(uid.to_string()),
            dtstart: Some(dtstart),
            recurrence: rrule.map(|r| Recurrence {
                frequency: r.to_string(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    fn now() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap()
    }

    // ---- render_occurrences: frame-preserving EXDATE agreement ----

    /// Regression for the DST-drift bug: a UTC-anchored winter master whose
    /// EXDATE lands on a *summer-side* slot (across a DST boundary) must skip
    /// that slot. Before the frame fix, expansion re-read the master as floating
    /// Local wall-clock and drifted the summer occurrence by the DST offset, so
    /// its canonical key (07:00Z on Pacific) never matched the EXDATE key
    /// (08:00Z) and the excluded occurrence still rendered. Anchoring expansion
    /// in UTC makes both paths agree on any machine timezone.
    #[test]
    fn exdate_across_dst_boundary_skips_the_occurrence() {
        use chrono::Utc;
        use std::collections::{BTreeMap, BTreeSet};

        let uid = "weekly@example.com";
        // 08:00Z every Wednesday, starting before US/Pacific DST (2026-03-08)…
        let dtstart = Utc
            .with_ymd_and_hms(2026, 2, 25, 8, 0, 0)
            .unwrap()
            .with_timezone(&Local);
        // …and the excluded slot is on the summer side of that boundary.
        let excluded = Utc
            .with_ymd_and_hms(2026, 3, 11, 8, 0, 0)
            .unwrap()
            .with_timezone(&Local);
        let present = Utc
            .with_ymd_and_hms(2026, 3, 18, 8, 0, 0)
            .unwrap()
            .with_timezone(&Local);

        let plan = make_plan("weekly review", uid, dtstart, Some("weekly"));
        let ics_plan = ICSPlan {
            path: std::path::PathBuf::from("weekly.ics"),
            exdates: BTreeSet::from([canonical_occurrence_key(excluded)]),
            overrides: BTreeMap::new(),
            plan,
        };

        let now = Utc
            .with_ymd_and_hms(2026, 1, 1, 0, 0, 0)
            .unwrap()
            .with_timezone(&Local);
        let ids: Vec<Uuid> = render_occurrences(&ics_plan, now, 6)
            .into_iter()
            .map(|a| a.id)
            .collect();

        let excluded_id = occurrence_action_id(uid, &canonical_occurrence_key(excluded));
        let present_id = occurrence_action_id(uid, &canonical_occurrence_key(present));
        assert!(
            !ids.contains(&excluded_id),
            "post-DST EXDATE slot must be skipped"
        );
        assert!(
            ids.contains(&present_id),
            "the following slot must still render"
        );
    }

    // ---- plan without external_id is skipped ----

    #[test]
    fn plan_without_external_id_is_skipped() {
        use std::collections::{BTreeMap, BTreeSet};
        let dtstart = Local.with_ymd_and_hms(2026, 5, 3, 10, 0, 0).unwrap();
        let ics = ICSPlan {
            path: std::path::PathBuf::from("p.ics"),
            plan: Plan {
                name: "no uid".to_string(),
                dtstart: Some(dtstart),
                external_id: None,
                ..Default::default()
            },
            exdates: BTreeSet::new(),
            overrides: BTreeMap::new(),
        };
        // No UID → no stable occurrence identity → nothing projects.
        assert!(render_occurrences(&ics, now(), 3).is_empty());
    }

    // ---- next_active_slot: single token, jump-forward ----

    fn weekly_ics(dtstart: DateTime<Local>) -> ICSPlan {
        use std::collections::{BTreeMap, BTreeSet};
        ICSPlan {
            path: std::path::PathBuf::from("w.ics"),
            plan: make_plan("weekly review", "wk@example.com", dtstart, Some("weekly")),
            exdates: BTreeSet::new(),
            overrides: BTreeMap::new(),
        }
    }

    #[test]
    fn initial_active_slot_is_the_next_upcoming() {
        // dtstart three Sundays before `now` (2026-05-01 Fri): 4/12, 4/19, 4/26,
        // 5/3, … The initial token is 5/3 — the first slot >= now — never a past one.
        let dtstart = Local.with_ymd_and_hms(2026, 4, 12, 9, 0, 0).unwrap();
        let slot = next_active_slot(&weekly_ics(dtstart), None, now()).unwrap();
        assert_eq!(slot, Local.with_ymd_and_hms(2026, 5, 3, 9, 0, 0).unwrap());
    }

    #[test]
    fn advance_jumps_forward_over_missed_slots() {
        // Resolve the long-stale first slot; the next active is the first slot
        // >= now (5/3), not the immediate successor (4/19, also past). Missed
        // intervening slots are skipped, not replayed one at a time.
        let dtstart = Local.with_ymd_and_hms(2026, 4, 12, 9, 0, 0).unwrap();
        let slot = next_active_slot(&weekly_ics(dtstart), Some(dtstart), now()).unwrap();
        assert_eq!(slot, Local.with_ymd_and_hms(2026, 5, 3, 9, 0, 0).unwrap());
        assert!(
            slot > dtstart + chrono::Duration::days(7),
            "not the mere successor"
        );
    }

    #[test]
    fn advance_at_cadence_is_the_next_period() {
        // Keeping up: `now` sits on the active slot; resolving it advances exactly
        // one period.
        let dtstart = now();
        let next = next_active_slot(&weekly_ics(dtstart), Some(dtstart), dtstart).unwrap();
        assert_eq!(next, dtstart + chrono::Duration::days(7));
    }

    #[test]
    fn active_slot_skips_an_exdated_slot() {
        use std::collections::BTreeSet;
        let dtstart = now();
        let mut ics = weekly_ics(dtstart);
        // EXDATE the would-be initial slot → the token is the following occurrence.
        ics.exdates = BTreeSet::from([canonical_occurrence_key(dtstart)]);
        let slot = next_active_slot(&ics, None, now()).unwrap();
        assert_eq!(slot, dtstart + chrono::Duration::days(7));
    }
}
