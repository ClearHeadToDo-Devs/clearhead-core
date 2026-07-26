use std::collections::HashSet;

use chrono::{DateTime, Local};
use uuid::Uuid;

use crate::workspace::actions::{Action, ActionState};
use super::ics::{ICSPlan, OccurrenceOverride, canonical_occurrence_key, occurrence_action_id};

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
        let key = canonical_occurrence_key(slot);
        if ics_plan.exdates.contains(&key) {
            continue; // excluded slot — does not occupy a window position
        }
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
        out.push(action);
    }
    out
}

/// The one definition of the action union: append every plan's projected
/// occurrences to `actions`, skipping any whose id is already present so a
/// **materialized action always wins** over its projection (same occurrence
/// UUIDv5).
///
/// Every site that needs "materialized ∪ projected" calls this — the
/// `Workspace → DomainModel` lowering and the CLI read collectors — so the union
/// rule lives in exactly one place and no consumer can re-derive it differently.
pub fn extend_with_projected_occurrences(
    actions: &mut Vec<Action>,
    plans: &[ICSPlan],
    now: DateTime<Local>,
    window: u32,
) {
    let materialized: HashSet<Uuid> = actions.iter().map(|a| a.id).collect();
    for plan in plans {
        for occurrence in render_occurrences(plan, now, window) {
            if !materialized.contains(&occurrence.id) {
                actions.push(occurrence);
            }
        }
    }
}

/// Overlay a `RECURRENCE-ID` override onto its rendered occurrence. `None`
/// fields inherit the value already rendered from the master.
fn apply_override(action: &mut Action, over: &OccurrenceOverride) {
    action.state = over.state.clone();
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
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Plan, Recurrence};
    use chrono::TimeZone;

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
        let dtstart = Utc.with_ymd_and_hms(2026, 2, 25, 8, 0, 0).unwrap().with_timezone(&Local);
        // …and the excluded slot is on the summer side of that boundary.
        let excluded = Utc.with_ymd_and_hms(2026, 3, 11, 8, 0, 0).unwrap().with_timezone(&Local);
        let present = Utc.with_ymd_and_hms(2026, 3, 18, 8, 0, 0).unwrap().with_timezone(&Local);

        let plan = make_plan("weekly review", uid, dtstart, Some("weekly"));
        let ics_plan = ICSPlan {
            path: std::path::PathBuf::from("weekly.ics"),
            exdates: BTreeSet::from([canonical_occurrence_key(excluded)]),
            overrides: BTreeMap::new(),
            plan,
        };

        let now = Utc.with_ymd_and_hms(2026, 1, 1, 0, 0, 0).unwrap().with_timezone(&Local);
        let ids: Vec<Uuid> = render_occurrences(&ics_plan, now, 6)
            .into_iter()
            .map(|a| a.id)
            .collect();

        let excluded_id = occurrence_action_id(uid, &canonical_occurrence_key(excluded));
        let present_id = occurrence_action_id(uid, &canonical_occurrence_key(present));
        assert!(!ids.contains(&excluded_id), "post-DST EXDATE slot must be skipped");
        assert!(ids.contains(&present_id), "the following slot must still render");
    }

    #[test]
    fn extend_unions_occurrences_and_materialized_wins() {
        use std::collections::{BTreeMap, BTreeSet};

        let dtstart = Local.with_ymd_and_hms(2026, 5, 4, 9, 0, 0).unwrap();
        let ics = ICSPlan {
            path: std::path::PathBuf::from("p.ics"),
            plan: make_plan("standup", "uid-x", dtstart, Some("daily")),
            exdates: BTreeSet::new(),
            overrides: BTreeMap::new(),
        };

        // Take the first projected occurrence's id, then seed a *materialized*
        // action with the same id but different content.
        let occ_id = render_occurrences(&ics, now(), 1)[0].id;
        let mut actions = vec![Action {
            id: occ_id,
            name: "standup (hand-edited)".to_string(),
            ..Default::default()
        }];

        extend_with_projected_occurrences(&mut actions, std::slice::from_ref(&ics), now(), 3);

        // The materialized line wins: exactly one action carries that id, unchanged.
        assert_eq!(actions.iter().filter(|a| a.id == occ_id).count(), 1, "no duplicate slot");
        assert!(
            actions.iter().any(|a| a.id == occ_id && a.name == "standup (hand-edited)"),
            "materialized content is preserved, not overwritten by the projection"
        );
        // Later slots still project alongside it.
        assert!(actions.len() > 1, "further occurrences are unioned in");
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
}
