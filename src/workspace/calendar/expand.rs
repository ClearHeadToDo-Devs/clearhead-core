use std::collections::HashMap;

use chrono::{DateTime, Local};
use uuid::Uuid;

use crate::domain::Plan;
use crate::workspace::actions::{Action, ActionState};
use super::ics::occurrence_action_id;

// ============================================================================
// Public types
// ============================================================================

/// Instance-count bounds for schedule expansion.
///
/// Both values are resolved per-plan: if the plan carries a `primary_instances`
/// override (from the `upcoming:` recurring VTODO DESCRIPTION directive), that overrides
/// `primary_instances` here. `total_instances` is global-only for now.
#[derive(Debug, Clone)]
pub struct ExpansionConfig {
    /// Total instances generated per schedule across both files.
    /// Must be greater than `primary_instances`.
    pub total_instances: u32,
    /// Instances placed in the primary `<charter>.actions` file.
    /// Remainder go to `<charter>.upcoming.actions`.
    pub primary_instances: u32,
}

impl Default for ExpansionConfig {
    fn default() -> Self {
        Self {
            total_instances: 2,
            primary_instances: 1,
        }
    }
}

/// Output of [`expand_plans_into_actions`].
///
/// `primary` actions go into `<charter>.actions`.
/// `upcoming` actions go into `<charter>.upcoming.actions`.
#[derive(Debug, Default)]
pub struct ExpandResult {
    pub primary: Vec<Action>,
    pub upcoming: Vec<Action>,
}

// ============================================================================
// Core expansion logic
// ============================================================================

/// Expand a set of plans into new actions split across primary and upcoming files.
///
/// Pure — no IO. Callers own file discovery, reading, and writing.
///
/// ## Slot accounting (per plan)
///
/// For each plan the function iterates future occurrences in chronological
/// order, computing the deterministic UUID for each. Existing occurrences in
/// `existing_primary` and `existing_upcoming` are matched by UUID:
///
/// - An occurrence in either file with an **open** state (`NotStarted` /
///   `InProgress`) counts toward that file's cap.
/// - An occurrence in either file with a **closed** state (`Completed` /
///   `Cancelled`) does **not** count — it vacates its slot so the next
///   occurrence fills it.
/// - An occurrence not present in either file is generated to whichever file
///   still has room.
///
/// ## Idempotency
///
/// Any occurrence whose UUID already appears in either existing slice
/// (regardless of state) is always skipped — it will not be regenerated.
/// Running expansion multiple times for the same inputs is always safe.
pub fn expand_plans_into_actions(
    plans: &[Plan],
    existing_primary: &[Action],
    existing_upcoming: &[Action],
    now: DateTime<Local>,
    config: &ExpansionConfig,
) -> ExpandResult {
    // Build UUID → state maps for O(1) per-occurrence lookups.
    let primary_by_id: HashMap<Uuid, &ActionState> =
        existing_primary.iter().map(|a| (a.id, &a.state)).collect();
    let upcoming_by_id: HashMap<Uuid, &ActionState> =
        existing_upcoming.iter().map(|a| (a.id, &a.state)).collect();

    let mut result = ExpandResult::default();

    for plan in plans {
        let vevent_uid = match &plan.external_id {
            Some(uid) => uid.as_str(),
            None => continue,
        };
        let Some(dtstart) = plan.dtstart else {
            continue;
        };

        // Per-plan primary cap: plan directive overrides global default
        let primary_cap = plan
            .primary_instances
            .unwrap_or(config.primary_instances)
            .min(config.total_instances); // safety clamp

        let upcoming_cap = config.total_instances.saturating_sub(primary_cap);

        // Enumerate future occurrences; we cap the search at a generous limit
        // to prevent unbounded iteration on infinite RRULEs.
        let occurrences: Vec<DateTime<Local>> = if plan.recurrence.is_some() {
            plan.expand_occurrences(dtstart, 1000)
                .into_iter()
                .map(|dt| dt.with_timezone(&Local))
                .filter(|&dt| dt >= now)
                .collect()
        } else if dtstart >= now {
            vec![dtstart]
        } else {
            vec![]
        };

        // Single-pass slot accounting:
        //   primary_slots_used counts open existing + new generated for primary
        //   upcoming_slots_used counts open existing + new generated for upcoming
        let mut primary_slots_used: u32 = 0;
        let mut upcoming_slots_used: u32 = 0;

        for occ_local in occurrences {
            if primary_slots_used >= primary_cap && upcoming_slots_used >= upcoming_cap {
                break;
            }

            let occ_key = occ_local.to_rfc3339();
            let action_id = occurrence_action_id(vevent_uid, &occ_key);

            // Check if this occurrence is already represented in either file.
            if let Some(state) = primary_by_id.get(&action_id) {
                if is_open(state) {
                    primary_slots_used = primary_slots_used.saturating_add(1);
                }
                continue; // already in primary — skip regardless of state
            }
            if let Some(state) = upcoming_by_id.get(&action_id) {
                if is_open(state) {
                    upcoming_slots_used = upcoming_slots_used.saturating_add(1);
                }
                continue; // already in upcoming — skip regardless of state
            }

            // Not in any file — generate to whichever slot still has room.
            let action = Action {
                id: action_id,
                state: ActionState::NotStarted,
                name: plan.name.clone(),
                scheduled_at: Some(occ_local),
                created_at: Some(now),
                ..Default::default()
            };

            if primary_slots_used < primary_cap {
                result.primary.push(action);
                primary_slots_used += 1;
            } else if upcoming_slots_used < upcoming_cap {
                result.upcoming.push(action);
                upcoming_slots_used += 1;
            }
        }
    }

    result
}

// ============================================================================
// Helpers
// ============================================================================

fn is_open(state: &ActionState) -> bool {
    matches!(state, ActionState::NotStarted | ActionState::InProgress)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Plan, Recurrence};
    use chrono::TimeZone;

    fn cfg(total: u32, primary: u32) -> ExpansionConfig {
        ExpansionConfig { total_instances: total, primary_instances: primary }
    }

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

    fn make_plan_with_primary_override(
        name: &str,
        uid: &str,
        dtstart: DateTime<Local>,
        rrule: Option<&str>,
        primary_instances: u32,
    ) -> Plan {
        Plan {
            primary_instances: Some(primary_instances),
            ..make_plan(name, uid, dtstart, rrule)
        }
    }

    fn now() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap()
    }

    // ---- one-shot plans ----

    #[test]
    fn one_shot_plan_produces_primary_action() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 3, 10, 0, 0).unwrap();
        let plan = make_plan("review specs", "uid-1", dtstart, None);
        let result = expand_plans_into_actions(&[plan], &[], &[], now(), &cfg(2, 1));

        assert_eq!(result.primary.len(), 1);
        assert!(result.upcoming.is_empty());
        assert_eq!(result.primary[0].name, "review specs");
    }

    #[test]
    fn one_shot_plan_in_the_past_is_skipped() {
        let dtstart = Local.with_ymd_and_hms(2026, 4, 1, 10, 0, 0).unwrap();
        let plan = make_plan("past event", "uid-past", dtstart, None);
        let result = expand_plans_into_actions(&[plan], &[], &[], now(), &cfg(2, 1));

        assert!(result.primary.is_empty());
        assert!(result.upcoming.is_empty());
    }

    // ---- recurring plans ----

    #[test]
    fn recurring_plan_fills_primary_then_upcoming() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 4, 9, 0, 0).unwrap();
        let plan = make_plan("standup", "uid-standup", dtstart, Some("daily"));
        let result = expand_plans_into_actions(&[plan], &[], &[], now(), &cfg(2, 1));

        assert_eq!(result.primary.len(), 1, "one primary slot");
        assert_eq!(result.upcoming.len(), 1, "one upcoming slot");
    }

    #[test]
    fn recurring_plan_with_total_3_primary_1_gives_1_primary_2_upcoming() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 4, 9, 0, 0).unwrap();
        let plan = make_plan("standup", "uid-standup2", dtstart, Some("daily"));
        let result = expand_plans_into_actions(&[plan], &[], &[], now(), &cfg(3, 1));

        assert_eq!(result.primary.len(), 1);
        assert_eq!(result.upcoming.len(), 2);
    }

    // ---- idempotency ----

    #[test]
    fn existing_open_primary_occupies_its_slot() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 3, 10, 0, 0).unwrap();
        let uid = "uid-idem";
        let action_id = occurrence_action_id(uid, &dtstart.to_rfc3339());

        let existing = Action {
            id: action_id,
            state: ActionState::NotStarted,
            name: "review specs".to_string(),
            ..Default::default()
        };

        let plan = make_plan("review specs", uid, dtstart, None);
        // primary has 1 open already → no new primaries needed
        let result = expand_plans_into_actions(&[plan], &[existing], &[], now(), &cfg(2, 1));

        assert!(result.primary.is_empty(), "slot already occupied");
        assert!(result.upcoming.is_empty(), "one-shot: no upcoming slot");
    }

    #[test]
    fn completed_primary_vacates_slot_for_next_occurrence() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 4, 9, 0, 0).unwrap();
        let uid = "uid-completed";
        let action_id = occurrence_action_id(uid, &dtstart.to_rfc3339());

        // Completed action in primary — does not count as open
        let completed = Action {
            id: action_id,
            state: ActionState::Completed,
            name: "standup".to_string(),
            ..Default::default()
        };

        let plan = make_plan("standup", uid, dtstart, Some("daily"));
        let result = expand_plans_into_actions(&[plan], &[completed], &[], now(), &cfg(2, 1));

        // primary slot is empty (completed doesn't count) → one new primary generated
        assert_eq!(result.primary.len(), 1);
        // upcoming slot also needs filling → one new upcoming
        assert_eq!(result.upcoming.len(), 1);
        // the new primary must be a different occurrence (not the completed one)
        assert_ne!(result.primary[0].id, action_id);
    }

    #[test]
    fn cancelled_upcoming_vacates_slot_for_next_occurrence() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 4, 9, 0, 0).unwrap();
        let uid = "uid-cancelled-up";
        // second occurrence goes to upcoming
        let second_occ =
            Local.with_ymd_and_hms(2026, 5, 5, 9, 0, 0).unwrap();
        let cancelled_id = occurrence_action_id(uid, &second_occ.to_rfc3339());

        let cancelled = Action {
            id: cancelled_id,
            state: ActionState::Cancelled,
            name: "standup".to_string(),
            ..Default::default()
        };

        let plan = make_plan("standup", uid, dtstart, Some("daily"));
        let result = expand_plans_into_actions(&[plan], &[], &[cancelled], now(), &cfg(2, 1));

        // upcoming slot vacated → a new upcoming occurrence should be generated
        assert_eq!(result.primary.len(), 1);
        assert_eq!(result.upcoming.len(), 1);
        // the new upcoming must not be the cancelled one
        assert_ne!(result.upcoming[0].id, cancelled_id);
    }

    // ---- per-plan primary_instances override ----

    #[test]
    fn per_plan_primary_override_zero_sends_all_to_upcoming() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 4, 9, 0, 0).unwrap();
        let plan =
            make_plan_with_primary_override("standup", "uid-override", dtstart, Some("daily"), 0);
        let result = expand_plans_into_actions(&[plan], &[], &[], now(), &cfg(2, 0));

        assert!(result.primary.is_empty());
        assert_eq!(result.upcoming.len(), 2);
    }

    // ---- plan without external_id is skipped ----

    #[test]
    fn plan_without_external_id_is_skipped() {
        let dtstart = Local.with_ymd_and_hms(2026, 5, 3, 10, 0, 0).unwrap();
        let plan = Plan {
            name: "no uid".to_string(),
            dtstart: Some(dtstart),
            external_id: None,
            ..Default::default()
        };
        let result = expand_plans_into_actions(&[plan], &[], &[], now(), &cfg(2, 1));
        assert!(result.primary.is_empty());
        assert!(result.upcoming.is_empty());
    }
}
