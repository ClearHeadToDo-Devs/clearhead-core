//! Three-way reconcile for CalDAV sync ([decision 31]).
//!
//! Edits to a scheduled action can originate on either side — in ClearHead or
//! in the calendar app — and we honor both without knowing *when* anything
//! changed. The trick is a stored merge base: the action's `scheduled_at` as of
//! the last sync. That gives three observable times for one action:
//!
//! - **A** — the action's current `scheduled_at`.
//! - **B** — the merge base (`scheduled_at_sync` in the sidecar).
//! - **C** — the `DTSTART` in the `.ics`.
//!
//! Each is an [`Option`]: `None` is "no scheduled time", a first-class value
//! here — an unscheduled action, a deleted event, or (for B) "never synced".
//!
//! Comparing A and C *each against B* tells us who moved, with no timestamps.
//! The eight rows of decision 31's table collapse to four branches once `None`
//! is treated as an ordinary value:
//!
//! | `A != B` | `C != B` | outcome                                    |
//! |----------|----------|--------------------------------------------|
//! | no       | no       | [`NoOp`] — nothing moved                    |
//! | yes      | no       | [`TakeAction`] — push A to the `.ics`       |
//! | no       | yes      | [`TakeCalendar`] — pull C into the action   |
//! | yes      | yes      | both moved (see below)                      |
//!
//! [`NoOp`]: Reconcile::NoOp
//! [`TakeAction`]: Reconcile::TakeAction
//! [`TakeCalendar`]: Reconcile::TakeCalendar
//!
//! When both sides moved we look once more: if they moved to the **same** time
//! the change is a clean convergence ([`Converged`]) — no payload to write, only
//! the stale merge base to restamp. Only genuinely divergent moves are a
//! [`Conflict`]. This refines decision 31's literal `changed/changed → conflict`
//! row to `changed/changed & A != C`, matching how a 3-way merge treats two
//! identical edits.
//!
//! [`Converged`]: Reconcile::Converged
//! [`Conflict`]: Reconcile::Conflict
//!
//! This module is **pure** — it decides, it does not write. Applying the
//! outcome (and stamping B in the same atomic commit as the winning payload) is
//! the reconcile shell's job.
//!
//! [decision 31]: ../../../DECISIONS.md

use chrono::{DateTime, Local};

/// A nullable scheduled time — `None` means "no scheduled time".
type Time = Option<DateTime<Local>>;

/// The outcome of reconciling one action's scheduled time across the three
/// sources. Each variant carries the resolved value so the shell can act
/// without re-deriving it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reconcile {
    /// A == B == C. Nothing moved; nothing to write.
    NoOp,
    /// Only the action moved — it wins. Write its time to the `.ics` and
    /// restamp the merge base. `None` means the action was unscheduled: remove
    /// the `.ics`.
    TakeAction(Time),
    /// Only the calendar moved — it wins. Write its time into the action and
    /// restamp the merge base. `None` means the event was deleted: clear the
    /// action's `scheduled_at`.
    TakeCalendar(Time),
    /// Both sides moved, but to the *same* time. No payload change — only the
    /// stale merge base needs restamping to the agreed value.
    Converged(Time),
    /// Both sides moved to *different* times. We cannot pick safely — defer to
    /// the human.
    Conflict { action: Time, calendar: Time },
}

/// Decide how to reconcile one action's scheduled time from the three sources.
///
/// `action` is A, `base` is B (the merge base), `ics` is C. See the module docs
/// for the full table.
pub fn reconcile(action: Time, base: Time, ics: Time) -> Reconcile {
    let action_moved = action != base;
    let calendar_moved = ics != base;

    match (action_moved, calendar_moved) {
        (false, false) => Reconcile::NoOp,
        (true, false) => Reconcile::TakeAction(action),
        (false, true) => Reconcile::TakeCalendar(ics),
        (true, true) if action == ics => Reconcile::Converged(action),
        (true, true) => Reconcile::Conflict { action, calendar: ics },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    /// Three distinct, fixed local times for table construction.
    fn t1() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 4, 27, 10, 0, 0).unwrap()
    }
    fn t2() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 4, 28, 14, 30, 0).unwrap()
    }
    fn t3() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap()
    }

    // --- the synced table: B is set (a prior sync established a merge base) ---

    #[test]
    fn all_equal_is_noop() {
        assert_eq!(reconcile(Some(t1()), Some(t1()), Some(t1())), Reconcile::NoOp);
    }

    #[test]
    fn action_moved_calendar_still_takes_action() {
        // A changed, C still at base → the action wins.
        assert_eq!(
            reconcile(Some(t2()), Some(t1()), Some(t1())),
            Reconcile::TakeAction(Some(t2()))
        );
    }

    #[test]
    fn calendar_moved_action_still_takes_calendar() {
        // C changed, A still at base → the calendar wins.
        assert_eq!(
            reconcile(Some(t1()), Some(t1()), Some(t2())),
            Reconcile::TakeCalendar(Some(t2()))
        );
    }

    #[test]
    fn both_moved_differently_is_conflict() {
        assert_eq!(
            reconcile(Some(t2()), Some(t1()), Some(t3())),
            Reconcile::Conflict { action: Some(t2()), calendar: Some(t3()) }
        );
    }

    #[test]
    fn both_moved_to_same_time_is_converged() {
        // The refinement: identical edits on both sides are not a conflict.
        assert_eq!(
            reconcile(Some(t2()), Some(t1()), Some(t2())),
            Reconcile::Converged(Some(t2()))
        );
    }

    // --- removal rows: the moved value is None ---

    #[test]
    fn action_unscheduled_removes_ics() {
        // A removed, C still at base → take the action's removal: drop the .ics.
        assert_eq!(
            reconcile(None, Some(t1()), Some(t1())),
            Reconcile::TakeAction(None)
        );
    }

    #[test]
    fn event_deleted_clears_action() {
        // C removed, A still at base → take the calendar's removal: clear the action.
        assert_eq!(
            reconcile(Some(t1()), Some(t1()), None),
            Reconcile::TakeCalendar(None)
        );
    }

    #[test]
    fn action_removed_calendar_moved_is_conflict() {
        assert_eq!(
            reconcile(None, Some(t1()), Some(t2())),
            Reconcile::Conflict { action: None, calendar: Some(t2()) }
        );
    }

    #[test]
    fn action_moved_calendar_removed_is_conflict() {
        assert_eq!(
            reconcile(Some(t2()), Some(t1()), None),
            Reconcile::Conflict { action: Some(t2()), calendar: None }
        );
    }

    // --- first sync: B is None ("never synced") — None is just another value ---

    #[test]
    fn never_synced_unscheduled_is_noop() {
        assert_eq!(reconcile(None, None, None), Reconcile::NoOp);
    }

    #[test]
    fn first_sync_of_new_action_creates_ics() {
        // A scheduled, never synced, no .ics yet → push A out (create the .ics).
        assert_eq!(
            reconcile(Some(t1()), None, None),
            Reconcile::TakeAction(Some(t1()))
        );
    }

    #[test]
    fn calendar_created_event_imports_into_action() {
        // An event exists for an unscheduled, never-synced action → pull it in.
        assert_eq!(
            reconcile(None, None, Some(t1())),
            Reconcile::TakeCalendar(Some(t1()))
        );
    }

    #[test]
    fn never_synced_but_both_agree_is_converged() {
        // Both sides independently hold the same time, no merge base yet:
        // a clean convergence, not a conflict.
        assert_eq!(
            reconcile(Some(t1()), None, Some(t1())),
            Reconcile::Converged(Some(t1()))
        );
    }

    #[test]
    fn never_synced_and_both_differ_is_conflict() {
        assert_eq!(
            reconcile(Some(t1()), None, Some(t2())),
            Reconcile::Conflict { action: Some(t1()), calendar: Some(t2()) }
        );
    }
}
