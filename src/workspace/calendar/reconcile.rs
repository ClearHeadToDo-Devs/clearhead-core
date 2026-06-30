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
use std::collections::HashMap;
use std::path::Path;
use uuid::Uuid;

use super::ics::parse_ics_file;
use super::plans::collect_plan_files_in;
use crate::domain::DomainModel;
use crate::workspace::store::WorkspaceError;

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

// ============================================================================
// Sync planning — the read-only shell
// ============================================================================

/// One standalone action's place in a planned sync.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyncEntry {
    pub action_id: Uuid,
    pub name: String,
    pub outcome: Reconcile,
}

/// Tally of planned outcomes by kind — the at-a-glance summary of a run.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SyncTally {
    pub take_action: usize,
    pub take_calendar: usize,
    pub converged: usize,
    pub conflict: usize,
}

/// What a sync run *would* do, computed without touching disk.
///
/// Only actionable entries are kept — `NoOp` actions (already in sync, or never
/// scheduled) are omitted. This is the dry-run; applying it is a separate step.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SyncReport {
    pub entries: Vec<SyncEntry>,
}

impl SyncReport {
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Count planned outcomes by kind.
    pub fn tally(&self) -> SyncTally {
        let mut t = SyncTally::default();
        for entry in &self.entries {
            match entry.outcome {
                Reconcile::NoOp => {}
                Reconcile::TakeAction(_) => t.take_action += 1,
                Reconcile::TakeCalendar(_) => t.take_calendar += 1,
                Reconcile::Converged(_) => t.converged += 1,
                Reconcile::Conflict { .. } => t.conflict += 1,
            }
        }
        t
    }

    /// The conflict entries — the ones a human must resolve.
    pub fn conflicts(&self) -> impl Iterator<Item = &SyncEntry> {
        self.entries
            .iter()
            .filter(|e| matches!(e.outcome, Reconcile::Conflict { .. }))
    }
}

/// Classify every standalone scheduled action against the calendar, with no
/// writes. `ics_dates` maps an action's id to the `DTSTART` in its `.ics` (`C`);
/// a missing key means no `.ics` exists for that action.
///
/// Plan-generated actions (`external_schedule_id` set) are skipped — they are
/// represented by their plan's recurring master, never individually mirrored.
pub fn plan_sync(
    model: &DomainModel,
    ics_dates: &HashMap<Uuid, Time>,
) -> SyncReport {
    let mut entries = Vec::new();

    for charter in &model.charters {
        for action in &charter.actions {
            if action.external_schedule_id.is_some() {
                continue;
            }

            let a = action.scheduled_at;
            let b = action.scheduled_at_sync;
            let c = ics_dates.get(&action.id).copied().flatten();

            let outcome = reconcile(a, b, c);
            if outcome != Reconcile::NoOp {
                entries.push(SyncEntry {
                    action_id: action.id,
                    name: action.name.clone(),
                    outcome,
                });
            }
        }
    }

    SyncReport { entries }
}

/// Read the calendar side (`C`) for a sync: parse every `.ics` under
/// `plans_root` and map each action-mirror's id to its `DTSTART`.
///
/// Only VEVENT UIDs that parse as a UUID are kept — recurring-plan files, whose
/// UIDs are plan identities, are ignored here. This is the only filesystem touch
/// in the read path; [`plan_sync`] stays pure on top of the returned map.
pub fn read_ics_dates(plans_root: &Path) -> Result<HashMap<Uuid, Time>, WorkspaceError> {
    let mut dates = HashMap::new();

    for entry in collect_plan_files_in(plans_root, None)? {
        for ics_plan in parse_ics_file(&entry.path)? {
            let Some(uid) = &ics_plan.plan.external_id else {
                continue;
            };
            if let Ok(id) = Uuid::parse_str(uid) {
                dates.insert(id, ics_plan.plan.dtstart);
            }
        }
    }

    Ok(dates)
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

#[cfg(test)]
mod shell_tests {
    use super::*;
    use crate::domain::{Action, Charter};
    use chrono::TimeZone;

    fn t1() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 4, 27, 10, 0, 0).unwrap()
    }
    fn t2() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 4, 28, 14, 30, 0).unwrap()
    }
    fn t3() -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap()
    }

    fn action(name: &str, scheduled: Time, sync: Time) -> Action {
        Action {
            id: Uuid::new_v4(),
            name: name.to_string(),
            scheduled_at: scheduled,
            scheduled_at_sync: sync,
            ..Default::default()
        }
    }

    fn model(actions: Vec<Action>) -> DomainModel {
        DomainModel {
            objectives: vec![],
            charters: vec![Charter { actions, ..Default::default() }],
        }
    }

    #[test]
    fn empty_model_plans_nothing() {
        let report = plan_sync(&model(vec![]), &HashMap::new());
        assert!(report.is_empty());
    }

    #[test]
    fn already_synced_action_is_noop_and_omitted() {
        // A == B == C → no-op, and no-ops never clutter the report.
        let a = action("synced", Some(t1()), Some(t1()));
        let ics = HashMap::from([(a.id, Some(t1()))]);
        let report = plan_sync(&model(vec![a]), &ics);
        assert!(report.is_empty());
    }

    #[test]
    fn first_sync_of_new_action_plans_take_action() {
        // A set, never synced (B None), no .ics (C absent) → push to the calendar.
        let a = action("new", Some(t1()), None);
        let id = a.id;
        let report = plan_sync(&model(vec![a]), &HashMap::new());
        assert_eq!(report.entries.len(), 1);
        assert_eq!(report.entries[0].action_id, id);
        assert_eq!(report.entries[0].outcome, Reconcile::TakeAction(Some(t1())));
        assert_eq!(report.tally().take_action, 1);
    }

    #[test]
    fn calendar_edit_plans_take_calendar() {
        // A == B, C moved → the calendar wins.
        let a = action("cal-edit", Some(t1()), Some(t1()));
        let ics = HashMap::from([(a.id, Some(t2()))]);
        let report = plan_sync(&model(vec![a]), &ics);
        assert_eq!(report.entries[0].outcome, Reconcile::TakeCalendar(Some(t2())));
        assert_eq!(report.tally().take_calendar, 1);
    }

    #[test]
    fn both_moved_differently_plans_conflict() {
        // A=t2, B=t1, C=t3 → conflict, surfaced for a human.
        let a = action("clash", Some(t2()), Some(t1()));
        let ics = HashMap::from([(a.id, Some(t3()))]);
        let report = plan_sync(&model(vec![a]), &ics);
        assert_eq!(report.tally().conflict, 1);
        assert_eq!(report.conflicts().count(), 1);
    }

    #[test]
    fn plan_generated_action_is_skipped() {
        // A moved (would be TakeAction), but external_schedule_id ⇒ never mirrored.
        let mut a = action("recurring-occurrence", Some(t2()), Some(t1()));
        a.external_schedule_id = Some("some-plan-uid".to_string());
        let report = plan_sync(&model(vec![a]), &HashMap::new());
        assert!(report.is_empty(), "plan-generated actions must not be mirrored");
    }

    #[test]
    fn mixed_batch_tallies_each_kind_and_omits_noops() {
        let new = action("new", Some(t1()), None); // TakeAction
        let cal = action("cal", Some(t1()), Some(t1())); // TakeCalendar (C moves)
        let noop = action("noop", Some(t1()), Some(t1())); // NoOp (C same)
        let ics = HashMap::from([(cal.id, Some(t2())), (noop.id, Some(t1()))]);
        let report = plan_sync(&model(vec![new, cal, noop]), &ics);
        let tally = report.tally();
        assert_eq!(tally.take_action, 1);
        assert_eq!(tally.take_calendar, 1);
        assert_eq!(report.entries.len(), 2, "the no-op must be omitted");
    }

    #[test]
    fn read_ics_dates_maps_uuid_uid_to_dtstart() {
        let dir = tempfile::tempdir().unwrap();
        let charter_dir = dir.path().join("work");
        std::fs::create_dir_all(&charter_dir).unwrap();
        let id = Uuid::new_v4();
        let ics = format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:{}\r\n\
             SUMMARY:Test\r\nDTSTART:20260427T100000\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n",
            id
        );
        std::fs::write(charter_dir.join(format!("{}.ics", id)), ics).unwrap();

        let dates = read_ics_dates(dir.path()).unwrap();
        assert!(dates.get(&id).expect("uuid uid mapped").is_some());
    }

    #[test]
    fn read_ics_dates_ignores_non_uuid_uids() {
        // A recurring-plan-style .ics whose UID is a plan identity, not a UUID.
        let dir = tempfile::tempdir().unwrap();
        let charter_dir = dir.path().join("work");
        std::fs::create_dir_all(&charter_dir).unwrap();
        let ics = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:weekly-standup\r\n\
                   SUMMARY:Standup\r\nDTSTART:20260427T100000\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n";
        std::fs::write(charter_dir.join("weekly-standup.ics"), ics).unwrap();

        let dates = read_ics_dates(dir.path()).unwrap();
        assert!(dates.is_empty(), "non-UUID UIDs are not action mirrors");
    }
}
