use std::collections::HashSet;

use chrono::{DateTime, Local};
use uuid::Uuid;

use crate::domain::Plan;
use crate::workspace::actions::{Action, ActionState};
use crate::workspace::ics::occurrence_act_id;

/// Expand a set of plans into new acts within a time window.
///
/// Pure — no IO. Callers own file discovery, writing, and any template
/// child expansion that requires filesystem access.
///
/// Returns only acts that are not already represented in `existing_ids`.
pub fn expand_plans_into_acts(
    plans: &[Plan],
    existing_ids: &HashSet<Uuid>,
    now: DateTime<Local>,
    horizon: DateTime<Local>,
) -> Vec<Action> {
    let mut new_acts = Vec::new();
    let mut seen = existing_ids.clone();

    for plan in plans {
        let vevent_uid = match &plan.external_id {
            Some(uid) => uid.as_str(),
            None => continue,
        };
        let Some(dtstart) = plan.dtstart else {
            continue;
        };

        if plan.recurrence.is_some() {
            for occ in plan.expand_occurrences(dtstart, 1000) {
                let occ_local = occ.with_timezone(&Local);
                if occ_local > horizon {
                    break;
                }
                if occ_local < now {
                    continue;
                }
                let occ_key = occ_local.to_rfc3339();
                let act_id = occurrence_act_id(vevent_uid, &occ_key);
                if seen.contains(&act_id) {
                    continue;
                }
                seen.insert(act_id);
                new_acts.push(Action {
                    id: act_id,
                    state: ActionState::NotStarted,
                    name: plan.name.clone(),
                    do_date_time: Some(occ_local),
                    created_date_time: Some(now),
                    ..Default::default()
                });
            }
        } else if dtstart >= now && dtstart <= horizon {
            let occ_key = dtstart.to_rfc3339();
            let act_id = occurrence_act_id(vevent_uid, &occ_key);
            if !seen.contains(&act_id) {
                seen.insert(act_id);
                new_acts.push(Action {
                    id: act_id,
                    state: ActionState::NotStarted,
                    name: plan.name.clone(),
                    do_date_time: Some(dtstart),
                    created_date_time: Some(now),
                    ..Default::default()
                });
            }
        }
    }

    new_acts
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::Plan;
    use chrono::TimeZone;

    fn make_plan(name: &str, uid: &str, dtstart: DateTime<Local>, rrule: Option<&str>) -> Plan {
        Plan {
            name: name.to_string(),
            external_id: Some(uid.to_string()),
            dtstart: Some(dtstart),
            recurrence: rrule.map(|r| crate::domain::Recurrence {
            frequency: r.to_string(),
            ..Default::default()
        }),
            ..Default::default()
        }
    }

    #[test]
    fn one_shot_plan_within_horizon_produces_act() {
        let now = Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap();
        let horizon = Local.with_ymd_and_hms(2026, 5, 8, 23, 59, 59).unwrap();
        let dtstart = Local.with_ymd_and_hms(2026, 5, 3, 10, 0, 0).unwrap();

        let plan = make_plan("review specs", "test-uid-1", dtstart, None);
        let acts = expand_plans_into_acts(&[plan], &HashSet::new(), now, horizon);

        assert_eq!(acts.len(), 1);
        assert_eq!(acts[0].name, "review specs");
    }

    #[test]
    fn one_shot_plan_outside_horizon_is_skipped() {
        let now = Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap();
        let horizon = Local.with_ymd_and_hms(2026, 5, 8, 23, 59, 59).unwrap();
        let dtstart = Local.with_ymd_and_hms(2026, 5, 15, 10, 0, 0).unwrap();

        let plan = make_plan("far future", "test-uid-2", dtstart, None);
        let acts = expand_plans_into_acts(&[plan], &HashSet::new(), now, horizon);

        assert!(acts.is_empty());
    }

    #[test]
    fn existing_id_is_not_duplicated() {
        let now = Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap();
        let horizon = Local.with_ymd_and_hms(2026, 5, 8, 23, 59, 59).unwrap();
        let dtstart = Local.with_ymd_and_hms(2026, 5, 3, 10, 0, 0).unwrap();
        let uid = "test-uid-3";

        let act_id = occurrence_act_id(uid, &dtstart.to_rfc3339());
        let mut existing = HashSet::new();
        existing.insert(act_id);

        let plan = make_plan("already there", uid, dtstart, None);
        let acts = expand_plans_into_acts(&[plan], &existing, now, horizon);

        assert!(acts.is_empty());
    }

    #[test]
    fn plan_without_external_id_is_skipped() {
        let now = Local.with_ymd_and_hms(2026, 5, 1, 9, 0, 0).unwrap();
        let horizon = Local.with_ymd_and_hms(2026, 5, 8, 23, 59, 59).unwrap();
        let dtstart = Local.with_ymd_and_hms(2026, 5, 3, 10, 0, 0).unwrap();

        let plan = Plan {
            name: "no uid".to_string(),
            dtstart: Some(dtstart),
            external_id: None,
            ..Default::default()
        };
        let acts = expand_plans_into_acts(&[plan], &HashSet::new(), now, horizon);
        assert!(acts.is_empty());
    }
}
