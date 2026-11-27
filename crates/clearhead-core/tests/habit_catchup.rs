use std::collections::HashSet;

use clearhead_core::domain::ActionState;
use clearhead_core::domain::habit_slots::{SlotFrame, canonical_slot, occurrence_id, step_id};
use clearhead_core::parse_document;
use clearhead_core::workspace::actions::{OutputFormat, format};
use clearhead_core::workspace::habit_stamp::{
    CatchUpInput, StampSource, StampedOccurrence, plan_catch_up, plan_text_catch_up,
};
use clearhead_core::workspace::predecessors::PredecessorScope;
use uuid::{Uuid, uuid};

const HABIT: Uuid = uuid!("00000000-0000-0000-0000-000000000001");
const STEP: Uuid = uuid!("00000000-0000-0000-0000-000000000002");
const SOURCE: &str = "charters/review.habits.actions";

#[test]
fn batches_resume_from_persisted_ids_without_losing_missed_slots() {
    let doc = parse_document(&format!(
        "[ ] Review @2026-11-23 R:FREQ=DAILY #{HABIT}\n>[ ] Clear inbox #{STEP}\n"
    ))
    .unwrap();
    let slots: Vec<_> = ["2026-11-23", "2026-11-24", "2026-11-25", "2026-11-26"]
        .map(|date| canonical_slot(SlotFrame::Date, date).unwrap())
        .into();
    let id = |index: usize| occurrence_id(HABIT, &slots[index].key);
    let excluded = HashSet::from([slots[1].key.clone()]);
    let scope = PredecessorScope::default();
    let now = "2026-11-26T12:00:00-08:00".parse().unwrap();
    let plan = |occupied: &HashSet<Uuid>, budget| {
        plan_text_catch_up(
            CatchUpInput {
                source: StampSource::new(SOURCE, &doc),
                habit_id: HABIT,
                named_template: None,
                scope: &scope,
                charter_id: None,
                occupied,
                excluded: &excluded,
                budget,
            },
            &now,
        )
        .unwrap()
    };
    let source_ids = |source: &str| {
        parse_document(source)
            .unwrap()
            .actions
            .into_iter()
            .map(|action| action.id)
    };
    let delivered_ids = |tree: &StampedOccurrence| {
        let source = format(&tree.actions().to_vec(), OutputFormat::Actions, None, None).unwrap();
        source_ids(&source)
    };
    let archived = format!("[x] Review %2026-11-23 #{}\n", id(0));
    // Monday is archived. Tuesday was excluded. Only Wed/Thu should be new.
    let mut occupied: HashSet<_> = source_ids(&archived).collect();
    let zero = plan(&occupied, 0);
    assert!(zero.occurrences.is_empty() && zero.has_backlog);

    let first = plan(&occupied, 1);
    assert!(first.has_backlog);
    assert_eq!(first.occurrences[0].actions()[0].id, id(2));
    assert_eq!(first.occurrences[0].actions()[1].id, step_id(id(2), STEP));
    assert_eq!(
        first.occurrences[0].actions()[0].state,
        ActionState::NotStarted
    );
    // Crash before delivery: without the new durable IDs the same plan is safe.
    assert_eq!(first.occurrences, plan(&occupied, 1).occurrences);

    occupied.extend(delivered_ids(&first.occurrences[0]));
    let next = plan(&occupied, 1);
    assert!(!next.has_backlog);
    assert_eq!(next.occurrences[0].actions()[0].id, id(3));
    occupied.extend(delivered_ids(&next.occurrences[0]));
    let retry = plan(&occupied, 1);
    assert!(retry.occurrences.is_empty() && !retry.has_backlog);
}

#[test]
fn date_floating_and_offset_rules_only_stage_arrived_slots() {
    let now = "2026-11-25T08:45:00-08:00"
        .parse::<chrono::DateTime<chrono::FixedOffset>>()
        .unwrap();
    for (spec, expected) in [
        (
            "@2026-11-23 R:FREQ=DAILY;COUNT=5",
            vec!["D:2026-11-23", "D:2026-11-24", "D:2026-11-25"],
        ),
        (
            "@2026-11-23T09:00 R:FREQ=DAILY;UNTIL=20261125T090000",
            vec!["F:2026-11-23T09:00:00", "F:2026-11-24T09:00:00"],
        ),
        (
            "@2026-11-23T09:00+05:30 R:FREQ=DAILY;UNTIL=20261124T033000Z",
            vec!["U:2026-11-23T03:30:00Z", "U:2026-11-24T03:30:00Z"],
        ),
    ] {
        let doc = parse_document(&format!("[ ] Review {spec} #{HABIT}\n")).unwrap();
        let batch = plan_text_catch_up(
            CatchUpInput {
                source: StampSource::new(SOURCE, &doc),
                habit_id: HABIT,
                named_template: None,
                scope: &PredecessorScope::default(),
                charter_id: None,
                occupied: &HashSet::new(),
                excluded: &HashSet::new(),
                budget: 10,
            },
            &now,
        )
        .unwrap();
        let slots: Vec<_> = batch
            .occurrences
            .iter()
            .map(|tree| {
                tree.actions()[0]
                    .native_occurrence
                    .as_ref()
                    .unwrap()
                    .occurrence_key
                    .as_str()
            })
            .collect();
        assert_eq!(slots, expected, "{spec}");
        assert!(!batch.has_backlog, "{spec}");
    }
}

#[test]
fn duplicate_recurrence_inputs_do_not_fill_the_batch_or_claim_backlog() {
    let doc = parse_document(&format!("[ ] Review @2026-11-23 R:FREQ=DAILY #{HABIT}\n")).unwrap();
    let slot = canonical_slot(SlotFrame::Date, "2026-11-23").unwrap();
    let result = plan_catch_up(
        CatchUpInput {
            source: StampSource::new(SOURCE, &doc),
            habit_id: HABIT,
            named_template: None,
            scope: &PredecessorScope::default(),
            charter_id: None,
            occupied: &HashSet::new(),
            excluded: &HashSet::new(),
            budget: 1,
        },
        [Ok(slot.clone()), Ok(slot)],
    )
    .unwrap();
    assert_eq!(result.occurrences.len(), 1);
    assert!(!result.has_backlog);
}

#[test]
fn a_partly_present_tree_is_not_mistaken_for_an_archived_occurrence() {
    let doc = parse_document(&format!(
        "[ ] Review @2026-11-23 R:FREQ=DAILY #{HABIT}\n>[ ] Clear inbox #{STEP}\n"
    ))
    .unwrap();
    let slot = canonical_slot(SlotFrame::Date, "2026-11-23").unwrap();
    let orphan = step_id(occurrence_id(HABIT, &slot.key), STEP);
    let error = plan_catch_up(
        CatchUpInput {
            source: StampSource::new(SOURCE, &doc),
            habit_id: HABIT,
            named_template: None,
            scope: &PredecessorScope::default(),
            charter_id: None,
            occupied: &HashSet::from([orphan]),
            excluded: &HashSet::new(),
            budget: 1,
        },
        [Ok(slot)],
    )
    .unwrap_err();
    assert!(error.to_string().contains(&orphan.to_string()));
}
