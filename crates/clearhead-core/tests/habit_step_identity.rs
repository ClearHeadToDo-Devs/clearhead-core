use clearhead_core::domain::habit_slots::{SlotFrame, canonical_slot, occurrence_id, step_id};
use clearhead_core::domain::{Action, ActionState, Charter};
use clearhead_core::parse_document;
#[cfg(feature = "formatting")]
use clearhead_core::workspace::actions::{OutputFormat, format};
use clearhead_core::workspace::charter::MarkdownCharter;
use clearhead_core::workspace::habit_stamp::{StampSource, stamp_occurrence};
use clearhead_core::workspace::predecessors::PredecessorScope;
use uuid::{Uuid, uuid};

const HABIT: Uuid = uuid!("00000000-0000-0000-0000-000000000001");
const FIRST: Uuid = uuid!("00000000-0000-0000-0000-000000000002");
const SECOND: Uuid = uuid!("00000000-0000-0000-0000-000000000003");
const CHILD: Uuid = uuid!("00000000-0000-0000-0000-000000000004");
const HABITS: &str = "review.habits.actions";
const TEMPLATE: &str = "templates/review.actions";

#[test]
fn stable_ids_and_sibling_links_survive_reorder_and_rename() {
    let doc = parse_document(&format!(
        "[-] Habit @2026-10-25 R:FREQ=DAILY #{HABIT}\n\
         >[ ] First =first #{FIRST}\n\
         >[ ] Second <first #{SECOND}\n\
         >>[ ] Child #{CHILD}\n"
    ))
    .unwrap();
    let revised = parse_document(&format!(
        "[-] Habit @2026-10-25 R:FREQ=DAILY #{HABIT}\n\
         >[ ] Second <first #{SECOND}\n\
         >>[ ] Child #{CHILD}\n\
         >[ ] Renamed =first #{FIRST}\n"
    ))
    .unwrap();
    let slot = canonical_slot(SlotFrame::Date, "2026-10-26").unwrap();
    let scope = PredecessorScope::default();
    let stamp = |doc| {
        stamp_occurrence(
            StampSource::new(HABITS, doc),
            HABIT,
            None,
            &slot,
            &scope,
            None,
        )
        .unwrap()
        .unwrap()
    };
    let stamped = stamp(&doc);
    let root = occurrence_id(HABIT, &slot.key);
    let first = step_id(root, FIRST);
    let second = step_id(root, SECOND);
    let child = step_id(root, CHILD);
    let actions = stamped.actions();
    assert_eq!(
        actions.iter().map(|a| a.id).collect::<Vec<_>>(),
        [root, first, second, child]
    );
    assert_eq!(actions[0].state, ActionState::NotStarted);
    assert_eq!(actions[0].planned.unwrap().start.to_string(), "2026-10-26");
    let lineage = actions[0].native_occurrence.as_ref().unwrap();
    assert_eq!(lineage.habit_id, HABIT);
    assert_eq!(lineage.occurrence_key, "D:2026-10-26");
    assert!(actions.iter().all(|a| a.recurrence_rule.is_none()
        && a.plan_id.is_none()
        && a.external_occurrence_key.is_none()));
    assert_eq!(actions[1].parent_id, Some(root));
    assert_eq!(actions[2].depends_on(), [first]);
    assert_eq!(actions[3].parent_id, Some(second));
    #[cfg(feature = "formatting")]
    {
        let rendered = format(&actions.to_vec(), OutputFormat::Actions, None, None).unwrap();
        let reread = parse_document(&rendered).unwrap();
        assert_eq!(reread.actions[2].depends_on(), [first]);
    }

    let mut existing = Vec::new();
    assert!(stamped.insert_into(&mut existing).unwrap());
    existing[1].name = "User edit".into();
    let expected = existing.clone();
    let restamp = stamp(&revised);
    assert_eq!(restamp.actions()[3].id, first);
    assert!(!restamp.insert_into(&mut existing).unwrap());
    assert_eq!(existing, expected);

    let mut collision = vec![Action {
        id: second,
        ..Action::new("Unrelated")
    }];
    let before = collision.clone();
    assert!(stamp(&doc).insert_into(&mut collision).is_err());
    assert_eq!(collision, before);
}

#[test]
fn repeated_stamps_keep_aliases_without_guessing_which_step_a_wait_names() {
    let stamp = |wait: &str, date: &str, scope: &PredecessorScope| {
        let doc = parse_document(&format!(
            "[ ] Habit =review @2026-10-25 R:FREQ=DAILY #{HABIT}\n\
             >[ ] First =first #{FIRST}\n\
             >[ ] Second <{wait} #{SECOND}\n"
        ))
        .unwrap();
        stamp_occurrence(
            StampSource::new(HABITS, &doc),
            HABIT,
            None,
            &canonical_slot(SlotFrame::Date, date).unwrap(),
            scope,
            None,
        )
        .unwrap()
        .unwrap()
    };
    let prior = stamp("first", "2026-10-25", &PredecessorScope::default());
    let scope = PredecessorScope::new(&[MarkdownCharter::from(Charter {
        actions: prior.actions().to_vec(),
        ..Default::default()
    })]);

    let ambiguous = stamp("first", "2026-10-26", &scope);
    assert_eq!(ambiguous.actions()[0].alias.as_deref(), Some("review"));
    assert_eq!(ambiguous.actions()[1].alias.as_deref(), Some("first"));
    let wait = &ambiguous.actions()[2].predecessors.as_ref().unwrap()[0];
    assert_eq!(wait.raw_ref, "first");
    assert_eq!(wait.resolved_uuid, None);

    let explicit = stamp(&FIRST.to_string(), "2026-10-26", &scope);
    assert_eq!(
        explicit.actions()[2].depends_on(),
        [explicit.actions()[1].id]
    );
    assert_ne!(explicit.actions()[1].id, prior.actions()[1].id);
}

#[test]
fn refuses_every_bad_line_before_returning_a_tree() {
    let habit = parse_document(&format!(
        "[ ] Habit =habit $template: review$ @2026-10-25 R:FREQ=DAILY #{HABIT}\n"
    ))
    .unwrap();
    let template = parse_document(&format!(
        "[ ] Parent #{FIRST}\n\
         >[ ] Child @2026-10-25 R:FREQ=DAILY <habit\n\
         >[ ] Other <{FIRST} #{SECOND}\n"
    ))
    .unwrap();
    let error = stamp_occurrence(
        StampSource::new(HABITS, &habit),
        HABIT,
        Some(StampSource::new(TEMPLATE, &template)),
        &canonical_slot(SlotFrame::Date, "2026-10-26").unwrap(),
        &PredecessorScope::default(),
        None,
    )
    .unwrap_err()
    .to_string();
    for (line, code) in [
        (2, "E002"),
        (2, "E009"),
        (2, "habit-id-missing"),
        (3, "E009"),
    ] {
        assert!(
            error
                .lines()
                .any(|s| s.contains(&format!("{TEMPLATE}:{line}:")) && s.contains(code)),
            "{error}"
        );
    }
}

#[test]
fn inline_steps_win_and_ephemeral_habit_ids_refuse() {
    let doc = parse_document(&format!(
        "[ ] Habit $template: review$ @2026-10-25 R:FREQ=DAILY\n>[ ] First #{FIRST}\n"
    ))
    .unwrap();
    let slot = canonical_slot(SlotFrame::Date, "2026-10-26").unwrap();
    let scope = PredecessorScope::default();
    let error = stamp_occurrence(
        StampSource::new(HABITS, &doc),
        doc.actions[0].id,
        None,
        &slot,
        &scope,
        None,
    )
    .unwrap_err()
    .to_string();
    assert!(error.contains("review.habits.actions:1:"));
    assert!(error.contains("habit-id-missing"));

    let doc = parse_document(&format!(
        "[ ] Habit $template: review$ @2026-10-25 R:FREQ=DAILY #{HABIT}\n>[ ] First #{FIRST}\n"
    ))
    .unwrap();
    let invalid_template = parse_document("[ ] Missing id @2026-10-25 R:FREQ=DAILY\n").unwrap();
    let tree = stamp_occurrence(
        StampSource::new(HABITS, &doc),
        HABIT,
        Some(StampSource::new(TEMPLATE, &invalid_template)),
        &slot,
        &scope,
        None,
    )
    .unwrap()
    .unwrap();
    assert_eq!(tree.actions()[1].name, "First");
}

#[test]
fn named_template_keeps_workspace_waits_but_rebases_template_waits() {
    let habit = parse_document(&format!(
        "[=] Habit $template: review$ @2026-10-25T09:00 R:FREQ=DAILY #{HABIT}\n"
    ))
    .unwrap();
    let template = parse_document(&format!(
        "[ ] First <global #{FIRST}\n[ ] Second <{FIRST} #{SECOND}\n"
    ))
    .unwrap();
    let mut external = Action::new("External prerequisite");
    external.alias = Some("global".into());
    let external_id = external.id;
    let scope = PredecessorScope::new(&[MarkdownCharter::from(Charter {
        actions: vec![external],
        ..Default::default()
    })]);
    let slot = canonical_slot(SlotFrame::Floating, "2026-10-26T09:00").unwrap();
    let stamp = stamp_occurrence(
        StampSource::new(HABITS, &habit),
        HABIT,
        Some(StampSource::new(TEMPLATE, &template)),
        &slot,
        &scope,
        None,
    )
    .unwrap()
    .unwrap();
    let [root, first, second] = stamp.actions() else {
        panic!("expected a root and two steps")
    };
    assert_eq!(root.planned.unwrap().start.to_string(), "2026-10-26T09:00");
    assert_eq!(
        root.native_occurrence.as_ref().unwrap().template.as_deref(),
        Some("review")
    );
    assert_eq!(first.predecessors.as_ref().unwrap()[0].raw_ref, "global");
    assert_eq!(first.depends_on(), [external_id]);
    assert_eq!(second.depends_on(), [first.id]);
}

#[test]
fn a_closed_habit_does_not_stamp() {
    let doc = parse_document(&format!(
        "[_] Habit @2026-10-25 R:FREQ=DAILY %2026-10-25 #{HABIT}\n"
    ))
    .unwrap();
    let slot = canonical_slot(SlotFrame::Date, "2026-10-26").unwrap();
    assert!(
        stamp_occurrence(
            StampSource::new(HABITS, &doc),
            HABIT,
            None,
            &slot,
            &PredecessorScope::default(),
            None,
        )
        .unwrap()
        .is_none()
    );
}

#[test]
fn rebases_windows_in_the_series_frame_across_gap_and_fold() {
    let doc = parse_document(&format!(
        "[ ] Habit @2026-01-03T01:30 R:FREQ=DAILY :2026-01-03T02:30/2026-01-04T03:30 #{HABIT}\n\
         >[ ] Child @2026-01-03T02:30 #{CHILD}\n"
    ))
    .unwrap();
    for (original, root_due, child_utc) in [
        (
            "2026-03-08T01:30",
            "2026-03-08T02:30-08:00/2026-03-09T03:30-07:00",
            "2026-03-08T10:30:00+00:00",
        ),
        (
            "2026-11-01T00:30",
            "2026-11-01T01:30-07:00/2026-11-02T02:30-08:00",
            "2026-11-01T08:30:00+00:00",
        ),
        (
            "2026-10-25T01:30",
            "2026-10-25T02:30-07:00/2026-10-26T03:30-07:00",
            "2026-10-25T09:30:00+00:00",
        ),
    ] {
        let slot =
            canonical_slot(SlotFrame::Tzid(chrono_tz::America::Los_Angeles), original).unwrap();
        let tree = stamp_occurrence(
            StampSource::new(HABITS, &doc),
            HABIT,
            None,
            &slot,
            &PredecessorScope::default(),
            None,
        )
        .unwrap()
        .unwrap();
        let actions = tree.actions();
        assert_eq!(actions[0].due_date.unwrap().to_string(), root_due);
        assert_eq!(
            actions[1]
                .planned
                .unwrap()
                .start
                .at_in(&chrono::Utc)
                .to_rfc3339(),
            child_utc
        );
    }
}
