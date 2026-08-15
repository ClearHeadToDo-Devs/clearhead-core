use chrono::{DateTime, Duration, Local, NaiveDate, TimeZone};
use clearhead_core::workspace::actions::lint_document;
use clearhead_core::{
    Action, ActionState, OutputFormat, PredecessorRef, close_subtree, collect_subtree_ids, format,
    parse_actions, parse_document,
};
use proptest::prelude::*;
use proptest::test_runner::FileFailurePersistence;
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

#[derive(Clone, Debug)]
struct ActionSeed {
    depth_key: usize,
    predecessor_key: Option<usize>,
    unresolved_predecessor: Option<String>,
    state: ActionState,
    name: String,
    description: Option<String>,
    priority: Option<u32>,
    contexts: Option<Vec<String>>,
    created_day: u16,
    scheduled_after_creation: Option<u16>,
    duration: Option<u32>,
    due_after_creation: Option<u16>,
    completed_after_creation: Option<u16>,
    charter: Option<String>,
    alias: Option<String>,
    is_sequential: Option<bool>,
}

fn plain_dsl_text() -> impl Strategy<Value = String> {
    let characters = vec![
        'a', 'b', 'Z', '0', ' ', '-', '_', '/', '\\', '$', '!', '*', '+', '@', '%', '^', '#', '>',
        '<', '~', '=', ':', '[', ']', 'é', 'λ',
    ];
    (
        prop::sample::select(vec!['a', 'Z', 'é', 'λ']),
        prop::collection::vec(prop::sample::select(characters), 0..24),
    )
        .prop_map(|(first, rest)| std::iter::once(first).chain(rest).collect())
        .prop_filter(
            "plain text must have normalized boundaries",
            |text: &String| !text.ends_with(char::is_whitespace) && !text.contains("[["),
        )
}

fn malformed_link() -> impl Strategy<Value = String> {
    prop::sample::select(vec![
        "[[]]",
        "[[|target]]",
        "[[label|]]",
        "[[label|target|extra]]",
        "[[target]tail]]",
        "[[bad[link]]",
    ])
    .prop_map(str::to_string)
}

fn dsl_text() -> impl Strategy<Value = String> {
    prop_oneof![
        5 => plain_dsl_text(),
        2 => (plain_dsl_text(), identifier(), plain_dsl_text())
            .prop_map(|(before, target, after)| format!("{before} [[{target}]] {after}")),
        2 => (plain_dsl_text(), identifier(), identifier(), plain_dsl_text())
            .prop_map(|(before, label, target, after)| {
                format!("{before} [[{label}|{target}]] {after}")
            }),
        1 => identifier().prop_map(|target| {
            format!("long-prefix-before-link [[{target}]] after")
        }),
        1 => (plain_dsl_text(), malformed_link(), plain_dsl_text())
            .prop_map(|(before, malformed, after)| format!("{before}{malformed}{after}")),
    ]
}

fn identifier() -> impl Strategy<Value = String> {
    proptest::string::string_regex("[a-z][a-z0-9-]{0,11}").expect("valid identifier regex")
}

fn action_state() -> impl Strategy<Value = ActionState> {
    prop_oneof![
        Just(ActionState::NotStarted),
        Just(ActionState::InProgress),
        Just(ActionState::BlockedOrAwaiting),
        Just(ActionState::Completed),
        Just(ActionState::Cancelled),
    ]
}

fn action_seed() -> impl Strategy<Value = ActionSeed> {
    (
        (
            any::<usize>(),
            any::<Option<usize>>(),
            prop::option::of(dsl_text()),
            action_state(),
            dsl_text(),
            prop::option::of(dsl_text()),
            prop::option::of(1u32..=9),
            prop::option::of(prop::collection::vec(identifier(), 1..4)),
        ),
        (
            0u16..=3_000,
            prop::option::of(0u16..=30),
            prop::option::of(1u32..=1_440),
            prop::option::of(0u16..=60),
            prop::option::of(0u16..=60),
            prop::option::of(dsl_text()),
            prop::option::of(identifier()),
            any::<bool>(),
        ),
    )
        .prop_map(
            |(
                (
                    depth_key,
                    predecessor_key,
                    unresolved_predecessor,
                    state,
                    name,
                    description,
                    priority,
                    contexts,
                ),
                (
                    created_day,
                    scheduled_after_creation,
                    duration,
                    due_after_creation,
                    completed_after_creation,
                    charter,
                    alias,
                    sequential,
                ),
            )| ActionSeed {
                depth_key,
                predecessor_key,
                unresolved_predecessor,
                state,
                name,
                description,
                priority,
                contexts,
                created_day,
                scheduled_after_creation,
                duration: scheduled_after_creation.and(duration),
                due_after_creation,
                completed_after_creation: matches!(
                    state,
                    ActionState::Completed | ActionState::Cancelled
                )
                .then_some(completed_after_creation.unwrap_or_default()),
                charter,
                alias,
                is_sequential: sequential.then_some(true),
            },
        )
}

fn generated_actions() -> impl Strategy<Value = Vec<Action>> {
    (any::<u64>(), prop::collection::vec(action_seed(), 1..10)).prop_map(|(salt, seeds)| {
        let ids: Vec<_> = (0..seeds.len())
            .map(|index| Uuid::from_u128((u128::from(salt) << 64) | (index as u128 + 1)))
            .collect();
        let mut ancestors = Vec::new();
        let mut actions = Vec::with_capacity(seeds.len());
        for (index, seed) in seeds.into_iter().enumerate() {
            let max_depth = ancestors.len().min(5);
            let depth = if index == 0 {
                0
            } else {
                seed.depth_key % (max_depth + 1)
            };
            let parent_id = depth
                .checked_sub(1)
                .map(|parent_depth| ancestors[parent_depth]);
            ancestors.truncate(depth);
            ancestors.push(ids[index]);

            let mut predecessors = Vec::new();
            if index > 0
                && let Some(key) = seed.predecessor_key
            {
                let id = ids[key % index];
                predecessors.push(PredecessorRef {
                    raw_ref: id.to_string(),
                    resolved_uuid: Some(id),
                });
            }
            if let Some(raw_ref) = seed.unresolved_predecessor {
                predecessors.push(PredecessorRef {
                    raw_ref,
                    resolved_uuid: None,
                });
            }
            let created_at = generated_datetime(seed.created_day, 0);
            actions.push(Action {
                id: ids[index],
                parent_id,
                state: seed.state,
                name: seed.name,
                description: seed.description,
                priority: seed.priority,
                contexts: seed.contexts,
                scheduled_at: seed
                    .scheduled_after_creation
                    .map(|days| generated_datetime(seed.created_day, days)),
                duration: seed.duration,
                due_date: seed
                    .due_after_creation
                    .map(|days| generated_datetime(seed.created_day, days)),
                completed_at: seed
                    .completed_after_creation
                    .map(|days| generated_datetime(seed.created_day, days)),
                created_at: Some(created_at),
                predecessors: (!predecessors.is_empty()).then_some(predecessors),
                charter: seed.charter,
                alias: seed.alias,
                is_sequential: seed.is_sequential,
                plan_id: None,
                external_occurrence_key: None,
            });
        }
        actions
    })
}

fn generated_datetime(created_day: u16, days_after_creation: u16) -> DateTime<Local> {
    let date = NaiveDate::from_ymd_opt(2020, 1, 1).expect("fixed date is valid")
        + Duration::days(i64::from(created_day + days_after_creation));
    Local
        .from_local_datetime(
            &date
                .and_hms_opt(12, 0, 0)
                .expect("midday is always a valid naive time"),
        )
        .single()
        .expect("local midday is unambiguous")
}

fn property_config() -> ProptestConfig {
    let mut config = ProptestConfig::with_failure_persistence(FileFailurePersistence::Direct(
        "tests/generated_invariants.proptest-regressions",
    ));
    config.cases = 256;
    config
}

fn reachable_ids(actions: &[Action], root_id: Uuid) -> HashSet<Uuid> {
    let mut reachable = HashSet::from([root_id]);
    loop {
        let before = reachable.len();
        for action in actions {
            if action.parent_id.is_some_and(|id| reachable.contains(&id)) {
                reachable.insert(action.id);
            }
        }
        if reachable.len() == before {
            return reachable;
        }
    }
}

proptest! {
    #![proptest_config(property_config())]

    #[test]
    fn actions_round_trip_preserves_generated_semantics(actions in generated_actions()) {
        let rendered = format(
            &actions,
            OutputFormat::Actions,
            None,
            None,
        ).expect("generated valid actions should format");
        let reparsed = parse_actions(&rendered)
            .unwrap_or_else(|error| panic!("formatted generated actions should parse strictly: {error}\n{rendered}"));

        prop_assert_eq!(reparsed, actions);
    }

    #[test]
    fn valid_links_survive_at_generated_offsets(
        prefix in proptest::string::string_regex("[a-z]{20,30}").expect("valid prefix regex"),
        target in proptest::string::string_regex("[a-z]{1,5}").expect("valid target regex"),
        suffix in proptest::string::string_regex("[a-z]{1,8}").expect("valid suffix regex"),
    ) {
        let expected_name = format!("{prefix} [[{target}]] {suffix}");
        let action = Action {
            id: Uuid::from_u128(1),
            name: expected_name.clone(),
            ..Action::default()
        };

        let rendered = format(&vec![action], OutputFormat::Actions, None, None)
            .expect("generated linked action should format");
        let reparsed = parse_actions(&rendered)
            .expect("formatted linked action should parse strictly");

        let rendered_link = format!("[[{target}]]");
        prop_assert!(rendered.contains(&rendered_link));
        prop_assert_eq!(&reparsed[0].name, &expected_name);
    }

    #[test]
    fn malformed_link_shapes_round_trip_as_literal_text(
        malformed in malformed_link(),
        prefix in proptest::string::string_regex("[a-z]{1,12}").expect("valid prefix regex"),
    ) {
        let expected_name = format!("{prefix}{malformed}suffix");
        let action = Action {
            id: Uuid::from_u128(1),
            name: expected_name.clone(),
            ..Action::default()
        };

        let rendered = format(&vec![action], OutputFormat::Actions, None, None)
            .expect("generated malformed-link prose should format as literals");
        let reparsed = parse_actions(&rendered)
            .expect("escaped malformed-link prose should parse strictly");

        prop_assert!(!rendered.contains(&malformed));
        prop_assert_eq!(&reparsed[0].name, &expected_name);
    }

    #[test]
    fn formatting_generated_actions_is_idempotent(actions in generated_actions()) {
        let once = format(&actions, OutputFormat::Actions, None, None)
            .expect("generated valid actions should format");
        let reparsed = parse_actions(&once)
            .unwrap_or_else(|error| panic!("formatted generated actions should parse strictly: {error}\n{once}"));
        let twice = format(&reparsed, OutputFormat::Actions, None, None)
            .expect("reparsed actions should format");

        prop_assert_eq!(twice, once);
    }

    #[test]
    fn tree_consistency_codes_follow_planted_closedness(
        parent_closed in any::<bool>(),
        child_closed in any::<bool>(),
        cancelled in any::<bool>(),
    ) {
        let closed_state = if cancelled {
            ActionState::Cancelled
        } else {
            ActionState::Completed
        };
        let parent_id = Uuid::from_u128(1);
        let completed_at = generated_datetime(4_000, 0);
        let actions = vec![
            Action {
                id: parent_id,
                name: "parent".to_string(),
                state: if parent_closed { closed_state } else { ActionState::NotStarted },
                completed_at: parent_closed.then_some(completed_at),
                ..Action::default()
            },
            Action {
                id: Uuid::from_u128(2),
                parent_id: Some(parent_id),
                name: "child".to_string(),
                state: if child_closed { closed_state } else { ActionState::NotStarted },
                completed_at: child_closed.then_some(completed_at),
                ..Action::default()
            },
        ];
        let rendered = format(&actions, OutputFormat::Actions, None, None)
            .expect("planted tree should format");
        let document = parse_document(&rendered).expect("planted tree should parse");
        let codes: HashSet<_> = lint_document(&document)
            .into_iter()
            .map(|diagnostic| diagnostic.code)
            .collect();

        prop_assert_eq!(codes.contains("W002"), parent_closed && !child_closed);
        prop_assert_eq!(codes.contains("W003"), !parent_closed && child_closed);
    }

    #[test]
    fn closing_a_generated_subtree_is_local_and_complete(
        actions in generated_actions(),
        root_key in any::<usize>(),
        cancelled in any::<bool>(),
    ) {
        let root = &actions[root_key % actions.len()];
        let expected_ids = reachable_ids(&actions, root.id);
        let original_actions = actions.clone();
        let original_by_id: HashMap<_, _> = actions
            .iter()
            .map(|action| (action.id, action.clone()))
            .collect();
        let closing_state = if cancelled {
            ActionState::Cancelled
        } else {
            ActionState::Completed
        };
        let closed_at = generated_datetime(4_000, 0);

        let closed = close_subtree(&actions, root.id, closing_state, closed_at);
        let actual_ids: HashSet<_> = closed.iter().map(|action| action.id).collect();
        let expected_order: Vec<_> = actions
            .iter()
            .filter(|action| expected_ids.contains(&action.id))
            .map(|action| action.id)
            .collect();
        let actual_order: Vec<_> = closed.iter().map(|action| action.id).collect();

        prop_assert_eq!(&actual_ids, &expected_ids);
        prop_assert_eq!(actual_order, expected_order);
        prop_assert_eq!(collect_subtree_ids(&actions, root.id).into_iter().collect::<HashSet<_>>(), expected_ids);
        prop_assert_eq!(&actions, &original_actions, "source order and values must remain unchanged");

        for action in &closed {
            let original = &original_by_id[&action.id];
            prop_assert_eq!(action.state, closing_state);
            prop_assert_eq!(action.completed_at, Some(closed_at));
            if action.id == root.id {
                prop_assert_eq!(action.parent_id, None);
            } else {
                prop_assert_eq!(action.parent_id, original.parent_id);
            }
            let mut expected = original.clone();
            expected.state = closing_state;
            expected.completed_at = Some(closed_at);
            if action.id == root.id {
                expected.parent_id = None;
            }
            prop_assert_eq!(action, &expected);
        }
    }
}
