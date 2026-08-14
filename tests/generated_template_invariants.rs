use chrono::{DateTime, Duration, Local, Utc};
use clearhead_core::workspace::{instantiate_template, resolve_template};
use clearhead_core::{Action, ActionState, PredecessorRef};
use proptest::prelude::*;
use proptest::test_runner::FileFailurePersistence;
use std::collections::HashMap;
use std::fs;
use uuid::Uuid;

#[derive(Clone, Copy, Debug)]
enum DeclaredParent {
    TemplateRoot,
    Internal(usize),
    External(Uuid),
}

#[derive(Clone, Debug)]
struct TemplateScenario {
    source: Vec<Action>,
    instance_ids: HashMap<Uuid, Uuid>,
    parents: Vec<DeclaredParent>,
    parent_override: Option<Uuid>,
}

fn generated_id(namespace: u32, seed: u32, index: usize) -> Uuid {
    Uuid::from_u128((u128::from(namespace) << 96) | (u128::from(seed) << 32) | index as u128)
}

fn populated_action(id: Uuid, parent_id: Option<Uuid>, seed: u32, index: usize) -> Action {
    let timestamp =
        DateTime::<Utc>::UNIX_EPOCH + Duration::minutes(i64::from(seed % 10_000) + index as i64);
    let local_timestamp = timestamp.with_timezone(&Local);
    let predecessor_id = generated_id(0x4100_0004, seed, index);

    Action {
        id,
        parent_id,
        state: match index % 5 {
            0 => ActionState::NotStarted,
            1 => ActionState::InProgress,
            2 => ActionState::BlockedOrAwaiting,
            3 => ActionState::Completed,
            _ => ActionState::Cancelled,
        },
        name: format!("Template action {seed}-{index}"),
        description: Some(format!("Description {index}")),
        priority: Some((index % 9 + 1) as u32),
        contexts: Some(vec![format!("context-{seed}"), format!("step-{index}")]),
        scheduled_at: Some(local_timestamp),
        duration: Some((index + 1) as u32 * 15),
        due_date: Some(local_timestamp + Duration::days(1)),
        completed_at: Some(local_timestamp + Duration::hours(2)),
        created_at: Some(local_timestamp - Duration::days(1)),
        predecessors: Some(vec![PredecessorRef {
            raw_ref: format!("predecessor-{index}"),
            resolved_uuid: Some(predecessor_id),
        }]),
        charter: Some(format!("charter-{seed}")),
        alias: Some(format!("action-{seed}-{index}")),
        is_sequential: Some(index.is_multiple_of(2)),
        plan_id: Some(generated_id(0x5100_0005, seed, index)),
        external_occurrence_key: Some(format!("slot-{seed}-{index}")),
    }
}

fn template_scenario() -> impl Strategy<Value = TemplateScenario> {
    (
        any::<u32>(),
        3usize..9,
        prop::collection::vec(any::<u8>(), 6),
        any::<bool>(),
    )
        .prop_map(|(seed, len, parent_keys, use_override)| {
            let source_ids: Vec<_> = (0..len)
                .map(|index| generated_id(0x1100_0001, seed, index))
                .collect();
            let instance_ids: HashMap<_, _> = source_ids
                .iter()
                .enumerate()
                .map(|(index, source_id)| (*source_id, generated_id(0x2100_0002, seed, index)))
                .collect();
            let external_id = generated_id(0x3100_0003, seed, 0);
            let parent_override = use_override.then(|| generated_id(0x6100_0006, seed, 0));

            let parents: Vec<_> = (0..len)
                .map(|index| match index {
                    0 => DeclaredParent::TemplateRoot,
                    1 => DeclaredParent::Internal(0),
                    2 => DeclaredParent::External(external_id),
                    _ => match parent_keys[index - 3] % 3 {
                        0 => DeclaredParent::TemplateRoot,
                        1 => DeclaredParent::Internal(parent_keys[index - 3] as usize % index),
                        _ => DeclaredParent::External(external_id),
                    },
                })
                .collect();
            let source = parents
                .iter()
                .enumerate()
                .map(|(index, parent)| {
                    let parent_id = match parent {
                        DeclaredParent::TemplateRoot => None,
                        DeclaredParent::Internal(parent_index) => Some(source_ids[*parent_index]),
                        DeclaredParent::External(id) => Some(*id),
                    };
                    populated_action(source_ids[index], parent_id, seed, index)
                })
                .collect();

            TemplateScenario {
                source,
                instance_ids,
                parents,
                parent_override,
            }
        })
}

fn assert_non_identity_fields_preserved(source: &Action, instance: &Action) {
    assert_eq!(instance.state, source.state);
    assert_eq!(instance.name, source.name);
    assert_eq!(instance.description, source.description);
    assert_eq!(instance.priority, source.priority);
    assert_eq!(instance.contexts, source.contexts);
    assert_eq!(instance.scheduled_at, source.scheduled_at);
    assert_eq!(instance.duration, source.duration);
    assert_eq!(instance.due_date, source.due_date);
    assert_eq!(instance.completed_at, source.completed_at);
    assert_eq!(instance.created_at, source.created_at);
    assert_eq!(instance.predecessors, source.predecessors);
    assert_eq!(instance.charter, source.charter);
    assert_eq!(instance.alias, source.alias);
    assert_eq!(instance.is_sequential, source.is_sequential);
    assert_eq!(instance.plan_id, source.plan_id);
    assert_eq!(
        instance.external_occurrence_key,
        source.external_occurrence_key
    );
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        failure_persistence: Some(Box::new(FileFailurePersistence::SourceParallel("proptest-regressions"))),
        ..ProptestConfig::default()
    })]

    #[test]
    fn instantiation_obeys_declared_identity_and_parent_mapping(scenario in template_scenario()) {
        let original = scenario.source.clone();
        let instances = instantiate_template(
            &scenario.source,
            |source_id| scenario.instance_ids[&source_id],
            scenario.parent_override,
        );

        prop_assert_eq!(instances.len(), scenario.source.len());
        for (index, (source, instance)) in scenario.source.iter().zip(&instances).enumerate() {
            prop_assert_eq!(instance.id, scenario.instance_ids[&source.id]);
            let expected_parent = match scenario.parents[index] {
                DeclaredParent::TemplateRoot => scenario.parent_override,
                DeclaredParent::Internal(parent_index) => {
                    Some(scenario.instance_ids[&scenario.source[parent_index].id])
                }
                DeclaredParent::External(id) => Some(id),
            };
            prop_assert_eq!(instance.parent_id, expected_parent);
            assert_non_identity_fields_preserved(source, instance);
        }
        prop_assert_eq!(scenario.source, original);
    }

    #[test]
    fn resolution_obeys_local_global_missing_precedence(
        seed in any::<u32>(),
        shape in 0u8..3,
    ) {
        let tmp = tempfile::tempdir().expect("temporary workspace");
        let data_root = tmp.path();
        let charter_dir = data_root.join("charters").join(format!("charter-{seed}"));
        let name = format!("template-{seed}");
        let filename = format!("{name}.actions");
        let local = charter_dir.join("templates").join(&filename);
        let global = data_root.join("templates").join(&filename);

        let expected = match shape {
            0 => {
                fs::create_dir_all(local.parent().expect("local templates parent")).unwrap();
                fs::create_dir_all(global.parent().expect("global templates parent")).unwrap();
                fs::write(&local, "[ ] Local\n").unwrap();
                fs::write(&global, "[ ] Global\n").unwrap();
                Some(local)
            }
            1 => {
                fs::create_dir_all(global.parent().expect("global templates parent")).unwrap();
                fs::write(&global, "[ ] Global\n").unwrap();
                Some(global)
            }
            _ => None,
        };

        let resolved = resolve_template(&charter_dir, data_root, &name).unwrap();
        prop_assert_eq!(resolved, expected);
    }
}
