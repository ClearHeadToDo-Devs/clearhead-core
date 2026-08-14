use clearhead_core::{
    Action, Charter, DomainModel, MarkdownCharter, Objective, Plan, ReferenceEntity,
    ReferenceErrorKind, ReferenceMatch, ReferenceOptions, ReferenceSelection, ReferenceTarget,
    filter_model_for_action, filter_model_for_charter, filter_model_for_plan, resolve_reference,
    select_reference,
};
use proptest::prelude::*;
use proptest::test_runner::FileFailurePersistence;
use uuid::Uuid;

#[derive(Clone, Debug)]
enum ExpectedResolution {
    Resolved(ReferenceTarget),
    Ambiguous,
    NotFound,
    WrongType(&'static str),
}

#[derive(Clone, Debug)]
struct ReferenceScenario {
    model: DomainModel,
    input: String,
    expected: ExpectedResolution,
}

fn generated_id(namespace: u32, seed: u32) -> Uuid {
    Uuid::from_u128((u128::from(namespace) << 96) | u128::from(seed))
}

fn plan(id: Uuid, name: String) -> Plan {
    Plan {
        id,
        name,
        description: None,
        recurrence: None,
        due_recurrence: None,
        external_id: None,
        template_name: None,
        dtstart: None,
    }
}

fn action(id: Uuid, alias: String, parent_id: Option<Uuid>, plan_id: Option<Uuid>) -> Action {
    Action {
        id,
        alias: Some(alias.clone()),
        name: format!("Action {alias}"),
        parent_id,
        plan_id,
        ..Action::default()
    }
}

fn objective(seed: u32) -> Objective {
    Objective {
        id: generated_id(0xf000_000f, seed),
        title: Some(format!("Objective {seed}")),
        description: None,
        alias: Some(format!("objective-{seed}")),
        parent: None,
        metrics: None,
    }
}

fn charter(
    id: Uuid,
    alias: String,
    parent: Option<String>,
    plans: Vec<Plan>,
    actions: Vec<Action>,
) -> Charter {
    Charter {
        id,
        title: format!("Charter {alias}"),
        description: None,
        alias: Some(alias),
        parent,
        objectives: None,
        state: None,
        plans,
        actions,
    }
}

fn reference_scenario() -> impl Strategy<Value = ReferenceScenario> {
    (any::<u32>(), 0_u8..21).prop_map(|(seed, shape)| {
        let root_id = generated_id(0x1000_0001, seed);
        let child_charter_id = generated_id(0x2000_0002, seed);
        let plan_id = generated_id(0x3000_0003, seed);
        let root_action_id = generated_id(0x4000_0004, seed);
        let child_action_id = generated_id(0x5000_0005, seed);
        let planned_action_id = generated_id(0x6000_0006, seed);
        let distractor_id = generated_id(0x7000_0007, seed);

        let root_alias = format!("root-{seed}");
        let child_charter_alias = format!("child-charter-{seed}");
        let root_action_alias = format!("root-action-{seed}");
        let child_action_alias = format!("child-action-{seed}");
        let planned_action_alias = format!("planned-action-{seed}");

        let root_action = action(root_action_id, root_action_alias.clone(), None, None);
        let child_action = action(
            child_action_id,
            child_action_alias.clone(),
            Some(root_action_id),
            None,
        );
        let planned_action = action(
            planned_action_id,
            planned_action_alias.clone(),
            None,
            Some(plan_id),
        );

        let mut root = Charter {
            id: root_id,
            title: format!("Root {seed}"),
            description: None,
            alias: Some(root_alias.clone()),
            parent: None,
            objectives: None,
            state: None,
            plans: vec![plan(plan_id, format!("Plan {seed}"))],
            actions: vec![root_action, child_action, planned_action],
        };
        let mut child_charter = Charter {
            id: child_charter_id,
            title: format!("Child {seed}"),
            description: None,
            alias: Some(child_charter_alias.clone()),
            parent: Some(root_alias.clone()),
            objectives: None,
            state: None,
            plans: vec![],
            actions: vec![],
        };
        let mut distractor = Charter {
            id: distractor_id,
            title: format!("Distractor {seed}"),
            description: None,
            alias: Some(format!("distractor-{seed}")),
            parent: None,
            objectives: None,
            state: None,
            plans: vec![],
            actions: vec![],
        };

        let (input, expected) = match shape {
            0 => (
                root_id.to_string(),
                ExpectedResolution::Resolved(ReferenceTarget::Charter(root_id)),
            ),
            1 => (
                plan_id.to_string()[..8].to_string(),
                ExpectedResolution::Resolved(ReferenceTarget::Plan(plan_id)),
            ),
            2 => (
                root_action_alias.to_ascii_uppercase(),
                ExpectedResolution::Resolved(ReferenceTarget::Action(root_action_id)),
            ),
            3 => (
                format!("c:{root_alias}/{child_charter_alias}"),
                ExpectedResolution::Resolved(ReferenceTarget::Charter(child_charter_id)),
            ),
            4 => (
                format!("p:{root_alias}/{}", &plan_id.to_string()[..8]),
                ExpectedResolution::Resolved(ReferenceTarget::Plan(plan_id)),
            ),
            5 => (
                format!("a:{root_alias}/{root_action_alias}"),
                ExpectedResolution::Resolved(ReferenceTarget::Action(root_action_id)),
            ),
            6 => (
                format!("{root_alias}/{root_action_alias}/{child_action_alias}"),
                ExpectedResolution::Resolved(ReferenceTarget::Action(child_action_id)),
            ),
            7 => (
                format!("{root_alias}/{}", &plan_id.to_string()[..8]),
                ExpectedResolution::Resolved(ReferenceTarget::Plan(plan_id)),
            ),
            8 => {
                let shared = format!("shared-{seed}");
                root.alias = Some(shared.clone());
                distractor.alias = Some(shared.clone());
                (shared, ExpectedResolution::Ambiguous)
            }
            9 => (format!("missing-{seed}"), ExpectedResolution::NotFound),
            10 => {
                root.alias = Some(plan_id.to_string());
                (
                    plan_id.to_string(),
                    ExpectedResolution::Resolved(ReferenceTarget::Plan(plan_id)),
                )
            }
            11 => {
                let shared = format!("global-charter-{seed}");
                root.alias = Some(shared.clone());
                distractor.alias = Some(shared.clone());
                (format!("c:{shared}"), ExpectedResolution::Ambiguous)
            }
            12 => {
                distractor.plans.push(plan(
                    generated_id(0x3000_0003, seed.wrapping_add(1)),
                    format!("Ambiguous Plan {seed}"),
                ));
                (
                    format!("p:{}", &plan_id.to_string()[..8]),
                    ExpectedResolution::Ambiguous,
                )
            }
            13 => {
                distractor.actions.push(action(
                    generated_id(0x7100_0007, seed),
                    root_action_alias.clone(),
                    None,
                    None,
                ));
                (
                    format!("a:{root_action_alias}"),
                    ExpectedResolution::Ambiguous,
                )
            }
            14 => {
                let shared = format!("ambiguous-root-{seed}");
                root.alias = Some(shared.clone());
                distractor.alias = Some(shared.clone());
                (
                    format!("{shared}/{child_charter_alias}"),
                    ExpectedResolution::Ambiguous,
                )
            }
            15 => {
                let shared = format!("ambiguous-child-{seed}");
                child_charter.alias = Some(shared.clone());
                root.actions[0].alias = Some(shared.clone());
                (
                    format!("{root_alias}/{shared}"),
                    ExpectedResolution::Ambiguous,
                )
            }
            16 => {
                root.actions.push(action(
                    generated_id(0x6100_0006, seed),
                    planned_action_alias.clone(),
                    None,
                    Some(plan_id),
                ));
                (
                    format!(
                        "{root_alias}/{}/{}",
                        &plan_id.to_string()[..8],
                        planned_action_alias
                    ),
                    ExpectedResolution::Ambiguous,
                )
            }
            17 => {
                root.actions.push(action(
                    generated_id(0x5100_0005, seed),
                    child_action_alias.clone(),
                    Some(root_action_id),
                    None,
                ));
                (
                    format!("{root_alias}/{root_action_alias}/{child_action_alias}"),
                    ExpectedResolution::Ambiguous,
                )
            }
            18 => (
                format!("p:{root_alias}/{root_action_alias}"),
                ExpectedResolution::WrongType("non-plan"),
            ),
            19 => (
                format!("c:{root_alias}/{}", &plan_id.to_string()[..8]),
                ExpectedResolution::WrongType("non-charter"),
            ),
            _ => (
                format!("a:{root_alias}/{child_charter_alias}"),
                ExpectedResolution::WrongType("non-action"),
            ),
        };

        ReferenceScenario {
            model: DomainModel {
                objectives: vec![],
                charters: vec![root, child_charter, distractor],
            },
            input,
            expected,
        }
    })
}

fn property_config() -> ProptestConfig {
    let mut config = ProptestConfig::with_failure_persistence(FileFailurePersistence::Direct(
        "tests/generated_reference_invariants.proptest-regressions",
    ));
    config.cases = 256;
    config
}

proptest! {
    #![proptest_config(property_config())]

    #[test]
    fn generated_reference_scenarios_have_the_declared_outcome(
        scenario in reference_scenario(),
    ) {
        let actual = resolve_reference(
            &scenario.model,
            &scenario.input,
            &ReferenceOptions::default(),
        );

        match &scenario.expected {
            ExpectedResolution::Resolved(expected) => {
                prop_assert_eq!(actual.ok(), Some(expected.clone()));
            }
            ExpectedResolution::Ambiguous => {
                let error = actual.expect_err("generated ambiguous references must be rejected");
                prop_assert_eq!(error.kind(), ReferenceErrorKind::Ambiguous);
            }
            ExpectedResolution::NotFound => {
                let error = actual.expect_err("generated missing references must be rejected");
                prop_assert_eq!(error.kind(), ReferenceErrorKind::NotFound);
                prop_assert_eq!(
                    error.to_string(),
                    format!("No entity matches reference '{}'", scenario.input),
                );
            }
            ExpectedResolution::WrongType(expected_fragment) => {
                let error = actual.expect_err("generated wrong-type paths must be rejected");
                prop_assert_eq!(error.kind(), ReferenceErrorKind::TypeMismatch);
                prop_assert!(error.to_string().contains(expected_fragment));
            }
        }
    }

    #[test]
    fn disabling_prefixes_treats_prefix_syntax_as_plain_reference_text(
        seed in any::<u32>(),
        prefix_shape in 0_u8..3,
    ) {
        let charter_id = generated_id(0x8100_0008, seed);
        let plan_id = generated_id(0x8200_0008, seed);
        let action_id = generated_id(0x8300_0008, seed);
        let charter_alias = format!("prefix-charter-{seed}");
        let action_alias = format!("prefix-action-{seed}");
        let model = DomainModel {
            objectives: vec![],
            charters: vec![charter(
                charter_id,
                charter_alias.clone(),
                None,
                vec![plan(plan_id, format!("Prefix Plan {seed}"))],
                vec![action(action_id, action_alias.clone(), None, None)],
            )],
        };
        let input = match prefix_shape {
            0 => format!("c:{charter_alias}"),
            1 => format!("p:{plan_id}"),
            _ => format!("a:{action_alias}"),
        };
        let options = ReferenceOptions {
            allow_prefixes: false,
            ..ReferenceOptions::default()
        };

        let error = resolve_reference(&model, &input, &options)
            .expect_err("disabled prefixes must remain part of plain reference text");

        prop_assert_eq!(error.kind(), ReferenceErrorKind::NotFound);
        prop_assert_eq!(
            error.to_string(),
            format!("No entity matches reference '{input}'"),
        );

        let (plain_input, expected) = match prefix_shape {
            0 => (charter_alias, ReferenceTarget::Charter(charter_id)),
            1 => (plan_id.to_string(), ReferenceTarget::Plan(plan_id)),
            _ => (action_alias, ReferenceTarget::Action(action_id)),
        };
        let resolved = resolve_reference(&model, &plain_input, &options)
            .expect("disabled prefixes must not disable ordinary references");
        prop_assert_eq!(resolved, expected);
    }

    #[test]
    fn generated_entity_adapters_preserve_identity_and_alias_capabilities(
        seed in any::<u32>(),
    ) {
        let charter_id = generated_id(0xa000_000a, seed);
        let charter_alias = format!("charter-{seed}");
        let charter = charter(
            charter_id,
            charter_alias.clone(),
            None,
            vec![],
            vec![],
        );
        let markdown_charter: MarkdownCharter = charter.clone().into();
        let plan_id = generated_id(0xb000_000b, seed);
        let generated_plan = plan(plan_id, format!("Plan {seed}"));
        let action_id = generated_id(0xb100_000b, seed);
        let action_alias = format!("action-{seed}");
        let generated_action = action(action_id, action_alias.clone(), None, None);

        prop_assert_eq!(charter.reference_id(), charter_id);
        prop_assert_eq!(charter.reference_alias(), Some(charter_alias.as_str()));
        prop_assert_eq!(markdown_charter.reference_id(), charter_id);
        prop_assert_eq!(markdown_charter.reference_alias(), Some(charter_alias.as_str()));
        prop_assert_eq!(generated_plan.reference_id(), plan_id);
        prop_assert_eq!(generated_plan.reference_alias(), None);
        prop_assert_eq!(generated_action.reference_id(), action_id);
        prop_assert_eq!(generated_action.reference_alias(), Some(action_alias.as_str()));
    }

    #[test]
    fn generated_charter_projection_matches_declared_topology(
        seed in any::<u32>(),
        recursive in any::<bool>(),
        target_exists in any::<bool>(),
    ) {
        let root_id = generated_id(0xc000_000c, seed);
        let child_id = generated_id(0xc100_000c, seed);
        let grandchild_id = generated_id(0xc200_000c, seed);
        let unrelated_id = generated_id(0xc300_000c, seed);
        let root_alias = format!("projection-root-{seed}");
        let child_alias = format!("projection-child-{seed}");
        let root = charter(root_id, root_alias.clone(), None, vec![], vec![]);
        let unrelated = charter(
            unrelated_id,
            format!("projection-unrelated-{seed}"),
            None,
            vec![],
            vec![],
        );
        let child = charter(
            child_id,
            child_alias.clone(),
            Some(root_alias),
            vec![],
            vec![],
        );
        let grandchild = charter(
            grandchild_id,
            format!("projection-grandchild-{seed}"),
            Some(child_alias),
            vec![],
            vec![],
        );
        let model = DomainModel {
            objectives: vec![objective(seed)],
            charters: vec![
                root.clone(),
                unrelated,
                child.clone(),
                grandchild.clone(),
            ],
        };
        let expected_charters = match (target_exists, recursive) {
            (false, _) => vec![],
            (true, true) => vec![root.clone(), child, grandchild],
            (true, false) => vec![root],
        };
        let selected_id = if target_exists {
            root_id
        } else {
            generated_id(0xc400_000c, seed)
        };

        let projected = filter_model_for_charter(&model, selected_id, recursive);

        prop_assert!(projected.objectives.is_empty());
        prop_assert_eq!(projected.charters, expected_charters);
    }

    #[test]
    fn generated_plan_projection_keeps_only_the_declared_owner_and_plan(
        seed in any::<u32>(),
        target_exists in any::<bool>(),
    ) {
        let charter_id = generated_id(0xd000_000d, seed);
        let target_plan = plan(generated_id(0xd100_000d, seed), format!("Target {seed}"));
        let other_plan = plan(generated_id(0xd200_000d, seed), format!("Other {seed}"));
        let owner = charter(
            charter_id,
            format!("plan-owner-{seed}"),
            None,
            vec![target_plan.clone(), other_plan],
            vec![action(
                generated_id(0xd400_000d, seed),
                format!("plan-owner-action-{seed}"),
                None,
                Some(target_plan.id),
            )],
        );
        let model = DomainModel {
            objectives: vec![objective(seed)],
            charters: vec![
                owner.clone(),
                charter(
                    generated_id(0xd300_000d, seed),
                    format!("plan-unrelated-{seed}"),
                    None,
                    vec![],
                    vec![],
                ),
            ],
        };
        let mut expected_owner = owner;
        expected_owner.plans = vec![target_plan.clone()];
        expected_owner.actions.clear();

        let selected_id = if target_exists {
            target_plan.id
        } else {
            generated_id(0xd500_000d, seed)
        };
        let expected_charters = if target_exists {
            vec![expected_owner]
        } else {
            vec![]
        };
        let projected = filter_model_for_plan(&model, selected_id);

        prop_assert!(projected.objectives.is_empty());
        prop_assert_eq!(projected.charters, expected_charters);
    }

    #[test]
    fn generated_action_projection_keeps_its_declared_plan_only(
        seed in any::<u32>(),
        has_plan in any::<bool>(),
        target_exists in any::<bool>(),
    ) {
        let charter_id = generated_id(0xe000_000e, seed);
        let target_plan = plan(generated_id(0xe100_000e, seed), format!("Target {seed}"));
        let other_plan = plan(generated_id(0xe200_000e, seed), format!("Other {seed}"));
        let target_action = action(
            generated_id(0xe300_000e, seed),
            format!("target-action-{seed}"),
            None,
            has_plan.then_some(target_plan.id),
        );
        let other_action = action(
            generated_id(0xe400_000e, seed),
            format!("other-action-{seed}"),
            None,
            Some(other_plan.id),
        );
        let owner = charter(
            charter_id,
            format!("action-owner-{seed}"),
            None,
            vec![target_plan.clone(), other_plan],
            vec![target_action.clone(), other_action],
        );
        let model = DomainModel {
            objectives: vec![objective(seed)],
            charters: vec![
                owner.clone(),
                charter(
                    generated_id(0xe500_000e, seed),
                    format!("action-unrelated-{seed}"),
                    None,
                    vec![],
                    vec![],
                ),
            ],
        };
        let mut expected_owner = owner;
        expected_owner.actions = vec![target_action.clone()];
        expected_owner.plans = if has_plan { vec![target_plan] } else { vec![] };

        let selected_id = if target_exists {
            target_action.id
        } else {
            generated_id(0xe600_000e, seed)
        };
        let expected_charters = if target_exists {
            vec![expected_owner]
        } else {
            vec![]
        };
        let projected = filter_model_for_action(&model, selected_id);

        prop_assert!(projected.objectives.is_empty());
        prop_assert_eq!(projected.charters, expected_charters);
    }

    #[test]
    fn generated_selection_scenarios_respect_identity_precedence(
        seed in any::<u32>(),
        use_full_uuid in any::<bool>(),
    ) {
        let identity_id = generated_id(0x8000_0008, seed);
        let alias_id = generated_id(0x9000_0009, seed);
        let input = if use_full_uuid {
            identity_id.to_string()
        } else {
            identity_id.to_string()[..8].to_string()
        };
        let candidates = vec![
            Charter {
                id: alias_id,
                title: "Alias candidate".to_string(),
                description: None,
                alias: Some(input.clone()),
                parent: None,
                objectives: None,
                state: None,
                plans: vec![],
                actions: vec![],
            },
            Charter {
                id: identity_id,
                title: "Identity candidate".to_string(),
                description: None,
                alias: Some(format!("identity-{seed}")),
                parent: None,
                objectives: None,
                state: None,
                plans: vec![],
                actions: vec![],
            },
        ];
        let expected_match = if use_full_uuid {
            ReferenceMatch::FullUuid
        } else {
            ReferenceMatch::ShortUuid
        };

        prop_assert_eq!(
            select_reference(&candidates, &input),
            ReferenceSelection::Unique {
                index: 1,
                matched_by: expected_match,
            },
        );
    }
}
