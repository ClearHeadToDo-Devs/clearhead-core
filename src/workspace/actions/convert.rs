use super::{Action, ActionList, ActionState, PredecessorRef};
use crate::domain::{ActPhase, Action as DomainAction, Charter, DomainModel};
use uuid::Uuid;

/// Namespace UUID for the synthetic inbox charter.
pub const INBOX_CHARTER_NS: Uuid = Uuid::from_bytes([
    0x69, 0x6e, 0x62, 0x6f, 0x78, 0x2d, 0x63, 0x68, 0x61, 0x72, 0x74, 0x65, 0x72, 0x2d, 0x6e, 0x73,
]);

/// Convert an ActionList into a Charter with a deterministic ID derived from the name.
pub fn from_actions_with_charter(actions: &ActionList, charter_name: String) -> Charter {
    let mut charter = crate::workspace::charter::implicit_charter(&charter_name);
    charter.actions = actions.iter().map(domain_action_from_action).collect();
    charter
}

fn domain_action_from_action(action: &Action) -> DomainAction {
    DomainAction {
        id: action.id,
        name: action.name.clone(),
        description: action.description.clone(),
        priority: action.priority,
        contexts: action.context_list.clone(),
        parent: action.parent_id,
        alias: action.alias.clone(),
        is_sequential: action.is_sequential,
        depends_on: action.predecessors.as_ref().map(|preds| {
            preds.iter().filter_map(|p| p.resolved_uuid).collect()
        }),
        plan_id: None,
        external_schedule_id: None,
        external_occurrence_key: None,
        phase: match action.state {
            ActionState::NotStarted => ActPhase::NotStarted,
            ActionState::InProgress => ActPhase::InProgress,
            ActionState::Completed => ActPhase::Completed,
            ActionState::BlockedorAwaiting => ActPhase::Blocked,
            ActionState::Cancelled => ActPhase::Cancelled,
        },
        scheduled_at: action.do_date_time,
        due_date: action.due_date_time,
        duration: action.do_duration,
        completed_at: action.completed_date_time,
        created_at: action.created_date_time,
        ..Default::default()
    }
}

impl From<&DomainAction> for Action {
    fn from(action: &DomainAction) -> Self {
        Action {
            id: action.id,
            parent_id: action.parent,
            state: match action.phase {
                ActPhase::NotStarted => ActionState::NotStarted,
                ActPhase::InProgress => ActionState::InProgress,
                ActPhase::Completed => ActionState::Completed,
                ActPhase::Blocked => ActionState::BlockedorAwaiting,
                ActPhase::Cancelled => ActionState::Cancelled,
            },
            name: action.name.clone(),
            description: action.description.clone(),
            priority: action.priority,
            context_list: action.contexts.clone(),
            do_date_time: action.scheduled_at,
            do_duration: action.duration,
            due_date_time: action.due_date,
            completed_date_time: action.completed_at,
            created_date_time: action.created_at,
            predecessors: action.depends_on.as_ref().map(|uuids| {
                uuids
                    .iter()
                    .map(|u| PredecessorRef {
                        raw_ref: u.to_string(),
                        resolved_uuid: Some(*u),
                    })
                    .collect()
            }),
            charter: None,
            alias: action.alias.clone(),
            is_sequential: action.is_sequential,
        }
    }
}

/// Patch a primary ActionList with updates from a secondary list.
///
/// Updates existing actions by ID, appends new ones.
pub fn patch_action_list(primary: &mut ActionList, secondary: &ActionList) {
    for patch_action in secondary {
        if let Some(original) = primary.iter_mut().find(|a| a.id == patch_action.id) {
            *original = patch_action.clone();
        } else {
            primary.push(patch_action.clone());
        }
    }
}

/// Convert a DomainModel back to an ActionList.
pub fn to_action_list(model: &DomainModel) -> ActionList {
    model
        .charters
        .iter()
        .flat_map(|charter| charter.actions.iter())
        .map(Action::from)
        .collect()
}

/// Convert a DomainModel back to an ActionList in a specific order.
pub fn to_action_list_ordered(model: &DomainModel, plan_order: &[String]) -> ActionList {
    plan_order
        .iter()
        .filter_map(|id| {
            model
                .charters
                .iter()
                .flat_map(|charter| charter.actions.iter())
                .find(|action| action.id.to_string() == *id)
        })
        .map(Action::from)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{ActPhase, Charter, DomainModel};

    fn make_action(name: &str) -> Action {
        Action {
            name: name.to_string(),
            created_date_time: None,
            ..Default::default()
        }
    }

    #[test]
    fn action_to_domain_action_state_mapping() {
        use ActionState::*;
        let cases = [
            (NotStarted, ActPhase::NotStarted),
            (InProgress, ActPhase::InProgress),
            (Completed, ActPhase::Completed),
            (BlockedorAwaiting, ActPhase::Blocked),
            (Cancelled, ActPhase::Cancelled),
        ];
        for (action_state, expected_phase) in cases {
            let mut action = make_action("task");
            action.state = action_state;
            assert_eq!(
                domain_action_from_action(&action).phase,
                expected_phase,
                "state: {action_state:?}"
            );
        }
    }

    #[test]
    fn round_trip_preserves_all_fields() {
        let mut action = make_action("Ship it");
        action.description = Some("details here".to_string());
        action.priority = Some(2);
        action.context_list = Some(vec!["work".to_string(), "deep".to_string()]);
        action.alias = Some("ship".to_string());
        action.is_sequential = Some(true);
        action.state = ActionState::InProgress;

        let dep_id = Uuid::new_v4();
        action.predecessors = Some(vec![PredecessorRef {
            raw_ref: dep_id.to_string(),
            resolved_uuid: Some(dep_id),
        }]);

        let charter = from_actions_with_charter(&vec![action.clone()], "work".to_string());
        let model = DomainModel {
            objectives: vec![],
            charters: vec![charter],
        };
        let roundtripped = to_action_list(&model)
            .into_iter()
            .next()
            .expect("one action");

        assert_eq!(roundtripped.id, action.id);
        assert_eq!(roundtripped.name, action.name);
        assert_eq!(roundtripped.description, action.description);
        assert_eq!(roundtripped.priority, action.priority);
        assert_eq!(roundtripped.context_list, action.context_list);
        assert_eq!(roundtripped.alias, action.alias);
        assert_eq!(roundtripped.is_sequential, action.is_sequential);
        assert_eq!(roundtripped.state, action.state);
        let deps = roundtripped.predecessors.unwrap();
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0].resolved_uuid, Some(dep_id));
    }

    #[test]
    fn unresolved_predecessors_are_dropped() {
        let mut action = make_action("task");
        let resolved_id = Uuid::new_v4();
        action.predecessors = Some(vec![
            PredecessorRef {
                raw_ref: "some name".to_string(),
                resolved_uuid: None,
            },
            PredecessorRef {
                raw_ref: resolved_id.to_string(),
                resolved_uuid: Some(resolved_id),
            },
        ]);

        let domain_action = domain_action_from_action(&action);
        let depends_on = domain_action.depends_on.unwrap();
        assert_eq!(depends_on, vec![resolved_id]);
    }

    #[test]
    fn to_action_list_ordered_respects_order_and_excludes_extras() {
        let id_a = Uuid::new_v4();
        let id_b = Uuid::new_v4();
        let id_c = Uuid::new_v4();

        let model = DomainModel {
            charters: vec![Charter {
                id: Uuid::new_v4(),
                title: "test".to_string(),
                actions: vec![
                    DomainAction {
                        id: id_a,
                        name: "Alpha".to_string(),
                        ..Default::default()
                    },
                    DomainAction {
                        id: id_b,
                        name: "Beta".to_string(),
                        ..Default::default()
                    },
                    DomainAction {
                        id: id_c,
                        name: "Gamma".to_string(),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            }],
            objectives: vec![],
        };

        // Request B then A — C excluded entirely
        let order = vec![id_b.to_string(), id_a.to_string()];
        let actions = to_action_list_ordered(&model, &order);

        assert_eq!(actions.len(), 2);
        assert_eq!(actions[0].name, "Beta");
        assert_eq!(actions[1].name, "Alpha");
    }

    #[test]
    fn domain_action_to_workspace_action_preserves_fields() {
        let action = DomainAction {
            id: Uuid::new_v4(),
            name: "task".to_string(),
            duration: Some(45),
            phase: ActPhase::InProgress,
            ..Default::default()
        };

        let workspace_action = Action::from(&action);
        assert_eq!(workspace_action.do_duration, Some(45));
        assert_eq!(workspace_action.state, ActionState::InProgress);
    }

    #[test]
    fn from_actions_with_charter_produces_domain_actions_not_plans() {
        let charter = from_actions_with_charter(&vec![make_action("empty")], "demo".to_string());
        assert!(charter.plans.is_empty());
        assert_eq!(charter.actions.len(), 1);
    }
}
