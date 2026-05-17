use super::ActionList;
use crate::domain::{Charter, DomainModel};
use uuid::Uuid;

/// Namespace UUID for the synthetic inbox charter.
pub const INBOX_CHARTER_NS: Uuid = Uuid::from_bytes([
    0x69, 0x6e, 0x62, 0x6f, 0x78, 0x2d, 0x63, 0x68, 0x61, 0x72, 0x74, 0x65, 0x72, 0x2d, 0x6e, 0x73,
]);

/// Convert an ActionList into a Charter with a deterministic ID derived from the name.
///
/// Since `Action` is now the unified type, no field conversion is needed.
pub fn from_actions_with_charter(actions: &ActionList, charter_name: String) -> Charter {
    let mut charter = crate::workspace::charter::implicit_charter(&charter_name);
    charter.actions = actions.clone();
    charter
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
        .flat_map(|charter| charter.actions.iter().cloned())
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
                .cloned()
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Action, Charter, DomainModel};
    use crate::workspace::actions::ActionState;

    fn make_action(name: &str) -> Action {
        Action::new(name)
    }

    #[test]
    fn from_actions_with_charter_produces_actions() {
        let charter = from_actions_with_charter(&vec![make_action("empty")], "demo".to_string());
        assert!(charter.plans.is_empty());
        assert_eq!(charter.actions.len(), 1);
    }

    #[test]
    fn round_trip_is_identity() {
        let action = Action {
            name: "Ship it".to_string(),
            description: Some("details here".to_string()),
            priority: Some(2),
            contexts: Some(vec!["work".to_string(), "deep".to_string()]),
            alias: Some("ship".to_string()),
            is_sequential: Some(true),
            state: ActionState::InProgress,
            predecessors: Some(vec![crate::domain::PredecessorRef {
                raw_ref: "abc".to_string(),
                resolved_uuid: Some(uuid::Uuid::new_v4()),
            }]),
            ..Default::default()
        };

        let charter = from_actions_with_charter(&vec![action.clone()], "work".to_string());
        let model = DomainModel { objectives: vec![], charters: vec![charter] };
        let roundtripped = to_action_list(&model).into_iter().next().unwrap();

        assert_eq!(roundtripped.id, action.id);
        assert_eq!(roundtripped.name, action.name);
        assert_eq!(roundtripped.description, action.description);
        assert_eq!(roundtripped.priority, action.priority);
        assert_eq!(roundtripped.contexts, action.contexts);
        assert_eq!(roundtripped.alias, action.alias);
        assert_eq!(roundtripped.is_sequential, action.is_sequential);
        assert_eq!(roundtripped.state, action.state);
    }

    #[test]
    fn to_action_list_ordered_respects_order() {
        let id_a = uuid::Uuid::new_v4();
        let id_b = uuid::Uuid::new_v4();

        let model = DomainModel {
            charters: vec![Charter {
                id: uuid::Uuid::new_v4(),
                title: "test".to_string(),
                actions: vec![
                    Action { id: id_a, name: "Alpha".to_string(), ..Default::default() },
                    Action { id: id_b, name: "Beta".to_string(), ..Default::default() },
                ],
                ..Default::default()
            }],
            objectives: vec![],
        };

        let order = vec![id_b.to_string(), id_a.to_string()];
        let actions = to_action_list_ordered(&model, &order);

        assert_eq!(actions.len(), 2);
        assert_eq!(actions[0].name, "Beta");
        assert_eq!(actions[1].name, "Alpha");
    }
}
