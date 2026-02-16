use crate::actions::{Action, ActionList, ActionState};
use serde_json::{Value, json};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, PartialEq, Clone)]
pub struct ActionDiff {
    pub id: Uuid,
    pub changes: Vec<FieldChange>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldChange {
    State {
        old: ActionState,
        new: ActionState,
    },
    Name {
        old: String,
        new: String,
    },
    Description {
        old: Option<String>,
        new: Option<String>,
    },
    Priority {
        old: Option<u32>,
        new: Option<u32>,
    },
    DoDate {
        old: Option<String>,
        new: Option<String>,
    },
    CompletedDate {
        old: Option<String>,
        new: Option<String>,
    },
    // We can add more fields as needed
    Generic {
        field: String,
        old: Value,
        new: Value,
    },
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Diff {
    pub added: Vec<Action>,
    pub removed: Vec<Action>,
    pub modified: Vec<ActionDiff>,
}

impl Diff {
    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty() && self.modified.is_empty()
    }
}

pub fn diff_actions(old_actions: &ActionList, new_actions: &ActionList) -> Diff {
    let mut diff = Diff::default();
    let old_map: HashMap<Uuid, &Action> = old_actions.iter().map(|a| (a.id, a)).collect();
    let new_map: HashMap<Uuid, &Action> = new_actions.iter().map(|a| (a.id, a)).collect();

    // Check for added and modified
    for new_action in new_actions {
        if let Some(old_action) = old_map.get(&new_action.id) {
            let changes = compare_action(old_action, new_action);
            if !changes.is_empty() {
                diff.modified.push(ActionDiff {
                    id: new_action.id,
                    changes,
                });
            }
        } else {
            diff.added.push(new_action.clone());
        }
    }

    // Check for removed
    for old_action in old_actions {
        if !new_map.contains_key(&old_action.id) {
            diff.removed.push(old_action.clone());
        }
    }

    diff
}

fn compare_action(old: &Action, new: &Action) -> Vec<FieldChange> {
    let mut changes = Vec::new();

    if old.state != new.state {
        changes.push(FieldChange::State {
            old: old.state,
            new: new.state,
        });
    }

    if old.name != new.name {
        changes.push(FieldChange::Name {
            old: old.name.clone(),
            new: new.name.clone(),
        });
    }

    if old.description != new.description {
        changes.push(FieldChange::Description {
            old: old.description.clone(),
            new: new.description.clone(),
        });
    }

    if old.priority != new.priority {
        changes.push(FieldChange::Priority {
            old: old.priority,
            new: new.priority,
        });
    }

    if old.do_date_time != new.do_date_time {
        changes.push(FieldChange::DoDate {
            old: old.do_date_time.map(|d| d.to_rfc3339()),
            new: new.do_date_time.map(|d| d.to_rfc3339()),
        });
    }

    if old.completed_date_time != new.completed_date_time {
        changes.push(FieldChange::CompletedDate {
            old: old.completed_date_time.map(|d| d.to_rfc3339()),
            new: new.completed_date_time.map(|d| d.to_rfc3339()),
        });
    }

    // Compare contexts (as sets for robust comparison, but list equality for now)
    if old.context_list != new.context_list {
        changes.push(FieldChange::Generic {
            field: "contexts".to_string(),
            old: json!(old.context_list),
            new: json!(new.context_list),
        });
    }

    if old.story != new.story {
        changes.push(FieldChange::Generic {
            field: "story".to_string(),
            old: json!(old.story),
            new: json!(new.story),
        });
    }

    changes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::ActionState;

    fn make_action(id: &str, name: &str) -> Action {
        Action {
            id: Uuid::parse_str(id).unwrap(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: name.to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: None,
            do_duration: None,
            recurrence: None,
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        }
    }

    #[test]
    fn test_diff_added() {
        let old = vec![];
        let new = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "New Action",
        )];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.added.len(), 1);
        assert_eq!(diff.removed.len(), 0);
        assert_eq!(diff.modified.len(), 0);
        assert_eq!(diff.added[0].name, "New Action");
    }

    #[test]
    fn test_diff_removed() {
        let old = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "Old Action",
        )];
        let new = vec![];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.added.len(), 0);
        assert_eq!(diff.removed.len(), 1);
        assert_eq!(diff.modified.len(), 0);
        assert_eq!(diff.removed[0].name, "Old Action");
    }

    #[test]
    fn test_diff_modified_state() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let mut old_action = make_action(id, "Task");
        old_action.state = ActionState::NotStarted;

        let mut new_action = make_action(id, "Task");
        new_action.state = ActionState::Completed;

        let old = vec![old_action];
        let new = vec![new_action];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.modified.len(), 1);
        let modification = &diff.modified[0];
        assert_eq!(modification.id.to_string(), id);
        assert_eq!(modification.changes.len(), 1);

        match &modification.changes[0] {
            FieldChange::State { old, new } => {
                assert_eq!(*old, ActionState::NotStarted);
                assert_eq!(*new, ActionState::Completed);
            }
            _ => panic!("Expected state change"),
        }
    }

    #[test]
    fn test_diff_modified_multiple_fields() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let mut old_action = make_action(id, "Task");
        old_action.priority = Some(1);

        let mut new_action = make_action(id, "Updated Task"); // Name change
        new_action.priority = Some(2); // Priority change

        let diff = diff_actions(&vec![old_action], &vec![new_action]);

        assert_eq!(diff.modified.len(), 1);
        let changes = &diff.modified[0].changes;
        assert_eq!(changes.len(), 2);

        // Sort changes isn't guaranteed, so check existence
        let has_name_change = changes
            .iter()
            .any(|c| matches!(c, FieldChange::Name { .. }));
        let has_prio_change = changes
            .iter()
            .any(|c| matches!(c, FieldChange::Priority { .. }));

        assert!(has_name_change);
        assert!(has_prio_change);
    }
}
