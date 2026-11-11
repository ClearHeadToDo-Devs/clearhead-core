use super::{Action, ActionList, ActionState};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::collections::HashSet;
use uuid::Uuid;

#[derive(Debug, PartialEq, Clone)]
pub struct SemanticMerge {
    pub merged: ActionList,
    pub conflicts: Vec<Conflict>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Conflict {
    FieldConflict(FieldConflict),
    ActionDeleted(ActionDeleted),
    BothAdded(BothAdded),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldConflict {
    pub id: Uuid,
    pub fields: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ActionDeleted {
    pub id: Uuid,
    pub deleted_by: Side,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Side {
    Ours,
    Theirs,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BothAdded {
    pub id: Uuid,
}

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

pub fn semantic_merge(base: &ActionList, ours: &ActionList, theirs: &ActionList) -> SemanticMerge {
    let mut merged = vec![];
    let mut conflicts = vec![];

    let index = (
        index_actions(base),
        index_actions(ours),
        index_actions(theirs),
    );

    let mut seen = HashSet::new();
    let ids: Vec<Uuid> = ours
        .iter()
        .chain(theirs)
        .chain(base)
        .map(|a| a.id)
        .filter(|id| seen.insert(*id)) // insert returns false if it was already there
        .collect();

    for id in ids {
        let base_action = index.0.get(&id);
        let our_action = index.1.get(&id);
        let their_action = index.2.get(&id);

        match (base_action, our_action, their_action) {
            (Some(base), Some(ours), Some(theirs)) => {
                let (merged_action, conflict) = merge_action(base, ours, theirs);
                merged.push(merged_action);
                if let Some(conflict) = conflict {
                    conflicts.push(conflict);
                }
            }
            (None, Some(ours), Some(theirs)) => {
                // Both added the same action independently; check for conflicts
                if ours == theirs {
                    merged.push((*ours).clone());
                } else {
                    // Conflict: both added different versions of the same action
                    merged.push((*ours).clone()); // or choose one arbitrarily
                    conflicts.push(Conflict::BothAdded(BothAdded { id }));
                }
            }
            (Some(base), Some(ours), None) => {
                if ours != base {
                    // Theirs removed it, but we modified it; keep ours and note conflict
                    merged.push((*ours).clone());
                    conflicts.push(Conflict::ActionDeleted(ActionDeleted {
                        id,
                        deleted_by: Side::Theirs,
                    }));
                }
            }
            (Some(base), None, Some(theirs)) => {
                if theirs != base {
                    // Ours removed it, but they modified it; keep theirs and note conflict
                    merged.push((*theirs).clone());
                    conflicts.push(Conflict::ActionDeleted(ActionDeleted {
                        id,
                        deleted_by: Side::Ours,
                    }));
                }
            }
            (Some(_), None, None) => {
                // Both removed it; nothing to do
            }
            (None, Some(ours), None) => {
                // Only ours added it
                merged.push((*ours).clone());
            }
            (None, None, Some(theirs)) => {
                // Only theirs added it
                merged.push((*theirs).clone());
            }
            (None, None, None) => {
                unreachable!()
            }
        }
    }
    SemanticMerge { merged, conflicts }
}

fn index_actions(actions: &ActionList) -> HashMap<Uuid, &Action> {
    actions.iter().map(|a| (a.id, a)).collect()
}

fn merge_action(base: &Action, ours: &Action, theirs: &Action) -> (Action, Option<Conflict>) {
    let mut fields = Vec::new();
    // Three-way merge one field by name; the label always matches the field.
    macro_rules! mergeField {
        ($f:ident) => {
            pick(stringify!($f), &base.$f, &ours.$f, &theirs.$f, &mut fields)
        };
    }
    // A struct literal without `..`: adding a field to Action fails to compile here
    // until the merge decides what to do with it.
    let merged = Action {
        id: ours.id,
        recurrence_rule: mergeField!(recurrence_rule),
        parent_id: mergeField!(parent_id),
        state: mergeField!(state),
        name: mergeField!(name),
        description: mergeField!(description),
        priority: mergeField!(priority),
        contexts: mergeField!(contexts),
        planned: mergeField!(planned),
        due_date: mergeField!(due_date),
        completed_at: mergeField!(completed_at),
        created_at: mergeField!(created_at),
        predecessors: mergeField!(predecessors),
        charter: mergeField!(charter),
        alias: mergeField!(alias),
        is_sequential: mergeField!(is_sequential),
        plan_id: mergeField!(plan_id),
        external_occurrence_key: mergeField!(external_occurrence_key),
        native_occurrence: mergeField!(native_occurrence),
    };

    let conflict = (!fields.is_empty()).then_some(Conflict::FieldConflict(FieldConflict {
        id: ours.id,
        fields,
    }));
    (merged, conflict)
}

fn pick<T: PartialEq + Clone>(
    field: &str,
    base: &T,
    ours: &T,
    theirs: &T,
    conflicts: &mut Vec<String>,
) -> T {
    merge_field(base, ours, theirs).unwrap_or_else(|| {
        conflicts.push(field.to_string());
        ours.clone() // placeholder on conflict, like Git's %A
    })
}

fn merge_field<T: PartialEq + Clone>(base: &T, ours: &T, theirs: &T) -> Option<T> {
    if ours == theirs {
        Some(ours.clone())
    } else if ours == base {
        Some(theirs.clone())
    } else if theirs == base {
        Some(ours.clone())
    } else {
        None
    }
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

    if old.planned != new.planned {
        changes.push(FieldChange::DoDate {
            old: old.planned.map(|p| p.to_string()),
            new: new.planned.map(|p| p.to_string()),
        });
    }

    if old.completed_at != new.completed_at {
        changes.push(FieldChange::CompletedDate {
            old: old.completed_at.map(|d| d.to_string()),
            new: new.completed_at.map(|d| d.to_string()),
        });
    }

    // Compare contexts (as sets for robust comparison, but list equality for now)
    if old.contexts != new.contexts {
        changes.push(FieldChange::Generic {
            field: "contexts".to_string(),
            old: json!(old.contexts),
            new: json!(new.contexts),
        });
    }

    if old.charter != new.charter {
        changes.push(FieldChange::Generic {
            field: "charter".to_string(),
            old: json!(old.charter),
            new: json!(new.charter),
        });
    }

    changes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::ActionState;

    fn make_action(id: &str, name: &str) -> Action {
        Action {
            id: Uuid::parse_str(id).unwrap(),
            state: ActionState::NotStarted,
            name: name.to_string(),
            ..Default::default()
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
