// Semantic Merge Tests, with the goal being to use this within our custom git driver

use clearhead_core::ActionList;
use clearhead_core::workspace::actions::diff::{ActionDeleted, Conflict, Side, semantic_merge};
use clearhead_core::workspace::actions::{Action, ActionState};
use uuid::Uuid;

fn base_action() -> ActionList {
    vec![Action {
        id: Uuid::new_v4(),
        name: "Task".to_string(),
        state: ActionState::NotStarted,
        ..Default::default()
    }]
}

// Test that semantic_merge returns an empty diff when there are no changes between base, ours, and theirs
#[test]
fn test_semantic_merge_no_op() {
    let base = base_action();
    let ours = base.clone();
    let theirs = base.clone();

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.conflicts.len(), 0);
    assert_eq!(merged.merged, base);
}

// Test semantic merge can handle a single edit from ours and none from theirs
#[test]
fn test_semantic_merge_single_edit() {
    let base = base_action();
    let ours = base.clone();
    let mut theirs = base.clone();
    theirs[0].name = "Updated Task".to_string();

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.conflicts.len(), 0);
    assert_eq!(merged.merged[0].name, "Updated Task");
}

// Test that when one action is deleted in ours but not in theirs, the merged result should resolve
// without conficts
#[test]
fn test_semantic_merge_delete_action() {
    let base = base_action();
    let theirs = base.clone();
    let ours = vec![]; // Ours deletes the action

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(ours, merged.merged);
    assert!(merged.conflicts.is_empty());
}

// Ours deletes A while theirs adds C: the deletion must not stop the loop before C.
#[test]
fn test_semantic_merge_delete_keeps_later_actions() {
    let base = base_action();
    let added = Action {
        id: Uuid::new_v4(),
        name: "C".to_string(),
        ..base[0].clone()
    };
    let ours = vec![]; // Ours deletes A
    let theirs = vec![base[0].clone(), added.clone()]; // Theirs keeps A untouched, adds C

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, vec![added]);
    assert!(
        merged.conflicts.is_empty(),
        "unexpected conflicts: {:?}",
        merged.conflicts
    );
}

// The mirror: theirs deletes an action ours didn't touch, so the deletion wins.
#[test]
fn test_semantic_merge_theirs_deletes_untouched() {
    let base = base_action();
    let ours = base.clone(); // Ours does not edit the action
    let theirs = vec![]; // Theirs deletes the action

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, vec![]);
    assert!(
        merged.conflicts.is_empty(),
        "unexpected conflicts: {:?}",
        merged.conflicts
    );
}

// Edited on one side, deleted on the other: a conflict, with the edit kept as the placeholder.
#[test]
fn test_semantic_merge_edit_and_delete() {
    let base = base_action();
    let mut ours = base.clone();
    ours[0].name = "Updated Task".to_string(); // Ours edits the action
    let theirs = vec![]; // Theirs deletes the action

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, ours);
    assert_eq!(
        merged.conflicts,
        vec![Conflict::ActionDeleted(ActionDeleted {
            id: base[0].id,
            deleted_by: Side::Theirs,
        })]
    );
}
