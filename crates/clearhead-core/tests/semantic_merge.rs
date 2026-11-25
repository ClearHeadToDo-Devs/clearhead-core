// Semantic Merge Tests, with the goal being to use this within our custom git driver

use clearhead_core::ActionList;
use clearhead_core::workspace::actions::diff::{
    ActionDeleted, BothAdded, Conflict, FieldConflict, ParentCycle, Side, semantic_merge,
};
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

// Both sides change the same field: a conflict, with ours kept as the placeholder (git's %A).
#[test]
fn test_semantic_merge_same_field_conflict() {
    let base = base_action();
    let mut ours = base.clone();
    ours[0].name = "Ours".to_string();
    let mut theirs = base.clone();
    theirs[0].name = "Theirs".to_string();

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, ours);
    assert_eq!(
        merged.conflicts,
        vec![Conflict::FieldConflict(FieldConflict {
            id: base[0].id,
            fields: vec!["name".to_string()],
        })]
    );
}

// Both sides add the same action, identically: one copy, no conflict.
#[test]
fn test_semantic_merge_both_added_same() {
    let base = vec![];
    let added = base_action();

    let merged = semantic_merge(&base, &added, &added);

    assert_eq!(merged.merged, added);
    assert!(merged.conflicts.is_empty());
}

// Both sides add the same UUID with different content: a conflict, ours kept as the placeholder.
#[test]
fn test_semantic_merge_both_added_different() {
    let base = vec![];
    let ours = base_action();
    let mut theirs = ours.clone();
    theirs[0].name = "Theirs".to_string();

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, ours);
    assert_eq!(
        merged.conflicts,
        vec![Conflict::BothAdded(BothAdded { id: ours[0].id })]
    );
}

// Theirs adds a child under the first of two actions. The list is written in order with depth
// markers, so the child must come right after its parent, not at the end under the wrong one.
#[test]
fn test_semantic_merge_added_child_follows_parent() {
    let a = base_action().remove(0);
    let b = Action {
        id: Uuid::new_v4(),
        name: "B".to_string(),
        ..a.clone()
    };
    let child = Action {
        id: Uuid::new_v4(),
        name: "Child of A".to_string(),
        parent_id: Some(a.id),
        ..a.clone()
    };
    let base = vec![a.clone(), b.clone()];
    let ours = base.clone();
    let theirs = vec![a.clone(), child.clone(), b.clone()];

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, vec![a, child, b]);
    assert!(merged.conflicts.is_empty());
}

fn named(name: &str, parent_id: Option<Uuid>) -> Action {
    Action {
        id: Uuid::new_v4(),
        name: name.to_string(),
        parent_id,
        ..Default::default()
    }
}

// Ours deletes A, which theirs left alone, while theirs adds a child under A. Adding the child
// edits A, so this is edit versus delete: A is kept with its child, and the deletion reported.
#[test]
fn test_semantic_merge_child_of_deleted_parent_conflicts() {
    let a = named("A", None);
    let child = named("Child of A", Some(a.id));
    let base = vec![a.clone()];
    let ours = vec![];
    let theirs = vec![a.clone(), child];

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, theirs);
    assert_eq!(
        merged.conflicts,
        vec![Conflict::ActionDeleted(ActionDeleted {
            id: a.id,
            deleted_by: Side::Ours,
        })]
    );
}

// Ours deletes G and its child A; theirs adds a grandchild under A. Both ancestors come back.
#[test]
fn test_semantic_merge_restores_deleted_ancestors() {
    let g = named("G", None);
    let a = named("A", Some(g.id));
    let grandchild = named("Grandchild", Some(a.id));
    let base = vec![g.clone(), a.clone()];
    let ours = vec![];
    let theirs = vec![g.clone(), a.clone(), grandchild];

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, theirs);
    assert_eq!(
        merged.conflicts,
        vec![
            Conflict::ActionDeleted(ActionDeleted {
                id: a.id,
                deleted_by: Side::Ours,
            }),
            Conflict::ActionDeleted(ActionDeleted {
                id: g.id,
                deleted_by: Side::Ours,
            }),
        ]
    );
}

// Ours moves A under B, theirs moves B under A. Each move merges cleanly, together they are a
// cycle: a conflict, with ours' tree as the placeholder and theirs' move of B named as undone.
#[test]
fn test_semantic_merge_parent_cycle_conflicts() {
    let a = named("A", None);
    let b = named("B", None);
    let base = vec![a.clone(), b.clone()];
    let a_under_b = Action {
        parent_id: Some(b.id),
        ..a.clone()
    };
    let b_under_a = Action {
        parent_id: Some(a.id),
        ..b.clone()
    };
    let ours = vec![b.clone(), a_under_b];
    let theirs = vec![a, b_under_a];

    let merged = semantic_merge(&base, &ours, &theirs);

    assert_eq!(merged.merged, ours);
    assert_eq!(
        merged.conflicts,
        vec![Conflict::ParentCycle(ParentCycle { ids: vec![b.id] })]
    );
}
