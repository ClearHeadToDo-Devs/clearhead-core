//! Preparation contracts migrated from the retired subtree/archive wrappers.
#![cfg(feature = "formatting")]
use chrono::Local;
use clearhead_core::workspace::action_lifecycle::{MoveMode, prepare_action_move};
use clearhead_core::workspace::mutate_actions::{ActionResourceState, SidecarResourceState};
use clearhead_core::workspace::resource::{Effect, EffectBatch, ExpectedResource, WorkspacePath};
use clearhead_core::workspace::transaction::{
    FileState, NormalizedOperation, PreparedTransactionOutcome, TransactionModel,
    prepare_transaction,
};
use clearhead_core::{Action, ActionState, plan_action_archive};
use uuid::Uuid;

fn action(name: &str, state: ActionState, parent: Option<Uuid>) -> Action {
    Action {
        state,
        parent_id: parent,
        ..Action::new(name)
    }
}
fn resource(name: &str, actions: Vec<Action>) -> ActionResourceState {
    ActionResourceState {
        path: WorkspacePath::new(format!("charters/{name}")).unwrap(),
        actions,
        expected: ExpectedResource::Missing,
    }
}
fn sidecar(name: &str) -> SidecarResourceState {
    SidecarResourceState {
        path: WorkspacePath::new(format!("charters/.{name}.json")).unwrap(),
        metadata: Default::default(),
        expected: ExpectedResource::Missing,
    }
}
fn transaction(
    active: Vec<Action>,
    completed: Vec<Action>,
    op: NormalizedOperation,
) -> (EffectBatch, PreparedTransactionOutcome) {
    prepare_transaction(
        TransactionModel::new(vec![FileState::new(
            WorkspacePath::new("charters/work.actions").unwrap(),
            WorkspacePath::new("charters/work.completed.actions").unwrap(),
            active,
            completed,
            ExpectedResource::Missing,
            ExpectedResource::Missing,
        )]),
        &[op],
        Local::now(),
        false,
    )
    .unwrap()
}
fn written(batch: &EffectBatch, path: &str) -> Vec<Action> {
    let bytes = batch
        .effects()
        .iter()
        .find_map(|effect| match effect {
            Effect::Write {
                path: location,
                bytes,
            } if location.path.as_str() == path => Some(bytes),
            _ => None,
        })
        .expect("expected a write");
    clearhead_core::parse_actions(std::str::from_utf8(bytes).unwrap()).unwrap()
}

#[test]
fn plan_archives_complete_terminal_trees_and_preserves_existing_history() {
    let root = action("Root", ActionState::Completed, None);
    let child = action("Child", ActionState::Cancelled, Some(root.id));
    let open = action("Open", ActionState::NotStarted, None);
    let old = action("Old", ActionState::Completed, None);
    let plan = plan_action_archive(
        &[root.clone(), child.clone(), open.clone()],
        std::slice::from_ref(&old),
        Local::now(),
    );
    assert_eq!(plan.archived_count, 2);
    assert_eq!(plan.active_actions, vec![open]);
    assert_eq!(plan.completed_actions, vec![old, root, child]);
}
#[test]
fn plan_keeps_terminal_root_when_a_descendant_is_open() {
    let root = action("Root", ActionState::Completed, None);
    let child = action("Child", ActionState::NotStarted, Some(root.id));
    let original = vec![root, child];
    let plan = plan_action_archive(&original, &[], Local::now());
    assert_eq!(plan.archived_count, 0);
    assert_eq!(plan.active_actions, original);
}
#[test]
fn close_preparation_writes_only_active_and_preserves_descendants() {
    let root = action("Root", ActionState::NotStarted, None);
    let child = action("Child", ActionState::NotStarted, Some(root.id));
    let (batch, outcome) = transaction(
        vec![root.clone(), child.clone()],
        vec![],
        NormalizedOperation::Complete { target: root.id },
    );
    assert!(
        matches!(outcome, PreparedTransactionOutcome::Committed { operations, .. } if operations.len() == 1)
    );
    assert_eq!(batch.effects().len(), 1);
    assert_eq!(batch.preconditions().len(), 2);
    let after = written(&batch, "charters/work.actions");
    assert_eq!(after.len(), 2);
    assert_eq!(after[0].state, ActionState::Completed);
    assert!(after[0].completed_at.is_some());
    assert_eq!(after[1], child);
}
#[test]
fn archive_emits_completed_write_before_active_write() {
    let root = action("Root", ActionState::Completed, None);
    let (batch, _) = prepare_action_move(
        resource("work.actions", vec![root.clone()]),
        resource("work.completed.actions", vec![]),
        sidecar("work"),
        sidecar("work.completed"),
        None,
        MoveMode::default(),
    )
    .unwrap();
    let paths: Vec<_> = batch
        .effects()
        .iter()
        .filter_map(|e| match e {
            Effect::Write { path, .. } => Some(path.path.as_str()),
            _ => None,
        })
        .collect();
    let destination = paths
        .iter()
        .position(|p| *p == "charters/work.completed.actions")
        .unwrap();
    let source = paths
        .iter()
        .position(|p| *p == "charters/work.actions")
        .unwrap();
    assert!(destination < source);
    assert_eq!(
        written(&batch, "charters/work.completed.actions"),
        vec![root]
    );
    assert!(written(&batch, "charters/work.actions").is_empty());
}
#[test]
fn reopen_writes_only_active_clears_target_date_and_preserves_children() {
    let mut root = action("Root", ActionState::Completed, None);
    root.completed_at = Some(clearhead_core::domain::time::Bound::minute(Local::now()));
    let child = action("Child", ActionState::Cancelled, Some(root.id));
    let (batch, outcome) = transaction(
        vec![root.clone(), child.clone()],
        vec![],
        NormalizedOperation::Reopen { target: root.id },
    );
    assert!(
        matches!(outcome, PreparedTransactionOutcome::Committed { operations, .. } if operations.len() == 1)
    );
    assert_eq!(batch.effects().len(), 1);
    assert_eq!(batch.preconditions().len(), 2);
    let after = written(&batch, "charters/work.actions");
    assert_eq!(after[0].state, ActionState::NotStarted);
    assert!(after[0].completed_at.is_none());
    assert_eq!(after[1], child);
}
#[test]
fn no_op_archive_retains_read_set_preconditions_without_effects() {
    let (batch, outcome) = prepare_action_move(
        resource(
            "work.actions",
            vec![action("Open", ActionState::NotStarted, None)],
        ),
        resource("work.completed.actions", vec![]),
        sidecar("work"),
        sidecar("work.completed"),
        None,
        MoveMode::default(),
    )
    .unwrap();
    assert_eq!(outcome.archived_count, 0);
    assert!(batch.effects().is_empty());
    assert_eq!(batch.preconditions().len(), 4);
}
#[test]
fn reopen_refuses_archived_subtree_and_names_root() {
    let root = action("Root", ActionState::Completed, None);
    let child = action("Child", ActionState::Cancelled, Some(root.id));
    let (batch, outcome) = transaction(
        vec![],
        vec![root.clone(), child.clone()],
        NormalizedOperation::Reopen { target: child.id },
    );
    let PreparedTransactionOutcome::Rejected { error, .. } = outcome else {
        panic!("expected rejection")
    };
    assert!(batch.effects().is_empty());
    assert!(error.to_string().contains("unarchive"));
    assert!(error.to_string().contains(&root.id.to_string()));
}
#[test]
fn reopen_of_an_already_open_action_is_rejected() {
    let root = action("Root", ActionState::NotStarted, None);
    let (batch, outcome) = transaction(
        vec![root.clone()],
        vec![],
        NormalizedOperation::Reopen { target: root.id },
    );
    let PreparedTransactionOutcome::Rejected { error, .. } = outcome else {
        panic!("expected rejection")
    };
    assert!(batch.effects().is_empty());
    assert!(error.to_string().contains("already open"));
}
