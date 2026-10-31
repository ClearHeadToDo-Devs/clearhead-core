use chrono::Local;
use clearhead_core::workspace::resource::{ExpectedResource, WorkspacePath};
use clearhead_core::workspace::transaction::{
    FileState, NormalizedOperation, TransactionModel, apply_operations,
};
use clearhead_core::{Action, ActionState, VerbError, canonical_id};
use uuid::Uuid;

fn assert_cancel_rejects_terminal_active_target(state: ActionState) {
    let target = Action {
        id: Uuid::now_v7(),
        name: "Hand-marked terminal action".to_string(),
        state,
        ..Action::default()
    };
    let child = Action {
        parent_id: Some(target.id),
        ..Action::default()
    };
    let active = vec![target.clone(), child];
    let mut model = TransactionModel::new(vec![FileState::new(
        WorkspacePath::new("charters/work.actions").unwrap(),
        WorkspacePath::new("charters/work.completed.actions").unwrap(),
        active.clone(),
        vec![],
        ExpectedResource::Missing,
        ExpectedResource::Missing,
    )]);

    let result = apply_operations(
        &mut model,
        &[NormalizedOperation::Cancel { target: target.id }],
        Local::now(),
    );
    let (index, error) = result.expect_err("terminal active targets must not be cancelled");
    assert_eq!(index, 0);
    match error {
        VerbError::AlreadyClosed { id, state, query } => {
            assert_eq!(id, canonical_id(target.id));
            assert_eq!(state, format!("{:?}", target.state));
            assert_eq!(query, canonical_id(target.id));
        }
        other => panic!("expected AlreadyClosed, got {other:?}"),
    }
    let file = &model.files[0];
    assert_eq!(file.active, active);
    assert!(file.completed.is_empty());
    assert!(!file.active_dirty);
    assert!(!file.completed_dirty);
}

#[test]
fn cancel_rejects_completed_target_still_in_active_file() {
    assert_cancel_rejects_terminal_active_target(ActionState::Completed);
}

#[test]
fn cancel_rejects_cancelled_target_still_in_active_file() {
    assert_cancel_rejects_terminal_active_target(ActionState::Cancelled);
}
