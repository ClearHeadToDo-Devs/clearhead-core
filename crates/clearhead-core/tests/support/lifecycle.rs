use chrono::{DateTime, Local};
use clearhead_core::workspace::resource::{ExpectedResource, WorkspacePath};
use clearhead_core::workspace::transaction::{
    FileState, TransactionModel, apply_operations, expand_cascade,
};
use clearhead_core::{Action, ActionState};
use uuid::Uuid;

pub fn cascade(
    actions: &[Action],
    root: Uuid,
    state: ActionState,
    now: DateTime<Local>,
) -> Vec<Action> {
    let mut model = TransactionModel::new(vec![FileState::new(
        WorkspacePath::new("charters/work.actions").unwrap(),
        WorkspacePath::new("charters/work.completed.actions").unwrap(),
        actions.to_vec(),
        vec![],
        ExpectedResource::Missing,
        ExpectedResource::Missing,
    )]);
    let operations = expand_cascade(&model, root, state).unwrap();
    apply_operations(&mut model, &operations, now).unwrap();
    model.files.remove(0).active
}
