use chrono::{Local, TimeZone};
use clearhead_core::domain::time::Bound;
use clearhead_core::{Action, ActionState, close_subtree};
use uuid::Uuid;

#[test]
fn cancellation_preserves_terminal_history_and_traverses_terminal_nodes() {
    let old = Local.with_ymd_and_hms(2026, 10, 1, 10, 0, 0).unwrap();
    let now = Local.with_ymd_and_hms(2026, 10, 31, 12, 0, 0).unwrap();
    // External parent, selected root, terminal branches (dated and undated),
    // open grandchildren below each terminal branch, and an unrelated sibling.
    let states = [
        ActionState::NotStarted,
        ActionState::InProgress,
        ActionState::Completed,
        ActionState::Cancelled,
        ActionState::Completed,
        ActionState::Cancelled,
        ActionState::NotStarted,
        ActionState::InProgress,
        ActionState::BlockedOrAwaiting,
        ActionState::NotStarted,
        ActionState::NotStarted,
    ];
    let parents = [
        None,
        Some(0),
        Some(1),
        Some(1),
        Some(1),
        Some(1),
        Some(2),
        Some(3),
        Some(4),
        Some(5),
        Some(0),
    ];
    let actions: Vec<_> = states
        .into_iter()
        .enumerate()
        .map(|(i, state)| Action {
            id: Uuid::from_u128(i as u128 + 1),
            name: format!("Action {i}"),
            state,
            parent_id: parents[i].map(|p| Uuid::from_u128(p + 1)),
            completed_at: matches!(i, 2 | 3).then_some(Bound::minute(old)),
            ..Action::default()
        })
        .collect();
    let original = actions.clone();
    let closed = close_subtree(&actions, actions[1].id, ActionState::Cancelled, now);
    assert_eq!(
        actions, original,
        "source must remain immutable, including unrelated actions"
    );
    assert_eq!(closed.len(), 9);
    for (actual, source) in closed.iter().zip(&original[1..10]) {
        let mut expected = source.clone();
        if !matches!(
            source.state,
            ActionState::Completed | ActionState::Cancelled
        ) {
            expected.state = ActionState::Cancelled;
            expected.completed_at = Some(Bound::minute(now));
        }
        if source.id == actions[1].id {
            expected.parent_id = None;
        }
        assert_eq!(actual, &expected);
    }
}
