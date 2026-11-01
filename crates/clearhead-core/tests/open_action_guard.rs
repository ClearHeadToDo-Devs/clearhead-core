use clearhead_core::{Action, ActionState, VerbError};
use uuid::Uuid;

#[test]
fn open_action_guard_accepts_every_live_state() {
    for state in [
        ActionState::NotStarted,
        ActionState::InProgress,
        ActionState::BlockedOrAwaiting,
    ] {
        let action = Action {
            state,
            ..Action::default()
        };
        assert_eq!(VerbError::require_open_action(&action, "selector"), Ok(()));
    }
}

#[test]
fn open_action_guard_reports_terminal_identity_state_and_original_selector() {
    for state in [ActionState::Completed, ActionState::Cancelled] {
        let action = Action {
            id: Uuid::from_u128(1),
            state,
            ..Action::default()
        };
        assert_eq!(
            VerbError::require_open_action(&action, "original selector"),
            Err(VerbError::AlreadyClosed {
                id: "urn:uuid:00000000-0000-0000-0000-000000000001".into(),
                state: format!("{state:?}"),
                query: "original selector".into(),
            })
        );
    }
}
