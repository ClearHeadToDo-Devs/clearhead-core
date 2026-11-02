//! Both delivery paths close one line and preserve all descendant facts.
mod common;

use chrono::{Local, TimeZone};
use clearhead_cli::filesystem::read_actions;
use clearhead_core::domain::time::Bound;
use clearhead_core::{Action, ActionState, OutputFormat, completed_actions_path, format};
use common::TestEnv;
use uuid::Uuid;

fn close_mixed_history(transaction: bool, closing_state: ActionState) {
    let env = TestEnv::new();
    let states = [
        ActionState::NotStarted,
        ActionState::InProgress,
        ActionState::Completed,
        ActionState::Cancelled,
        ActionState::Completed,
        ActionState::Cancelled,
        ActionState::NotStarted,
        ActionState::BlockedOrAwaiting,
        ActionState::InProgress,
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
    let original: Vec<_> = states
        .into_iter()
        .enumerate()
        .map(|(i, state)| Action {
            id: Uuid::from_u128(0x019f733d460070008000000000000001 + i as u128),
            name: format!("Action {i}"),
            state,
            parent_id: parents[i].map(|p| Uuid::from_u128(0x019f733d460070008000000000000001 + p)),
            completed_at: match i {
                2 => Some(Bound::minute(
                    Local.with_ymd_and_hms(2026, 10, 1, 10, 0, 0).unwrap(),
                )),
                3 => Some(Bound::minute(
                    Local.with_ymd_and_hms(2026, 10, 2, 10, 0, 0).unwrap(),
                )),
                _ => None,
            },
            ..Action::default()
        })
        .collect();
    // Plaintext indentation requires depth-first order: keep each grandchild
    // immediately beneath its own terminal parent before the next sibling.
    let original: Vec<_> = [0, 1, 2, 6, 3, 7, 4, 8, 5, 9, 10]
        .into_iter()
        .map(|i| original[i].clone())
        .collect();
    env.write_actions(
        "work.actions",
        &format(&original, OutputFormat::Actions, None, None).unwrap(),
    );
    let source = env.data_dir.join("charters/work.actions");
    let history_path = completed_actions_path(&source);
    let existing = Action {
        state: ActionState::Completed,
        ..Action::new("Existing history")
    };
    std::fs::write(
        &history_path,
        format(&vec![existing.clone()], OutputFormat::Actions, None, None).unwrap(),
    )
    .unwrap();

    let verb = if closing_state == ActionState::Cancelled {
        "cancel"
    } else {
        "complete"
    };
    if transaction {
        env.command()
            .arg("transact")
            .write_stdin(format!(
                r#"{{"operations":[{{"op":"{verb}-action","target":"urn:uuid:{}"}}]}}"#,
                original[1].id
            ))
            .assert()
            .success();
    } else {
        env.command()
            .args([verb, "action", &original[1].id.to_string(), "--file"])
            .arg(&source)
            .assert()
            .success();
    }

    let after = read_actions(&source).unwrap();
    assert_eq!(after.len(), original.len());
    assert_eq!(read_actions(&history_path).unwrap(), vec![existing]);
    let closed_at = after[1].completed_at;
    assert!(closed_at.is_some());
    for (actual, source) in after.iter().zip(&original) {
        let mut expected = source.clone();
        if source.id == original[1].id {
            expected.state = closing_state;
            expected.completed_at = closed_at;
        }
        assert_eq!(actual, &expected);
    }
}

#[test]
fn ordinary_cancel_preserves_closed_child_history() {
    close_mixed_history(false, ActionState::Cancelled);
}

#[test]
fn transaction_cancel_preserves_closed_child_history() {
    close_mixed_history(true, ActionState::Cancelled);
}

#[test]
fn ordinary_complete_preserves_closed_child_history() {
    close_mixed_history(false, ActionState::Completed);
}

#[test]
fn transaction_complete_preserves_closed_child_history() {
    close_mixed_history(true, ActionState::Completed);
}
