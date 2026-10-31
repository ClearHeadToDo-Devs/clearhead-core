//! Both delivery paths must archive the same factual mixed-state subtree.
mod common;

use chrono::{Local, TimeZone};
use clearhead_cli::filesystem::read_actions;
use clearhead_core::domain::time::Bound;
use clearhead_core::{Action, ActionState, OutputFormat, completed_actions_path, format};
use common::TestEnv;
use std::collections::HashSet;
use uuid::Uuid;

fn cancel_mixed_history(transaction: bool) {
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

    if transaction {
        env.command()
            .arg("transact")
            .write_stdin(format!(
                r#"{{"operations":[{{"op":"cancel-action","target":"urn:uuid:{}"}}]}}"#,
                original[1].id
            ))
            .assert()
            .success();
    } else {
        env.command()
            .args(["cancel", "action", &original[1].id.to_string(), "--file"])
            .arg(&source)
            .assert()
            .success();
    }

    assert_eq!(
        read_actions(&source).unwrap(),
        vec![original[0].clone(), original[10].clone()]
    );
    let history = read_actions(&history_path).unwrap();
    assert_eq!(
        history.len(),
        10,
        "existing history plus every subtree member exactly once"
    );
    let ids: HashSet<_> = history.iter().map(|a| a.id).collect();
    let expected_ids: HashSet<_> = std::iter::once(existing.id)
        .chain(original[1..10].iter().map(|a| a.id))
        .collect();
    assert_eq!(ids, expected_ids);
    assert_eq!(
        history.iter().find(|a| a.id == existing.id).unwrap(),
        &existing
    );
    let cancelled_at = history
        .iter()
        .find(|a| a.id == original[1].id)
        .unwrap()
        .completed_at;
    assert!(cancelled_at.is_some());
    for source in &original[1..10] {
        let actual = history.iter().find(|a| a.id == source.id).unwrap();
        let mut expected = source.clone();
        if !matches!(
            source.state,
            ActionState::Completed | ActionState::Cancelled
        ) {
            expected.state = ActionState::Cancelled;
            expected.completed_at = cancelled_at;
        }
        if source.id == original[1].id {
            expected.parent_id = None;
        }
        assert_eq!(actual, &expected);
    }
}

#[test]
fn ordinary_cancel_preserves_closed_child_history() {
    cancel_mixed_history(false);
}

#[test]
fn transaction_cancel_preserves_closed_child_history() {
    cancel_mixed_history(true);
}
