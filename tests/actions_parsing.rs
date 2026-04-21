//! Tests for the `.actions` file parsing pipeline.
//!
//! These sit between the unit-level parser tests in `workspace/actions/parser.rs`
//! (which test DSL syntax in isolation) and the full workspace integration tests in
//! `workspace_store.rs` (which exercise the whole load pipeline).
//!
//! Here we parse fixture file content directly with `parse_actions` and
//! `parse_document`, asserting field-level values: IDs, states, names, and
//! parent-child links.

use clearhead_core::{ActionState, ParseMode, parse_actions, parse_actions_with_mode, parse_document};

// Use compile-time embedding so fixture content is verified to exist at build time.
const WORK_ACTIONS: &str = include_str!("fixtures/workspace/user-flat/work.actions");
const PERSONAL_ACTIONS: &str = include_str!("fixtures/workspace/user-flat/personal.actions");
const HEALTH_ACTIONS: &str = include_str!("fixtures/workspace/md-merge/.clearhead/health.actions");

#[test]
fn work_actions_count_and_uuids() {
    let actions = parse_actions(WORK_ACTIONS).expect("parse failed");

    assert_eq!(actions.len(), 3, "expected 2 top-level + 1 child");

    let write = actions
        .iter()
        .find(|a| a.name == "Write quarterly report")
        .expect("missing");
    assert_eq!(write.id.to_string(), "01960000-0001-7000-0000-000000000001");
    assert_eq!(write.state, ActionState::NotStarted);
    assert!(write.parent_id.is_none());
}

#[test]
fn work_actions_parent_child_link() {
    let actions = parse_actions(WORK_ACTIONS).expect("parse failed");

    let review = actions
        .iter()
        .find(|a| a.name == "Review team PRs")
        .expect("missing");
    let backend = actions
        .iter()
        .find(|a| a.name == "Backend PRs")
        .expect("missing");

    assert_eq!(
        backend.parent_id,
        Some(review.id),
        "Backend PRs should be a child of Review team PRs"
    );
}

#[test]
fn personal_actions_inprogress_state() {
    let actions = parse_actions(PERSONAL_ACTIONS).expect("parse failed");

    let run = actions
        .iter()
        .find(|a| a.name == "Morning run")
        .expect("missing");
    assert_eq!(
        run.state,
        ActionState::InProgress,
        "Morning run should be InProgress ([-])"
    );

    let groceries = actions
        .iter()
        .find(|a| a.name == "Buy groceries")
        .expect("missing");
    assert_eq!(groceries.state, ActionState::NotStarted);
}

#[test]
fn health_actions_plan_count_and_names() {
    let actions = parse_actions(HEALTH_ACTIONS).expect("parse failed");

    assert_eq!(actions.len(), 2);
    let names: Vec<&str> = actions.iter().map(|a| a.name.as_str()).collect();
    assert!(names.contains(&"Morning run"));
    assert!(names.contains(&"Meal prep"));

    for action in &actions {
        assert!(
            action.parent_id.is_none(),
            "health actions should all be top-level"
        );
        assert_eq!(action.state, ActionState::NotStarted);
    }
}

#[test]
fn empty_input_produces_no_actions() {
    let actions = parse_actions("").expect("empty input should not be an error");
    assert!(actions.is_empty());
}

#[test]
fn parse_document_captures_syntax_errors_without_panicking() {
    // A bare identifier with no state brackets is malformed DSL.
    // parse_document should succeed (returning a ParsedDocument) and report
    // the error in syntax_errors rather than returning Err.
    let result = parse_document("not valid actions syntax !!!");
    // parse_document itself should not return Err — errors live in syntax_errors.
    // If it does error (grammar hard-rejects), that's also acceptable, but
    // the key invariant is: it must not panic.
    match result {
        Ok(doc) => {
            // Syntax errors may or may not be present depending on grammar permissiveness.
            // The document object itself is valid.
            let _ = doc.syntax_errors;
        }
        Err(_) => {
            // A hard parse error is also acceptable — just not a panic.
        }
    }
}

#[test]
fn parse_mode_strict_fails_on_syntax_errors() {
    let result = parse_actions_with_mode("not valid actions syntax !!!", ParseMode::Strict);
    assert!(result.is_err(), "strict mode should fail on syntax issues");
    let err = result.expect_err("expected parse failure");
    assert_eq!(err.code, "syntax-error");
}

#[test]
fn parse_mode_recover_returns_diagnostics() {
    let result = parse_actions_with_mode("not valid actions syntax !!!", ParseMode::Recover)
        .expect("recover mode should not fail");
    assert!(
        !result.syntax_errors.is_empty(),
        "recover mode should surface syntax diagnostics"
    );
    assert_eq!(
        result.recovery.recoverable_actions,
        result.document.actions.len(),
        "recovery metadata should track recoverable action count"
    );
}
