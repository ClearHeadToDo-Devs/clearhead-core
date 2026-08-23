//! Opt-in conformance against the inert oracle data in `specifications`.
//!
//! The specification owns source fixtures and the canonical JSON Schema. Core,
//! as one peer implementation, proves its semantic projection, formatter, and
//! linter against those artifacts. A default `cargo test` remains independent;
//! this suite runs only with `--features spec-conformance`.
#![cfg(feature = "spec-conformance")]

use clearhead_core::workspace::actions::{LintDiagnostic, lint_document};
use clearhead_core::{
    ActionState, OutputFormat, SourceRange, format, parse_actions, parse_document,
};
use std::path::{Path, PathBuf};

fn spec_dir() -> PathBuf {
    std::env::var("CLEARHEAD_SPEC_DIR")
        .map(PathBuf::from)
        // From crates/clearhead-core up to the platform root, then into the
        // specifications submodule (the standalone fallback when the env var,
        // which validate-pinned relies on, is unset).
        .unwrap_or_else(|_| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../specifications")
        })
}

fn fixture(relative: &str) -> String {
    let path = spec_dir().join("examples/conformance").join(relative);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("failed to read specification fixture {path:?}: {error}"))
}

fn collect_actions_fixtures(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_actions_fixtures(&path, out);
        } else if path.extension().and_then(|extension| extension.to_str()) == Some("actions") {
            out.push(path);
        }
    }
}

fn semantic_fixtures() -> Vec<PathBuf> {
    let root = spec_dir().join("examples/conformance");
    let mut fixtures = Vec::new();
    for family in ["parse", "diagnostics", "archive"] {
        collect_actions_fixtures(&root.join(family), &mut fixtures);
    }
    fixtures.sort();
    assert!(
        !fixtures.is_empty(),
        "no semantic conformance fixtures found under {root:?}"
    );
    fixtures
}

fn diagnostics(relative: &str) -> Vec<LintDiagnostic> {
    lint_document(&parse_document(&fixture(relative)).expect("fixture should parse"))
        .into_iter()
        .collect()
}

#[test]
fn core_projection_conforms_to_spec_schema() {
    let schema_path = spec_dir().join("schemas/actions.schema.json");
    assert!(
        schema_path.exists(),
        "spec schema not found at {schema_path:?}; set CLEARHEAD_SPEC_DIR to a specifications checkout"
    );
    let schema: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&schema_path).unwrap()).unwrap();
    let validator = jsonschema::JSONSchema::compile(&schema).expect("spec schema compiles");

    for path in semantic_fixtures() {
        let source = std::fs::read_to_string(&path).unwrap();
        let actions = clearhead_core::parse_actions(&source)
            .unwrap_or_else(|error| panic!("Core failed to parse {path:?}: {error}"));
        let value =
            serde_json::to_value(clearhead_core::schema_export::to_schema_document(&actions))
                .unwrap();

        if let Err(errors) = validator.validate(&value) {
            let detail: Vec<String> = errors
                .map(|error| format!("  - {} (at {})", error, error.instance_path))
                .collect();
            panic!(
                "Core's projection of {path:?} violates the spec schema:\n{}",
                detail.join("\n")
            );
        }
    }
}

#[test]
fn every_field_fixture_has_the_specified_domain_meaning() {
    let actions = parse_actions(&fixture("parse/every_field.actions")).unwrap();
    assert_eq!(actions.len(), 1);
    let action = &actions[0];

    assert_eq!(action.state, ActionState::NotStarted);
    assert_eq!(action.name, "Root action with every field");
    assert_eq!(
        action.description.as_deref(),
        Some("This action is used to verify that all metadata is correctly parsed and displayed.")
    );
    assert_eq!(action.priority, Some(1));
    assert_eq!(
        action.contexts.as_deref(),
        Some(["testing".to_string(), "metadata".to_string()].as_slice())
    );
    assert_eq!(action.charter.as_deref(), Some("Conformance-Project"));
    assert_eq!(
        action
            .scheduled_at
            .unwrap()
            .format("%Y-%m-%dT%H:%M:%S")
            .to_string(),
        "2026-01-01T09:00:00"
    );
    assert_eq!(action.duration, Some(60));
    assert_eq!(
        action
            .created_at
            .unwrap()
            .format("%Y-%m-%dT%H:%M:%S")
            .to_string(),
        "2026-01-03T12:00:00"
    );
    assert_eq!(
        action.id.to_string(),
        "01942db4-0000-7000-8000-000000000001"
    );
    assert!(diagnostics("parse/every_field.actions").is_empty());
}

#[test]
fn semantic_edge_fixture_covers_decoded_text_hierarchy_and_references() {
    let actions = parse_actions(&fixture("parse/semantic_edges.actions")).unwrap();
    assert_eq!(actions.len(), 2);
    let root = &actions[0];
    let child = &actions[1];

    assert_eq!(root.name, "Escaped $ ! * + @ % # > title");
    assert_eq!(
        root.description.as_deref(),
        Some("Description with ! * + @ % # > and [[docs|https://example.com]]")
    );
    assert_eq!(
        root.contexts.as_deref(),
        Some(["ctx".to_string(), "second".to_string()].as_slice())
    );
    assert_eq!(root.charter.as_deref(), Some("Charter!"));
    assert_eq!(root.alias.as_deref(), Some("root-edge"));
    assert_eq!(root.is_sequential, Some(true));
    assert!(root.due_date.is_some());

    assert_eq!(child.parent_id, Some(root.id));
    assert_eq!(child.alias.as_deref(), Some("child-edge"));
    assert_eq!(child.predecessors.as_ref().unwrap()[0].raw_ref, "root-edge");
}

#[test]
fn uuid_v7_fixture_preserves_identity_without_a_creation_date_diagnostic() {
    let actions = parse_actions(&fixture("parse/uuid_v7_derivation.actions")).unwrap();
    assert_eq!(actions.len(), 1);
    assert_eq!(
        actions[0].id.to_string(),
        "01942db4-ec68-7000-8000-000000000008"
    );
    assert!(
        diagnostics("parse/uuid_v7_derivation.actions")
            .iter()
            .all(|diagnostic| diagnostic.code != "W004")
    );
}

#[test]
fn diagnostic_fixtures_pin_code_and_source_span() {
    let cases = [
        (
            "diagnostics/inconsistent_tree.actions",
            "W002",
            SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 5,
                end_col: 77,
            },
        ),
        (
            "diagnostics/completed_subtasks.actions",
            "W003",
            SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 4,
                end_col: 87,
            },
        ),
    ];

    for (relative, code, range) in cases {
        let matching: Vec<_> = diagnostics(relative)
            .into_iter()
            .filter(|diagnostic| diagnostic.code == code)
            .collect();
        assert_eq!(matching.len(), 1, "{relative} must emit exactly one {code}");
        assert_eq!(matching[0].range, range, "{relative} diagnostic moved");
    }
}

#[test]
fn completed_tree_fixture_is_archive_ready() {
    let relative = "archive/completed_tree.actions";
    let actions = parse_actions(&fixture(relative)).unwrap();
    assert_eq!(actions.len(), 2);
    assert!(actions.iter().all(|action| matches!(
        action.state,
        ActionState::Completed | ActionState::Cancelled
    )));
    assert!(diagnostics(relative).is_empty());
}

fn schema_validator(relative: &str) -> jsonschema::JSONSchema {
    let path = spec_dir().join(relative);
    assert!(
        path.exists(),
        "spec schema not found at {path:?}; set CLEARHEAD_SPEC_DIR to a specifications checkout"
    );
    let schema: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
    jsonschema::JSONSchema::compile(&schema).expect("spec schema compiles")
}

fn transaction_fixture(relative: &str) -> serde_json::Value {
    let path = spec_dir().join("examples/transactions").join(relative);
    serde_json::from_str(&std::fs::read_to_string(&path).unwrap())
        .unwrap_or_else(|error| panic!("failed to read transaction fixture {path:?}: {error}"))
}

#[test]
fn transaction_request_fixtures_match_schema_and_parse_in_core() {
    let validator = schema_validator("schemas/transaction_request.schema.json");

    // The valid fixture satisfies the schema and round-trips through core's
    // request types and its state-free normalization.
    let valid = transaction_fixture("request/valid_multi_op.json");
    assert!(
        validator.validate(&valid).is_ok(),
        "valid_multi_op must satisfy the request schema"
    );
    let request: clearhead_core::TransactionRequest = serde_json::from_value(valid).unwrap();
    clearhead_core::workspace::normalize_request(request).expect("valid request normalizes");

    // The schema is the front gate: it must reject the invalid fixtures.
    for name in [
        "request/invalid_empty_set.json",
        "request/invalid_unknown_op.json",
    ] {
        assert!(
            validator.validate(&transaction_fixture(name)).is_err(),
            "{name} must be rejected by the request schema"
        );
    }
}

#[test]
fn transaction_result_fixtures_match_schema() {
    let validator = schema_validator("schemas/transaction_result.schema.json");
    for name in [
        "result/committed.json",
        "result/dry_run.json",
        "result/rejected_not_found.json",
    ] {
        assert!(
            validator.validate(&transaction_fixture(name)).is_ok(),
            "{name} must satisfy the result schema"
        );
    }
    assert!(
        validator
            .validate(&transaction_fixture(
                "result/invalid_updated_with_children.json"
            ))
            .is_err(),
        "invalid_updated_with_children must be rejected by the result schema"
    );
}

#[test]
fn core_emitted_transaction_result_conforms_to_schema() {
    // Producer-boundary guard: whatever `transact` actually serializes must
    // validate against the published result schema — for every variant and
    // every VerbOutcome shape — so the Rust types cannot silently drift.
    use clearhead_core::{TransactionOutcome, VerbError, VerbOutcome, canonical_id};

    let validator = schema_validator("schemas/transaction_result.schema.json");
    let id = canonical_id(uuid::Uuid::now_v7());

    let samples = [
        TransactionOutcome::Committed {
            operations: vec![
                VerbOutcome::Updated { id: id.clone() },
                VerbOutcome::Completed {
                    id: id.clone(),
                    children: 2,
                },
            ],
            files: vec![
                "/ws/charters/work.actions".into(),
                "/ws/charters/work.completed.actions".into(),
            ],
        },
        TransactionOutcome::DryRun {
            operations: vec![VerbOutcome::Cancelled {
                id: id.clone(),
                children: 0,
            }],
            files: vec!["/ws/charters/work.actions".into()],
        },
        TransactionOutcome::Rejected {
            operation: 1,
            error: VerbError::NotFound { query: id.clone() },
        },
    ];

    for outcome in samples {
        let value = serde_json::to_value(&outcome).unwrap();
        if let Err(errors) = validator.validate(&value) {
            let detail: Vec<String> = errors
                .map(|error| format!("  - {} (at {})", error, error.instance_path))
                .collect();
            panic!(
                "core-emitted result {value} violates the schema:\n{}",
                detail.join("\n")
            );
        }
    }
}

#[test]
fn semantic_corpus_formats_idempotently_and_round_trips() {
    for path in semantic_fixtures() {
        let source = std::fs::read_to_string(&path).unwrap();
        let actions = parse_actions(&source)
            .unwrap_or_else(|error| panic!("Core failed to parse {path:?}: {error}"));
        let once = format(&actions, OutputFormat::Actions, None, None)
            .unwrap_or_else(|error| panic!("Core failed to format {path:?}: {error}"));
        let reparsed = parse_actions(&once)
            .unwrap_or_else(|error| panic!("Core failed to reparse {path:?}: {error}"));
        let twice = format(&reparsed, OutputFormat::Actions, None, None).unwrap();

        assert_eq!(reparsed, actions, "semantic round trip changed {path:?}");
        assert_eq!(twice, once, "formatting was not idempotent for {path:?}");
    }
}
