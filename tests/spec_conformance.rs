//! End-to-end conformance: parse the specifications repo's own `.actions`
//! fixtures with Core, project them to the canonical schema JSON, and validate
//! against the spec's `actions.schema.json`.
//!
//! Runs only under `--features spec-conformance`, so a bare `cargo test` needs
//! neither `jsonschema` nor a specifications checkout. The spec is located via
//! `CLEARHEAD_SPEC_DIR` (default: the sibling `../specifications`), matching the
//! platform workspace layout — never the platform meta-repo.
#![cfg(feature = "spec-conformance")]

use std::path::{Path, PathBuf};

fn spec_dir() -> PathBuf {
    std::env::var("CLEARHEAD_SPEC_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../specifications"))
}

fn collect_actions_fixtures(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_actions_fixtures(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("actions") {
            out.push(path);
        }
    }
}

#[test]
fn core_projection_conforms_to_spec_schema() {
    let spec = spec_dir();
    let schema_path = spec.join("schemas/actions.schema.json");
    assert!(
        schema_path.exists(),
        "spec schema not found at {schema_path:?}; set CLEARHEAD_SPEC_DIR to a specifications checkout"
    );

    let schema: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&schema_path).unwrap()).unwrap();
    let validator = jsonschema::JSONSchema::compile(&schema).expect("spec schema compiles");

    let mut fixtures = Vec::new();
    collect_actions_fixtures(&spec.join("examples/conformance"), &mut fixtures);
    fixtures.sort();
    assert!(
        !fixtures.is_empty(),
        "no .actions conformance fixtures found under {spec:?}/examples/conformance"
    );

    for path in &fixtures {
        let source = std::fs::read_to_string(path).unwrap();
        let actions = clearhead_core::parse_actions(&source)
            .unwrap_or_else(|e| panic!("Core failed to parse {path:?}: {e}"));
        let document = clearhead_core::schema_export::to_schema_document(&actions);
        let value = serde_json::to_value(&document).unwrap();

        if let Err(errors) = validator.validate(&value) {
            let detail: Vec<String> = errors
                .map(|e| format!("  - {} (at {})", e, e.instance_path))
                .collect();
            panic!(
                "Core's projection of {path:?} violates the spec schema:\n{}",
                detail.join("\n")
            );
        }
    }
}
