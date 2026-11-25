//! Release adoption only: no native Habit semantics are asserted here.

#[test]
fn habit_release_uses_published_registry_grammar_and_specification() {
    assert_eq!(clearhead_core::SPECIFICATION, "v0.8.0");
    assert_eq!(
        clearhead_core::grammar_version().as_deref(),
        Some("v0.12.0")
    );
    assert_eq!(
        clearhead_core::workspace::sidecar::CHARTER_METADATA_SCHEMA_URL,
        "https://clearhead.dev/schemas/v0.8.0/charter_metadata.schema.json"
    );
    assert_eq!(
        clearhead_core::workspace::manifest::WORKSPACE_SCHEMA_URL,
        "https://clearhead.dev/schemas/v0.8.0/workspace.schema.json"
    );

    let output = std::process::Command::new("cargo")
        .args(["metadata", "--locked", "--format-version", "1"])
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .expect("cargo metadata must run");
    assert!(output.status.success(), "{:?}", output);
    let metadata: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    let grammars: Vec<_> = metadata["packages"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|package| package["name"] == "tree-sitter-actions")
        .collect();
    assert_eq!(grammars.len(), 1, "one registry grammar, no local patch");
    assert_eq!(grammars[0]["version"], "0.12.0");
    assert_eq!(
        grammars[0]["source"],
        "registry+https://github.com/rust-lang/crates.io-index"
    );
    assert!(include_str!("../../../Cargo.lock").contains(
        "checksum = \"e5898d0807a18071aa1b9fce10aa86119ff7f6bc16ae406e8d7765a207197d83\""
    ));
}
