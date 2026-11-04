//! Release adoption only: no native Habit semantics are asserted here.

#[test]
fn habit_release_uses_published_registry_grammar_and_specification() {
    assert_eq!(clearhead_core::SPECIFICATION, "v0.5.0");
    assert_eq!(
        clearhead_core::grammar_version().as_deref(),
        Some("v0.11.1")
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
    assert_eq!(grammars[0]["version"], "0.11.1");
    assert_eq!(
        grammars[0]["source"],
        "registry+https://github.com/rust-lang/crates.io-index"
    );
    assert!(include_str!("../../../Cargo.lock").contains(
        "checksum = \"e48148b8765ef46ccfddcbedeb72604b60c8db47b51b4c76e232700190c57216\""
    ));
}
