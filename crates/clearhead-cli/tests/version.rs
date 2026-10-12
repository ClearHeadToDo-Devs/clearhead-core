//! `clearhead --version` names the build, the specification release it
//! implements and the grammar release it parses with, so a user can tell which
//! release a binary conforms to and an editor can detect grammar drift.
use assert_cmd::Command;

#[test]
fn version_names_the_specification_and_grammar_releases() {
    // Editors parse `grammar vX.Y.Z` from this line, so the linked grammar must
    // record its version (ABI 15 or later).
    let grammar = clearhead_core::grammar_version().expect("linked grammar reports its version");
    Command::new(assert_cmd::cargo::cargo_bin!("clearhead"))
        .arg("--version")
        .assert()
        .success()
        .stdout(format!(
            "clearhead {} (specification {}, grammar {grammar})\n",
            env!("CARGO_PKG_VERSION"),
            clearhead_core::SPECIFICATION
        ));
}
