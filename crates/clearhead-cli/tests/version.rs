//! `clearhead --version` names the build and the specification release it
//! implements, so a user can tell which release a binary conforms to.
use assert_cmd::Command;

#[test]
fn version_names_the_specification_release() {
    Command::new(assert_cmd::cargo::cargo_bin!("clearhead"))
        .arg("--version")
        .assert()
        .success()
        .stdout(format!(
            "clearhead {} (specification {})\n",
            env!("CARGO_PKG_VERSION"),
            clearhead_core::SPECIFICATION
        ));
}
