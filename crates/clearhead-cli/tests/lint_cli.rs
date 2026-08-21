use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

/// Helper to get the binary path using the non-deprecated macro
fn get_cmd() -> Command {
    Command::new(assert_cmd::cargo::cargo_bin!("clearhead"))
}

#[test]
fn test_lint_cli_info_diagnostics() -> Result<(), Box<dyn std::error::Error>> {
    // Test with info-level diagnostic: completed action missing completion date (I001)
    let file_content =
        "[x] Completed action with no completion date #01942d99-4c27-77f6-9316-107024843939";
    let file_path = "test_lint_info.actions";

    fs::write(file_path, file_content)?;

    let mut cmd = get_cmd();

    cmd.arg("lint").arg("file").arg(file_path);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("I001"))
        .stdout(predicate::str::contains("INFO"));

    fs::remove_file(file_path)?;

    Ok(())
}

#[test]
fn test_lint_cli_success() -> Result<(), Box<dyn std::error::Error>> {
    let file_content = "[ ] Valid Action ^2025-01-01T12:00 #01942d99-4c27-77f6-9316-107024843939";
    let file_path = "test_lint_success.actions";

    fs::write(file_path, file_content)?;

    let mut cmd = get_cmd();

    cmd.arg("lint").arg("file").arg(file_path);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("no issues found"));

    fs::remove_file(file_path)?;

    Ok(())
}
