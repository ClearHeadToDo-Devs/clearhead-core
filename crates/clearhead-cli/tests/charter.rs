mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::fs;

const CHARTER_MD: &str = "\
---
id: 019cffb8-0000-7000-8000-000000000001
alias: my-charter
---
# My Charter
";

#[test]
fn close_charter_by_query_updates_state() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    env.command()
        .args(["close", "charter", "my-charter"])
        .assert()
        .success();

    let content = fs::read_to_string(env.data_dir.join("charters/my-charter.md")).unwrap();
    assert!(
        content.contains("state: Closed"),
        "Expected state: Closed in:\n{content}"
    );
}

#[test]
fn close_charter_creates_md_for_implicit_charter() {
    let env = TestEnv::new();
    env.write_actions("my-charter.actions", "");

    env.command()
        .args(["close", "charter", "my-charter"])
        .assert()
        .success();

    let md_path = env.data_dir.join("charters/my-charter.md");
    assert!(
        md_path.exists(),
        ".md file should be created for implicit charter"
    );
    let content = fs::read_to_string(&md_path).unwrap();
    assert!(content.contains("state: Closed"));
}

#[test]
fn close_charter_by_file_resolves_from_actions_path() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    let actions_path = env.data_dir.join("charters/my-charter.actions");

    env.command()
        .args(["close", "charter", "--file"])
        .arg(&actions_path)
        .assert()
        .success();

    let content = fs::read_to_string(env.data_dir.join("charters/my-charter.md")).unwrap();
    assert!(content.contains("state: Closed"));
}

#[test]
fn close_charter_dry_run_does_not_write() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    let md_path = env.data_dir.join("charters/my-charter.md");
    let before = fs::read_to_string(&md_path).unwrap();

    env.command()
        .args(["close", "charter", "my-charter", "--dry-run"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Would"));

    let after = fs::read_to_string(&md_path).unwrap();
    assert_eq!(before, after, "dry-run must not modify the file");
}

#[test]
fn close_charter_no_args_fails() {
    let env = TestEnv::new();
    env.command()
        .args(["close", "charter"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Provide"));
}

#[test]
fn close_charter_unknown_file_fails() {
    let env = TestEnv::new();
    env.command()
        .args([
            "close",
            "charter",
            "--file",
            "/nonexistent/path/foo.actions",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("No charter found"));
}

#[test]
fn archive_charter_uses_the_native_adapter_end_to_end() {
    let env = TestEnv::new();
    let id = "019cffb8-0000-7000-8000-000000000010";
    env.write_text(
        "charters/done.md",
        &format!("---\nid: {id}\nalias: done\nstate: Closed\n---\n# Done\n"),
    );
    env.write_actions(
        "done.actions",
        "[x] Finished #019cffb8-0000-7000-8000-000000000011\n",
    );

    env.command()
        .args(["archive", "charter", "done"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Archived charter 'done'"));

    assert!(!env.data_dir.join("charters/done.actions").exists());
    assert!(env.data_dir.join(format!("archive/{id}.actions")).exists());
    assert!(env.data_dir.join(format!("archive/{id}.md")).exists());
}

#[test]
fn archive_closed_sweeps_terminal_charters_but_leaves_active_ones() {
    let env = TestEnv::new();
    env.write_text(
        "charters/done.md",
        "---\nid: 019cffb8-0000-7000-8000-000000000020\nalias: done\nstate: Closed\n---\n# Done\n",
    );
    env.write_actions("done.actions", "");
    env.write_text(
        "charters/live.md",
        "---\nid: 019cffb8-0000-7000-8000-000000000021\nalias: live\nstate: Active\n---\n# Live\n",
    );
    env.write_actions("live.actions", "");

    env.command()
        .args(["archive", "charter", "--closed"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Archived charter 'done'"));

    assert!(!env.data_dir.join("charters/done.md").exists());
    assert!(env.data_dir.join("charters/live.md").exists());
}
