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
fn add_charter_writes_explicit_new_state() {
    let env = TestEnv::new();

    env.command()
        .args(["add", "charter", "Fresh Work", "--alias", "fresh-work"])
        .assert()
        .success();

    let path = env.data_dir.join("charters/fresh-work.md");
    let Ok(content) = fs::read_to_string(path) else {
        panic!("new Charter document should be readable");
    };
    assert!(content.contains("state: New"), "{content}");
}

#[test]
fn read_charters_json_materializes_omitted_state_as_new() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    let assert = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let Ok(rows) = serde_json::from_slice::<serde_json::Value>(&assert.get_output().stdout) else {
        panic!("read charters should emit JSON");
    };

    let Some(charter) = rows
        .as_array()
        .and_then(|rows| rows.iter().find(|row| row["alias"] == "my-charter"))
    else {
        panic!("my-charter should be listed: {rows}");
    };
    assert_eq!(charter["state"], "New");
}

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
fn jot_into_project_root_charter_creates_readme_not_phantom() {
    // Project layout: a `.clearhead/` under the working dir. The root
    // `next.actions` charter is named for the project and pairs with README.md;
    // a derived `next.md` would infer to a separate "next" charter — the bug.
    let env = TestEnv::new();
    let charters = env.work_dir.join(".clearhead/charters");
    fs::create_dir_all(&charters).unwrap();
    fs::write(charters.join("next.actions"), "").unwrap();

    // jot used to bail on a primary charter with no `.md`; it now materializes
    // the correctly-paired document instead.
    env.command()
        .args(["jot", "a project finding"])
        .assert()
        .success();

    let readme = charters.join("README.md");
    assert!(readme.exists(), "project root should materialize README.md");
    assert!(
        !charters.join("next.md").exists(),
        "must not create a phantom next.md"
    );
    assert!(
        fs::read_to_string(&readme)
            .unwrap()
            .contains("a project finding")
    );

    let assert = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let rows: serde_json::Value = serde_json::from_slice(&assert.get_output().stdout).unwrap();
    assert_eq!(
        rows.as_array().map(|r| r.len()),
        Some(1),
        "next.actions + README.md must pair into one charter, not collide: {rows}"
    );
}

#[test]
fn jot_into_user_root_charter_creates_readme_not_phantom() {
    // Both scopes share one root shape: the user root's `next.actions` pairs
    // with README.md exactly like a project root's.
    let env = TestEnv::new();
    env.write_actions("next.actions", "");

    env.command()
        .args(["jot", "a user finding"])
        .assert()
        .success();

    assert!(
        env.data_dir.join("charters/README.md").exists(),
        "user root should materialize README.md"
    );
    assert!(
        !env.data_dir.join("charters/next.md").exists(),
        "must not create a phantom next.md"
    );

    let assert = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let rows: serde_json::Value = serde_json::from_slice(&assert.get_output().stdout).unwrap();
    assert_eq!(
        rows.as_array().map(|r| r.len()),
        Some(1),
        "next.actions + README.md must pair into one charter, not collide: {rows}"
    );
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
