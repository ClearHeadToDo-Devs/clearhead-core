mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::process::Stdio;

// ── open-only filter ─────────────────────────────────────────────────────────

#[test]
fn open_only_excludes_completed_and_cancelled() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "\
[ ] Open task  #019e0000-0000-7000-0000-000000000001\n\
[x] Done task  #019e0000-0000-7000-0000-000000000002\n\
[_] Cancelled task  #019e0000-0000-7000-0000-000000000003\n\
[-] In progress task  #019e0000-0000-7000-0000-000000000004\n\
",
    );
    env.command()
        .args(["read", "actions", "--open-only", "--format", "json"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Open task"))
        .stdout(predicate::str::contains("In progress task"))
        .stdout(predicate::str::contains("Done task").not())
        .stdout(predicate::str::contains("Cancelled task").not());
}

// ── context filter ───────────────────────────────────────────────────────────

#[test]
fn context_filter_selects_matching_tag() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "\
[ ] Tagged task  +work  #019e0000-0000-7000-0000-000000000001\n\
[ ] Other task  +personal  #019e0000-0000-7000-0000-000000000002\n\
[ ] Untagged task  #019e0000-0000-7000-0000-000000000003\n\
",
    );
    env.command()
        .args(["read", "actions", "--context", "work", "--format", "json"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Tagged task"))
        .stdout(predicate::str::contains("Other task").not())
        .stdout(predicate::str::contains("Untagged task").not());
}

#[test]
fn context_filter_with_tag_hierarchy_matches_descendants() {
    let env = TestEnv::new();
    // computer → terminal → neovim hierarchy
    env.write_config(
        r#"{
        "tag_hierarchies": {
            "computer": ["terminal"],
            "terminal": ["neovim"]
        }
    }"#,
    );
    env.write_actions(
        "inbox.actions",
        "\
[ ] Neovim task  +neovim  #019e0000-0000-7000-0000-000000000001\n\
[ ] Terminal task  +terminal  #019e0000-0000-7000-0000-000000000002\n\
[ ] Unrelated task  +personal  #019e0000-0000-7000-0000-000000000003\n\
",
    );
    // Filtering by "computer" should match neovim and terminal (both descendants).
    env.command()
        .args([
            "read",
            "actions",
            "--context",
            "computer",
            "--format",
            "json",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Neovim task"))
        .stdout(predicate::str::contains("Terminal task"))
        .stdout(predicate::str::contains("Unrelated task").not());
}

// ── open-only + context combined ─────────────────────────────────────────────

#[test]
fn open_only_and_context_filter_combine() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "\
[ ] Open work task  +work  #019e0000-0000-7000-0000-000000000001\n\
[x] Done work task  +work  #019e0000-0000-7000-0000-000000000002\n\
[ ] Open personal task  +personal  #019e0000-0000-7000-0000-000000000003\n\
",
    );
    env.command()
        .args([
            "read",
            "actions",
            "--open-only",
            "--context",
            "work",
            "--format",
            "json",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("Open work task"))
        .stdout(predicate::str::contains("Done work task").not())
        .stdout(predicate::str::contains("Open personal task").not());
}

// ── DSL pipe output ───────────────────────────────────────────────────────────

#[test]
fn pipe_output_is_dsl_format() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "\
[ ] Open task  #019e0000-0000-7000-0000-000000000001\n\
[x] Done task  #019e0000-0000-7000-0000-000000000002\n\
",
    );
    // No --format flag + piped stdout → .actions DSL, open-only keeps only open.
    env.command()
        .args(["read", "actions", "--open-only"])
        .assert()
        .success()
        .stdout(predicate::str::contains("[ ] Open task"))
        .stdout(predicate::str::contains("[x] Done task").not());
}

// ── no filter: all actions returned ──────────────────────────────────────────

#[test]
fn closed_stdout_is_a_clean_pipeline_exit() {
    let env = TestEnv::new();
    let actions = (0..20_000)
        .map(|i| format!("[ ] Task {i} #019e0000-0000-7000-8000-{i:012x}"))
        .collect::<Vec<_>>()
        .join("\n");
    env.write_actions("inbox.actions", &actions);

    let mut command = env.std_command();
    command
        .args(["read", "actions", "--format", "ids"])
        .stdout(Stdio::piped());
    let mut child = command.spawn().unwrap();
    drop(child.stdout.take());
    let status = child.wait().unwrap();
    assert!(status.success(), "closed stdout exited with {status}");
}

#[test]
fn no_filter_returns_all_states() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "\
[ ] Open task  #019e0000-0000-7000-0000-000000000001\n\
[x] Done task  #019e0000-0000-7000-0000-000000000002\n\
[_] Cancelled task  #019e0000-0000-7000-0000-000000000003\n\
",
    );
    env.command()
        .args(["read", "actions", "--format", "json"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Open task"))
        .stdout(predicate::str::contains("Done task"))
        .stdout(predicate::str::contains("Cancelled task"));
}
