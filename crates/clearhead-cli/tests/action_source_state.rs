//! A retained closed line must not shadow a verb's source-state target.
mod common;

use clearhead_cli::filesystem::read_actions;
use clearhead_core::ActionState;
use common::TestEnv;

const CLOSED: &str = "01951111-0000-7000-8000-000000000001";
const OPEN: &str = "01951111-0000-7000-8000-000000000002";

#[test]
fn close_prefers_open_exact_and_substring_matches() {
    for verb in ["complete", "cancel"] {
        for (closed, open, query) in [
            ("Water plants", "Water plants", "Water plants"),
            ("Draft report", "Draft report v2", "Draft"),
        ] {
            for explicit in [false, true] {
                let env = TestEnv::new();
                env.write_actions(
                    "work.actions",
                    &format!("[x] {closed} %2026-10-01T10:00 #{CLOSED}\n[ ] {open} #{OPEN}\n"),
                );
                let path = env.data_dir.join("charters/work.actions");
                let mut command = env.command();
                command.args([verb, "action", query]);
                if explicit {
                    command.arg("--file").arg(&path);
                }
                command.assert().success();
                let actions = read_actions(&path).unwrap();
                assert!(actions[1].state.is_terminal());
                assert_eq!(actions[0].state, ActionState::Completed);
            }
        }
    }
}

#[test]
fn reopen_prefers_closed_matches_even_across_files() {
    for separate_files in [false, true] {
        let env = TestEnv::new();
        env.write_actions("a.actions", &format!("[ ] Water plants #{OPEN}\n"));
        if separate_files {
            env.write_actions(
                "b.actions",
                &format!("[x] Water plants %2026-10-01T10:00 #{CLOSED}\n"),
            );
        } else {
            env.write_actions(
                "a.actions",
                &format!(
                    "[ ] Water plants #{OPEN}\n[x] Water plants %2026-10-01T10:00 #{CLOSED}\n"
                ),
            );
        }
        env.command()
            .args(["reopen", "action", "Water plants"])
            .assert()
            .success();
        let path = env.data_dir.join(if separate_files {
            "charters/b.actions"
        } else {
            "charters/a.actions"
        });
        assert!(
            read_actions(&path)
                .unwrap()
                .iter()
                .all(|a| !a.state.is_terminal())
        );
    }
}

#[test]
fn update_prefers_open_match_across_files() {
    let env = TestEnv::new();
    env.write_actions(
        "a.actions",
        &format!("[x] Water plants %2026-10-01T10:00 #{CLOSED}\n"),
    );
    env.write_actions("b.actions", &format!("[ ] Water plants #{OPEN}\n"));
    env.command()
        .args(["update", "action", "Water plants", "--name", "Water garden"])
        .assert()
        .success();
    assert_eq!(
        read_actions(&env.data_dir.join("charters/a.actions")).unwrap()[0].name,
        "Water plants"
    );
    assert_eq!(
        read_actions(&env.data_dir.join("charters/b.actions")).unwrap()[0].name,
        "Water garden"
    );
}

#[test]
fn explicit_ids_still_report_wrong_source_state() {
    use predicates::prelude::*;
    let env = TestEnv::new();
    env.write_actions(
        "work.actions",
        &format!("[x] Water plants %2026-10-01T10:00 #{CLOSED}\n[ ] Water plants #{OPEN}\n"),
    );
    env.command()
        .args(["complete", "action", CLOSED])
        .assert()
        .failure()
        .stdout(predicate::str::contains("already-closed"));
    env.command()
        .args(["reopen", "action", OPEN])
        .assert()
        .failure()
        .stdout(predicate::str::contains("already-open"));
}
