//! Locked reload must preserve exact identity across active and completed files.
mod common;

use common::TestEnv;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;

const ROOT: &str = "01951111-0000-7000-8000-000000000001";
const OTHER: &str = "01951111-0000-7000-8000-000000000002";
const THIRD: &str = "01951111-0000-7000-8000-000000000003";

fn fixture(active: &str, completed: &str) -> TestEnv {
    let env = TestEnv::new();
    env.write_actions("work.actions", active);
    env.write_actions("work.completed.actions", completed);
    env
}

fn snapshot(env: &TestEnv) -> Vec<(Vec<u8>, std::time::SystemTime)> {
    ["work.actions", "work.completed.actions"]
        .iter()
        .map(|name| {
            let path = env.data_dir.join("charters").join(name);
            (
                fs::read(&path).unwrap(),
                fs::metadata(path).unwrap().modified().unwrap(),
            )
        })
        .collect()
}

fn command(env: &TestEnv, verb: &str) -> assert_cmd::Command {
    let mut command = env.command();
    command
        .args([verb, "action", ROOT, "--file"])
        .arg(env.data_dir.join("charters/work.actions"));
    command
}

#[test]
fn exact_id_survives_duplicate_names_on_either_side() {
    for verb in ["complete", "cancel", "reopen", "archive", "unarchive"] {
        for duplicates_in_source in [false, true] {
            let state = if matches!(verb, "complete" | "cancel") {
                "[ ]"
            } else {
                "[x]"
            };
            let target = format!("{state} Chore #{ROOT}\n");
            let duplicates = format!("[x] Chore #{OTHER}\n[x] Chore #{THIRD}\n");
            let (source, destination) = if duplicates_in_source {
                (format!("{target}{duplicates}"), String::new())
            } else {
                (target, duplicates)
            };
            let (a, c) = if verb == "unarchive" {
                (&destination, &source)
            } else {
                (&source, &destination)
            };
            let env = fixture(a, c);
            let before = snapshot(&env);
            command(&env, verb).assert().success();
            let after = snapshot(&env);
            let moved = matches!(verb, "archive" | "unarchive");
            for (index, name) in ["work.actions", "work.completed.actions"]
                .iter()
                .enumerate()
            {
                let actions = clearhead_cli::filesystem::read_actions(
                    &env.data_dir.join("charters").join(name),
                )
                .unwrap();
                for id in [OTHER, THIRD] {
                    let old = clearhead_core::workspace::actions::parse_actions(
                        std::str::from_utf8(&before[index].0).unwrap(),
                    )
                    .unwrap();
                    assert_eq!(
                        actions.iter().find(|a| a.id.to_string() == id),
                        old.iter().find(|a| a.id.to_string() == id),
                        "{verb}"
                    );
                }
                if !moved && index == 1 {
                    assert_eq!(after[index], before[index]);
                }
            }
            let active = clearhead_cli::filesystem::read_actions(
                &env.data_dir.join("charters/work.actions"),
            )
            .unwrap();
            let completed = clearhead_cli::filesystem::read_actions(
                &env.data_dir.join("charters/work.completed.actions"),
            )
            .unwrap();
            let target_in_completed = verb == "archive";
            let target = if target_in_completed {
                &completed
            } else {
                &active
            };
            let action = target
                .iter()
                .find(|a| a.id.to_string() == ROOT)
                .expect("exact target in correct file");
            assert_eq!(
                action.state,
                match verb {
                    "complete" | "archive" | "unarchive" => clearhead_core::ActionState::Completed,
                    "cancel" => clearhead_core::ActionState::Cancelled,
                    _ => clearhead_core::ActionState::NotStarted,
                }
            );
            assert!(
                !(if target_in_completed {
                    &active
                } else {
                    &completed
                })
                .iter()
                .any(|a| a.id.to_string() == ROOT)
            );
        }
    }
}

#[test]
fn exact_id_in_wrong_file_is_not_replaced_by_same_named_action() {
    for verb in ["archive", "unarchive"] {
        let target = format!("[x] Chore #{ROOT}\n");
        let decoy = format!("[x] Chore #{OTHER}\n");
        let (a, c) = if verb == "archive" {
            (&decoy, &target)
        } else {
            (&target, &decoy)
        };
        let env = fixture(a, c);
        let before = snapshot(&env);
        command(&env, verb)
            .assert()
            .failure()
            .stdout(predicate::str::contains(if verb == "archive" {
                "already archived"
            } else {
                "already active"
            }));
        assert_eq!(snapshot(&env), before);
    }
}

#[test]
fn archived_close_cascade_is_noop_only_when_outcome_already_holds() {
    for (verb, matching, opposite) in [("complete", "[x]", "[_]"), ("cancel", "[_]", "[x]")] {
        for dry_run in [false, true] {
            for explicit_file in [false, true] {
                for (root_state, child_state, succeeds) in [
                    (matching, opposite, true),
                    (opposite, matching, false),
                    (matching, "[ ]", false),
                ] {
                    let env = fixture(
                        "",
                        &format!("{root_state} Root #{ROOT}\n>{child_state} Child #{OTHER}\n"),
                    );
                    let before = snapshot(&env);
                    let mut cmd = env.command();
                    cmd.args([verb, "action", ROOT, "--cascade"]);
                    if explicit_file {
                        cmd.arg("--file")
                            .arg(env.data_dir.join("charters/work.actions"));
                    }
                    if dry_run {
                        cmd.arg("--dry-run");
                    }
                    if succeeds {
                        let output = cmd.assert().success().get_output().stdout.clone();
                        let result: Value = serde_json::from_slice(&output).unwrap();
                        if dry_run {
                            assert_eq!(
                                result,
                                serde_json::json!({"kind":"dry-run", "operations":[], "files":[]})
                            );
                        } else {
                            assert_eq!(
                                result["kind"],
                                if verb == "complete" {
                                    "completed"
                                } else {
                                    "cancelled"
                                }
                            );
                            assert_eq!(result["id"], format!("urn:uuid:{ROOT}"));
                            assert_eq!(result["open_children"], serde_json::json!([]));
                        }
                    } else {
                        cmd.assert().failure().stdout(
                            predicate::str::contains("\"kind\":\"archived\"")
                                .and(predicate::str::contains("unarchive"))
                                .and(predicate::str::contains(ROOT))
                                .and(predicate::str::contains("already-closed").not()),
                        );
                    }
                    assert_eq!(
                        snapshot(&env),
                        before,
                        "{verb}, dry_run={dry_run}, explicit_file={explicit_file}"
                    );
                }
            }
        }
    }
}
