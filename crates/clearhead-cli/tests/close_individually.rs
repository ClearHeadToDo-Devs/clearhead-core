//! Decisions 58/59: closure changes state; only root-tree archival moves facts.
mod common;

use clearhead_cli::filesystem::read_actions;
use clearhead_core::ActionState;
use common::TestEnv;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;

const ROOT: &str = "01951111-0000-7000-8000-000000000001";
const CHILD: &str = "01951111-0000-7000-8000-000000000002";
const CLOSED: &str = "01951111-0000-7000-8000-000000000003";
const GRAND: &str = "01951111-0000-7000-8000-000000000004";

fn setup(root: &str, child: &str) -> TestEnv {
    let env = TestEnv::new();
    let (root_state, root_date) = root.split_once(" %").unwrap_or((root, ""));
    let (child_state, child_date) = child.split_once(" %").unwrap_or((child, ""));
    let root_date = if root_date.is_empty() {
        String::new()
    } else {
        format!("%{root_date}")
    };
    let child_date = if child_date.is_empty() {
        String::new()
    } else {
        format!("%{child_date}")
    };
    env.write_actions("work.actions", &format!("{root_state} Root {root_date} #{ROOT}\n>{child_state} Child {child_date} #{CHILD}\n>>[_] Closed %2026-10-01T10:00 #{CLOSED}\n>>>[-] Grandchild #{GRAND}\n"));
    env
}

fn run(env: &TestEnv, verb: &str, id: &str, flags: &[&str]) -> Value {
    let output = env
        .command()
        .args([verb, "action", id, "--file"])
        .arg(env.data_dir.join("charters/work.actions"))
        .args(flags)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    serde_json::from_slice(&output).expect("structured outcome")
}

fn active(env: &TestEnv) -> Vec<clearhead_core::Action> {
    read_actions(&env.data_dir.join("charters/work.actions")).unwrap()
}

#[test]
fn single_close_keeps_tree_and_reports_open_descendants() {
    for verb in ["complete", "cancel"] {
        let env = setup("[ ]", "[-]");
        let before = active(&env);
        let result = run(&env, verb, ROOT, &[]);
        assert!(result.get("children").is_none());
        assert_eq!(
            result["open_children"],
            serde_json::json!([format!("urn:uuid:{CHILD}"), format!("urn:uuid:{GRAND}")])
        );
        let after = active(&env);
        assert_eq!(after.len(), 4);
        assert_eq!(&after[1..], &before[1..]);
        assert!(after[0].state.is_terminal());
        assert!(after[0].completed_at.is_some());
        assert!(
            !env.data_dir
                .join("charters/work.completed.actions")
                .exists()
        );
        run(&env, "reopen", ROOT, &[]);
        assert_eq!(active(&env), before);
    }
}

#[test]
fn cascade_forces_only_target_and_open_members_and_is_noop_when_done() {
    for (verb, desired, opposite) in [
        ("complete", ActionState::Completed, "[_]"),
        ("cancel", ActionState::Cancelled, "[x]"),
    ] {
        let env = setup(&format!("{opposite} %2026-10-02T11:00"), "[ ]");
        let before = active(&env);
        run(&env, verb, ROOT, &["--cascade"]);
        let after = active(&env);
        assert_eq!(after[0].state, desired);
        assert_ne!(after[0].completed_at, before[0].completed_at);
        assert_eq!(after[1].state, desired);
        assert_eq!(after[2], before[2]);
        assert_eq!(after[3].state, desired);
        let path = env.data_dir.join("charters/work.actions");
        let bytes = fs::read(&path).unwrap();
        let modified = fs::metadata(&path).unwrap().modified().unwrap();
        run(&env, verb, ROOT, &["--cascade"]);
        assert_eq!(fs::read(&path).unwrap(), bytes);
        assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), modified);
        assert_eq!(active(&env)[0].completed_at, after[0].completed_at);
    }
}

#[test]
fn both_close_cascades_preserve_matching_target_and_both_closed_member_states() {
    for (verb, marker) in [("complete", "[x]"), ("cancel", "[_]")] {
        let env = setup(
            &format!("{marker} %2026-10-02T11:00"),
            "[x] %2026-10-03T11:00",
        );
        let before = active(&env);
        run(&env, verb, ROOT, &["--cascade"]);
        let after = active(&env);
        assert_eq!(&after[..3], &before[..3]);
        assert_eq!(after[3].state, after[0].state);
        assert!(after[3].completed_at.is_some());
        let path = env.data_dir.join("charters/work.actions");
        let bytes = fs::read(&path).unwrap();
        let modified = fs::metadata(&path).unwrap().modified().unwrap();
        run(&env, verb, ROOT, &["--cascade"]);
        assert_eq!(fs::read(&path).unwrap(), bytes);
        assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), modified);
    }
}

#[test]
fn reopen_cascade_walks_through_open_members_and_noop_writes_nothing() {
    let env = setup("[-]", "[x] %2026-10-02T11:00");
    let before = active(&env);
    run(&env, "reopen", ROOT, &["--cascade"]);
    let after = active(&env);
    assert_eq!(after[0], before[0]);
    assert_eq!(after[3], before[3]);
    for i in [1, 2] {
        assert_eq!(after[i].state, ActionState::NotStarted);
        assert!(after[i].completed_at.is_none());
    }
    let path = env.data_dir.join("charters/work.actions");
    let modified = fs::metadata(&path).unwrap().modified().unwrap();
    run(&env, "reopen", ROOT, &["--cascade"]);
    assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), modified);
}

#[test]
fn single_verbs_and_update_refuse_closed_active_target() {
    for state in ["[x]", "[_]"] {
        let env = setup(state, "[ ]");
        for verb in ["complete", "cancel", "update"] {
            let mut cmd = env.command();
            cmd.args([verb, "action", ROOT, "--file"])
                .arg(env.data_dir.join("charters/work.actions"));
            if verb == "update" {
                cmd.args(["--name", "Do not rename"]);
            }
            cmd.assert()
                .failure()
                .stdout(predicate::str::contains("already-closed"));
        }
        env.command().arg("transact").write_stdin(format!(r#"{{"operations":[{{"op":"update-action","target":"urn:uuid:{ROOT}","set":{{"name":"No"}}}}]}}"#))
            .assert().failure().stdout(predicate::str::contains("already-closed"));
    }
}

#[test]
fn transaction_closes_one_and_can_reopen_then_update_in_order() {
    let env = setup("[ ]", "[ ]");
    env.command().arg("transact").write_stdin(format!(r#"{{"operations":[{{"op":"complete-action","target":"urn:uuid:{ROOT}"}},{{"op":"reopen-action","target":"urn:uuid:{ROOT}"}},{{"op":"update-action","target":"urn:uuid:{ROOT}","set":{{"name":"Renamed"}}}}]}}"#))
        .assert().success();
    let actions = active(&env);
    assert_eq!(actions[0].name, "Renamed");
    assert_eq!(actions[0].state, ActionState::NotStarted);
    assert!(actions[0].completed_at.is_none());
    assert_eq!(actions[1].state, ActionState::NotStarted);
    assert_eq!(actions[2].state, ActionState::Cancelled);
}

#[test]
fn singles_then_archive_equal_cascade_then_archive() {
    let single = setup("[ ]", "[ ]");
    let cascade = setup("[ ]", "[ ]");
    for id in [ROOT, CHILD, GRAND] {
        run(&single, "complete", id, &[]);
    }
    run(&cascade, "complete", ROOT, &["--cascade"]);
    for env in [&single, &cascade] {
        run(env, "archive", ROOT, &[]);
    }
    assert_eq!(
        read_actions(&single.data_dir.join("charters/work.completed.actions")).unwrap(),
        read_actions(&cascade.data_dir.join("charters/work.completed.actions")).unwrap()
    );
    assert!(active(&single).is_empty());
}

#[test]
fn archive_without_metadata_does_not_create_sidecars() {
    let env = setup("[x]", "[x]");
    run(&env, "complete", GRAND, &[]);
    for verb in ["archive", "unarchive"] {
        run(&env, verb, ROOT, &[]);
        for name in [".work.json", ".work.completed.json"] {
            assert!(!env.data_dir.join("charters").join(name).exists());
        }
    }
}

#[test]
fn archive_without_matching_metadata_leaves_sidecars_unchanged() {
    let env = setup("[x]", "[x]");
    run(&env, "complete", GRAND, &[]);
    let metadata = "{\"actions\": {}}\n";
    env.write_text("charters/.work.json", metadata);
    let path = env.data_dir.join("charters/.work.json");
    let modified = fs::metadata(&path).unwrap().modified().unwrap();
    run(&env, "archive", ROOT, &[]);
    assert_eq!(fs::read_to_string(&path).unwrap(), metadata);
    assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), modified);
    assert!(!env.data_dir.join("charters/.work.completed.json").exists());
}

#[test]
fn storage_moves_distinguish_already_stored_roots_from_missing_ids() {
    let env = setup("[x]", "[x]");
    run(&env, "complete", GRAND, &[]);
    for explicit_file in [false, true] {
        let mut command = env.command();
        command.args(["unarchive", "action", ROOT]);
        if explicit_file {
            command
                .arg("--file")
                .arg(env.data_dir.join("charters/work.actions"));
        }
        command.assert().failure().stderr(
            predicate::str::contains("already active")
                .and(predicate::str::contains("not found").not()),
        );
    }
    run(&env, "archive", ROOT, &[]);
    for explicit_file in [false, true] {
        let mut command = env.command();
        command.args(["archive", "action", ROOT]);
        if explicit_file {
            command
                .arg("--file")
                .arg(env.data_dir.join("charters/work.actions"));
        }
        command.assert().failure().stderr(
            predicate::str::contains("already archived")
                .and(predicate::str::contains("not found").not()),
        );
    }
    for verb in ["archive", "unarchive"] {
        env.command()
            .args([verb, "action", "01951111-0000-7000-8000-000000000099"])
            .assert()
            .failure()
            .stderr(
                predicate::str::contains("not found")
                    .and(predicate::str::contains("already").not()),
            );
    }
}

#[test]
fn transition_previews_are_enveloped_and_write_nothing() {
    for verb in ["complete", "cancel", "reopen"] {
        for cascade in [false, true] {
            let env = if verb == "reopen" {
                setup("[x]", "[_]")
            } else {
                setup("[ ]", "[-]")
            };
            let path = env.data_dir.join("charters/work.actions");
            let before = fs::read(&path).unwrap();
            let modified = fs::metadata(&path).unwrap().modified().unwrap();
            let mut flags = vec!["--dry-run"];
            if cascade {
                flags.push("--cascade");
            }
            let preview = run(&env, verb, ROOT, &flags);
            assert_eq!(fs::read(&path).unwrap(), before);
            assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), modified);
            assert_eq!(preview["kind"], "dry-run", "{verb}, cascade={cascade}");
            assert!(!preview["operations"].as_array().unwrap().is_empty());
            assert!(
                preview["files"]
                    .as_array()
                    .unwrap()
                    .contains(&serde_json::json!(path))
            );
            flags.remove(0);
            let committed = run(&env, verb, ROOT, &flags);
            assert_ne!(preview, committed);
            assert_eq!(committed["kind"], preview["operations"][0]["kind"]);
        }
    }
}

#[test]
fn storage_move_dry_runs_are_marked_and_do_not_write() {
    let env = setup("[x]", "[x]");
    run(&env, "complete", GRAND, &[]);
    for verb in ["archive", "unarchive"] {
        let before = active(&env);
        let completed = env.data_dir.join("charters/work.completed.actions");
        let completed_before = fs::read(&completed).ok();
        let result = run(&env, verb, ROOT, &["--dry-run"]);
        assert_eq!(result["kind"], "dry-run");
        assert_eq!(
            result["operations"][0]["kind"],
            if verb == "archive" {
                "archived"
            } else {
                "unarchived"
            }
        );
        assert_eq!(active(&env), before);
        assert_eq!(fs::read(&completed).ok(), completed_before);
        assert!(!result["files"].as_array().unwrap().is_empty());
        let committed = run(&env, verb, ROOT, &[]);
        assert_eq!(committed["kind"], "committed");
        assert_eq!(committed["operations"], result["operations"]);
        assert_eq!(committed["files"], result["files"]);
    }
}

#[test]
fn archive_unarchive_roundtrip_is_lossless_with_sidecars() {
    let env = setup("[x] %2026-10-02T11:00", "[_] %2026-10-03T11:00");
    run(&env, "complete", GRAND, &[]);
    let before = active(&env);
    let metadata = serde_json::json!({"actions": { ROOT: {"created":"2026-10-01T00:00:00Z"}, CLOSED: {"created":"2026-10-02T00:00:00Z"}}});
    let canonical = clearhead_core::workspace::sidecar::render_sidecar(
        &clearhead_core::workspace::sidecar::parse_sidecar(&metadata.to_string()).unwrap(),
    )
    .unwrap();
    let metadata: Value = serde_json::from_str(&canonical).unwrap();
    env.write_text("charters/.work.json", &canonical);
    run(&env, "archive", ROOT, &[]);
    assert!(active(&env).is_empty());
    let moved: Value = serde_json::from_slice(
        &fs::read(env.data_dir.join("charters/.work.completed.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(moved, metadata);
    run(&env, "unarchive", ROOT, &[]);
    assert_eq!(active(&env), before);
    let restored: Value =
        serde_json::from_slice(&fs::read(env.data_dir.join("charters/.work.json")).unwrap())
            .unwrap();
    assert_eq!(restored, metadata);
}

#[test]
fn archive_and_unarchive_children_name_root_and_reopen_archived_refuses_cleanly() {
    let env = setup("[x]", "[x]");
    run(&env, "complete", GRAND, &[]);
    env.command()
        .args(["archive", "action", CHILD])
        .assert()
        .failure()
        .stderr(predicate::str::contains(ROOT));
    run(&env, "archive", ROOT, &[]);
    for (verb, flags) in [
        ("unarchive", vec![]),
        ("reopen", vec![]),
        ("reopen", vec!["--cascade"]),
    ] {
        let assertion = env
            .command()
            .args([verb, "action", CHILD])
            .args(flags)
            .assert()
            .failure();
        let output = assertion.get_output();
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(text.contains(ROOT), "{text}");
        if verb == "reopen" {
            assert!(text.contains("unarchive"), "{text}");
        }
    }
}

#[test]
fn archive_charter_sweeps_closed_lines_to_completed_file() {
    let env = TestEnv::new();
    env.write_text(
        "charters/work.md",
        &format!("---\nid: {ROOT}\nalias: work\nstate: Closed\n---\n# Work\n"),
    );
    env.write_actions("work.actions", &format!("[x] Finished #{CHILD}\n"));
    env.command()
        .args(["archive", "charter", "work"])
        .assert()
        .success();
    assert!(
        read_actions(&env.data_dir.join(format!("archive/{ROOT}.actions")))
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        read_actions(
            &env.data_dir
                .join(format!("archive/{ROOT}.completed.actions"))
        )
        .unwrap()[0]
            .id
            .to_string(),
        CHILD
    );
}

#[test]
fn orient_reads_active_completions_and_releases_successors() {
    let env = TestEnv::new();
    env.write_text(
        "charters/README.md",
        "---\nalias: testws\nstate: Active\n---\n# Testws\n",
    );
    env.write_text(
        "charters/work.md",
        "---\nid: 01951111-0000-7000-8000-000000000099\nalias: work\nstate: Active\n---\n# Work\n",
    );
    env.write_actions(
        "work.actions",
        &format!("[x] Predecessor %2026-11-01T10:00 #{ROOT}\n[ ] Successor <{ROOT} #{CHILD}\n"),
    );
    let output = env
        .command()
        .arg("orient")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let value: Value = serde_json::from_slice(&output).unwrap();
    assert!(
        value["recent_completions"]["items"]
            .as_array()
            .unwrap()
            .iter()
            .any(|a| a["name"] == "Predecessor")
    );
    assert!(
        value["unscheduled"]["items"]
            .as_array()
            .unwrap()
            .iter()
            .any(|a| a["name"] == "Successor"),
        "{value}"
    );
}

#[test]
fn occurrence_closed_in_place_keeps_plan_link_through_archive() {
    let env = TestEnv::new();
    env.write_actions("health.actions", "");
    env.write_text("plans/health/run.ics", "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:run@example.com\r\nSUMMARY:Run\r\nDTSTART:20260101T080000Z\r\nRRULE:FREQ=DAILY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n");
    clearhead_cli::filesystem::sync_calendar(&env.data_dir, None).unwrap();
    let path = env.data_dir.join("charters/health.actions");
    let token = read_actions(&path).unwrap()[0].clone();
    env.command()
        .arg("transact")
        .write_stdin(format!(
            r#"{{"operations":[{{"op":"complete-action","target":"urn:uuid:{}"}}]}}"#,
            token.id
        ))
        .assert()
        .success();
    assert!(
        read_actions(&path)
            .unwrap()
            .iter()
            .any(|a| a.id == token.id && a.state == ActionState::Completed)
    );
    let sidecar_path = env.data_dir.join("charters/.health.json");
    let sidecar: Value = serde_json::from_slice(&fs::read(sidecar_path).unwrap()).unwrap();
    let record = &sidecar["actions"][token.id.to_string()]["occurrence"];
    assert_eq!(record["plan_uid"], "run@example.com");
    assert!(record["occurrence_key"].is_string());
    env.command()
        .args(["archive", "action", &token.id.to_string()])
        .assert()
        .success();
    let moved: Value = serde_json::from_slice(
        &fs::read(env.data_dir.join("charters/.health.completed.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(
        &moved["actions"][token.id.to_string()]["occurrence"],
        record
    );
    assert!(
        fs::read_to_string(env.data_dir.join("plans/health/run.ics"))
            .unwrap()
            .contains("RECURRENCE-ID")
    );
}

#[test]
fn unarchive_reopen_flags_are_composable() {
    for cascade in [false, true] {
        let env = setup("[x]", "[_]");
        run(&env, "complete", GRAND, &[]);
        let before = active(&env);
        run(&env, "archive", ROOT, &[]);
        env.command()
            .args(["unarchive", "action", ROOT, "--cascade"])
            .assert()
            .failure();
        let flags = if cascade {
            vec!["--reopen", "--cascade"]
        } else {
            vec!["--reopen"]
        };
        run(&env, "unarchive", ROOT, &flags);
        let after = active(&env);
        assert_eq!(after[0].state, ActionState::NotStarted);
        if cascade {
            assert!(
                after
                    .iter()
                    .all(|a| a.state == ActionState::NotStarted && a.completed_at.is_none())
            );
        } else {
            assert_eq!(&after[1..], &before[1..]);
        }
    }
}
