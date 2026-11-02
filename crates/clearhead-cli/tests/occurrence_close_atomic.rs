//! Recurring resolution is part of the same speculative fold as closure.
mod common;
use clearhead_cli::filesystem::{read_actions, sync_calendar};
use common::TestEnv;
use serde_json::{Value, json};
use std::fs;

fn setup() -> TestEnv {
    let env = TestEnv::new();
    env.write_actions("health.actions", "");
    for uid in ["run", "walk"] {
        env.write_text(&format!("plans/health/{uid}.ics"), &format!("BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{uid}@example.com\r\nSUMMARY:{uid}\r\nDTSTART:20260101T080000Z\r\nRRULE:FREQ=DAILY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"));
    }
    sync_calendar(&env.data_dir, None).unwrap();
    env
}

#[test]
fn multiple_occurrences_resolve_once_in_one_ordered_batch() {
    let env = setup();
    let path = env.data_dir.join("charters/health.actions");
    let tokens = read_actions(&path).unwrap();
    let operations: Vec<_> = tokens
        .iter()
        .map(|a| json!({"op":"complete-action", "target":format!("urn:uuid:{}", a.id)}))
        .collect();
    env.command()
        .arg("transact")
        .write_stdin(json!({"operations": operations}).to_string())
        .assert()
        .success();
    let after = read_actions(&path).unwrap();
    assert_eq!(after.len(), 4);
    for token in tokens {
        assert!(
            after
                .iter()
                .any(|a| a.id == token.id && a.state.is_terminal())
        );
        let sidecar: Value =
            serde_json::from_slice(&fs::read(env.data_dir.join("charters/.health.json")).unwrap())
                .unwrap();
        assert!(sidecar["actions"][token.id.to_string()]["occurrence"].is_object());
        let plan = env
            .data_dir
            .join(format!("plans/health/{}.ics", token.name));
        let bytes = fs::read(&plan).unwrap();
        let metadata = fs::metadata(&plan).unwrap().modified().unwrap();
        env.command()
            .args(["cancel", "action", &token.id.to_string(), "--cascade"])
            .assert()
            .success();
        assert_eq!(fs::read(&plan).unwrap(), bytes);
        assert_eq!(fs::metadata(&plan).unwrap().modified().unwrap(), metadata);
    }
}

#[test]
fn rejected_and_dry_run_closes_write_no_occurrence_resources() {
    for dry_run in [false, true] {
        let env = setup();
        let paths = [
            "charters/health.actions",
            "charters/.health.json",
            "plans/health/run.ics",
            "plans/health/walk.ics",
            "sync/plans.json",
        ];
        let before: Vec<_> = paths
            .iter()
            .map(|p| fs::read(env.data_dir.join(p)).unwrap())
            .collect();
        let id = read_actions(&env.data_dir.join(paths[0])).unwrap()[0].id;
        let mut operations =
            vec![json!({"op":"complete-action", "target":format!("urn:uuid:{id}")})];
        if !dry_run {
            operations.push(json!({"op":"complete-action", "target":"urn:uuid:01951111-0000-7000-8000-000000000099"}));
        }
        let mut command = env.command();
        command.arg("transact");
        if dry_run {
            command.arg("--dry-run");
        }
        let assertion = command
            .write_stdin(json!({"operations":operations}).to_string())
            .assert();
        if dry_run {
            assertion.success();
        } else {
            assertion.failure();
        }
        for (path, bytes) in paths.iter().zip(before) {
            assert_eq!(fs::read(env.data_dir.join(path)).unwrap(), bytes, "{path}");
        }
    }
}
