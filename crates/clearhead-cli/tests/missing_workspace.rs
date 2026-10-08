mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::fs;

#[test]
fn stale_workspace_is_skipped_by_action_verbs_and_reported_by_doctor() {
    let env = TestEnv::new();
    let project = env.work_dir.join(".clearhead");
    fs::create_dir_all(project.join("charters")).unwrap();
    fs::write(project.join("charters/next.actions"), "").unwrap();
    fs::write(
        project.join("config.json"),
        r#"{"additional_workspaces":["../../missing/nested","../../other"]}"#,
    )
    .unwrap();
    let other = env._temp_dir.path().join("other/.clearhead/charters");
    fs::create_dir_all(&other).unwrap();
    let id = "019e0000-0000-7000-0000-000000000003";
    fs::write(
        other.join("next.actions"),
        format!("[ ] sibling task #{id}\n"),
    )
    .unwrap();
    let warning = "Skipping missing additional_workspaces entry '../../missing/nested'";
    let primary_file = project.join("charters/next.actions");
    let primary_file = primary_file.to_str().unwrap();

    for args in [
        vec!["show", "action", id],
        vec!["update", "action", id, "-p", "1"],
        vec!["complete", "action", id],
        vec!["delete", "action", id],
        vec!["add", "action", "primary task", "--file", primary_file],
        vec!["read", "actions"],
    ] {
        env.command()
            .args(args)
            .assert()
            .success()
            .stderr(predicate::str::contains(warning));
        assert!(!env._temp_dir.path().join("missing").exists());
    }
    assert!(
        !fs::read_to_string(other.join("next.actions"))
            .unwrap()
            .contains(id)
    );
    assert!(
        !fs::read_to_string(other.join("next.completed.actions"))
            .unwrap()
            .contains(id)
    );

    for args in [
        vec!["doctor", "--json"],
        vec!["doctor", "--fix"],
        vec!["doctor", "--fix", "--dry-run"],
    ] {
        env.command()
            .args(args)
            .assert()
            .code(1)
            .stderr(predicate::str::contains(warning))
            .stdout(predicate::str::contains("missing-additional-workspace"))
            .stdout(predicate::str::contains("../../missing/nested"));
        assert!(!env._temp_dir.path().join("missing").exists());
    }
}

#[test]
fn stale_global_config_warns_even_without_workspace_fanout() {
    let env = TestEnv::new();
    env.write_config(r#"{"additional_workspaces":["../../missing"]}"#);
    env.command()
        .args(["debug"])
        .assert()
        .success()
        .stderr(predicate::str::contains("entry '../../missing'"));
    assert!(!env._temp_dir.path().join("missing").exists());
}
