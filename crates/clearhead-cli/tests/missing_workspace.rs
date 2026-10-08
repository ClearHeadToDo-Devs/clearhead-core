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
        r#"{"additional_workspaces":["../../missing/nested","../../missing/nested","../../other"]}"#,
    )
    .unwrap();
    let other = env._temp_dir.path().join("other/.clearhead/charters");
    fs::create_dir_all(&other).unwrap();
    let read_id = "019e0000-0000-7000-0000-000000000003";
    let update_id = "019e0000-0000-7000-0000-000000000004";
    let complete_id = "019e0000-0000-7000-0000-000000000005";
    let delete_id = "019e0000-0000-7000-0000-000000000006";
    fs::write(
        other.join("next.actions"),
        format!(
            "[ ] sibling task #{read_id}\n[ ] update sibling #{update_id}\n[ ] complete sibling #{complete_id}\n[ ] delete sibling #{delete_id}\n"
        ),
    )
    .unwrap();
    let warning = "Skipping missing additional_workspaces entry '../../missing/nested'";
    let primary_file = project.join("charters/next.actions");
    let primary_file = primary_file.to_str().unwrap();

    for args in [vec!["show", "action", read_id], vec!["read", "actions"]] {
        env.command()
            .args(args)
            .assert()
            .success()
            .stderr(predicate::str::contains(warning).count(1))
            .stdout(predicate::str::contains("sibling task"));
    }

    for (args, id) in [
        (vec!["update", "action", update_id, "-p", "1"], update_id),
        (vec!["complete", "action", complete_id], complete_id),
        (vec!["delete", "action", delete_id], delete_id),
    ] {
        env.command()
            .args(args)
            .assert()
            .success()
            .stderr(predicate::str::contains(warning).count(1));
        let active = fs::read_to_string(other.join("next.actions")).unwrap();
        if id == update_id {
            let line = active.lines().find(|line| line.contains(id)).unwrap();
            assert!(line.contains("!1"), "{line}");
        } else {
            assert!(!active.contains(id));
            let completed = fs::read_to_string(other.join("next.completed.actions")).unwrap();
            assert_eq!(completed.contains(id), id == complete_id);
        }
        assert!(!env._temp_dir.path().join("missing").exists());
    }

    env.command()
        .args(["add", "action", "primary task", "--file", primary_file])
        .assert()
        .success()
        .stderr(predicate::str::contains(warning).count(1));
    assert!(
        fs::read_to_string(primary_file)
            .unwrap()
            .contains("primary task")
    );
    env.command()
        .args(["read", "actions"])
        .assert()
        .success()
        .stdout(predicate::str::contains("sibling task"))
        .stdout(predicate::str::contains("primary task"))
        .stderr(predicate::str::contains(warning).count(1));

    for args in [vec!["doctor", "--json"], vec!["doctor", "--fix"]] {
        env.command()
            .args(args)
            .assert()
            .code(1)
            .stderr(predicate::str::contains(warning).count(1))
            .stdout(predicate::str::contains("missing-additional-workspace").count(1))
            .stdout(predicate::str::contains("../../missing/nested"));
        assert!(!env._temp_dir.path().join("missing").exists());
    }

    env.command()
        .args(["doctor", "--fix", "--dry-run"])
        .assert()
        .success()
        .stderr(predicate::str::contains(warning).count(1))
        .stdout(predicate::str::contains(
            "Nothing for doctor --fix to repair.",
        ))
        .stdout(predicate::str::contains("checked ").not())
        .stdout(predicate::str::contains("missing-additional-workspace").not());
}

#[test]
fn dry_run_with_stale_workspace_and_fixable_finding_only_previews_repairs() {
    let env = TestEnv::new();
    env.write_config(r#"{"additional_workspaces":["../../missing"]}"#);
    env.write_text("charters/.gone.json", r#"{"actions":{}}"#);
    env.command()
        .args(["doctor", "--fix", "--dry-run"])
        .assert()
        .success()
        .stderr(predicate::str::contains("entry '../../missing'").count(1))
        .stdout(predicate::str::contains("Would remove orphaned sidecar"))
        .stdout(predicate::str::contains("Dry run:"))
        .stdout(predicate::str::contains("checked ").not())
        .stdout(predicate::str::contains("missing-additional-workspace").not());
    assert!(env.data_dir.join("charters/.gone.json").is_file());
    assert!(!env._temp_dir.path().join("missing").exists());
}

#[test]
fn non_directory_workspace_is_missing_and_resolved_duplicates_warn_once() {
    let env = TestEnv::new();
    let file = env.config_dir.join("not-a-directory");
    fs::write(&file, "not a workspace").unwrap();
    env.write_config(
        &serde_json::json!({"additional_workspaces": ["not-a-directory", file.to_str().unwrap()]})
            .to_string(),
    );
    env.command()
        .args(["read", "actions"])
        .assert()
        .success()
        .stderr(predicate::str::contains("Skipping missing additional_workspaces entry").count(1));
    env.command()
        .args(["doctor", "--json"])
        .assert()
        .code(1)
        .stderr(predicate::str::contains("Skipping missing additional_workspaces entry").count(1))
        .stdout(predicate::str::contains("missing-additional-workspace").count(1))
        .stdout(predicate::str::contains("not-a-directory"));
    assert_eq!(fs::read_to_string(file).unwrap(), "not a workspace");
}

#[test]
fn stale_global_config_warns_even_without_workspace_fanout() {
    let env = TestEnv::new();
    env.write_config(r#"{"additional_workspaces":["../../missing"]}"#);
    env.command()
        .args(["debug"])
        .assert()
        .success()
        .stderr(predicate::str::contains("entry '../../missing'").count(1));
    assert!(!env._temp_dir.path().join("missing").exists());
}
