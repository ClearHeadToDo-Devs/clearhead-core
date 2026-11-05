mod common;
use common::TestEnv;

#[test]
fn parent_roles_and_directory_charters_are_ordinary_paths() {
    for parent in ["archive", "templates"] {
        for charter in ["home", "archive/next", "templates/next"] {
            let mut env = TestEnv::new();
            env.work_dir = env.work_dir.join(parent).join("ws");
            env.data_dir = env.work_dir.join(".clearhead");
            env.write_text("workspace.json", "{}");
            let action = format!("charters/{charter}.actions");
            let habit = format!("charters/{charter}.habits.actions");
            env.write_text(&action, "[ ] Task #01951111-0000-7000-8000-000000000010\n");
            env.write_text(
                &habit,
                "[ ] Habit @2026-11-05 R:FREQ=WRONG #01951111-0000-7000-8000-000000000011\n",
            );
            env.command()
                .args([
                    "update",
                    "action",
                    "01951111-0000-7000-8000-000000000010",
                    "--file",
                    env.data_dir.join(&action).to_str().unwrap(),
                    "--name",
                    "Updated",
                ])
                .assert()
                .success();
            env.command()
                .args(["lint", "file", env.data_dir.join(&action).to_str().unwrap()])
                .assert()
                .success();
            env.command()
                .args(["lint", "file", env.data_dir.join(&habit).to_str().unwrap()])
                .assert()
                .failure()
                .stdout(predicates::str::contains("E002"))
                .stdout(predicates::str::contains("habit-scope").not());
            let model = clearhead_cli::filesystem::load_domain_model(&env.data_dir).unwrap();
            assert_eq!(model.charters.iter().flat_map(|c| &c.habits).count(), 1);
            assert_eq!(model.all_actions().len(), 1);
        }
    }
}
use predicates::prelude::*;

#[test]
fn only_workspace_root_directories_are_roles() {
    use clearhead_core::workspace::action_files::{
        is_action_history, is_live_actions, require_action_resource,
    };
    for path in [
        "charters/archive/next.actions",
        "charters/templates/next.actions",
        "charters/home.actions",
    ] {
        assert!(is_live_actions(std::path::Path::new(path)));
        assert!(require_action_resource(std::path::Path::new(path)).is_ok());
    }
    assert!(!is_live_actions(std::path::Path::new(
        "archive/home.actions"
    )));
    assert!(!is_live_actions(std::path::Path::new(
        "templates/home.actions"
    )));
    assert!(require_action_resource(std::path::Path::new("templates/home.actions")).is_err());
    assert!(is_action_history(std::path::Path::new(
        "archive/home.completed.actions"
    )));
    assert!(!is_action_history(std::path::Path::new(
        "templates/home.completed.actions"
    )));
}
