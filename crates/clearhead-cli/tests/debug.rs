use common::TestEnv;

mod common;

const ALPHA_MD: &str = "\
---
id: 01951111-0000-7000-0000-0000000000a0
alias: alpha
state: Active
---
# Alpha
";

#[test]
fn debug_emits_json_when_piped() {
    let env = TestEnv::new();
    env.write_text("charters/alpha.md", ALPHA_MD);
    env.write_actions(
        "alpha.actions",
        "[ ] Something #01951111-0000-7000-0000-0000000000a1\n",
    );

    let assert = env.command().arg("debug").assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let value: serde_json::Value = serde_json::from_str(&stdout).expect("debug emits JSON");

    let config = &value["config"];
    assert!(config["global_config_file"]["path"].is_string());
    assert!(config["global_config_file"]["exists"].is_boolean());
    // Unset values are null, never a display placeholder.
    for key in ["data_dir", "config_dir"] {
        assert!(config[key].is_null() || config[key].is_string());
        assert_ne!(config[key], "<xdg-config-default>");
        assert_ne!(config[key], "<project-root-or-xdg-default>");
    }
    assert!(config["default_file"].is_string());
    assert!(config["additional_workspaces"].is_array());
    assert!(config["plan_component"].is_string());
    // No project root under a bare TestEnv work_dir: the two project file
    // entries are omitted from the JSON entirely, not emitted as null.
    assert!(config.get("project_config_file").is_none());
    assert!(config.get("project_local_config_file").is_none());

    let workspace = &value["workspace"];
    assert!(workspace["resolved_data_root"].is_string());
    assert!(workspace["resolution"].is_string());
    assert!(workspace["root_charter"].is_string() || workspace["root_charter"].is_null());
    assert_ne!(workspace["root_charter"], "-");
    let charters = workspace["charters"].as_array().unwrap();
    assert!(
        charters
            .iter()
            .any(|c| c["alias"] == "alpha" && c["source"] == "actions+markdown")
    );
    for entry in charters {
        assert!(entry["file"].is_string());
        assert!(entry["parent"].is_string() || entry["parent"].is_null());
        assert_ne!(entry["parent"], "-");
    }
    let summary = &workspace["graph_summary"];
    assert!(summary["charters"].as_u64().unwrap() >= 1);
    assert_eq!(summary["actions"], 1);
    assert!(summary["plans"].is_number());
    assert!(summary["violations"].is_number());
    assert!(summary["warnings"].is_number());

    // Rendering-only fields never leak into the JSON contract.
    assert!(workspace.get("plans_root").is_none());
    assert!(workspace.get("has_findings").is_none());
}

#[test]
fn debug_json_omits_workspace_identity_when_unset() {
    let env = TestEnv::new();
    env.write_actions(
        "work.actions",
        "[ ] Something #01951111-0000-7000-0000-000000000b01\n",
    );

    let assert = env.command().arg("debug").assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let value: serde_json::Value = serde_json::from_str(&stdout).unwrap();

    assert!(value["workspace"].get("workspace_id").is_none());
    assert!(value["workspace"].get("workspace_name").is_none());
}
