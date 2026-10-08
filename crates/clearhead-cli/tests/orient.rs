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

const BETA_MD: &str = "\
---
id: 01951111-0000-7000-0000-0000000000b0
alias: beta
state: Blocked
---
# Beta
";

#[test]
fn orient_reports_bounded_sections_with_omission_counts() {
    let env = TestEnv::new();
    env.write_text("charters/alpha.md", ALPHA_MD);
    env.write_actions(
        "alpha.actions",
        "[ ] Open work #01951111-0000-7000-0000-0000000000a1\n\
         [=] Waiting on something external #01951111-0000-7000-0000-0000000000a2\n",
    );

    env.write_text("charters/beta.md", BETA_MD);
    env.write_actions("beta.actions", "");

    let completed: String = (0..12)
        .map(|i| {
            format!(
                "[x] Completed {i} %2026-01-{day:02}T09:00 #01951111-0000-7000-0000-0000000001{i:02}\n",
                day = i + 1,
            )
        })
        .collect();
    env.write_actions("alpha.completed.actions", &completed);

    let assert = env.command().arg("orient").assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let value: serde_json::Value = serde_json::from_str(&stdout).expect("orient emits JSON");

    let active_charters = &value["active_charters"];
    let active_titles: Vec<&str> = active_charters["items"]
        .as_array()
        .unwrap()
        .iter()
        .map(|item| item["title"].as_str().unwrap())
        .collect();
    assert!(
        active_titles.contains(&"Alpha"),
        "Alpha is Active: {active_titles:?}"
    );
    assert!(
        !active_titles.contains(&"Beta"),
        "Beta is Blocked, not Active: {active_titles:?}"
    );
    assert_eq!(active_charters["omitted"], 0);

    let blockers = value["blockers"]["items"].as_array().unwrap();
    assert_eq!(blockers.len(), 2, "beta charter + the one blocked action");
    assert!(
        blockers
            .iter()
            .any(|b| b["kind"] == "charter" && b["title"] == "Beta")
    );
    assert!(
        blockers
            .iter()
            .any(|b| b["kind"] == "action" && b["name"] == "Waiting on something external")
    );

    let recent = &value["recent_completions"];
    let recent_items = recent["items"].as_array().unwrap();
    assert_eq!(recent_items.len(), 10, "bounded to the section limit");
    assert_eq!(recent["omitted"], 2, "12 completions, 10 shown");
    assert_eq!(
        recent_items[0]["name"], "Completed 11",
        "sorted most-recent first"
    );
    assert_eq!(
        recent_items[9]["name"], "Completed 2",
        "10th newest, matching the descending sort"
    );

    // The unscheduled section is always present, even if empty in this fixture
    // (no query-eligible actions were set up here) — it's part of the shape.
    assert!(value["unscheduled"]["items"].is_array());
    assert!(value["unscheduled"]["omitted"].is_number());
}

#[test]
fn orient_renders_human_readable_at_a_terminal_shape() {
    // Piped output (the default for assert_cmd) is JSON; this only proves the
    // command succeeds and produces some structured payload with all four
    // sections. The terminal-render branch is exercised via manual/pty use.
    let env = TestEnv::new();
    env.write_actions(
        "work.actions",
        "[ ] Something #01951111-0000-7000-0000-000000000c01\n",
    );

    let assert = env.command().arg("orient").assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let value: serde_json::Value = serde_json::from_str(&stdout).unwrap();

    for section in [
        "active_charters",
        "unscheduled",
        "blockers",
        "recent_completions",
    ] {
        assert!(value[section]["items"].is_array(), "missing {section}");
        assert!(
            value[section]["omitted"].is_number(),
            "missing {section} omitted count"
        );
    }
}

fn multi_workspace_env() -> TestEnv {
    let env = TestEnv::new();
    env.write_text("charters/alpha.md", ALPHA_MD);
    env.write_actions(
        "alpha.actions",
        "[=] Primary blocker #01951111-0000-7000-0000-0000000000a2\n",
    );
    env.write_actions(
        "alpha.completed.actions",
        "[x] Primary completion %2026-01-01T09:00 #01951111-0000-7000-0000-0000000000a3\n",
    );

    let second = env.work_dir.join("second");
    std::fs::create_dir_all(second.join("charters")).unwrap();
    for (path, content) in [
        (
            "alpha.md",
            ALPHA_MD
                .replace("Alpha", "Secondary Alpha")
                .replace("0000000000a0", "0000000000c0"),
        ),
        ("beta.md", BETA_MD.to_owned()),
        (
            "alpha.actions",
            "[=] Secondary blocker #01951111-0000-7000-0000-0000000000b2\n".to_owned(),
        ),
        ("beta.actions", String::new()),
        (
            "alpha.completed.actions",
            "[x] Secondary completion %2026-02-01T09:00 #01951111-0000-7000-0000-0000000000b3\n"
                .to_owned(),
        ),
    ] {
        std::fs::write(second.join("charters").join(path), content).unwrap();
    }
    env.write_config(&format!(
        r#"{{"additional_workspaces":["{}"]}}"#,
        second.display()
    ));
    env
}

#[test]
fn orient_aggregates_every_loaded_workspace() {
    let env = multi_workspace_env();
    let result = env.command().arg("orient").assert().success();
    let value: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    let titles: Vec<_> = value["active_charters"]["items"]
        .as_array()
        .unwrap()
        .iter()
        .map(|item| item["title"].as_str().unwrap())
        .collect();
    assert_eq!(titles, ["Alpha", "Secondary Alpha"]);
    let blockers = value["blockers"]["items"].as_array().unwrap();
    assert_eq!(blockers.len(), 3);
    assert!(
        blockers
            .iter()
            .any(|item| item["name"] == "Primary blocker")
    );
    assert!(
        blockers
            .iter()
            .any(|item| item["name"] == "Secondary blocker")
    );
    assert!(blockers.iter().any(|item| item["title"] == "Beta"));
    let completions = value["recent_completions"]["items"].as_array().unwrap();
    assert_eq!(completions.len(), 2);
    assert_eq!(completions[0]["name"], "Secondary completion");
    assert_eq!(completions[1]["name"], "Primary completion");
}

#[test]
fn orient_skips_broken_secondary_completed_file() {
    for content in [b"not valid actions syntax !!!\n".as_slice(), &[0xff]] {
        let env = multi_workspace_env();
        let path = env.work_dir.join("second/charters/alpha.completed.actions");
        std::fs::write(&path, content).unwrap();

        let result = env
            .command()
            .env("RUST_LOG", "warn")
            .arg("orient")
            .assert()
            .success();
        let value: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
        assert_eq!(
            value["active_charters"]["items"].as_array().unwrap().len(),
            2
        );
        assert_eq!(value["blockers"]["items"].as_array().unwrap().len(), 3);
        let completions = value["recent_completions"]["items"].as_array().unwrap();
        assert_eq!(completions.len(), 1);
        assert_eq!(completions[0]["name"], "Primary completion");
        let stderr = String::from_utf8_lossy(&result.get_output().stderr);
        assert!(stderr.contains("Skipping completed actions"), "{stderr}");
        assert!(stderr.contains(&path.display().to_string()), "{stderr}");
    }
}

#[test]
fn orient_fails_on_broken_primary_completed_file() {
    let env = multi_workspace_env();
    env.write_actions("alpha.completed.actions", "not valid actions syntax !!!\n");
    env.command().arg("orient").assert().failure();
}

#[test]
fn orient_workspace_filter_applies_to_every_section() {
    let env = multi_workspace_env();
    let result = env
        .command()
        .args(["orient", "--workspace", "second"])
        .assert()
        .success();
    let value: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    assert_eq!(
        value["active_charters"]["items"].as_array().unwrap().len(),
        1
    );
    assert_eq!(
        value["active_charters"]["items"][0]["title"],
        "Secondary Alpha"
    );
    assert_eq!(value["blockers"]["items"].as_array().unwrap().len(), 2);
    assert_eq!(
        value["recent_completions"]["items"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        value["recent_completions"]["items"][0]["name"],
        "Secondary completion"
    );

    let result = env
        .command()
        .args(["orient", "--workspace", "no-such-workspace"])
        .assert()
        .success();
    let value: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    for section in [
        "active_charters",
        "unscheduled",
        "blockers",
        "recent_completions",
    ] {
        assert_eq!(value[section]["items"], serde_json::json!([]));
        assert_eq!(value[section]["omitted"], 0);
    }
}

#[test]
fn orient_bounds_sections_after_combining_workspaces() {
    let env = TestEnv::new();
    let second = env.work_dir.join("second");
    std::fs::create_dir_all(second.join("charters")).unwrap();
    env.write_config(&format!(
        r#"{{"additional_workspaces":["{}"]}}"#,
        second.display()
    ));
    for (index, root) in [&env.data_dir, &second].into_iter().enumerate() {
        std::fs::create_dir_all(root.join("charters")).unwrap();
        for i in 0..6 {
            let stem = root.join("charters").join(format!("charter-{i}"));
            std::fs::write(
                stem.with_extension("md"),
                format!("---\nstate: Active\n---\n# Charter {index}-{i}\n"),
            )
            .unwrap();
            std::fs::write(
                stem.with_extension("actions"),
                format!("[=] Blocker {index}-{i} #01951111-0000-7000-0000-000000000{index}{i}1\n"),
            )
            .unwrap();
            std::fs::write(
                stem.with_extension("completed.actions"),
                format!("[x] Completion {index}-{i} %2026-0{month}-0{day}T09:00 #01951111-0000-7000-0000-000000000{index}{i}2\n", month = index + 1, day = i + 1),
            )
            .unwrap();
        }
    }
    let result = env.command().arg("orient").assert().success();
    let value: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    for section in ["active_charters", "blockers", "recent_completions"] {
        assert_eq!(value[section]["items"].as_array().unwrap().len(), 10);
        assert_eq!(value[section]["omitted"], 2);
    }
    assert_eq!(
        value["recent_completions"]["items"][0]["name"],
        "Completion 1-5"
    );
    assert_eq!(
        value["recent_completions"]["items"][9]["name"],
        "Completion 0-2"
    );
}

#[test]
fn orient_loads_the_workspace_once() {
    // Every load notes a violation once, so one notice line means one load:
    // every section reads the same snapshot.
    let env = TestEnv::new();
    env.write_actions("work.actions", "[ ] Something\n");
    env.write_text("charters/broken.actions", "not valid actions syntax !!!\n");

    let assert = env.command().arg("orient").assert().success();
    let stderr = String::from_utf8(assert.get_output().stderr.clone()).unwrap();
    assert_eq!(
        stderr.matches("workspace violation").count(),
        1,
        "stderr: {stderr}"
    );
}
