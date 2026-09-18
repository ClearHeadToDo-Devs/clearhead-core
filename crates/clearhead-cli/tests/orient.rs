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
