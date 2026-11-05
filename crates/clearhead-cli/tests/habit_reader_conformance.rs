//! Suffix roles are a workspace contract, not a grammar distinction.
mod common;
use clearhead_cli::filesystem::load_domain_model;
use common::TestEnv;
use std::fs;

const HABIT: &str = "[ ] Review @2026-11-05T08:00 R:FREQ=DAILY #01951111-0000-7000-8000-000000000010\n  >[ ] Template step #01951111-0000-7000-8000-000000000011\n";

#[test]
fn habit_reader_conformance_root_flat_directory_and_templates() {
    for anchor in ["next", "review", "project/next"] {
        let env = TestEnv::new();
        env.write_actions(
            &format!("{anchor}.actions"),
            "[ ] Live #01951111-0000-7000-8000-000000000020\n",
        );
        env.write_actions(&format!("{anchor}.habits.actions"), HABIT);
        env.write_actions(
            &format!("{anchor}.completed.actions"),
            "[x] History %2026-11-04 #01951111-0000-7000-8000-000000000030\n",
        );
        env.write_text("templates/named.actions", HABIT);
        env.write_text("archive/old.habits.actions", HABIT);
        env.write_text(
            "archive/old.actions",
            "[ ] Archived #01951111-0000-7000-8000-000000000040\n",
        );
        let model = load_domain_model(&env.data_dir).unwrap();
        assert_eq!(
            model.all_actions().len(),
            1,
            "{anchor}: only live executable work"
        );
        let habits: Vec<_> = model.charters.iter().flat_map(|c| &c.habits).collect();
        assert_eq!(habits.len(), 1, "{anchor}: exactly one native Habit");
        assert_eq!(habits[0].recurrence_rule, "FREQ=DAILY");
        assert_eq!(habits[0].children.len(), 1);
        assert!(
            !model.charters.iter().any(|c| c.title.contains("habits")),
            "no suffix-created charter"
        );
    }
}

#[test]
fn habit_reader_conformance_plain_rule_survives_cli_mutation() {
    let env = TestEnv::new();
    let source = "[ ] Review @2026-11-05T08:00 R:FREQ=WEEKLY;BYDAY=MO,WE #01951111-0000-7000-8000-000000000010\n";
    env.write_actions("review.actions", source);
    env.command()
        .args([
            "update",
            "action",
            "01951111-0000-7000-8000-000000000010",
            "--name",
            "Renamed",
        ])
        .assert()
        .success();
    let changed = fs::read_to_string(env.data_dir.join("charters/review.actions")).unwrap();
    assert_eq!(
        changed,
        source.replace("Review", "Renamed"),
        "ordinary mutation must not drop or rewrite the rule"
    );
}
