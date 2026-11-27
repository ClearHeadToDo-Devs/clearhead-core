mod common;

use std::fs;

use clearhead_cli::filesystem::habits::{apply_habits, fast_forward};
use clearhead_core::domain::habit_slots::occurrence_id;
use common::{TestEnv, snapshot_tree};
use predicates::str::contains;

const HABIT: &str = "01951111-0000-7000-8000-000000000011";
const OTHER: &str = "01951111-0000-7000-8000-000000000012";
const SIDECAR: &str = "charters/.review.json";
const SOURCE: &str = "review.habits.actions";

#[test]
fn cli_fast_forward_previews_and_skips_only_the_selected_habit() {
    let env = TestEnv::new();
    env.write_actions(SOURCE, &format!(
        "[ ] Test recurrence =test @2020-01-01 R:FREQ=DAILY;COUNT=101 #{HABIT}\n[ ] Other =other @2020-01-01 R:FREQ=DAILY;COUNT=1 #{OTHER}\n"
    ));
    let before = snapshot_tree(&env.data_dir);
    let skip = ["apply", "habits", "--fast-forward", "test"];
    env.command()
        .args(skip)
        .arg("--dry-run")
        .assert()
        .success()
        .stdout(contains("Would skip 101 due slot(s)"))
        .stdout(contains("No future slots"));
    assert_eq!(snapshot_tree(&env.data_dir), before);

    env.command()
        .args(skip)
        .assert()
        .success()
        .stdout(contains("Skipped 101 due slot(s)"));
    assert!(!env.data_dir.join("charters/review.actions").exists());
    let skipped = snapshot_tree(&env.data_dir);
    env.command()
        .args(skip)
        .assert()
        .success()
        .stdout(contains("Skipped 0 due slot(s)"));
    assert_eq!(snapshot_tree(&env.data_dir), skipped);

    env.command()
        .args(["apply", "habits"])
        .assert()
        .success()
        .stdout(contains("Materialized 1 Habit occurrence(s)"));
    let rows = fs::read_to_string(env.data_dir.join("charters/review.actions")).unwrap();
    assert!(rows.contains("Other"));
    assert!(!rows.contains("Test recurrence"));
}

#[test]
fn exclusions_preserve_history_and_do_not_materialize_the_next_slot_early() {
    let env = TestEnv::new();
    let definition = format!("[ ] Review @2026-11-05T09:00+01:00 R:FREQ=DAILY;COUNT=5 #{HABIT}\n");
    env.write_actions(SOURCE, &definition);
    let key = |day: u8| format!("U:2026-11-{day:02}T08:00:00Z");
    let slot_id = |day| occurrence_id(HABIT.parse().unwrap(), &key(day).parse().unwrap());
    let live = format!("[ ] Edited live occurrence #{}\n", slot_id(5));
    let archived = format!("[x] Finished earlier %2026-11-06 #{}\n", slot_id(6));
    env.write_actions("review.actions", &live);
    env.write_actions("review.completed.actions", &archived);
    env.write_text(SIDECAR, &format!(r#"{{"habits":{{"{HABIT}":{{"frame":"UTC","exdate":["EXDATE:20261108T080000Z"],"keep":42}}}}}}"#));

    let cutoff = "2026-11-07T09:00:00+01:00".parse().unwrap();
    let result = fast_forward(&env.data_dir, HABIT, cutoff, false).unwrap();
    assert_eq!(result.skipped, 1); // Exact boundary is due, not future.
    assert_eq!(result.next_slot.unwrap().as_str(), key(9));
    assert_eq!(
        apply_habits(&env.data_dir, cutoff, 10, false)
            .unwrap()
            .occurrences,
        0
    );
    let meta: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(env.data_dir.join(SIDECAR)).unwrap()).unwrap();
    assert_eq!(
        meta["habits"][HABIT]["exdate"],
        serde_json::json!(["EXDATE:20261108T080000Z", "EXDATE:20261107T080000Z"])
    );
    assert_eq!(meta["habits"][HABIT]["keep"], 42);
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters/review.actions")).unwrap(),
        live
    );
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters/review.completed.actions")).unwrap(),
        archived
    );
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters").join(SOURCE)).unwrap(),
        definition
    );

    let later = "2026-11-09T08:00:00Z".parse().unwrap();
    assert_eq!(
        apply_habits(&env.data_dir, later, 10, false)
            .unwrap()
            .occurrences,
        1
    );
    let rows = fs::read_to_string(env.data_dir.join("charters/review.actions")).unwrap();
    assert!(rows.starts_with(&live));
    assert!(rows.contains(&slot_id(9).to_string()));
}

#[test]
fn editing_rule_can_introduce_due_slots_that_fast_forward_never_skipped() {
    let env = TestEnv::new();
    let defining =
        |interval| format!("[ ] Review @2026-11-05 R:FREQ=DAILY;INTERVAL={interval} #{HABIT}\n");
    env.write_actions(SOURCE, &defining(2));
    let now = "2026-11-06T12:00:00Z".parse().unwrap();
    assert_eq!(
        fast_forward(&env.data_dir, HABIT, now, false)
            .unwrap()
            .skipped,
        1
    );
    env.write_actions(SOURCE, &defining(1));
    assert_eq!(
        apply_habits(&env.data_dir, now, 10, false)
            .unwrap()
            .occurrences,
        1
    );
    let model = clearhead_cli::filesystem::load_domain_model(&env.data_dir).unwrap();
    let actions = model.all_actions();
    assert_eq!(actions.len(), 1);
    assert_eq!(
        actions[0]
            .native_occurrence
            .as_ref()
            .unwrap()
            .occurrence_key,
        "D:2026-11-06"
    );
}

#[test]
fn ambiguous_alias_cannot_discard_another_habits_backlog() {
    let env = TestEnv::new();
    env.write_actions(SOURCE, &format!(
        "[ ] First =test @2020-01-01 R:FREQ=DAILY;COUNT=1 #{HABIT}\n[ ] Second =test @2020-01-01 R:FREQ=DAILY;COUNT=1 #{OTHER}\n"
    ));
    let before = snapshot_tree(&env.data_dir);
    env.command()
        .args(["apply", "habits", "--fast-forward", "test"])
        .assert()
        .failure()
        .stderr(contains("Habit reference 'test' is ambiguous"));
    assert_eq!(snapshot_tree(&env.data_dir), before);
}
