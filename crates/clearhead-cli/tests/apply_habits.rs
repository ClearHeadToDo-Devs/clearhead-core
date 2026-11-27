mod common;

use std::fs;

use clearhead_cli::filesystem::{habits::apply_habits, load_domain_model};
use clearhead_core::domain::habit_slots::{SlotKey, occurrence_id, step_id};
use clearhead_core::workspace::calendar::reconcile::plan_one_off_sync;
use clearhead_core::workspace::calendar::sync_store::PlansSyncStore;
use common::{TestEnv, snapshot_tree};
use predicates::{prelude::PredicateBooleanExt, str::contains};
use uuid::Uuid;

const HABIT: &str = "01951111-0000-7000-8000-000000000011";
const STEP: &str = "01951111-0000-7000-8000-000000000012";
const SECOND: &str = "01951111-0000-7000-8000-000000000013";
const ROOT: &str = "charters/review.actions";
const SIDECAR: &str = "charters/.review.json";
const DEFINITION: &str = "review.habits.actions";

fn slot(day: &str) -> Uuid {
    let key: SlotKey = format!("D:2026-11-{day}").parse().unwrap();
    occurrence_id(HABIT.parse().unwrap(), &key)
}

#[test]
fn cli_preview_and_apply_preserve_source_and_hydrate_lineage() {
    let env = TestEnv::new();
    let definition =
        format!("[=] Review $template: review$ @2020-11-05 R:FREQ=DAILY;COUNT=2 #{HABIT}\n");
    env.write_actions(DEFINITION, &definition);
    env.write_text(
        "templates/review.actions",
        &format!("[ ] Checklist @2020-11-05 #{STEP}\n"),
    );
    let original = format!("[ ]  Preserve this spacing #{SECOND}");
    env.write_actions("review.actions", &original);

    let before = snapshot_tree(&env.data_dir);
    env.command()
        .args(["apply", "habits", "--dry-run"])
        .assert()
        .success()
        .stdout(contains(
            "Would materialize 2 Habit occurrence(s) (4 action(s))",
        ));
    assert_eq!(snapshot_tree(&env.data_dir), before);

    env.command()
        .args(["apply", "habits"])
        .assert()
        .success()
        .stdout(contains("Materialized 2 Habit occurrence(s) (4 action(s))"));
    let actions = fs::read_to_string(env.data_dir.join(ROOT)).unwrap();
    assert!(actions.starts_with(&format!("{original}\n")));
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters").join(DEFINITION)).unwrap(),
        definition
    );
    assert!(!env.data_dir.join("plans").exists());
    let sidecar = fs::read_to_string(env.data_dir.join(SIDECAR)).unwrap();
    let model = load_domain_model(&env.data_dir).unwrap();
    let all_actions = model.all_actions();
    for day in ["05", "06"] {
        let key = format!("D:2020-11-{day}");
        let id = occurrence_id(HABIT.parse().unwrap(), &key.parse().unwrap());
        let root = all_actions.iter().find(|action| action.id == id).unwrap();
        let lineage = root.native_occurrence.as_ref().unwrap();
        assert_eq!(lineage.habit_id.to_string(), HABIT);
        assert_eq!(lineage.occurrence_key, key);
        assert_eq!(lineage.template.as_deref(), Some("review"));
        assert!(root.plan_id.is_none());
        let child_id = step_id(id, STEP.parse().unwrap());
        let child = all_actions
            .iter()
            .find(|action| action.id == child_id)
            .unwrap();
        assert_eq!(child.parent_id, Some(id));
    }
    let sync_store = PlansSyncStore::new(&env.data_dir.join("plans"));
    assert!(
        plan_one_off_sync(&model, &sync_store, &[])
            .unwrap()
            .entries
            .is_empty()
    );

    env.command()
        .args(["apply", "habits"])
        .assert()
        .success()
        .stdout(contains("Materialized 0 Habit occurrence(s)"));
    assert_eq!(
        fs::read_to_string(env.data_dir.join(ROOT)).unwrap(),
        actions
    );
    assert_eq!(
        fs::read_to_string(env.data_dir.join(SIDECAR)).unwrap(),
        sidecar
    );
}

#[test]
fn bounded_replay_resumes_after_sidecar_only_delivery_and_skips_both_archive_locations() {
    let env = TestEnv::new();
    env.write_actions(
        DEFINITION,
        &format!("[-] Review @2026-11-05 R:FREQ=DAILY;COUNT=6 #{HABIT}\n >[ ] Step #{STEP}\n"),
    );
    let existing = format!("[x] Completed earlier #{}\n", slot("05"));
    env.write_actions("other.completed.actions", &existing);
    // An archived row with recorded lineage also suppresses a slot even when
    // its row UUID is not the current deterministic occurrence UUID.
    let old = format!("[_] Cancelled earlier #{SECOND}\n");
    env.write_text("archive/nested/review.actions", &old);
    env.write_text("archive/nested/.review.json", &format!(r#"{{"actions":{{"{SECOND}":{{"occurrence":{{"habit_id":"{HABIT}","occurrence_key":"D:2026-11-06","plan_title":"Review"}}}}}}}}"#));
    env.write_text(SIDECAR, &format!(r#"{{"habits":{{"{HABIT}":{{"frame":"DATE","exdate":["EXDATE;VALUE=DATE:20261107"]}}}}}}"#));
    let now = "2026-11-09T11:00:00Z".parse().unwrap();
    let apply = || apply_habits(&env.data_dir, now, 1, false).unwrap();

    let first = apply();
    assert_eq!(
        (first.occurrences, first.actions, first.has_backlog),
        (1, 2, true)
    );
    let saved = fs::read_to_string(env.data_dir.join(ROOT)).unwrap();
    assert!(saved.contains(&slot("08").to_string()));
    assert!(!saved.contains(&slot("09").to_string()));
    // Reproduce the on-disk state after publishing just the sidecar.
    fs::remove_file(env.data_dir.join(ROOT)).unwrap();
    assert_eq!(apply().occurrences, 1);
    assert_eq!(fs::read_to_string(env.data_dir.join(ROOT)).unwrap(), saved);

    // Move a real occurrence tree and its metadata through the normal archive path.
    env.command()
        .args(["complete", "action", &slot("08").to_string(), "--cascade"])
        .assert()
        .success();
    env.command()
        .args(["archive", "action", &slot("08").to_string()])
        .assert()
        .success();
    let moved = env.data_dir.join("charters/review.completed.actions");
    let archived = fs::read_to_string(&moved).unwrap();
    let facts = clearhead_cli::filesystem::read_archived_action_facts(&env.data_dir).unwrap();
    assert!(
        facts
            .iter()
            .any(|fact| fact.action.id == slot("08") && fact.action.native_occurrence.is_some())
    );
    let last = apply();
    assert_eq!(
        (last.occurrences, last.actions, last.has_backlog),
        (1, 2, false)
    );
    let rows = fs::read_to_string(env.data_dir.join(ROOT)).unwrap();
    assert!(rows.contains(&slot("09").to_string()));
    for day in ["05", "06", "07", "08", "10"] {
        assert!(!rows.contains(&slot(day).to_string()));
    }
    assert_eq!(apply().occurrences, 0);
    assert_eq!(fs::read_to_string(moved).unwrap(), archived);
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters/other.completed.actions")).unwrap(),
        existing
    );
    assert_eq!(
        fs::read_to_string(env.data_dir.join("archive/nested/review.actions")).unwrap(),
        old
    );
}

#[test]
fn cli_reports_backlog_and_replays_the_remaining_slots() {
    let env = TestEnv::new();
    env.write_actions(
        DEFINITION,
        &format!("[ ] Review @2020-01-01 R:FREQ=DAILY;COUNT=101 #{HABIT}\n"),
    );
    env.command()
        .args(["apply", "habits"])
        .assert()
        .success()
        .stdout(
            contains("Materialized 100 Habit occurrence(s)")
                .and(contains("More Habit slots are due")),
        );
    env.command()
        .args(["apply", "habits"])
        .assert()
        .success()
        .stdout(
            contains("Materialized 1 Habit occurrence(s)")
                .and(contains("More Habit slots are due").not()),
        );
    assert_eq!(
        load_domain_model(&env.data_dir)
            .unwrap()
            .all_actions()
            .len(),
        101
    );
}

#[test]
fn calendar_only_schedule_does_not_silently_fall_back_to_the_text_rule() {
    let env = TestEnv::new();
    env.write_actions(
        DEFINITION,
        &format!("[ ] Review @2020-11-05T09:00 R:FREQ=DAILY;COUNT=1 #{HABIT}\n"),
    );
    env.write_text(
        SIDECAR,
        &format!(r#"{{"habits":{{"{HABIT}":{{"frame":"Europe/Paris"}}}}}}"#),
    );
    let before = snapshot_tree(&env.data_dir);
    env.command()
        .args(["apply", "habits"])
        .assert()
        .failure()
        .stderr(
            contains("review.habits.actions:1:").and(contains("not supported by text catch-up")),
        );
    assert_eq!(snapshot_tree(&env.data_dir), before);
}

#[test]
fn refused_habit_does_not_block_others_and_terminal_habits_do_not_catch_up() {
    let env = TestEnv::new();
    env.write_actions(DEFINITION, &format!("[ ] Bad @2020-11-05 R:FREQ=DAILY;COUNT=1 #{HABIT}\n >[ ] Step @2020-11-05 R:FREQ=DAILY #{STEP}\n[x] Finished @2020-11-05 R:FREQ=DAILY %2020-11-06\n[_] Cancelled @2020-11-05 R:FREQ=DAILY %2020-11-06\n[ ] Valid @2020-11-05 R:FREQ=DAILY;COUNT=1 #{SECOND}\n"));
    env.command()
        .args(["apply", "habits"])
        .assert()
        .failure()
        .stderr(contains("review.habits.actions:2:").and(contains("E002")))
        .stdout(contains("Materialized 1 Habit occurrence(s)"));
    let rows = fs::read_to_string(env.data_dir.join(ROOT)).unwrap();
    assert!(rows.contains("Valid"));
    assert_eq!(clearhead_core::parse_actions(&rows).unwrap().len(), 1);
}
