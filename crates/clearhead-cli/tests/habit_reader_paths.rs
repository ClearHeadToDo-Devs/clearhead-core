mod common;
use clearhead_core::workspace::{
    action_files::{ActionFileRole, actions_anchor, classify_action_file},
    habit_files::lint_action_file,
};
use common::TestEnv;
use std::{fs, path::Path};

const ID: &str = "01951111-0000-7000-8000-000000000010";
const HABIT_ID: &str = "01951111-0000-7000-8000-000000000011";
const CHILD: &str = "01951111-0000-7000-8000-000000000012";
fn habit() -> String {
    format!(
        "[_] Review @2026-11-05T08:00 R:FREQ=WEEKLY;BYDAY=MO,WE %2026-11-05 #{HABIT_ID}\n  >[ ] Template step #{CHILD}\n"
    )
}

#[test]
fn habit_reader_conformance_suffix_and_sidecar_roles() {
    for stem in ["next", "review", "project/next"] {
        let path = format!("{stem}.habits.actions");
        assert_eq!(
            classify_action_file(Path::new(&path)).unwrap().1,
            ActionFileRole::Habits
        );
        assert_eq!(
            actions_anchor(Path::new(&path)).unwrap(),
            Path::new(&format!("{stem}.actions"))
        );
        assert_eq!(
            clearhead_core::workspace::sidecar::sidecar_path(Path::new(&path)),
            clearhead_core::workspace::sidecar::sidecar_path(Path::new(&format!("{stem}.actions")))
        );
    }
}

#[test]
fn habit_reader_conformance_invalid_recurrence_is_e002_not_parser_recovery() {
    for source in [
        "[ ] Missing rule @2026-11-05",
        "[ ] Missing start",
        "[ ] Bad frequency @2026-11-05 R:FREQ=WRONG",
        "[ ] Bad weekday @2026-11-05 R:FREQ=WEEKLY;BYDAY=XY",
        "[ ] Conflicting limits @2026-11-05 R:FREQ=DAILY;COUNT=3;UNTIL=20261110T000000Z",
        "[ ] Root @2026-11-05 R:FREQ=DAILY\n  >[ ] Child @2026-11-05 R:FREQ=DAILY",
    ] {
        let document = clearhead_core::workspace::parse_document(source).unwrap();
        assert!(document.syntax_errors.is_empty(), "{source}");
        assert!(
            lint_action_file(&document, Path::new("review.habits.actions"))
                .iter()
                .any(|d| d.code == "E002"),
            "{source}"
        );
    }
}

#[test]
fn habit_reader_conformance_scope_warning_in_plain_and_named_template() {
    let env = TestEnv::new();
    for path in [
        "charters/review.actions",
        "templates/named.actions",
        "templates/named.habits.actions",
    ] {
        env.write_text(path, &habit());
        let document = clearhead_core::workspace::parse_document(&habit()).unwrap();
        assert!(
            lint_action_file(&document, Path::new(path))
                .iter()
                .any(|d| d.code == "habit-scope")
        );
        env.command()
            .args(["lint", "file", env.data_dir.join(path).to_str().unwrap()])
            .assert()
            .success()
            .stdout(predicates::str::contains("habit-scope"));
    }
}

#[test]
fn habit_reader_conformance_archive_moves_habits_verbatim_without_sweeping_templates() {
    // Dispatcher answer 2026-11-05: retain the current UUID-flat charter archive
    // contract. Path-preserving archival is a separate increment.
    for (anchor, document) in [
        ("next", "README.md"),
        ("review", "review.md"),
        ("project/next", "project/README.md"),
    ] {
        let env = TestEnv::new();
        env.write_text(
            &format!("charters/{document}"),
            &format!("---\nid: {ID}\nalias: owner\nstate: Closed\n---\n# Owner\n"),
        );
        env.write_actions(&format!("{anchor}.actions"), "");
        env.write_actions(&format!("{anchor}.habits.actions"), &habit());
        if anchor.contains('/') {
            env.write_text("charters/project/inventory.txt", "supporting bytes\n");
        }
        env.command()
            .args([
                "archive",
                "charter",
                "--file",
                env.data_dir
                    .join(format!("charters/{anchor}.habits.actions"))
                    .to_str()
                    .unwrap(),
            ])
            .assert()
            .success();
        assert!(
            !env.data_dir
                .join(format!("charters/{anchor}.habits.actions"))
                .exists()
        );
        assert_eq!(
            fs::read_to_string(env.data_dir.join(format!("archive/{ID}.habits.actions"))).unwrap(),
            habit()
        );
        let terminal = env.data_dir.join(format!("archive/{ID}.completed.actions"));
        if terminal.exists() {
            assert!(!fs::read_to_string(terminal).unwrap().contains(HABIT_ID));
        }
        if anchor.contains('/') {
            assert_eq!(
                fs::read_to_string(env.data_dir.join(format!("archive/{ID}.inventory.txt")))
                    .unwrap(),
                "supporting bytes\n"
            );
        }
        let model = clearhead_cli::filesystem::load_domain_model(&env.data_dir).unwrap();
        assert!(model.all_actions().is_empty());
        assert!(model.charters.iter().all(|c| c.habits.is_empty()));
        assert!(
            clearhead_cli::filesystem::read_archived_action_facts(&env.data_dir)
                .unwrap()
                .is_empty()
        );
    }
}

#[test]
fn habit_reader_conformance_habit_only_owner_locations_and_sidecar_doctor() {
    let env = TestEnv::new();
    env.write_actions("review.habits.actions", &habit());
    env.write_text(
        "charters/.review.json",
        &format!(r#"{{"actions":{{"{HABIT_ID}":{{}},"{CHILD}":{{}}}}}}"#),
    );
    let workspace = clearhead_cli::filesystem::load_workspace_model(&env.data_dir).unwrap();
    let model = clearhead_cli::filesystem::load_domain_model(&env.data_dir).unwrap();
    assert_eq!(model.charters.iter().flat_map(|c| &c.habits).count(), 1);
    assert!(model.all_actions().is_empty());
    let locations = clearhead_core::rdf::app::Locations::of(&workspace);
    let id = HABIT_ID.parse().unwrap();
    assert_eq!(locations.files[&id], "charters/review.habits.actions");
    assert_eq!(locations.lines[&id], 1);
    let graph = clearhead_core::rdf::app::project_app(
        &model,
        &locations,
        None,
        &chrono::Utc,
        clearhead_core::rdf::workspace_graph_name(&uuid::Uuid::nil().to_string()),
    )
    .unwrap();
    let triples = graph
        .into_iter()
        .map(|q| oxrdf::Triple::from(q).to_string())
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        triples.contains(&format!(
            "<urn:uuid:{HABIT_ID}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <{}Habit>",
            clearhead_core::rdf::app::APP_NS
        )),
        "{triples}"
    );
    assert!(
        !triples.contains(CHILD),
        "template data must not become Action nodes"
    );
    let result = env.command().args(["doctor"]).output().unwrap();
    let output = format!(
        "{}{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(
        !output.contains("sidecar-orphan") && !output.contains("orphaned-sidecar"),
        "{output}"
    );
    env.write_text("charters/.review.json", "{}");
    let evidence = clearhead_cli::filesystem::observe_doctor(&env.data_dir).unwrap();
    assert!(
        evidence
            .sidecars
            .iter()
            .all(|sidecar| sidecar.companion_exists),
        "an empty shared sidecar still belongs to the Habit-only owner"
    );
}

#[test]
fn habit_reader_conformance_discovery_and_explicit_action_guards() {
    for anchor in ["next", "review", "project/next"] {
        let env = TestEnv::new();
        env.write_actions(&format!("{anchor}.actions"), "");
        env.write_actions(&format!("{anchor}.habits.actions"), &habit());
        env.write_text("templates/named.actions", &habit());
        let files = clearhead_cli::filesystem::list_action_files(&env.data_dir).unwrap();
        assert_eq!(
            files,
            vec![env.data_dir.join(format!("charters/{anchor}.actions"))]
        );
        let manifest =
            clearhead_cli::filesystem::collect_workspace_manifest(&env.data_dir).unwrap();
        assert_eq!(
            manifest.len(),
            1,
            "no duplicate charter or local-template owner"
        );
        let path = env
            .data_dir
            .join(format!("charters/{anchor}.habits.actions"));
        assert!(clearhead_cli::filesystem::read_actions(&path).is_err());
        env.command()
            .args([
                "complete",
                "action",
                HABIT_ID,
                "--file",
                path.to_str().unwrap(),
            ])
            .assert()
            .failure();
        assert_eq!(fs::read_to_string(path).unwrap(), habit());
    }
}

#[test]
fn habit_reader_conformance_rule_text_is_not_normalized() {
    let env = TestEnv::new();
    for rule in [
        "R:FREQ=WEEKLY;BYDAY=WE,MO;INTERVAL=02",
        "R: FREQ=WEEKLY;BYDAY=MO,WE",
    ] {
        let source = format!("[ ] Review @2026-11-05T08:00 {rule} #{ID}\n");
        env.write_actions("review.actions", &source);
        env.command()
            .args(["update", "action", ID, "--name", "Renamed"])
            .assert()
            .success();
        assert_eq!(
            fs::read_to_string(env.data_dir.join("charters/review.actions")).unwrap(),
            source.replace("Review", "Renamed")
        );
    }
}

#[test]
fn habit_reader_conformance_transaction_retains_plain_rule() {
    let env = TestEnv::new();
    let source = format!("[ ] Review @2026-11-05T08:00 R:FREQ=WEEKLY;BYDAY=MO,WE #{ID}\n");
    env.write_actions("review.actions", &source);
    env.command().arg("transact").write_stdin(format!(r#"{{"operations":[{{"op":"update-action","target":"urn:uuid:{ID}","set":{{"name":"Renamed"}}}}]}}"#)).assert().success();
    assert_eq!(
        fs::read_to_string(env.data_dir.join("charters/review.actions")).unwrap(),
        source.replace("Review", "Renamed")
    );
}

#[test]
fn habit_reader_conformance_malformed_anchor_is_never_lossily_rewritten() {
    let env = TestEnv::new();
    for block in [
        "@2026-99-99 R:FREQ=DAILY",
        "@2026-11-05 R:FREQ=DAILY @2026-11-06",
    ] {
        let source = format!("[ ] Review {block} #{ID}\n");
        env.write_actions("review.actions", &source);
        env.command()
            .args(["update", "action", ID, "--name", "Renamed"])
            .assert()
            .failure();
        assert_eq!(
            fs::read_to_string(env.data_dir.join("charters/review.actions")).unwrap(),
            source
        );
    }
}

#[test]
fn habit_reader_conformance_lifecycle_writes_keep_plain_rule() {
    let env = TestEnv::new();
    env.write_actions(
        "review.actions",
        &format!("[ ] Review @2026-11-05T08:00 R:FREQ=WEEKLY;BYDAY=MO,WE #{ID}\n"),
    );
    for args in [
        vec!["complete", "action", ID],
        vec!["archive", "action", ID],
        vec!["unarchive", "action", ID, "--reopen"],
    ] {
        env.command().args(&args).assert().success();
        let mut rules = 0;
        for file in ["review.actions", "review.completed.actions"] {
            let content =
                fs::read_to_string(env.data_dir.join("charters").join(file)).unwrap_or_default();
            rules += content.matches("R:FREQ=WEEKLY;BYDAY=MO,WE").count();
        }
        assert_eq!(rules, 1, "{args:?}");
    }
}
