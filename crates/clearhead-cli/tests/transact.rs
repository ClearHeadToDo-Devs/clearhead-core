//! End-to-end tests for `clearhead transact`.

mod common;
use common::TestEnv;
use predicates::prelude::*;

const A: &str = "019f733d-4600-7000-8000-000000000001";
const B: &str = "019f733d-4600-7000-8000-000000000002";

fn active(env: &TestEnv) -> Vec<clearhead_core::Action> {
    clearhead_core::read_actions(&env.data_dir.join("charters/work.actions")).unwrap()
}

#[test]
fn transact_commits_a_batch_from_a_request_file() {
    let env = TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));

    let request = format!(
        r#"{{"operations":[
            {{"op":"update-action","target":"urn:uuid:{A}","set":{{"priority":1}}}},
            {{"op":"complete-action","target":"urn:uuid:{B}"}}
        ]}}"#
    );
    let req_path = env.data_dir.join("req.json");
    std::fs::write(&req_path, request).unwrap();

    env.command()
        .arg("transact")
        .arg(&req_path)
        .assert()
        .success()
        .stdout(predicate::str::contains(r#""kind":"committed""#));

    let remaining = active(&env);
    assert_eq!(remaining.len(), 1, "Beta completed → out of active");
    assert_eq!(remaining[0].name, "Alpha");
    assert_eq!(remaining[0].priority, Some(1));
}

#[test]
fn transact_reads_the_request_from_stdin() {
    let env = TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n"));

    let request = format!(
        r#"{{"operations":[{{"op":"update-action","target":"urn:uuid:{A}","set":{{"name":"Renamed"}}}}]}}"#
    );

    env.command()
        .arg("transact")
        .write_stdin(request)
        .assert()
        .success()
        .stdout(predicate::str::contains(r#""kind":"committed""#));

    assert_eq!(active(&env)[0].name, "Renamed");
}

#[test]
fn transact_rejects_the_whole_batch_and_writes_nothing() {
    let env = TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n"));
    let missing = "019f733d-4600-7000-8000-0000000000ff";

    let request = format!(
        r#"{{"operations":[
            {{"op":"update-action","target":"urn:uuid:{A}","set":{{"priority":1}}}},
            {{"op":"complete-action","target":"urn:uuid:{missing}"}}
        ]}}"#
    );

    env.command()
        .arg("transact")
        .write_stdin(request)
        .assert()
        .failure()
        .stdout(
            predicate::str::contains(r#""kind":"rejected""#)
                .and(predicate::str::contains(r#""operation":1"#)),
        );

    assert_eq!(
        active(&env)[0].priority,
        None,
        "a rejected batch commits nothing"
    );
}

#[test]
fn transact_dry_run_reports_without_writing() {
    let env = TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n"));

    let request = format!(
        r#"{{"operations":[{{"op":"update-action","target":"urn:uuid:{A}","set":{{"priority":1}}}}]}}"#
    );

    env.command()
        .arg("transact")
        .arg("--dry-run")
        .write_stdin(request)
        .assert()
        .success()
        .stdout(predicate::str::contains(r#""kind":"dry-run""#));

    assert_eq!(active(&env)[0].priority, None, "dry-run wrote nothing");
}

#[test]
fn transact_rejects_a_terminal_update_state_as_a_request_error() {
    let env = TestEnv::new();
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n"));

    let request = format!(
        r#"{{"operations":[{{"op":"update-action","target":"urn:uuid:{A}","set":{{"state":"completed"}}}}]}}"#
    );

    // A terminal update-state is a request error (not a `rejected` result):
    // it fails before the lock, like the single update verb.
    env.command()
        .arg("transact")
        .write_stdin(request)
        .assert()
        .failure()
        .stderr(predicate::str::contains("complete-action"));
}
