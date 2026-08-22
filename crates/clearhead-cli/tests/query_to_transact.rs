//! The read→write contract: canonical ids emitted by the index projection feed
//! `clearhead transact` verbatim, with no id reshaping between them.
//!
//! This is the double-entry check on the "select from the graph, act atomically"
//! pipeline: if `query index --format ids` and transact's `target` ever drift
//! apart (bare vs `urn:uuid:`, stray whitespace, a different id spelling), this
//! test fails even though each side's own tests still pass.

#![cfg(feature = "sparql")]

mod common;
use common::TestEnv;
use predicates::prelude::*;

const A: &str = "019f733d-4600-7000-8000-000000000001";
const B: &str = "019f733d-4600-7000-8000-000000000002";
const WS: &str = "00000000-0000-0000-0000-0000000000aa";

/// Run the in-process index projection and return its raw `--format ids` lines.
fn index_ids(env: &TestEnv, view: &str) -> Vec<String> {
    let output = env
        .std_command()
        .args(["query", "index", view, "--format", "ids"])
        .output()
        .expect("failed to run clearhead query index");
    assert!(
        output.status.success(),
        "index {view} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .map(str::to_owned)
        .collect()
}

#[test]
fn index_ids_feed_transact_verbatim() {
    let env = TestEnv::new();
    env.write_text(
        "workspace.json",
        &format!(r#"{{"workspace_id":"{WS}","workspace_name":"testws"}}"#),
    );
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));

    let ids = index_ids(&env, "default");
    assert_eq!(ids.len(), 2, "both open actions project: {ids:?}");
    assert!(
        ids.iter().all(|id| id.starts_with("urn:uuid:")),
        "index emits canonical urn:uuid ids: {ids:?}"
    );

    // Wrap each id as a complete-action target WITHOUT touching the id string —
    // the whole point is that the index's id IS transact's target.
    let operations = ids
        .iter()
        .map(|id| format!(r#"{{"op":"complete-action","target":"{id}"}}"#))
        .collect::<Vec<_>>()
        .join(",");
    let request = format!(r#"{{"operations":[{operations}]}}"#);

    env.command()
        .arg("transact")
        .write_stdin(request)
        .assert()
        .success()
        .stdout(
            predicate::str::contains(r#""kind":"committed""#)
                .and(predicate::str::contains(format!("urn:uuid:{A}")))
                .and(predicate::str::contains(format!("urn:uuid:{B}"))),
        );

    // The graph-selected actions were actually acted on: both left the active file.
    let active =
        clearhead_workspace_fs::read_actions(&env.data_dir.join("charters/work.actions")).unwrap();
    assert!(
        active.is_empty(),
        "both actions should be completed and out of the active file; got {active:?}"
    );
}
