//! The read→write contract: canonical ids emitted by graphd's index projection
//! feed `clearhead transact` verbatim, with no id reshaping between them.
//!
//! This is the double-entry check on the "select from the graph, act atomically"
//! pipeline: if graphd's `--format ids` and transact's `target` ever drift apart
//! (bare vs `urn:uuid:`, stray whitespace, a different id spelling), this test
//! fails even though each side's own tests still pass.

mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::path::PathBuf;
use std::process::Command;

const A: &str = "019f733d-4600-7000-8000-000000000001";
const B: &str = "019f733d-4600-7000-8000-000000000002";

fn graphd_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("clearhead-cli parent")
        .join("clearhead-graphd/target/debug/clearhead-graphd")
}

/// Run graphd's index projection against the test workspace and return the raw
/// `--format ids` lines, using the same XDG environment the CLI would pass it.
fn graphd_index_ids(env: &TestEnv, view: &str) -> Vec<String> {
    let output = Command::new(graphd_bin())
        .env("XDG_CONFIG_HOME", env.config_dir.parent().unwrap())
        .env("XDG_DATA_HOME", env.data_dir.parent().unwrap())
        .env("XDG_STATE_HOME", &env.state_dir)
        .args([
            "-w",
            env.data_dir.to_str().unwrap(),
            "query",
            "index",
            view,
            "--format",
            "ids",
        ])
        .output()
        .expect("failed to run graphd");
    assert!(
        output.status.success(),
        "graphd index {view} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .map(str::to_owned)
        .collect()
}

#[test]
fn graphd_index_ids_feed_transact_verbatim() {
    if !graphd_bin().exists() {
        eprintln!("clearhead-graphd binary not built; skipping handoff test");
        return;
    }

    let env = TestEnv::new();
    env.with_workspace_identity();
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));

    let ids = graphd_index_ids(&env, "default");
    assert_eq!(
        ids.len(),
        2,
        "graphd should project both open actions: {ids:?}"
    );
    assert!(
        ids.iter().all(|id| id.starts_with("urn:uuid:")),
        "graphd emits canonical urn:uuid ids: {ids:?}"
    );

    // Wrap each id as a complete-action target WITHOUT touching the id string —
    // the whole point is that graphd's id IS transact's target.
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
                // Every graph-selected id round-trips into a completed outcome.
                .and(predicate::str::contains(format!("urn:uuid:{A}")))
                .and(predicate::str::contains(format!("urn:uuid:{B}"))),
        );

    // The graph-selected actions were actually acted on: both left the active file.
    let active = clearhead_core::read_actions(&env.data_dir.join("charters/work.actions")).unwrap();
    assert!(
        active.is_empty(),
        "both actions should be completed and out of the active file; got {active:?}"
    );
}
