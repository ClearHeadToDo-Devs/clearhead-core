//! The `clearhead query` facade forwards to graphd as a pure projection, and
//! its `--format ids` output drives `clearhead transact` — the full "select
//! from the graph, act atomically" loop, all through the CLI.

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

fn seed() -> TestEnv {
    let env = TestEnv::new();
    env.with_workspace_identity();
    env.write_actions("work.actions", &format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));
    env
}

#[test]
fn query_facade_is_a_pure_projection_of_direct_graphd() {
    if !graphd_bin().exists() {
        eprintln!("clearhead-graphd not built; skipping facade projection test");
        return;
    }
    let env = seed();

    // Through the CLI facade.
    let facade = env
        .std_command()
        .args(["query", "index", "default", "--format", "ids"])
        .output()
        .expect("run clearhead query");

    // Directly against graphd, same workspace + environment.
    let direct = Command::new(graphd_bin())
        .env("XDG_CONFIG_HOME", env.config_dir.parent().unwrap())
        .env("XDG_DATA_HOME", env.data_dir.parent().unwrap())
        .env("XDG_STATE_HOME", &env.state_dir)
        .args([
            "-w",
            env.data_dir.to_str().unwrap(),
            "query",
            "index",
            "default",
            "--format",
            "ids",
        ])
        .output()
        .expect("run graphd directly");

    assert!(
        facade.status.success(),
        "facade query failed: {}",
        String::from_utf8_lossy(&facade.stderr)
    );
    assert_eq!(
        facade.stdout, direct.stdout,
        "the facade must forward graphd's bytes unchanged"
    );
    assert_eq!(
        facade.status.code(),
        direct.status.code(),
        "the facade must propagate graphd's exit status"
    );
}

#[test]
fn query_facade_ids_feed_transact_end_to_end() {
    if !graphd_bin().exists() {
        eprintln!("clearhead-graphd not built; skipping facade→transact loop");
        return;
    }
    let env = seed();

    let ids_output = env
        .std_command()
        .args(["query", "index", "default", "--format", "ids"])
        .output()
        .expect("run clearhead query");
    assert!(ids_output.status.success());
    let ids: Vec<String> = String::from_utf8(ids_output.stdout)
        .unwrap()
        .lines()
        .map(str::to_owned)
        .collect();
    assert_eq!(ids.len(), 2, "both actions projected: {ids:?}");

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
        .stdout(predicate::str::contains(r#""kind":"committed""#));

    let active = clearhead_core::read_actions(&env.data_dir.join("charters/work.actions")).unwrap();
    assert!(active.is_empty(), "graph-selected actions were acted on");
}

#[test]
fn query_chain_resolves_a_fuzzy_query_and_forwards() {
    if !graphd_bin().exists() {
        eprintln!("clearhead-graphd not built; skipping chain adapter test");
        return;
    }
    let env = seed();

    // `chain` takes a fuzzy name, resolves it to a canonical IRI here, and
    // forwards `index chain --target <iri>` to graphd — so a name that resolves
    // succeeds, and an unknown one fails in the CLI before ever calling graphd.
    env.command()
        .args(["query", "chain", "Alpha", "--format", "ids"])
        .assert()
        .success();

    env.command()
        .args(["query", "chain", "does-not-exist"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("No open action found matching"));
}
