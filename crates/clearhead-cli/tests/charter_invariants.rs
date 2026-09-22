//! Invariant-level guards for the crate-merge-and-charter-identity plan.
//!
//! These tests assert the *invariants* (I4, I5, I6), not the mechanisms that
//! implement them. A mechanism test can pass while the invariant is broken —
//! the 2026-09-18 overnight run shipped a real compare-and-swap gap that every
//! gate passed, because `update`/`close` captured their revision after
//! computing the new text. When an invariant gains a new verb, add a case here
//! rather than trusting the existing mechanism tests.

mod common;

use common::TestEnv;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

const CHARTER_MD: &str = "\
---
alias: my-charter
---
# My Charter

Body.
";

const ACTION_ID: &str = "01a0b000-0000-7000-8000-000000000001";
const ACTION_LINE: &str =
    "[ ] First action ^2026-09-01T10:00 #01a0b000-0000-7000-8000-000000000001";

fn seed(env: &TestEnv) {
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", &format!("{ACTION_LINE}\n"));
}

/// Every regular file under `root`, keyed by relative path, with its bytes, so
/// any content, addition, or removal anywhere in the workspace shows up as a
/// difference.
fn snapshot(root: &Path) -> BTreeMap<PathBuf, Vec<u8>> {
    let mut out = BTreeMap::new();
    collect(root, root, &mut out);
    out
}

fn collect(root: &Path, dir: &Path, out: &mut BTreeMap<PathBuf, Vec<u8>>) {
    for entry in fs::read_dir(dir).expect("read workspace dir") {
        let path = entry.expect("read dir entry").path();
        if path.is_dir() {
            collect(root, &path, out);
        } else {
            out.insert(
                path.strip_prefix(root)
                    .expect("relative path")
                    .to_path_buf(),
                fs::read(&path).expect("read file"),
            );
        }
    }
}

fn run(env: &TestEnv, args: &[&str]) {
    let output = env
        .std_command()
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("clearhead {args:?} failed to run: {e}"));
    let code = output
        .status
        .code()
        .unwrap_or_else(|| panic!("clearhead {args:?} was killed by a signal"));
    // Exit 2 is clap's usage error: a misspelled verb or flag would otherwise
    // make this test silently vacuous. Findings (`doctor`) and lint hits exit 1
    // and are legitimate reads.
    assert_ne!(code, 2, "clearhead {args:?} is not a valid invocation");
}

/// I4 — reads never write.
///
/// The exit status is deliberately not asserted: `doctor` and `lint` report
/// findings with a non-zero code, and that is still a read. The invariant is
/// that the workspace bytes are untouched.
#[test]
fn read_verbs_leave_the_workspace_bytes_untouched() {
    let env = TestEnv::new();
    seed(&env);
    let lint_path = env.data_dir.join("charters/my-charter.actions");
    let lint_path = lint_path.to_str().unwrap();

    let before = snapshot(&env.data_dir);

    for args in [
        vec!["show", "charter", "my-charter"],
        vec!["show", "action", ACTION_ID],
        vec!["read", "charters", "--format", "json"],
        vec!["read", "actions", "--format", "json"],
        vec!["orient"],
        vec!["query", "index"],
        vec!["doctor"],
        vec!["lint", "file", lint_path],
    ] {
        run(&env, &args);
    }

    let after = snapshot(&env.data_dir);
    assert_eq!(
        before, after,
        "a read verb wrote to the workspace; reads must leave the disk untouched"
    );
}

/// I5 — stamping is deliberate.
///
/// Only `normalize` and the creation of a whole new document may write a
/// charter id. `jot` into an existing id-less document must edit its text and
/// leave it id-less. (`update` and `close` are covered in
/// `tests/charter.rs`; creation is covered by the `jot_into_*_root_charter`
/// tests there.)
#[test]
fn jot_does_not_stamp_an_id_on_an_existing_charter() {
    let env = TestEnv::new();
    seed(&env);

    env.command()
        .args(["jot", "a finding worth keeping", "--charter", "my-charter"])
        .assert()
        .success();

    let content = fs::read_to_string(env.data_dir.join("charters/my-charter.md")).unwrap();
    assert!(content.contains("## Log"), "jot lost the log:\n{content}");
    assert!(content.contains("a finding worth keeping"), "{content}");
    assert!(
        !content
            .lines()
            .any(|line| line.trim_start().starts_with("id:")),
        "jot stamped an id on an existing document:\n{content}"
    );
}
