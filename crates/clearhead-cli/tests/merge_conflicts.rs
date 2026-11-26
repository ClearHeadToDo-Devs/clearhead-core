//! An unresolved git merge conflict in an action file (specification E010):
//! doctor reports it, reads leave the file's actions out, and writes refuse it.

mod common;
use common::TestEnv;

const HOME_MD: &str =
    "---\nid: 01951111-0000-7000-0000-0000000000aa\nalias: home\nstate: Active\n---\n# Home\n";
const CONFLICTED: &str = "[ ] Plan the release #01951111-0000-7000-0000-000000000001
<<<<<<< ours
>[ ] Write the notes !1 #01951111-0000-7000-0000-000000000002
=======
>[ ] Write the notes !2 #01951111-0000-7000-0000-000000000002
>>>>>>> theirs
";

fn conflicted_workspace() -> TestEnv {
    let env = TestEnv::new();
    env.with_workspace_identity();
    env.write_text("charters/home.md", HOME_MD);
    env.write_actions("home.actions", CONFLICTED);
    env
}

fn output(assert: &assert_cmd::assert::Assert) -> String {
    let out = assert.get_output();
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

#[test]
fn doctor_reports_e010_naming_the_conflict_lines() {
    let env = conflicted_workspace();
    let assert = env.command().arg("doctor").assert().failure();
    let text = output(&assert);
    assert!(text.contains("E010"), "{text}");
    assert!(
        text.contains("unresolved merge conflict at lines 2-6; file quarantined until resolved"),
        "{text}"
    );
    assert!(
        !text.contains("duplicate-uuid"),
        "the file is quarantined, not loaded: {text}"
    );
}

#[test]
fn reads_leave_the_conflicted_actions_out() {
    let env = conflicted_workspace();
    let assert = env.command().args(["read", "actions"]).assert().success();
    let out = String::from_utf8_lossy(&assert.get_output().stdout).to_string();
    assert!(!out.contains("Write the notes"), "{out}");

    let file = env.data_dir.join("charters/home.actions");
    let assert = env
        .command()
        .args(["read", "actions", "--file"])
        .arg(&file)
        .assert()
        .failure();
    assert!(output(&assert).contains("unresolved merge conflict at lines 2-6"));
}

#[test]
fn writes_refuse_the_conflicted_file() {
    let env = conflicted_workspace();
    let file = env.data_dir.join("charters/home.actions");
    for args in [
        vec!["add", "action", "New task", "--charter", "home"],
        vec!["complete", "action", "01951111-0000-7000-0000-000000000001"],
    ] {
        let assert = env.command().args(&args).assert().failure();
        assert!(
            output(&assert).contains("unresolved merge conflict"),
            "{args:?}"
        );
    }
    let assert = env
        .command()
        .args(["format", "file"])
        .arg(&file)
        .assert()
        .failure();
    assert!(output(&assert).contains("unresolved merge conflict"));
    assert_eq!(std::fs::read_to_string(&file).unwrap(), CONFLICTED);
}
