mod common;

use common::TestEnv;
use std::path::Path;

fn e009_lines(output: &[u8]) -> Vec<String> {
    String::from_utf8_lossy(output)
        .lines()
        .filter(|line| line.ends_with("E009") || line.ends_with("[E009]"))
        .map(str::to_owned)
        .collect()
}

#[test]
fn ancestor_lint_uses_workspace_identity_and_paths_not_display_names() {
    let env = TestEnv::new();
    env.write_text(
        "charters/README.md",
        "---\nid: 019c4f48-0000-7000-8000-000000000001\nalias: work\n---\n# Work\n",
    );
    env.write_actions(
        "next.actions",
        "[ ] Plan the trip =trip #0195aa11-0000-7000-8000-000000000000\n\
           >[ ] Choose dates =dates\n\
           >>[ ] By UUID <0195aa11-0000-7000-8000-000000000000\n\
           >>[ ] By short UUID <0195aa11\n\
           >>[ ] By path <work/trip/dates\n\
           >>[ ] By typed alias <a:dates\n\
           >>[ ] Ambiguous alias <trip\n\
           >>[ ] Ambiguous prefix <0195aa\n\
           >>[ ] Display name <Plan the trip\n\
           >>[ ] Wrong type <c:dates\n",
    );
    env.write_actions(
        "other.actions",
        "[ ] Different trip =TRIP #0195aa22-0000-7000-8000-000000000000\n",
    );
    let doctor = env.command().arg("doctor").output().unwrap();
    assert_eq!(
        e009_lines(&doctor.stdout),
        (3..=6)
            .map(|line| format!("  [next.actions:{line}] E009"))
            .collect::<Vec<_>>(),
        "{}",
        String::from_utf8_lossy(&doctor.stdout)
    );

    let lint = env
        .command()
        .args(["lint", "file"])
        .arg(env.data_dir.join("charters/next.actions"))
        .output()
        .unwrap();
    assert!(!lint.status.success());
    let lines = e009_lines(&lint.stdout);
    assert_eq!(lines.len(), 4, "{lines:?}");
    for (diagnostic, line) in lines.iter().zip(3..=6) {
        assert!(
            diagnostic.contains(&format!("next.actions:{line}:")),
            "{diagnostic}"
        );
    }
}

#[test]
fn ancestor_lint_checks_inline_habits_and_named_templates() {
    let env = TestEnv::new();
    env.write_text(
        "charters/README.md",
        "---\nid: 019c4f48-0000-7000-8000-000000000001\nalias: work\n---\n# Work\n",
    );
    for (path, source) in [
        (
            "charters/next.habits.actions",
            "[ ] Weekly review =weekly @2026-11-09T09:00 R:FREQ=WEEKLY\n  >[ ] Clear inbox <a:work/weekly\n",
        ),
        (
            "templates/named.actions",
            "[ ] Root =scaffold\n  >[ ] Child <scaffold\n",
        ),
    ] {
        env.write_text(path, source);
        let result = env
            .command()
            .args(["lint", "file"])
            .arg(env.data_dir.join(path))
            .output()
            .unwrap();
        let lines = e009_lines(&result.stdout);
        let file = Path::new(path).file_name().unwrap().to_str().unwrap();
        assert_eq!(lines.len(), 1, "{path}: {result:?}");
        assert!(lines[0].contains(&format!("{file}:2:")), "{lines:?}");
    }

    // A live Action with the same alias makes the named-template wait ambiguous.
    // Both file lint and the guard must leave it unresolved, not call it E009.
    env.write_actions("next.actions", "[ ] Another candidate =scaffold\n");
    env.command()
        .args(["lint", "file"])
        .arg(env.data_dir.join("templates/named.actions"))
        .assert()
        .success();
    env.command()
        .args(["apply", "template", "named", "--file"])
        .arg(env.data_dir.join("charters/next.actions"))
        .arg("--dry-run")
        .assert()
        .success();
}
