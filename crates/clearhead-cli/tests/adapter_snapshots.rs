mod common;

use common::{TestEnv, WorkspaceTree, snapshot_tree, specification_examples_dir};
use std::collections::BTreeSet;
use std::fmt::Write;
use std::fs;

#[test]
fn lint_named_template() {
    let fixtures = specification_examples_dir().join("habits");
    insta::glob!(fixtures, "weekly-review.habits.actions", |source| {
        let env = TestEnv::new();
        // Identical bytes to the published Habit. The workspace path gives them
        // a different role; recurrence is invalid in a named template.
        let path = "templates/weekly-review.actions";
        env.write_text(path, &fs::read_to_string(source).unwrap());
        let before = snapshot_tree(&env.data_dir);
        let mut command = match std::env::var_os("CLEARHEAD_SNAPSHOT_BIN") {
            Some(binary) => env.command_with_binary(binary),
            None => env.std_command(),
        };
        let output = command
            .current_dir(&env.data_dir)
            .env("RUST_LOG", "off")
            .args(["lint", "file", path])
            .output()
            .unwrap();
        let after = snapshot_tree(&env.data_dir);
        let fixture = source.file_name().unwrap().to_str().unwrap();
        let review = format!(
            "fixture: specifications/examples/actions/habits/{fixture}\n\
             command: clearhead lint file {path}\n\
             exit code: {}\n\nstdout:\n{}\nstderr:\n{}\nworkspace changes:\n{}",
            output
                .status
                .code()
                .expect("clearhead terminated by a signal"),
            stream(&output.stdout),
            stream(&output.stderr),
            changed_entries(&before, &after),
        );

        insta::assert_snapshot!("lint_named_template", review);
    });
}

#[test]
fn workspace_delta_keeps_additions_deletions_and_newline_changes() {
    let before = WorkspaceTree::from([
        ("edited.actions".into(), Some(b"[ ] Edit\n".to_vec())),
        ("removed.actions".into(), Some(Vec::new())),
    ]);
    let after = WorkspaceTree::from([
        ("added.actions".into(), Some(Vec::new())),
        ("edited.actions".into(), Some(b"[ ] Edit".to_vec())),
    ]);
    assert_eq!(
        changed_entries(&before, &after),
        "added.actions\n  before: (missing)\n  after: \"\"\n\
         edited.actions\n  before: \"[ ] Edit\\n\"\n  after: \"[ ] Edit\"\n\
         removed.actions\n  before: \"\"\n  after: (missing)\n"
    );
}

fn stream(bytes: &[u8]) -> &str {
    if bytes.is_empty() {
        "(empty)\n"
    } else {
        std::str::from_utf8(bytes).expect("CLI output should be UTF-8")
    }
}

fn changed_entries(before: &WorkspaceTree, after: &WorkspaceTree) -> String {
    let paths = before.keys().chain(after.keys()).collect::<BTreeSet<_>>();
    let describe = |entry: Option<&Option<Vec<u8>>>| match entry {
        None => "(missing)".into(),
        Some(None) => "(directory)".into(),
        Some(Some(bytes)) => match std::str::from_utf8(bytes) {
            Ok(text) => format!("{text:?}"), // Keep newline-only changes visible.
            Err(_) => format!("{bytes:?}"),
        },
    };
    let mut changes = String::new();
    for path in paths {
        let (old, new) = (before.get(path), after.get(path));
        if old != new {
            writeln!(
                changes,
                "{}\n  before: {}\n  after: {}",
                path.display(),
                describe(old),
                describe(new),
            )
            .unwrap();
        }
    }
    if changes.is_empty() {
        "(none)\n".into()
    } else {
        changes
    }
}
