use assert_cmd::cargo::cargo_bin;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};

const ATTRIBUTES: &str = "*.actions merge=clearhead-actions\n*.completed.actions merge=union\n";
const BASE: &str = "[ ] Draft #01951111-0000-7000-8000-000000000001\n\
[ ] Publish #01951111-0000-7000-8000-000000000002\n";

fn git(dir: &Path, args: &[&str]) -> Output {
    Command::new("git")
        .current_dir(dir)
        // A Git hook exports GIT_DIR; inherited, it points this git at the real repository, and init reinitializes it.
        .env_remove("GIT_DIR")
        .env_remove("GIT_WORK_TREE")
        .env_remove("GIT_INDEX_FILE")
        .env("GIT_CONFIG_NOSYSTEM", "1")
        .env("GIT_CONFIG_GLOBAL", dir.join("unused-gitconfig"))
        .args([
            "-c",
            "user.name=ClearHead test",
            "-c",
            "user.email=test@example.invalid",
            "-c",
            "commit.gpgsign=false",
        ])
        .args(args)
        .output()
        .expect("running git")
}

fn git_ok(dir: &Path, args: &[&str]) {
    let output = git(dir, args);
    assert!(output.status.success(), "git {args:?}: {output:?}");
}

fn merge_branches(base: &str, ours: &str, theirs: &str) -> (Output, String, Output) {
    merge_file("release.actions", base, ours, theirs)
}

fn merge_file(file: &str, base: &str, ours: &str, theirs: &str) -> (Output, String, Output) {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path();
    let file = dir.join(file);
    git_ok(dir, &["init", "-q", "-b", "main"]);
    fs::write(dir.join(".git/info/attributes"), ATTRIBUTES).unwrap();
    fs::write(&file, base).unwrap();
    git_ok(dir, &["add", "."]);
    git_ok(dir, &["commit", "-qm", "base"]);
    git_ok(dir, &["checkout", "-qb", "theirs"]);
    fs::write(&file, theirs).unwrap();
    git_ok(dir, &["commit", "-qam", "theirs"]);
    git_ok(dir, &["checkout", "-q", "main"]);
    fs::write(&file, ours).unwrap();
    git_ok(dir, &["commit", "-qam", "ours"]);
    let driver = format!(
        "\"{}\" merge-actions %O %A %B",
        cargo_bin!("clearhead").display()
    );
    git_ok(dir, &["config", "merge.clearhead-actions.driver", &driver]);

    let merge = git(dir, &["merge", "--no-edit", "theirs"]);
    (
        merge,
        fs::read_to_string(&file).unwrap(),
        git(dir, &["ls-files", "-u"]),
    )
}

#[test]
fn git_merges_two_fields_on_the_same_line() {
    let ours = BASE.replace("Draft", "Outline");
    let theirs = BASE.replace("Draft", "Draft !2");
    let (merge, text, unmerged) = merge_branches(BASE, &ours, &theirs);

    assert!(merge.status.success(), "{merge:?}");
    assert!(unmerged.stdout.is_empty(), "{unmerged:?}");
    assert_eq!(text, BASE.replace("Draft", "Outline !2"));
}

#[test]
fn completed_action_archives_still_union_concurrent_appends() {
    let base = "[x] First\n";
    let (merge, text, unmerged) = merge_file(
        "release.completed.actions",
        base,
        "[x] First\n[x] Second\n",
        "[x] First\n[x] Third\n",
    );

    assert!(merge.status.success(), "{merge:?}");
    assert!(unmerged.stdout.is_empty(), "{unmerged:?}");
    let mut lines: Vec<_> = text.lines().collect();
    lines.sort_unstable();
    assert_eq!(lines, ["[x] First", "[x] Second", "[x] Third"]);
}

#[test]
fn git_stops_with_a_local_conflict_and_keeps_a_clean_edit() {
    let theirs = BASE
        .replace("Draft", "Write")
        .replace("Publish", "Publish !3");
    let (merge, text, unmerged) = merge_branches(BASE, &BASE.replace("Draft", "Outline"), &theirs);

    assert_eq!(merge.status.code(), Some(1), "{merge:?}");
    assert!(!unmerged.stdout.is_empty(), "{unmerged:?}");
    let (draft, publish) = BASE.split_once('\n').unwrap();
    assert_eq!(
        text,
        format!(
            "<<<<<<< ours\n{}\n=======\n{}\n>>>>>>> theirs\n{}",
            draft.replace("Draft", "Outline"),
            draft.replace("Draft", "Write"),
            publish.replace("Publish", "Publish !3")
        )
    );
}

#[test]
fn git_marks_a_parent_cycle_without_moving_unrelated_actions() {
    let row = |name: &str, id: u8| format!("[ ] {name} #01951111-0000-7000-8000-{id:012}\n");
    let head = format!("{}{}", row("Plan", 1), row("Prepare", 2));
    let a = row("Draft", 3);
    let b = row("Review", 4);
    let tail = row("Publish", 5);
    let ours = format!("{b}    >{a}");
    let theirs = format!("{a}    >{b}");
    let (merge, text, unmerged) = merge_branches(
        &format!("{head}{a}{b}{tail}"),
        &format!("{head}{ours}{tail}"),
        &format!("{head}{theirs}{tail}"),
    );

    assert_eq!(merge.status.code(), Some(1), "{merge:?}");
    assert!(!unmerged.stdout.is_empty(), "{unmerged:?}");
    assert_eq!(
        text,
        format!("{head}<<<<<<< ours\n{ours}=======\n{theirs}>>>>>>> theirs\n{tail}")
    );
    assert!(
        clearhead_core::parse_actions(&text)
            .unwrap_err()
            .contains("E010")
    );
}

#[test]
fn git_line_merges_valid_actions_without_uuids() {
    let base = "[ ] Draft\n\n[ ] Publish\n";
    let ours = base.replace("Draft", "Outline");
    let theirs = base.replace("Publish", "Publish !3");
    let (merge, text, unmerged) = merge_branches(base, &ours, &theirs);

    assert!(merge.status.success(), "{merge:?}");
    assert!(unmerged.stdout.is_empty(), "{unmerged:?}");
    assert_eq!(text, "[ ] Outline\n\n[ ] Publish !3\n");
}

#[test]
fn git_line_merge_writes_readable_markers_for_a_same_line_conflict() {
    let (merge, text, unmerged) = merge_branches("[ ] Draft\n", "[ ] Outline\n", "[ ] Draft !2\n");

    assert_eq!(merge.status.code(), Some(1), "{merge:?}");
    assert!(!unmerged.stdout.is_empty(), "{unmerged:?}");
    assert_eq!(
        text,
        "<<<<<<< ours\n[ ] Outline\n=======\n[ ] Draft !2\n>>>>>>> theirs\n"
    );
    assert!(
        clearhead_core::parse_actions(&text)
            .unwrap_err()
            .contains("E010")
    );
}

#[test]
fn git_line_merge_does_not_write_a_clean_result_with_duplicate_uuids() {
    let base = "[ ] Draft\n[ ] Review\n[ ] Publish\n";
    let added = "[ ] New #01951111-0000-7000-8000-000000000001\n";
    let ours = format!("{added}{base}");
    let theirs = format!("{base}{added}");
    let (merge, text, unmerged) = merge_branches(base, &ours, &theirs);

    assert_eq!(merge.status.code(), Some(1), "{merge:?}");
    assert!(!unmerged.stdout.is_empty(), "{unmerged:?}");
    assert!(
        String::from_utf8_lossy(&merge.stderr).contains("text merge result: duplicate UUID"),
        "{merge:?}"
    );
    assert_eq!(text, ours);
}

#[test]
fn git_line_merge_does_not_write_markers_inside_a_description() {
    let base = "[ ] Draft $ intro\nBase\nlast line $\n";
    let ours = base.replace("Base", "Ours");
    let theirs = base.replace("Base", "Theirs");
    let (merge, text, unmerged) = merge_branches(base, &ours, &theirs);

    assert_eq!(merge.status.code(), Some(1), "{merge:?}");
    assert!(!unmerged.stdout.is_empty(), "{unmerged:?}");
    assert!(
        String::from_utf8_lossy(&merge.stderr)
            .contains("conflict markers where ClearHead cannot read them"),
        "{merge:?}"
    );
    assert_eq!(text, ours);
}

#[test]
fn malformed_input_does_not_replace_ours_even_if_a_uuid_is_missing() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path();
    let base = "[ ] Draft\n";
    fs::write(dir.join("base"), base).unwrap();
    fs::write(dir.join("ours"), base).unwrap();
    fs::write(
        dir.join("theirs"),
        "[ ] broken $ missing closing delimiter\n",
    )
    .unwrap();
    let output = Command::new(cargo_bin!("clearhead"))
        .current_dir(dir)
        .args(["merge-actions", "base", "ours", "theirs"])
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("theirs: syntax-error"),
        "{output:?}"
    );
    assert_eq!(fs::read_to_string(dir.join("ours")).unwrap(), base);
}
