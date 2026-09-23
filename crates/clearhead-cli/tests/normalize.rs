mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::fs;

#[test]
fn test_empty_actions_file() {
    let env = TestEnv::new();
    env.write_actions("empty.actions", "");
    let empty_path = env.data_dir.join("charters").join("empty.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--file")
        .arg(empty_path)
        .assert()
        .success();
}

#[test]
fn test_actions_file_with_only_whitespace() {
    let env = TestEnv::new();
    env.write_actions("whitespace.actions", "   \n\n  \t  \n");
    let ws_path = env.data_dir.join("charters").join("whitespace.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--file")
        .arg(ws_path)
        .assert()
        .success();
}

#[test]
fn test_normalize_adds_uuids() {
    let env = TestEnv::new();
    env.write_actions("no_id.actions", "[ ] Task without ID");
    let file_path = env.data_dir.join("charters").join("no_id.actions");
    env.command()
        .arg("normalize")
        .arg("file")
        .arg(&file_path)
        .arg("--write")
        .assert()
        .success();
    let content = fs::read_to_string(&file_path).unwrap();
    assert!(content.contains("#"));
}

#[test]
fn normalize_charter_stamps_once_and_doctor_clears_warning() {
    let env = TestEnv::new();
    let original = "---\nalias: work\ncustom: keep\n---\n# Work\n\n## Log\n\n- keep this entry\n";
    env.write_text("charters/work.md", original);
    env.write_text("charters/work.actions", "");
    let path = env.data_dir.join("charters/work.md");
    env.command()
        .args(["normalize", "file"])
        .arg(&path)
        .arg("--write")
        .assert()
        .success();
    let first = fs::read_to_string(&path).unwrap();
    assert!(first.starts_with("---\nid: "), "{first}");
    assert!(
        first.contains("alias: work\ncustom: keep\n---\n# Work\n\n## Log\n\n- keep this entry\n")
    );
    env.command()
        .args(["normalize", "file"])
        .arg(&path)
        .arg("--write")
        .assert()
        .success();
    assert_eq!(first, fs::read_to_string(&path).unwrap());
    env.command()
        .arg("doctor")
        .assert()
        .code(1)
        .stdout(predicate::str::contains("charter-document-without-id").not());
}

#[test]
fn normalize_charter_adopts_sidecar_id_and_preserves_crlf() {
    let env = TestEnv::new();
    env.write_text(
        "charters/work.md",
        "---\r\nalias: work\r\n---\r\n# Work\r\n",
    );
    env.write_text(
        "charters/.work.json",
        r#"{"charter":{"id":"01951111-0000-7000-0000-0000000000aa"}}"#,
    );
    let path = env.data_dir.join("charters/work.md");
    env.command()
        .args(["normalize", "file"])
        .arg(&path)
        .arg("--write")
        .assert()
        .success();
    assert_eq!(
        fs::read_to_string(&path).unwrap(),
        "---\r\nid: 01951111-0000-7000-0000-0000000000aa\r\nalias: work\r\n---\r\n# Work\r\n"
    );
}

#[test]
fn normalize_charter_handles_mixed_newlines_and_null_id() {
    let env = TestEnv::new();
    env.write_text("charters/mixed.md", "---\nalias: mixed\n---\n# Mixed\r\n");
    env.write_text(
        "charters/null.md",
        "---\nid: null # retain this note\nalias: null\n---\n# Null\n",
    );
    for name in ["mixed", "null"] {
        let path = env.data_dir.join("charters").join(format!("{name}.md"));
        env.command()
            .args(["normalize", "file"])
            .arg(&path)
            .arg("--write")
            .assert()
            .success();
        let text = fs::read_to_string(&path).unwrap();
        let document = clearhead_core::workspace::parse_charter(&text).unwrap();
        assert!(document.id.is_some(), "{text}");
        assert_eq!(text.matches("id:").count(), 1, "{text}");
        if name == "null" {
            assert!(text.contains(" # retain this note\n"), "{text}");
        }
        if name == "mixed" {
            assert!(text.starts_with("---\nid: "), "{text}");
            assert!(text.ends_with("# Mixed\r\n"));
        }
    }
}

#[test]
fn normalize_charter_handles_spaced_opening_and_rejects_ambiguous_id() {
    let env = TestEnv::new();
    env.write_text(
        "charters/spaced.md",
        "---   \nalias: work\n---   \n# Work\nid: null\n",
    );
    let path = env.data_dir.join("charters/spaced.md");
    env.command()
        .args(["normalize", "file"])
        .arg(&path)
        .arg("--write")
        .assert()
        .success();
    let text = fs::read_to_string(&path).unwrap();
    assert!(
        clearhead_core::workspace::parse_charter(&text)
            .unwrap()
            .id
            .is_some()
    );
    assert_eq!(text.matches("---").count(), 2, "{text}");
    assert!(
        text.ends_with("# Work\nid: null\n"),
        "body must remain intact: {text}"
    );

    for (name, source) in [
        ("spaced-id", "---\nid : null\n---\n# Work\n"),
        ("quoted-id", "---\n'id': null\n---\n# Work\n"),
    ] {
        env.write_text(&format!("charters/{name}.md"), source);
        let path = env.data_dir.join("charters").join(format!("{name}.md"));
        env.command()
            .args(["normalize", "file"])
            .arg(&path)
            .arg("--write")
            .assert()
            .failure()
            .stderr(predicate::str::contains("file not modified"));
        assert_eq!(fs::read_to_string(path).unwrap(), source);
    }
}

#[test]
fn test_patch_updates_existing_actions() {
    let env = TestEnv::new();
    let uuid = "8975ca06-f358-4846-916a-b32bb1fd7f7a";
    env.write_actions("primary.actions", &format!("[ ] Task A #{}", uuid));
    env.write_actions("secondary.actions", &format!("[x] Task A #{}", uuid));
    let primary_path = env.data_dir.join("charters").join("primary.actions");
    let secondary_path = env.data_dir.join("charters").join("secondary.actions");
    env.command()
        .arg("patch")
        .arg("file")
        .arg("--primary")
        .arg(&primary_path)
        .arg("--secondary")
        .arg(&secondary_path)
        .arg("--write")
        .assert()
        .success();
    let content = fs::read_to_string(&primary_path).unwrap();
    assert!(content.contains("[x] Task A"));
    assert!(content.contains(uuid));
}

#[test]
fn test_patch_appends_new_actions() {
    let env = TestEnv::new();
    let uuid_a = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa";
    let uuid_b = "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb";
    env.write_actions("primary.actions", &format!("[ ] Task A #{}", uuid_a));
    env.write_actions(
        "secondary.actions",
        &format!("[ ] Task A #{}\n[ ] Task B #{}", uuid_a, uuid_b),
    );
    let primary_path = env.data_dir.join("charters").join("primary.actions");
    let secondary_path = env.data_dir.join("charters").join("secondary.actions");
    env.command()
        .arg("patch")
        .arg("file")
        .arg("--primary")
        .arg(&primary_path)
        .arg("--secondary")
        .arg(&secondary_path)
        .arg("--write")
        .assert()
        .success();
    let content = fs::read_to_string(&primary_path).unwrap();
    assert!(content.contains("Task A"));
    assert!(content.contains("Task B"));
}

#[test]
fn test_normalize_file_write_parse_error_keeps_file_unchanged_and_fails() {
    let env = TestEnv::new();
    let malformed = "not valid actions syntax !!!\n[ ] Keep normalize file\n";
    env.write_text("charters/normalize-bad.actions", malformed);
    let path = env.data_dir.join("charters").join("normalize-bad.actions");
    env.command()
        .arg("normalize")
        .arg("file")
        .arg(&path)
        .arg("--write")
        .assert()
        .failure()
        .stderr(predicate::str::contains("file not modified"));
    assert_eq!(
        fs::read_to_string(&path).unwrap(),
        malformed,
        "malformed file should remain byte-stable"
    );
}

#[test]
fn test_format_file_refuses_recovered_source_even_for_stdout() {
    let env = TestEnv::new();
    let malformed = concat!(
        "[ ] Read [[docs|https://example.com\n",
        "[ ] Keep formatting preview #019f0000-0000-7000-8000-000000000001\n",
    );
    env.write_text("charters/format-recover.actions", malformed);
    let path = env.data_dir.join("charters").join("format-recover.actions");
    env.command()
        .arg("format")
        .arg("file")
        .arg(&path)
        .assert()
        .failure()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::contains("Source not rewritten"));
    assert_eq!(
        fs::read_to_string(&path).unwrap(),
        malformed,
        "format refusal must leave recovered source byte-stable"
    );
}

#[test]
fn test_normalize_write_creates_sidecar() {
    use clearhead_cli::filesystem::sidecar::read_sidecar;
    let env = TestEnv::new();
    let uuid = "01951111-0000-7000-0000-000000000001";
    env.write_actions("work.actions", &format!("[ ] Task one #{}\n", uuid));
    let file_path = env.data_dir.join("charters").join("work.actions");
    env.command()
        .arg("normalize")
        .arg("file")
        .arg(&file_path)
        .arg("--write")
        .assert()
        .success();
    let sidecar_path = env.data_dir.join("charters").join(".work.json");
    assert!(
        sidecar_path.exists(),
        "sidecar must be created by normalize --write"
    );
    let meta = read_sidecar(&sidecar_path).unwrap();
    assert!(
        meta.actions.contains_key(uuid),
        "sidecar must have entry for the action UUID"
    );
    assert!(
        meta.actions[uuid].created.is_some(),
        "sidecar entry must have created timestamp"
    );
}

#[test]
fn test_sidecar_additive_on_repeated_normalize() {
    use clearhead_cli::filesystem::sidecar::read_sidecar;
    let env = TestEnv::new();
    let uuid = "01951111-0000-7000-0000-000000000001";
    env.write_actions("work.actions", &format!("[ ] Task #{}\n", uuid));
    let file_path = env.data_dir.join("charters").join("work.actions");
    let sidecar_path = env.data_dir.join("charters").join(".work.json");
    env.command()
        .arg("normalize")
        .arg("file")
        .arg(&file_path)
        .arg("--write")
        .assert()
        .success();
    let created_first = read_sidecar(&sidecar_path).unwrap().actions[uuid]
        .created
        .unwrap();
    env.command()
        .arg("normalize")
        .arg("file")
        .arg(&file_path)
        .arg("--write")
        .assert()
        .success();
    let created_second = read_sidecar(&sidecar_path).unwrap().actions[uuid]
        .created
        .unwrap();
    assert_eq!(
        created_first, created_second,
        "created timestamp must not change on re-normalize"
    );
}
