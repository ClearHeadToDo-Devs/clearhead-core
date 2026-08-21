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
    use clearhead_core::workspace::sidecar::read_sidecar;
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
    use clearhead_core::workspace::sidecar::read_sidecar;
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
