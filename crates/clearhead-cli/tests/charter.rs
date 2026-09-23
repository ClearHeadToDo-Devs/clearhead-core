mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::fs;

const CHARTER_MD: &str = "\
---
id: 019cffb8-0000-7000-8000-000000000001
alias: my-charter
---
# My Charter
";

#[test]
fn add_charter_writes_explicit_new_state() {
    let env = TestEnv::new();

    env.command()
        .args(["add", "charter", "Fresh Work", "--alias", "fresh-work"])
        .assert()
        .success();

    let path = env.data_dir.join("charters/fresh-work.md");
    let Ok(content) = fs::read_to_string(path) else {
        panic!("new Charter document should be readable");
    };
    assert!(content.contains("state: New"), "{content}");
}

#[test]
fn read_charters_json_materializes_omitted_state_as_new() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    let assert = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let Ok(rows) = serde_json::from_slice::<serde_json::Value>(&assert.get_output().stdout) else {
        panic!("read charters should emit JSON");
    };

    let Some(charter) = rows["charters"]
        .as_array()
        .and_then(|rows| rows.iter().find(|row| row["alias"] == "my-charter"))
    else {
        panic!("my-charter should be listed: {rows}");
    };
    assert_eq!(charter["state"], "new");
}

#[test]
fn read_charters_json_is_source_aware_and_schema_shaped() {
    let env = TestEnv::new();
    env.write_text("charters/work.md", "---\nalias: work\ndefaults:\n  context: focused\n---\n# Work\n\nCore description.\n\n## Log\n\n- 2026-09-17T23:28-07:00 — offset entry\n- undated entry\n\n## Notes\n\nKeep these.\n");
    env.write_actions(
        "work.actions",
        "[ ] Do work #01951111-0000-7000-8000-0000000000cc\n",
    );
    env.write_text(
        "charters/.work.json",
        r#"{"charter":{"id":"01951111-0000-7000-8000-0000000000bb"}}"#,
    );
    let result = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let json: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    let rows = json["charters"]
        .as_array()
        .expect("single charter document");
    let row = rows.iter().find(|row| row["alias"] == "work").unwrap();
    assert_eq!(row["state"], "new");
    assert_eq!(row["description"], "Core description.");
    assert_eq!(row["defaults"]["context"], "focused");
    assert_eq!(row["log"][0]["at"], "2026-09-17T23:28-07:00");
    assert_eq!(row["log"][1]["text"], "undated entry");
    assert_eq!(row["sections"][0]["heading"], "Notes");
    assert!(
        row.get("id").is_none(),
        "sidecar identity cannot be emitted: {row}"
    );
    assert!(row.get("actions").is_none() && row.get("plans").is_none());
    assert!(!serde_json::to_string(row).unwrap().contains("null"));
}

#[test]
fn read_charters_json_is_one_document_across_workspaces_and_empty_when_filtered() {
    let env = TestEnv::new();
    env.write_text("charters/first.md", "---\nalias: first\n---\n# First\n");
    env.write_actions("first.actions", "");
    let second = env.work_dir.join("second");
    fs::create_dir_all(second.join("charters")).unwrap();
    fs::write(
        second.join("charters/second.md"),
        "---\nalias: second\n---\n# Second\n",
    )
    .unwrap();
    fs::write(second.join("charters/second.actions"), "").unwrap();
    env.write_config(&format!(
        r#"{{"additional_workspaces":["{}"]}}"#,
        second.display()
    ));
    let result = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let json: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    let rows = json["charters"].as_array().unwrap();
    assert!(rows.iter().any(|row| row["alias"] == "first"));
    assert!(rows.iter().any(|row| row["alias"] == "second"));

    let empty = TestEnv::new();
    let result = empty
        .command()
        .args([
            "read",
            "charters",
            "--format",
            "json",
            "--workspace",
            "no-such-workspace",
        ])
        .assert()
        .success();
    let json: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    assert_eq!(json, serde_json::json!({"charters": []}));
}

#[test]
fn update_charter_edits_source_text_without_stamping_an_id() {
    let env = TestEnv::new();
    env.write_text(
        "charters/my-charter.md",
        "---\nalias: my-charter\ndefaults:\n  context: work\n---\n# My Charter\n\nBody.\n",
    );
    env.write_actions("my-charter.actions", "");

    env.command()
        .args([
            "update",
            "charter",
            "my-charter",
            "--title",
            "Renamed Charter",
            "--alias",
            "renamed",
        ])
        .assert()
        .success();

    let content = fs::read_to_string(env.data_dir.join("charters/my-charter.md")).unwrap();
    assert!(content.contains("defaults:\n  context: work\n"));
    assert!(content.contains("alias: renamed\n"));
    assert!(content.contains("# Renamed Charter\n\nBody.\n"));
    assert!(
        !content.contains("id:"),
        "update must not mint an id: {content}"
    );
}

#[test]
fn close_charter_by_query_updates_state() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    env.command()
        .args(["close", "charter", "my-charter"])
        .assert()
        .success();

    let content = fs::read_to_string(env.data_dir.join("charters/my-charter.md")).unwrap();
    assert!(
        content.contains("state: Closed"),
        "Expected state: Closed in:\n{content}"
    );
}

#[test]
fn close_charter_preserves_source_text_without_stamping_an_id() {
    let env = TestEnv::new();
    env.write_text(
        "charters/my-charter.md",
        "---\nalias: my-charter\ndefaults:\n  context: work\n---\n# My Charter\n\nBody.\n",
    );
    env.write_actions("my-charter.actions", "");

    env.command()
        .args(["close", "charter", "my-charter"])
        .assert()
        .success();

    let content = fs::read_to_string(env.data_dir.join("charters/my-charter.md")).unwrap();
    assert!(content.contains("defaults:\n  context: work\n"));
    assert!(content.contains("state: Closed\n"));
    assert!(content.contains("# My Charter\n\nBody.\n"));
    assert!(
        !content.contains("id:"),
        "close must not mint an id: {content}"
    );
}

#[test]
fn close_charter_creates_md_for_implicit_charter() {
    let env = TestEnv::new();
    env.write_actions("my-charter.actions", "");

    env.command()
        .args(["close", "charter", "my-charter"])
        .assert()
        .success();

    let md_path = env.data_dir.join("charters/my-charter.md");
    assert!(
        md_path.exists(),
        ".md file should be created for implicit charter"
    );
    let content = fs::read_to_string(&md_path).unwrap();
    assert!(content.contains("state: Closed"));
}

#[test]
fn read_charters_hides_undeclared_ids_in_ids_and_jsonld() {
    let env = TestEnv::new();
    let declared = "01951111-0000-7000-8000-0000000000aa";
    env.write_text(
        "charters/one.md",
        &format!("---\nid: {declared}\nalias: one\n---\n# One\n"),
    );
    env.write_actions("one.actions", "");
    env.write_text(
        "charters/two.md",
        "---\nalias: two\nparent: one\n---\n# Two\n",
    );
    let action_id = "01951111-0000-7000-8000-0000000000cc";
    env.write_actions("two.actions", &format!("[ ] Child work #{action_id}\n"));
    let sidecar_id = "01951111-0000-7000-8000-0000000000bb";
    env.write_text(
        "charters/.two.json",
        &format!(r#"{{"charter":{{"id":"{sidecar_id}"}}}}"#),
    );

    let ids = env
        .command()
        .args(["read", "charters", "--format", "ids"])
        .assert()
        .success();
    assert_eq!(
        String::from_utf8_lossy(&ids.get_output().stdout).trim(),
        declared
    );

    let result = env
        .command()
        .args(["read", "charters", "--format", "json-ld"])
        .assert()
        .success();
    let json: serde_json::Value = serde_json::from_slice(&result.get_output().stdout).unwrap();
    let serialized = serde_json::to_string(&json).unwrap();
    assert!(serialized.contains(&format!("urn:uuid:{declared}")));
    assert!(
        !serialized.contains(sidecar_id),
        "sidecar id is not declared in document"
    );
    let graph = json["@graph"][0]["@graph"].as_array().unwrap();
    let two = graph
        .iter()
        .find(|node| {
            node["http://www.w3.org/2000/01/rdf-schema#label"]
                .as_array()
                .is_some_and(|values| values.iter().any(|value| value["@value"] == "Two"))
        })
        .expect("id-less charter node");
    assert!(two["@id"].as_str().unwrap().starts_with("_:"), "{two}");
    assert!(
        two.get("https://clearhead.us/vocab/actions/v4#hasUUID")
            .is_none(),
        "{two}"
    );
    let one = graph
        .iter()
        .find(|node| node["@id"] == format!("urn:uuid:{declared}"))
        .unwrap();
    assert_eq!(
        one["https://clearhead.us/vocab/actions/v4#hasSubCharter"][0]["@id"],
        two["@id"]
    );
    let action = graph
        .iter()
        .find(|node| node["@id"] == format!("urn:uuid:{action_id}"))
        .expect("child action projected");
    assert_eq!(
        action["http://purl.obolibrary.org/obo/BFO_0000050"][0]["@id"],
        two["@id"]
    );

    // Without a sidecar the shell mints a different id on each load. Neither
    // the ids view nor JSON-LD may expose that transient join key.
    fs::remove_file(env.data_dir.join("charters/.two.json")).unwrap();
    let minted_ids = env
        .command()
        .args(["read", "charters", "--format", "ids"])
        .assert()
        .success();
    assert_eq!(
        String::from_utf8_lossy(&minted_ids.get_output().stdout).trim(),
        declared
    );
    let minted = env
        .command()
        .args(["read", "charters", "--format", "json-ld"])
        .assert()
        .success();
    let minted: serde_json::Value = serde_json::from_slice(&minted.get_output().stdout).unwrap();
    let minted_graph = minted["@graph"][0]["@graph"].as_array().unwrap();
    let minted_two = minted_graph
        .iter()
        .find(|node| {
            node["http://www.w3.org/2000/01/rdf-schema#label"]
                .as_array()
                .is_some_and(|values| values.iter().any(|value| value["@value"] == "Two"))
        })
        .unwrap();
    assert!(minted_two["@id"].as_str().unwrap().starts_with("_:"));
    assert!(
        minted_two
            .get("https://clearhead.us/vocab/actions/v4#hasUUID")
            .is_none()
    );
}

#[test]
fn no_output_surface_publishes_an_undeclared_charter_id() {
    // A sidecar id is stable, so "it never appears" is checkable. Decision 1:
    // only an id declared by the charter's own document is published.
    let env = TestEnv::new();
    let declared = "01951111-0000-7000-8000-0000000000aa";
    env.write_text(
        "charters/one.md",
        &format!("---\nid: {declared}\nalias: one\nstate: Active\n---\n# One\n"),
    );
    env.write_actions("one.actions", "");
    env.write_text(
        "charters/two.md",
        "---\nalias: two\nparent: one\nstate: Active\n---\n# Two\n",
    );
    env.write_actions(
        "two.actions",
        "[ ] Child work #01951111-0000-7000-8000-0000000000cc\n",
    );
    let hidden = "01951111-0000-7000-8000-0000000000bb";
    env.write_text(
        "charters/.two.json",
        &format!(r#"{{"charter":{{"id":"{hidden}"}}}}"#),
    );

    let mut surfaces: Vec<Vec<&str>> = vec![
        vec!["read", "charters"],
        vec!["read", "charters", "--format", "table"],
        vec!["read", "actions", "--format", "json-ld"],
        vec!["show", "charter", "two"],
        vec!["orient"],
        vec!["export", "workspace", "--format", "nquads"],
    ];
    #[cfg(feature = "sparql")]
    surfaces.extend([
        vec!["query", "tree", "--format", "json"],
        vec!["query", "graph", "--format", "dot"],
    ]);

    for args in surfaces {
        let output = env.command().args(&args).assert().success();
        let stdout = String::from_utf8_lossy(&output.get_output().stdout);
        assert!(
            stdout.contains("Two"),
            "{args:?} lost the charter: {stdout}"
        );
        assert!(!stdout.contains(hidden), "{args:?} leaked: {stdout}");
    }
}

#[test]
fn jot_into_project_root_charter_creates_readme_not_phantom() {
    // Project layout: a `.clearhead/` under the working dir. The root
    // `next.actions` charter is named for the project and pairs with README.md;
    // a derived `next.md` would infer to a separate "next" charter — the bug.
    let env = TestEnv::new();
    let charters = env.work_dir.join(".clearhead/charters");
    fs::create_dir_all(&charters).unwrap();
    fs::write(charters.join("next.actions"), "").unwrap();

    // jot used to bail on a primary charter with no `.md`; it now materializes
    // the correctly-paired document instead.
    env.command()
        .args(["jot", "a project finding"])
        .assert()
        .success();

    let readme = charters.join("README.md");
    assert!(readme.exists(), "project root should materialize README.md");
    assert!(
        !charters.join("next.md").exists(),
        "must not create a phantom next.md"
    );
    assert!(
        fs::read_to_string(&readme)
            .unwrap()
            .contains("a project finding")
    );

    let assert = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let rows: serde_json::Value = serde_json::from_slice(&assert.get_output().stdout).unwrap();
    assert_eq!(
        rows["charters"].as_array().map(|r| r.len()),
        Some(1),
        "next.actions + README.md must pair into one charter, not collide: {rows}"
    );
}

#[test]
fn jot_stamps_a_parseable_utc_offset() {
    let env = TestEnv::new();
    env.write_actions("next.actions", "");
    env.command()
        .args(["jot", "offset proof"])
        .assert()
        .success();
    let content = fs::read_to_string(env.data_dir.join("charters/README.md")).unwrap();
    let log_line = content
        .lines()
        .find(|line| line.contains("offset proof"))
        .unwrap();
    let stamp = log_line
        .strip_prefix("- ")
        .unwrap()
        .split(" — ")
        .next()
        .unwrap();
    chrono::DateTime::parse_from_str(stamp, "%Y-%m-%dT%H:%M%:z")
        .expect("jot must store an ISO timestamp with UTC offset");
    assert!(
        stamp.ends_with("+00:00")
            || stamp.ends_with("-00:00")
            || stamp
                .as_bytes()
                .get(stamp.len() - 6)
                .is_some_and(|byte| matches!(byte, b'+' | b'-')),
        "missing UTC offset: {stamp}"
    );
}

#[test]
fn jot_into_user_root_charter_creates_readme_not_phantom() {
    // Both scopes share one root shape: the user root's `next.actions` pairs
    // with README.md exactly like a project root's.
    let env = TestEnv::new();
    env.write_actions("next.actions", "");

    env.command()
        .args(["jot", "a user finding"])
        .assert()
        .success();

    assert!(
        env.data_dir.join("charters/README.md").exists(),
        "user root should materialize README.md"
    );
    assert!(
        !env.data_dir.join("charters/next.md").exists(),
        "must not create a phantom next.md"
    );

    let assert = env
        .command()
        .args(["read", "charters", "--format", "json"])
        .assert()
        .success();
    let rows: serde_json::Value = serde_json::from_slice(&assert.get_output().stdout).unwrap();
    assert_eq!(
        rows["charters"].as_array().map(|r| r.len()),
        Some(1),
        "next.actions + README.md must pair into one charter, not collide: {rows}"
    );
}

#[test]
fn close_charter_by_file_resolves_from_actions_path() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    let actions_path = env.data_dir.join("charters/my-charter.actions");

    env.command()
        .args(["close", "charter", "--file"])
        .arg(&actions_path)
        .assert()
        .success();

    let content = fs::read_to_string(env.data_dir.join("charters/my-charter.md")).unwrap();
    assert!(content.contains("state: Closed"));
}

#[test]
fn close_charter_dry_run_does_not_write() {
    let env = TestEnv::new();
    env.write_text("charters/my-charter.md", CHARTER_MD);
    env.write_actions("my-charter.actions", "");

    let md_path = env.data_dir.join("charters/my-charter.md");
    let before = fs::read_to_string(&md_path).unwrap();

    env.command()
        .args(["close", "charter", "my-charter", "--dry-run"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Would"));

    let after = fs::read_to_string(&md_path).unwrap();
    assert_eq!(before, after, "dry-run must not modify the file");
}

#[test]
fn close_charter_no_args_fails() {
    let env = TestEnv::new();
    env.command()
        .args(["close", "charter"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Provide"));
}

#[test]
fn close_charter_unknown_file_fails() {
    let env = TestEnv::new();
    env.command()
        .args([
            "close",
            "charter",
            "--file",
            "/nonexistent/path/foo.actions",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("No charter found"));
}

#[test]
fn archive_charter_uses_the_native_adapter_end_to_end() {
    let env = TestEnv::new();
    let id = "019cffb8-0000-7000-8000-000000000010";
    env.write_text(
        "charters/done.md",
        &format!("---\nid: {id}\nalias: done\nstate: Closed\n---\n# Done\n"),
    );
    env.write_actions(
        "done.actions",
        "[x] Finished #019cffb8-0000-7000-8000-000000000011\n",
    );

    env.command()
        .args(["archive", "charter", "done"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Archived charter 'done'"));

    assert!(!env.data_dir.join("charters/done.actions").exists());
    assert!(env.data_dir.join(format!("archive/{id}.actions")).exists());
    assert!(env.data_dir.join(format!("archive/{id}.md")).exists());
}

#[test]
fn archive_refuses_a_charter_whose_document_declares_no_id() {
    let env = TestEnv::new();
    // A document with no `id:` in frontmatter loads with an ephemeral,
    // per-load id; archiving must refuse rather than bake it into names.
    env.write_text(
        "charters/note.md",
        "---\nalias: note\nstate: Closed\n---\n# Note\n",
    );
    env.write_actions("note.actions", "");

    env.command()
        .args(["archive", "charter", "note"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("declares no id"));

    // Nothing was written: the charter files stay in place and no archive dir
    // appears.
    assert!(env.data_dir.join("charters/note.md").exists());
    assert!(env.data_dir.join("charters/note.actions").exists());
    assert!(!env.data_dir.join("archive").exists());
}

#[test]
fn archive_closed_sweeps_terminal_charters_but_leaves_active_ones() {
    let env = TestEnv::new();
    env.write_text(
        "charters/done.md",
        "---\nid: 019cffb8-0000-7000-8000-000000000020\nalias: done\nstate: Closed\n---\n# Done\n",
    );
    env.write_actions("done.actions", "");
    env.write_text(
        "charters/live.md",
        "---\nid: 019cffb8-0000-7000-8000-000000000021\nalias: live\nstate: Active\n---\n# Live\n",
    );
    env.write_actions("live.actions", "");

    env.command()
        .args(["archive", "charter", "--closed"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Archived charter 'done'"));

    assert!(!env.data_dir.join("charters/done.md").exists());
    assert!(env.data_dir.join("charters/live.md").exists());
}
