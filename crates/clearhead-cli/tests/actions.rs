mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::fs;

#[test]
fn test_read_acts_with_default_file() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Test task");
    env.command()
        .arg("read")
        .arg("actions")
        .assert()
        .success()
        .stdout(predicate::str::contains("Test task"));
}

#[test]
fn test_read_acts_specific_file() {
    let env = TestEnv::new();
    env.write_actions("work.actions", "[-] In progress task");
    let work_path = env.data_dir.join("charters").join("work.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--file")
        .arg(work_path)
        .assert()
        .success()
        .stdout(predicate::str::contains("In progress task"));
}

#[test]
fn test_read_acts_open_only_filters_closed_states_in_open_file() {
    let env = TestEnv::new();
    env.write_actions(
        "work.actions",
        "[ ] Open task\n[x] Done task\n[_] Cancelled task",
    );
    let work_path = env.data_dir.join("charters").join("work.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--open-only")
        .arg("--file")
        .arg(work_path)
        .assert()
        .success()
        .stdout(predicate::str::contains("Open task"))
        .stdout(predicate::str::contains("Done task").not())
        .stdout(predicate::str::contains("Cancelled task").not());
}

#[test]
fn test_show_act_resolves_by_name() {
    let env = TestEnv::new();
    env.write_actions("work.actions", "[ ] Inspect CLI $Useful detail$ +cli");
    let work_path = env.data_dir.join("charters").join("work.actions");
    env.command()
        .arg("show")
        .arg("action")
        .arg("Inspect")
        .arg("--file")
        .arg(work_path)
        .assert()
        .success()
        .stdout(predicate::str::contains("Inspect CLI"))
        .stdout(predicate::str::contains("description:"));
}

#[test]
fn test_read_acts_json_format() {
    let env = TestEnv::new();
    env.write_actions("test.actions", "[x] Test $with description$ !1 +context");
    let test_path = env.data_dir.join("charters").join("test.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--format")
        .arg("json")
        .arg("--file")
        .arg(&test_path)
        .assert()
        .success()
        // `json` is an alias for the json-ld output mode.
        .stdout(predicate::str::contains("\"name\""))
        .stdout(predicate::str::contains("Test"));
}

#[test]
fn test_read_acts_with_hierarchy() {
    let env = TestEnv::new();
    env.write_actions("test.actions", "[x] Parent\n>[ ] Child\n>>[ ] Grandchild");
    let test_path = env.data_dir.join("charters").join("test.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--file")
        .arg(test_path)
        .assert()
        .success()
        .stdout(predicate::str::contains("Parent"))
        .stdout(predicate::str::contains("Child"))
        .stdout(predicate::str::contains("Grandchild"));
}

#[test]
fn test_format_style_flags() {
    let env = TestEnv::new();
    env.write_actions("compact.actions", "[ ] Root\n>[ ] Child");
    let compact_path = env.data_dir.join("charters").join("compact.actions");
    env.command()
        .arg("format")
        .arg("file")
        .arg(&compact_path)
        .arg("--style")
        .arg("compact")
        .arg("--indent-width")
        .arg("2")
        .assert()
        .success()
        .stdout(predicate::str::contains(">[ ] Child"));
    env.write_actions("list.actions", "[ ] Root $ Desc $");
    let list_path = env.data_dir.join("charters").join("list.actions");
    env.command()
        .arg("format")
        .arg("file")
        .arg(&list_path)
        .arg("--style")
        .arg("list")
        .arg("--indent-width")
        .arg("4")
        .assert()
        .success()
        // Description hugs its $ markers (icon->value compact, like !1/#id); the
        // spaced input `$ Desc $` normalises to `$Desc$`.
        .stdout(predicate::str::contains("$Desc$"));
    env.command()
        .arg("format")
        .arg("file")
        .arg(&compact_path)
        .arg("--indent-style")
        .arg("tabs")
        .arg("--indent-width")
        .arg("1")
        .assert()
        .success()
        .stdout(predicate::str::contains(">[ ] Child"));
}

#[test]
fn test_json_output_validates_against_schema() {
    let env = TestEnv::new();
    env.write_actions(
        "test.actions",
        "[x] Parent task $description$ !1 +work,urgent\n> [ ] Child task\n>> [-] Grandchild task",
    );
    let test_path = env.data_dir.join("charters").join("test.actions");
    let output = env
        .command()
        .arg("read")
        .arg("actions")
        .arg("--format")
        .arg("json-ld")
        .arg("--file")
        .arg(&test_path)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let json_str = String::from_utf8(output).expect("Invalid UTF-8");
    let json_value: serde_json::Value = serde_json::from_str(&json_str).expect("Invalid JSON");
    // read actions emits a JSON-LD document: an object carrying @context and a @graph array of nodes.
    assert!(
        json_value.is_object(),
        "Expected a JSON-LD document object from read actions"
    );
    assert!(
        json_value
            .get("@graph")
            .and_then(|g| g.as_array())
            .is_some(),
        "Expected a @graph array in the JSON-LD document",
    );
    assert!(json_str.contains("Parent task"));
}

#[test]
fn test_add_action_defaults_to_only_charter() {
    let env = TestEnv::new();
    env.write_actions("work.actions", "[ ] Existing work\n");

    env.command()
        .arg("add")
        .arg("action")
        .arg("New task")
        .assert()
        .success()
        .stdout(predicate::str::contains("Added action"));

    let content = fs::read_to_string(env.data_dir.join("charters").join("work.actions")).unwrap();
    assert!(content.contains("[ ] Existing work"));
    assert!(content.contains("[ ] New task"));
}

#[test]
fn test_add_action_defaults_to_existing_default_file() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Existing inbox\n");

    env.command()
        .arg("add")
        .arg("action")
        .arg("New inbox task")
        .assert()
        .success()
        .stdout(predicate::str::contains("Added action"));

    let content = fs::read_to_string(env.data_dir.join("charters").join("inbox.actions")).unwrap();
    assert!(content.contains("[ ] Existing inbox"));
    assert!(content.contains("[ ] New inbox task"));
}

#[test]
fn test_add_child_inserts_after_parent_descendants_before_next_root() {
    let env = TestEnv::new();
    let path = env.data_dir.join("charters/work.actions");
    env.write_actions(
        "work.actions",
        "[ ] First root #019f733d-45b2-7f21-bcad-5610887b7230\n> [ ] Existing child #019f733d-45c2-7dd2-91dc-8631f33c6b77\n[ ] Second root #019f733d-45d2-7dd2-91dc-8631f33c6b77\n",
    );

    env.command()
        .arg("add")
        .arg("action")
        .arg("New child")
        .arg("--parent")
        .arg("First root")
        .arg("--file")
        .arg(&path)
        .assert()
        .success();

    let actions = clearhead_core::read_actions(&path).unwrap();
    let names: Vec<_> = actions.iter().map(|action| action.name.as_str()).collect();
    assert_eq!(
        names,
        ["First root", "Existing child", "New child", "Second root"]
    );
    assert_eq!(actions[2].parent_id, Some(actions[0].id));
    assert!(actions[3].parent_id.is_none());
}

#[test]
fn test_add_and_update_action_predecessors() {
    let env = TestEnv::new();
    let path = env.data_dir.join("charters/work.actions");
    env.write_actions(
        "work.actions",
        "[ ] Foundation #019f733d-45b2-7f21-bcad-5610887b7230\n",
    );

    env.command()
        .arg("add")
        .arg("action")
        .arg("Dependent")
        .arg("--predecessor")
        .arg("Foundation")
        .arg("--predecessor")
        .arg("external gate")
        .arg("--file")
        .arg(&path)
        .assert()
        .success();

    let actions = clearhead_core::read_actions(&path).unwrap();
    let dependent = actions
        .iter()
        .find(|action| action.name == "Dependent")
        .unwrap();
    let refs: Vec<_> = dependent
        .predecessors
        .as_ref()
        .unwrap()
        .iter()
        .map(|predecessor| predecessor.raw_ref.as_str())
        .collect();
    assert_eq!(refs, ["Foundation", "external gate"]);

    env.command()
        .arg("update")
        .arg("action")
        .arg("Dependent")
        .arg("--predecessor")
        .arg("019f733d-45b2-7f21-bcad-5610887b7230")
        .arg("--file")
        .arg(&path)
        .assert()
        .success();

    let actions = clearhead_core::read_actions(&path).unwrap();
    let dependent = actions
        .iter()
        .find(|action| action.name == "Dependent")
        .unwrap();
    let predecessors = dependent.predecessors.as_ref().unwrap();
    assert_eq!(predecessors.len(), 1);
    assert_eq!(
        predecessors[0].raw_ref,
        "019f733d-45b2-7f21-bcad-5610887b7230"
    );
}

#[test]
fn test_update_rejects_terminal_state_but_allows_non_terminal() {
    let env = TestEnv::new();
    let path = env.data_dir.join("charters/work.actions");
    env.write_actions("work.actions", "[ ] Task\n");

    // A terminal state is not a field edit: it must go through complete/cancel,
    // which cascade and archive. update refuses it and leaves the file untouched.
    for terminal in ["completed", "cancelled"] {
        env.command()
            .arg("update")
            .arg("action")
            .arg("Task")
            .arg("--state")
            .arg(terminal)
            .arg("--file")
            .arg(&path)
            .assert()
            .failure()
            .stderr(predicate::str::contains("use complete/cancel"));

        let actions = clearhead_core::read_actions(&path).unwrap();
        let task = actions.iter().find(|a| a.name == "Task").unwrap();
        assert_eq!(task.state, clearhead_core::ActionState::NotStarted);
    }

    // A non-terminal transition is an ordinary field edit and still succeeds.
    env.command()
        .arg("update")
        .arg("action")
        .arg("Task")
        .arg("--state")
        .arg("in-progress")
        .arg("--file")
        .arg(&path)
        .assert()
        .success();

    let actions = clearhead_core::read_actions(&path).unwrap();
    let task = actions.iter().find(|a| a.name == "Task").unwrap();
    assert_eq!(task.state, clearhead_core::ActionState::InProgress);
}

#[test]
fn test_delete_reaches_an_action_in_the_completed_file() {
    let env = TestEnv::new();
    let completed = env.data_dir.join("charters/work.completed.actions");
    env.write_actions("work.actions", "[ ] Live\n");
    env.write_actions(
        "work.completed.actions",
        "[x] Archived thing #019f733d-45b2-7f21-bcad-5610887b7230\n",
    );

    // Given only the active file, delete still finds and removes an action that
    // lives in the completed sibling — deletion reaches an action anywhere.
    env.command()
        .arg("delete")
        .arg("action")
        .arg("Archived thing")
        .arg("--file")
        .arg(env.data_dir.join("charters/work.actions"))
        .assert()
        .success()
        .stdout(predicate::str::contains("Deleted action"));

    assert!(
        clearhead_core::read_actions(&completed).unwrap().is_empty(),
        "the completed action should be gone"
    );
    assert_eq!(
        clearhead_core::read_actions(&env.data_dir.join("charters/work.actions"))
            .unwrap()
            .len(),
        1,
        "the active file is untouched"
    );
}

#[test]
fn test_add_action_without_target_errors_when_ambiguous() {
    let env = TestEnv::new();
    env.write_actions("one.actions", "[ ] One\n");
    env.write_actions("two.actions", "[ ] Two\n");

    env.command()
        .arg("add")
        .arg("action")
        .arg("Ambiguous task")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Specify --charter <name> or --file <path>",
        ));
}

#[test]
fn test_complete_command() {
    let env = TestEnv::new();
    let uuid = "019baaec-00b6-7991-be34-94b68212619a";
    env.write_actions("inbox.actions", &format!("[ ] Task to complete #{}", uuid));
    env.command()
        .arg("complete")
        .arg("action")
        .arg(uuid)
        .assert()
        .success();
    let content = fs::read_to_string(
        env.data_dir
            .join("charters")
            .join("inbox.completed.actions"),
    )
    .unwrap();
    assert!(content.contains("[x] Task to complete"));
    assert!(content.contains("%")); // Completed date
}

#[test]
fn test_complete_command_by_name() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Unique Task Name");
    env.command()
        .arg("complete")
        .arg("action")
        .arg("Unique Task")
        .assert()
        .success();
    let content = fs::read_to_string(
        env.data_dir
            .join("charters")
            .join("inbox.completed.actions"),
    )
    .unwrap();
    assert!(content.contains("[x] Unique Task Name"));
}

#[test]
fn test_complete_command_project_root_next_actions_uses_project_name() {
    let env = TestEnv::new();
    let project_root = env.work_dir.join("sample-project");
    let charters_dir = project_root.join(".clearhead").join("charters");
    fs::create_dir_all(&charters_dir).unwrap();

    let next_path = charters_dir.join("next.actions");
    let uuid = "019baaec-00b6-7991-be34-94b68212619b";
    fs::write(&next_path, format!("[ ] Project root task #{}", uuid)).unwrap();

    let mut cmd = env.command();
    cmd.current_dir(&project_root)
        .arg("complete")
        .arg("action")
        .arg(uuid)
        .arg("--file")
        .arg(&next_path)
        .assert()
        .success();

    let completed_path = charters_dir.join("sample-project.completed.actions");
    assert!(
        completed_path.exists(),
        "expected {} to exist",
        completed_path.display()
    );
    let content = fs::read_to_string(&completed_path).unwrap();
    assert!(content.contains("[x] Project root task"));
    assert!(!charters_dir.join("charters.completed.actions").exists());
}

#[test]
fn test_archive_actions_project_root_next_actions_uses_project_name() {
    let env = TestEnv::new();
    let project_root = env.work_dir.join("sample-project");
    let charters_dir = project_root.join(".clearhead").join("charters");
    fs::create_dir_all(&charters_dir).unwrap();

    let next_path = charters_dir.join("next.actions");
    fs::write(&next_path, "[x] Already done").unwrap();

    let mut cmd = env.command();
    cmd.current_dir(&project_root)
        .arg("archive")
        .arg("actions")
        .arg("--file")
        .arg(&next_path)
        .assert()
        .success();

    let completed_path = charters_dir.join("sample-project.completed.actions");
    assert!(
        completed_path.exists(),
        "expected {} to exist",
        completed_path.display()
    );
    let content = fs::read_to_string(&completed_path).unwrap();
    assert!(content.contains("[x] Already done"));
    assert!(
        content.contains(" %"),
        "archival must stamp a missing completion date before moving the action: {content}"
    );
    assert!(!charters_dir.join("charters.completed.actions").exists());
}

#[test]
fn test_archive_actions_keeps_a_terminal_parent_with_an_open_child() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "[x] Terminal parent #019f733d-45b2-7f21-bcad-5610887b7230\n> [ ] Open child #019f733d-45c2-7dd2-91dc-8631f33c6b77\n",
    );

    env.command()
        .arg("archive")
        .arg("actions")
        .assert()
        .success()
        .stdout(predicate::str::contains("Nothing to archive."));

    let active = fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap();
    assert!(active.contains("Terminal parent"));
    assert!(active.contains("Open child"));
    assert!(
        !env.data_dir
            .join("charters/inbox.completed.actions")
            .exists()
    );
}

#[test]
fn test_archive_actions_refuses_to_race_an_existing_writer() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[x] Done\n");
    let _lock = clearhead_core::workspace::durability::WorkspaceLock::try_acquire(&env.data_dir)
        .unwrap()
        .unwrap();

    env.command()
        .arg("archive")
        .arg("actions")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Workspace is locked"));

    let active = fs::read_to_string(env.data_dir.join("charters/inbox.actions")).unwrap();
    assert!(active.contains("Done"));
    assert!(
        !env.data_dir
            .join("charters/inbox.completed.actions")
            .exists()
    );
}

#[test]
fn test_complete_command_already_closed_is_typed_data() {
    // Verb errors are data (query_output.md): with stdout piped, an
    // already-completed target comes back as a branchable JSON result,
    // not stderr prose.
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[x] Already Done");
    env.command()
        .arg("complete")
        .arg("action")
        .arg("Already Done")
        .assert()
        .failure()
        .stdout(predicate::str::contains(r#""kind":"already-closed""#))
        .stdout(predicate::str::contains(r#""state":"Completed""#));
}

#[test]
fn test_complete_command_unknown_target_is_typed_not_found() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Something else");
    env.command()
        .arg("complete")
        .arg("action")
        .arg("urn:uuid:01951111-dead-7000-8000-000000000009")
        .assert()
        .failure()
        .stdout(predicate::str::contains(r#""kind":"not-found""#));
}

#[test]
fn test_read_acts_aggregates_all_files() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Inbox task");
    env.write_actions("work.actions", "[ ] Work task");
    let project_dir = env.data_dir.join("charters").join("project1");
    fs::create_dir_all(&project_dir).unwrap();
    fs::write(project_dir.join("next.actions"), "[ ] Project task").unwrap();
    env.command()
        .arg("read")
        .arg("actions")
        .assert()
        .success()
        .stdout(predicate::str::contains("Inbox task"))
        .stdout(predicate::str::contains("Work task"))
        .stdout(predicate::str::contains("Project task"));
}

#[test]
fn test_read_acts_file_flag() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Inbox task");
    env.write_actions("work.actions", "[ ] Work task");
    let work_path = env.data_dir.join("charters").join("work.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--file")
        .arg(&work_path)
        .assert()
        .success()
        .stdout(predicate::str::contains("Work task"))
        .stdout(predicate::str::contains("Inbox task").not());
}

#[test]
fn test_read_acts_skips_hidden_directories() {
    let env = TestEnv::new();
    env.write_actions("inbox.actions", "[ ] Visible task");
    let hidden_dir = env.data_dir.join(".git");
    fs::create_dir_all(&hidden_dir).unwrap();
    fs::write(hidden_dir.join("state.actions"), "[ ] Hidden task").unwrap();
    env.command()
        .arg("read")
        .arg("actions")
        .assert()
        .success()
        .stdout(predicate::str::contains("Visible task"))
        .stdout(predicate::str::contains("Hidden task").not());
}

#[test]
fn test_read_acts_file_quarantines_malformed_semantics() {
    // Relaxed parsing still diagnoses the source, but recovered field/UUID
    // attachment is not trustworthy enough to enter semantic command output.
    let env = TestEnv::new();
    env.write_text(
        "charters/malformed.actions",
        "not valid actions syntax !!!\n[ ] Do not misattach me\n",
    );
    let path = env.data_dir.join("charters").join("malformed.actions");
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--file")
        .arg(&path)
        .assert()
        .success()
        .stderr(predicate::str::contains("file quarantined"))
        .stdout(predicate::str::contains("Do not misattach me").not());
}

#[test]
fn test_read_actions_context_filter_exact_match() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "[ ] Write tests +work\n[ ] Buy milk +personal\n",
    );
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--context")
        .arg("work")
        .assert()
        .success()
        .stdout(predicate::str::contains("Write tests"))
        .stdout(predicate::str::contains("Buy milk").not());
}

#[test]
fn test_read_actions_context_filter_expands_hierarchy() {
    let env = TestEnv::new();
    // Config: computer → terminal → neovim
    env.write_config(r#"{"tag_hierarchies": {"computer": ["terminal"], "terminal": ["neovim"]}}"#);
    env.write_actions(
        "inbox.actions",
        "[ ] Edit config +neovim\n[ ] Browse web +browser\n[ ] Read a book +personal\n",
    );
    // Filtering by +computer should match +neovim (child of terminal, which is child of computer)
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--context")
        .arg("computer")
        .assert()
        .success()
        .stdout(predicate::str::contains("Edit config"))
        .stdout(predicate::str::contains("Browse web").not())
        .stdout(predicate::str::contains("Read a book").not());
}

#[test]
fn test_read_actions_context_filter_multiple_flags() {
    let env = TestEnv::new();
    env.write_actions(
        "inbox.actions",
        "[ ] Work task +work\n[ ] Personal task +personal\n[ ] Other task +other\n",
    );
    env.command()
        .arg("read")
        .arg("actions")
        .arg("--context")
        .arg("work")
        .arg("--context")
        .arg("personal")
        .assert()
        .success()
        .stdout(predicate::str::contains("Work task"))
        .stdout(predicate::str::contains("Personal task"))
        .stdout(predicate::str::contains("Other task").not());
}
