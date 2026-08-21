mod common;
use common::TestEnv;

fn flatten_names(nodes: &[serde_json::Value], names: &mut Vec<String>) {
    for node in nodes {
        names.push(node["name"].as_str().expect("name").to_string());
        if let Some(children) = node.get("children").and_then(|value| value.as_array()) {
            flatten_names(children, names);
        }
    }
}

#[test]
fn work_map_emits_nested_charter_and_action_hierarchy() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] parent action #01900000-0000-7000-8000-000000000101\n\
         >[ ] child action #01900000-0000-7000-8000-000000000102\n\
         [ ] sibling action #01900000-0000-7000-8000-000000000103\n",
    );

    let output = env
        .command()
        .args(["query", "tree", "work-map", "--format", "json"])
        .output()
        .expect("run tree query");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let tree: serde_json::Value = serde_json::from_slice(&output.stdout).expect("nested JSON");
    let roots = tree.as_array().expect("root array");
    assert_eq!(roots.len(), 1, "one implicit charter root: {tree}");
    assert_eq!(roots[0]["kind"], "charter");

    let charter_children = roots[0]["children"].as_array().expect("charter children");
    let parent = charter_children
        .iter()
        .find(|node| node["name"] == "parent action")
        .expect("parent action");
    assert_eq!(parent["children"][0]["name"], "child action");

    let mut names = Vec::new();
    flatten_names(roots, &mut names);
    assert!(names.contains(&"sibling action".to_string()));
}

#[test]
fn tree_pipe_defaults_to_nested_json() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] action #01900000-0000-7000-8000-000000000105\n",
    );

    let output = env
        .command()
        .args(["query", "tree", "work-map"])
        .output()
        .expect("run tree query with captured stdout");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let tree: serde_json::Value = serde_json::from_slice(&output.stdout).expect("nested JSON");
    assert!(
        tree.is_array(),
        "tree machine output must be a JSON array: {tree}"
    );
}

#[test]
fn work_map_resolves_charter_parent_by_alias() {
    let env = TestEnv::new();
    env.with_workspace_identity();
    env.write_text(
        ".clearhead/charters/root/README.md",
        "---\nid: 01900000-0000-7000-8000-000000000110\nalias: root\n---\n# Root charter\n",
    );
    env.write_text(
        ".clearhead/charters/child/README.md",
        "---\nid: 01900000-0000-7000-8000-000000000111\nalias: child\nparent: root\n---\n# Child charter\n",
    );

    let output = env
        .command()
        .args(["query", "tree", "work-map", "--format", "json"])
        .output()
        .expect("run tree query");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let tree: serde_json::Value = serde_json::from_slice(&output.stdout).expect("nested JSON");
    let root = tree
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["name"] == "Root charter")
        .expect("root");
    assert_eq!(root["source_file"], "root/README.md");
    assert_eq!(root["children"][0]["name"], "Child charter");
    assert_eq!(root["children"][0]["source_file"], "child/README.md");
}

#[test]
fn work_map_uses_filesystem_hierarchy_for_markdown_only_charters() {
    let env = TestEnv::new();
    env.with_workspace_identity();
    env.write_text(
        ".clearhead/charters/someday/README.md",
        "---\nid: 01900000-0000-7000-8000-000000000120\nalias: someday\n---\n# Someday\n",
    );
    env.write_text(
        ".clearhead/charters/someday/agent-surface/README.md",
        "---\nid: 01900000-0000-7000-8000-000000000121\nalias: agent-surface\n---\n# Agent Surface\n",
    );

    let output = env
        .command()
        .args(["query", "tree", "work-map", "--format", "json"])
        .output()
        .expect("run tree query");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let tree: serde_json::Value = serde_json::from_slice(&output.stdout).expect("nested JSON");
    let someday = tree
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["name"] == "Someday")
        .expect("someday root");
    assert_eq!(someday["children"][0]["name"], "Agent Surface");
    assert_eq!(
        someday["children"][0]["source_file"],
        "someday/agent-surface/README.md"
    );
}

#[test]
fn project_tree_query_must_satisfy_contract() {
    let env = TestEnv::new();
    env.with_workspace_identity().write_actions(
        "next.actions",
        "[ ] action #01900000-0000-7000-8000-000000000104\n",
    );
    env.write_text(
        ".clearhead/queries/tree/bad.sparql",
        "SELECT (\"urn:uuid:test\" AS ?id) (\"missing kind\" AS ?name) WHERE {}",
    );

    env.command()
        .args(["query", "tree", "bad", "--format", "json"])
        .assert()
        .failure()
        .stderr(predicates::str::contains("tree contract"))
        .stderr(predicates::str::contains("kind"));
}

#[test]
fn work_map_is_listed_and_inspectable_as_standard_sparql() {
    let env = TestEnv::new();
    env.command()
        .args(["query", "list"])
        .assert()
        .success()
        .stdout(predicates::str::contains("work-map"))
        .stdout(predicates::str::contains("tree"));
    env.command()
        .args(["query", "show", "work-map"])
        .assert()
        .success()
        .stdout(predicates::str::contains("SELECT"))
        .stdout(predicates::str::contains("?parent"));
}
