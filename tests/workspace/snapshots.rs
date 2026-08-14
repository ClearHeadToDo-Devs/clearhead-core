use super::common::*;
use clearhead_core::{ManifestSourceType, collect_workspace_manifest, load_domain_model};

// =============================================================================
// Fixture-based tests (checked-in workspace trees)
// =============================================================================
//
// These tests load from real fixture directories so that the expected semantic
// model state is visible on disk alongside the test assertions.
//
// RON snapshots live next to the fixtures under `tests/fixtures/workspace/`.
// On first run (no snapshot file) the test writes the snapshot; on subsequent
// runs it asserts byte-for-byte equality. Set `UPDATE_SNAPSHOTS=1` to
// regenerate a snapshot after an intentional model change.

#[test]
fn fixture_user_flat_charter_names_and_action_counts() {
    let root = fixture_path("user-flat");
    let model = load_domain_model(&root).expect("load failed");

    let mut names: Vec<String> = model.charters.iter().map(|c| c.title.clone()).collect();
    names.sort();
    assert_eq!(names, vec!["Work", "personal"]);

    let work = model.charters.iter().find(|c| c.title == "Work").unwrap();
    assert_eq!(work.actions.len(), 3, "work: 2 top-level + 1 subtask");

    let personal = model
        .charters
        .iter()
        .find(|c| c.title == "personal")
        .unwrap();
    assert_eq!(personal.actions.len(), 2);
}

#[test]
fn fixture_user_flat_ron_snapshot() {
    let root = fixture_path("user-flat");
    let model = load_domain_model(&root).expect("load failed");
    let ron = model_to_ron(&model);
    let snapshot = fixture_path("user-flat.ron");
    assert_snapshot(&snapshot, &ron);
}

#[test]
fn fixture_project_nested_parent_links() {
    let root = fixture_path("project-nested");
    let model = load_domain_model(&root).expect("load failed");

    let mut names: Vec<String> = model.charters.iter().map(|c| c.title.clone()).collect();
    names.sort();
    assert_eq!(names, vec!["ops", "project-nested", "work"]);

    let work = model
        .charters
        .iter()
        .find(|c| c.title == "work")
        .expect("work charter");
    assert_eq!(
        work.parent.as_deref(),
        Some("project-nested"),
        "work should be a child of the project root"
    );

    let ops = model
        .charters
        .iter()
        .find(|c| c.title == "ops")
        .expect("ops charter");
    assert_eq!(
        ops.parent.as_deref(),
        Some("work"),
        "ops should be a child of work"
    );
}

#[test]
fn fixture_project_nested_ron_snapshot() {
    let root = fixture_path("project-nested");
    let model = load_domain_model(&root).expect("load failed");
    let ron = model_to_ron(&model);
    let snapshot = fixture_path("project-nested.ron");
    assert_snapshot(&snapshot, &ron);
}

#[test]
fn fixture_md_merge_title_alias_and_description() {
    let root = fixture_path("md-merge");
    let model = load_domain_model(&root).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    let charter = &model.charters[0];
    assert_eq!(charter.title, "Health & Fitness");
    assert_eq!(charter.alias.as_deref(), Some("health"));
    assert_eq!(charter.actions.len(), 2);
    assert!(
        charter.description.is_some(),
        "description should be populated from .md body"
    );
}

#[test]
fn fixture_md_merge_ron_snapshot() {
    let root = fixture_path("md-merge");
    let model = load_domain_model(&root).expect("load failed");
    let ron = model_to_ron(&model);
    let snapshot = fixture_path("md-merge.ron");
    assert_snapshot(&snapshot, &ron);
}

// =============================================================================
// Workspace manifest tests
// =============================================================================

#[test]
fn fixture_user_flat_manifest() {
    let root = fixture_path("user-flat");
    let mut manifest = collect_workspace_manifest(&root).expect("manifest failed");
    manifest.sort_by(|a, b| a.path.cmp(&b.path));

    assert_eq!(manifest.len(), 2);

    let personal = manifest
        .iter()
        .find(|e| e.charter_name == "personal")
        .unwrap();
    assert_eq!(personal.source_type, ManifestSourceType::Actions);
    assert!(personal.inferred_parent.is_none());

    let work = manifest.iter().find(|e| e.charter_name == "work").unwrap();
    assert_eq!(work.source_type, ManifestSourceType::ActionsPlusMarkdown);
    assert!(work.inferred_parent.is_none());

    let ron = manifest_to_ron(&manifest);
    assert_snapshot(&fixture_path("user-flat-manifest.ron"), &ron);
}

#[test]
fn fixture_project_nested_manifest() {
    let root = fixture_path("project-nested");
    let manifest = collect_workspace_manifest(&root).expect("manifest failed");

    assert_eq!(manifest.len(), 3);

    let root_entry = manifest
        .iter()
        .find(|e| e.charter_name == "project-nested")
        .unwrap();
    assert!(
        root_entry.inferred_parent.is_none(),
        "project root has no parent"
    );

    let work_entry = manifest.iter().find(|e| e.charter_name == "work").unwrap();
    assert_eq!(
        work_entry.inferred_parent.as_deref(),
        Some("project-nested")
    );

    let ops_entry = manifest.iter().find(|e| e.charter_name == "ops").unwrap();
    assert_eq!(ops_entry.inferred_parent.as_deref(), Some("work"));

    let ron = manifest_to_ron(&manifest);
    assert_snapshot(&fixture_path("project-nested-manifest.ron"), &ron);
}

#[test]
fn fixture_md_merge_manifest_source_type() {
    let root = fixture_path("md-merge");
    let manifest = collect_workspace_manifest(&root).expect("manifest failed");

    assert_eq!(manifest.len(), 1);
    assert_eq!(manifest[0].charter_name, "health");
    assert_eq!(
        manifest[0].source_type,
        ManifestSourceType::ActionsPlusMarkdown
    );

    let ron = manifest_to_ron(&manifest);
    assert_snapshot(&fixture_path("md-merge-manifest.ron"), &ron);
}
