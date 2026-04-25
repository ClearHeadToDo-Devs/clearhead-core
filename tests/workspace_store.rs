//! Integration tests for the workspace store (load/save pipeline).
//!
//! These tests exercise the full path: `.actions` files on disk → `DomainModel` → back to disk.
//! Each test creates an isolated temp workspace so there are no shared-state concerns.

use clearhead_core::sync::domain_semantically_equal;
use clearhead_core::{
    ManifestSourceType, collect_workspace_manifest, load_domain_model, save_domain_model,
};
use std::fs;
use std::path::Path;
use tempfile::TempDir;

// --- Fixture helpers ---

/// Create a temp workspace with the given files under `.clearhead/`.
///
/// Project-level layout: `resolve_workspace_layout` finds `.clearhead/` and
/// uses the root directory name as `project_root_charter`.
fn make_workspace(files: &[(&str, &str)]) -> TempDir {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let data = dir.path().join(".clearhead");
    fs::create_dir_all(&data).expect("failed to create .clearhead dir");
    for (name, content) in files {
        fs::write(data.join(name), content).expect("failed to write fixture file");
    }
    dir
}

/// Create a named project directory inside a temp dir.
///
/// Useful when you need the project name to be predictable (e.g. "my-project")
/// rather than the random name `tempdir()` generates.
fn make_named_project(name: &str, files: &[(&str, &str)]) -> (TempDir, std::path::PathBuf) {
    let outer = tempfile::tempdir().expect("failed to create temp dir");
    let project = outer.path().join(name);
    let data = project.join(".clearhead");
    fs::create_dir_all(&data).expect("failed to create project dir");
    for (filename, content) in files {
        fs::write(data.join(filename), content).expect("failed to write fixture file");
    }
    (outer, project)
}

/// User-level layout: files live directly in root, no `.clearhead/` subdirectory.
/// `project_root_charter` will be `None` — charter names come purely from filenames.
fn make_user_workspace(files: &[(&str, &str)]) -> TempDir {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    for (name, content) in files {
        fs::write(dir.path().join(name), content).expect("failed to write fixture file");
    }
    dir
}

// --- Tests ---

#[test]
fn roundtrip_preserves_model() {
    // Fixture uses explicit UUIDs so IDs are stable across loads.
    let workspace = make_workspace(&[(
        "tasks.actions",
        "[ ] Task one #01951111-0000-7000-0000-000000000001\n\
         [ ] Task two #01951111-0000-7000-0000-000000000002\n\
         > [ ] Subtask of two #01951111-0000-7000-0000-000000000003\n",
    )]);

    let model_a = load_domain_model(workspace.path()).expect("first load failed");
    save_domain_model(workspace.path(), &model_a).expect("save failed");
    let model_b = load_domain_model(workspace.path()).expect("second load failed");

    assert!(
        domain_semantically_equal(&model_a, &model_b),
        "model changed across a save/reload cycle"
    );
}

#[test]
fn load_discovers_all_action_files() {
    // Two files → two charters, three plans total.
    let workspace = make_workspace(&[
        (
            "work.actions",
            "[ ] Write tests #01951111-0000-7000-0000-000000000010\n\
             [ ] Review PR #01951111-0000-7000-0000-000000000011\n",
        ),
        (
            "personal.actions",
            "[ ] Buy groceries #01951111-0000-7000-0000-000000000020\n",
        ),
    ]);

    let model = load_domain_model(workspace.path()).expect("load failed");

    assert_eq!(model.charters.len(), 2, "expected 2 charters");
    assert_eq!(model.all_plans().len(), 3, "expected 3 plans total");
}

#[test]
fn load_infers_parent_charter_from_directory_structure() {
    // A file nested under a subdirectory should infer a parent charter.
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let data = dir.path().join(".clearhead");
    let nested = data.join("project");
    fs::create_dir_all(&nested).expect("failed to create nested dir");
    fs::write(
        nested.join("tasks.actions"),
        "[ ] Nested task #01951111-0000-7000-0000-000000000030\n",
    )
    .expect("failed to write fixture");

    let model = load_domain_model(dir.path()).expect("load failed");

    let charter = model
        .charters
        .iter()
        .find(|c| c.title == "tasks")
        .expect("charter not found");
    assert!(
        charter.parent.is_some(),
        "nested charter should have a parent"
    );
}

#[test]
fn project_layout_next_actions_uses_project_name_as_charter() {
    // In project layout, `next.actions` at the root of `.clearhead/` is the
    // "primary" file — its charter name becomes the project directory name,
    // not "next".
    let (_outer, project) = make_named_project(
        "my-project",
        &[(
            "next.actions",
            "[ ] Root task #01951111-0000-7000-0000-000000000050\n",
        )],
    );

    let model = load_domain_model(&project).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    assert_eq!(model.charters[0].title, "my-project");
}

#[test]
fn project_layout_next_ics_uses_project_name_as_charter() {
    let (_outer, project) = make_named_project(
        "my-project",
        &[
            (
                "next.actions",
                "[ ] Root task #01951111-0000-7000-0000-000000000051\n",
            ),
            (
                "next.ics",
                "BEGIN:VCALENDAR\n\
VERSION:2.0\n\
PRODID:-//clearhead//NONSGML v1.0//EN\n\
BEGIN:VEVENT\n\
UID:root-plan-1\n\
DTSTART:20260101T080000Z\n\
SUMMARY:Project root plan\n\
END:VEVENT\n\
END:VCALENDAR\n",
            ),
        ],
    );

    let model = load_domain_model(&project).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    assert_eq!(model.charters[0].alias.as_deref(), Some("my-project"));
    assert_eq!(model.charters[0].plans.len(), 2);
    assert!(
        model.charters[0]
            .plans
            .iter()
            .any(|plan| plan.name == "Project root plan"),
        "root next.ics plan should be attached to the project charter"
    );

    let manifest = collect_workspace_manifest(&project).expect("manifest failed");
    assert_eq!(manifest.len(), 1);
    assert_eq!(manifest[0].path, "next.actions");
    assert_eq!(manifest[0].charter_name, "my-project");
    assert_eq!(
        manifest[0].source_type,
        ManifestSourceType::ActionsPlusIcs
    );
}

#[test]
fn user_layout_uses_filename_as_charter() {
    // In user layout (no `.clearhead/`), there is no special project root —
    // every file's stem becomes the charter name directly.
    let workspace = make_user_workspace(&[(
        "next.actions",
        "[ ] User task #01951111-0000-7000-0000-000000000060\n",
    )]);

    let model = load_domain_model(workspace.path()).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    assert_eq!(model.charters[0].title, "next");
}

#[test]
fn roundtrip_is_stable_across_multiple_cycles() {
    // Repeated save/reload should converge — not drift on each cycle.
    let workspace = make_workspace(&[(
        "tasks.actions",
        "[ ] Stable task #01951111-0000-7000-0000-000000000040\n",
    )]);

    let model_a = load_domain_model(workspace.path()).expect("load failed");
    save_domain_model(workspace.path(), &model_a).expect("first save failed");

    let model_b = load_domain_model(workspace.path()).expect("second load failed");
    save_domain_model(workspace.path(), &model_b).expect("second save failed");

    let model_c = load_domain_model(workspace.path()).expect("third load failed");

    assert!(
        domain_semantically_equal(&model_b, &model_c),
        "model drifted between save cycles"
    );
}

// --- Explicit charter (.md) + implicit (.actions) merge tests ---

#[test]
fn explicit_charter_title_does_not_overwrite_alias() {
    // An explicit .md file with a human-readable title and NO alias in frontmatter
    // should NOT clobber the alias set by implicit_charter() during .actions loading.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("health.actions"),
        "[ ] Morning run #01951111-0000-7000-0000-000000000100\n",
    )
    .expect("write actions");
    fs::write(
        data.join("health.md"),
        "# Health & Fitness\n\nStay healthy.\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");
    let charter = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("health"))
        .expect("health charter not found by alias");

    assert_eq!(
        charter.title, "Health & Fitness",
        "title should be human-readable"
    );
    assert_eq!(
        charter.alias,
        Some("health".to_string()),
        "alias should be the inferred filesystem name"
    );
}

#[test]
fn explicit_charter_with_alias_in_frontmatter_overrides_correctly() {
    // An explicit .md with `alias: fitness` should override the inferred alias ("h").
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("h.actions"),
        "[ ] Morning run #01951111-0000-7000-0000-000000000110\n",
    )
    .expect("write actions");
    fs::write(
        data.join("h.md"),
        "---\nalias: fitness\n---\n# Health & Fitness\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");
    let charter = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("fitness"))
        .expect("charter not found by explicit alias");

    assert_eq!(charter.title, "Health & Fitness");
    assert_eq!(charter.alias, Some("fitness".to_string()));
}

#[test]
fn alias_is_always_set_after_load() {
    // alias should be Some(...) for every charter regardless of whether it has
    // an explicit .md file, an aliased .md file, or no .md at all.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead");
    fs::create_dir_all(&data).expect("create .clearhead");

    // Implicit only
    fs::write(
        data.join("implicit.actions"),
        "[ ] Task #01951111-0000-7000-0000-000000000120\n",
    )
    .expect("write actions");

    // Explicit with alias
    fs::write(
        data.join("explicit.actions"),
        "[ ] Task #01951111-0000-7000-0000-000000000121\n",
    )
    .expect("write actions");
    fs::write(
        data.join("explicit.md"),
        "---\nalias: ex\n---\n# Explicit Charter\n",
    )
    .expect("write md");

    // Explicit without alias
    fs::write(
        data.join("noalias.actions"),
        "[ ] Task #01951111-0000-7000-0000-000000000122\n",
    )
    .expect("write actions");
    fs::write(data.join("noalias.md"), "# No Alias Charter\n").expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");

    for charter in &model.charters {
        assert!(
            charter.alias.is_some(),
            "charter '{}' has no alias — invariant violated",
            charter.title
        );
    }
}

#[test]
fn parent_reference_uses_machine_key_not_title() {
    // When a parent charter has a human-readable title (from .md), child charters
    // discovered via path inference should still have parent = machine key (inferred name).
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead");
    let work_sub = data.join("work");
    fs::create_dir_all(&work_sub).expect("create work subdir");

    fs::write(
        data.join("next.actions"),
        "[ ] Root task #01951111-0000-7000-0000-000000000130\n",
    )
    .expect("write root actions");
    fs::write(
        data.join("work.actions"),
        "[ ] Work task #01951111-0000-7000-0000-000000000131\n",
    )
    .expect("write work actions");
    // Explicit .md overwrites title with human-readable string, no alias
    fs::write(data.join("work.md"), "# Work Stuff\n\nAll work items.\n").expect("write work md");
    fs::write(
        work_sub.join("ops.actions"),
        "[ ] Ops task #01951111-0000-7000-0000-000000000132\n",
    )
    .expect("write ops actions");

    let model = load_domain_model(dir.path()).expect("load failed");

    let ops = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("ops") || c.title == "ops")
        .expect("ops charter not found");

    assert_eq!(
        ops.parent.as_deref(),
        Some("work"),
        "parent should be machine key 'work', not title 'Work Stuff'"
    );

    // Verify the work charter itself got the human-readable title
    let work = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("work"))
        .expect("work charter not found");
    assert_eq!(work.title, "Work Stuff");
}

#[test]
fn load_md_only_charter_produces_empty_plan_list() {
    // A .md file with no matching .actions file should produce a charter with zero plans,
    // not be silently dropped.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("health.md"),
        "---\nalias: health\n---\n# Health & Fitness\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");

    let charter = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("health"))
        .expect("health charter should be present even without .actions file");

    assert_eq!(
        charter.plans.len(),
        0,
        "charter from .md-only should have no plans"
    );
    assert_eq!(charter.title, "Health & Fitness");
}

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

fn fixture_path(name: &str) -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/workspace")
        .join(name)
}

/// Serialize `model` to deterministic RON.
///
/// Charters are sorted by title so the output is stable regardless of
/// HashMap iteration order during loading.
fn model_to_ron(model: &clearhead_core::DomainModel) -> String {
    let mut sorted = model.clone();
    sorted.charters.sort_by(|a, b| a.title.cmp(&b.title));
    for charter in &mut sorted.charters {
        charter.plans.sort_by(|a, b| a.id.cmp(&b.id));
    }
    ron::ser::to_string_pretty(&sorted, ron::ser::PrettyConfig::default())
        .expect("RON serialization failed")
}

/// Serialize a manifest to deterministic RON.
///
/// Entries are sorted by path so the output is stable across runs.
fn manifest_to_ron(manifest: &[clearhead_core::WorkspaceManifestEntry]) -> String {
    let mut sorted = manifest.to_vec();
    sorted.sort_by(|a, b| a.path.cmp(&b.path));
    ron::ser::to_string_pretty(&sorted, ron::ser::PrettyConfig::default())
        .expect("RON serialization failed")
}

/// Assert `actual` matches the snapshot at `path`, creating it if absent.
fn assert_snapshot(snapshot_path: &Path, actual: &str) {
    if !snapshot_path.exists() || std::env::var("UPDATE_SNAPSHOTS").is_ok() {
        fs::write(snapshot_path, actual).expect("failed to write snapshot");
        return;
    }
    let expected = fs::read_to_string(snapshot_path).expect("failed to read snapshot");
    assert_eq!(
        actual,
        expected,
        "snapshot mismatch — run with UPDATE_SNAPSHOTS=1 to regenerate: {}",
        snapshot_path.display()
    );
}

#[test]
fn fixture_user_flat_charter_names_and_plan_counts() {
    let root = fixture_path("user-flat");
    let model = load_domain_model(&root).expect("load failed");

    let mut names: Vec<String> = model.charters.iter().map(|c| c.title.clone()).collect();
    names.sort();
    assert_eq!(names, vec!["personal", "work"]);

    let work = model.charters.iter().find(|c| c.title == "work").unwrap();
    assert_eq!(work.plans.len(), 3, "work: 2 top-level + 1 subtask");

    let personal = model
        .charters
        .iter()
        .find(|c| c.title == "personal")
        .unwrap();
    assert_eq!(personal.plans.len(), 2);
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
    assert_eq!(charter.plans.len(), 2);
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
    assert_eq!(work.source_type, ManifestSourceType::Actions);
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

#[test]
fn load_recovers_valid_actions_when_file_has_parse_issues() {
    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Valid one #01961111-0000-7000-0000-000000000001\n\
         this line is malformed and should not be parsed\n\
         [ ] Valid two #01961111-0000-7000-0000-000000000002\n",
    )]);

    let model = load_domain_model(workspace.path()).expect("load should recover valid actions");
    let work = model
        .charters
        .iter()
        .find(|c| c.title == "work")
        .expect("work charter");

    assert_eq!(work.plans.len(), 2, "valid actions should still be loaded");
    assert!(work.plans.iter().any(|p| p.name == "Valid one"));
    assert!(work.plans.iter().any(|p| p.name == "Valid two"));
}

#[test]
fn unresolvable_parent_does_not_crash_load() {
    // A charter with `parent: "Work Stuff"` (display title) should load
    // successfully — the unresolvable parent only emits a warning, it does
    // not abort. We can't easily capture stderr here, so we just assert
    // the load succeeds and the parent string is preserved as-is.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("child.actions"),
        "[ ] A task #01960000-9999-7000-0000-000000000001\n",
    )
    .expect("write actions");
    fs::write(
        data.join("child.md"),
        "---\nparent: Work Stuff\n---\n# Child Charter\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load should succeed despite bad parent");
    let child = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("child"))
        .expect("child charter");
    assert_eq!(
        child.parent.as_deref(),
        Some("Work Stuff"),
        "bad parent should be preserved, not silently dropped"
    );
}

#[test]
fn mixed_workspace_loads_actions_and_ics_plans() {
    let root = fixture_path("project-mixed");
    let model = load_domain_model(&root).expect("load failed");

    let mut names: Vec<String> = model
        .charters
        .iter()
        .map(|c| c.alias.clone().unwrap_or_default())
        .collect();
    names.sort();
    assert_eq!(names, vec!["health", "project-mixed"]);

    let project = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("project-mixed"))
        .unwrap();
    assert_eq!(project.plans.len(), 1);
    assert_eq!(project.plans[0].name, "Buy domain name");

    let health = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("health"))
        .unwrap();
    assert_eq!(health.plans.len(), 1);

    let plan = &health.plans[0];
    assert_eq!(plan.name, "Go for a run");
    assert_eq!(plan.external_id.as_deref(), Some("health-workout-1"));
    assert_eq!(plan.template_name.as_deref(), Some("workout"));
    assert!(plan.recurrence.is_some(), "recurrence should be populated");
}

#[test]
fn mixed_workspace_ron_snapshots() {
    let root = fixture_path("project-mixed");
    let model = load_domain_model(&root).expect("load failed");
    let ron = model_to_ron(&model);
    assert_snapshot(&fixture_path("project-mixed.ron"), &ron);

    let manifest = collect_workspace_manifest(&root).expect("manifest failed");
    let manifest_ron = manifest_to_ron(&manifest);
    assert_snapshot(&fixture_path("project-mixed-manifest.ron"), &manifest_ron);
}
