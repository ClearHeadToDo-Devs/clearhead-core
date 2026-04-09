//! Integration tests for the workspace store (load/save pipeline).
//!
//! These tests exercise the full path: `.actions` files on disk → `DomainModel` → back to disk.
//! Each test creates an isolated temp workspace so there are no shared-state concerns.

use clearhead_core::{load_domain_model, save_domain_model};
use clearhead_core::sync::domain_semantically_equal;
use std::fs;
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

    let charter = model.charters.iter().find(|c| c.title == "tasks").expect("charter not found");
    assert!(charter.parent.is_some(), "nested charter should have a parent");
}

#[test]
fn project_layout_next_actions_uses_project_name_as_charter() {
    // In project layout, `next.actions` at the root of `.clearhead/` is the
    // "primary" file — its charter name becomes the project directory name,
    // not "next".
    let (_outer, project) = make_named_project(
        "my-project",
        &[("next.actions", "[ ] Root task #01951111-0000-7000-0000-000000000050\n")],
    );

    let model = load_domain_model(&project).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    assert_eq!(model.charters[0].title, "my-project");
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
