//! Shared fixture helpers for the workspace-store integration tests.
//!
//! These integration tests exercise the native loader that now lives in this
//! adapter crate. The one-argument shims below delegate to the two-argument
//! `clearhead_workspace_fs` loaders (external plans default to `None`) so the
//! test bodies read the same as when the loader lived in Core.

use clearhead_core::DomainModel;
use clearhead_core::workspace::{MarkdownCharter, WorkspaceError, WorkspaceRead};
use std::fs;
use std::path::Path;
use tempfile::TempDir;

pub use clearhead_workspace_fs::{
    ManifestSourceType, WorkspaceManifestEntry, collect_workspace_manifest,
};

/// Load a workspace's domain model from the native filesystem (no external plans mount).
pub fn load_domain_model(root: &Path) -> Result<DomainModel, WorkspaceError> {
    clearhead_workspace_fs::load_domain_model(root, None)
}

/// Load a workspace's charter list from the native filesystem.
pub fn load_workspace(root: &Path) -> Result<Vec<MarkdownCharter>, WorkspaceError> {
    clearhead_workspace_fs::load_workspace(root, None)
}

/// Relaxed read of a workspace from the native filesystem.
pub fn read_workspace(root: &Path) -> Result<WorkspaceRead, WorkspaceError> {
    clearhead_workspace_fs::read_workspace(root, None)
}

pub fn make_workspace(files: &[(&str, &str)]) -> TempDir {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let data = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(&data).expect("failed to create .clearhead dir");
    for (name, content) in files {
        let path = data.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create fixture parent");
        }
        fs::write(path, content).expect("failed to write fixture file");
    }
    dir
}

/// Create a named project directory inside a temp dir.
///
/// Useful when you need the project name to be predictable (e.g. "my-project")
/// rather than the random name `tempdir()` generates.
pub fn make_named_project(name: &str, files: &[(&str, &str)]) -> (TempDir, std::path::PathBuf) {
    let outer = tempfile::tempdir().expect("failed to create temp dir");
    let project = outer.path().join(name);
    let data = project.join(".clearhead").join("charters");
    fs::create_dir_all(&data).expect("failed to create project dir");
    for (filename, content) in files {
        let path = data.join(filename);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create fixture parent");
        }
        fs::write(path, content).expect("failed to write fixture file");
    }
    (outer, project)
}

/// User-level layout: files live directly in root, no `.clearhead/` subdirectory.
/// `project_root_charter` will be `None` — charter names come purely from filenames.
pub fn make_user_workspace(files: &[(&str, &str)]) -> TempDir {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let data = dir.path().join("charters");
    fs::create_dir_all(&data).expect("failed to create charters dir");
    for (name, content) in files {
        let path = data.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create fixture parent");
        }
        fs::write(path, content).expect("failed to write fixture file");
    }
    dir
}
pub fn fixture_path(name: &str) -> std::path::PathBuf {
    // Fixtures and RON snapshots live in the Core crate's test tree (shared with
    // Core's parser tests via `include_str!`); reference them from there rather
    // than duplicating the corpus into this adapter crate.
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../tests/fixtures/workspace")
        .join(name)
}

/// Serialize `model` to deterministic RON.
///
/// Charters are sorted by title so the output is stable regardless of
/// HashMap iteration order during loading.
pub fn model_to_ron(model: &clearhead_core::DomainModel) -> String {
    let mut sorted = model.clone();
    sorted.charters.sort_by(|a, b| a.title.cmp(&b.title));
    for charter in &mut sorted.charters {
        charter.plans.sort_by_key(|a| a.id);
        charter.actions.sort_by_key(|a| a.id);
    }
    ron::ser::to_string_pretty(&sorted, ron::ser::PrettyConfig::default())
        .expect("RON serialization failed")
}

/// Serialize a manifest to deterministic RON.
///
/// Entries are sorted by path so the output is stable across runs.
pub fn manifest_to_ron(manifest: &[WorkspaceManifestEntry]) -> String {
    let mut sorted = manifest.to_vec();
    sorted.sort_by(|a, b| a.path.cmp(&b.path));
    ron::ser::to_string_pretty(&sorted, ron::ser::PrettyConfig::default())
        .expect("RON serialization failed")
}

/// Assert `actual` matches the snapshot at `path`, creating it if absent.
pub fn assert_snapshot(snapshot_path: &Path, actual: &str) {
    if !snapshot_path.exists() || std::env::var("UPDATE_SNAPSHOTS").is_ok() {
        fs::write(snapshot_path, actual).expect("failed to write snapshot");
        return;
    }
    let expected = fs::read_to_string(snapshot_path).expect("failed to read snapshot");
    assert_eq!(
        actual.trim_end(),
        expected.trim_end(),
        "snapshot mismatch — run with UPDATE_SNAPSHOTS=1 to regenerate: {}",
        snapshot_path.display()
    );
}
