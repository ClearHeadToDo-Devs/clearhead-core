//! Native persistence for the host-neutral workspace identity manifest.

use std::path::{Path, PathBuf};

use clearhead_core::workspace::{
    WorkspaceManifest, parse_workspace_manifest, render_workspace_manifest,
};

use crate::workspace_data_root;

/// Physical manifest path for either supported native workspace layout.
pub fn workspace_manifest_path(root: &Path) -> PathBuf {
    workspace_data_root(root).join("workspace.json")
}

/// Read workspace identity, degrading missing or malformed content to an empty manifest.
pub fn read_workspace_manifest(root: &Path) -> WorkspaceManifest {
    std::fs::read_to_string(workspace_manifest_path(root))
        .ok()
        .and_then(|source| parse_workspace_manifest(&source).ok())
        .unwrap_or_default()
}

/// Persist workspace identity and its schema pointer.
pub fn write_workspace_manifest(root: &Path, manifest: &WorkspaceManifest) -> std::io::Result<()> {
    let path = workspace_manifest_path(root);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let source = render_workspace_manifest(manifest).map_err(std::io::Error::other)?;
    std::fs::write(path, source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifest_roundtrips_through_native_layout() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir(root.path().join(".clearhead")).unwrap();
        let manifest = WorkspaceManifest {
            workspace_id: Some("019f0000-0000-7000-8000-000000000001".into()),
            workspace_name: Some("demo".into()),
            created_at: Some("2026-08-21".into()),
        };
        write_workspace_manifest(root.path(), &manifest).unwrap();
        assert_eq!(read_workspace_manifest(root.path()), manifest);
        let source = std::fs::read_to_string(workspace_manifest_path(root.path())).unwrap();
        assert!(source.contains("$schema"));
    }

    #[test]
    fn malformed_manifest_degrades_to_empty_identity() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir(root.path().join(".clearhead")).unwrap();
        std::fs::write(workspace_manifest_path(root.path()), "not json").unwrap();
        assert_eq!(
            read_workspace_manifest(root.path()),
            WorkspaceManifest::default()
        );
    }
}
