//! The per-workspace identity manifest (`.clearhead/workspace.json`).
//!
//! Identity is a tool-managed *fact* about one workspace — its durable
//! `workspace_id` (the RDF named-graph handle), display `workspace_name`, and
//! `created_at`. It lives in its own file, separate from `config.json`, because
//! it must NOT participate in the config precedence chain: a `workspace_id` in a
//! *global* config, or a `CLEARHEAD_WORKSPACE_ID` env override, is meaningless.
//! See `specifications/workspace.md#workspace-identity`.
//!
//! The manifest is near-static — written once by `init`, touched again only on
//! rename — and carries workspace-level facts only. Per-charter, per-action, and
//! per-plan metadata live in their co-located sidecars, never here.

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

use super::store::resolve_workspace_layout;

/// Published schema for `workspace.json`, stamped on write so editors validate.
pub const WORKSPACE_SCHEMA_URL: &str = "https://raw.githubusercontent.com/ClearHeadToDo-Devs/specifications/master/schemas/workspace.schema.json";

/// The identity facts that name a workspace and its RDF named graph.
///
/// Deserialized from `workspace.json`. Every field is optional so a partially
/// written or absent manifest still parses — an absent `workspace_id` simply
/// means the workspace has no durable identity yet.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct WorkspaceManifest {
    /// Durable UUID for this workspace's RDF named graph, assigned once by
    /// `clearhead init`. `None` → the read side mints an ephemeral id per load.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workspace_id: Option<String>,

    /// Display name for multi-workspace output and cross-workspace references.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workspace_name: Option<String>,

    /// ISO 8601 date the workspace was initialized. Informational only.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub created_at: Option<String>,
}

impl WorkspaceManifest {
    /// Path to the manifest for a workspace rooted at `root`, honoring both the
    /// project layout (`<root>/.clearhead/workspace.json`) and the user layout
    /// (`<root>/workspace.json`).
    pub fn path(root: &Path) -> PathBuf {
        resolve_workspace_layout(root)
            .data_root
            .join("workspace.json")
    }
}

/// Parse the host-neutral workspace identity document.
pub fn parse_workspace_manifest(source: &str) -> Result<WorkspaceManifest, serde_json::Error> {
    let value: serde_json::Value = serde_json::from_str(source)?;
    Ok(WorkspaceManifest {
        workspace_id: str_field(&value, "workspace_id"),
        workspace_name: str_field(&value, "workspace_name"),
        created_at: str_field(&value, "created_at"),
    })
}

/// Serialize workspace identity with the published schema pointer.
pub fn render_workspace_manifest(
    manifest: &WorkspaceManifest,
) -> Result<String, serde_json::Error> {
    let mut value = serde_json::to_value(manifest)?;
    if let Some(object) = value.as_object_mut() {
        object.insert(
            "$schema".to_string(),
            serde_json::Value::String(WORKSPACE_SCHEMA_URL.to_string()),
        );
    }
    serde_json::to_string_pretty(&value)
}

fn str_field(v: &serde_json::Value, key: &str) -> Option<String> {
    v.get(key).and_then(|x| x.as_str()).map(|s| s.to_string())
}
