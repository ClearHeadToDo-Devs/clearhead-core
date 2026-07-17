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
        resolve_workspace_layout(root).data_root.join("workspace.json")
    }

    /// Read the manifest for the workspace at `root`.
    ///
    /// Identity lives only in `workspace.json`. A missing file yields an empty
    /// manifest — the workspace has no durable identity and the read side mints
    /// an ephemeral one per load. Never fails: a missing or unparseable file
    /// degrades to empty so an uninitialized or damaged workspace stays
    /// queryable; `doctor` reports the missing `workspace_id` separately.
    pub fn read(root: &Path) -> Self {
        read_identity_fields(&Self::path(root))
    }

    /// Write the manifest to `root`, stamping `$schema`. Creates the data-root
    /// directory if needed.
    pub fn write(&self, root: &Path) -> std::io::Result<()> {
        let path = Self::path(root);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let mut value = serde_json::to_value(self).unwrap_or_else(|_| serde_json::json!({}));
        if let Some(obj) = value.as_object_mut() {
            obj.insert(
                "$schema".to_string(),
                serde_json::Value::String(WORKSPACE_SCHEMA_URL.to_string()),
            );
        }
        let json = serde_json::to_string_pretty(&value)?;
        std::fs::write(&path, json)
    }
}

/// Extract the three identity fields from a JSON file, when present and parseable.
fn read_identity_fields(path: &Path) -> WorkspaceManifest {
    let Ok(text) = std::fs::read_to_string(path) else {
        return WorkspaceManifest::default();
    };
    match serde_json::from_str::<serde_json::Value>(&text) {
        Ok(v) => WorkspaceManifest {
            workspace_id: str_field(&v, "workspace_id"),
            workspace_name: str_field(&v, "workspace_name"),
            created_at: str_field(&v, "created_at"),
        },
        // A corrupt identity file degrades to "no durable identity" (ephemeral
        // id on read) rather than failing the load; `doctor` surfaces the
        // missing `workspace_id` so the damage is still reported.
        Err(_) => WorkspaceManifest::default(),
    }
}

fn str_field(v: &serde_json::Value, key: &str) -> Option<String> {
    v.get(key).and_then(|x| x.as_str()).map(|s| s.to_string())
}
