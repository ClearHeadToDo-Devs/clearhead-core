//! Per-charter `.<charter>.json` sidecar for machine-oriented metadata.
//!
//! Hidden JSON files that live alongside `.actions` files, holding data
//! that tooling needs but humans don't want cluttering the DSL:
//! created timestamps, VEVENT linkage, etc.

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use super::store::WorkspaceError;

/// Root of the per-charter sidecar JSON (`.<charter>.json`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CharterMetadata {
    /// Charter-level metadata (creation timestamp).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub charter: Option<CharterMeta>,
    /// Per-action metadata keyed by UUID string.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub acts: HashMap<String, ActMeta>,
    /// Per-plan metadata keyed by UUID string.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub plans: HashMap<String, PlanMeta>,
}

/// Charter-level sidecar metadata.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CharterMeta {
    /// When this charter was first created by tooling.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub created: Option<DateTime<Local>>,
}

/// Per-action sidecar metadata.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ActMeta {
    /// When this action was first created by tooling.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub created: Option<DateTime<Local>>,
    /// VEVENT UID this action was generated from (links back to the ICS source).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_vevent: Option<String>,
    /// The action's `scheduled_at` as of the last reconcile — the **B** column,
    /// the three-way merge base against the action (A) and the `.ics` (C).
    /// Machine-owned: only the reconcile engine may move it (see decision 31).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub scheduled_at_sync: Option<DateTime<Local>>,
    /// The action's `due_date` as of the last reconcile — the **B** column for
    /// the deadline. Machine-owned merge base; see `scheduled_at_sync`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub due_date_sync: Option<DateTime<Local>>,
}

/// Per-plan sidecar metadata.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PlanMeta {
    /// When `expand acts` last ran for this plan.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub last_expanded: Option<DateTime<Local>>,
}

/// Derive the sidecar path from an `.actions` file path.
///
/// - `inbox.actions`                → `.inbox.json`
/// - `health.actions`               → `.health.json`
/// - `work/next.actions`            → `work/.next.json`
/// - `work/feature/next.actions`    → `work/feature/.next.json`
pub fn sidecar_path(actions_path: &Path) -> PathBuf {
    let stem = actions_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");
    let dir = actions_path.parent().unwrap_or(Path::new(""));
    dir.join(format!(".{}.json", stem))
}

/// Read sidecar metadata from disk. Returns default if the file doesn't exist.
pub fn read_sidecar(path: &Path) -> Result<CharterMetadata, WorkspaceError> {
    if !path.exists() {
        return Ok(CharterMetadata::default());
    }
    let content = std::fs::read_to_string(path)?;
    serde_json::from_str(&content).map_err(|e| WorkspaceError::Parse(format!("sidecar: {}", e)))
}

/// Hydrate acts with metadata from the sidecar.
///
/// For each act, if the sidecar has a matching entry (by UUID string key),
/// fills in `created_at` and `external_schedule_id` where the act doesn't
/// already have them (DSL values are authoritative).
///
/// The merge-base copies (`scheduled_at_sync` / `due_date_sync`) have no DSL
/// form at all — the sidecar is their sole source, so they are assigned
/// directly rather than filled-if-absent.
pub fn hydrate_acts(acts: &mut [crate::workspace::actions::repository::SourcedAction], metadata: &CharterMetadata) {
    for sa in acts.iter_mut() {
        let act = &mut sa.action;
        let key = act
            .plan_id
            .map(|id| id.to_string())
            .unwrap_or_else(|| act.id.to_string());
        if let Some(meta) = metadata.acts.get(&key) {
            if act.created_at.is_none() {
                act.created_at = meta.created;
            }
            if act.external_schedule_id.is_none() {
                act.external_schedule_id = meta.source_vevent.clone();
            }
            act.scheduled_at_sync = meta.scheduled_at_sync;
            act.due_date_sync = meta.due_date_sync;
        }
    }
}

/// Stamp `created` in the sidecar for any actions that don't already have an entry.
///
/// Uses the UUIDv7 embedded timestamp as the creation time — more accurate than
/// wall-clock time at save because the UUID is generated when the action is first typed.
pub fn stamp_sidecar_entries(
    actions_path: &Path,
    actions: &[crate::domain::Action],
) -> Result<(), WorkspaceError> {
    let sc_path = sidecar_path(actions_path);
    let mut meta = read_sidecar(&sc_path)?;
    for action in actions {
        let key = action.id.to_string();
        meta.acts.entry(key).or_insert_with(|| ActMeta {
            created: Some(created_from_uuid(action.id).unwrap_or_else(Local::now)),
            ..Default::default()
        });
    }
    write_sidecar(&sc_path, &meta)
}

/// Extract the creation timestamp embedded in a UUIDv7.
fn created_from_uuid(id: uuid::Uuid) -> Option<DateTime<Local>> {
    let timestamp_ms = (id.as_u128() >> 80) as i64;
    DateTime::from_timestamp(timestamp_ms / 1000, ((timestamp_ms % 1000) * 1_000_000) as u32)
        .map(|dt| dt.into())
}

/// Write sidecar metadata to disk, atomically.
pub fn write_sidecar(path: &Path, metadata: &CharterMetadata) -> Result<(), WorkspaceError> {
    let content =
        serde_json::to_string_pretty(metadata).map_err(|e| WorkspaceError::Parse(e.to_string()))?;
    super::durability::atomic_write(path, content.as_bytes())?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    // ===== Path derivation =====

    #[test]
    fn sidecar_path_simple_charter() {
        let path = sidecar_path(Path::new("inbox.actions"));
        assert_eq!(path, PathBuf::from(".inbox.json"));
    }

    #[test]
    fn sidecar_path_named_charter() {
        let path = sidecar_path(Path::new("health.actions"));
        assert_eq!(path, PathBuf::from(".health.json"));
    }

    #[test]
    fn sidecar_path_directory_form() {
        let path = sidecar_path(Path::new("work/next.actions"));
        assert_eq!(path, PathBuf::from("work/.next.json"));
    }

    #[test]
    fn sidecar_path_nested_charter() {
        let path = sidecar_path(Path::new("work/feature/next.actions"));
        assert_eq!(path, PathBuf::from("work/feature/.next.json"));
    }

    #[test]
    fn sidecar_path_non_next_in_directory() {
        let path = sidecar_path(Path::new("work/bugs.actions"));
        assert_eq!(path, PathBuf::from("work/.bugs.json"));
    }

    // ===== Roundtrip serialization =====

    #[test]
    fn empty_metadata_roundtrips() {
        let meta = CharterMetadata::default();
        let json = serde_json::to_string(&meta).unwrap();
        let parsed: CharterMetadata = serde_json::from_str(&json).unwrap();
        assert!(parsed.charter.is_none());
        assert!(parsed.acts.is_empty());
        assert!(parsed.plans.is_empty());
    }

    #[test]
    fn metadata_with_act_roundtrips() {
        let mut meta = CharterMetadata::default();
        let synced = Local::now();
        meta.acts.insert(
            "019dad29-c05d-7781-a92c-40d71adfb88e".to_string(),
            ActMeta {
                created: Some(Local::now()),
                source_vevent: Some("weekly-review@clearhead.us".to_string()),
                scheduled_at_sync: Some(synced),
                due_date_sync: None,
            },
        );
        let json = serde_json::to_string_pretty(&meta).unwrap();
        let parsed: CharterMetadata = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.acts.len(), 1);
        let act = &parsed.acts["019dad29-c05d-7781-a92c-40d71adfb88e"];
        assert!(act.created.is_some());
        assert_eq!(
            act.source_vevent.as_deref(),
            Some("weekly-review@clearhead.us")
        );
        // The B column round-trips; an unset side stays None (skip_serializing_if).
        assert_eq!(act.scheduled_at_sync, Some(synced));
        assert!(act.due_date_sync.is_none());
    }

    #[test]
    fn metadata_with_charter_and_plan_roundtrips() {
        let mut meta = CharterMetadata::default();
        meta.charter = Some(CharterMeta {
            created: Some(Local::now()),
        });
        meta.plans.insert(
            "weekly-review".to_string(),
            PlanMeta {
                last_expanded: Some(Local::now()),
            },
        );
        let json = serde_json::to_string_pretty(&meta).unwrap();
        let parsed: CharterMetadata = serde_json::from_str(&json).unwrap();
        assert!(parsed.charter.is_some());
        assert_eq!(parsed.plans.len(), 1);
    }

    #[test]
    fn deserialize_ignores_unknown_fields() {
        let json = r#"{
            "acts": {
                "some-id": {
                    "created": "2026-04-20T16:11:00-05:00",
                    "custom_tool_field": "should not break"
                }
            },
            "unknown_section": { "whatever": true }
        }"#;
        let parsed: CharterMetadata = serde_json::from_str(json).unwrap();
        assert_eq!(parsed.acts.len(), 1);
    }

    #[test]
    fn empty_json_object_parses() {
        let parsed: CharterMetadata = serde_json::from_str("{}").unwrap();
        assert!(parsed.charter.is_none());
        assert!(parsed.acts.is_empty());
    }

    // ===== Hydration =====

    fn make_sourced(action: crate::domain::Action) -> crate::workspace::actions::repository::SourcedAction {
        use crate::workspace::actions::repository::{ActionSource, SourcedAction};
        use std::path::PathBuf;
        SourcedAction {
            action,
            source: ActionSource { file_path: PathBuf::new(), project: None },
            source_metadata: None,
        }
    }

    #[test]
    fn hydrate_fills_created_at_from_sidecar() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let created = Local::now();
        let mut acts = vec![make_sourced(Action { id, ..Default::default() })];
        let mut meta = CharterMetadata::default();
        meta.acts.insert(id.to_string(), ActMeta { created: Some(created), ..Default::default() });

        hydrate_acts(&mut acts, &meta);
        assert_eq!(acts[0].action.created_at, Some(created));
    }

    #[test]
    fn hydrate_does_not_overwrite_existing_created_at() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let dsl_created = Local::now();
        let sidecar_created = dsl_created - chrono::Duration::hours(1);
        let mut acts = vec![make_sourced(Action { id, created_at: Some(dsl_created), ..Default::default() })];
        let mut meta = CharterMetadata::default();
        meta.acts.insert(id.to_string(), ActMeta { created: Some(sidecar_created), ..Default::default() });

        hydrate_acts(&mut acts, &meta);
        assert_eq!(acts[0].action.created_at, Some(dsl_created));
    }

    #[test]
    fn hydrate_fills_external_schedule_id() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let mut acts = vec![make_sourced(Action { id, ..Default::default() })];
        let mut meta = CharterMetadata::default();
        meta.acts.insert(
            id.to_string(),
            ActMeta { created: None, source_vevent: Some("weekly-review@clearhead.us".to_string()), ..Default::default() },
        );

        hydrate_acts(&mut acts, &meta);
        assert_eq!(acts[0].action.external_schedule_id.as_deref(), Some("weekly-review@clearhead.us"));
    }

    #[test]
    fn hydrate_fills_merge_base_b_columns() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let synced = Local::now();
        let mut acts = vec![make_sourced(Action { id, ..Default::default() })];
        let mut meta = CharterMetadata::default();
        meta.acts.insert(
            id.to_string(),
            ActMeta { scheduled_at_sync: Some(synced), ..Default::default() },
        );

        hydrate_acts(&mut acts, &meta);
        assert_eq!(acts[0].action.scheduled_at_sync, Some(synced));
        // No sidecar value for the deadline → stays "not yet synced".
        assert!(acts[0].action.due_date_sync.is_none());
    }

    #[test]
    fn hydrate_skips_acts_not_in_sidecar() {
        use crate::domain::Action;
        use uuid::Uuid;

        let mut acts = vec![make_sourced(Action { id: Uuid::now_v7(), ..Default::default() })];
        let meta = CharterMetadata::default();

        hydrate_acts(&mut acts, &meta);
        assert!(acts[0].action.created_at.is_none());
    }

    // ===== Filesystem read/write =====

    #[test]
    fn read_sidecar_missing_file_returns_default() {
        let result = read_sidecar(Path::new("/nonexistent/.inbox.json")).unwrap();
        assert!(result.acts.is_empty());
    }

    #[test]
    fn write_and_read_sidecar_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(".test.json");

        let mut meta = CharterMetadata::default();
        meta.acts.insert(
            "test-uuid".to_string(),
            ActMeta {
                created: Some(Local::now()),
                ..Default::default()
            },
        );

        write_sidecar(&path, &meta).unwrap();
        let loaded = read_sidecar(&path).unwrap();
        assert_eq!(loaded.acts.len(), 1);
        assert!(loaded.acts.contains_key("test-uuid"));
    }
}
