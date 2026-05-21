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
pub fn hydrate_acts(acts: &mut [crate::domain::Action], metadata: &CharterMetadata) {
    for act in acts.iter_mut() {
        // Sidecar keys are the action UUID from the DSL.
        // Fall back to act.id for acts constructed without a plan_id (e.g. unit tests).
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
        }
    }
}

/// Write sidecar metadata to disk.
pub fn write_sidecar(path: &Path, metadata: &CharterMetadata) -> Result<(), WorkspaceError> {
    let content =
        serde_json::to_string_pretty(metadata).map_err(|e| WorkspaceError::Parse(e.to_string()))?;
    std::fs::write(path, content)?;
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
        meta.acts.insert(
            "019dad29-c05d-7781-a92c-40d71adfb88e".to_string(),
            ActMeta {
                created: Some(Local::now()),
                source_vevent: Some("weekly-review@clearhead.us".to_string()),
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

    #[test]
    fn hydrate_fills_created_at_from_sidecar() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let created = Local::now();
        let mut acts = vec![Action {
            id,
            ..Default::default()
        }];
        let mut meta = CharterMetadata::default();
        meta.acts.insert(
            id.to_string(),
            ActMeta {
                created: Some(created),
                source_vevent: None,
            },
        );

        hydrate_acts(&mut acts, &meta);
        assert_eq!(acts[0].created_at, Some(created));
    }

    #[test]
    fn hydrate_does_not_overwrite_existing_created_at() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let dsl_created = Local::now();
        let sidecar_created = dsl_created - chrono::Duration::hours(1);
        let mut acts = vec![Action {
            id,
            created_at: Some(dsl_created),
            ..Default::default()
        }];
        let mut meta = CharterMetadata::default();
        meta.acts.insert(
            id.to_string(),
            ActMeta {
                created: Some(sidecar_created),
                source_vevent: None,
            },
        );

        hydrate_acts(&mut acts, &meta);
        assert_eq!(acts[0].created_at, Some(dsl_created));
    }

    #[test]
    fn hydrate_fills_external_schedule_id() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let mut acts = vec![Action {
            id,
            ..Default::default()
        }];
        let mut meta = CharterMetadata::default();
        meta.acts.insert(
            id.to_string(),
            ActMeta {
                created: None,
                source_vevent: Some("weekly-review@clearhead.us".to_string()),
            },
        );

        hydrate_acts(&mut acts, &meta);
        assert_eq!(
            acts[0].external_schedule_id.as_deref(),
            Some("weekly-review@clearhead.us")
        );
    }

    #[test]
    fn hydrate_skips_acts_not_in_sidecar() {
        use crate::domain::Action;
        use uuid::Uuid;

        let mut acts = vec![Action {
            id: Uuid::now_v7(),
            ..Default::default()
        }];
        let meta = CharterMetadata::default();

        hydrate_acts(&mut acts, &meta);
        assert!(acts[0].created_at.is_none());
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
                source_vevent: None,
            },
        );

        write_sidecar(&path, &meta).unwrap();
        let loaded = read_sidecar(&path).unwrap();
        assert_eq!(loaded.acts.len(), 1);
        assert!(loaded.acts.contains_key("test-uuid"));
    }
}
