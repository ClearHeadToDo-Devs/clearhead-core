//! Per-charter `.<charter>.json` sidecar for machine-oriented metadata.
//!
//! Hidden JSON files that live alongside `.actions` files, holding data
//! that tooling needs but humans don't want cluttering the DSL:
//! created timestamps, recurring Plan linkage, etc.

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use super::store::WorkspaceError;

/// The published contract for this file's shape. Stamped into every sidecar
/// on save (see [`write_sidecar`]) so the file is self-describing and editors
/// validate on write — the same declarative-filesystem theme as recording
/// `charter.id`. Points at `master`; retargeting to a tagged release is
/// tracked separately (see the schema-source-of-truth decision).
pub const CHARTER_METADATA_SCHEMA_URL: &str = "https://raw.githubusercontent.com/ClearHeadToDo-Devs/specifications/master/schemas/charter_metadata.schema.json";

/// Root of the per-charter sidecar JSON (`.<charter>.json`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CharterMetadata {
    /// Schema contract pointer. Always overwritten with [`CHARTER_METADATA_SCHEMA_URL`]
    /// by `write_sidecar` regardless of what a file previously carried, the same
    /// self-healing treatment the `acts` → `actions` key rename got.
    #[serde(rename = "$schema", default, skip_serializing_if = "Option::is_none")]
    pub schema: Option<String>,
    /// Charter-level metadata (creation timestamp).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub charter: Option<CharterMeta>,
    /// Per-action metadata keyed by UUID string.
    ///
    /// A `BTreeMap` so the committed JSON serializes in a stable key order —
    /// a `HashMap` reshuffles on every save and turns each write into diff
    /// noise, which defeats the sidecar's job as a plaintext audit surface.
    ///
    /// `alias = "acts"` reads pre-rename sidecars written under the old key;
    /// every write always emits `actions`, so files migrate to the new key
    /// the next time anything touches them.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty", alias = "acts")]
    pub actions: BTreeMap<String, ActionMeta>,
    /// Per-plan metadata keyed by UUID string. `BTreeMap` for the same
    /// stable-ordering reason as `actions`.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub plans: BTreeMap<String, PlanMeta>,
}

/// Charter-level sidecar metadata.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CharterMeta {
    /// The charter's identity, recorded so the sidecar can re-join its charter
    /// by id rather than by file path — self-identifying, and move-safe even for
    /// a charter with no actions to match on. A *reference*: the charter's own
    /// declaration (frontmatter `id`, or the file itself for an action-only
    /// charter) stays authoritative and doctor verifies agreement.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<uuid::Uuid>,
    /// When this charter was first created by tooling.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub created: Option<DateTime<Local>>,
}

/// Per-action sidecar metadata.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ActionMeta {
    /// When this action was first created by tooling.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub created: Option<DateTime<Local>>,
    /// Recurring Plan UID this action was generated from.
    #[serde(
        default,
        alias = "source_vevent",
        skip_serializing_if = "Option::is_none"
    )]
    pub external_schedule_id: Option<String>,
}

/// Per-plan sidecar metadata.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PlanMeta {
    /// When `expand actions` last ran for this plan.
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

/// Union every sidecar under `charter_root` into one `uuid -> ActionMeta` map.
///
/// Sidecars are re-joined to actions by UUID, not by file path, so a sidecar
/// left behind when its `.actions` file moved or was renamed still hydrates its
/// actions (see [`hydrate_actions_map`]). Unreadable or corrupt sidecars are
/// skipped here — the loader reports those against their own path.
pub fn collect_sidecar_actions(charter_root: &Path) -> BTreeMap<String, ActionMeta> {
    let mut union: BTreeMap<String, ActionMeta> = BTreeMap::new();
    for path in sidecar_files(charter_root) {
        let Ok(meta) = read_sidecar(&path) else { continue };
        for (key, action) in meta.actions {
            merge_action(union.entry(key).or_default(), action);
        }
    }
    union
}

/// Fold `from` into `to`, keeping the first present value per field. Callers
/// walk sidecars in sorted path order for a deterministic result on the rare
/// duplicate.
fn merge_action(to: &mut ActionMeta, from: ActionMeta) {
    to.created = to.created.or(from.created);
    to.external_schedule_id = to.external_schedule_id.take().or(from.external_schedule_id);
}

/// Every hidden `.json` file under `dir`, recursively, sorted for determinism.
/// The pattern matches sidecars (`.<stem>.json`); a stray non-sidecar `.json`
/// simply deserializes to an empty [`CharterMetadata`] and contributes nothing.
fn sidecar_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(current) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&current) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let hidden_name = path
                .file_name()
                .map(|n| n.to_string_lossy().starts_with('.'))
                .unwrap_or(false);
            if path.is_dir() {
                if !hidden_name {
                    stack.push(path);
                }
            } else if hidden_name && path.extension().and_then(|e| e.to_str()) == Some("json") {
                out.push(path);
            }
        }
    }
    out.sort();
    out
}

/// Hydrate actions with metadata from the sidecar.
///
/// For each action, if the sidecar has a matching entry (by UUID string key),
/// fills in `created_at` and `external_schedule_id` where the action doesn't
/// already have them (DSL values are authoritative).
pub fn hydrate_actions(actions: &mut [crate::workspace::actions::repository::SourcedAction], metadata: &CharterMetadata) {
    hydrate_actions_map(actions, &metadata.actions);
}

/// Hydrate actions from a bare `uuid -> ActionMeta` map.
///
/// The loader passes a *union* of every sidecar in the workspace here, not just
/// the charter's own file. Metadata is keyed by action UUID, so an entry reaches
/// its action wherever the line now lives — even if the sidecar was orphaned by
/// a moved or renamed `.actions` file. Location is storage, not identity.
pub fn hydrate_actions_map(
    actions: &mut [crate::workspace::actions::repository::SourcedAction],
    actions_meta: &BTreeMap<String, ActionMeta>,
) {
    for sa in actions.iter_mut() {
        let action = &mut sa.action;
        let key = action
            .plan_id
            .map(|id| id.to_string())
            .unwrap_or_else(|| action.id.to_string());
        if let Some(meta) = actions_meta.get(&key) {
            if action.created_at.is_none() {
                action.created_at = meta.created;
            }
            if action.external_schedule_id.is_none() {
                action.external_schedule_id = meta.external_schedule_id.clone();
            }
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
        meta.actions.entry(key).or_insert_with(|| ActionMeta {
            created: Some(created_from_uuid(action.id).unwrap_or_else(Local::now)),
            ..Default::default()
        });
    }
    write_sidecar(&sc_path, &meta)
}

/// Record the charter's identity in its sidecar (`charter.id`).
///
/// This makes the sidecar self-identifying, so the loader resolves a charter's
/// identity from the data rather than re-deriving it from the filename. Idempotent
/// and freeze-preserving: it sets the id only when none is recorded, never
/// overwriting a value already frozen by an earlier stamp.
pub fn stamp_charter_id(actions_path: &Path, charter_id: uuid::Uuid) -> Result<(), WorkspaceError> {
    let sc_path = sidecar_path(actions_path);
    let mut meta = read_sidecar(&sc_path)?;
    let charter = meta.charter.get_or_insert_with(CharterMeta::default);
    if charter.id.is_none() {
        charter.id = Some(charter_id);
        write_sidecar(&sc_path, &meta)?;
    }
    Ok(())
}

/// Extract the creation timestamp embedded in a UUIDv7.
///
/// Only v7 carries a timestamp in its high bits. For any other version — a
/// hand- or agent-authored v4 `#id`, say — those bits are random and would
/// decode to a nonsense far-future date, so we return `None` and let the
/// caller fall back to the wall clock ("the date we first saw it").
fn created_from_uuid(id: uuid::Uuid) -> Option<DateTime<Local>> {
    if id.get_version_num() != 7 {
        return None;
    }
    let timestamp_ms = (id.as_u128() >> 80) as i64;
    DateTime::from_timestamp(timestamp_ms / 1000, ((timestamp_ms % 1000) * 1_000_000) as u32)
        .map(|dt| dt.into())
}

/// Write sidecar metadata to disk, atomically.
///
/// Always stamps `$schema` to [`CHARTER_METADATA_SCHEMA_URL`] before writing,
/// overwriting whatever value (or absence) the in-memory metadata carried in.
pub fn write_sidecar(path: &Path, metadata: &CharterMetadata) -> Result<(), WorkspaceError> {
    let mut metadata = metadata.clone();
    metadata.schema = Some(CHARTER_METADATA_SCHEMA_URL.to_string());
    let content =
        serde_json::to_string_pretty(&metadata).map_err(|e| WorkspaceError::Parse(e.to_string()))?;
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
        assert!(parsed.actions.is_empty());
        assert!(parsed.plans.is_empty());
    }

    #[test]
    fn metadata_with_action_roundtrips() {
        let mut meta = CharterMetadata::default();
        meta.actions.insert(
            "019dad29-c05d-7781-a92c-40d71adfb88e".to_string(),
            ActionMeta {
                created: Some(Local::now()),
                external_schedule_id: Some("weekly-review@clearhead.us".to_string()),
            },
        );
        let json = serde_json::to_string_pretty(&meta).unwrap();
        let parsed: CharterMetadata = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.actions.len(), 1);
        let action = &parsed.actions["019dad29-c05d-7781-a92c-40d71adfb88e"];
        assert!(action.created.is_some());
        assert_eq!(
            action.external_schedule_id.as_deref(),
            Some("weekly-review@clearhead.us")
        );
    }

    #[test]
    fn metadata_with_charter_and_plan_roundtrips() {
        let charter_id = uuid::Uuid::new_v4();
        let mut meta = CharterMetadata::default();
        meta.charter = Some(CharterMeta {
            id: Some(charter_id),
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
        assert_eq!(parsed.charter.and_then(|c| c.id), Some(charter_id));
        assert_eq!(parsed.plans.len(), 1);
    }

    #[test]
    fn deserialize_ignores_unknown_fields() {
        let json = r#"{
            "actions": {
                "some-id": {
                    "created": "2026-04-20T16:11:00-05:00",
                    "custom_tool_field": "should not break"
                }
            },
            "unknown_section": { "whatever": true }
        }"#;
        let parsed: CharterMetadata = serde_json::from_str(json).unwrap();
        assert_eq!(parsed.actions.len(), 1);
    }

    #[test]
    fn empty_json_object_parses() {
        let parsed: CharterMetadata = serde_json::from_str("{}").unwrap();
        assert!(parsed.charter.is_none());
        assert!(parsed.actions.is_empty());
    }

    // ===== Backward compatibility: pre-rename "acts" key =====

    #[test]
    fn deserialize_accepts_legacy_acts_key() {
        // Sidecars written before the acts -> actions rename still deserialize —
        // #[serde(alias = "acts")] on `CharterMetadata::actions` reads the old key.
        let json = r#"{"acts": {"legacy-id": {"created": "2026-04-20T16:11:00-05:00"}}}"#;
        let parsed: CharterMetadata = serde_json::from_str(json).unwrap();
        assert_eq!(parsed.actions.len(), 1);
        assert!(parsed.actions.contains_key("legacy-id"));
    }

    #[test]
    fn legacy_acts_key_is_rewritten_as_actions_on_save() {
        // Reading an old-format sidecar and writing it back migrates the key —
        // this is the self-healing half of the migration (no explicit tool needed).
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(".legacy.json");
        std::fs::write(&path, r#"{"acts": {"legacy-id": {"created": "2026-04-20T16:11:00-05:00"}}}"#).unwrap();

        let meta = read_sidecar(&path).unwrap();
        write_sidecar(&path, &meta).unwrap();

        let rewritten = std::fs::read_to_string(&path).unwrap();
        assert!(rewritten.contains("\"actions\""));
        assert!(!rewritten.contains("\"acts\""));
    }

    // ===== Hydration =====

    fn make_sourced(action: crate::domain::Action) -> crate::workspace::actions::repository::SourcedAction {
        use crate::workspace::actions::repository::SourcedAction;
        SourcedAction {
            action,
            source_metadata: None,
        }
    }

    #[test]
    fn hydrate_fills_created_at_from_sidecar() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let created = Local::now();
        let mut actions = vec![make_sourced(Action { id, ..Default::default() })];
        let mut meta = CharterMetadata::default();
        meta.actions.insert(id.to_string(), ActionMeta { created: Some(created), ..Default::default() });

        hydrate_actions(&mut actions, &meta);
        assert_eq!(actions[0].action.created_at, Some(created));
    }

    #[test]
    fn hydrate_does_not_overwrite_existing_created_at() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let dsl_created = Local::now();
        let sidecar_created = dsl_created - chrono::Duration::hours(1);
        let mut actions = vec![make_sourced(Action { id, created_at: Some(dsl_created), ..Default::default() })];
        let mut meta = CharterMetadata::default();
        meta.actions.insert(id.to_string(), ActionMeta { created: Some(sidecar_created), ..Default::default() });

        hydrate_actions(&mut actions, &meta);
        assert_eq!(actions[0].action.created_at, Some(dsl_created));
    }

    #[test]
    fn hydrate_fills_external_schedule_id() {
        use crate::domain::Action;
        use uuid::Uuid;

        let id = Uuid::now_v7();
        let mut actions = vec![make_sourced(Action { id, ..Default::default() })];
        let mut meta = CharterMetadata::default();
        meta.actions.insert(
            id.to_string(),
            ActionMeta { created: None, external_schedule_id: Some("weekly-review@clearhead.us".to_string()), ..Default::default() },
        );

        hydrate_actions(&mut actions, &meta);
        assert_eq!(actions[0].action.external_schedule_id.as_deref(), Some("weekly-review@clearhead.us"));
    }

    #[test]
    fn hydrate_skips_actions_not_in_sidecar() {
        use crate::domain::Action;
        use uuid::Uuid;

        let mut actions = vec![make_sourced(Action { id: Uuid::now_v7(), ..Default::default() })];
        let meta = CharterMetadata::default();

        hydrate_actions(&mut actions, &meta);
        assert!(actions[0].action.created_at.is_none());
    }

    // ===== created_from_uuid version guard =====

    #[test]
    fn created_from_uuid_reads_v7_timestamp() {
        use uuid::Uuid;
        // A v7 id minted now decodes to ~now (within a second).
        let id = Uuid::now_v7();
        let created = created_from_uuid(id).expect("v7 carries a timestamp");
        assert!((Local::now() - created).num_seconds().abs() < 5);
    }

    #[test]
    fn created_from_uuid_rejects_v4() {
        use uuid::Uuid;
        // v4 high bits are random — decoding them would manufacture a nonsense
        // (often far-future) date, so the guard must return None instead.
        for _ in 0..1000 {
            assert!(created_from_uuid(Uuid::new_v4()).is_none());
        }
    }

    #[test]
    fn stamp_uses_now_for_non_v7_ids() {
        use crate::domain::Action;
        use uuid::Uuid;

        let dir = tempfile::tempdir().unwrap();
        let actions_path = dir.path().join("next.actions");
        let v4 = Uuid::new_v4();
        stamp_sidecar_entries(&actions_path, &[Action { id: v4, ..Default::default() }]).unwrap();

        let meta = read_sidecar(&sidecar_path(&actions_path)).unwrap();
        let created = meta.actions[&v4.to_string()].created.expect("stamped");
        // "The date we saw it", not a decoded far-future date.
        assert!((Local::now() - created).num_seconds().abs() < 5);
    }

    // ===== Filesystem read/write =====

    #[test]
    fn read_sidecar_missing_file_returns_default() {
        let result = read_sidecar(Path::new("/nonexistent/.inbox.json")).unwrap();
        assert!(result.actions.is_empty());
    }

    #[test]
    fn write_sidecar_stamps_schema_pointer() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(".test.json");

        write_sidecar(&path, &CharterMetadata::default()).unwrap();

        let raw = std::fs::read_to_string(&path).unwrap();
        assert!(raw.contains(&format!("\"$schema\": \"{}\"", CHARTER_METADATA_SCHEMA_URL)));

        let loaded = read_sidecar(&path).unwrap();
        assert_eq!(loaded.schema.as_deref(), Some(CHARTER_METADATA_SCHEMA_URL));
    }

    #[test]
    fn write_sidecar_overwrites_a_stale_schema_pointer() {
        // A sidecar carrying an old/foreign $schema value gets corrected on save,
        // the same self-healing treatment the acts -> actions rename got.
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(".test.json");
        std::fs::write(&path, r#"{"$schema": "https://example.com/stale.json"}"#).unwrap();

        let meta = read_sidecar(&path).unwrap();
        write_sidecar(&path, &meta).unwrap();

        let reloaded = read_sidecar(&path).unwrap();
        assert_eq!(reloaded.schema.as_deref(), Some(CHARTER_METADATA_SCHEMA_URL));
    }

    #[test]
    fn write_and_read_sidecar_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(".test.json");

        let mut meta = CharterMetadata::default();
        meta.actions.insert(
            "test-uuid".to_string(),
            ActionMeta {
                created: Some(Local::now()),
                ..Default::default()
            },
        );

        write_sidecar(&path, &meta).unwrap();
        let loaded = read_sidecar(&path).unwrap();
        assert_eq!(loaded.actions.len(), 1);
        assert!(loaded.actions.contains_key("test-uuid"));
    }

    // ===== Union across sidecars (location-independent hydration) =====

    #[test]
    fn collect_sidecar_actions_unions_across_files_and_dirs() {
        let dir = tempfile::tempdir().unwrap();
        let sub = dir.path().join("feature");
        std::fs::create_dir_all(&sub).unwrap();
        std::fs::write(
            dir.path().join(".root.json"),
            r#"{"actions": {"aaa": {"created": "2024-01-01T00:00:00+00:00"}}}"#,
        )
        .unwrap();
        std::fs::write(
            sub.join(".nested.json"),
            r#"{"actions": {"bbb": {"created": "2024-02-02T00:00:00+00:00"}}}"#,
        )
        .unwrap();
        // A non-sidecar json and a non-json hidden file contribute nothing.
        std::fs::write(dir.path().join(".config.json"), r#"{"unrelated": true}"#).unwrap();
        std::fs::write(dir.path().join(".keep"), "ignore me").unwrap();

        let union = collect_sidecar_actions(dir.path());
        assert!(union.contains_key("aaa"));
        assert!(union.contains_key("bbb"));
        assert_eq!(union.len(), 2);
    }

    #[test]
    fn merge_action_never_clobbers_source_linkage() {
        // Same uuid in two sidecars: one carries source linkage that cannot be
        // recomputed, the other only a created stamp. The union must keep the
        // linkage regardless of which file is seen first.
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join(".aaa.json"),
            r#"{"actions": {"dup": {"created": "2024-01-01T00:00:00+00:00", "source_vevent": "vevent-7"}}}"#,
        )
        .unwrap();
        std::fs::write(
            dir.path().join(".zzz.json"),
            r#"{"actions": {"dup": {"created": "2025-05-05T00:00:00+00:00"}}}"#,
        )
        .unwrap();

        let union = collect_sidecar_actions(dir.path());
        let entry = &union["dup"];
        assert_eq!(
            entry.external_schedule_id.as_deref(),
            Some("vevent-7"),
            "the external_schedule_id must survive an empty re-stamp under the same uuid",
        );
        assert!(entry.created.is_some());
    }
}
