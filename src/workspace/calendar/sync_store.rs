//! Per-sync-pair merge bases for calendar reconciliation.
//!
//! Merge bases describe the last agreement between one workspace and one
//! remote. They are therefore machine-local sync state, not action metadata.
//! Each pair owns one JSON file under `<data_root>/sync/`.

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use uuid::Uuid;

use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};

pub const DEFAULT_SYNC_PAIR: &str = "caldav";
pub const SCHEDULED_AT_FIELD: &str = "scheduled_at";
const STORE_VERSION: u32 = 1;

type Time = Option<DateTime<Local>>;

/// Machine-local merge bases, keyed first by action UUID and then by field.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SyncStore {
    pub version: u32,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub actions: BTreeMap<Uuid, BTreeMap<String, Value>>,
}

impl Default for SyncStore {
    fn default() -> Self {
        Self {
            version: STORE_VERSION,
            actions: BTreeMap::new(),
        }
    }
}

impl SyncStore {
    /// Return the scheduled-time merge bases used by `plan_sync`.
    pub fn scheduled_at_bases(&self) -> Result<HashMap<Uuid, Time>, WorkspaceError> {
        let mut bases = HashMap::new();
        for (id, fields) in &self.actions {
            let Some(value) = fields.get(SCHEDULED_AT_FIELD) else {
                continue;
            };
            let time = serde_json::from_value(value.clone()).map_err(|error| {
                WorkspaceError::Parse(format!(
                    "calendar sync store: invalid {SCHEDULED_AT_FIELD} for {id}: {error}"
                ))
            })?;
            bases.insert(*id, time);
        }
        Ok(bases)
    }

    /// Stamp a field's resolved value after a successful reconcile.
    pub fn stamp_scheduled_at(&mut self, action_id: Uuid, time: Time) {
        self.actions.entry(action_id).or_default().insert(
            SCHEDULED_AT_FIELD.to_string(),
            serde_json::to_value(time).expect("datetime serializes"),
        );
    }
}

/// Resolve the local state file for one sync pair.
pub fn sync_store_path(root: &Path, pair: &str) -> Result<PathBuf, WorkspaceError> {
    if pair.is_empty() || pair == "." || pair == ".." || pair.contains('/') || pair.contains('\\') {
        return Err(WorkspaceError::Parse(format!(
            "invalid sync pair name: {pair:?}"
        )));
    }
    Ok(resolve_workspace_layout(root)
        .data_root
        .join("sync")
        .join(format!("{pair}.json")))
}

/// Load a pair's store. If it does not exist, seed it from legacy sidecar
/// `scheduled_at_sync` values so existing workspaces retain their merge bases.
pub fn read_sync_store(root: &Path, pair: &str) -> Result<SyncStore, WorkspaceError> {
    let path = sync_store_path(root, pair)?;
    if path.exists() {
        let content = std::fs::read_to_string(&path)?;
        let store: SyncStore = serde_json::from_str(&content)
            .map_err(|error| WorkspaceError::Parse(format!("calendar sync store: {error}")))?;
        if store.version != STORE_VERSION {
            return Err(WorkspaceError::Parse(format!(
                "unsupported calendar sync store version {} (expected {STORE_VERSION})",
                store.version
            )));
        }
        return Ok(store);
    }

    seed_legacy_sidecars(&resolve_workspace_layout(root).charter_root)
}

pub(crate) fn serialize_sync_store(store: &SyncStore) -> Result<String, WorkspaceError> {
    serde_json::to_string_pretty(store).map_err(|error| WorkspaceError::Parse(error.to_string()))
}

#[derive(Default, Deserialize)]
struct LegacySidecar {
    #[serde(default, alias = "acts")]
    actions: BTreeMap<String, LegacyActionMeta>,
}

#[derive(Default, Deserialize)]
struct LegacyActionMeta {
    scheduled_at_sync: Option<DateTime<Local>>,
}

fn seed_legacy_sidecars(charter_root: &Path) -> Result<SyncStore, WorkspaceError> {
    let mut store = SyncStore::default();
    for path in hidden_json_files(charter_root) {
        let Ok(content) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Ok(sidecar) = serde_json::from_str::<LegacySidecar>(&content) else {
            // Workspace loading/doctor owns corrupt-sidecar diagnostics. A
            // broken unrelated sidecar must not prevent migration of the rest.
            continue;
        };
        for (raw_id, meta) in sidecar.actions {
            let (Ok(id), Some(time)) = (Uuid::parse_str(&raw_id), meta.scheduled_at_sync) else {
                continue;
            };
            store.stamp_scheduled_at(id, Some(time));
        }
    }
    Ok(store)
}

fn hidden_json_files(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let hidden = path
                .file_name()
                .map(|name| name.to_string_lossy().starts_with('.'))
                .unwrap_or(false);
            if path.is_dir() {
                if !hidden {
                    stack.push(path);
                }
            } else if hidden && path.extension().and_then(|ext| ext.to_str()) == Some("json") {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    #[test]
    fn roundtrips_nullable_field_values() {
        let id = Uuid::new_v4();
        let time = Local.with_ymd_and_hms(2026, 4, 28, 10, 0, 0).unwrap();
        let mut store = SyncStore::default();
        store.stamp_scheduled_at(id, Some(time));
        let decoded: SyncStore =
            serde_json::from_str(&serialize_sync_store(&store).unwrap()).unwrap();
        assert_eq!(
            decoded.scheduled_at_bases().unwrap().get(&id),
            Some(&Some(time))
        );

        store.stamp_scheduled_at(id, None);
        let decoded: SyncStore =
            serde_json::from_str(&serialize_sync_store(&store).unwrap()).unwrap();
        assert_eq!(decoded.scheduled_at_bases().unwrap().get(&id), Some(&None));
    }

    #[test]
    fn missing_store_seeds_legacy_sidecars_once_store_is_absent() {
        let root = tempfile::tempdir().unwrap();
        let charter_root = root.path().join("charters");
        std::fs::create_dir_all(&charter_root).unwrap();
        let id = Uuid::new_v4();
        std::fs::write(
            charter_root.join(".next.json"),
            format!(
                r#"{{"actions":{{"{id}":{{"scheduled_at_sync":"2026-04-28T10:00:00-07:00"}}}}}}"#
            ),
        )
        .unwrap();

        let store = read_sync_store(root.path(), DEFAULT_SYNC_PAIR).unwrap();
        assert!(store.scheduled_at_bases().unwrap().contains_key(&id));
    }

    #[test]
    fn pair_name_cannot_escape_sync_directory() {
        let root = tempfile::tempdir().unwrap();
        assert!(sync_store_path(root.path(), "../remote").is_err());
    }
}
