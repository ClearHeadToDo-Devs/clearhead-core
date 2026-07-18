//! Local merge bases for the configured plans vdir projection.
//!
//! ClearHead synchronizes actions with one configured directory of vdir-compatible
//! iCalendar files. This store records the last agreement between those two
//! projections. It is machine-local bookkeeping, not action or sidecar metadata.

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use uuid::Uuid;

use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};

pub const SCHEDULED_AT_FIELD: &str = "scheduled_at";
const STORE_VERSION: u32 = 1;

type Time = Option<DateTime<Local>>;

/// Machine-local merge bases, keyed first by action UUID and then by field.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PlansSyncStore {
    pub version: u32,
    /// The configured vdir this projection state belongs to. A different path
    /// starts with an empty store rather than reusing unrelated merge bases.
    pub plans_root: PathBuf,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub actions: BTreeMap<Uuid, BTreeMap<String, Value>>,
}

impl PlansSyncStore {
    pub fn new(plans_root: &Path) -> Self {
        Self {
            version: STORE_VERSION,
            plans_root: plans_root.to_path_buf(),
            actions: BTreeMap::new(),
        }
    }

    /// Return the scheduled-time merge bases used by `plan_sync`.
    pub fn scheduled_at_bases(&self) -> Result<HashMap<Uuid, Time>, WorkspaceError> {
        let mut bases = HashMap::new();
        for (id, fields) in &self.actions {
            let Some(value) = fields.get(SCHEDULED_AT_FIELD) else {
                continue;
            };
            let time = serde_json::from_value(value.clone()).map_err(|error| {
                WorkspaceError::Parse(format!(
                    "plans sync store: invalid {SCHEDULED_AT_FIELD} for {id}: {error}"
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

/// Resolve the one local state file for the configured plans projection.
pub fn plans_sync_store_path(root: &Path) -> PathBuf {
    resolve_workspace_layout(root)
        .data_root
        .join("sync")
        .join("plans.json")
}

/// Load merge bases for `plans_root`. A missing store or a store belonging to
/// another configured path has clean first-sync semantics.
pub fn read_plans_sync_store(
    root: &Path,
    plans_root: &Path,
) -> Result<PlansSyncStore, WorkspaceError> {
    let path = plans_sync_store_path(root);
    if !path.exists() {
        return Ok(PlansSyncStore::new(plans_root));
    }

    let content = std::fs::read_to_string(&path)?;
    let store: PlansSyncStore = serde_json::from_str(&content)
        .map_err(|error| WorkspaceError::Parse(format!("plans sync store: {error}")))?;
    if store.version != STORE_VERSION {
        return Err(WorkspaceError::Parse(format!(
            "unsupported plans sync store version {} (expected {STORE_VERSION})",
            store.version
        )));
    }
    if store.plans_root != plans_root {
        return Ok(PlansSyncStore::new(plans_root));
    }
    Ok(store)
}

pub(crate) fn serialize_plans_sync_store(
    store: &PlansSyncStore,
) -> Result<String, WorkspaceError> {
    serde_json::to_string_pretty(store).map_err(|error| WorkspaceError::Parse(error.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    #[test]
    fn roundtrips_nullable_field_values() {
        let plans_root = Path::new("/tmp/plans");
        let id = Uuid::new_v4();
        let time = Local.with_ymd_and_hms(2026, 4, 28, 10, 0, 0).unwrap();
        let mut store = PlansSyncStore::new(plans_root);
        store.stamp_scheduled_at(id, Some(time));
        let decoded: PlansSyncStore =
            serde_json::from_str(&serialize_plans_sync_store(&store).unwrap()).unwrap();
        assert_eq!(
            decoded.scheduled_at_bases().unwrap().get(&id),
            Some(&Some(time))
        );

        store.stamp_scheduled_at(id, None);
        let decoded: PlansSyncStore =
            serde_json::from_str(&serialize_plans_sync_store(&store).unwrap()).unwrap();
        assert_eq!(decoded.scheduled_at_bases().unwrap().get(&id), Some(&None));
    }

    #[test]
    fn missing_store_starts_empty() {
        let root = tempfile::tempdir().unwrap();
        let store = read_plans_sync_store(root.path(), Path::new("/tmp/plans")).unwrap();
        assert!(store.actions.is_empty());
    }

    #[test]
    fn changing_the_configured_vdir_starts_empty() {
        let root = tempfile::tempdir().unwrap();
        let path = plans_sync_store_path(root.path());
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();

        let id = Uuid::new_v4();
        let mut old = PlansSyncStore::new(Path::new("/tmp/old-plans"));
        old.stamp_scheduled_at(id, None);
        std::fs::write(&path, serialize_plans_sync_store(&old).unwrap()).unwrap();

        let current = read_plans_sync_store(root.path(), Path::new("/tmp/new-plans")).unwrap();
        assert!(current.actions.is_empty());
        assert_eq!(current.plans_root, Path::new("/tmp/new-plans"));
    }

    #[test]
    fn project_workspace_store_lives_under_clearhead() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir(root.path().join(".clearhead")).unwrap();
        assert_eq!(
            plans_sync_store_path(root.path()),
            root.path().join(".clearhead/sync/plans.json")
        );
    }
}
