//! Local merge bases for the plans vdir projection.
//!
//! ClearHead synchronizes actions with the workspace's vdir of iCalendar files.
//! This store records the last agreement between those two projections. It is
//! machine-local and reconstructible: durable Plan links, including a recurring
//! occurrence's slot, live in the Action sidecar, never here.

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use uuid::Uuid;

use crate::workspace::store::WorkspaceError;

pub const SCHEDULED_AT_FIELD: &str = "scheduled_at";
pub const DUE_DATE_FIELD: &str = "due_date";
pub const STATE_FIELD: &str = "state";
pub const TITLE_FIELD: &str = "title";
pub const DESCRIPTION_FIELD: &str = "description";
pub const PRIORITY_FIELD: &str = "priority";
pub const CONTEXTS_FIELD: &str = "contexts";
pub const UID_FIELD: &str = "uid";
/// A recurring master's canonical-origin `DTSTART`, keyed by the plan's id.
/// Holds the anchor fixed across syncs so a foreign roll-forward (an advanced
/// `DTSTART`) can be detected against it.
pub const MASTER_DTSTART_FIELD: &str = "master_dtstart";
const STORE_VERSION: u32 = 1;

type Time = Option<DateTime<Local>>;

/// Machine-local merge bases, keyed first by action UUID and then by field.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PlansSyncStore {
    pub version: u32,
    /// The plans vdir this projection state belongs to. A different path (the
    /// workspace moved) starts with an empty store rather than reusing
    /// unrelated merge bases.
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

    /// Decode one independently reconciled field's merge bases.
    pub fn field_bases<T: DeserializeOwned>(
        &self,
        field: &str,
    ) -> Result<HashMap<Uuid, T>, WorkspaceError> {
        let mut bases = HashMap::new();
        for (id, fields) in &self.actions {
            let Some(value) = fields.get(field) else {
                continue;
            };
            let value = serde_json::from_value(value.clone()).map_err(|error| {
                WorkspaceError::Parse(format!(
                    "plans sync store: invalid {field} for {id}: {error}"
                ))
            })?;
            bases.insert(*id, value);
        }
        Ok(bases)
    }

    pub fn scheduled_at_bases(&self) -> Result<HashMap<Uuid, Time>, WorkspaceError> {
        self.field_bases(SCHEDULED_AT_FIELD)
    }

    /// Stamp any field's resolved value after a successful reconcile.
    pub fn stamp<T: Serialize>(
        &mut self,
        action_id: Uuid,
        field: &str,
        value: &T,
    ) -> Result<(), WorkspaceError> {
        let value = serde_json::to_value(value)
            .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
        self.actions
            .entry(action_id)
            .or_default()
            .insert(field.to_string(), value);
        Ok(())
    }

    pub fn stamp_scheduled_at(&mut self, action_id: Uuid, time: Time) {
        self.stamp(action_id, SCHEDULED_AT_FIELD, &time)
            .expect("datetime serializes");
    }

    /// Remove all projection-owned merge bases for one Action.
    pub fn clear_action_bases(&mut self, action_id: Uuid) {
        self.actions.remove(&action_id);
    }
}

/// Decode host-supplied merge-base bytes for one plans projection.
pub fn decode_plans_sync_store(
    content: Option<&str>,
    plans_root: &Path,
) -> Result<PlansSyncStore, WorkspaceError> {
    let Some(content) = content else {
        return Ok(PlansSyncStore::new(plans_root));
    };
    let store: PlansSyncStore = serde_json::from_str(content)
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

pub fn encode_plans_sync_store(store: &PlansSyncStore) -> Result<String, WorkspaceError> {
    serde_json::to_string_pretty(store).map_err(|error| WorkspaceError::Parse(error.to_string()))
}

pub(crate) fn serialize_plans_sync_store(store: &PlansSyncStore) -> Result<String, WorkspaceError> {
    encode_plans_sync_store(store)
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
    fn missing_or_different_projection_starts_empty() {
        let missing = decode_plans_sync_store(None, Path::new("/tmp/plans")).unwrap();
        assert!(missing.actions.is_empty());

        let id = Uuid::new_v4();
        let mut old = PlansSyncStore::new(Path::new("/tmp/old-plans"));
        old.stamp_scheduled_at(id, None);
        let content = serialize_plans_sync_store(&old).unwrap();
        let current = decode_plans_sync_store(Some(&content), Path::new("/tmp/new-plans")).unwrap();
        assert!(current.actions.is_empty());
        assert_eq!(current.plans_root, Path::new("/tmp/new-plans"));
    }
}
