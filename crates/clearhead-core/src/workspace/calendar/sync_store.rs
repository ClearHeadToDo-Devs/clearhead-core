//! Local merge bases for the configured plans vdir projection.
//!
//! ClearHead synchronizes actions with one configured directory of vdir-compatible
//! iCalendar files. This store records the last agreement between those two
//! projections. It is machine-local bookkeeping, not action or sidecar metadata.

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
/// A materialized occurrence's link back to its recurring master, keyed by the
/// occurrence's action id: which plan (`OCCURRENCE_PLAN_FIELD`) and which slot
/// (`OCCURRENCE_SLOT_FIELD`, a `canonical_occurrence_key`). Neither the `.actions`
/// DSL nor the sidecar persists an action's `plan_id`/`external_occurrence_key`,
/// so the stamper records the link here; the completion hook reads it to target
/// the master deviation, then clears it.
pub const OCCURRENCE_PLAN_FIELD: &str = "occurrence_plan";
pub const OCCURRENCE_SLOT_FIELD: &str = "occurrence_slot";
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

    /// Record a materialized occurrence's link to its master `(plan_id, slot)`.
    pub fn stamp_occurrence_link(
        &mut self,
        occurrence_id: Uuid,
        plan_id: Uuid,
        slot_key: &str,
    ) -> Result<(), WorkspaceError> {
        self.stamp(occurrence_id, OCCURRENCE_PLAN_FIELD, &plan_id)?;
        self.stamp(occurrence_id, OCCURRENCE_SLOT_FIELD, &slot_key)?;
        Ok(())
    }

    /// The `(plan_id, slot key)` a materialized occurrence links to, if recorded.
    pub fn occurrence_link(&self, occurrence_id: Uuid) -> Option<(Uuid, String)> {
        let fields = self.actions.get(&occurrence_id)?;
        let plan = serde_json::from_value(fields.get(OCCURRENCE_PLAN_FIELD)?.clone()).ok()?;
        let slot = serde_json::from_value(fields.get(OCCURRENCE_SLOT_FIELD)?.clone()).ok()?;
        Some((plan, slot))
    }

    /// All recorded occurrence links, as `occurrence_id -> (plan_id, slot)`.
    /// The stamper uses this to tell whether a plan already has a live token.
    pub fn occurrence_links(&self) -> HashMap<Uuid, (Uuid, String)> {
        self.actions
            .keys()
            .filter_map(|id| self.occurrence_link(*id).map(|link| (*id, link)))
            .collect()
    }

    /// Drop an occurrence's linkage once its deviation has landed. Removes the
    /// whole entry if no other merge bases remain under that id.
    pub fn clear_occurrence_link(&mut self, occurrence_id: Uuid) {
        if let Some(fields) = self.actions.get_mut(&occurrence_id) {
            fields.remove(OCCURRENCE_PLAN_FIELD);
            fields.remove(OCCURRENCE_SLOT_FIELD);
            if fields.is_empty() {
                self.actions.remove(&occurrence_id);
            }
        }
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
    fn occurrence_link_roundtrips_and_clears() {
        let plans_root = Path::new("/tmp/plans");
        let occ = Uuid::new_v4();
        let plan = Uuid::new_v4();
        let mut store = PlansSyncStore::new(plans_root);

        store
            .stamp_occurrence_link(occ, plan, "20260503T090000Z")
            .unwrap();
        let decoded: PlansSyncStore =
            serde_json::from_str(&serialize_plans_sync_store(&store).unwrap()).unwrap();
        assert_eq!(
            decoded.occurrence_link(occ),
            Some((plan, "20260503T090000Z".to_string()))
        );
        assert_eq!(
            decoded.occurrence_links().get(&occ),
            Some(&(plan, "20260503T090000Z".to_string()))
        );

        // Clearing after the deviation lands drops the (now-empty) entry entirely.
        store.clear_occurrence_link(occ);
        assert!(store.occurrence_link(occ).is_none());
        assert!(
            store.actions.is_empty(),
            "empty entry is removed, not left dangling"
        );
    }

    #[test]
    fn clearing_occurrence_link_preserves_other_bases() {
        // An id carrying an unrelated merge base keeps that base when its
        // occurrence link is cleared.
        let occ = Uuid::new_v4();
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        store
            .stamp_occurrence_link(occ, Uuid::new_v4(), "slot")
            .unwrap();
        store.stamp(occ, UID_FIELD, &"keep-me").unwrap();
        store.clear_occurrence_link(occ);
        assert!(store.occurrence_link(occ).is_none());
        assert!(
            store.actions.contains_key(&occ),
            "the surviving base keeps the entry"
        );
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
