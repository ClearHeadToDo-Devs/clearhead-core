//! Per-charter `.open.ttl` / `.closed.ttl` reader/writer for PlannedAct persistence.
//!
//! Each charter's acts are stored in two Turtle files alongside the `.actions` file:
//! - `<charter>.open.ttl`   — upcoming/in-progress acts
//! - `<charter>.closed.ttl` — completed/cancelled acts
//!
//! Path derivation follows `infer_charter_name` logic but does NOT skip "inbox",
//! and always uses the charter stem (parent dir name for `next.actions`).

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use uuid::Uuid;

use crate::domain::{DomainModel, PlannedAct};
use crate::graph;
use crate::workspace::store::WorkspaceError;

// ============================================================================
// Path derivation
// ============================================================================

/// Derive the charter stem from a `.actions` file path.
///
/// - `health.actions`                   → `"health"`
/// - `inbox.actions`                    → `"inbox"` (NOT skipped — unlike infer_charter_name)
/// - `build_clearhead/next.actions`     → `"build_clearhead"` (parent dir)
/// - `build_clearhead/observ.actions`   → `"observ"` (file stem)
fn charter_stem(actions_path: &Path) -> String {
    let filename = actions_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("");

    if filename == "next.actions" {
        // Use parent directory name
        if let Some(parent) = actions_path.parent() {
            if let Some(dir_name) = parent.file_name().and_then(|n| n.to_str()) {
                return dir_name.to_string();
            }
        }
    }

    // Use file stem (strip .actions)
    actions_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string()
}

/// Return the open acts path for a `.actions` file.
///
/// `health.actions`              → `health.open.ttl` (same dir)
/// `build_clearhead/next.actions` → `build_clearhead/build_clearhead.open.ttl`
/// `build_clearhead/obs.actions`  → `build_clearhead/obs.open.ttl`
pub fn open_acts_path(actions_path: &Path) -> PathBuf {
    let stem = charter_stem(actions_path);
    let dir = actions_path.parent().unwrap_or(Path::new(""));
    dir.join(format!("{}.open.ttl", stem))
}

/// Return the closed acts path for a `.actions` file.
///
/// Same logic as `open_acts_path` but with `.closed.ttl` suffix.
pub fn closed_acts_path(actions_path: &Path) -> PathBuf {
    let stem = charter_stem(actions_path);
    let dir = actions_path.parent().unwrap_or(Path::new(""));
    dir.join(format!("{}.closed.ttl", stem))
}

// ============================================================================
// Public API
// ============================================================================

/// Read [`PlannedAct`]s from a Turtle file.
///
/// Returns `Ok(vec![])` if the file is absent — absence is normal for
/// non-recurring plans or files that have never been expanded.
pub fn read_acts(path: &Path) -> Result<Vec<PlannedAct>, WorkspaceError> {
    if !path.exists() {
        return Ok(Vec::new());
    }

    let content = std::fs::read_to_string(path)
        .map_err(|e| WorkspaceError::Io(e))?;

    let store = graph::create_store()
        .map_err(|e| WorkspaceError::Acts(e.to_string()))?;
    graph::load_turtle(&store, &content)
        .map_err(|e| WorkspaceError::Acts(e.to_string()))?;

    graph::load_planned_acts_from_store(&store)
        .map_err(|e| WorkspaceError::Acts(e.to_string()))
}

/// Write [`PlannedAct`]s to a Turtle file via oxigraph.
///
/// Creates parent directories as needed. Overwrites any existing file.
pub fn write_acts(acts: &[PlannedAct], path: &Path) -> Result<(), WorkspaceError> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent)
                .map_err(|e| WorkspaceError::Io(e))?;
        }
    }

    let store = graph::create_store()
        .map_err(|e| WorkspaceError::Acts(e.to_string()))?;
    graph::load_acts_into_store(&store, acts)
        .map_err(|e| WorkspaceError::Acts(e.to_string()))?;
    let ttl = graph::dump_store_to_turtle(&store)
        .map_err(|e| WorkspaceError::Acts(e.to_string()))?;

    std::fs::write(path, ttl).map_err(|e| WorkspaceError::Io(e))
}

/// Write acts belonging to specific plans, preserving other plans' acts already in the file.
///
/// Workflow:
/// 1. Load existing acts from `path`
/// 2. Remove acts whose `plan_id` is in `plan_ids` (they will be replaced)
/// 3. Append `updated_acts`
/// 4. Write everything back
pub fn write_acts_for_plans(
    updated: &[PlannedAct],
    plan_ids: &HashSet<Uuid>,
    path: &Path,
) -> Result<(), WorkspaceError> {
    let mut existing = read_acts(path)?;
    existing.retain(|a| !plan_ids.contains(&a.plan_id));
    existing.extend_from_slice(updated);
    write_acts(&existing, path)
}

/// Merge a list of loaded [`PlannedAct`]s into a [`DomainModel`].
///
/// For each plan in the model, replaces the plan's synthetic acts with
/// the loaded acts where `plan_id` matches. Plans with no matching acts
/// in the sidecar keep their existing (synthetic) acts as a fallback.
pub fn merge_acts_into_model(model: &mut DomainModel, acts: Vec<PlannedAct>) {
    let mut acts_by_plan: HashMap<Uuid, Vec<PlannedAct>> = HashMap::new();
    for act in acts {
        acts_by_plan.entry(act.plan_id).or_default().push(act);
    }

    fn apply_to_plan(plan: &mut crate::domain::Plan, acts: &mut HashMap<Uuid, Vec<PlannedAct>>) {
        if let Some(loaded) = acts.remove(&plan.id) {
            plan.acts = loaded;
        }
        for sub in &mut plan.sub_plans {
            apply_to_plan(sub, acts);
        }
    }

    fn apply_to_charter(charter: &mut crate::domain::Charter, acts: &mut HashMap<Uuid, Vec<PlannedAct>>) {
        for plan in &mut charter.plans {
            apply_to_plan(plan, acts);
        }
        for sub in &mut charter.sub_charters {
            apply_to_charter(sub, acts);
        }
    }

    for charter in &mut model.charters {
        apply_to_charter(charter, &mut acts_by_plan);
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{ActPhase, Charter, DomainModel, Plan, PlannedAct};
    use chrono::TimeZone;

    fn make_act(plan_id: Uuid) -> PlannedAct {
        PlannedAct {
            id: Uuid::new_v4(),
            plan_id,
            scheduled_at: Some(
                chrono::Local
                    .with_ymd_and_hms(2026, 3, 18, 9, 0, 0)
                    .unwrap(),
            ),
            created_at: Some(chrono::Local.with_ymd_and_hms(2026, 3, 1, 0, 0, 0).unwrap()),
            duration: Some(30),
            ..Default::default()
        }
    }

    // ========================================================================
    // Path derivation
    // ========================================================================

    #[test]
    fn test_open_acts_path() {
        // Root-level file
        assert_eq!(
            open_acts_path(Path::new("/data/health.actions")),
            PathBuf::from("/data/health.open.ttl")
        );
        // inbox NOT skipped
        assert_eq!(
            open_acts_path(Path::new("inbox.actions")),
            PathBuf::from("inbox.open.ttl")
        );
        // next.actions → parent dir name
        assert_eq!(
            open_acts_path(Path::new("build_clearhead/next.actions")),
            PathBuf::from("build_clearhead/build_clearhead.open.ttl")
        );
        // sub-charter: use file stem
        assert_eq!(
            open_acts_path(Path::new("build_clearhead/observability.actions")),
            PathBuf::from("build_clearhead/observability.open.ttl")
        );
    }

    #[test]
    fn test_closed_acts_path() {
        assert_eq!(
            closed_acts_path(Path::new("/data/health.actions")),
            PathBuf::from("/data/health.closed.ttl")
        );
        assert_eq!(
            closed_acts_path(Path::new("build_clearhead/next.actions")),
            PathBuf::from("build_clearhead/build_clearhead.closed.ttl")
        );
    }

    // ========================================================================
    // Round-trip I/O
    // ========================================================================

    #[test]
    fn test_missing_file_returns_empty() {
        let result = read_acts(Path::new("/nonexistent/path.open.ttl"));
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_roundtrip() {
        let plan_id = Uuid::new_v4();
        let act = make_act(plan_id);
        let acts = vec![act.clone()];

        let tmp_dir = tempfile::tempdir().expect("tempdir");
        let path = tmp_dir.path().join("inbox.open.ttl");

        write_acts(&acts, &path).expect("write");
        let loaded = read_acts(&path).expect("read");

        assert_eq!(loaded.len(), 1);
        let loaded_act = &loaded[0];
        assert_eq!(loaded_act.id, act.id);
        assert_eq!(loaded_act.plan_id, act.plan_id);
        assert_eq!(loaded_act.phase, act.phase);
        assert_eq!(loaded_act.duration, act.duration);
        assert!(loaded_act.scheduled_at.is_some());
        assert!(loaded_act.created_at.is_some());
        assert!(loaded_act.completed_at.is_none());
    }

    #[test]
    fn test_read_noncanonical_predicates_returns_empty() {
        let plan_id = Uuid::new_v4();
        let act_id = Uuid::new_v4();
        let ttl = format!(
            "@prefix actions: <https://clearhead.us/vocab/actions/v4#> .\n\
             @prefix cco: <https://www.commoncoreontologies.org/> .\n\
             @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n\
             <urn:uuid:{act_id}> a cco:{planned_act} ;\n\
                 actions:id \"{act_id}\" ;\n\
                 actions:prescribedBy <urn:uuid:{plan_id}> ;\n\
                 cco:{status_prop} actions:NotStarted ;\n\
                 actions:duration \"30\"^^xsd:integer ;\n\
                 actions:createdAt \"2026-04-09T09:00:00-07:00\"^^xsd:dateTime .\n",
            planned_act = crate::graph::CCO_PLANNED_ACT,
            status_prop = crate::graph::CCO_STATUS_PROP,
        );

        let tmp_dir = tempfile::tempdir().expect("tempdir");
        let path = tmp_dir.path().join("legacy.open.ttl");
        std::fs::write(&path, ttl).expect("write ttl");

        let loaded = read_acts(&path).expect("read acts");
        assert!(loaded.is_empty());
    }

    #[test]
    fn test_all_phases_roundtrip() {
        let plan_id = Uuid::new_v4();
        let phases = [
            ActPhase::NotStarted,
            ActPhase::InProgress,
            ActPhase::Completed,
            ActPhase::Blocked,
            ActPhase::Cancelled,
        ];

        let tmp_dir = tempfile::tempdir().expect("tempdir");

        for phase in phases {
            let act = PlannedAct {
                id: Uuid::new_v4(),
                plan_id,
                phase,
                ..Default::default()
            };

            let path = tmp_dir.path().join(format!("{:?}.open.ttl", phase));
            write_acts(&[act.clone()], &path).expect("write");
            let loaded = read_acts(&path).expect("read");
            assert_eq!(loaded[0].phase, phase);
        }
    }

    // ========================================================================
    // write_acts_for_plans
    // ========================================================================

    #[test]
    fn test_write_acts_for_plans_preserves_others() {
        let plan_a = Uuid::new_v4();
        let plan_b = Uuid::new_v4();

        let act_a = PlannedAct { id: Uuid::new_v4(), plan_id: plan_a, ..Default::default() };
        let act_b = PlannedAct { id: Uuid::new_v4(), plan_id: plan_b, ..Default::default() };

        let tmp_dir = tempfile::tempdir().expect("tempdir");
        let path = tmp_dir.path().join("health.open.ttl");

        // Write both acts initially
        write_acts(&[act_a.clone(), act_b.clone()], &path).expect("initial write");

        // Now update plan_a's act only — plan_b's act should be preserved
        let updated_a = PlannedAct {
            phase: ActPhase::InProgress,
            ..act_a.clone()
        };
        let mut plan_ids = HashSet::new();
        plan_ids.insert(plan_a);
        write_acts_for_plans(&[updated_a.clone()], &plan_ids, &path).expect("partial write");

        let loaded = read_acts(&path).expect("read");
        assert_eq!(loaded.len(), 2);

        let loaded_a = loaded.iter().find(|a| a.plan_id == plan_a).expect("act_a");
        let loaded_b = loaded.iter().find(|a| a.plan_id == plan_b).expect("act_b");
        assert_eq!(loaded_a.phase, ActPhase::InProgress);
        assert_eq!(loaded_b.id, act_b.id);
    }

    // ========================================================================
    // merge_acts_into_model
    // ========================================================================

    #[test]
    fn test_merge_acts_into_model() {
        let plan_id = Uuid::new_v4();
        let act = PlannedAct {
            id: Uuid::new_v4(),
            plan_id,
            phase: ActPhase::Completed,
            ..Default::default()
        };

        let plan = Plan {
            id: plan_id,
            name: "Test plan".to_string(),
            acts: Vec::new(),
            ..Default::default()
        };

        let charter = Charter {
            id: Uuid::new_v4(),
            title: "Test".to_string(),
            plans: vec![plan],
            ..Default::default()
        };

        let mut model = DomainModel {
            objectives: Vec::new(),
            charters: vec![charter],
        };

        merge_acts_into_model(&mut model, vec![act.clone()]);

        let loaded_acts = &model.charters[0].plans[0].acts;
        assert_eq!(loaded_acts.len(), 1);
        assert_eq!(loaded_acts[0].id, act.id);
        assert_eq!(loaded_acts[0].phase, ActPhase::Completed);
    }
}
