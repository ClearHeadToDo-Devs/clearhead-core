//! Native plans-vdir discovery, immutable reads, and sync-store persistence.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use uuid::Uuid;

use clearhead_core::Plan;
use clearhead_core::workspace::OccurrenceOp;
use clearhead_core::workspace::calendar::ics::{
    ICSPlan, VTodoAction, parse_ics, parse_vtodo_actions_content, render_occurrence_deviation,
    render_plan_resource,
};
use clearhead_core::workspace::calendar::plans::{
    infer_plan_charter_name_for_workspace, infer_plan_parent_for_workspace,
};
use clearhead_core::workspace::calendar::reconcile::{
    PlanResourceState, prepare_master_rollforwards,
};
use clearhead_core::workspace::calendar::sync_store::{PlansSyncStore, decode_plans_sync_store};
use clearhead_core::workspace::durability::{WorkspaceLock, recover_pending};
use clearhead_core::workspace::resource::{
    Effect, EffectBatch, ExpectedResource, MountId, MountInventory, ReadPlan, ResourceLocation,
    ResourcePrecondition, ResourceRevision, WorkspaceMounts, WorkspacePath,
};
use clearhead_core::workspace::{VTodoResource, WorkspaceError};

use crate::mounts::NativeWorkspaceMounts;

/// One immutable native `.ics` resource in the effective plans mount.
#[derive(Clone, Debug)]
pub struct CalendarResource {
    pub location: ResourceLocation,
    pub path: PathBuf,
    /// Path relative to the effective plans root, including collection and file.
    pub relative_path: PathBuf,
    pub charter_name: String,
    pub inferred_parent: Option<String>,
    pub bytes: Vec<u8>,
    pub revision: ResourceRevision,
}

/// Immutable effective-vdir evidence retained for stale inventory validation.
#[derive(Clone, Debug)]
pub struct CalendarObservation {
    pub mounts: NativeWorkspaceMounts,
    pub inventory: MountInventory,
    pub resources: Vec<CalendarResource>,
}

/// Discover and read all visible `.ics` resources from the effective plans mount.
pub fn read_calendar_resources(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<Vec<CalendarResource>, WorkspaceError> {
    Ok(observe_calendar_resources(workspace_root, external_plans)?.resources)
}

/// Observe the effective plans inventory and immutable resource bytes together.
pub fn observe_calendar_resources(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<CalendarObservation, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let inventory = mounts.inventory()?;
    let effective_mount = if mounts.external_plans.is_some() {
        MountId::ExternalPlans
    } else {
        MountId::Workspace
    };
    let effective_inventory = match effective_mount {
        MountId::Workspace => &inventory.workspace,
        MountId::ExternalPlans => inventory
            .external_plans
            .as_ref()
            .ok_or_else(|| WorkspaceError::Actions("external plans inventory is missing".into()))?,
    };
    let paths = effective_inventory
        .files
        .paths()
        .filter(|path| calendar_relative_path(effective_mount, path).is_some())
        .cloned()
        .collect::<Vec<_>>();
    let read_plans = WorkspaceMounts {
        workspace: if effective_mount == MountId::Workspace {
            ReadPlan::new(paths.clone())
        } else {
            ReadPlan::default()
        },
        external_plans: mounts.external_plans.as_ref().map(|_| {
            if effective_mount == MountId::ExternalPlans {
                ReadPlan::new(paths.clone())
            } else {
                ReadPlan::default()
            }
        }),
    };
    let reads = mounts.read(&read_plans, &inventory)?;
    let evidence = match effective_mount {
        MountId::Workspace => &reads.workspace,
        MountId::ExternalPlans => reads.external_plans.as_ref().ok_or_else(|| {
            WorkspaceError::Actions("external plans read evidence is missing".into())
        })?,
    };
    if let Some(failure) = evidence.failures.first() {
        return Err(WorkspaceError::Actions(format!(
            "could not read calendar resource {}: {}",
            failure.path, failure.message
        )));
    }

    let project_root = mounts.scope.project_root_charter();
    let mut resources = Vec::new();
    for snapshot in evidence.snapshot.resources() {
        let Some(relative_path) = calendar_relative_path(effective_mount, snapshot.path()) else {
            continue;
        };
        let relative = PathBuf::from(relative_path);
        let Some(charter_name) = infer_plan_charter_name_for_workspace(&relative, project_root)
        else {
            continue;
        };
        let inferred_parent = infer_plan_parent_for_workspace(&relative, project_root);
        let location = ResourceLocation::new(effective_mount, snapshot.path().clone());
        resources.push(CalendarResource {
            path: mounts.physical_path(&location)?,
            location,
            relative_path: relative,
            charter_name,
            inferred_parent,
            bytes: snapshot.bytes().to_vec(),
            revision: snapshot.revision().clone(),
        });
    }
    resources.sort_by(|left, right| left.relative_path.cmp(&right.relative_path));
    Ok(CalendarObservation {
        mounts,
        inventory: effective_inventory.clone(),
        resources,
    })
}

fn effective_inventory(mounts: &NativeWorkspaceMounts) -> Result<MountInventory, WorkspaceError> {
    let inventory = mounts.inventory()?;
    if mounts.external_plans.is_some() {
        inventory
            .external_plans
            .ok_or_else(|| WorkspaceError::Actions("external plans inventory is missing".into()))
    } else {
        Ok(inventory.workspace)
    }
}

fn calendar_relative_path(mount: MountId, path: &WorkspacePath) -> Option<&str> {
    let relative = match mount {
        MountId::Workspace => path.as_str().strip_prefix("plans/")?,
        MountId::ExternalPlans => path.as_str(),
    };
    if !relative.ends_with(".ics")
        || relative
            .split('/')
            .any(|component| component.starts_with('.'))
    {
        return None;
    }
    Some(relative)
}

/// Apply one projected-occurrence operation through a stale-guarded native batch.
pub fn apply_occurrence_op(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    plan_id: Uuid,
    occurrence_key: &str,
    op: &OccurrenceOp,
) -> Result<(), WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let journal_dir = mounts.workspace.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;

    let observation = observe_calendar_resources(workspace_root, external_plans)?;
    let mut matched: Option<(&CalendarResource, ICSPlan)> = None;
    for resource in &observation.resources {
        let source = std::str::from_utf8(&resource.bytes)
            .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
        for plan in parse_ics(source, &resource.relative_path)? {
            if plan.plan.id != plan_id {
                continue;
            }
            if matched.is_some() {
                return Err(WorkspaceError::Parse(format!(
                    "recurring plan {plan_id} appears more than once in the configured plans vdir"
                )));
            }
            matched = Some((resource, plan));
        }
    }
    let Some((resource, plan)) = matched else {
        return Err(WorkspaceError::Parse(format!(
            "recurring plan {plan_id} not found in the configured plans vdir"
        )));
    };
    let uid = plan.plan.external_id.as_deref().ok_or_else(|| {
        WorkspaceError::Parse(format!("recurring plan {plan_id} has no UID to key on"))
    })?;
    let source = std::str::from_utf8(&resource.bytes)
        .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
    let rendered = render_occurrence_deviation(source, uid, occurrence_key, op)?;
    let preconditions = observation
        .resources
        .iter()
        .map(|resource| ResourcePrecondition {
            path: resource.location.clone(),
            expected: ExpectedResource::Revision(resource.revision.clone()),
        })
        .collect();
    let effects = EffectBatch::new(
        vec![Effect::Write {
            path: resource.location.clone(),
            bytes: rendered.into_bytes(),
        }],
        preconditions,
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;

    if effective_inventory(&observation.mounts)? != observation.inventory {
        return Err(WorkspaceError::Actions(
            "configured plans vdir changed before occurrence delivery".into(),
        ));
    }
    crate::validate_preconditions(&observation.mounts, effects.preconditions())?;
    crate::execute_effects(&observation.mounts, &journal_dir, effects.effects())
}

/// Normalize foreign recurring-master roll-forwards in one mounted transaction.
pub fn sync_master_rollforwards(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<usize, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let journal_dir = mounts.workspace.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;

    let observation = observe_calendar_resources(workspace_root, external_plans)?;
    let plans_root = observation
        .mounts
        .external_plans
        .clone()
        .unwrap_or_else(|| observation.mounts.workspace.join("plans"));
    let store_path = observation.mounts.workspace.join("sync/plans.json");
    let store_location =
        ResourceLocation::workspace(WorkspacePath::new("sync/plans.json").unwrap());
    let (store_source, store_expected) = match std::fs::read(&store_path) {
        Ok(bytes) => (
            Some(
                std::str::from_utf8(&bytes)
                    .map_err(|error| WorkspaceError::Parse(error.to_string()))?
                    .to_owned(),
            ),
            ExpectedResource::Revision(crate::mounts::content_revision(&bytes)),
        ),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            (None, ExpectedResource::Missing)
        }
        Err(error) => return Err(error.into()),
    };
    let store = decode_plans_sync_store(store_source.as_deref(), &plans_root)?;
    let resources = observation
        .resources
        .iter()
        .map(|resource| {
            Ok(PlanResourceState {
                location: resource.location.clone(),
                source: std::str::from_utf8(&resource.bytes)
                    .map_err(|error| WorkspaceError::Parse(error.to_string()))?
                    .to_owned(),
                expected: ExpectedResource::Revision(resource.revision.clone()),
            })
        })
        .collect::<Result<Vec<_>, WorkspaceError>>()?;
    let prepared = prepare_master_rollforwards(store, store_location, store_expected, &resources)?;

    if effective_inventory(&observation.mounts)? != observation.inventory {
        return Err(WorkspaceError::Actions(
            "configured plans vdir changed before roll-forward delivery".into(),
        ));
    }
    crate::validate_preconditions(&observation.mounts, prepared.effects().preconditions())?;
    crate::execute_effects(
        &observation.mounts,
        &journal_dir,
        prepared.effects().effects(),
    )?;
    Ok(prepared
        .adopt::<String>(Ok(()))
        .expect("successful native calendar delivery releases prepared state")
        .outcome)
}

/// Parse all standalone VTODO projections from the effective plans mount.
pub fn read_vtodo_actions(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<HashMap<Uuid, VTodoResource>, WorkspaceError> {
    let mut actions = HashMap::new();
    for resource in read_calendar_resources(workspace_root, external_plans)? {
        let plans_dir = resource
            .relative_path
            .parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| WorkspaceError::InvalidPath(resource.relative_path.clone()))?;
        let source = std::str::from_utf8(&resource.bytes)
            .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
        for action in parse_vtodo_actions_content(source)? {
            let projected = VTodoResource {
                action: action.clone(),
                path: resource.path.clone(),
                plans_dir: plans_dir.clone(),
                charter_name: resource.charter_name.clone(),
            };
            if actions.insert(action.id, projected).is_some() {
                return Err(WorkspaceError::Parse(format!(
                    "duplicate standalone VTODO Action identity {} in configured plans vdir",
                    action.id
                )));
            }
        }
    }
    Ok(actions)
}

fn absolute_path(path: &Path) -> Result<PathBuf, WorkspaceError> {
    if path.is_absolute() {
        Ok(path.to_path_buf())
    } else {
        Ok(std::env::current_dir()?.join(path))
    }
}

fn logical_path(path: &Path) -> Result<WorkspacePath, WorkspaceError> {
    let logical = path
        .components()
        .map(|component| component.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| WorkspaceError::InvalidPath(path.to_path_buf()))?
        .join("/");
    WorkspacePath::new(logical).map_err(|_| WorkspaceError::InvalidPath(path.to_path_buf()))
}

fn mutation_target(
    workspace_root: &Path,
    configured_external: Option<&Path>,
    target: &Path,
) -> Result<(NativeWorkspaceMounts, ResourceLocation, PathBuf), WorkspaceError> {
    let target = absolute_path(target)?;
    let configured = NativeWorkspaceMounts::resolve(workspace_root, configured_external);
    if let Ok(relative) = target.strip_prefix(&configured.workspace) {
        let location = ResourceLocation::workspace(logical_path(relative)?);
        return Ok((configured, location, target));
    }
    if let Some(external) = &configured.external_plans
        && let Ok(relative) = target.strip_prefix(external)
    {
        let location = ResourceLocation::external_plans(logical_path(relative)?);
        return Ok((configured, location, target));
    }

    // Loose `--file`: preserve exactly the named file and use its parent only as
    // an invocation-scoped external mount. No charter/vdir hierarchy is inferred.
    let parent = target
        .parent()
        .ok_or_else(|| WorkspaceError::InvalidPath(target.clone()))?;
    let file_name = target
        .file_name()
        .ok_or_else(|| WorkspaceError::InvalidPath(target.clone()))?;
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, Some(parent));
    let location = ResourceLocation::external_plans(logical_path(Path::new(file_name))?);
    Ok((mounts, location, target))
}

/// Write one Plan through the mounted, stale-guarded native effect boundary.
pub fn write_plan_file(
    workspace_root: &Path,
    configured_external: Option<&Path>,
    path: &Path,
    plan: &Plan,
) -> Result<(), WorkspaceError> {
    let (mounts, location, target) = mutation_target(workspace_root, configured_external, path)?;
    let journal_dir = mounts.workspace.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;
    let (source, expected) = match std::fs::read(&target) {
        Ok(bytes) => {
            let source = std::str::from_utf8(&bytes)
                .map_err(|error| WorkspaceError::Parse(error.to_string()))?
                .to_owned();
            (
                Some(source),
                ExpectedResource::Revision(crate::mounts::content_revision(&bytes)),
            )
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            (None, ExpectedResource::Missing)
        }
        Err(error) => return Err(error.into()),
    };
    let rendered = render_plan_resource(source.as_deref(), plan)?;
    let effects = EffectBatch::new(
        vec![Effect::Write {
            path: location.clone(),
            bytes: rendered.into_bytes(),
        }],
        vec![ResourcePrecondition {
            path: location,
            expected,
        }],
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    crate::validate_preconditions(&mounts, effects.preconditions())?;
    crate::execute_effects(&mounts, &journal_dir, effects.effects())
}

/// Delete one explicitly selected Plan resource through durable removal.
pub fn delete_plan_file(
    workspace_root: &Path,
    configured_external: Option<&Path>,
    path: &Path,
) -> Result<(), WorkspaceError> {
    let (mounts, location, target) = mutation_target(workspace_root, configured_external, path)?;
    let journal_dir = mounts.workspace.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;
    let bytes = std::fs::read(&target)?;
    let effects = EffectBatch::new(
        vec![Effect::Remove {
            path: location.clone(),
        }],
        vec![ResourcePrecondition {
            path: location,
            expected: ExpectedResource::Revision(crate::mounts::content_revision(&bytes)),
        }],
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    crate::validate_preconditions(&mounts, effects.preconditions())?;
    crate::execute_effects(&mounts, &journal_dir, effects.effects())
}

/// Read and parse one explicitly named calendar file.
///
/// This is the loose `--file` lane: the caller's path is preserved and no vdir
/// hierarchy is inferred or imposed.
pub fn read_ics_file(path: &Path) -> Result<Vec<ICSPlan>, WorkspaceError> {
    let content = std::fs::read_to_string(path)?;
    parse_ics(&content, path)
}

/// Read standalone Action projections from one explicitly named calendar file.
pub fn read_vtodo_file(path: &Path) -> Result<Vec<VTodoAction>, WorkspaceError> {
    let content = std::fs::read_to_string(path)?;
    parse_vtodo_actions_content(&content)
}

/// Native location of the machine-local plans merge-base store.
pub fn plans_sync_store_path(workspace_root: &Path) -> PathBuf {
    NativeWorkspaceMounts::resolve(workspace_root, None)
        .workspace
        .join("sync/plans.json")
}

/// Read and decode the plans merge-base store for the effective physical vdir.
pub fn read_plans_sync_store(
    workspace_root: &Path,
    plans_root: &Path,
) -> Result<PlansSyncStore, WorkspaceError> {
    let path = plans_sync_store_path(workspace_root);
    let content = match std::fs::read_to_string(path) {
        Ok(content) => Some(content),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
        Err(error) => return Err(error.into()),
    };
    decode_plans_sync_store(content.as_deref(), plans_root)
}

#[cfg(test)]
mod tests {
    use super::*;

    const PLAN: &str = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:weekly@example.com\r\nSUMMARY:Weekly\r\nDTSTART:20260821T120000Z\r\nRRULE:FREQ=WEEKLY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n";

    #[test]
    fn external_vdir_stays_a_distinct_mount_and_wins_when_configured() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let internal = project.join(".clearhead/plans/next");
        let external = temp.path().join("vdir/next");
        std::fs::create_dir_all(project.join(".clearhead/charters")).unwrap();
        std::fs::create_dir_all(&internal).unwrap();
        std::fs::create_dir_all(&external).unwrap();
        std::fs::write(internal.join("internal.ics"), PLAN).unwrap();
        std::fs::write(external.join("external.ics"), PLAN).unwrap();

        let resources = read_calendar_resources(&project, Some(&temp.path().join("vdir"))).unwrap();

        assert_eq!(resources.len(), 1);
        assert_eq!(resources[0].location.mount, MountId::ExternalPlans);
        assert_eq!(resources[0].location.path.as_str(), "next/external.ics");
        assert_eq!(
            resources[0].relative_path,
            PathBuf::from("next/external.ics")
        );
        assert_eq!(resources[0].charter_name, "project");
        assert_eq!(resources[0].path, external.join("external.ics"));
    }

    #[test]
    fn missing_sync_store_uses_the_effective_projection_root() {
        let temp = tempfile::tempdir().unwrap();
        let plans = temp.path().join("vdir");
        let store = read_plans_sync_store(temp.path(), &plans).unwrap();
        assert_eq!(store.plans_root, plans);
        assert!(store.actions.is_empty());
    }

    #[test]
    fn occurrence_write_targets_the_external_mount_without_flattening_it() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let external_file = external_root.join("next/weekly.ics");
        std::fs::create_dir_all(project.join(".clearhead/charters")).unwrap();
        std::fs::create_dir_all(external_file.parent().unwrap()).unwrap();
        std::fs::write(&external_file, PLAN).unwrap();
        let plan_id =
            clearhead_core::workspace::calendar::ics::plan_id_from_ics_uid("weekly@example.com");

        apply_occurrence_op(
            &project,
            Some(&external_root),
            plan_id,
            "20260821T120000Z",
            &OccurrenceOp::Skip,
        )
        .unwrap();

        let rendered = std::fs::read_to_string(&external_file).unwrap();
        assert!(rendered.contains("EXDATE:20260821T120000Z"));
        assert!(!project.join(".clearhead/plans").exists());
    }

    #[test]
    fn roll_forward_and_store_commit_across_workspace_and_external_mounts() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let external_file = external_root.join("next/weekly.ics");
        std::fs::create_dir_all(project.join(".clearhead/charters")).unwrap();
        std::fs::create_dir_all(external_file.parent().unwrap()).unwrap();
        std::fs::write(&external_file, PLAN).unwrap();

        assert_eq!(
            sync_master_rollforwards(&project, Some(&external_root)).unwrap(),
            0
        );
        let advanced = PLAN.replace("20260821T120000Z", "20260828T120000Z");
        std::fs::write(&external_file, advanced).unwrap();

        assert_eq!(
            sync_master_rollforwards(&project, Some(&external_root)).unwrap(),
            1
        );
        let rendered = std::fs::read_to_string(&external_file).unwrap();
        assert!(rendered.contains("DTSTART:20260821T120000Z"));
        assert!(rendered.contains("RECURRENCE-ID:20260821T120000Z"));
        assert!(plans_sync_store_path(&project).exists());
    }
}
