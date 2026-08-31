//! Native plans-vdir discovery, immutable reads, and sync-store persistence.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use chrono::Local;
use uuid::Uuid;

use crate::durability::{WorkspaceLock, recover_pending};
use clearhead_core::PlanComponentKind;
use clearhead_core::domain::DomainModel;
use clearhead_core::workspace::OccurrenceOp;
use clearhead_core::workspace::calendar::ics::{
    ICSPlan, VTodoAction, parse_ics, parse_vtodo_actions_content, plan_id_from_ics_uid,
    render_occurrence_deviation, render_plan_resource_with_component,
};
use clearhead_core::workspace::calendar::plans::{
    infer_plan_charter_name_for_workspace, infer_plan_parent_for_workspace,
};
use clearhead_core::workspace::calendar::reconcile::{
    AppliedSync, CalendarSyncPreparationInput, MaterializedOccurrenceArchiveState,
    MaterializedOccurrencePreparationInput, PlanResourceState, SyncActionResourceState,
    SyncConflictResolution, SyncMirrorResourceState, SyncPlanTemplate, SyncReport,
    plan_one_off_sync, plan_sync, prepare_master_rollforward_changes, prepare_master_rollforwards,
    prepare_materialized_occurrence_resolution, prepare_sync, sync_import_actions_file,
};
use clearhead_core::workspace::calendar::sync_store::{PlansSyncStore, decode_plans_sync_store};
use clearhead_core::workspace::resource::{
    Effect, EffectBatch, ExpectedResource, MountId, MountInventory, ReadPlan, ResourceLocation,
    ResourcePrecondition, ResourceRevision, WorkspaceMounts, WorkspacePath,
};
use clearhead_core::workspace::{
    VTodoResource, WorkspaceError, completed_actions_path, parse_actions, parse_sidecar,
    sidecar_path,
};
use clearhead_core::{Plan, action_mirror_path};

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

/// Report and delivery tally computed from the same locked sync evidence.
#[derive(Debug)]
pub struct CalendarSyncResult {
    pub report: SyncReport,
    pub applied: AppliedSync,
    pub rolled_forward: usize,
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

/// Recompute and deliver one calendar sync from fresh evidence under the native lock.
pub fn sync_calendar(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    conflict: Option<SyncConflictResolution>,
) -> Result<CalendarSyncResult, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let journal_dir = mounts.workspace.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;

    let inventory = mounts.inventory()?;
    let workspace = crate::mounts::load_workspace_model(workspace_root, external_plans)?;
    let mut observation = observe_calendar_resources(workspace_root, external_plans)?;
    let observed_effective = if mounts.external_plans.is_some() {
        inventory.external_plans.as_ref()
    } else {
        Some(&inventory.workspace)
    }
    .ok_or_else(|| WorkspaceError::Actions("external plans inventory is missing".into()))?;
    if &observation.inventory != observed_effective {
        return Err(WorkspaceError::Actions(
            "plans vdir changed while calendar sync was being read".into(),
        ));
    }

    let plans_root = mounts
        .external_plans
        .as_deref()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| mounts.workspace.join("plans"));
    let store_path = WorkspacePath::new("sync/plans.json")
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let store_location = ResourceLocation::workspace(store_path);
    let store_expected = expected_resource(&mounts, &store_location)?;
    let store = read_plans_sync_store(workspace_root, &plans_root)?;
    let plan_resources = plan_resource_states(&observation.resources)?;
    let rollforwards = prepare_master_rollforward_changes(store, &plan_resources)?;
    for write in &rollforwards.calendar_writes {
        let resource = observation
            .resources
            .iter_mut()
            .find(|resource| resource.location == write.location)
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "roll-forward write has no observed calendar resource: {}",
                    write.location
                ))
            })?;
        resource.bytes = write.content.as_bytes().to_vec();
    }
    let rolled_forward = rollforwards.recorded;
    let calendar_writes = rollforwards.calendar_writes;
    let store = rollforwards.store;
    let model = DomainModel {
        objectives: Vec::new(),
        charters: workspace.charters.iter().cloned().map(Into::into).collect(),
    };
    let LinkedOneOffResources {
        plans: one_off_plans,
        mirrors: linked_mirrors,
        plan_ids: linked_plan_ids,
        mut warnings,
    } = sync_linked_one_off_resources(&observation.resources, &model)?;
    let (mut calendar_actions, mut mirror_resources) =
        sync_mirror_resources(&observation.resources)?;
    calendar_actions.retain(|_, resource| {
        !linked_plan_ids.contains(&plan_id_from_ics_uid(&resource.action.uid))
    });
    let linked_action_ids = linked_mirrors
        .iter()
        .map(|resource| resource.action_id)
        .collect::<HashSet<_>>();
    mirror_resources.retain(|resource| !linked_action_ids.contains(&resource.action_id));

    let mut report = plan_sync(&model, &store, &calendar_actions)?;
    report.entries.retain(|entry| {
        model
            .all_actions()
            .into_iter()
            .find(|action| action.id == entry.action_id)
            .is_some_and(|action| action.plan_id.is_none())
    });
    report
        .imports
        .retain(|import| !linked_plan_ids.contains(&plan_id_from_ics_uid(&import.action.uid)));
    let linked_report = plan_one_off_sync(&model, &store, &one_off_plans)?;
    report.entries.extend(linked_report.entries);
    report.warnings.append(&mut warnings);
    let report = report.resolve_conflicts(conflict);
    mirror_resources.extend(linked_mirrors);

    for entry in &report.entries {
        if mirror_resources
            .iter()
            .any(|resource| resource.action_id == entry.action_id)
        {
            continue;
        }
        let (charter_idx, action_idx) = workspace
            .charters
            .iter()
            .enumerate()
            .find_map(|(charter_idx, charter)| {
                charter
                    .actions
                    .iter()
                    .position(|action| action.action.id == entry.action_id)
                    .map(|action_idx| (charter_idx, action_idx))
            })
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "sync action not found in workspace: {}",
                    entry.action_id
                ))
            })?;
        let target = action_mirror_path(
            &plans_root,
            &workspace.charters[charter_idx],
            &workspace.charters[charter_idx].actions[action_idx].action,
        );
        let (target_mounts, location, _) =
            mutation_target(workspace_root, external_plans, &target)?;
        if target_mounts != mounts {
            return Err(WorkspaceError::Actions(format!(
                "calendar mirror target escaped the configured plans mount: {}",
                target.display()
            )));
        }
        mirror_resources.push(SyncMirrorResourceState {
            action_id: entry.action_id,
            location,
            expected: ExpectedResource::Missing,
            source: None,
            component_kind: PlanComponentKind::VTodo,
        });
    }

    let action_resources = sync_action_resources(&workspace, &report, &mounts)?;
    let templates = sync_plan_templates(&workspace, &mounts.workspace)?;
    let observed_resources = sync_read_preconditions(&mounts, &inventory)?;
    let prepared = prepare_sync(
        CalendarSyncPreparationInput {
            workspace,
            store,
            action_resources,
            mirror_resources,
            calendar_writes,
            templates,
            observed_resources,
            now: Local::now(),
            store_location,
            store_expected,
        },
        &report,
    )?;

    if mounts.inventory()? != inventory {
        return Err(WorkspaceError::Actions(
            "workspace or plans vdir changed before calendar sync delivery".into(),
        ));
    }
    let applied = super::deliver(&mounts, &journal_dir, prepared)?;
    Ok(CalendarSyncResult {
        report,
        applied,
        rolled_forward,
    })
}

/// Resolve one closed materialized recurring token through native mounted delivery.
pub fn resolve_materialized_occurrence(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    occurrence_id: Uuid,
    operation: &OccurrenceOp,
    now: chrono::DateTime<Local>,
) -> Result<bool, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let journal_dir = mounts.workspace.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;

    let inventory = mounts.inventory()?;
    let workspace = crate::mounts::load_workspace_model(workspace_root, external_plans)?;
    let observation = observe_calendar_resources(workspace_root, external_plans)?;
    let observed_effective = if mounts.external_plans.is_some() {
        inventory.external_plans.as_ref()
    } else {
        Some(&inventory.workspace)
    }
    .ok_or_else(|| WorkspaceError::Actions("external plans inventory is missing".into()))?;
    if &observation.inventory != observed_effective {
        return Err(WorkspaceError::Actions(
            "plans vdir changed while materialized occurrence was being read".into(),
        ));
    }

    let plans_root = mounts
        .external_plans
        .as_deref()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| mounts.workspace.join("plans"));
    let store_path = WorkspacePath::new("sync/plans.json")
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let store_location = ResourceLocation::workspace(store_path);
    let store_expected = expected_resource(&mounts, &store_location)?;
    let store = read_plans_sync_store(workspace_root, &plans_root)?;
    let plan_resources = plan_resource_states(&observation.resources)?;
    let action_resources = sync_action_resources(&workspace, &SyncReport::default(), &mounts)?;
    let templates = sync_plan_templates(&workspace, &mounts.workspace)?;
    let archive = materialized_occurrence_archive(&workspace, &mounts, occurrence_id)?;
    let observed_resources = sync_read_preconditions(&mounts, &inventory)?;
    let prepared =
        prepare_materialized_occurrence_resolution(MaterializedOccurrencePreparationInput {
            workspace,
            store,
            occurrence_id,
            operation: operation.clone(),
            now,
            plan_resources,
            action_resources,
            templates,
            archive,
            observed_resources,
            store_location,
            store_expected,
        })?;
    if !prepared.outcome() {
        return Ok(false);
    }
    if mounts.inventory()? != inventory {
        return Err(WorkspaceError::Actions(
            "workspace or plans vdir changed before occurrence delivery".into(),
        ));
    }
    super::deliver(&mounts, &journal_dir, prepared)
}

fn materialized_occurrence_archive(
    workspace: &clearhead_core::workspace::Workspace,
    mounts: &NativeWorkspaceMounts,
    occurrence_id: Uuid,
) -> Result<Option<MaterializedOccurrenceArchiveState>, WorkspaceError> {
    for charter in &workspace.charters {
        let Some(actions_file) = &charter.actions_file else {
            continue;
        };
        let completed = completed_actions_path(actions_file);
        let completed_location =
            ResourceLocation::workspace(logical_path(&Path::new("charters").join(&completed))?);
        let completed_path = mounts.physical_path(&completed_location)?;
        let content = match std::fs::read_to_string(completed_path) {
            Ok(content) => content,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => return Err(error.into()),
        };
        let Some(action) = parse_actions(&content)
            .map_err(WorkspaceError::Actions)?
            .into_iter()
            .find(|action| action.id == occurrence_id)
        else {
            continue;
        };
        let sidecar = sidecar_path(&completed);
        let sidecar_location =
            ResourceLocation::workspace(logical_path(&Path::new("charters").join(sidecar))?);
        let sidecar_path = mounts.physical_path(&sidecar_location)?;
        let (metadata, sidecar_expected) = match std::fs::read(&sidecar_path) {
            Ok(bytes) => (
                parse_sidecar(
                    std::str::from_utf8(&bytes)
                        .map_err(|error| WorkspaceError::Parse(error.to_string()))?,
                )?,
                ExpectedResource::Revision(super::revision(&bytes)),
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                (Default::default(), ExpectedResource::Missing)
            }
            Err(error) => return Err(error.into()),
        };
        return Ok(Some(MaterializedOccurrenceArchiveState {
            action,
            metadata,
            sidecar_location,
            sidecar_expected,
        }));
    }
    Ok(None)
}

fn plan_resource_states(
    resources: &[CalendarResource],
) -> Result<Vec<PlanResourceState>, WorkspaceError> {
    resources
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
        .collect()
}

fn sync_mirror_resources(
    resources: &[CalendarResource],
) -> Result<(HashMap<Uuid, VTodoResource>, Vec<SyncMirrorResourceState>), WorkspaceError> {
    let mut actions = HashMap::new();
    let mut mirrors = Vec::new();
    for resource in resources {
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
            mirrors.push(SyncMirrorResourceState {
                action_id: action.id,
                location: resource.location.clone(),
                expected: ExpectedResource::Revision(resource.revision.clone()),
                source: Some(source.to_owned()),
                component_kind: PlanComponentKind::VTodo,
            });
        }
    }
    Ok((actions, mirrors))
}

struct LinkedOneOffResources {
    plans: Vec<ICSPlan>,
    mirrors: Vec<SyncMirrorResourceState>,
    plan_ids: HashSet<Uuid>,
    warnings: Vec<String>,
}

fn sync_linked_one_off_resources(
    resources: &[CalendarResource],
    model: &DomainModel,
) -> Result<LinkedOneOffResources, WorkspaceError> {
    let mut actions_by_plan = HashMap::new();
    for action in model
        .all_actions()
        .into_iter()
        .filter(|action| action.external_occurrence_key.is_none())
    {
        let Some(plan_id) = action.plan_id else {
            continue;
        };
        if actions_by_plan.insert(plan_id, action.id).is_some() {
            return Err(WorkspaceError::Parse(format!(
                "one-off Plan {plan_id} is linked to more than one Action"
            )));
        }
    }

    let mut plans = Vec::new();
    let mut mirrors = Vec::new();
    let mut linked_plan_ids = HashSet::new();
    let mut linked_action_ids = HashSet::new();
    let mut warnings = Vec::new();
    for resource in resources {
        let source = std::str::from_utf8(&resource.bytes)
            .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
        for plan in parse_ics(source, &resource.relative_path)? {
            if plan.plan.recurrence.is_none() {
                if let Some(&action_id) = actions_by_plan.get(&plan.plan.id) {
                    if !linked_action_ids.insert(action_id) {
                        return Err(WorkspaceError::Parse(format!(
                            "Action {action_id} is linked to more than one one-off Plan resource"
                        )));
                    }
                    linked_plan_ids.insert(plan.plan.id);
                    mirrors.push(SyncMirrorResourceState {
                        action_id,
                        location: resource.location.clone(),
                        expected: ExpectedResource::Revision(resource.revision.clone()),
                        source: Some(source.to_owned()),
                        component_kind: plan.component_kind,
                    });
                } else if plan.component_kind == PlanComponentKind::VEvent {
                    warnings.push(format!(
                        "calendar-created one-off VEVENT adoption is not implemented yet: {}",
                        resource.relative_path.display()
                    ));
                }
            }
            plans.push(plan);
        }
    }
    for (plan_id, action_id) in actions_by_plan {
        if !linked_plan_ids.contains(&plan_id) {
            warnings.push(format!(
                "linked one-off Plan {plan_id} for Action {action_id} is missing; deletion-as-unscheduling is not implemented yet"
            ));
        }
    }
    Ok(LinkedOneOffResources {
        plans,
        mirrors,
        plan_ids: linked_plan_ids,
        warnings,
    })
}

fn sync_action_resources(
    workspace: &clearhead_core::workspace::Workspace,
    report: &SyncReport,
    mounts: &NativeWorkspaceMounts,
) -> Result<Vec<SyncActionResourceState>, WorkspaceError> {
    let mut actions_files = workspace
        .charters
        .iter()
        .filter_map(|charter| charter.actions_file.clone())
        .collect::<Vec<_>>();
    for import in &report.imports {
        let charter = workspace
            .charters
            .iter()
            .find(|charter| charter.plans_dir == import.plans_dir)
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "calendar collection '{}' has no owning charter; run `clearhead doctor`",
                    import.plans_dir.display()
                ))
            })?;
        actions_files.push(sync_import_actions_file(charter, import));
    }
    actions_files.sort();
    actions_files.dedup();

    let mut resources = Vec::new();
    for actions_file in actions_files {
        let path = logical_path(&Path::new("charters").join(&actions_file))?;
        let location = ResourceLocation::workspace(path);
        let expected = expected_resource(mounts, &location)?;
        resources.push(SyncActionResourceState {
            actions_file,
            location,
            expected,
        });
    }
    Ok(resources)
}

fn sync_read_preconditions(
    mounts: &NativeWorkspaceMounts,
    inventory: &clearhead_core::workspace::resource::WorkspaceMounts<MountInventory>,
) -> Result<Vec<ResourcePrecondition>, WorkspaceError> {
    let mut preconditions = Vec::new();
    for path in inventory.workspace.files.paths() {
        let location = ResourceLocation::workspace(path.clone());
        let expected = expected_resource(mounts, &location)?;
        if expected == ExpectedResource::Missing {
            return Err(WorkspaceError::Actions(format!(
                "workspace resource disappeared during calendar sync: {location}"
            )));
        }
        preconditions.push(ResourcePrecondition {
            path: location,
            expected,
        });
    }
    if let Some(external) = &inventory.external_plans {
        for path in external.files.paths() {
            let location = ResourceLocation::external_plans(path.clone());
            let expected = expected_resource(mounts, &location)?;
            if expected == ExpectedResource::Missing {
                return Err(WorkspaceError::Actions(format!(
                    "calendar resource disappeared during sync: {location}"
                )));
            }
            preconditions.push(ResourcePrecondition {
                path: location,
                expected,
            });
        }
    }
    Ok(preconditions)
}

fn expected_resource(
    mounts: &NativeWorkspaceMounts,
    location: &ResourceLocation,
) -> Result<ExpectedResource, WorkspaceError> {
    match std::fs::read(mounts.physical_path(location)?) {
        Ok(bytes) => Ok(ExpectedResource::Revision(super::revision(&bytes))),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(ExpectedResource::Missing),
        Err(error) => Err(error.into()),
    }
}

fn sync_plan_templates(
    workspace: &clearhead_core::workspace::Workspace,
    data_root: &Path,
) -> Result<Vec<SyncPlanTemplate>, WorkspaceError> {
    let mut templates = Vec::new();
    for charter in &workspace.charters {
        let Some(actions_file) = &charter.actions_file else {
            continue;
        };
        let actions_path = data_root.join("charters").join(actions_file);
        let charter_dir = actions_path
            .parent()
            .unwrap_or_else(|| Path::new(data_root));
        for plan in &charter.plans {
            let Some(template_name) = plan.plan.template_name.as_deref() else {
                continue;
            };
            let Some(template_path) =
                crate::templates::resolve_template(charter_dir, data_root, template_name)?
            else {
                continue;
            };
            let steps = crate::read_actions(&template_path)?;
            let generated_ids = steps.iter().map(|_| Uuid::now_v7()).collect();
            templates.push(SyncPlanTemplate {
                plan_id: plan.plan.id,
                steps,
                generated_ids,
            });
        }
    }
    Ok(templates)
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
    component_kind: PlanComponentKind,
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
    let rendered = render_plan_resource_with_component(source.as_deref(), plan, component_kind)?;
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
    fn sync_commits_action_mirror_and_store_across_distinct_mounts() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(external_root.join("inbox")).unwrap();
        let action_id = Uuid::parse_str("019baaec-00b6-7991-be34-94b68212619a").unwrap();
        std::fs::write(
            &actions,
            format!("[ ] Sync me @2026-04-28T10:00 #{action_id}\n"),
        )
        .unwrap();

        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert_eq!(result.applied.take_action, 1);
        let mirror = external_root.join("inbox").join(format!("{action_id}.ics"));
        assert!(
            std::fs::read_to_string(mirror)
                .unwrap()
                .contains("SUMMARY:Sync me")
        );
        assert!(plans_sync_store_path(&project).exists());
        assert!(!project.join(".clearhead/plans").exists());
    }

    #[test]
    fn sync_patches_multiple_owned_vtodos_in_one_resource() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let calendar = external_root.join("inbox/shared.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(calendar.parent().unwrap()).unwrap();
        let first = "019baaec-00b6-7991-be34-94b68212619a";
        let second = "019baaec-00b6-7991-be34-94b68212619b";
        std::fs::write(
            &actions,
            format!("[ ] First #{first}\n[ ] Second #{second}\n"),
        )
        .unwrap();
        std::fs::write(
            &calendar,
            format!(
                "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{first}\r\nSUMMARY:First\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nBEGIN:VTODO\r\nUID:{second}\r\nSUMMARY:Second\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
            ),
        )
        .unwrap();
        sync_calendar(&project, Some(&external_root), None).unwrap();

        std::fs::write(
            &actions,
            format!("[ ] First changed #{first}\n[ ] Second changed #{second}\n"),
        )
        .unwrap();
        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert_eq!(result.applied.take_action, 2);
        let rendered = std::fs::read_to_string(calendar).unwrap();
        assert!(rendered.contains("SUMMARY:First changed"));
        assert!(rendered.contains("SUMMARY:Second changed"));
        assert_eq!(rendered.matches("BEGIN:VCALENDAR").count(), 1);
    }

    fn write_plan_link_sidecar(actions: &Path, action_id: Uuid, uid: &str) {
        let sidecar = actions.parent().unwrap().join(".inbox.json");
        std::fs::write(
            sidecar,
            format!(r#"{{"actions":{{"{action_id}":{{"plan":{{"uid":"{uid}"}}}}}}}}"#),
        )
        .unwrap();
    }

    #[test]
    fn unlinked_vevent_adoption_is_explicitly_deferred() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let calendar = external_root.join("inbox/event.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(calendar.parent().unwrap()).unwrap();
        std::fs::write(&actions, "").unwrap();
        std::fs::write(
            calendar,
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:new@example.com\r\nSUMMARY:New\r\nDTSTART:20260420T100000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n",
        )
        .unwrap();

        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert!(
            result
                .report
                .warnings
                .iter()
                .any(|warning| warning.contains("VEVENT adoption is not implemented"))
        );
        assert!(
            parse_actions(&std::fs::read_to_string(actions).unwrap())
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn missing_linked_plan_deletion_is_explicitly_deferred() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(external_root.join("inbox")).unwrap();
        let action_id = Uuid::parse_str("019baaec-00b6-7991-be34-94b68212619c").unwrap();
        std::fs::write(
            &actions,
            format!("[ ] Keep me @2026-04-20T10:00:00+00:00 #{action_id}\n"),
        )
        .unwrap();
        write_plan_link_sidecar(&actions, action_id, "missing@example.com");

        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert!(
            result
                .report
                .warnings
                .iter()
                .any(|warning| warning.contains("deletion-as-unscheduling is not implemented"))
        );
        let parsed = parse_actions(&std::fs::read_to_string(actions).unwrap()).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].id, action_id);
        assert!(parsed[0].scheduled_at.is_some());
    }

    #[test]
    fn linked_vevent_reschedule_updates_only_action_schedule() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let calendar = external_root.join("inbox/event.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(calendar.parent().unwrap()).unwrap();
        let action_id = Uuid::parse_str("019baaec-00b6-7991-be34-94b68212619a").unwrap();
        let uid = "foreign-event@example.com";
        std::fs::write(
            &actions,
            format!(
                "[ ] Local title @2026-04-20T10:00:00+00:00 :2026-04-20T11:00:00+00:00 !1 +local #{action_id}\n"
            ),
        )
        .unwrap();
        write_plan_link_sidecar(&actions, action_id, uid);
        let initial = format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:{uid}\r\nSUMMARY:Calendar display\r\nDTSTART:20260420T100000Z\r\nDTEND:20260420T110000Z\r\nX-VENDOR-KEEP:yes\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
        );
        std::fs::write(&calendar, &initial).unwrap();
        sync_calendar(&project, Some(&external_root), None).unwrap();

        std::fs::write(
            &calendar,
            initial
                .replace("SUMMARY:Calendar display", "SUMMARY:Peer renamed")
                .replace("20260420T100000Z", "20260421T120000Z")
                .replace("20260420T110000Z", "20260421T130000Z"),
        )
        .unwrap();
        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert_eq!(result.applied.take_calendar, 1);
        let parsed = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].id, action_id);
        assert_eq!(parsed[0].name, "Local title");
        assert_eq!(
            parsed[0].state,
            clearhead_core::domain::ActionState::NotStarted
        );
        assert_eq!(parsed[0].priority, Some(1));
        assert_eq!(
            parsed[0].contexts.as_deref(),
            Some(["local".to_string()].as_slice())
        );
        assert_eq!(
            parsed[0]
                .scheduled_at
                .unwrap()
                .with_timezone(&chrono::Utc)
                .format("%Y%m%dT%H%M%SZ")
                .to_string(),
            "20260421T120000Z"
        );
        let rendered = std::fs::read_to_string(calendar).unwrap();
        assert!(rendered.contains("SUMMARY:Peer renamed"));
        assert!(rendered.contains("X-VENDOR-KEEP:yes"));
    }

    #[test]
    fn linked_vtodo_peer_edit_updates_full_profile_without_reidentifying_action() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let calendar = external_root.join("inbox/task.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(calendar.parent().unwrap()).unwrap();
        let action_id = Uuid::parse_str("019baaec-00b6-7991-be34-94b68212619b").unwrap();
        let uid = "foreign-task@example.com";
        std::fs::write(
            &actions,
            format!(
                "[ ] Base title $Base description$ @2026-04-20T10:00:00+00:00 :2026-04-20T11:00:00+00:00 !5 +base #{action_id}\n"
            ),
        )
        .unwrap();
        write_plan_link_sidecar(&actions, action_id, uid);
        let initial = format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Base title\r\nDESCRIPTION:Base description\r\nDTSTART:20260420T100000Z\r\nDUE:20260420T110000Z\r\nSTATUS:NEEDS-ACTION\r\nPRIORITY:5\r\nCATEGORIES:base\r\nX-VENDOR-KEEP:yes\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        );
        std::fs::write(&calendar, &initial).unwrap();
        sync_calendar(&project, Some(&external_root), None).unwrap();

        let edited = format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Calendar title\r\nDESCRIPTION:Calendar description\r\nDTSTART:20260421T120000Z\r\nDUE:20260421T130000Z\r\nSTATUS:IN-PROCESS\r\nPRIORITY:2\r\nCATEGORIES:calendar,home\r\nX-VENDOR-KEEP:yes\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        );
        std::fs::write(&calendar, edited).unwrap();
        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert_eq!(result.applied.take_calendar, 1);
        let parsed = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(parsed.len(), 1);
        let action = &parsed[0];
        assert_eq!(action.id, action_id);
        assert_eq!(action.name, "Calendar title");
        assert_eq!(action.description.as_deref(), Some("Calendar description"));
        assert_eq!(
            action.state,
            clearhead_core::domain::ActionState::InProgress
        );
        assert_eq!(action.priority, Some(2));
        assert_eq!(
            action.contexts.as_deref(),
            Some(["calendar".to_string(), "home".to_string()].as_slice())
        );
        assert_eq!(
            action
                .scheduled_at
                .unwrap()
                .with_timezone(&chrono::Utc)
                .format("%Y%m%dT%H%M%SZ")
                .to_string(),
            "20260421T120000Z"
        );
        assert!(
            std::fs::read_to_string(calendar)
                .unwrap()
                .contains("X-VENDOR-KEEP:yes")
        );
    }

    #[test]
    fn calendar_sync_folds_rollforward_into_its_mixed_mount_batch() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let external_file = external_root.join("next/weekly.ics");
        let actions = project.join(".clearhead/charters/next.actions");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(external_file.parent().unwrap()).unwrap();
        std::fs::write(&actions, "").unwrap();
        std::fs::write(&external_file, PLAN).unwrap();

        assert_eq!(
            sync_calendar(&project, Some(&external_root), None)
                .unwrap()
                .rolled_forward,
            0
        );
        let advanced = PLAN.replace("20260821T120000Z", "20260828T120000Z");
        std::fs::write(&external_file, advanced).unwrap();

        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert_eq!(result.rolled_forward, 1);
        let rendered = std::fs::read_to_string(&external_file).unwrap();
        assert!(rendered.contains("DTSTART:20260821T120000Z"));
        assert!(rendered.contains("RECURRENCE-ID:20260821T120000Z"));
        assert!(plans_sync_store_path(&project).exists());
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
