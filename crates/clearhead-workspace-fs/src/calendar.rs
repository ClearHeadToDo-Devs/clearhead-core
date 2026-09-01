//! Native plans-vdir discovery, immutable reads, and sync-store persistence.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use chrono::Local;
use uuid::Uuid;

use crate::durability::{WorkspaceLock, recover_pending};
use clearhead_core::PlanComponentKind;
use clearhead_core::domain::{ActionState, DomainModel};
use clearhead_core::workspace::OccurrenceOp;
use clearhead_core::workspace::calendar::ics::{
    ICSPlan, PlanActionProjection, parse_ics, render_occurrence_deviation,
    render_plan_resource_with_component,
};
use clearhead_core::workspace::calendar::plans::{
    infer_plan_charter_name_for_workspace, infer_plan_parent_for_workspace,
};
use clearhead_core::workspace::calendar::reconcile::{
    AppliedSync, CalendarSyncPreparationInput, CalendarSyncState,
    MaterializedOccurrenceArchiveState, MaterializedOccurrencePreparationInput, PlanResourceState,
    SyncActionResourceState, SyncCodecMigration, SyncConflictResolution, SyncImport,
    SyncLifecycleEntry, SyncLifecycleKind, SyncMirrorResourceState, SyncPlanLink, SyncPlanTemplate,
    SyncPlanUnlink, SyncReport, plan_one_off_sync, plan_recurring_occurrence_sync,
    prepare_master_rollforward_changes, prepare_master_rollforwards,
    prepare_materialized_occurrence_resolution, prepare_sync, sync_import_actions_file,
};
use clearhead_core::workspace::calendar::sync_store::{PlansSyncStore, decode_plans_sync_store};
use clearhead_core::workspace::resource::{
    Effect, EffectBatch, ExpectedResource, MountId, MountInventory, PreparedMutation, ReadPlan,
    ResourceLocation, ResourcePrecondition, ResourceRevision, WorkspaceMounts, WorkspacePath,
};
use clearhead_core::workspace::{
    WorkspaceError, completed_actions_path, parse_actions, parse_sidecar, sidecar_path,
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

/// Read-only result from the exact planner used by an applied calendar sync.
#[derive(Debug)]
pub struct CalendarSyncPreview {
    pub report: SyncReport,
    pub rolled_forward: usize,
}

struct PreparedCalendarSync {
    mounts: NativeWorkspaceMounts,
    inventory: clearhead_core::workspace::resource::WorkspaceMounts<MountInventory>,
    report: SyncReport,
    prepared: PreparedMutation<CalendarSyncState, AppliedSync>,
    rolled_forward: usize,
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
    sync_calendar_with_component(
        workspace_root,
        external_plans,
        conflict,
        PlanComponentKind::VTodo,
    )
}

pub fn sync_calendar_with_component(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    conflict: Option<SyncConflictResolution>,
    configured_component: PlanComponentKind,
) -> Result<CalendarSyncResult, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let journal_dir = mounts.workspace.join("charters");
    std::fs::create_dir_all(&journal_dir)?;
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(mounts.workspace.clone()))?;
    recover_pending(&journal_dir)?;

    let planned = prepare_calendar_sync(
        workspace_root,
        external_plans,
        conflict,
        configured_component,
        mounts,
    )?;
    if planned.mounts.inventory()? != planned.inventory {
        return Err(WorkspaceError::Actions(
            "workspace or plans vdir changed before calendar sync delivery".into(),
        ));
    }
    let applied = super::deliver(&planned.mounts, &journal_dir, planned.prepared)?;
    Ok(CalendarSyncResult {
        report: planned.report,
        applied,
        rolled_forward: planned.rolled_forward,
    })
}

/// Compute the exact Plan-native lifecycle sync without locking, journaling, or delivery.
pub fn preview_calendar_sync_with_component(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    conflict: Option<SyncConflictResolution>,
    configured_component: PlanComponentKind,
) -> Result<CalendarSyncPreview, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let planned = prepare_calendar_sync(
        workspace_root,
        external_plans,
        conflict,
        configured_component,
        mounts,
    )?;
    Ok(CalendarSyncPreview {
        report: planned.report,
        rolled_forward: planned.rolled_forward,
    })
}

fn prepare_calendar_sync(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    conflict: Option<SyncConflictResolution>,
    configured_component: PlanComponentKind,
    mounts: NativeWorkspaceMounts,
) -> Result<PreparedCalendarSync, WorkspaceError> {
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
        mirrors: mut linked_mirrors,
        plan_ids: observed_one_off_plan_ids,
        unlinked: unlinked_one_off_plans,
    } = sync_linked_one_off_resources(&observation.resources, &model)?;
    let mut linked_model = model.clone();
    let mut plan_links = Vec::new();
    let mut lifecycle_imports = Vec::new();
    let uid_evidence: HashMap<Uuid, String> =
        store.field_bases(clearhead_core::workspace::calendar::sync_store::UID_FIELD)?;
    for observed in unlinked_one_off_plans {
        let uid = observed.plan.plan.external_id.clone().ok_or_else(|| {
            WorkspaceError::Parse(format!(
                "one-off Plan {} has no interoperable UID",
                observed.plan.plan.id
            ))
        })?;
        let migration_candidates = workspace
            .charters
            .iter()
            .filter(|charter| charter.plans_dir == observed.plans_dir)
            .flat_map(|charter| &charter.actions)
            .filter(|action| {
                action.action.plan_id.is_none()
                    && action.action.external_occurrence_key.is_none()
                    && (Uuid::parse_str(&uid).ok() == Some(action.action.id)
                        || uid_evidence.get(&action.action.id) == Some(&uid))
            })
            .map(|action| action.action.id)
            .collect::<Vec<_>>();
        let migration_action = match migration_candidates.as_slice() {
            [] => None,
            [action_id] => Some(*action_id),
            _ => {
                return Err(WorkspaceError::Parse(format!(
                    "legacy VTODO UID {uid} matches more than one unlinked Action in its charter"
                )));
            }
        };

        let action_id = if let Some(action_id) = migration_action {
            for charter in &mut linked_model.charters {
                if let Some(action) = charter
                    .actions
                    .iter_mut()
                    .find(|action| action.id == action_id)
                {
                    action.plan_id = Some(observed.plan.plan.id);
                }
            }
            action_id
        } else {
            let action_id = Uuid::now_v7();
            lifecycle_imports.push(SyncImport {
                action: action_projection_from_plan(action_id, &observed.plan)?,
                plans_dir: observed.plans_dir,
                charter_name: observed.charter_name,
            });
            action_id
        };
        linked_mirrors.push(SyncMirrorResourceState {
            action_id,
            location: observed.location,
            expected: observed.expected,
            source: Some(observed.source),
            component_kind: observed.plan.component_kind,
        });
        plan_links.push(SyncPlanLink { action_id, uid });
    }

    let linked_locations = linked_mirrors
        .iter()
        .map(|resource| (resource.action_id, resource.location.clone()))
        .collect::<HashMap<_, _>>();
    let newly_linked_ids = plan_links
        .iter()
        .map(|link| link.action_id)
        .collect::<HashSet<_>>();
    let mut plan_unlinks = Vec::new();
    let mut lifecycle = Vec::new();
    for action in linked_model.all_actions() {
        let Some(plan_id) = action.plan_id else {
            continue;
        };
        if action.external_occurrence_key.is_some() {
            continue;
        }
        let calendar_location = linked_locations.get(&action.id).cloned();
        let calendar_deleted = !observed_one_off_plan_ids.contains(&plan_id);
        let action_unscheduled = !newly_linked_ids.contains(&action.id)
            && action.scheduled_at.is_none()
            && action.due_date.is_none();
        if calendar_deleted || action_unscheduled {
            let kind = if action_unscheduled {
                SyncLifecycleKind::ActionUnscheduled
            } else {
                SyncLifecycleKind::CalendarDeleted
            };
            lifecycle.push(SyncLifecycleEntry {
                action_id: action.id,
                kind,
            });
            plan_unlinks.push(SyncPlanUnlink {
                action_id: action.id,
                calendar_location: if action_unscheduled {
                    calendar_location
                } else {
                    None
                },
            });
        }
    }
    let unlink_ids = plan_unlinks
        .iter()
        .map(|unlink| unlink.action_id)
        .collect::<HashSet<_>>();

    let mut report = plan_one_off_sync(&linked_model, &store, &one_off_plans)?;
    let recurring_report = plan_recurring_occurrence_sync(&linked_model, &store, &one_off_plans)?;
    report.entries.extend(recurring_report.entries);
    report.warnings.extend(recurring_report.warnings);
    report.imports.extend(lifecycle_imports);
    report.lifecycle = lifecycle;
    let mut mirror_resources = linked_mirrors;
    mirror_resources.extend(sync_recurring_mirror_resources(
        &observation.resources,
        &store,
    )?);
    report
        .entries
        .retain(|entry| !unlink_ids.contains(&entry.action_id));

    let existing_mirror_ids = mirror_resources
        .iter()
        .map(|resource| resource.action_id)
        .collect::<HashSet<_>>();
    let creation_ids = report
        .entries
        .iter()
        .filter_map(|entry| {
            linked_model
                .all_actions()
                .into_iter()
                .find(|action| action.id == entry.action_id)
                .filter(|action| {
                    action.plan_id.is_none()
                        && action.scheduled_at.is_some()
                        && !existing_mirror_ids.contains(&action.id)
                })
                .map(|action| action.id)
        })
        .collect::<HashSet<_>>();
    if configured_component == PlanComponentKind::VEvent {
        for entry in &mut report.entries {
            if creation_ids.contains(&entry.action_id) {
                entry.state = clearhead_core::workspace::calendar::Reconcile::NoOp;
                entry.title = clearhead_core::workspace::calendar::Reconcile::NoOp;
                entry.description = clearhead_core::workspace::calendar::Reconcile::NoOp;
                entry.priority = clearhead_core::workspace::calendar::Reconcile::NoOp;
                entry.contexts = clearhead_core::workspace::calendar::Reconcile::NoOp;
            }
        }
    }
    let mut report = report.resolve_conflicts(conflict);
    let codec_migrations = if report.tally().conflict == 0 {
        let migrations = prepare_codec_migrations(&observation.resources, configured_component)?;
        if !migrations.is_empty() {
            report.warnings.push(format!(
                "migrate {} Plan resource(s) to configured {} codec",
                migrations.len(),
                configured_component
            ));
        }
        migrations
    } else {
        report.warnings.push(
            "codec migration deferred until calendar synchronization conflicts are resolved".into(),
        );
        Vec::new()
    };

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
        let component_kind = if creation_ids.contains(&entry.action_id) {
            plan_links.push(SyncPlanLink {
                action_id: entry.action_id,
                uid: entry.action_id.to_string(),
            });
            configured_component
        } else {
            PlanComponentKind::VTodo
        };
        mirror_resources.push(SyncMirrorResourceState {
            action_id: entry.action_id,
            location,
            expected: ExpectedResource::Missing,
            source: None,
            component_kind,
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
            plan_links,
            plan_unlinks,
            mirror_resources,
            calendar_writes,
            codec_migrations,
            templates,
            observed_resources,
            now: Local::now(),
            store_location,
            store_expected,
        },
        &report,
    )?;

    Ok(PreparedCalendarSync {
        mounts,
        inventory,
        report,
        prepared,
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

struct ObservedOneOffPlan {
    plan: ICSPlan,
    plans_dir: PathBuf,
    charter_name: String,
    location: ResourceLocation,
    expected: ExpectedResource,
    source: String,
}

struct LinkedOneOffResources {
    plans: Vec<ICSPlan>,
    mirrors: Vec<SyncMirrorResourceState>,
    plan_ids: HashSet<Uuid>,
    unlinked: Vec<ObservedOneOffPlan>,
}

fn prepare_codec_migrations(
    resources: &[CalendarResource],
    configured_component: PlanComponentKind,
) -> Result<Vec<SyncCodecMigration>, WorkspaceError> {
    let mut migrations = Vec::new();
    for resource in resources {
        let source = std::str::from_utf8(&resource.bytes)
            .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
        for plan in parse_ics(source, &resource.relative_path)? {
            if plan.component_kind != configured_component {
                migrations.push(SyncCodecMigration {
                    location: resource.location.clone(),
                    source: source.to_owned(),
                    plan: plan.plan,
                    component_kind: configured_component,
                });
            }
        }
    }
    Ok(migrations)
}

fn sync_recurring_mirror_resources(
    resources: &[CalendarResource],
    store: &PlansSyncStore,
) -> Result<Vec<SyncMirrorResourceState>, WorkspaceError> {
    let links = store.occurrence_links();
    let occurrence_ids_by_plan = links.into_iter().fold(
        HashMap::<Uuid, Vec<Uuid>>::new(),
        |mut by_plan, (occurrence_id, (plan_id, _))| {
            by_plan.entry(plan_id).or_default().push(occurrence_id);
            by_plan
        },
    );
    let mut mirrors = Vec::new();
    let mut found_plans = HashSet::new();
    for resource in resources {
        let source = std::str::from_utf8(&resource.bytes)
            .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
        for plan in parse_ics(source, &resource.relative_path)? {
            let Some(occurrence_ids) = occurrence_ids_by_plan.get(&plan.plan.id) else {
                continue;
            };
            if !found_plans.insert(plan.plan.id) {
                return Err(WorkspaceError::Parse(format!(
                    "recurring Plan {} is present in more than one calendar resource",
                    plan.plan.id
                )));
            }
            for &action_id in occurrence_ids {
                mirrors.push(SyncMirrorResourceState {
                    action_id,
                    location: resource.location.clone(),
                    expected: ExpectedResource::Revision(resource.revision.clone()),
                    source: Some(source.to_owned()),
                    component_kind: plan.component_kind,
                });
            }
        }
    }
    Ok(mirrors)
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
    let mut unlinked = Vec::new();
    for resource in resources {
        let source = std::str::from_utf8(&resource.bytes)
            .map_err(|error| WorkspaceError::Parse(error.to_string()))?;
        for plan in parse_ics(source, &resource.relative_path)? {
            if plan.plan.recurrence.is_none() {
                if !linked_plan_ids.insert(plan.plan.id) {
                    return Err(WorkspaceError::Parse(format!(
                        "one-off Plan {} is present in more than one calendar resource",
                        plan.plan.id
                    )));
                }
                if let Some(&action_id) = actions_by_plan.get(&plan.plan.id) {
                    if !linked_action_ids.insert(action_id) {
                        return Err(WorkspaceError::Parse(format!(
                            "Action {action_id} is linked to more than one one-off Plan resource"
                        )));
                    }
                    mirrors.push(SyncMirrorResourceState {
                        action_id,
                        location: resource.location.clone(),
                        expected: ExpectedResource::Revision(resource.revision.clone()),
                        source: Some(source.to_owned()),
                        component_kind: plan.component_kind,
                    });
                } else {
                    unlinked.push(ObservedOneOffPlan {
                        plan: plan.clone(),
                        plans_dir: resource
                            .relative_path
                            .parent()
                            .map(Path::to_path_buf)
                            .ok_or_else(|| {
                                WorkspaceError::InvalidPath(resource.relative_path.clone())
                            })?,
                        charter_name: resource.charter_name.clone(),
                        location: resource.location.clone(),
                        expected: ExpectedResource::Revision(resource.revision.clone()),
                        source: source.to_owned(),
                    });
                }
            }
            plans.push(plan);
        }
    }
    Ok(LinkedOneOffResources {
        plans,
        mirrors,
        plan_ids: linked_plan_ids,
        unlinked,
    })
}

fn action_projection_from_plan(
    action_id: Uuid,
    plan: &ICSPlan,
) -> Result<PlanActionProjection, WorkspaceError> {
    let uid = plan.plan.external_id.clone().ok_or_else(|| {
        WorkspaceError::Parse(format!("one-off Plan {} has no UID", plan.plan.id))
    })?;
    let task = plan.task_fields.as_ref();
    Ok(PlanActionProjection {
        id: action_id,
        uid,
        scheduled_at: plan.plan.dtstart,
        due_date: plan.schedule_end,
        state: task.map_or(ActionState::NotStarted, |task| task.state),
        title: plan.plan.name.clone(),
        description: plan.plan.description.clone(),
        priority: task.and_then(|task| task.priority),
        contexts: task.and_then(|task| task.contexts.clone()),
        completed_at: task.and_then(|task| task.completed_at),
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
        let sidecar_relative = sidecar_path(&actions_file);
        let sidecar_location = ResourceLocation::workspace(logical_path(
            &Path::new("charters").join(sidecar_relative),
        )?);
        let sidecar_expected = expected_resource(mounts, &sidecar_location)?;
        let sidecar_physical = mounts.physical_path(&sidecar_location)?;
        let sidecar = match std::fs::read_to_string(sidecar_physical) {
            Ok(source) => parse_sidecar(&source)?,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Default::default(),
            Err(error) => return Err(error.into()),
        };
        resources.push(SyncActionResourceState {
            actions_file,
            location,
            expected,
            sidecar_location,
            sidecar_expected,
            sidecar,
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
    use chrono::Timelike;

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
            format!(
                "[ ] First @2026-04-20T10:00:00+00:00 #{first}\n[ ] Second @2026-04-20T11:00:00+00:00 #{second}\n"
            ),
        )
        .unwrap();
        std::fs::write(
            &calendar,
            format!(
                "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{first}\r\nSUMMARY:First\r\nDTSTART:20260420T100000Z\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nBEGIN:VTODO\r\nUID:{second}\r\nSUMMARY:Second\r\nDTSTART:20260420T110000Z\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
            ),
        )
        .unwrap();
        sync_calendar(&project, Some(&external_root), None).unwrap();
        let sidecar = std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json"))
            .unwrap_or_else(|_| "MISSING".into());
        assert!(sidecar.contains(first), "{sidecar}");
        assert!(sidecar.contains(second), "{sidecar}");

        std::fs::write(
            &actions,
            format!(
                "[ ] First changed @2026-04-20T10:00:00+00:00 #{first}\n[ ] Second changed @2026-04-20T11:00:00+00:00 #{second}\n"
            ),
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
    fn unlinked_vevent_adopts_native_action_and_is_idempotent() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let calendar = external_root.join("inbox/event.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(calendar.parent().unwrap()).unwrap();
        std::fs::write(&actions, "").unwrap();
        std::fs::write(
            &calendar,
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:new@example.com\r\nSUMMARY:New\r\nDTSTART:20260420T100000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n",
        )
        .unwrap();

        let result = sync_calendar(&project, Some(&external_root), None).unwrap();

        assert_eq!(result.applied.take_calendar, 1);
        let adopted = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(adopted.len(), 1);
        assert_eq!(adopted[0].name, "New");
        assert_eq!(adopted[0].state, ActionState::NotStarted);
        assert_eq!(adopted[0].id.get_version_num(), 7);
        let sidecar = parse_sidecar(
            &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            sidecar.actions[&adopted[0].id.to_string()]
                .plan
                .as_ref()
                .unwrap()
                .uid,
            "new@example.com"
        );
        assert!(calendar.exists(), "transport-selected path is retained");

        sync_calendar(&project, Some(&external_root), None).unwrap();
        assert_eq!(
            parse_actions(&std::fs::read_to_string(actions).unwrap())
                .unwrap()
                .len(),
            1
        );
    }

    #[test]
    fn scheduled_action_creates_configured_vevent_with_link_and_bases() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(external_root.join("inbox")).unwrap();
        let id = Uuid::parse_str("019baaec-00b6-7991-be34-94b6821261a1").unwrap();
        std::fs::write(
            &actions,
            format!("[ ] Focus $Local detail$ @2026-04-20T10:00:00+00:00 :2026-04-20T11:00:00+00:00 !2 +deep #{id}\n"),
        )
        .unwrap();

        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();

        let resource = external_root.join(format!("inbox/{id}.ics"));
        let rendered = std::fs::read_to_string(&resource).unwrap();
        assert!(rendered.contains("BEGIN:VEVENT"));
        assert!(!rendered.contains("BEGIN:VTODO"));
        assert!(rendered.contains("UID:019baaec-00b6-7991-be34-94b6821261a1"));
        assert!(rendered.contains("SUMMARY:Focus"));
        assert!(rendered.contains("DESCRIPTION:Local detail"));
        assert!(!rendered.contains("PRIORITY:2"));
        let sidecar = parse_sidecar(
            &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            sidecar.actions[&id.to_string()].plan.as_ref().unwrap().uid,
            id.to_string()
        );
        let store = read_plans_sync_store(&project, &external_root).unwrap();
        assert_eq!(
            store
                .field_bases::<String>(clearhead_core::workspace::calendar::sync_store::UID_FIELD,)
                .unwrap()[&id],
            id.to_string()
        );

        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();
        assert_eq!(
            read_calendar_resources(&project, Some(&external_root))
                .unwrap()
                .len(),
            1
        );
    }

    #[test]
    fn scheduled_action_creates_full_profile_vtodo() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(external_root.join("inbox")).unwrap();
        let id = Uuid::parse_str("019baaec-00b6-7991-be34-94b6821261a2").unwrap();
        std::fs::write(
            &actions,
            format!("[-] Focus $Local detail$ @2026-04-20T10:00:00+00:00 :2026-04-20T11:00:00+00:00 !2 +deep #{id}\n"),
        )
        .unwrap();

        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();

        let rendered =
            std::fs::read_to_string(external_root.join(format!("inbox/{id}.ics"))).unwrap();
        assert!(rendered.contains("BEGIN:VTODO"));
        assert!(rendered.contains("STATUS:IN-PROCESS"));
        assert!(rendered.contains("PRIORITY:2"));
        assert!(rendered.contains("CATEGORIES:deep"));
        assert!(rendered.contains("DESCRIPTION:Local detail"));
    }

    #[test]
    fn arbitrary_uid_vtodo_adopts_native_action_with_full_profile() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let resource = external_root.join("inbox/transport-name.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(resource.parent().unwrap()).unwrap();
        std::fs::write(&actions, "").unwrap();
        std::fs::write(
            &resource,
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:peer-owned@example.com\r\nSUMMARY:Peer task\r\nDESCRIPTION:Peer detail\r\nDTSTART:20260420T100000Z\r\nDUE:20260420T110000Z\r\nSTATUS:IN-PROCESS\r\nPRIORITY:3\r\nCATEGORIES:home,phone\r\nX-VENDOR-KEEP:yes\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
        )
        .unwrap();

        sync_calendar(&project, Some(&external_root), None).unwrap();

        let adopted = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(adopted.len(), 1);
        let action = &adopted[0];
        assert_eq!(action.id.get_version_num(), 7);
        assert_eq!(action.name, "Peer task");
        assert_eq!(action.description.as_deref(), Some("Peer detail"));
        assert_eq!(action.state, ActionState::InProgress);
        assert_eq!(action.priority, Some(3));
        assert_eq!(
            action.contexts.as_deref(),
            Some(["home".to_string(), "phone".to_string()].as_slice())
        );
        assert!(resource.exists());
        assert!(
            std::fs::read_to_string(&resource)
                .unwrap()
                .contains("X-VENDOR-KEEP:yes")
        );
        let sidecar = parse_sidecar(
            &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            sidecar.actions[&action.id.to_string()]
                .plan
                .as_ref()
                .unwrap()
                .uid,
            "peer-owned@example.com"
        );

        sync_calendar(&project, Some(&external_root), None).unwrap();
        assert_eq!(
            parse_actions(&std::fs::read_to_string(actions).unwrap())
                .unwrap()
                .len(),
            1
        );
    }

    #[test]
    fn uuid_uid_migration_links_same_charter_action_without_duplicate() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let id = Uuid::parse_str("019baaec-00b6-7991-be34-94b6821261a3").unwrap();
        let resource = external_root.join("inbox/legacy.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(resource.parent().unwrap()).unwrap();
        std::fs::write(
            &actions,
            format!("[ ] Existing @2026-04-20T10:00:00+00:00 #{id}\n"),
        )
        .unwrap();
        std::fs::write(
            &resource,
            format!("BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{id}\r\nSUMMARY:Existing\r\nDTSTART:20260420T100000Z\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"),
        )
        .unwrap();

        sync_calendar(&project, Some(&external_root), None).unwrap();

        let parsed = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].id, id);
        let sidecar = parse_sidecar(
            &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            sidecar.actions[&id.to_string()].plan.as_ref().unwrap().uid,
            id.to_string()
        );
        assert!(resource.exists());
    }

    #[test]
    fn arbitrary_uid_migration_reuses_projection_store_evidence() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let resource = external_root.join("inbox/legacy.ics");
        let action_id = Uuid::parse_str("019baaec-00b6-7991-be34-94b6821261a4").unwrap();
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(resource.parent().unwrap()).unwrap();
        std::fs::write(
            &actions,
            format!("[ ] Existing @2026-04-20T10:00:00+00:00 #{action_id}\n"),
        )
        .unwrap();
        std::fs::write(
            &resource,
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:legacy@example.com\r\nSUMMARY:Existing\r\nDTSTART:20260420T100000Z\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
        )
        .unwrap();
        let mut store = PlansSyncStore::new(&external_root);
        store
            .stamp(
                action_id,
                clearhead_core::workspace::calendar::sync_store::UID_FIELD,
                &"legacy@example.com",
            )
            .unwrap();
        let store_path = project.join(".clearhead/sync/plans.json");
        std::fs::create_dir_all(store_path.parent().unwrap()).unwrap();
        std::fs::write(
            &store_path,
            clearhead_core::workspace::calendar::sync_store::encode_plans_sync_store(&store)
                .unwrap(),
        )
        .unwrap();

        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();

        let parsed = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].id, action_id);
        let sidecar = parse_sidecar(
            &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            sidecar.actions[&action_id.to_string()]
                .plan
                .as_ref()
                .unwrap()
                .uid,
            "legacy@example.com"
        );
    }

    #[test]
    fn configured_codec_migration_is_atomic_idempotent_and_seeds_vtodo_profile() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let action_id = Uuid::parse_str("019baaec-00b6-7991-be34-94b6821261a5").unwrap();
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(external_root.join("inbox")).unwrap();
        std::fs::write(
            &actions,
            format!("[-] Focus $Detail$ @2026-04-20T10:00:00+00:00 :2026-04-20T11:00:00+00:00 !3 +deep #{action_id}\n"),
        )
        .unwrap();
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();
        let resource = external_root.join(format!("inbox/{action_id}.ics"));
        let source = std::fs::read_to_string(&resource)
            .unwrap()
            .replace("END:VTODO", "X-VENDOR-KEEP:yes\r\nEND:VTODO");
        std::fs::write(&resource, source).unwrap();

        let preview = preview_calendar_sync_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();
        assert!(
            preview
                .report
                .warnings
                .iter()
                .any(|value| value.contains("migrate 1"))
        );
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();
        let event = std::fs::read_to_string(&resource).unwrap();
        assert!(event.contains("BEGIN:VEVENT"));
        assert!(!event.contains("BEGIN:VTODO"));
        assert!(event.contains("X-VENDOR-KEEP:yes"));
        assert_eq!(
            parse_actions(&std::fs::read_to_string(&actions).unwrap())
                .unwrap()
                .len(),
            1
        );

        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();
        let todo = std::fs::read_to_string(&resource).unwrap();
        assert!(todo.contains("BEGIN:VTODO"));
        assert!(todo.contains("STATUS:IN-PROCESS"));
        assert!(todo.contains("PRIORITY:3"));
        assert!(todo.contains("CATEGORIES:deep"));
        assert!(todo.contains("X-VENDOR-KEEP:yes"));
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();
        assert_eq!(std::fs::read_to_string(&resource).unwrap(), todo);
    }

    #[test]
    fn recurring_vevent_moves_roundtrip_through_the_materialized_action() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let resource = external_root.join("inbox/series.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(resource.parent().unwrap()).unwrap();
        std::fs::write(&actions, "").unwrap();
        let now = Local::now()
            .with_timezone(&chrono::Utc)
            .with_nanosecond(0)
            .unwrap()
            .with_second(0)
            .unwrap();
        let anchor = now + chrono::Duration::hours(1);
        let key = anchor.format("%Y%m%dT%H%M%SZ").to_string();
        std::fs::write(
            &resource,
            format!("BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:series@example.com\r\nSUMMARY:Series\r\nDTSTART:{key}\r\nRRULE:FREQ=DAILY\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"),
        )
        .unwrap();
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();
        let initial = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(initial.len(), 1);
        let occurrence_id = initial[0].id;

        let calendar_move = anchor + chrono::Duration::hours(2);
        let calendar_move_text = calendar_move.format("%Y%m%dT%H%M%SZ");
        let source = std::fs::read_to_string(&resource)
            .unwrap()
            .replace(
                "END:VCALENDAR",
                &format!("BEGIN:VEVENT\r\nUID:series@example.com\r\nRECURRENCE-ID:{key}\r\nSUMMARY:Series\r\nDTSTART:{calendar_move_text}\r\nEND:VEVENT\r\nEND:VCALENDAR"),
            );
        std::fs::write(&resource, source).unwrap();
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();
        let pulled = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(pulled[0].id, occurrence_id);
        assert_eq!(
            pulled[0].scheduled_at.unwrap().with_timezone(&chrono::Utc),
            calendar_move
        );

        let action_move = calendar_move + chrono::Duration::hours(1);
        std::fs::write(
            &actions,
            format!(
                "[ ] Series @{} #{occurrence_id}\n",
                action_move.to_rfc3339()
            ),
        )
        .unwrap();
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();
        let rendered = std::fs::read_to_string(&resource).unwrap();
        assert_eq!(rendered.matches(&format!("RECURRENCE-ID:{key}")).count(), 1);
        assert!(rendered.contains(&format!("DTSTART:{}", action_move.format("%Y%m%dT%H%M%SZ"))));
    }

    #[test]
    fn recurring_vtodo_terminal_pull_snapshots_lineage_and_advances_once() {
        let temp = tempfile::tempdir().unwrap();
        let project = temp.path().join("project");
        let external_root = temp.path().join("vdir");
        let actions = project.join(".clearhead/charters/inbox.actions");
        let resource = external_root.join("inbox/series.ics");
        std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
        std::fs::create_dir_all(resource.parent().unwrap()).unwrap();
        std::fs::write(&actions, "").unwrap();
        let anchor = Local::now()
            .with_timezone(&chrono::Utc)
            .with_nanosecond(0)
            .unwrap()
            .with_second(0)
            .unwrap()
            + chrono::Duration::hours(1);
        let key = anchor.format("%Y%m%dT%H%M%SZ").to_string();
        std::fs::write(
            &resource,
            format!("BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:series-task@example.com\r\nSUMMARY:Series task\r\nDTSTART:{key}\r\nRRULE:FREQ=DAILY\r\nSTATUS:NEEDS-ACTION\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"),
        )
        .unwrap();
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();
        let first = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        let completed_id = first[0].id;
        let completed_at = (anchor + chrono::Duration::minutes(5))
            .format("%Y%m%dT%H%M%SZ")
            .to_string();
        let source = std::fs::read_to_string(&resource)
            .unwrap()
            .replace(
                "END:VCALENDAR",
                &format!("BEGIN:VTODO\r\nUID:series-task@example.com\r\nRECURRENCE-ID:{key}\r\nSUMMARY:Series task\r\nDTSTART:{key}\r\nSTATUS:COMPLETED\r\nCOMPLETED:{completed_at}\r\nEND:VTODO\r\nEND:VCALENDAR"),
            );
        std::fs::write(&resource, source).unwrap();

        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();

        let parsed = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
        assert_eq!(
            parsed
                .iter()
                .filter(|action| action.id == completed_id)
                .count(),
            1
        );
        assert_eq!(
            parsed
                .iter()
                .find(|action| action.id == completed_id)
                .unwrap()
                .state,
            ActionState::Completed
        );
        assert_eq!(
            parsed
                .iter()
                .filter(|action| action.state == ActionState::NotStarted)
                .count(),
            1,
            "exactly one next token is stamped"
        );
        let sidecar = parse_sidecar(
            &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            sidecar.actions[&completed_id.to_string()]
                .occurrence
                .as_ref()
                .unwrap()
                .occurrence_key,
            key
        );
        let store = read_plans_sync_store(&project, &external_root).unwrap();
        assert!(store.occurrence_link(completed_id).is_none());
        assert_eq!(store.occurrence_links().len(), 1);

        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VTodo,
        )
        .unwrap();
        assert_eq!(
            parse_actions(&std::fs::read_to_string(&actions).unwrap())
                .unwrap()
                .len(),
            2,
            "repeat sync does not advance twice"
        );
    }

    #[test]
    fn calendar_deletion_unschedules_unlinks_and_can_be_rescheduled_in_both_codecs() {
        for (index, component_kind) in [PlanComponentKind::VEvent, PlanComponentKind::VTodo]
            .into_iter()
            .enumerate()
        {
            let temp = tempfile::tempdir().unwrap();
            let project = temp.path().join("project");
            let external_root = temp.path().join("vdir");
            let actions = project.join(".clearhead/charters/inbox.actions");
            std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
            std::fs::create_dir_all(external_root.join("inbox")).unwrap();
            let action_id =
                Uuid::parse_str(&format!("019baaec-00b6-7991-be34-94b6821261c{index}")).unwrap();
            std::fs::write(
                &actions,
                format!(
                    "[-] Keep me $detail$ @2026-04-20T10:00:00+00:00 :2026-04-20T11:00:00+00:00 !4 +local #{action_id}\n"
                ),
            )
            .unwrap();

            sync_calendar_with_component(&project, Some(&external_root), None, component_kind)
                .unwrap();
            let resource = external_root.join(format!("inbox/{action_id}.ics"));
            assert!(resource.exists());
            std::fs::remove_file(&resource).unwrap();

            sync_calendar_with_component(&project, Some(&external_root), None, component_kind)
                .unwrap();

            let parsed = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
            assert_eq!(parsed.len(), 1);
            let action = &parsed[0];
            assert_eq!(action.id, action_id);
            assert_eq!(action.state, ActionState::InProgress);
            assert_eq!(action.name, "Keep me");
            assert_eq!(action.description.as_deref(), Some("detail"));
            assert_eq!(action.priority, Some(4));
            assert_eq!(
                action.contexts.as_deref(),
                Some(["local".into()].as_slice())
            );
            assert!(action.scheduled_at.is_none());
            assert!(action.due_date.is_none());
            assert!(!resource.exists(), "deleted Plan must not be recreated");
            let sidecar = parse_sidecar(
                &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
            )
            .unwrap();
            assert!(!sidecar.actions.contains_key(&action_id.to_string()));
            let store = read_plans_sync_store(&project, &external_root).unwrap();
            assert!(!store.actions.contains_key(&action_id));

            // Repeating deletion reconciliation is a no-op.
            sync_calendar_with_component(&project, Some(&external_root), None, component_kind)
                .unwrap();
            assert!(!resource.exists());

            // Scheduling the preserved Action again creates a fresh canonical Plan.
            std::fs::write(
                &actions,
                format!(
                    "[-] Keep me $detail$ @2026-04-22T10:00:00+00:00 :2026-04-22T11:00:00+00:00 !4 +local #{action_id}\n"
                ),
            )
            .unwrap();
            sync_calendar_with_component(&project, Some(&external_root), None, component_kind)
                .unwrap();
            let rendered = std::fs::read_to_string(&resource).unwrap();
            match component_kind {
                PlanComponentKind::VEvent => assert!(rendered.contains("BEGIN:VEVENT")),
                PlanComponentKind::VTodo => assert!(rendered.contains("BEGIN:VTODO")),
            }
            let sidecar = parse_sidecar(
                &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
            )
            .unwrap();
            assert_eq!(
                sidecar.actions[&action_id.to_string()]
                    .plan
                    .as_ref()
                    .unwrap()
                    .uid,
                action_id.to_string()
            );
        }
    }

    #[test]
    fn action_schedule_clearing_removes_and_unlinks_both_codecs() {
        for (index, component_kind) in [PlanComponentKind::VEvent, PlanComponentKind::VTodo]
            .into_iter()
            .enumerate()
        {
            let temp = tempfile::tempdir().unwrap();
            let project = temp.path().join("project");
            let external_root = temp.path().join("vdir");
            let actions = project.join(".clearhead/charters/inbox.actions");
            std::fs::create_dir_all(actions.parent().unwrap()).unwrap();
            std::fs::create_dir_all(external_root.join("inbox")).unwrap();
            let action_id =
                Uuid::parse_str(&format!("019baaec-00b6-7991-be34-94b6821261d{index}")).unwrap();
            std::fs::write(
                &actions,
                format!(
                    "[ ] Clear me @2026-04-20T10:00:00+00:00 :2026-04-20T11:00:00+00:00 !2 +local #{action_id}\n"
                ),
            )
            .unwrap();
            sync_calendar_with_component(&project, Some(&external_root), None, component_kind)
                .unwrap();
            let resource = external_root.join(format!("inbox/{action_id}.ics"));
            assert!(resource.exists());

            std::fs::write(&actions, format!("[ ] Clear me !2 +local #{action_id}\n")).unwrap();
            sync_calendar_with_component(&project, Some(&external_root), None, component_kind)
                .unwrap();

            assert!(!resource.exists());
            let parsed = parse_actions(&std::fs::read_to_string(&actions).unwrap()).unwrap();
            assert_eq!(parsed[0].id, action_id);
            assert!(parsed[0].scheduled_at.is_none());
            assert!(parsed[0].due_date.is_none());
            assert_eq!(parsed[0].priority, Some(2));
            let sidecar = parse_sidecar(
                &std::fs::read_to_string(actions.parent().unwrap().join(".inbox.json")).unwrap(),
            )
            .unwrap();
            assert!(!sidecar.actions.contains_key(&action_id.to_string()));
            assert!(
                !read_plans_sync_store(&project, &external_root)
                    .unwrap()
                    .actions
                    .contains_key(&action_id)
            );

            sync_calendar_with_component(&project, Some(&external_root), None, component_kind)
                .unwrap();
            assert!(!resource.exists());
        }
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
        sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();

        std::fs::write(
            &calendar,
            initial
                .replace("SUMMARY:Calendar display", "SUMMARY:Peer renamed")
                .replace("20260420T100000Z", "20260421T120000Z")
                .replace("20260420T110000Z", "20260421T130000Z"),
        )
        .unwrap();
        let result = sync_calendar_with_component(
            &project,
            Some(&external_root),
            None,
            PlanComponentKind::VEvent,
        )
        .unwrap();

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
