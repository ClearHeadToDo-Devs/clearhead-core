//! Native plans-vdir discovery, immutable reads, and sync-store persistence.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use uuid::Uuid;

use clearhead_core::workspace::calendar::ics::{
    ICSPlan, VTodoAction, parse_ics, parse_vtodo_actions_content,
};
use clearhead_core::workspace::calendar::plans::{
    infer_plan_charter_name_for_workspace, infer_plan_parent_for_workspace,
};
use clearhead_core::workspace::calendar::sync_store::{PlansSyncStore, decode_plans_sync_store};
use clearhead_core::workspace::resource::{
    MountId, ReadPlan, ResourceLocation, ResourceRevision, WorkspaceMounts, WorkspacePath,
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

/// Discover and read all visible `.ics` resources from the effective plans mount.
pub fn read_calendar_resources(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<Vec<CalendarResource>, WorkspaceError> {
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
    Ok(resources)
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
}
