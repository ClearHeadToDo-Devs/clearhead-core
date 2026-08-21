//! Native resolution, inventory, and byte reads for Core workspace mounts.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use clearhead_core::workspace::WorkspaceError;
use clearhead_core::workspace::resource::{
    MountInventory, MountReadEvidence, ReadPlan, ResourceReadFailure, ResourceRevision,
    ResourceSnapshot, WorkspaceInventory, WorkspaceMounts, WorkspacePath, WorkspaceScope,
    WorkspaceSnapshot,
};

/// Physical roots resolved by the native adapter.
///
/// The workspace mount is rooted at the data directory (`.clearhead/` for a
/// project workspace). An external plans mount, when configured, remains a
/// second physical and logical namespace.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeWorkspaceMounts {
    pub workspace: PathBuf,
    pub external_plans: Option<PathBuf>,
    pub scope: WorkspaceScope,
}

impl NativeWorkspaceMounts {
    pub fn resolve(workspace_root: &Path, external_plans: Option<&Path>) -> Self {
        let project_data = workspace_root.join(".clearhead");
        let (workspace, scope) = if project_data.is_dir() {
            let root_charter_name = workspace_root
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("workspace")
                .to_owned();
            (project_data, WorkspaceScope::Project { root_charter_name })
        } else {
            (workspace_root.to_path_buf(), WorkspaceScope::User)
        };
        Self {
            workspace,
            external_plans: external_plans.map(Path::to_path_buf),
            scope,
        }
    }

    pub fn inventory(&self) -> Result<WorkspaceMounts<MountInventory>, WorkspaceError> {
        Ok(WorkspaceMounts {
            workspace: inventory_mount(&self.workspace)?,
            external_plans: self
                .external_plans
                .as_deref()
                .map(inventory_mount)
                .transpose()?,
        })
    }

    pub fn read(
        &self,
        plans: &WorkspaceMounts<ReadPlan>,
    ) -> Result<WorkspaceMounts<MountReadEvidence>, WorkspaceError> {
        Ok(WorkspaceMounts {
            workspace: read_mount(&self.workspace, &plans.workspace)?,
            external_plans: match (&self.external_plans, &plans.external_plans) {
                (Some(root), Some(plan)) => Some(read_mount(root, plan)?),
                (None, None) => None,
                _ => {
                    return Err(WorkspaceError::Actions(
                        "external plans read plan does not match resolved mounts".into(),
                    ));
                }
            },
        })
    }
}

fn inventory_mount(root: &Path) -> Result<MountInventory, WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.to_path_buf()));
    }
    let mut files = Vec::new();
    let mut collections = BTreeSet::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(directory) = stack.pop() {
        let mut entries = std::fs::read_dir(&directory)?.collect::<Result<Vec<_>, _>>()?;
        entries.sort_by_key(std::fs::DirEntry::path);
        for entry in entries {
            let path = entry.path();
            let file_type = entry.file_type()?;
            let relative = path
                .strip_prefix(root)
                .map_err(|_| WorkspaceError::InvalidPath(path.clone()))?;
            let logical = logical_path(relative)?;
            if file_type.is_dir() {
                collections.insert(logical);
                stack.push(path);
            } else if file_type.is_file() {
                let bytes = std::fs::read(&path)?;
                files.push((logical, revision(&bytes)));
            }
        }
    }
    Ok(MountInventory {
        files: WorkspaceInventory::new(files),
        collections,
    })
}

fn read_mount(root: &Path, plan: &ReadPlan) -> Result<MountReadEvidence, WorkspaceError> {
    let mut snapshots = Vec::new();
    let mut failures = Vec::new();
    for logical in plan.paths() {
        let path = root.join(logical.as_str());
        match std::fs::read(&path) {
            Ok(bytes) => snapshots.push(ResourceSnapshot::new(
                logical.clone(),
                bytes.clone(),
                revision(&bytes),
            )),
            Err(error) => failures.push(ResourceReadFailure {
                path: logical.clone(),
                message: error.to_string(),
            }),
        }
    }
    Ok(MountReadEvidence {
        snapshot: WorkspaceSnapshot::new(snapshots)
            .map_err(|error| WorkspaceError::Actions(error.to_string()))?,
        failures,
    })
}

fn logical_path(path: &Path) -> Result<WorkspacePath, WorkspaceError> {
    let logical = path
        .components()
        .map(|part| part.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| WorkspaceError::InvalidPath(path.to_path_buf()))?
        .join("/");
    WorkspacePath::new(logical).map_err(|_| WorkspaceError::InvalidPath(path.to_path_buf()))
}

fn revision(bytes: &[u8]) -> ResourceRevision {
    ResourceRevision::new(blake3::hash(bytes).to_hex().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use clearhead_core::workspace::plan_workspace_read;

    #[test]
    fn resolves_project_and_external_plans_as_distinct_mounts() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(root.path().join("project/.clearhead/charters")).unwrap();
        let external = root.path().join("vdir");
        std::fs::create_dir_all(external.join("next")).unwrap();

        let mounts =
            NativeWorkspaceMounts::resolve(&root.path().join("project"), Some(external.as_path()));
        assert_eq!(mounts.workspace, root.path().join("project/.clearhead"));
        assert_eq!(mounts.external_plans, Some(external));
        assert_eq!(
            mounts.scope,
            WorkspaceScope::Project {
                root_charter_name: "project".into()
            }
        );
    }

    #[test]
    fn inventories_empty_collections_and_keeps_equal_paths_separate() {
        let root = tempfile::tempdir().unwrap();
        let workspace = root.path().join("workspace");
        let external = root.path().join("vdir");
        std::fs::create_dir_all(workspace.join("plans/next")).unwrap();
        std::fs::create_dir_all(external.join("next")).unwrap();
        std::fs::write(workspace.join("plans/next/same.ics"), "workspace").unwrap();
        std::fs::write(external.join("next/same.ics"), "external").unwrap();

        let mounts = NativeWorkspaceMounts::resolve(&workspace, Some(&external));
        let inventory = mounts.inventory().unwrap();
        assert!(
            inventory
                .workspace
                .collections
                .contains(&WorkspacePath::new("plans/next").unwrap())
        );
        assert!(
            inventory
                .external_plans
                .as_ref()
                .unwrap()
                .collections
                .contains(&WorkspacePath::new("next").unwrap())
        );

        let plans = plan_workspace_read(&inventory);
        let reads = mounts.read(&plans).unwrap();
        assert_eq!(
            reads
                .workspace
                .snapshot
                .resource(&WorkspacePath::new("plans/next/same.ics").unwrap())
                .unwrap()
                .bytes(),
            b"workspace"
        );
        assert_eq!(
            reads
                .external_plans
                .unwrap()
                .snapshot
                .resource(&WorkspacePath::new("next/same.ics").unwrap())
                .unwrap()
                .bytes(),
            b"external"
        );
    }
}
