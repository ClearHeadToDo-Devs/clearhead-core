//! Native resolution, inventory, and byte reads for Core workspace mounts.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use clearhead_core::domain::DomainModel;
use clearhead_core::workspace::resource::{
    MountId, MountInventory, MountReadEvidence, ReadPlan, ResourceLocation, ResourceReadFailure,
    ResourceRevision, ResourceSnapshot, WorkspaceInventory, WorkspaceMounts, WorkspacePath,
    WorkspaceSnapshot,
};
use clearhead_core::workspace::{
    MarkdownCharter, Workspace, WorkspaceAssemblyInput, WorkspaceError, WorkspaceRead,
    assemble_workspace, plan_workspace_read,
};

use crate::calendar::read_plans_sync_store;

/// Physical roots resolved by the native adapter.
///
/// The workspace mount is rooted at the data directory (`.clearhead/` for a
/// project workspace). An external plans mount, when configured, remains a
/// second physical and logical namespace.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeWorkspaceMounts {
    pub workspace: PathBuf,
    pub external_plans: Option<PathBuf>,
    /// Persisted name of the workspace's single root charter.
    pub root_charter: String,
}

impl NativeWorkspaceMounts {
    pub fn resolve(workspace_root: &Path, external_plans: Option<&Path>) -> Self {
        let project_data = workspace_root.join(".clearhead");
        let workspace = if project_data.is_dir() {
            project_data
        } else {
            workspace_root.to_path_buf()
        };
        let root_charter = persisted_root_charter_name(&workspace);
        Self {
            workspace,
            external_plans: external_plans.map(Path::to_path_buf),
            root_charter,
        }
    }

    /// Physical root containing workspace-managed resources.
    pub fn data_root(&self) -> &Path {
        &self.workspace
    }

    /// Physical root containing the charter tree.
    pub fn charter_root(&self) -> PathBuf {
        self.workspace.join("charters")
    }

    /// Effective physical plans root, honoring an external plans mount.
    pub fn plans_root(&self) -> PathBuf {
        self.external_plans
            .clone()
            .unwrap_or_else(|| self.workspace.join("plans"))
    }

    /// Name of the workspace's single root charter.
    pub fn root_charter(&self) -> &str {
        &self.root_charter
    }

    /// Resolve a logical resource location to its native physical path.
    pub fn physical_path(&self, location: &ResourceLocation) -> Result<PathBuf, WorkspaceError> {
        let root = match location.mount {
            MountId::Workspace => &self.workspace,
            MountId::ExternalPlans => self.external_plans.as_ref().ok_or_else(|| {
                WorkspaceError::Actions(
                    "an external-plans resource was requested without an external plans mount"
                        .into(),
                )
            })?,
        };
        Ok(root.join(location.path.as_str()))
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
        inventory: &WorkspaceMounts<MountInventory>,
    ) -> Result<WorkspaceMounts<MountReadEvidence>, WorkspaceError> {
        Ok(WorkspaceMounts {
            workspace: read_mount(
                &self.workspace,
                &plans.workspace,
                &inventory.workspace.files,
            )?,
            external_plans: match (
                &self.external_plans,
                &plans.external_plans,
                &inventory.external_plans,
            ) {
                (Some(root), Some(plan), Some(inventory)) => {
                    Some(read_mount(root, plan, &inventory.files)?)
                }
                (None, None, None) => None,
                _ => {
                    return Err(WorkspaceError::Actions(
                        "external plans read plan does not match resolved mounts".into(),
                    ));
                }
            },
        })
    }
}

/// Detect and return the physical data root for a native workspace.
pub fn workspace_data_root(root: &Path) -> PathBuf {
    NativeWorkspaceMounts::resolve(root, None).workspace
}

/// Detect and return the physical charter root for a native workspace.
pub fn charter_root(root: &Path) -> PathBuf {
    NativeWorkspaceMounts::resolve(root, None).charter_root()
}

/// Detect and return the default physical plans root for a native workspace.
pub fn plans_root(root: &Path) -> PathBuf {
    NativeWorkspaceMounts::resolve(root, None).plans_root()
}

/// The persisted root charter name for a native workspace.
pub fn root_charter_name(root: &Path) -> String {
    NativeWorkspaceMounts::resolve(root, None).root_charter
}

/// Root charter name when neither a README alias nor a persisted workspace name
/// exists; `doctor` reports that gap.
pub const UNNAMED_ROOT_CHARTER: &str = "workspace";

/// The root charter's persisted name: its README `alias`, else the manifest's
/// `workspace_name`. Never derived from the directory or the environment.
///
/// Reads `workspace.json` directly: `read_workspace_manifest` resolves mounts
/// and would re-enter this function.
fn persisted_root_charter_name(data_root: &Path) -> String {
    use clearhead_core::workspace::init::{MANIFEST_PATH, ROOT_README_PATH};
    let read = |path: &str| std::fs::read_to_string(data_root.join(path)).ok();
    read(ROOT_README_PATH)
        .and_then(|source| clearhead_core::workspace::parse_charter(&source).ok())
        .and_then(|charter| charter.alias)
        .or_else(|| {
            read(MANIFEST_PATH)
                .and_then(|source| {
                    clearhead_core::workspace::parse_workspace_manifest(&source).ok()
                })
                .and_then(|manifest| manifest.workspace_name)
        })
        .unwrap_or_else(|| UNNAMED_ROOT_CHARTER.to_owned())
}

/// Relaxed native read: inventory and read bytes without replaying pending intent.
pub fn read_workspace(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<WorkspaceRead, WorkspaceError> {
    assemble_native(workspace_root, external_plans)
}

/// Native load: inventory, read, and surface findings as warnings.
pub fn load_workspace(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<Vec<MarkdownCharter>, WorkspaceError> {
    let read = assemble_native(workspace_root, external_plans)?;
    for finding in &read.findings {
        eprintln!("warning: [{}] {}", finding.path.display(), finding.message);
    }
    Ok(read.charters)
}

/// Discover active `.actions` resources and map them to native paths.
pub fn list_action_files(workspace_root: &Path) -> Result<Vec<PathBuf>, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, None);
    let inventory = mounts.inventory()?;
    let mut paths = inventory
        .workspace
        .files
        .paths()
        .filter(|path| path.as_str().starts_with("charters/"))
        .filter(|path| path.as_str().ends_with(".actions"))
        .filter(|path| !path.as_str().ends_with(".completed.actions"))
        .filter(|path| !path.as_str().ends_with(".upcoming.actions"))
        .map(|path| mounts.workspace.join(path.as_str()))
        .collect::<Vec<_>>();
    paths.sort();
    Ok(paths)
}

pub fn load_domain_model(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<DomainModel, WorkspaceError> {
    let charters = load_workspace(workspace_root, external_plans)?;
    Ok(load_workspace_envelope(workspace_root, charters).into())
}

pub fn load_workspace_envelope(workspace_root: &Path, charters: Vec<MarkdownCharter>) -> Workspace {
    let manifest = crate::read_workspace_manifest(workspace_root);
    Workspace::from_parts(
        workspace_root.to_path_buf(),
        manifest.workspace_id,
        manifest.workspace_name,
        charters,
    )
}

pub fn load_workspace_model(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<Workspace, WorkspaceError> {
    let charters = load_workspace(workspace_root, external_plans)?;
    Ok(load_workspace_envelope(workspace_root, charters))
}

fn assemble_native(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<WorkspaceRead, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let inventory = mounts.inventory()?;
    let plans = plan_workspace_read(&inventory);
    let reads = mounts.read(&plans, &inventory)?;
    let effective_plans_root = mounts
        .external_plans
        .as_deref()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| mounts.workspace.join("plans"));
    let occurrence_links = read_plans_sync_store(workspace_root, &effective_plans_root)
        .map(|store| store.occurrence_links().clone())
        .unwrap_or_default();
    assemble_workspace(&WorkspaceAssemblyInput {
        root_charter: mounts.root_charter,
        inventory,
        reads,
        occurrence_links,
    })
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
                let revision = entry
                    .metadata()
                    .map(|metadata| metadata_revision(&metadata))
                    .unwrap_or_else(|_| ResourceRevision::new("metadata-unavailable"));
                files.push((logical, revision));
            }
        }
    }
    Ok(MountInventory {
        files: WorkspaceInventory::new(files),
        collections,
    })
}

fn read_mount(
    root: &Path,
    plan: &ReadPlan,
    inventory: &WorkspaceInventory,
) -> Result<MountReadEvidence, WorkspaceError> {
    let mut snapshots = Vec::new();
    let mut failures = Vec::new();
    for logical in plan.paths() {
        let path = root.join(logical.as_str());
        let before = match std::fs::metadata(&path) {
            Ok(metadata) => metadata_revision(&metadata),
            Err(error) => {
                failures.push(ResourceReadFailure {
                    path: logical.clone(),
                    message: error.to_string(),
                });
                continue;
            }
        };
        if inventory.revision(logical) != Some(&before) {
            failures.push(ResourceReadFailure {
                path: logical.clone(),
                message: "resource changed after inventory".into(),
            });
            continue;
        }
        match std::fs::read(&path) {
            Ok(bytes) => {
                let after = std::fs::metadata(&path)
                    .map(|metadata| metadata_revision(&metadata))
                    .ok();
                if after.as_ref() != Some(&before) {
                    failures.push(ResourceReadFailure {
                        path: logical.clone(),
                        message: "resource changed while it was read".into(),
                    });
                    continue;
                }
                let revision = content_revision(&bytes);
                snapshots.push(ResourceSnapshot::new(logical.clone(), bytes, revision));
            }
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

pub(crate) fn content_revision(bytes: &[u8]) -> ResourceRevision {
    ResourceRevision::new(blake3::hash(bytes).to_hex().to_string())
}

fn metadata_revision(metadata: &std::fs::Metadata) -> ResourceRevision {
    let modified = metadata
        .modified()
        .ok()
        .and_then(|time| time.duration_since(std::time::UNIX_EPOCH).ok());
    let (seconds, nanos) = modified
        .map(|duration| (duration.as_secs(), duration.subsec_nanos()))
        .unwrap_or_default();
    ResourceRevision::new(format!("{}:{seconds}:{nanos}", metadata.len()))
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
        assert_eq!(mounts.external_plans, Some(external.clone()));
        assert_eq!(mounts.data_root(), root.path().join("project/.clearhead"));
        assert_eq!(
            mounts.charter_root(),
            root.path().join("project/.clearhead/charters")
        );
        assert_eq!(mounts.plans_root(), external);
        assert_eq!(mounts.root_charter(), UNNAMED_ROOT_CHARTER);
    }

    #[test]
    fn native_layout_helpers_distinguish_project_and_user_workspaces() {
        let root = tempfile::tempdir().unwrap();
        let project = root.path().join("project");
        std::fs::create_dir_all(project.join(".clearhead")).unwrap();
        let user = root.path().join("user");
        std::fs::create_dir_all(&user).unwrap();

        assert_eq!(workspace_data_root(&project), project.join(".clearhead"));
        assert_eq!(charter_root(&project), project.join(".clearhead/charters"));
        assert_eq!(plans_root(&project), project.join(".clearhead/plans"));

        assert_eq!(workspace_data_root(&user), user);
        assert_eq!(charter_root(&user), user.join("charters"));
        assert_eq!(plans_root(&user), user.join("plans"));
    }

    #[test]
    fn root_charter_name_prefers_readme_alias_then_persisted_workspace_name() {
        let root = tempfile::tempdir().unwrap();
        let project = root.path().join("checkout-dir");
        std::fs::create_dir_all(project.join(".clearhead/charters")).unwrap();
        assert_eq!(root_charter_name(&project), UNNAMED_ROOT_CHARTER);

        std::fs::write(
            project.join(".clearhead/workspace.json"),
            r#"{"workspace_name": "platform"}"#,
        )
        .unwrap();
        assert_eq!(root_charter_name(&project), "platform");

        std::fs::write(
            project.join(".clearhead/charters/README.md"),
            "---\nalias: renamed\n---\n# Platform\n",
        )
        .unwrap();
        assert_eq!(root_charter_name(&project), "renamed");
    }

    #[test]
    fn native_loader_assembles_domain_model() {
        let action_id = "019fa000-0000-7000-8000-000000000001";
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(root.path().join("charters")).unwrap();
        std::fs::write(
            root.path().join("charters/work.actions"),
            format!("[ ] Shared semantics #{action_id}"),
        )
        .unwrap();
        std::fs::write(
            root.path().join("charters/work.md"),
            "---\nalias: work\n---\n# Work\n",
        )
        .unwrap();

        let mounted = load_domain_model(root.path(), None).unwrap();
        assert_eq!(mounted.charters.len(), 2, "work plus the implicit root");
        let work = mounted
            .charters
            .iter()
            .find(|charter| charter.alias.as_deref() == Some("work"))
            .unwrap();
        assert_eq!(work.actions.len(), 1);
        assert_eq!(work.actions[0].id.to_string(), action_id);
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
        let reads = mounts.read(&plans, &inventory).unwrap();
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
