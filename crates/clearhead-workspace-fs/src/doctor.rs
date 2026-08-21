//! Native doctor observation and typed repair execution.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use chrono::Local;
use clearhead_core::workspace::durability::{WorkspaceLock, recover_pending};
use clearhead_core::workspace::resource::{
    MountId, ResourceLocation, ResourceRevision, WorkspacePath,
};
use clearhead_core::workspace::{
    Diagnosis, DoctorCollectionEvidence, DoctorDocument, DoctorEvidence, DoctorRepair,
    DoctorSidecarEvidence, DurabilityResidue, DurabilityResidueKind, WorkspaceError, WorkspaceRead,
    diagnose,
};

use crate::mounts::NativeWorkspaceMounts;

/// Observe and diagnose a native workspace without replaying pending intent.
pub fn diagnose_workspace(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<Diagnosis, WorkspaceError> {
    let read = crate::read_workspace(workspace_root, external_plans)?;
    diagnose_workspace_read(workspace_root, external_plans, &read)
}

/// Diagnose an already assembled workspace while observing doctor's additional
/// native evidence exactly once.
pub fn diagnose_workspace_read(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    read: &WorkspaceRead,
) -> Result<Diagnosis, WorkspaceError> {
    let evidence = observe_doctor(workspace_root, external_plans)?;
    Ok(diagnose(read, &evidence))
}

/// Gather native facts that normal workspace assembly intentionally excludes.
pub fn observe_doctor(
    workspace_root: &Path,
    external_plans: Option<&Path>,
) -> Result<DoctorEvidence, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    if !mounts.workspace.is_dir() {
        return Err(WorkspaceError::InvalidPath(mounts.workspace));
    }
    let charter_root = mounts.workspace.join("charters");
    let archive_root = mounts.workspace.join("archive");
    let plans_root = mounts
        .external_plans
        .clone()
        .unwrap_or_else(|| mounts.workspace.join("plans"));
    let plans_mount = if mounts.external_plans.is_some() {
        MountId::ExternalPlans
    } else {
        MountId::Workspace
    };

    let completed_actions = walk_visible_files(&charter_root)
        .into_iter()
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.ends_with(".completed.actions"))
        })
        .map(|path| observe_document(&charter_root, path))
        .collect::<Result<Vec<_>, _>>()?;
    let archived_actions = walk_visible_files(&archive_root)
        .into_iter()
        .filter(|path| path.extension().and_then(|extension| extension.to_str()) == Some("actions"))
        .map(|path| observe_document(&archive_root, path))
        .collect::<Result<Vec<_>, _>>()?;
    let sidecars = walk_visible_files(&charter_root)
        .into_iter()
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with('.') && name.ends_with(".json"))
        })
        .map(|path| observe_sidecar(&charter_root, path))
        .collect::<Result<Vec<_>, _>>()?;
    let plan_collections = observe_plan_collections(&plans_root, plans_mount)?;

    let mut durability_residue = Vec::new();
    if charter_root.join(".pending").is_file() {
        durability_residue.push(DurabilityResidue {
            location: ResourceLocation::new(MountId::Workspace, logical_path(".pending")?),
            kind: DurabilityResidueKind::PendingJournal,
        });
    }
    collect_temps(&charter_root, MountId::Workspace, &mut durability_residue)?;
    collect_temps(&plans_root, plans_mount, &mut durability_residue)?;

    Ok(DoctorEvidence {
        manifest: crate::read_workspace_manifest(workspace_root),
        completed_actions,
        archived_actions,
        sidecars,
        plan_collections,
        durability_residue,
        observed_at: Local::now(),
    })
}

/// Execute repairs selected by Core while holding the native workspace lock.
pub fn apply_doctor_repairs(
    workspace_root: &Path,
    external_plans: Option<&Path>,
    repairs: &[DoctorRepair],
) -> Result<(), WorkspaceError> {
    if repairs.is_empty() {
        return Ok(());
    }
    let mounts = NativeWorkspaceMounts::resolve(workspace_root, external_plans);
    let charter_root = mounts.workspace.join("charters");
    let _lock = WorkspaceLock::try_acquire(&mounts.workspace)?
        .ok_or_else(|| WorkspaceError::Actions("workspace is locked by another writer".into()))?;
    recover_pending(&charter_root)?;

    let current = diagnose_workspace(workspace_root, external_plans)?;
    if current.repairs != repairs {
        return Err(WorkspaceError::Actions(
            "doctor repair evidence is stale; rerun doctor and review the current repair plan"
                .into(),
        ));
    }

    let removed_sidecars = repairs
        .iter()
        .filter_map(|repair| match repair {
            DoctorRepair::RemoveSidecar { path, expected } => {
                Some((path.clone(), expected.clone()))
            }
            _ => None,
        })
        .collect::<BTreeMap<_, _>>();
    let mut pruned_entries: BTreeMap<WorkspacePath, (ResourceRevision, BTreeSet<String>)> =
        BTreeMap::new();
    for repair in repairs {
        if let DoctorRepair::PruneSidecarEntry { path, id, expected } = repair
            && !removed_sidecars.contains_key(path)
        {
            let entry = pruned_entries
                .entry(path.clone())
                .or_insert_with(|| (expected.clone(), BTreeSet::new()));
            if entry.0 != *expected {
                return Err(WorkspaceError::Actions(format!(
                    "doctor repair has inconsistent revisions for sidecar '{}'",
                    path
                )));
            }
            entry.1.insert(id.clone());
        }
    }
    for (relative, (expected, ids)) in pruned_entries {
        let path = charter_root.join(relative.as_str());
        validate_file_revision(&path, &expected)?;
        let mut metadata = crate::sidecar::read_sidecar(&path)?;
        for id in ids {
            metadata.actions.remove(&id);
        }
        crate::sidecar::write_sidecar(&path, &metadata)?;
    }
    for (relative, expected) in removed_sidecars {
        let path = charter_root.join(relative.as_str());
        validate_file_revision(&path, &expected)?;
        remove_file_if_present(&path)?;
    }
    for repair in repairs {
        let DoctorRepair::RemovePlansCollection { location, expected } = repair else {
            continue;
        };
        if location.path.as_str().contains('/') {
            return Err(WorkspaceError::Actions(format!(
                "doctor refused unsafe calendar collection path '{}'",
                location.path
            )));
        }
        let root = match location.mount {
            MountId::Workspace => mounts.workspace.join("plans"),
            MountId::ExternalPlans => mounts.external_plans.clone().ok_or_else(|| {
                WorkspaceError::Actions(
                    "doctor repair names an external plans mount that is not configured".into(),
                )
            })?,
        };
        let path = root.join(location.path.as_str());
        if collection_revision(&path)? != *expected {
            return Err(WorkspaceError::Actions(format!(
                "doctor repair evidence for calendar collection '{}' is stale",
                location.path
            )));
        }
        remove_dir_if_present(&path)?;
    }
    Ok(())
}

fn observe_document(root: &Path, path: PathBuf) -> Result<DoctorDocument, WorkspaceError> {
    let relative = path
        .strip_prefix(root)
        .map_err(|_| WorkspaceError::InvalidPath(path.clone()))?;
    let bytes = std::fs::read(&path).map_err(|error| error.to_string());
    let revision = bytes
        .as_ref()
        .map_or_else(|error| revision(error.as_bytes()), |bytes| revision(bytes));
    Ok(DoctorDocument {
        path: logical_path_from_native(relative)?,
        bytes,
        revision,
    })
}

fn observe_sidecar(
    charter_root: &Path,
    path: PathBuf,
) -> Result<DoctorSidecarEvidence, WorkspaceError> {
    let companion = companion_path(&path);
    Ok(DoctorSidecarEvidence {
        document: observe_document(charter_root, path)?,
        companion_exists: companion.is_file(),
    })
}

fn companion_path(sidecar: &Path) -> PathBuf {
    let filename = sidecar
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or_default();
    let stem = filename
        .strip_prefix('.')
        .and_then(|name| name.strip_suffix(".json"))
        .unwrap_or(filename);
    sidecar.with_file_name(format!("{stem}.actions"))
}

fn observe_plan_collections(
    root: &Path,
    mount: MountId,
) -> Result<Vec<DoctorCollectionEvidence>, WorkspaceError> {
    let mut collections = Vec::new();
    let entries = match std::fs::read_dir(root) {
        Ok(entries) => entries.collect::<Result<Vec<_>, _>>()?,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(collections),
        Err(error) => return Err(error.into()),
    };
    for entry in entries {
        if entry.file_type()?.is_dir() {
            let path = logical_path_from_native(Path::new(&entry.file_name()))?;
            collections.push(DoctorCollectionEvidence {
                location: ResourceLocation::new(mount, path),
                revision: collection_revision(&entry.path())?,
            });
        }
    }
    collections.sort_by(|a, b| a.location.cmp(&b.location));
    Ok(collections)
}

fn collection_revision(root: &Path) -> Result<ResourceRevision, WorkspaceError> {
    let mut hasher = blake3::Hasher::new();
    if !root.is_dir() {
        return Ok(revision(b"missing"));
    }
    let mut stack = vec![root.to_path_buf()];
    while let Some(directory) = stack.pop() {
        let mut entries = std::fs::read_dir(&directory)?.collect::<Result<Vec<_>, _>>()?;
        entries.sort_by_key(std::fs::DirEntry::path);
        for entry in entries {
            let path = entry.path();
            let relative = path
                .strip_prefix(root)
                .map_err(|_| WorkspaceError::InvalidPath(path.clone()))?;
            let file_type = entry.file_type()?;
            hasher.update(if file_type.is_dir() { b"d" } else { b"f" });
            hasher.update(relative.to_string_lossy().as_bytes());
            if file_type.is_dir() {
                stack.push(path);
            } else if file_type.is_file() {
                hasher.update(&std::fs::read(path)?);
            }
        }
    }
    Ok(ResourceRevision::new(
        hasher.finalize().to_hex().to_string(),
    ))
}

fn revision(bytes: &[u8]) -> ResourceRevision {
    ResourceRevision::new(blake3::hash(bytes).to_hex().to_string())
}

fn collect_temps(
    root: &Path,
    mount: MountId,
    residue: &mut Vec<DurabilityResidue>,
) -> Result<(), WorkspaceError> {
    for path in walk_visible_files(root) {
        if path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.starts_with(".tmp"))
        {
            let relative = path
                .strip_prefix(root)
                .map_err(|_| WorkspaceError::InvalidPath(path.clone()))?;
            residue.push(DurabilityResidue {
                location: ResourceLocation::new(mount, logical_path_from_native(relative)?),
                kind: DurabilityResidueKind::OrphanedTemp,
            });
        }
    }
    Ok(())
}

fn walk_visible_files(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(directory) = stack.pop() {
        let Ok(mut entries) =
            std::fs::read_dir(directory).and_then(|entries| entries.collect::<Result<Vec<_>, _>>())
        else {
            continue;
        };
        entries.sort_by_key(std::fs::DirEntry::path);
        for entry in entries {
            let path = entry.path();
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            if file_type.is_dir() {
                let hidden = path
                    .file_name()
                    .is_some_and(|name| name.to_string_lossy().starts_with('.'));
                if !hidden {
                    stack.push(path);
                }
            } else if file_type.is_file() {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

fn logical_path(path: &str) -> Result<WorkspacePath, WorkspaceError> {
    WorkspacePath::new(path).map_err(|error| WorkspaceError::Actions(error.to_string()))
}

fn logical_path_from_native(path: &Path) -> Result<WorkspacePath, WorkspaceError> {
    let logical = path
        .components()
        .map(|component| component.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| WorkspaceError::InvalidPath(path.to_path_buf()))?
        .join("/");
    logical_path(&logical)
}

fn validate_file_revision(path: &Path, expected: &ResourceRevision) -> Result<(), WorkspaceError> {
    let actual = std::fs::read(path).map(|bytes| revision(&bytes))?;
    if &actual == expected {
        Ok(())
    } else {
        Err(WorkspaceError::Actions(format!(
            "doctor repair evidence for '{}' is stale",
            path.display()
        )))
    }
}

fn remove_file_if_present(path: &Path) -> Result<(), WorkspaceError> {
    match std::fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error.into()),
    }
}

fn remove_dir_if_present(path: &Path) -> Result<(), WorkspaceError> {
    match std::fs::remove_dir_all(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error.into()),
    }
}
