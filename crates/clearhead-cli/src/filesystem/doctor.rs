//! Native doctor observation and typed repair execution.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use chrono::Local;
use clearhead_core::workspace::resource::{
    MountId, ResourceLocation, ResourceRevision, WorkspacePath,
};
use clearhead_core::workspace::{
    Diagnosis, DoctorCollectionEvidence, DoctorDocument, DoctorEvidence, DoctorRepair,
    DoctorSidecarEvidence, DurabilityResidue, DurabilityResidueKind, WorkspaceError, WorkspaceRead,
    diagnose,
};

use crate::filesystem::mounts::NativeWorkspaceMounts;

/// Observe and diagnose a native workspace without replaying pending intent.
pub fn diagnose_workspace(workspace_root: &Path) -> Result<Diagnosis, WorkspaceError> {
    let read = crate::filesystem::read_workspace(workspace_root)?;
    diagnose_workspace_read(workspace_root, &read)
}

/// Diagnose an already assembled workspace while observing doctor's additional
/// native evidence exactly once.
pub fn diagnose_workspace_read(
    workspace_root: &Path,
    read: &WorkspaceRead,
) -> Result<Diagnosis, WorkspaceError> {
    let evidence = observe_doctor(workspace_root)?;
    let mut diagnosis = diagnose(read, &evidence);
    let charter_root = crate::filesystem::charter_root(workspace_root);
    for finding in &mut diagnosis.findings {
        if finding.code == "charter-document-without-id" {
            let document = charter_root.join(&finding.path);
            finding.message = finding
                .message
                .replace("<charter.md>", &document.display().to_string());
        }
    }
    Ok(diagnosis)
}

/// Gather native facts that normal workspace assembly intentionally excludes.
pub fn observe_doctor(workspace_root: &Path) -> Result<DoctorEvidence, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root);
    if !mounts.workspace.is_dir() {
        return Err(WorkspaceError::InvalidPath(mounts.workspace));
    }
    let charter_root = mounts.workspace.join("charters");
    let archive_root = mounts.workspace.join("archive");
    let plans_root = mounts.plans_root();

    let completed_actions = walk_visible_files(&charter_root)
        .into_iter()
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.ends_with(".completed.actions"))
                && clearhead_core::workspace::action_files::is_action_history(
                    &super::action_files::role_path(&mounts.workspace, path),
                )
        })
        .map(|path| observe_document(&charter_root, path))
        .collect::<Result<Vec<_>, _>>()?;
    let archived_actions = walk_visible_files(&archive_root)
        .into_iter()
        .filter(|path| {
            clearhead_core::workspace::action_files::is_action_history(
                &super::action_files::role_path(&mounts.workspace, path),
            ) || matches!(
                clearhead_core::workspace::action_files::classify_action_file(path),
                Some((
                    _,
                    clearhead_core::workspace::action_files::ActionFileRole::Habits
                ))
            )
        })
        .map(|path| observe_document(&archive_root, path))
        .collect::<Result<Vec<_>, _>>()?;
    let archived_charters = walk_visible_files(&archive_root)
        .into_iter()
        .filter(|path| path.extension().and_then(|extension| extension.to_str()) == Some("md"))
        .map(|path| observe_document(&archive_root, path))
        .collect::<Result<Vec<_>, _>>()?;
    let readme = charter_root.join(clearhead_core::workspace::PRIMARY_DOCUMENT_FILE);
    let root_readme = readme
        .is_file()
        .then(|| observe_document(&charter_root, readme))
        .transpose()?;
    let sidecars = observe_sidecars(&charter_root)?;
    let archived_sidecars = observe_sidecars(&archive_root)?;
    let plan_collections = observe_plan_collections(&plans_root, MountId::Workspace)?;

    let mut durability_residue = Vec::new();
    if charter_root.join(".pending").is_file() {
        durability_residue.push(DurabilityResidue {
            location: ResourceLocation::new(MountId::Workspace, logical_path(".pending")?),
            kind: DurabilityResidueKind::PendingJournal,
        });
    }
    collect_temps(&charter_root, MountId::Workspace, &mut durability_residue)?;
    collect_temps(&plans_root, MountId::Workspace, &mut durability_residue)?;

    Ok(DoctorEvidence {
        manifest: crate::filesystem::read_workspace_manifest(workspace_root),
        completed_actions,
        archived_actions,
        archived_charters,
        root_readme,
        sidecars,
        archived_sidecars,
        plan_collections,
        durability_residue,
        observed_at: Local::now(),
    })
}

/// Execute repairs selected by Core against freshly-read workspace state.
pub fn apply_doctor_repairs(
    workspace_root: &Path,
    repairs: &[DoctorRepair],
) -> Result<(), WorkspaceError> {
    if repairs.is_empty() {
        return Ok(());
    }
    let mounts = NativeWorkspaceMounts::resolve(workspace_root);
    let current = diagnose_workspace(workspace_root)?;
    if current.repairs != repairs {
        return Err(WorkspaceError::Actions(
            "doctor repair evidence is stale; rerun doctor and review the current repair plan"
                .into(),
        ));
    }

    repair_sidecars(&mounts.workspace, repairs)?;
    for repair in repairs {
        match repair {
            DoctorRepair::RemovePlansCollection { location, expected } => {
                let path = collection_dir(&mounts, location, expected)?;
                remove_dir_if_present(&path)?;
            }
            DoctorRepair::WriteCollectionDisplayname {
                location,
                name,
                expected,
            } => {
                let path = collection_dir(&mounts, location, expected)?;
                let displayname = path.join(crate::filesystem::COLLECTION_DISPLAYNAME_FILE);
                crate::filesystem::durability::atomic_write(&displayname, name)?;
            }
            _ => {}
        }
    }
    Ok(())
}

/// Load each affected sidecar once, validate all evidence, then deliver gains
/// before removals. A failed relocation can leave duplicates, never lost notes.
fn repair_sidecars(root: &Path, repairs: &[DoctorRepair]) -> Result<(), WorkspaceError> {
    use crate::filesystem::sidecar::{lock_sidecar, write_sidecar};
    let mut expected = BTreeMap::new();
    for repair in repairs {
        match repair {
            DoctorRepair::RelocateNotes {
                source,
                destination,
                expected: revision,
                destination_expected,
                ..
            } => {
                expected.insert(source.clone(), Some(revision.clone()));
                expected.insert(destination.clone(), destination_expected.clone());
            }
            DoctorRepair::PruneSidecarEntry {
                path,
                expected: revision,
                ..
            }
            | DoctorRepair::RemoveSidecar {
                path,
                expected: revision,
            }
            | DoctorRepair::MirrorRootCharterId {
                path,
                expected: revision,
                ..
            } => {
                expected.insert(
                    WorkspacePath::new(format!("charters/{path}")).unwrap(),
                    Some(revision.clone()),
                );
            }
            _ => {}
        }
    }
    // Deterministic lock order also covers an append racing a relocation.
    let lock_paths = expected
        .keys()
        .map(|path| root.join(path.as_str()).with_file_name(".clearhead.lock"))
        .collect::<BTreeSet<_>>();
    let _locks = lock_paths
        .iter()
        .map(|path| lock_sidecar(path))
        .collect::<Result<Vec<_>, _>>()?;
    let mut metadata = BTreeMap::new();
    for (path, expected_revision) in &expected {
        let physical = root.join(path.as_str());
        let bytes = match std::fs::read(&physical) {
            Ok(bytes) => Some(bytes),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error.into()),
        };
        if bytes.as_deref().map(revision).as_ref() != expected_revision.as_ref() {
            return Err(WorkspaceError::Actions(format!(
                "doctor repair evidence for '{path}' is stale"
            )));
        }
        let value = match bytes {
            Some(bytes) => clearhead_core::workspace::parse_sidecar(
                std::str::from_utf8(&bytes)
                    .map_err(|error| WorkspaceError::Parse(error.to_string()))?,
            )?,
            None => Default::default(),
        };
        metadata.insert(path.clone(), value);
    }
    let mut gains = BTreeSet::new();
    // Clone source logs before merging: a sidecar may both gain and lose logs.
    let transfers = repairs
        .iter()
        .filter_map(|repair| {
            if let DoctorRepair::RelocateNotes {
                source,
                destination,
                id,
                ..
            } = repair
            {
                Some((
                    source.clone(),
                    destination.clone(),
                    id.clone(),
                    metadata[source].notes[id].clone(),
                ))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    for (_, destination, id, notes) in &transfers {
        metadata
            .get_mut(destination)
            .unwrap()
            .notes
            .entry(id.clone())
            .or_default()
            .extend(notes.clone());
        gains.insert(destination.clone());
    }
    for path in gains {
        write_sidecar(&root.join(path.as_str()), &metadata[&path])?;
    }
    let mut changed = BTreeSet::new();
    for (source, _, id, _) in transfers {
        metadata.get_mut(&source).unwrap().notes.remove(&id);
        changed.insert(source);
    }
    for repair in repairs {
        match repair {
            DoctorRepair::PruneSidecarEntry { path, id, .. } => {
                let path = WorkspacePath::new(format!("charters/{path}")).unwrap();
                metadata.get_mut(&path).unwrap().actions.remove(id);
                changed.insert(path);
            }
            DoctorRepair::MirrorRootCharterId { path, id, .. } => {
                let path = WorkspacePath::new(format!("charters/{path}")).unwrap();
                metadata
                    .get_mut(&path)
                    .unwrap()
                    .charter
                    .get_or_insert_with(Default::default)
                    .id = Some(*id);
                changed.insert(path);
            }
            _ => {}
        }
    }
    for path in changed {
        write_sidecar(&root.join(path.as_str()), &metadata[&path])?;
    }
    for repair in repairs {
        if let DoctorRepair::RemoveSidecar { path, .. } = repair {
            remove_file_if_present(&root.join("charters").join(path.as_str()))?;
        }
    }
    Ok(())
}

/// Resolve a repair's calendar collection, refusing nested paths, an
/// unconfigured mount, or a collection that changed since diagnosis.
fn collection_dir(
    mounts: &NativeWorkspaceMounts,
    location: &ResourceLocation,
    expected: &ResourceRevision,
) -> Result<PathBuf, WorkspaceError> {
    if location.path.as_str().contains('/') {
        return Err(WorkspaceError::Actions(format!(
            "doctor refused unsafe calendar collection path '{}'",
            location.path
        )));
    }
    if !location.mount.is_workspace() {
        return Err(WorkspaceError::Actions(
            "doctor repair names a calendar collection outside the workspace".into(),
        ));
    }
    let path = mounts.plans_root().join(location.path.as_str());
    if collection_revision(&path)? != *expected {
        return Err(WorkspaceError::Actions(format!(
            "doctor repair evidence for calendar collection '{}' is stale",
            location.path
        )));
    }
    Ok(path)
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

fn observe_sidecars(root: &Path) -> Result<Vec<DoctorSidecarEvidence>, WorkspaceError> {
    walk_visible_files(root)
        .into_iter()
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with('.') && name.ends_with(".json"))
        })
        .map(|path| observe_sidecar(root, path))
        .collect()
}

fn observe_sidecar(
    charter_root: &Path,
    path: PathBuf,
) -> Result<DoctorSidecarEvidence, WorkspaceError> {
    let companion = companion_path(&path);
    Ok(DoctorSidecarEvidence {
        document: observe_document(charter_root, path)?,
        companion_exists: companion.is_file()
            || companion.with_extension("habits.actions").is_file(),
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
            let displayname = std::fs::read_to_string(
                entry
                    .path()
                    .join(crate::filesystem::COLLECTION_DISPLAYNAME_FILE),
            )
            .ok();
            collections.push(DoctorCollectionEvidence {
                location: ResourceLocation::new(mount, path),
                revision: collection_revision(&entry.path())?,
                displayname,
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
