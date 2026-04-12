use super::discovery::discover_action_files;
use super::WorkspaceLayout;
use super::{resolve_workspace_layout, WorkspaceError};
use crate::domain::{Charter, DomainModel, Plan, PlannedAct};
use crate::workspace::actions::convert::merge_to_action;
use crate::workspace::{format, ActionList, OutputFormat};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use uuid::Uuid;

/// Save a `DomainModel` to a workspace root directory.
///
/// Only writes files whose content has changed and removes orphaned files
/// (those no longer represented in the model). Directories are created as
/// needed and pruned when empty.
pub fn save_domain_model(root: &Path, model: &DomainModel) -> Result<(), WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.to_path_buf()));
    }

    let layout = resolve_workspace_layout(root);
    std::fs::create_dir_all(&layout.data_root)?;

    let manifest = build_action_file_manifest(model, &layout)?;

    // Write only files whose content differs from what's on disk.
    for (relative_path, content) in &manifest {
        let absolute_path = layout.data_root.join(relative_path);
        let needs_write = std::fs::read_to_string(&absolute_path)
            .map(|existing| existing != *content)
            .unwrap_or(true); // file missing → write it

        if needs_write {
            if let Some(parent) = absolute_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(absolute_path, content)?;
        }
    }

    // Remove orphaned files (on disk but not in the new manifest).
    let manifest_paths: HashSet<PathBuf> = manifest.keys().cloned().collect();
    for existing in discover_action_files(&layout.data_root)? {
        let relative = existing
            .strip_prefix(&layout.data_root)
            .unwrap_or(&existing)
            .to_path_buf();
        if !manifest_paths.contains(&relative) {
            std::fs::remove_file(&existing)?;
            if let Some(parent) = existing.parent() {
                prune_empty_directories(parent, &layout.data_root)?;
            }
        }
    }

    Ok(())
}

fn build_action_file_manifest(
    model: &DomainModel,
    layout: &WorkspaceLayout,
) -> Result<BTreeMap<PathBuf, String>, WorkspaceError> {
    let key_by_id: HashMap<Uuid, String> = model
        .charters
        .iter()
        .map(|charter| (charter.id, charter_reference_name(charter)))
        .collect();

    let mut key_by_alias: HashMap<String, String> = HashMap::new();
    let mut key_by_title: HashMap<String, String> = HashMap::new();
    for charter in &model.charters {
        let key = charter_reference_name(charter);
        if let Some(alias) = &charter.alias {
            key_by_alias.insert(alias.clone(), key.clone());
        }
        key_by_title.insert(charter.title.clone(), key);
    }

    let parent_by_key: HashMap<String, Option<String>> = model
        .charters
        .iter()
        .map(|charter| {
            let key = charter_reference_name(charter);
            let parent = charter.parent.as_ref().and_then(|raw_parent| {
                key_by_alias
                    .get(raw_parent)
                    .cloned()
                    .or_else(|| key_by_title.get(raw_parent).cloned())
            });
            (key, parent)
        })
        .collect();

    let project_root_key =
        resolve_project_root_charter_key(model, layout.project_root_charter.as_deref());

    let mut manifest = BTreeMap::new();
    for charter in &model.charters {
        let key = key_by_id
            .get(&charter.id)
            .cloned()
            .unwrap_or_else(|| charter_reference_name(charter));

        let path =
            relative_actions_path_for_charter(&key, &parent_by_key, project_root_key.as_deref());
        let actions = actions_for_charter(charter, &key);
        let rendered =
            format(&actions, OutputFormat::Actions, None, None).map_err(WorkspaceError::Acts)?;
        manifest.insert(path, rendered);
    }

    Ok(manifest)
}

fn resolve_project_root_charter_key(
    model: &DomainModel,
    project_root_name: Option<&str>,
) -> Option<String> {
    let project_root_name = project_root_name?;

    model
        .charters
        .iter()
        .find(|charter| {
            charter_reference_name(charter) == project_root_name
                || charter.title == project_root_name
                || charter.alias.as_deref() == Some(project_root_name)
        })
        .map(charter_reference_name)
}

fn relative_actions_path_for_charter(
    key: &str,
    parent_by_key: &HashMap<String, Option<String>>,
    project_root_key: Option<&str>,
) -> PathBuf {
    if project_root_key == Some(key) {
        return PathBuf::from("next.actions");
    }

    let mut chain = Vec::new();
    let mut visited = HashSet::new();
    let mut current = Some(key.to_string());

    while let Some(name) = current {
        if !visited.insert(name.clone()) {
            break;
        }
        if Some(name.as_str()) == project_root_key {
            break;
        }

        chain.push(name.clone());
        current = parent_by_key.get(&name).cloned().flatten();
    }

    chain.reverse();
    if chain.is_empty() {
        return PathBuf::from(format!("{}.actions", key));
    }

    let mut path = PathBuf::new();
    for parent in chain.iter().take(chain.len().saturating_sub(1)) {
        path.push(parent);
    }
    if let Some(name) = chain.last() {
        path.push(format!("{}.actions", name));
    }
    path
}

fn charter_reference_name(charter: &Charter) -> String {
    charter
        .alias
        .clone()
        .unwrap_or_else(|| charter.title.clone())
}

fn representative_act(plan: &Plan) -> PlannedAct {
    if let Some(act) = plan
        .acts
        .iter()
        .find(|act| act.id == Uuid::new_v5(&plan.id, b"act-0"))
    {
        return act.clone();
    }

    if let Some(act) = plan.acts.first() {
        return act.clone();
    }

    PlannedAct {
        id: Uuid::new_v5(&plan.id, b"act-0"),
        plan_id: plan.id,
        ..Default::default()
    }
}

fn actions_for_charter(charter: &Charter, charter_name: &str) -> ActionList {
    let mut plans: Vec<&Plan> = charter.plans.iter().collect();
    plans.sort_by_key(|plan| plan.id.to_string());

    plans
        .into_iter()
        .map(|plan| {
            let mut action = merge_to_action(plan, &representative_act(plan), plan.id);
            action.charter = Some(charter_name.to_string());
            action
        })
        .collect()
}

fn prune_empty_directories(start: &Path, stop_at: &Path) -> Result<(), WorkspaceError> {
    let mut current = start.to_path_buf();

    loop {
        if current == *stop_at {
            break;
        }

        match std::fs::remove_dir(&current) {
            Ok(()) => {
                if let Some(parent) = current.parent() {
                    current = parent.to_path_buf();
                } else {
                    break;
                }
            }
            Err(err) if err.kind() == std::io::ErrorKind::DirectoryNotEmpty => break,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => break,
            Err(err) => return Err(WorkspaceError::Io(err)),
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Charter, DomainModel, Plan};
    use crate::workspace::store::load_domain_model;
    use chrono::Local;

    fn test_plan(name: &str) -> Plan {
        let id = Uuid::new_v4();
        Plan {
            id,
            name: name.to_string(),
            acts: vec![PlannedAct {
                id: Uuid::new_v5(&id, b"act-0"),
                plan_id: id,
                duration: Some(30),
                created_at: Some(Local::now()),
                ..Default::default()
            }],
            ..Default::default()
        }
    }

    #[test]
    fn save_project_local_layout() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("platform");
        std::fs::create_dir_all(root.join(".clearhead")).expect("create .clearhead");

        std::fs::write(root.join(".clearhead").join("stale.actions"), "[ ] stale\n")
            .expect("write stale");

        let model = DomainModel {
            objectives: vec![],
            charters: vec![
                Charter {
                    id: Uuid::new_v4(),
                    title: "platform".to_string(),
                    alias: Some("platform".to_string()),
                    plans: vec![test_plan("Ship v1")],
                    ..Default::default()
                },
                Charter {
                    id: Uuid::new_v4(),
                    title: "infra".to_string(),
                    alias: Some("infra".to_string()),
                    parent: Some("platform".to_string()),
                    plans: vec![test_plan("Harden CI")],
                    ..Default::default()
                },
                Charter {
                    id: Uuid::new_v4(),
                    title: "deploy".to_string(),
                    alias: Some("deploy".to_string()),
                    parent: Some("infra".to_string()),
                    plans: vec![test_plan("Blue/green rollout")],
                    ..Default::default()
                },
            ],
        };

        save_domain_model(&root, &model).expect("save model");

        assert!(root.join(".clearhead/next.actions").exists());
        assert!(root.join(".clearhead/infra.actions").exists());
        assert!(root.join(".clearhead/infra/deploy.actions").exists());
        assert!(!root.join(".clearhead/stale.actions").exists());

        let loaded = load_domain_model(&root).expect("load model");
        let mut names: Vec<String> = loaded.charters.iter().map(|c| c.title.clone()).collect();
        names.sort();
        assert_eq!(names, vec!["deploy", "infra", "platform"]);

        let infra = loaded
            .charters
            .iter()
            .find(|c| c.title == "infra")
            .expect("infra charter");
        assert_eq!(infra.parent.as_deref(), Some("platform"));

        let deploy = loaded
            .charters
            .iter()
            .find(|c| c.title == "deploy")
            .expect("deploy charter");
        assert_eq!(deploy.parent.as_deref(), Some("infra"));
    }

    #[test]
    fn save_global_layout() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().to_path_buf();

        let model = DomainModel {
            objectives: vec![],
            charters: vec![
                Charter {
                    id: Uuid::new_v4(),
                    title: "work".to_string(),
                    description: None,
                    alias: Some("work".to_string()),
                    parent: None,
                    objectives: None,
                    plans: vec![test_plan("Quarter plan")],
                },
                Charter {
                    id: Uuid::new_v4(),
                    title: "ops".to_string(),
                    description: None,
                    alias: Some("ops".to_string()),
                    parent: Some("work".to_string()),
                    objectives: None,
                    plans: vec![test_plan("Backups")],
                },
            ],
        };

        save_domain_model(&root, &model).expect("save model");

        assert!(root.join("work.actions").exists());
        assert!(root.join("work/ops.actions").exists());
    }

    #[test]
    fn incremental_save_only_writes_changed_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().to_path_buf();

        let work_id = Uuid::new_v4();
        let ops_id = Uuid::new_v4();

        let model = DomainModel {
            objectives: vec![],
            charters: vec![
                Charter {
                    id: work_id,
                    title: "work".to_string(),
                    description: None,
                    alias: Some("work".to_string()),
                    parent: None,
                    objectives: None,
                    plans: vec![test_plan("Quarter plan")],
                },
                Charter {
                    id: ops_id,
                    title: "ops".to_string(),
                    description: None,
                    alias: Some("ops".to_string()),
                    parent: Some("work".to_string()),
                    objectives: None,
                    plans: vec![test_plan("Backups")],
                },
            ],
        };

        save_domain_model(&root, &model).expect("initial save");

        // Capture the content of the unchanged file before the second save.
        let work_content_before =
            std::fs::read_to_string(root.join("work.actions")).expect("read work.actions");

        // Save again with only ops modified.
        let mut updated = model.clone();
        updated.charters[1].plans[0].name = "Backups v2".to_string();
        save_domain_model(&root, &updated).expect("incremental save");

        // ops should reflect the change.
        let ops_content =
            std::fs::read_to_string(root.join("work/ops.actions")).expect("read ops.actions");
        assert!(
            ops_content.contains("Backups v2"),
            "ops file should be updated"
        );

        // work should be byte-for-byte identical — not rewritten.
        let work_content_after =
            std::fs::read_to_string(root.join("work.actions")).expect("read work.actions");
        assert_eq!(
            work_content_before, work_content_after,
            "unchanged charter file should not be rewritten"
        );
    }

    #[test]
    fn incremental_save_removes_orphaned_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().to_path_buf();

        let work_id = Uuid::new_v4();
        let ops_id = Uuid::new_v4();

        let model = DomainModel {
            objectives: vec![],
            charters: vec![
                Charter {
                    id: work_id,
                    title: "work".to_string(),
                    description: None,
                    alias: Some("work".to_string()),
                    parent: None,
                    objectives: None,
                    plans: vec![test_plan("Quarter plan")],
                },
                Charter {
                    id: ops_id,
                    title: "ops".to_string(),
                    description: None,
                    alias: Some("ops".to_string()),
                    parent: Some("work".to_string()),
                    objectives: None,
                    plans: vec![test_plan("Backups")],
                },
            ],
        };

        save_domain_model(&root, &model).expect("initial save");
        assert!(root.join("work/ops.actions").exists());

        // Remove ops from the model and save again.
        let mut trimmed = model.clone();
        trimmed.charters.retain(|c| c.id != ops_id);
        save_domain_model(&root, &trimmed).expect("trimmed save");

        assert!(
            !root.join("work/ops.actions").exists(),
            "orphaned file should be removed"
        );
        assert!(
            root.join("work.actions").exists(),
            "surviving file should remain"
        );
    }
}
