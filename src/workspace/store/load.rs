use super::discovery::{discover_action_files, discover_charter_files};
use super::pathing::{infer_charter_name_for_workspace, infer_parent_charter_name_for_workspace};
use super::{WorkspaceError, resolve_workspace_layout};
use crate::domain::{Charter, DomainModel};
use crate::workspace::actions::convert::from_actions_with_charter;
use crate::workspace::charter::parse_charter;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Load a `DomainModel` from a workspace root directory.
pub fn load_domain_model(root: &Path) -> Result<DomainModel, WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.to_path_buf()));
    }

    let layout = resolve_workspace_layout(root);
    let mut charters: HashMap<String, Charter> = HashMap::new();
    let mut path_for_name: HashMap<String, PathBuf> = HashMap::new();

    for file_path in discover_action_files(&layout.data_root)? {
        let relative = file_path
            .strip_prefix(&layout.data_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;

        let mut actions = super::super::parse_actions(&std::fs::read_to_string(&file_path)?)
            .map_err(WorkspaceError::Parse)?;

        for action in &mut actions {
            if action.charter.is_none() {
                action.charter = Some(name.clone());
            }
        }

        let charter = from_actions_with_charter(&actions, name.clone());
        charters
            .entry(name.clone())
            .and_modify(|c| c.plans.extend(charter.plans.clone()))
            .or_insert(charter);
        path_for_name.entry(name).or_insert(relative);
    }

    for file_path in discover_charter_files(&layout.data_root)? {
        let relative = file_path
            .strip_prefix(&layout.data_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;

        let explicit =
            parse_charter(&std::fs::read_to_string(&file_path)?).map_err(WorkspaceError::Parse)?;

        charters
            .entry(name)
            .and_modify(|implicit| {
                implicit.id = explicit.id;
                implicit.title = explicit.title.clone();
                implicit.description = explicit.description.clone();
                if explicit.alias.is_some() {
                    implicit.alias = explicit.alias.clone();
                }
                if explicit.parent.is_some() {
                    implicit.parent = explicit.parent.clone();
                }
                if explicit.objectives.is_some() {
                    implicit.objectives = explicit.objectives.clone();
                }
            })
            .or_insert(explicit);
    }

    for (name, parent) in parent_hints(&path_for_name, layout.project_root_charter.as_deref()) {
        if let Some(charter) = charters.get_mut(&name)
            && charter.parent.is_none()
        {
            charter.parent = Some(parent);
        }
    }

    Ok(DomainModel {
        objectives: vec![],
        charters: assemble_charter_tree(charters),
    })
}

/// Move non-root charters into their parent's `sub_charters`, returning only roots.
fn assemble_charter_tree(mut flat: HashMap<String, Charter>) -> Vec<Charter> {
    // Build alias → key lookup for parent resolution
    let alias_to_key: HashMap<String, String> = flat
        .iter()
        .filter_map(|(k, c)| c.alias.as_ref().map(|a| (a.to_lowercase(), k.clone())))
        .collect();

    // Determine which keys are roots (parent is absent or unresolvable)
    let root_keys: Vec<String> = flat
        .iter()
        .filter(|(_k, c)| {
            let Some(parent_str) = &c.parent else { return true };
            let p = parent_str.to_lowercase();
            let resolves_to = alias_to_key.get(&p).cloned().unwrap_or_else(|| p.clone());
            !flat.contains_key(&resolves_to) && !flat.contains_key(&p)
        })
        .map(|(k, _)| k.clone())
        .collect();

    fn build(key: &str, flat: &mut HashMap<String, Charter>, alias_to_key: &HashMap<String, String>) -> Option<Charter> {
        let mut charter = flat.remove(key)?;
        // Collect keys of direct children
        let child_keys: Vec<String> = flat
            .iter()
            .filter(|(_, c)| {
                let Some(parent_str) = &c.parent else { return false };
                let p = parent_str.to_lowercase();
                let resolved = alias_to_key.get(&p).map(|s| s.as_str()).unwrap_or(&p);
                resolved.to_lowercase() == key.to_lowercase()
                    || (charter.alias.as_deref().map(|a| a.to_lowercase()).as_deref() == Some(resolved))
            })
            .map(|(k, _)| k.clone())
            .collect();

        for child_key in child_keys {
            if let Some(child) = build(&child_key, flat, alias_to_key) {
                charter.sub_charters.push(child);
            }
        }
        Some(charter)
    }

    root_keys
        .iter()
        .filter_map(|k| build(k, &mut flat, &alias_to_key))
        .collect()
}

fn parent_hints(
    path_for_name: &HashMap<String, PathBuf>,
    project_root_charter: Option<&str>,
) -> Vec<(String, String)> {
    path_for_name
        .iter()
        .filter_map(|(name, path)| {
            infer_parent_charter_name_for_workspace(path, project_root_charter)
                .map(|parent| (name.clone(), parent))
        })
        .collect()
}
