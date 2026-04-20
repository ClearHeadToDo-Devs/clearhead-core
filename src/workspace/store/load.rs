use super::discovery::{discover_action_files, discover_charter_files};
use super::pathing::{infer_charter_name_for_workspace, infer_parent_charter_name_for_workspace};
use super::{WorkspaceError, resolve_workspace_layout};
use crate::domain::{Charter, DomainModel};
use crate::workspace::actions::convert::from_actions_with_charter;
use crate::workspace::acts::{closed_acts_path, merge_acts_into_model, open_acts_path, read_acts};
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
    let mut action_file_paths: Vec<PathBuf> = Vec::new();

    for file_path in discover_action_files(&layout.data_root)? {
        let relative = file_path
            .strip_prefix(&layout.data_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;

        let action_source = std::fs::read_to_string(&file_path)?;
        let parsed_doc = super::super::parse_document(&action_source)
            .map_err(|e| WorkspaceError::Parse(format!("{}: {}", file_path.display(), e)))?;
        let mut actions = parsed_doc.actions;

        if !parsed_doc.syntax_errors.is_empty() {
            eprintln!(
                "warning: [{}] parsed with {} issue(s); loaded {} recoverable action(s)",
                file_path.display(),
                parsed_doc.syntax_errors.len(),
                actions.len()
            );

            for diagnostic in parsed_doc.syntax_errors.iter().take(5) {
                eprintln!(
                    "  - line {}, col {}: {}",
                    diagnostic.range.start_row + 1,
                    diagnostic.range.start_col + 1,
                    diagnostic.message
                );
            }

            let remaining = parsed_doc.syntax_errors.len().saturating_sub(5);
            if remaining > 0 {
                eprintln!("  - ... and {} more issue(s)", remaining);
            }
        }

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
        action_file_paths.push(file_path);
    }

    for file_path in discover_charter_files(&layout.data_root)? {
        let relative = file_path
            .strip_prefix(&layout.data_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;

        let explicit = parse_charter(&std::fs::read_to_string(&file_path)?)
            .map_err(|e| WorkspaceError::Parse(format!("{}: {}", file_path.display(), e)))?;

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

    // Explicit frontmatter wins: translate filesystem-inferred parent names to their
    // resolved aliases so that e.g. a root charter with alias "build_clearhead" (not
    // the raw directory name "workspace") is what sibling charters reference.
    let name_to_alias: std::collections::HashMap<String, String> = charters
        .iter()
        .filter_map(|(name, c)| c.alias.as_ref().map(|a| (name.clone(), a.clone())))
        .collect();

    let resolved_hints: Vec<(String, String)> =
        parent_hints(&path_for_name, layout.project_root_charter.as_deref())
            .into_iter()
            .map(|(name, parent_name)| {
                let alias = name_to_alias
                    .get(&parent_name)
                    .cloned()
                    .unwrap_or(parent_name);
                (name, alias)
            })
            .collect();

    for (name, parent_alias) in resolved_hints {
        if let Some(charter) = charters.get_mut(&name)
            && charter.parent.is_none()
        {
            charter.parent = Some(parent_alias);
        }
    }

    // Warn about parents that can't be resolved to a known charter alias.
    let known_aliases: std::collections::HashSet<&str> = charters
        .values()
        .filter_map(|c| c.alias.as_deref())
        .collect();
    for (name, charter) in &charters {
        if let Some(parent) = &charter.parent {
            if !known_aliases.contains(parent.as_str()) {
                let file = path_for_name
                    .get(name)
                    .map(|p| p.display().to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                eprintln!(
                    "warning: [{}] charter '{}' has unresolvable parent '{}' — \
                     use the alias (machine key), not the display title",
                    file,
                    charter.alias.as_deref().unwrap_or(&charter.title),
                    parent
                );
            }
        }
    }

    let mut model = DomainModel {
        objectives: vec![],
        charters: charters.into_values().collect(),
    };

    let mut all_acts = Vec::new();
    for file_path in &action_file_paths {
        let open = open_acts_path(file_path);
        let closed = closed_acts_path(file_path);
        all_acts.extend(
            read_acts(&open)
                .map_err(|e| WorkspaceError::Acts(format!("{}: {}", open.display(), e)))?,
        );
        all_acts.extend(
            read_acts(&closed)
                .map_err(|e| WorkspaceError::Acts(format!("{}: {}", closed.display(), e)))?,
        );
    }
    merge_acts_into_model(&mut model, all_acts);

    Ok(model)
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
