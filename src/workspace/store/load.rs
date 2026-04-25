use super::discovery::{discover_action_files, discover_charter_files};
use super::pathing::{infer_charter_name_for_workspace, infer_parent_charter_name_for_workspace};
use super::{WorkspaceError, resolve_workspace_layout};
use crate::domain::{Charter, DomainModel};
use crate::workspace::actions::convert::from_actions_with_charter;
use crate::workspace::charter::{MarkdownCharter, parse_charter};
use crate::workspace::ics::parse_ics_file;
use crate::workspace::plans::collect_plan_files;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Load a `DomainModel` from a workspace root directory.
///
/// Delegates to `load_markdown_charters` and strips file-path metadata.
pub fn load_domain_model(root: &Path) -> Result<DomainModel, WorkspaceError> {
    let charters = load_markdown_charters(root)?;
    Ok(DomainModel {
        objectives: vec![],
        charters: charters.into_iter().map(Charter::from).collect(),
    })
}

/// Load the workspace as a `Vec<MarkdownCharter>`, preserving the file paths
/// each charter's plans and acts came from.
pub fn load_markdown_charters(root: &Path) -> Result<Vec<MarkdownCharter>, WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.to_path_buf()));
    }

    let layout = resolve_workspace_layout(root);
    let mut charters: HashMap<String, MarkdownCharter> = HashMap::new();
    let mut path_for_name: HashMap<String, PathBuf> = HashMap::new();

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

        let base: Charter = from_actions_with_charter(&actions, name.clone());
        let mut mc = MarkdownCharter::from(base);
        mc.acts_file = Some(relative.clone());
        charters
            .entry(name.clone())
            .and_modify(|c| c.plans.extend(mc.plans.clone()))
            .or_insert(mc);
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

        let explicit = parse_charter(&std::fs::read_to_string(&file_path)?)
            .map_err(|e| WorkspaceError::Parse(format!("{}: {}", file_path.display(), e)))?;

        let md_relative = relative.clone();
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
                implicit.md_file = Some(md_relative.clone());
            })
            .or_insert_with(|| {
                let mut mc = MarkdownCharter::from(explicit);
                mc.md_file = Some(md_relative);
                mc
            });
    }

    // Explicit frontmatter wins: translate filesystem-inferred parent names to their
    // resolved aliases so that e.g. a root charter with alias "build_clearhead" (not
    // the raw directory name "workspace") is what sibling charters reference.
    let name_to_alias: HashMap<String, String> = charters
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

    let mut charters: Vec<MarkdownCharter> = charters.into_values().collect();

    // Load ICS schedules: each .ics VEVENT becomes a Plan in the matching charter.
    for entry in collect_plan_files(root)? {
        let plans = parse_ics_file(&entry.path)?;
        if plans.is_empty() {
            continue;
        }
        if let Some(charter) = charters.iter_mut().find(|c| {
            c.alias.as_deref() == Some(&entry.charter_name) || c.title == entry.charter_name
        }) {
            charter.ics_file = Some(entry.relative_path.clone());
            charter.plans.extend(plans);
        }
    }

    Ok(charters)
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
