use super::discovery::{discover_action_files, discover_charter_files};
use super::pathing::{infer_charter_name_for_workspace, infer_parent_charter_name_for_workspace};
use super::{WorkspaceError, resolve_workspace_layout};
use crate::workspace::durability::recover_pending;
use crate::domain::{Charter, DomainModel};
use crate::workspace::actions::convert::from_actions_with_charter;
use crate::workspace::actions::repository::{ActionSource, SourcedAction};
use crate::workspace::charter::{MarkdownCharter, frontmatter_has_parent_key, implicit_charter, parse_charter};
use crate::workspace::ics::parse_ics_file;
use crate::workspace::plans::collect_plan_files_in;
use crate::workspace::sidecar::{hydrate_acts, read_sidecar, sidecar_path};
use crate::workspace::store::infer_charter_name;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// The complete filesystem representation of a workspace.
///
/// Holds all file-layer types ([`MarkdownCharter`] → [`ICSPlan`] / [`SourcedAction`]).
/// Convert to a pure [`DomainModel`] via `From` at the workspace boundary —
/// all file paths and source metadata are stripped in that conversion.
///
/// [`ICSPlan`]: crate::workspace::ics::ICSPlan
pub struct Workspace {
    pub root: PathBuf,
    /// Stable UUID for this workspace's RDF named graph. `None` for uninitialized workspaces.
    pub id: Option<String>,
    /// Display name — used to scope output in multi-workspace contexts.
    pub name: Option<String>,
    pub charters: Vec<MarkdownCharter>,
}

impl Workspace {
    pub fn load(root: &Path) -> Result<Self, WorkspaceError> {
        Self::load_with_plans(root, None)
    }

    /// Like [`Workspace::load`] but reads plan `.ics` from `plan_override` when
    /// given, instead of the workspace's own `plans/` directory.
    pub fn load_with_plans(
        root: &Path,
        plan_override: Option<&Path>,
    ) -> Result<Self, WorkspaceError> {
        let charters = load_workspace_with_plans(root, plan_override)?;
        Ok(Self { root: root.to_path_buf(), id: None, name: None, charters })
    }
}

/// Load the primary workspace and all additional workspaces listed in `config`.
///
/// The primary workspace gets `id` and `name` from `config`.
/// Additional workspaces have neither — each has its own config that
/// only the calling tool knows how to load.
///
/// Returns `Err` on the first workspace that fails to load.
pub fn load_workspaces(
    primary_root: &Path,
    config: &crate::config::WorkspaceConfig,
) -> Result<Vec<Workspace>, WorkspaceError> {
    // The primary workspace honors a configured plan_path; additional workspaces
    // own their own config (which only the calling tool knows how to load) and so
    // fall back to their default plans/ directory.
    let plan_override = config.plan_path.as_deref().map(Path::new);
    let mut primary = Workspace::load_with_plans(primary_root, plan_override)?;
    primary.id = config.workspace_id.clone();
    primary.name = config.workspace_name.clone();
    let mut workspaces = vec![primary];
    for path_str in &config.additional_workspaces {
        workspaces.push(Workspace::load(Path::new(path_str))?);
    }
    Ok(workspaces)
}

impl From<Workspace> for DomainModel {
    fn from(ws: Workspace) -> DomainModel {
        DomainModel {
            objectives: vec![],
            charters: ws.charters.into_iter().map(Charter::from).collect(),
        }
    }
}

/// Load a [`DomainModel`] from a workspace root directory.
pub fn load_domain_model(root: &Path) -> Result<DomainModel, WorkspaceError> {
    Ok(Workspace::load(root)?.into())
}

/// Like [`load_domain_model`] but reads plan `.ics` from `plan_override` when
/// given (the resolved `plan_path` config value), instead of the workspace's own
/// `plans/` directory. `None` is identical to [`load_domain_model`].
pub fn load_domain_model_with_plans(
    root: &Path,
    plan_override: Option<&Path>,
) -> Result<DomainModel, WorkspaceError> {
    Ok(Workspace::load_with_plans(root, plan_override)?.into())
}

/// Load the workspace as a [`FileSystemWorkspace`], preserving file-layer metadata.
///
/// Prefer [`FileSystemWorkspace::load`] directly. This free function exists for
/// callers that need only the charter list without the root path.
pub fn load_workspace(root: &Path) -> Result<Vec<MarkdownCharter>, WorkspaceError> {
    load_workspace_with_plans(root, None)
}

/// Like [`load_workspace`] but reads plan `.ics` from `plan_override` when given,
/// instead of the workspace's own `plans/` directory.
pub fn load_workspace_with_plans(
    root: &Path,
    plan_override: Option<&Path>,
) -> Result<Vec<MarkdownCharter>, WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.to_path_buf()));
    }

    let layout = resolve_workspace_layout(root);

    // Replay any journal left by an interrupted save before reading any files.
    if layout.charter_root.exists() {
        recover_pending(&layout.charter_root)?;
    }

    let mut charters: HashMap<String, MarkdownCharter> = HashMap::new();
    let mut path_for_name: HashMap<String, PathBuf> = HashMap::new();

    for file_path in discover_action_files(&layout.charter_root)? {
        let relative = file_path
            .strip_prefix(&layout.charter_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;

        let action_source = std::fs::read_to_string(&file_path)?;
        let parsed_doc = super::super::parse_document(&action_source)
            .map_err(|e| WorkspaceError::Parse(format!("{}: {}", file_path.display(), e)))?;

        if !parsed_doc.syntax_errors.is_empty() {
            eprintln!(
                "warning: [{}] parsed with {} issue(s); loaded {} recoverable action(s)",
                file_path.display(),
                parsed_doc.syntax_errors.len(),
                parsed_doc.actions.len()
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

        let project = infer_charter_name(&relative);
        let source_map = parsed_doc.source_map;
        let mut sourced: Vec<SourcedAction> = parsed_doc.actions
            .into_iter()
            .map(|mut action| {
                if action.charter.is_none() {
                    action.charter = Some(name.clone());
                }
                let metadata = source_map.get(&action.id).cloned();
                SourcedAction {
                    action,
                    source: ActionSource { file_path: relative.clone(), project: project.clone() },
                    source_metadata: metadata,
                }
            })
            .collect();

        let base: Charter = from_actions_with_charter(
            &sourced.iter().map(|sa| sa.action.clone()).collect::<Vec<_>>(),
            name.clone(),
        );
        let mut mc = MarkdownCharter::from(base);
        mc.acts_file = Some(relative.clone());
        mc.actions = sourced.clone();

        let sc_path = layout.charter_root.join(sidecar_path(&relative));
        let sidecar = read_sidecar(&sc_path)?;
        hydrate_acts(&mut sourced, &sidecar);
        mc.actions = sourced;

        charters
            .entry(name.clone())
            .and_modify(|c| c.plans.extend(mc.plans.clone()))
            .or_insert(mc);
        path_for_name.entry(name).or_insert(relative);
    }

    // Charters whose .md file contains an explicit `parent:` key (even null).
    // These are exempt from implicit path-inferred parent hints.
    let mut explicit_parent_charters: std::collections::HashSet<String> =
        std::collections::HashSet::new();

    for file_path in discover_charter_files(&layout.charter_root)? {
        let relative = file_path
            .strip_prefix(&layout.charter_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let name =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
                .ok_or_else(|| WorkspaceError::Parse("Failed to infer charter name".to_string()))?;

        let content = std::fs::read_to_string(&file_path)?;
        if frontmatter_has_parent_key(&content) {
            explicit_parent_charters.insert(name.clone());
        }
        let explicit = parse_charter(&content)
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
                if explicit.state.is_some() {
                    implicit.state = explicit.state;
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
        // Skip charters whose .md file had an explicit `parent:` key — they own their parent.
        if explicit_parent_charters.contains(&name) {
            continue;
        }
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

    let mut charters_by_name = charters;

    // Load ICS schedules: each .ics VEVENT becomes a Plan in the matching charter.
    // entry.relative_path is relative to plans_root (e.g. "inbox/uid.ics").
    // entry.charter_name is the slug used as the directory name (e.g. "work-feature").
    // Match against charters via computed slug: parent-alias + "-" + alias.
    // A configured plan_path overrides where the .ics live; otherwise use the
    // workspace's own plans/ directory.
    let plans_root = plan_override.unwrap_or(&layout.plans_root);
    for entry in collect_plan_files_in(plans_root, layout.project_root_charter.as_deref())? {
        let plans = parse_ics_file(&entry.path)?;
        if plans.is_empty() {
            continue;
        }
        // plans_dir: the charter's subdirectory relative to plans_root (e.g. "inbox")
        let plans_dir = entry
            .relative_path
            .parent()
            .map(std::path::Path::to_path_buf)
            .ok_or_else(|| WorkspaceError::InvalidPath(entry.relative_path.clone()))?;
        if let Some(charter) = charters_by_name.values_mut().find(|c| {
            charter_plans_slug(c) == entry.charter_name
                || c.alias.as_deref() == Some(&entry.charter_name)
                || c.title == entry.charter_name
        }) {
            charter.plans_dir = Some(plans_dir.clone());
            charter.plans.extend(plans);
        } else {
            let mut charter = MarkdownCharter::from(implicit_charter(&entry.charter_name));
            charter.parent = entry.inferred_parent.clone();
            charter.plans_dir = Some(plans_dir.clone());
            charter.plans = plans;
            path_for_name.entry(entry.charter_name.clone()).or_insert(plans_dir);
            charters_by_name.insert(entry.charter_name.clone(), charter);
        }
    }

    let charters: Vec<MarkdownCharter> = charters_by_name.into_values().collect();

    Ok(charters)
}

/// Compute the plans directory slug for a charter: `<parent>-<alias>` for sub-charters,
/// `<alias>` for top-level charters. Matches the directory name under `plans/`.
fn charter_plans_slug(c: &MarkdownCharter) -> String {
    let alias = c.alias.as_deref().unwrap_or(&c.title);
    match &c.parent {
        None => alias.to_string(),
        Some(parent) => format!("{}-{}", parent, alias),
    }
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
