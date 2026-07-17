use super::discovery::{discover_action_files, discover_charter_files};
use super::findings::Finding;
use super::pathing::{infer_charter_name_for_workspace, infer_parent_charter_name_for_workspace};
use super::{WorkspaceError, resolve_workspace_layout};
use crate::workspace::durability::recover_pending;
use crate::domain::{Charter, DomainModel};
use crate::workspace::actions::convert::from_actions_with_charter;
use crate::workspace::actions::repository::SourcedAction;
use crate::workspace::charter::{
    MarkdownCharter, frontmatter_has_id_key, frontmatter_has_parent_key, implicit_charter,
    parse_charter,
};
use crate::workspace::calendar::ics::parse_ics_file;
use crate::workspace::calendar::plans::collect_plan_files_in;
use crate::workspace::sidecar::{collect_sidecar_actions, hydrate_actions_map, read_sidecar, sidecar_path};
use crate::workspace::manifest::WorkspaceManifest;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use uuid::Uuid;

/// The complete filesystem representation of a workspace.
///
/// Holds all file-layer types ([`MarkdownCharter`] → [`ICSPlan`] / [`SourcedAction`]).
/// Convert to a pure [`DomainModel`] via `From` at the workspace boundary —
/// all file paths and source metadata are stripped in that conversion.
///
/// [`ICSPlan`]: crate::workspace::calendar::ics::ICSPlan
pub struct Workspace {
    pub root: PathBuf,
    /// Durable UUID for this workspace's RDF named graph, read from the
    /// [`WorkspaceManifest`]. `None` for a workspace with no persisted identity.
    pub id: Option<String>,
    /// Display name — used to scope output in multi-workspace contexts.
    pub name: Option<String>,
    /// A random UUID minted once per load, used as the graph identity only when
    /// `id` is absent. Ephemeral by design: distinct per load, never persisted,
    /// and never derived from the root path — a workspace without a durable id
    /// stays queryable, but its graph URI is not stable across sessions.
    ephemeral_id: String,
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
        let manifest = WorkspaceManifest::read(root);
        Ok(Self {
            root: root.to_path_buf(),
            id: manifest.workspace_id,
            name: manifest.workspace_name,
            ephemeral_id: Uuid::now_v7().to_string(),
            charters,
        })
    }

    /// The workspace's graph id: its durable [`id`](Self::id) when persisted,
    /// otherwise the per-load [`ephemeral_id`](Self::ephemeral_id). Never
    /// derived from the root path — see the field docs for why.
    pub fn effective_id(&self) -> String {
        self.id.clone().unwrap_or_else(|| self.ephemeral_id.clone())
    }

    /// The workspace's display name, falling back to its directory name.
    pub fn effective_name(&self) -> String {
        self.name.clone().unwrap_or_else(|| {
            self.root
                .canonicalize()
                .unwrap_or_else(|_| self.root.clone())
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_else(|| "workspace".to_string())
        })
    }
}

/// Load the primary workspace and all additional workspaces listed in `config`.
///
/// Every workspace reads its own `id` and `name` from its co-located
/// [`WorkspaceManifest`] during load — identity is a property of the workspace,
/// not of the (layered) config.
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
    let primary = Workspace::load_with_plans(primary_root, plan_override)?;
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
    let layout = resolve_workspace_layout(root);

    // Replay any journal left by an interrupted save before reading any files.
    // Recovery-to-consistency is loading's obligation (Decision 34); the pure
    // reader below never mutates.
    if layout.charter_root.exists() {
        recover_pending(&layout.charter_root)?;
    }

    let read = read_workspace_with_plans(root, plan_override)?;
    for finding in &read.findings {
        eprintln!("warning: [{}] {}", finding.path.display(), finding.message);
    }
    Ok(read.charters)
}

/// What a pure read of the workspace produced: everything that loaded, plus
/// a [`Finding`] for everything that didn't (or loaded with issues).
pub struct WorkspaceRead {
    pub charters: Vec<MarkdownCharter>,
    pub findings: Vec<Finding>,
}

/// Read the workspace without mutating it and without refusing it.
///
/// The relaxed reader (Decision 34): per-file failures — unreadable files,
/// unparseable documents, corrupt sidecars — become [`Finding`]s alongside
/// whatever did load. Does **not** replay `.pending` journals; callers that
/// want crash recovery use [`load_workspace`]. `Err` is reserved for
/// workspace-level problems (root is not a directory).
pub fn read_workspace(root: &Path) -> Result<WorkspaceRead, WorkspaceError> {
    read_workspace_with_plans(root, None)
}

/// Like [`read_workspace`] but reads plan `.ics` from `plan_override` when
/// given, instead of the workspace's own `plans/` directory.
pub fn read_workspace_with_plans(
    root: &Path,
    plan_override: Option<&Path>,
) -> Result<WorkspaceRead, WorkspaceError> {
    if !root.is_dir() {
        return Err(WorkspaceError::InvalidPath(root.to_path_buf()));
    }

    let layout = resolve_workspace_layout(root);
    let mut findings: Vec<Finding> = Vec::new();

    let mut charters: HashMap<String, MarkdownCharter> = HashMap::new();
    let mut path_for_name: HashMap<String, PathBuf> = HashMap::new();

    // One union of every sidecar's per-action metadata, keyed by UUID. Actions
    // hydrate from this rather than from a single path-derived sidecar, so a
    // sidecar orphaned by a moved or renamed `.actions` file still finds its
    // actions wherever their lines now live.
    let global_actions = collect_sidecar_actions(&layout.charter_root);

    for file_path in discover_action_files(&layout.charter_root)? {
        let relative = file_path
            .strip_prefix(&layout.charter_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let Some(name) =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
        else {
            findings.push(Finding::violation(
                "charter-name-unresolved",
                &relative,
                "failed to infer a charter name from the file path; file skipped",
            ));
            continue;
        };

        let action_source = match std::fs::read_to_string(&file_path) {
            Ok(source) => source,
            Err(e) => {
                findings.push(Finding::violation(
                    "unreadable-file",
                    &relative,
                    format!("could not read file: {e}; file skipped"),
                ));
                continue;
            }
        };
        let parsed_doc = match super::super::parse_document(&action_source) {
            Ok(doc) => doc,
            Err(e) => {
                findings.push(Finding::violation(
                    "unparseable-file",
                    &relative,
                    format!("could not parse file: {e}; file skipped"),
                ));
                continue;
            }
        };

        if !parsed_doc.syntax_errors.is_empty() {
            findings.push(Finding::warning(
                "syntax-errors",
                &relative,
                syntax_error_summary(&parsed_doc),
            ));
        }

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
                    source_metadata: metadata,
                }
            })
            .collect();

        let base: Charter = from_actions_with_charter(
            &sourced.iter().map(|sa| sa.action.clone()).collect::<Vec<_>>(),
            name.clone(),
        );
        let mut mc = MarkdownCharter::from(base);
        mc.actions_file = Some(relative.clone());
        mc.actions = sourced.clone();

        // The sidecar at the conventional path is still checked for corruption,
        // but hydration draws from the workspace-wide union so a sidecar orphaned
        // by a moved or renamed `.actions` file still reaches its actions by UUID.
        let sc_path = layout.charter_root.join(sidecar_path(&relative));
        if let Err(e) = read_sidecar(&sc_path) {
            findings.push(Finding::violation(
                "sidecar-corrupt",
                sidecar_path(&relative),
                format!("could not read sidecar at expected path: {e}"),
            ));
        }
        hydrate_actions_map(&mut sourced, &global_actions);
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

    // Charters whose `.md` frontmatter declares an explicit `id:` — a real
    // identity declaration, never to be superseded by a recorded sidecar id.
    let mut explicit_id_charters: std::collections::HashSet<String> =
        std::collections::HashSet::new();

    for file_path in discover_charter_files(&layout.charter_root)? {
        let relative = file_path
            .strip_prefix(&layout.charter_root)
            .unwrap_or(&file_path)
            .to_path_buf();
        let Some(name) =
            infer_charter_name_for_workspace(&relative, layout.project_root_charter.as_deref())
        else {
            findings.push(Finding::violation(
                "charter-name-unresolved",
                &relative,
                "failed to infer a charter name from the file path; file skipped",
            ));
            continue;
        };

        let content = match std::fs::read_to_string(&file_path) {
            Ok(content) => content,
            Err(e) => {
                findings.push(Finding::violation(
                    "unreadable-file",
                    &relative,
                    format!("could not read file: {e}; file skipped"),
                ));
                continue;
            }
        };
        if frontmatter_has_parent_key(&content) {
            explicit_parent_charters.insert(name.clone());
        }
        if frontmatter_has_id_key(&content) {
            explicit_id_charters.insert(name.clone());
        }
        let explicit = match parse_charter(&content) {
            Ok(charter) => charter,
            Err(e) => {
                findings.push(Finding::violation(
                    "unparseable-file",
                    &relative,
                    format!("could not parse charter frontmatter: {e}; file skipped"),
                ));
                continue;
            }
        };

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

    // Resolve charter identity: an id derived from the name/title (no explicit
    // frontmatter `id:`) yields to a recorded sidecar `charter.id`, so identity
    // survives a rename that would otherwise recompute it. Inert until the write
    // side records ids — an absent id leaves the derived seed untouched.
    for (name, charter) in charters.iter_mut() {
        if explicit_id_charters.contains(name) {
            continue;
        }
        let Some(actions_file) = &charter.actions_file else {
            continue;
        };
        let sc_path = layout.charter_root.join(sidecar_path(actions_file));
        if let Ok(recorded) = read_sidecar(&sc_path).map(|sc| sc.charter.and_then(|c| c.id)) {
            if let Some(id) = recorded {
                charter.id = id;
            }
        }
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

    // Flag parents that can't be resolved to a known charter alias.
    let known_aliases: std::collections::HashSet<&str> = charters
        .values()
        .filter_map(|c| c.alias.as_deref())
        .collect();
    for (name, charter) in &charters {
        if let Some(parent) = &charter.parent {
            if !known_aliases.contains(parent.as_str()) {
                let file = path_for_name
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| PathBuf::from("<unknown>"));
                findings.push(Finding::warning(
                    "unresolvable-parent",
                    file,
                    format!(
                        "charter '{}' has unresolvable parent '{}' — \
                         use the alias (machine key), not the display title",
                        charter.alias.as_deref().unwrap_or(&charter.title),
                        parent
                    ),
                ));
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
        let plans = match parse_ics_file(&entry.path) {
            Ok(plans) => plans,
            Err(e) => {
                findings.push(Finding::violation(
                    "unparseable-file",
                    &entry.relative_path,
                    format!("could not parse ics: {e}; file skipped"),
                ));
                continue;
            }
        };
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

    let mut charters: Vec<MarkdownCharter> = charters_by_name.into_values().collect();
    resolve_predecessor_aliases(&mut charters);

    Ok(WorkspaceRead { charters, findings })
}

/// Resolve alias predecessor references (`<alias`) to UUIDs once the whole
/// workspace is loaded. UUID refs resolve at parse time; alias refs need the
/// workspace-wide alias table, so this is the earliest they can bind.
/// Unresolvable refs stay `None` — doctor reports those as dangling.
fn resolve_predecessor_aliases(charters: &mut [MarkdownCharter]) {
    let alias_to_id: HashMap<String, uuid::Uuid> = charters
        .iter()
        .flat_map(|c| &c.actions)
        .filter_map(|sa| sa.action.alias.as_ref().map(|a| (a.to_lowercase(), sa.action.id)))
        .collect();
    if alias_to_id.is_empty() {
        return;
    }
    for charter in charters.iter_mut() {
        for sa in &mut charter.actions {
            for pred in sa.action.predecessors.iter_mut().flatten() {
                if pred.resolved_uuid.is_none() {
                    pred.resolved_uuid =
                        alias_to_id.get(&pred.raw_ref.trim().to_lowercase()).copied();
                }
            }
        }
    }
}

/// One human-readable summary of a document's recoverable syntax issues,
/// detailing the first few diagnostics. Shared with `doctor`, which makes the
/// same observation about completed archives (outside the loader's scope).
pub(crate) fn syntax_error_summary(doc: &crate::workspace::actions::ParsedDocument) -> String {
    let mut msg = format!(
        "parsed with {} issue(s); loaded {} recoverable action(s)",
        doc.syntax_errors.len(),
        doc.actions.len()
    );
    for diagnostic in doc.syntax_errors.iter().take(5) {
        msg.push_str(&format!(
            "\n  - line {}, col {}: {}",
            diagnostic.range.start_row + 1,
            diagnostic.range.start_col + 1,
            diagnostic.message
        ));
    }
    let remaining = doc.syntax_errors.len().saturating_sub(5);
    if remaining > 0 {
        msg.push_str(&format!("\n  - ... and {} more issue(s)", remaining));
    }
    msg
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
