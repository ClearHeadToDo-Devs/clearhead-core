//! `doctor` — read-only workspace fsck (trust charter, Decision 34).
//!
//! Cross-file coherence checks over what [`read_workspace_with_plans`] loaded,
//! plus raw filesystem observations the loader never makes. Strictly
//! read-only: it reports and never heals — cleanup belongs to the commands
//! that own each file surface, and journal replay belongs to loading.

use super::findings::{Finding, FindingSeverity};
use super::load::{read_workspace_with_plans, syntax_error_summary};
use super::{WorkspaceError, resolve_workspace_layout};
use crate::domain::{Action, ActionState};
use crate::workspace::action_files::completed_actions_path;
use crate::workspace::charter::MarkdownCharter;
use crate::workspace::sidecar::{read_sidecar, sidecar_path};
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use uuid::Uuid;

/// Everything a doctor run observed. Nothing was changed to produce it.
#[derive(Debug, Serialize)]
pub struct Diagnosis {
    /// All findings, most severe first, then by path.
    pub findings: Vec<Finding>,
    pub checked_charters: usize,
    /// Open actions plus actions in completed archives.
    pub checked_actions: usize,
}

impl Diagnosis {
    pub fn violations(&self) -> usize {
        self.count(FindingSeverity::Violation)
    }

    pub fn warnings(&self) -> usize {
        self.count(FindingSeverity::Warning)
    }

    fn count(&self, severity: FindingSeverity) -> usize {
        self.findings.iter().filter(|f| f.severity == severity).count()
    }
}

/// Run every workspace coherence check and return the combined findings —
/// the loader's per-file findings plus doctor's cross-file ones.
pub fn diagnose(
    root: &Path,
    plan_override: Option<&Path>,
) -> Result<Diagnosis, WorkspaceError> {
    let read = read_workspace_with_plans(root, plan_override)?;
    Ok(diagnose_read(root, &read))
}

/// Like [`diagnose`], but over a workspace the caller already read — for
/// read-only surfaces (e.g. `debug`) that need the workspace *and* its
/// diagnosis without reading twice.
pub fn diagnose_read(
    root: &Path,
    read: &super::load::WorkspaceRead,
) -> Diagnosis {
    let layout = resolve_workspace_layout(root);
    let mut findings = read.findings.clone();

    check_workspace_identity(root, &mut findings);

    // Completed archives are outside the loader's scope but inside the
    // coherence universe: predecessors may point at closed actions, and a
    // crash mid-archive can leave an action in both files.
    let completed = collect_completed_actions(&layout.charter_root, &mut findings);
    // Archived actions (plaintext, in the archive/ region) let predecessors that
    // point into the archive resolve to satisfied/abandoned instead of dangling.
    let archived = collect_archived_action_states(&layout.data_root.join("archive"), &mut findings);
    let charters = &read.charters;

    check_duplicate_uuids(charters, &completed, &mut findings);
    check_dangling_predecessors(charters, &completed, &archived, &mut findings);
    check_charter_alias_collisions(charters, &mut findings);
    check_open_actions_under_unresolved_parents(charters, &mut findings);
    check_sidecar_coherence(&layout.charter_root, charters, &completed, &mut findings);
    check_sidecar_created_sanity(&layout.charter_root, charters, &mut findings);
    check_orphaned_sidecars(&layout.charter_root, &mut findings);
    check_dangling_vevent_links(charters, &mut findings);
    check_charterless_plans(charters, &mut findings);
    check_durability_residue(&layout.charter_root, &layout.plans_root, &mut findings);

    findings.sort_by(|a, b| {
        b.severity
            .cmp(&a.severity)
            .then_with(|| a.path.cmp(&b.path))
            .then_with(|| a.code.cmp(&b.code))
    });

    let open_actions: usize = charters.iter().map(|c| c.actions.len()).sum();
    let completed_actions: usize = completed.values().map(Vec::len).sum();
    Diagnosis {
        findings,
        checked_charters: charters.len(),
        checked_actions: open_actions + completed_actions,
    }
}

/// A workspace with no persisted `workspace_id` still works — queries fall back
/// to an ephemeral per-load identity ([`Workspace::effective_id`]) — but that
/// identity is not stable across sessions, breaking named-graph continuity for
/// archived or shared data. Doctor nudges toward the durable id, reading it from
/// the [`WorkspaceManifest`] (`workspace.json`).
///
/// [`Workspace::effective_id`]: crate::workspace::store::load::Workspace::effective_id
/// [`WorkspaceManifest`]: crate::workspace::manifest::WorkspaceManifest
fn check_workspace_identity(root: &Path, findings: &mut Vec<Finding>) {
    if crate::workspace::manifest::WorkspaceManifest::read(root)
        .workspace_id
        .is_none()
    {
        findings.push(Finding::warning(
            "uninitialized-workspace",
            "workspace.json",
            "workspace has no workspace_id — queries use an ephemeral identity that changes every session; run `clearhead init` to assign a durable one",
        ));
    }
}

/// Parse every `*.completed.actions` archive, keyed by charter-root-relative
/// path. Unparseable archives become findings, like the loader's own files.
fn collect_completed_actions(
    charter_root: &Path,
    findings: &mut Vec<Finding>,
) -> HashMap<PathBuf, Vec<Action>> {
    let mut completed = HashMap::new();
    for path in walk_visible_files(charter_root) {
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        if !name.ends_with(".completed.actions") {
            continue;
        }
        let relative = path.strip_prefix(charter_root).unwrap_or(&path).to_path_buf();
        let source = match std::fs::read_to_string(&path) {
            Ok(source) => source,
            Err(e) => {
                findings.push(Finding::violation(
                    "unreadable-file",
                    &relative,
                    format!("could not read completed archive: {e}"),
                ));
                continue;
            }
        };
        match crate::workspace::parse_document(&source) {
            Ok(doc) => {
                if !doc.syntax_errors.is_empty() {
                    findings.push(Finding::warning(
                        "syntax-errors",
                        &relative,
                        syntax_error_summary(&doc),
                    ));
                }
                completed.insert(relative, doc.actions);
            }
            Err(e) => {
                findings.push(Finding::violation(
                    "unparseable-file",
                    &relative,
                    format!("could not parse completed archive: {e}"),
                ));
            }
        }
    }
    completed
}

/// A UUID appearing more than once across open files and completed archives —
/// copy-pasted lines, or a crash mid-archive leaving both copies.
fn check_duplicate_uuids(
    charters: &[MarkdownCharter],
    completed: &HashMap<PathBuf, Vec<Action>>,
    findings: &mut Vec<Finding>,
) {
    let mut seen: HashMap<Uuid, Vec<PathBuf>> = HashMap::new();
    for (file, action) in all_actions(charters, completed) {
        seen.entry(action.id).or_default().push(file.clone());
    }
    for (id, files) in seen {
        if files.len() > 1 {
            let list: Vec<String> = files.iter().map(|f| f.display().to_string()).collect();
            findings.push(Finding::violation(
                "duplicate-uuid",
                &files[0],
                format!("uuid {} appears {} times: {}", id, files.len(), list.join(", ")),
            ));
        }
    }
}

/// Index every archived action by UUID to its terminal state, reading the
/// `archive/` region as plaintext (no graph). This is what lets a predecessor
/// pointing into the archive resolve to a *state* — satisfied / abandoned —
/// rather than reading as a broken reference. Unparseable archives become
/// warnings, not violations: archived history is lower-stakes than live files.
fn collect_archived_action_states(
    archive_root: &Path,
    findings: &mut Vec<Finding>,
) -> HashMap<Uuid, ActionState> {
    let mut states = HashMap::new();
    if !archive_root.is_dir() {
        return states;
    }
    for path in walk_visible_files(archive_root) {
        let is_actions = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e == "actions")
            .unwrap_or(false);
        if !is_actions {
            continue;
        }
        let relative = path.strip_prefix(archive_root).unwrap_or(&path).to_path_buf();
        let Ok(source) = std::fs::read_to_string(&path) else {
            findings.push(Finding::warning(
                "unreadable-archive",
                &relative,
                "could not read archived actions file",
            ));
            continue;
        };
        match crate::workspace::parse_document(&source) {
            Ok(doc) => {
                for action in doc.actions {
                    states.insert(action.id, action.state);
                }
            }
            Err(e) => findings.push(Finding::warning(
                "unparseable-archive",
                &relative,
                format!("could not parse archived actions: {e}"),
            )),
        }
    }
    states
}

/// A predecessor reference (`<uuid`) resolved against the live workspace and,
/// failing that, the archive. Only UUID-shaped references are judged; unresolved
/// name/alias text is the linter's live-buffer territory. An archived target
/// resolves three ways:
///
/// - **satisfied** — Completed: the dependency was met; not a finding.
/// - **abandoned** — Cancelled (or otherwise non-completed): you depend on
///   something that was dropped; a `warning`.
/// - **dangling** — resolves nowhere, live or archived: a genuine broken
///   reference; a `violation`.
fn check_dangling_predecessors(
    charters: &[MarkdownCharter],
    completed: &HashMap<PathBuf, Vec<Action>>,
    archived: &HashMap<Uuid, ActionState>,
    findings: &mut Vec<Finding>,
) {
    let known: HashSet<Uuid> = all_actions(charters, completed).map(|(_, a)| a.id).collect();
    for charter in charters {
        let Some(file) = &charter.actions_file else { continue };
        for sa in &charter.actions {
            for pred in sa.action.predecessors.iter().flatten() {
                let target = pred
                    .resolved_uuid
                    .or_else(|| Uuid::parse_str(pred.raw_ref.trim()).ok());
                let Some(target) = target else { continue };
                if known.contains(&target) {
                    continue; // live target — resolves normally
                }
                match archived.get(&target) {
                    // satisfied — the dependency was completed before archival.
                    Some(ActionState::Completed) => {}
                    // abandoned — the dependency was cancelled/dropped.
                    Some(_) => findings.push(Finding::warning(
                        "abandoned-predecessor",
                        file,
                        format!(
                            "action '{}' depends on {} which was archived without completing (abandoned)",
                            sa.action.name, target
                        ),
                    )),
                    // dangling — resolves nowhere, live or archived.
                    None => findings.push(Finding::violation(
                        "dangling-predecessor",
                        file,
                        format!(
                            "action '{}' depends on {} which matches no action, open, completed, or archived",
                            sa.action.name, target
                        ),
                    )),
                }
            }
        }
    }
}

/// Two charters claiming the same alias — resolution becomes last-writer-wins.
fn check_charter_alias_collisions(charters: &[MarkdownCharter], findings: &mut Vec<Finding>) {
    let mut by_alias: HashMap<&str, Vec<&MarkdownCharter>> = HashMap::new();
    for charter in charters {
        if let Some(alias) = charter.alias.as_deref() {
            by_alias.entry(alias).or_default().push(charter);
        }
    }
    for (alias, group) in by_alias {
        if group.len() > 1 {
            let titles: Vec<&str> = group.iter().map(|c| c.title.as_str()).collect();
            findings.push(Finding::violation(
                "alias-collision",
                charter_file(group[0]),
                format!(
                    "alias '{}' is claimed by {} charters: {} — references resolve to an arbitrary one",
                    alias,
                    group.len(),
                    titles.join(", ")
                ),
            ));
        }
    }
}

/// Open actions under a charter whose parent cannot be resolved to any loaded
/// charter. The common real-world cause is a parent charter that was archived
/// while a child charter still has live work, so agenda views keep surfacing
/// that work under a now-missing branch.
fn check_open_actions_under_unresolved_parents(
    charters: &[MarkdownCharter],
    findings: &mut Vec<Finding>,
) {
    let known_aliases: HashSet<&str> = charters
        .iter()
        .filter_map(|c| c.alias.as_deref())
        .collect();

    for charter in charters {
        let Some(parent) = charter.parent.as_deref() else {
            continue;
        };
        let nested_under_another_charter = charter
            .actions_file
            .as_ref()
            .map(|path| path.components().count() > 1)
            .unwrap_or(false);
        if !nested_under_another_charter || known_aliases.contains(parent) {
            continue;
        }

        let open_count = charter
            .actions
            .iter()
            .filter(|sa| !matches!(sa.action.state, ActionState::Completed | ActionState::Cancelled))
            .count();
        if open_count == 0 {
            continue;
        }

        findings.push(Finding::warning(
            "archived-parent-open-actions",
            charter_file(charter),
            format!(
                "charter '{}' has {} open action(s) but its parent '{}' is not loaded; this usually means the parent charter was archived or deleted while child work remains open",
                charter.alias.as_deref().unwrap_or(&charter.title),
                open_count,
                parent,
            ),
        ));
    }
}

/// Sidecar entries whose UUID matches no action in the charter file or its
/// completed archive — stale metadata with no owner.
fn check_sidecar_coherence(
    charter_root: &Path,
    charters: &[MarkdownCharter],
    completed: &HashMap<PathBuf, Vec<Action>>,
    findings: &mut Vec<Finding>,
) {
    for charter in charters {
        let Some(actions_file) = &charter.actions_file else { continue };
        let sc_relative = sidecar_path(actions_file);
        let Ok(meta) = read_sidecar(&charter_root.join(&sc_relative)) else {
            continue; // corrupt sidecars are already a loader finding
        };

        // Entries are keyed by action id — or plan id for plan-generated actions.
        let mut allowed: HashSet<Uuid> = HashSet::new();
        for sa in &charter.actions {
            allowed.insert(sa.action.id);
            allowed.extend(sa.action.plan_id);
        }
        for action in completed.get(&completed_actions_path(actions_file)).into_iter().flatten() {
            allowed.insert(action.id);
            allowed.extend(action.plan_id);
        }

        for key in meta.actions.keys() {
            let orphaned = match Uuid::parse_str(key) {
                Ok(id) => !allowed.contains(&id),
                Err(_) => true,
            };
            if orphaned {
                findings.push(Finding::warning(
                    "sidecar-orphan",
                    &sc_relative,
                    format!(
                        "entry '{}' matches no action in {} or its completed archive",
                        key,
                        actions_file.display()
                    ),
                ));
            }
        }
    }
}

/// The earliest plausible `created` timestamp. ClearHead's files-as-truth
/// storage did not exist before this, so anything earlier is a corrupt
/// derivation (a non-v7 id whose random bits decoded as a timestamp), not
/// real history.
const EARLIEST_PLAUSIBLE_CREATED: &str = "2020-01-01T00:00:00Z";

/// Sidecar `created` timestamps outside a sane window — after now, or before
/// ClearHead could have created anything.
///
/// The classic failure is a non-v7 id decoded as if its high bits were a v7
/// timestamp: 12 such entries in the 5081–10143 range were found on
/// 2026-07-10, live in the RDF graph where recency queries surfaced them
/// first. The schema types `created` as a string and lint's W005 future check
/// runs pre-hydration on the DSL, so neither observes the sidecar value —
/// this is the only place the invariant actually runs.
fn check_sidecar_created_sanity(
    charter_root: &Path,
    charters: &[MarkdownCharter],
    findings: &mut Vec<Finding>,
) {
    let now = chrono::Local::now();
    let floor = chrono::DateTime::parse_from_rfc3339(EARLIEST_PLAUSIBLE_CREATED)
        .expect("EARLIEST_PLAUSIBLE_CREATED is a valid RFC3339 constant")
        .with_timezone(&chrono::Local);
    for charter in charters {
        let Some(actions_file) = &charter.actions_file else { continue };
        let sc_relative = sidecar_path(actions_file);
        let Ok(meta) = read_sidecar(&charter_root.join(&sc_relative)) else {
            continue; // corrupt sidecars are already a loader finding
        };
        for (key, action) in &meta.actions {
            let Some(created) = action.created else { continue };
            if created > now || created < floor {
                findings.push(Finding::warning(
                    "implausible-created",
                    &sc_relative,
                    format!(
                        "entry '{}' has created '{}', outside the plausible window (after now, or before {})",
                        key,
                        created.to_rfc3339(),
                        EARLIEST_PLAUSIBLE_CREATED,
                    ),
                ));
            }
        }
    }
}

/// A `.<stem>.json` sidecar whose `<stem>.actions` file is gone entirely.
fn check_orphaned_sidecars(charter_root: &Path, findings: &mut Vec<Finding>) {
    for path in walk_visible_files(charter_root) {
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        let Some(stem) = name.strip_prefix('.').and_then(|n| n.strip_suffix(".json")) else {
            continue;
        };
        let dir = path.parent().unwrap_or(charter_root);
        if !dir.join(format!("{stem}.actions")).exists() {
            let relative = path.strip_prefix(charter_root).unwrap_or(&path);
            findings.push(Finding::warning(
                "orphaned-sidecar",
                relative,
                format!("sidecar has no matching {stem}.actions file"),
            ));
        }
    }
}

/// An action recorded as generated from a VEVENT that no longer exists in
/// `plans/` — the calendar side of the link was deleted or moved.
fn check_dangling_vevent_links(charters: &[MarkdownCharter], findings: &mut Vec<Finding>) {
    let known_uids: HashSet<&str> = charters
        .iter()
        .flat_map(|c| c.plans.iter())
        .filter_map(|p| p.plan.external_id.as_deref())
        .collect();
    for charter in charters {
        let Some(file) = &charter.actions_file else { continue };
        for sa in &charter.actions {
            if let Some(uid) = sa.action.external_schedule_id.as_deref()
                && !known_uids.contains(uid)
            {
                findings.push(Finding::warning(
                    "dangling-vevent-link",
                    file,
                    format!(
                        "action '{}' was generated from VEVENT '{}' which no longer exists in plans/",
                        sa.action.name, uid
                    ),
                ));
            }
        }
    }
}

/// A `plans/<slug>/` directory that matched no charter — the loader invents
/// an implicit charter for it rather than telling anyone (load.rs).
fn check_charterless_plans(charters: &[MarkdownCharter], findings: &mut Vec<Finding>) {
    for charter in charters {
        if !charter.plans.is_empty() && charter.actions_file.is_none() && charter.md_file.is_none() {
            let dir = charter.plans_dir.clone().unwrap_or_else(|| PathBuf::from(&charter.title));
            findings.push(Finding::warning(
                "charterless-plans",
                dir,
                format!(
                    "plans directory matches no charter; an implicit charter '{}' is invented on load",
                    charter.title
                ),
            ));
        }
    }
}

/// Crash residue: a `.pending` journal (reported, never replayed — that is
/// loading's job) and orphaned `.tmp.*` staging files nothing ever sweeps.
fn check_durability_residue(charter_root: &Path, plans_root: &Path, findings: &mut Vec<Finding>) {
    if charter_root.join(".pending").exists() {
        findings.push(Finding::warning(
            "pending-journal",
            ".pending",
            "interrupted write batch; the next loading command will replay it (doctor does not)",
        ));
    }
    for root in [charter_root, plans_root] {
        for path in walk_visible_files(root) {
            if let Some(name) = path.file_name().and_then(|n| n.to_str())
                && name.starts_with(".tmp")
            {
                let relative = path.strip_prefix(root).unwrap_or(&path);
                findings.push(Finding::warning(
                    "orphaned-temp",
                    relative,
                    "staging file from an interrupted write; safe to delete once no clearhead process is running",
                ));
            }
        }
    }
}

/// Every action with the file it lives in: open actions from loaded charters,
/// closed ones from their completed archives.
fn all_actions<'a>(
    charters: &'a [MarkdownCharter],
    completed: &'a HashMap<PathBuf, Vec<Action>>,
) -> impl Iterator<Item = (&'a PathBuf, &'a Action)> {
    let open = charters.iter().filter_map(|c| c.actions_file.as_ref().map(|f| (f, c))).flat_map(
        |(file, charter)| charter.actions.iter().map(move |sa| (file, &sa.action)),
    );
    let closed = completed
        .iter()
        .flat_map(|(file, actions)| actions.iter().map(move |a| (file, a)));
    open.chain(closed)
}

fn charter_file(charter: &MarkdownCharter) -> PathBuf {
    charter
        .actions_file
        .clone()
        .or_else(|| charter.md_file.clone())
        .unwrap_or_else(|| PathBuf::from(&charter.title))
}

/// All files under `dir`, recursively, skipping hidden *directories* (matching
/// discovery) but including hidden files — sidecars and crash residue are the
/// point here.
fn walk_visible_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(current) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&current) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let hidden = path
                    .file_name()
                    .map(|n| n.to_string_lossy().starts_with('.'))
                    .unwrap_or(false);
                if !hidden {
                    stack.push(path);
                }
            } else if path.is_file() {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}
