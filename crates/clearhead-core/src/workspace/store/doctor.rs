//! Pure workspace diagnosis over host-supplied doctor evidence.
//!
//! Core decides what observations mean and which repairs are safe. Native file
//! discovery, byte reads, clocks, locking, and repair execution belong to the
//! workspace adapter.

use super::findings::{Finding, FindingSeverity};
use super::load::{WorkspaceRead, syntax_error_summary};
use crate::domain::{Action, ActionState};
use crate::workspace::charter::MarkdownCharter;
use crate::workspace::manifest::WorkspaceManifest;
use crate::workspace::resource::{ResourceLocation, ResourceRevision, WorkspacePath};
use crate::workspace::sidecar::{CharterMetadata, parse_sidecar};
use chrono::{DateTime, Local};
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use uuid::Uuid;

/// One host-observed text resource. Read failure is evidence, not an I/O error
/// leaked into Core.
#[derive(Clone, Debug)]
pub struct DoctorDocument {
    pub path: WorkspacePath,
    pub bytes: Result<Vec<u8>, String>,
    pub revision: ResourceRevision,
}

/// One sidecar and whether its path-derived companion action file exists.
#[derive(Clone, Debug)]
pub struct DoctorSidecarEvidence {
    pub document: DoctorDocument,
    pub companion_exists: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DurabilityResidueKind {
    PendingJournal,
    OrphanedTemp,
}

#[derive(Clone, Debug)]
pub struct DurabilityResidue {
    pub location: ResourceLocation,
    pub kind: DurabilityResidueKind,
}

/// Opaque revision evidence for one observed calendar collection.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DoctorCollectionEvidence {
    pub location: ResourceLocation,
    pub revision: ResourceRevision,
}

/// Host-neutral observations required by doctor beyond normal workspace
/// assembly.
#[derive(Clone, Debug)]
pub struct DoctorEvidence {
    pub manifest: WorkspaceManifest,
    pub completed_actions: Vec<DoctorDocument>,
    pub archived_actions: Vec<DoctorDocument>,
    pub sidecars: Vec<DoctorSidecarEvidence>,
    pub plan_collections: Vec<DoctorCollectionEvidence>,
    pub durability_residue: Vec<DurabilityResidue>,
    pub observed_at: DateTime<Local>,
}

/// A repair Core has proven safe from the supplied evidence. Adapters execute
/// these typed instructions; shells never scrape human finding messages.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DoctorRepair {
    PruneSidecarEntry {
        path: WorkspacePath,
        id: String,
        expected: ResourceRevision,
    },
    RemoveSidecar {
        path: WorkspacePath,
        expected: ResourceRevision,
    },
    RemovePlansCollection {
        location: ResourceLocation,
        expected: ResourceRevision,
    },
}

/// Everything a doctor run concluded. Nothing was changed to produce it.
#[derive(Debug, Serialize)]
pub struct Diagnosis {
    /// All findings, most severe first, then by path.
    pub findings: Vec<Finding>,
    pub checked_charters: usize,
    /// Open actions plus actions in completed archives.
    pub checked_actions: usize,
    /// Typed, host-neutral repairs omitted from the stable JSON report.
    #[serde(skip)]
    pub repairs: Vec<DoctorRepair>,
}

impl Diagnosis {
    pub fn violations(&self) -> usize {
        self.count(FindingSeverity::Violation)
    }

    pub fn warnings(&self) -> usize {
        self.count(FindingSeverity::Warning)
    }

    fn count(&self, severity: FindingSeverity) -> usize {
        self.findings
            .iter()
            .filter(|f| f.severity == severity)
            .count()
    }
}

/// Run every coherence check over a workspace read and immutable native
/// observations. This function performs no I/O and is suitable for any host.
pub fn diagnose(read: &WorkspaceRead, evidence: &DoctorEvidence) -> Diagnosis {
    let mut findings = read.findings.clone();
    let mut repairs = Vec::new();

    check_workspace_identity(&evidence.manifest, &mut findings);
    let completed = collect_completed_actions(&evidence.completed_actions, &mut findings);
    let archived = collect_archived_action_states(&evidence.archived_actions, &mut findings);
    let charters = &read.charters;

    check_duplicate_uuids(charters, &completed, &mut findings);
    check_dangling_predecessors(charters, &completed, &archived, &mut findings);
    check_charter_alias_collisions(charters, &mut findings);
    check_open_actions_under_unresolved_parents(charters, &mut findings);
    let known_action_ids = collect_known_action_ids(charters, &completed);
    let has_quarantined_source = findings.iter().any(|f| f.code == "syntax-errors");
    if !has_quarantined_source {
        check_sidecar_coherence(
            &evidence.sidecars,
            &known_action_ids,
            &mut findings,
            &mut repairs,
        );
        check_orphaned_sidecars(
            &evidence.sidecars,
            &known_action_ids,
            &mut findings,
            &mut repairs,
        );
    }
    check_sidecar_created_sanity(&evidence.sidecars, evidence.observed_at, &mut findings);
    check_durability_residue(&evidence.durability_residue, &mut findings);

    repairs.extend(findings.iter().filter_map(|finding| {
        if finding.code != "unowned-plans-collection" {
            return None;
        }
        let path = WorkspacePath::new(path_text(&finding.path)).ok()?;
        let location = ResourceLocation::new(finding.mount, path);
        evidence
            .plan_collections
            .iter()
            .find(|collection| collection.location == location)
            .map(|collection| DoctorRepair::RemovePlansCollection {
                location,
                expected: collection.revision.clone(),
            })
    }));

    findings.sort_by(|a, b| {
        b.severity
            .cmp(&a.severity)
            .then_with(|| a.mount.cmp(&b.mount))
            .then_with(|| a.path.cmp(&b.path))
            .then_with(|| a.code.cmp(&b.code))
    });

    let open_actions: usize = charters.iter().map(|c| c.actions.len()).sum();
    let completed_actions: usize = completed.values().map(Vec::len).sum();
    Diagnosis {
        findings,
        checked_charters: charters.len(),
        checked_actions: open_actions + completed_actions,
        repairs,
    }
}

fn check_workspace_identity(manifest: &WorkspaceManifest, findings: &mut Vec<Finding>) {
    if manifest.workspace_id.is_none() {
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
    documents: &[DoctorDocument],
    findings: &mut Vec<Finding>,
) -> HashMap<PathBuf, Vec<Action>> {
    let mut completed = HashMap::new();
    for document in documents {
        let relative = logical_path_buf(&document.path);
        let source = match document_text(document) {
            Ok(source) => source,
            Err(error) => {
                findings.push(Finding::violation(
                    "unreadable-file",
                    &relative,
                    format!("could not read completed archive: {error}"),
                ));
                continue;
            }
        };
        match crate::workspace::parse_document(source) {
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
            Err(error) => findings.push(Finding::violation(
                "unparseable-file",
                &relative,
                format!("could not parse completed archive: {error}"),
            )),
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
                format!(
                    "uuid {} appears {} times: {}",
                    id,
                    files.len(),
                    list.join(", ")
                ),
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
    documents: &[DoctorDocument],
    findings: &mut Vec<Finding>,
) -> HashMap<Uuid, ActionState> {
    let mut states = HashMap::new();
    for document in documents {
        let relative = logical_path_buf(&document.path);
        let source = match document_text(document) {
            Ok(source) => source,
            Err(error) => {
                findings.push(Finding::warning(
                    "unreadable-archive",
                    &relative,
                    format!("could not read archived actions file: {error}"),
                ));
                continue;
            }
        };
        match crate::workspace::parse_document(source) {
            Ok(doc) => {
                for action in doc.actions {
                    states.insert(action.id, action.state);
                }
            }
            Err(error) => findings.push(Finding::warning(
                "unparseable-archive",
                &relative,
                format!("could not parse archived actions: {error}"),
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
    let known: HashSet<Uuid> = all_actions(charters, completed)
        .map(|(_, a)| a.id)
        .collect();
    for charter in charters {
        let Some(file) = &charter.actions_file else {
            continue;
        };
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
    let known_aliases: HashSet<&str> = charters.iter().filter_map(|c| c.alias.as_deref()).collect();

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
            .filter(|sa| {
                !matches!(
                    sa.action.state,
                    ActionState::Completed | ActionState::Cancelled
                )
            })
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
fn collect_known_action_ids(
    charters: &[MarkdownCharter],
    completed: &HashMap<PathBuf, Vec<Action>>,
) -> HashSet<Uuid> {
    let mut known = HashSet::new();
    for charter in charters {
        for sa in &charter.actions {
            known.insert(sa.action.id);
            known.extend(sa.action.plan_id);
        }
    }
    for actions in completed.values() {
        for action in actions {
            known.insert(action.id);
            known.extend(action.plan_id);
        }
    }
    known
}

fn check_sidecar_coherence(
    sidecars: &[DoctorSidecarEvidence],
    known_action_ids: &HashSet<Uuid>,
    findings: &mut Vec<Finding>,
    repairs: &mut Vec<DoctorRepair>,
) {
    for sidecar in sidecars {
        let Some(metadata) = parsed_sidecar(sidecar) else {
            continue;
        };
        let relative = logical_path_buf(&sidecar.document.path);
        for key in metadata.actions.keys() {
            let orphaned = Uuid::parse_str(key)
                .map(|id| !known_action_ids.contains(&id))
                .unwrap_or(true);
            if orphaned {
                let companion = companion_actions_path(&sidecar.document.path);
                findings.push(Finding::warning(
                    "sidecar-orphan",
                    &relative,
                    format!(
                        "entry '{}' matches no action in {} or its completed archive",
                        key, companion
                    ),
                ));
                repairs.push(DoctorRepair::PruneSidecarEntry {
                    path: sidecar.document.path.clone(),
                    id: key.clone(),
                    expected: sidecar.document.revision.clone(),
                });
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
    sidecars: &[DoctorSidecarEvidence],
    observed_at: DateTime<Local>,
    findings: &mut Vec<Finding>,
) {
    let floor = DateTime::parse_from_rfc3339(EARLIEST_PLAUSIBLE_CREATED)
        .expect("EARLIEST_PLAUSIBLE_CREATED is a valid RFC3339 constant")
        .with_timezone(&Local);
    for sidecar in sidecars {
        let Some(metadata) = parsed_sidecar(sidecar) else {
            continue;
        };
        let relative = logical_path_buf(&sidecar.document.path);
        for (key, action) in &metadata.actions {
            let Some(created) = action.created else {
                continue;
            };
            if created > observed_at || created < floor {
                findings.push(Finding::warning(
                    "implausible-created",
                    &relative,
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
fn check_orphaned_sidecars(
    sidecars: &[DoctorSidecarEvidence],
    known_action_ids: &HashSet<Uuid>,
    findings: &mut Vec<Finding>,
    repairs: &mut Vec<DoctorRepair>,
) {
    for sidecar in sidecars.iter().filter(|sidecar| !sidecar.companion_exists) {
        let retains_live_metadata = parsed_sidecar(sidecar).is_some_and(|metadata| {
            metadata.actions.keys().any(|key| {
                Uuid::parse_str(key)
                    .ok()
                    .is_some_and(|id| known_action_ids.contains(&id))
            })
        });
        if retains_live_metadata {
            continue;
        }
        let relative = logical_path_buf(&sidecar.document.path);
        findings.push(Finding::warning(
            "orphaned-sidecar",
            &relative,
            format!(
                "sidecar has no matching {} file",
                companion_actions_path(&sidecar.document.path)
            ),
        ));
        repairs.push(DoctorRepair::RemoveSidecar {
            path: sidecar.document.path.clone(),
            expected: sidecar.document.revision.clone(),
        });
    }
}

/// Crash residue: a `.pending` journal (reported, never replayed — that is
/// loading's job) and orphaned `.tmp.*` staging files nothing ever sweeps.
fn check_durability_residue(residue: &[DurabilityResidue], findings: &mut Vec<Finding>) {
    for item in residue {
        let (code, message) = match item.kind {
            DurabilityResidueKind::PendingJournal => (
                "pending-journal",
                "interrupted write batch; the next loading command will replay it (doctor does not)",
            ),
            DurabilityResidueKind::OrphanedTemp => (
                "orphaned-temp",
                "staging file from an interrupted write; safe to delete once no clearhead process is running",
            ),
        };
        findings.push(Finding::warning_at(
            item.location.mount,
            code,
            logical_path_buf(&item.location.path),
            message,
        ));
    }
}

/// Every action with the file it lives in: open actions from loaded charters,
/// closed ones from their completed archives.
fn all_actions<'a>(
    charters: &'a [MarkdownCharter],
    completed: &'a HashMap<PathBuf, Vec<Action>>,
) -> impl Iterator<Item = (&'a PathBuf, &'a Action)> {
    let open = charters
        .iter()
        .filter_map(|c| c.actions_file.as_ref().map(|f| (f, c)))
        .flat_map(|(file, charter)| charter.actions.iter().map(move |sa| (file, &sa.action)));
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

fn parsed_sidecar(evidence: &DoctorSidecarEvidence) -> Option<CharterMetadata> {
    document_text(&evidence.document)
        .ok()
        .and_then(|source| parse_sidecar(source).ok())
}

fn document_text(document: &DoctorDocument) -> Result<&str, String> {
    let bytes = document.bytes.as_ref().map_err(Clone::clone)?;
    std::str::from_utf8(bytes).map_err(|error| error.to_string())
}

fn companion_actions_path(path: &WorkspacePath) -> String {
    let (parent, filename) = path
        .as_str()
        .rsplit_once('/')
        .map_or(("", path.as_str()), |(parent, filename)| (parent, filename));
    let stem = filename
        .strip_prefix('.')
        .and_then(|name| name.strip_suffix(".json"))
        .unwrap_or(filename);
    if parent.is_empty() {
        format!("{stem}.actions")
    } else {
        format!("{parent}/{stem}.actions")
    }
}

fn logical_path_buf(path: &WorkspacePath) -> PathBuf {
    PathBuf::from(path.as_str())
}

fn path_text(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}
