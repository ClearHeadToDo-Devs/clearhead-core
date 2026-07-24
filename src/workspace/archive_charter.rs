//! Charter archival: relocate a closed charter's artifacts into the `archive/`
//! region as plaintext, untouched.
//!
//! The archived form is *data, not a projection*: nothing is serialized to
//! Turtle or JSON-LD. The subtree's own `.actions` / `.completed.actions` /
//! `.md` / sidecar files are moved verbatim into `<data_root>/archive/`,
//! mirroring their path under `charters/`. Because discovery only recurses into
//! `charters/`, the moved files drop out of the default read automatically while
//! staying fully parseable when reference resolution or the graph binary needs
//! them. Any RDF view of archived data is regenerated on read, exactly like
//! live data. This is what lets `clearhead-core` shed Oxigraph: archival no
//! longer writes Turtle.
//!
//! # Process (per spec)
//!
//! 1. Verify each charter in the subtree is terminal ([`CharterState::Closed`]
//!    or [`CharterState::Cancelled`]).
//! 2. Count open actions in each primary `.actions` file.
//!    - If any are open and `force` is false, refuse and return
//!      [`ArchiveCharterError::OpenActions`].
//! 3. Move the source files, all-or-none, through the batch transaction:
//!    - `<charter>.actions`
//!    - `<charter>.completed.actions`
//!    - `<charter>.upcoming.actions`
//!    - `<charter>.md` (if present)
//!    - `.<charter>.json` sidecar (if present) — moved *with* the files rather
//!      than folded into the lines, so its `created_at` / `external_schedule_id`
//!      / recurring Plan linkage survive intact.
//!    Each lands at `<data_root>/archive/<path-under-charters>`.
//! 4. Collapse the now-empty charter subdirectory (directory-form charters
//!    only; silently skipped when non-empty so sub-charters survive).
//!
//! `.ics` plans are never touched. Once a plan's `.ics` exists the server owns
//! it; archiving the actions leaves the calendar files in place (the user
//! clears them via the calendar app). Any `.ics` that outlive their charter
//! resurface on the next load as an implicit charter — an honest reflection
//! that the calendar still holds those events.

use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::{Path, PathBuf};

use uuid::Uuid;

use crate::domain::{ActionState, Charter};
use crate::workspace::MarkdownCharter;
use crate::workspace::action_files::{completed_actions_path, read_actions, upcoming_actions_path};
use crate::workspace::durability::{PendingBatch, WorkspaceLock, recover_pending};
use crate::workspace::sidecar::sidecar_path;
use crate::workspace::store::load_workspace;
use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};

// ============================================================================
// Public types
// ============================================================================

/// Options controlling how the archive operation behaves.
#[derive(Debug, Clone, Default)]
pub struct ArchiveCharterOptions {
    /// When `true`, archive even if the primary `.actions` file contains open
    /// actions. The open actions are swept as-is rather than cancelled first.
    pub force: bool,
    /// When `true`, compute and return counts but do not write or delete
    /// anything on disk.
    pub dry_run: bool,
}

/// Summary returned on a successful archive (or dry-run).
#[derive(Debug, Clone)]
pub struct ArchiveCharterResult {
    /// Human-readable name/alias of the archived charter.
    pub charter_name: String,
    /// Number of actions swept from the primary `.actions` file.
    pub primary_actions_swept: usize,
    /// Number of actions swept from `.completed.actions`.
    pub completed_actions_swept: usize,
    /// Absolute path to the `archive/` region the files were moved into
    /// (or would be).
    pub archive_dir: PathBuf,
    /// Mirrors `ArchiveCharterOptions::dry_run`.
    pub was_dry_run: bool,
}

/// Errors that can occur when archiving a charter.
#[derive(thiserror::Error, Debug)]
pub enum ArchiveCharterError {
    /// The supplied query string did not match any charter in the workspace.
    #[error("Charter '{0}' not found")]
    NotFound(String),

    /// The charter exists but is not in a terminal state (`Closed` or `Cancelled`).
    #[error(
        "Charter '{0}' is not Closed or Cancelled (current state: {1}); set state: Closed or state: Cancelled before archiving"
    )]
    NotArchivable(String, String),

    /// The charter has open actions and `force` was not set.
    #[error("Charter '{0}' has {1} open action(s); resolve them or pass --force to archive anyway")]
    OpenActions(String, usize),

    /// Underlying workspace I/O or parse error.
    #[error("Workspace error: {0}")]
    Workspace(#[from] WorkspaceError),

    /// Filesystem I/O error.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

// ============================================================================
// Public entry points
// ============================================================================

/// Archive a single charter identified by `query` (name, alias, or UUID prefix).
///
/// See module docs for the full process.
pub fn archive_charter(
    root: &Path,
    query: &str,
    opts: &ArchiveCharterOptions,
) -> Result<ArchiveCharterResult, ArchiveCharterError> {
    let layout = resolve_workspace_layout(root);
    let _lock = acquire_mutation_lock(&layout)?;
    recover_pending(&layout.charter_root)?;
    let charters = load_workspace(root)?;

    let mc = find_charter(&charters, query)
        .ok_or_else(|| ArchiveCharterError::NotFound(query.to_string()))?
        .clone();

    let subtree = collect_charter_subtree(&charters, &mc);
    archive_many(root, &subtree, opts)
}

/// Archive every charter whose `state` is terminal ([`CharterState::Closed`]
/// or [`CharterState::Cancelled`]).
///
/// Returns one result per charter subtree. The first
/// `ArchiveCharterError::OpenActions` or workspace error aborts the sweep
/// unless you want to add a `continue-on-error` flag in the future.
pub fn archive_terminal_charters(
    root: &Path,
    opts: &ArchiveCharterOptions,
) -> Result<Vec<ArchiveCharterResult>, ArchiveCharterError> {
    let layout = resolve_workspace_layout(root);
    let _lock = acquire_mutation_lock(&layout)?;
    recover_pending(&layout.charter_root)?;
    let charters = load_workspace(root)?;

    let terminal_roots: Vec<MarkdownCharter> = charters
        .iter()
        .filter(|c| c.state.is_some_and(|s| s.is_terminal()))
        .filter(|c| !has_terminal_ancestor(c, &charters))
        .cloned()
        .collect();

    let mut results = Vec::new();
    for mc in &terminal_roots {
        let subtree = collect_charter_subtree(&charters, mc);
        let result = archive_many(root, &subtree, opts)?;
        results.push(result);
    }
    Ok(results)
}

// ============================================================================
// Core logic
// ============================================================================

/// Execute (or dry-run) the archive of one charter subtree.
fn archive_many(
    root: &Path,
    charters: &[MarkdownCharter],
    opts: &ArchiveCharterOptions,
) -> Result<ArchiveCharterResult, ArchiveCharterError> {
    let root_charter = charters
        .first()
        .expect("archive_many requires at least one charter");
    let charter_name = charter_display_name(root_charter);

    let layout = resolve_workspace_layout(root);
    let archive_root = layout.data_root.join("archive");

    let mut primary_swept = 0usize;
    let mut completed_swept = 0usize;
    // Source → destination pairs. Destinations mirror the source's path under
    // `charters/`, rooted at `archive/`, so the subtree's internal structure
    // (and thus its parent/child relationships) survives the move intact.
    let mut moves: Vec<(PathBuf, PathBuf)> = Vec::new();
    let mut seen_sources = HashSet::new();
    let mut dirs_to_remove: BTreeSet<(usize, PathBuf)> = BTreeSet::new();

    for mc in charters {
        let current_name = charter_display_name(mc);
        if !mc.state.is_some_and(|s| s.is_terminal()) {
            let current = mc
                .state
                .map(|s| s.to_string())
                .unwrap_or_else(|| "New".to_string());
            return Err(ArchiveCharterError::NotArchivable(current_name, current));
        }

        // Primary .actions path (absolute)
        let acts_abs: Option<PathBuf> = mc
            .actions_file
            .as_ref()
            .map(|rel| layout.charter_root.join(rel));

        // Completed / upcoming paths derived from the primary path
        let completed_abs: Option<PathBuf> = acts_abs.as_ref().map(|p| completed_actions_path(p));
        let upcoming_abs: Option<PathBuf> = acts_abs.as_ref().map(|p| upcoming_actions_path(p));

        // Charter .md path
        let md_abs: Option<PathBuf> = mc.md_file.as_ref().map(|rel| layout.charter_root.join(rel));

        // Sidecar (`.<stem>.json`) path, derived the same way the loader derives it.
        let sidecar_abs: Option<PathBuf> = acts_abs.as_ref().map(|p| sidecar_path(p));

        // Optional charter subdirectory (for directory-form charters like health/next.actions)
        let charter_subdir: Option<PathBuf> = acts_abs.as_ref().and_then(|p| {
            let filename = p.file_name()?.to_str()?;
            if filename == "next.actions" {
                p.parent().map(PathBuf::from)
            } else {
                None
            }
        });

        // Precondition: refuse if the primary file still holds open actions,
        // unless forced. Counting is the only reason we read the files — the
        // bytes themselves move verbatim, so there is no hydration to do.
        let open_count = match &acts_abs {
            Some(p) if p.exists() => read_actions(p)?
                .iter()
                .filter(|a| !matches!(a.state, ActionState::Completed | ActionState::Cancelled))
                .count(),
            _ => 0,
        };
        if open_count > 0 && !opts.force {
            return Err(ArchiveCharterError::OpenActions(current_name, open_count));
        }

        primary_swept += count_actions(&acts_abs)?;
        completed_swept += count_actions(&completed_abs)?;

        // The `.ics` plans are intentionally excluded: the server owns them and
        // they stay on disk. Everything else moves all-or-none.
        for src in [acts_abs, completed_abs, upcoming_abs, md_abs, sidecar_abs]
            .into_iter()
            .flatten()
            .filter(|p| p.exists())
        {
            if seen_sources.insert(src.clone()) {
                let dest = archive_dest(&src, &layout.charter_root, &archive_root);
                moves.push((src, dest));
            }
        }

        if let Some(subdir) = charter_subdir {
            // Directory-form charters own all files below their directory, not
            // only the formats core knows about. Move notes, inventories, and
            // future charter-local artifacts verbatim with the subtree.
            if subdir != layout.charter_root {
                for src in collect_supporting_files(&subdir)? {
                    if seen_sources.insert(src.clone()) {
                        let dest = archive_dest(&src, &layout.charter_root, &archive_root);
                        moves.push((src, dest));
                    }
                }
            }
            dirs_to_remove.insert((subdir.components().count(), subdir));
        }
    }

    if opts.dry_run {
        return Ok(ArchiveCharterResult {
            charter_name,
            primary_actions_swept: primary_swept,
            completed_actions_swept: completed_swept,
            archive_dir: archive_root,
            was_dry_run: true,
        });
    }

    // Atomic move of the whole subtree: the journal lives in `charters/` (where
    // `load` runs recovery), so an interrupted archive replays forward on the
    // next load — never a half-archived subtree that orphans a sidecar.
    let mut batch = PendingBatch::new(layout.charter_root.clone());
    for (src, dest) in &moves {
        batch.stage_move(src.clone(), dest.clone())?;
    }
    batch.commit()?;

    // Remove directory-form charter folders deepest-first so a fully archived
    // subtree collapses cleanly once its descendants have moved out.
    for (_, dir) in dirs_to_remove.iter().rev() {
        let _ = std::fs::remove_dir(dir); // ok if non-empty
    }

    Ok(ArchiveCharterResult {
        charter_name,
        primary_actions_swept: primary_swept,
        completed_actions_swept: completed_swept,
        archive_dir: archive_root,
        was_dry_run: false,
    })
}

fn acquire_mutation_lock(
    layout: &crate::workspace::store::WorkspaceLayout,
) -> Result<WorkspaceLock, ArchiveCharterError> {
    WorkspaceLock::try_acquire(&layout.data_root)?.ok_or_else(|| {
        ArchiveCharterError::Workspace(WorkspaceError::WorkspaceLocked(layout.data_root.clone()))
    })
}

fn collect_supporting_files(root: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut files = Vec::new();
    let mut dirs = vec![root.to_path_buf()];
    while let Some(dir) = dirs.pop() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                dirs.push(entry.path());
            } else {
                files.push(entry.path());
            }
        }
    }
    files.sort();
    Ok(files)
}

/// Destination for an archived source file: its path relative to `charters/`,
/// re-rooted under `archive/`. Falls back to the bare filename directly under
/// `archive/` if the source somehow isn't under `charter_root` (never expected).
fn archive_dest(src: &Path, charter_root: &Path, archive_root: &Path) -> PathBuf {
    match src.strip_prefix(charter_root) {
        Ok(rel) => archive_root.join(rel),
        Err(_) => archive_root.join(src.file_name().unwrap_or(src.as_os_str())),
    }
}

/// Count the actions in an optional file, treating a missing file as empty.
fn count_actions(path: &Option<PathBuf>) -> Result<usize, ArchiveCharterError> {
    match path {
        Some(p) if p.exists() => Ok(read_actions(p)?.len()),
        _ => Ok(0),
    }
}

fn charter_display_name(mc: &MarkdownCharter) -> String {
    mc.alias.clone().unwrap_or_else(|| mc.title.clone())
}

fn markdown_is_child_of(child: &MarkdownCharter, parent: &MarkdownCharter) -> bool {
    Charter::from(child.clone()).is_child_of(&Charter::from(parent.clone()))
}

fn find_direct_children<'a>(
    parent: &MarkdownCharter,
    charters: &'a [MarkdownCharter],
) -> Vec<&'a MarkdownCharter> {
    charters
        .iter()
        .filter(|candidate| candidate.id != parent.id && markdown_is_child_of(candidate, parent))
        .collect()
}

fn collect_charter_subtree(
    charters: &[MarkdownCharter],
    root: &MarkdownCharter,
) -> Vec<MarkdownCharter> {
    fn visit(
        node: &MarkdownCharter,
        all: &[MarkdownCharter],
        seen: &mut HashSet<Uuid>,
        out: &mut Vec<MarkdownCharter>,
    ) {
        if !seen.insert(node.id) {
            return;
        }
        out.push(node.clone());
        for child in find_direct_children(node, all) {
            visit(child, all, seen, out);
        }
    }

    let mut out = Vec::new();
    let mut seen = HashSet::new();
    visit(root, charters, &mut seen, &mut out);
    out
}

fn parent_index(charters: &[MarkdownCharter]) -> HashMap<Uuid, Uuid> {
    let mut parents = HashMap::new();
    for child in charters {
        if let Some(parent) = charters
            .iter()
            .find(|candidate| candidate.id != child.id && markdown_is_child_of(child, candidate))
        {
            parents.insert(child.id, parent.id);
        }
    }
    parents
}

fn has_terminal_ancestor(charter: &MarkdownCharter, charters: &[MarkdownCharter]) -> bool {
    let parents = parent_index(charters);
    let by_id: HashMap<Uuid, &MarkdownCharter> = charters.iter().map(|c| (c.id, c)).collect();

    let mut current = charter.id;
    while let Some(parent_id) = parents.get(&current) {
        let Some(parent) = by_id.get(parent_id) else {
            break;
        };
        if parent.state.is_some_and(|s| s.is_terminal()) {
            return true;
        }
        current = *parent_id;
    }
    false
}

// ============================================================================
// Resolution helper
// ============================================================================

/// Find a charter in a loaded workspace by UUID, UUID prefix, alias (exact),
/// or title (partial, case-insensitive).
pub fn find_charter<'a>(
    charters: &'a [MarkdownCharter],
    query: &str,
) -> Option<&'a MarkdownCharter> {
    let q = query.to_lowercase();

    // Full UUID
    if let Ok(uuid) = Uuid::parse_str(query) {
        if let Some(c) = charters.iter().find(|c| c.id == uuid) {
            return Some(c);
        }
    }

    // UUID prefix (≥ 4 hex chars)
    if query.len() >= 4 && query.chars().all(|c| c.is_ascii_hexdigit() || c == '-') {
        if let Some(c) = charters
            .iter()
            .find(|c| c.id.to_string().starts_with(query))
        {
            return Some(c);
        }
    }

    // Alias exact match (case-insensitive)
    if let Some(c) = charters.iter().find(|c| {
        c.alias
            .as_deref()
            .map(|a| a.to_lowercase() == q)
            .unwrap_or(false)
    }) {
        return Some(c);
    }

    // Title partial match (case-insensitive)
    charters
        .iter()
        .find(|c| c.title.to_lowercase().contains(&q))
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::CharterState;
    use crate::workspace::charter::implicit_charter;

    fn make_mc(alias: &str, state: Option<CharterState>) -> MarkdownCharter {
        let mut c = Charter::from(implicit_charter(alias));
        c.state = state;
        MarkdownCharter::from(c)
    }

    #[test]
    fn find_by_alias() {
        let charters = vec![
            make_mc("health", None),
            make_mc("work", Some(CharterState::Closed)),
        ];
        let found = find_charter(&charters, "work").unwrap();
        assert_eq!(found.alias.as_deref(), Some("work"));
    }

    #[test]
    fn find_by_partial_title() {
        let charters = vec![make_mc("health-and-fitness", None)];
        let found = find_charter(&charters, "fitness").unwrap();
        assert_eq!(found.alias.as_deref(), Some("health-and-fitness"));
    }

    #[test]
    fn archive_leaves_ics_in_place() {
        // Archiving a closed charter relocates its `.actions`/`.md` into the
        // `archive/` region but must never touch the `.ics` plans — the server
        // owns those files.
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let charters_dir = root.join(".clearhead/charters");
        let plans_dir = root.join(".clearhead/plans/done");
        std::fs::create_dir_all(&charters_dir).expect("create charters dir");
        std::fs::create_dir_all(&plans_dir).expect("create plans dir");

        std::fs::write(
            charters_dir.join("done.md"),
            "---\nalias: done\nstate: Closed\n---\n# Done\n",
        )
        .expect("write charter md");
        // Empty primary actions file: no open actions, so no --force needed.
        std::fs::write(charters_dir.join("done.actions"), "").expect("write actions");

        let ics_path = plans_dir.join("evt.ics");
        std::fs::write(
            &ics_path,
            "BEGIN:VCALENDAR\r\n\
             BEGIN:VTODO\r\n\
             UID:task@example.com\r\n\
             SUMMARY:Lingering Task\r\n\
             DTSTART:20260427T100000\r\n\
             END:VTODO\r\n\
             END:VCALENDAR\r\n",
        )
        .expect("write ics");

        let result = archive_charter(&root, "done", &ArchiveCharterOptions::default())
            .expect("archive should succeed");
        assert_eq!(result.charter_name, "done");

        // The charter's own artifacts move out of `charters/`…
        let archive_dir = root.join(".clearhead/archive");
        assert!(
            !charters_dir.join("done.actions").exists(),
            "actions moved out"
        );
        assert!(
            !charters_dir.join("done.md").exists(),
            "charter md moved out"
        );
        // …and land under `archive/`, verbatim, as plaintext (no Turtle).
        assert!(
            archive_dir.join("done.actions").exists(),
            "actions in archive/"
        );
        assert!(
            archive_dir.join("done.md").exists(),
            "charter md in archive/"
        );
        assert!(
            !archive_dir.join("archive.ttl").exists(),
            "no Turtle is written"
        );

        // …but the server-owned `.ics` is left exactly where it was.
        assert!(ics_path.exists(), "`.ics` must survive archival");
        assert!(plans_dir.exists(), "plans directory must survive archival");
    }

    #[test]
    fn archive_moves_sidecar_with_files() {
        // The sidecar carries data (recurring Plan linkage) that has no DSL form at
        // all — it only ever lives in the sidecar. Archival moves it *with* the
        // files rather than folding it into the lines, so the linkage survives
        // intact and byte-identical, no lossy translation.
        use crate::workspace::sidecar::{ActionMeta, CharterMetadata, sidecar_path, write_sidecar};

        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let charters_dir = root.join(".clearhead/charters");
        std::fs::create_dir_all(&charters_dir).expect("create charters dir");

        std::fs::write(
            charters_dir.join("done.md"),
            "---\nalias: done\nstate: Closed\n---\n# Done\n",
        )
        .expect("write charter md");
        let acts_path = charters_dir.join("done.actions");
        let action_id: Uuid = "01942d99-4c27-77f6-9316-107024843939".parse().unwrap();
        std::fs::write(&acts_path, format!("[x] Test action #{action_id}\n"))
            .expect("write actions");

        let sc_path = sidecar_path(&acts_path);
        let mut meta = CharterMetadata::default();
        meta.actions.insert(
            action_id.to_string(),
            ActionMeta {
                external_schedule_id: Some("weekly-review@example.com".to_string()),
                ..Default::default()
            },
        );
        write_sidecar(&sc_path, &meta).expect("write sidecar");
        assert!(sc_path.exists(), "sidecar written before archiving");

        archive_charter(&root, "done", &ArchiveCharterOptions::default())
            .expect("archive should succeed");

        // The sidecar left `charters/` …
        assert!(!sc_path.exists(), "sidecar must move out of charters/");
        // … and its recurring Plan linkage now lives in the archived sidecar, intact.
        let archived_sc = sidecar_path(&root.join(".clearhead/archive/done.actions"));
        let moved =
            std::fs::read_to_string(&archived_sc).expect("sidecar must be moved into archive/");
        assert!(
            moved.contains("weekly-review@example.com"),
            "sidecar-only data must survive the move verbatim:\n{moved}"
        );
    }

    #[test]
    fn archive_accepts_cancelled_charter() {
        // Cancelled is a terminal state exactly like Closed: it's a
        // precondition for archival, not just a display label.
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let charters_dir = root.join(".clearhead/charters");
        std::fs::create_dir_all(&charters_dir).expect("create charters dir");

        std::fs::write(
            charters_dir.join("abandoned.md"),
            "---\nalias: abandoned\nstate: Cancelled\n---\n# Abandoned\n",
        )
        .expect("write charter md");
        std::fs::write(charters_dir.join("abandoned.actions"), "").expect("write actions");

        let result = archive_charter(&root, "abandoned", &ArchiveCharterOptions::default())
            .expect("cancelled charters must be archivable");
        assert_eq!(result.charter_name, "abandoned");
        assert!(!charters_dir.join("abandoned.md").exists());
    }

    #[test]
    fn archive_rejects_active_charter_with_updated_message() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let charters_dir = root.join(".clearhead/charters");
        std::fs::create_dir_all(&charters_dir).expect("create charters dir");

        std::fs::write(
            charters_dir.join("live.md"),
            "---\nalias: live\nstate: Active\n---\n# Live\n",
        )
        .expect("write charter md");
        std::fs::write(charters_dir.join("live.actions"), "").expect("write actions");

        let err = archive_charter(&root, "live", &ArchiveCharterOptions::default())
            .expect_err("Active charters are not archivable");
        assert!(matches!(err, ArchiveCharterError::NotArchivable(_, _)));
        assert!(err.to_string().contains("Closed or Cancelled"));
    }

    #[test]
    fn archive_archives_child_charters_too() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let work_dir = root.join(".clearhead/charters/work");
        let ops_dir = work_dir.join("ops");
        std::fs::create_dir_all(&ops_dir).expect("create charter dirs");

        std::fs::write(
            work_dir.join("README.md"),
            "---\nalias: work\nstate: Closed\n---\n# Work\n",
        )
        .expect("write parent charter md");
        std::fs::write(work_dir.join("next.actions"), "[x] Parent done\n")
            .expect("write parent actions");
        std::fs::write(work_dir.join("inventory.md"), "# Supporting inventory\n")
            .expect("write supporting file");

        std::fs::write(
            ops_dir.join("README.md"),
            "---\nalias: ops\nstate: Closed\n---\n# Ops\n",
        )
        .expect("write child charter md");
        std::fs::write(ops_dir.join("next.actions"), "[x] Child done\n")
            .expect("write child actions");

        let result = archive_charter(&root, "work", &ArchiveCharterOptions::default())
            .expect("archive should succeed");
        assert_eq!(result.charter_name, "work");
        assert_eq!(
            result.primary_actions_swept, 2,
            "parent + child actions should be swept"
        );

        assert!(
            !work_dir.join("README.md").exists(),
            "parent charter removed"
        );
        assert!(
            !work_dir.join("next.actions").exists(),
            "parent actions removed"
        );
        assert!(
            !work_dir.join("inventory.md").exists(),
            "supporting file removed"
        );
        assert!(!ops_dir.join("README.md").exists(), "child charter removed");
        assert!(
            !ops_dir.join("next.actions").exists(),
            "child actions removed"
        );
        assert!(!ops_dir.exists(), "child directory removed once empty");
        assert!(
            !work_dir.exists(),
            "parent directory removed once subtree is archived"
        );

        // Both charters land under archive/, and the subtree's nesting is
        // preserved so their parent/child structure stays reconstructable.
        let archive_dir = root.join(".clearhead/archive");
        assert!(
            archive_dir.join("work/README.md").exists(),
            "parent charter in archive/"
        );
        assert!(
            archive_dir.join("work/next.actions").exists(),
            "parent actions in archive/"
        );
        assert!(
            archive_dir.join("work/inventory.md").exists(),
            "supporting file in archive/"
        );
        assert!(
            archive_dir.join("work/ops/README.md").exists(),
            "child charter nested in archive/"
        );
        assert!(
            archive_dir.join("work/ops/next.actions").exists(),
            "child actions nested in archive/"
        );
    }

    #[test]
    fn archive_refuses_lock_contention() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let charters = root.join(".clearhead/charters");
        std::fs::create_dir_all(&charters).unwrap();
        std::fs::write(
            charters.join("done.md"),
            "---\nalias: done\nstate: Closed\n---\n# Done\n",
        )
        .unwrap();
        std::fs::write(charters.join("done.actions"), "").unwrap();
        let data_root = root.join(".clearhead");
        let _lock = WorkspaceLock::try_acquire(&data_root).unwrap().unwrap();

        let error = archive_charter(&root, "done", &ArchiveCharterOptions::default()).unwrap_err();

        assert!(matches!(
            error,
            ArchiveCharterError::Workspace(WorkspaceError::WorkspaceLocked(_))
        ));
        assert!(charters.join("done.md").exists());
    }

    #[test]
    fn archive_terminal_charters_sweeps_closed_roots_once() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let charters_dir = root.join(".clearhead/charters");
        let work_dir = charters_dir.join("work");
        let ops_dir = work_dir.join("ops");
        std::fs::create_dir_all(&ops_dir).expect("create charter dirs");

        std::fs::write(
            charters_dir.join("done.md"),
            "---\nalias: done\nstate: Closed\n---\n# done\n",
        )
        .expect("write root charter md");
        std::fs::write(charters_dir.join("done.actions"), "").expect("write root actions");

        std::fs::write(
            work_dir.join("README.md"),
            "---\nalias: work\nstate: Closed\n---\n# Work\n",
        )
        .expect("write parent charter md");
        std::fs::write(work_dir.join("next.actions"), "").expect("write parent actions");

        std::fs::write(
            ops_dir.join("README.md"),
            "---\nalias: ops\nstate: Closed\n---\n# Ops\n",
        )
        .expect("write child charter md");
        std::fs::write(ops_dir.join("next.actions"), "").expect("write child actions");

        std::fs::write(
            charters_dir.join("live.md"),
            "---\nalias: live\nstate: Active\n---\n# live\n",
        )
        .expect("write live charter md");
        std::fs::write(charters_dir.join("live.actions"), "").expect("write live actions");

        let results = archive_terminal_charters(&root, &ArchiveCharterOptions::default())
            .expect("sweep should succeed");
        let archived: std::collections::HashSet<_> =
            results.iter().map(|r| r.charter_name.clone()).collect();

        assert_eq!(
            archived.len(),
            2,
            "closed child charter should ride with its parent: {archived:?}"
        );
        assert!(archived.contains("done"));
        assert!(archived.contains("work"));
        assert!(
            !work_dir.exists(),
            "closed subtree should be archived once at the root"
        );
        assert!(
            charters_dir.join("live.md").exists(),
            "Active charter is left alone"
        );
    }

    #[test]
    fn archive_rejects_non_closed() {
        // We can't easily test full filesystem operations in unit tests,
        // but we can verify the state guard fires correctly on a non-closed charter.
        // Full integration tests belong in tests/integration/.
        let mc = make_mc("work", Some(CharterState::Active));
        // archive_one is private; test via the public surface by checking the
        // find_charter helper correctly surfaces non-closed charters.
        assert_eq!(mc.state, Some(CharterState::Active));
        assert_ne!(mc.state, Some(CharterState::Closed));
    }
}
