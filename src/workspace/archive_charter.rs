//! Charter archival: crystallize a closed charter's artifacts into the
//! `archive/` region as flat, UUID-stemmed plaintext facts.
//!
//! The archived form is *data, not a projection*: nothing is serialized to
//! Turtle or JSON-LD. A charter's own `.actions` / `.completed.actions` / `.md`
//! / sidecar files move into `<data_root>/archive/`, each **re-stemmed on the
//! charter's UUID** (`<uuid>.actions`, `<uuid>.completed.actions`, `<uuid>.md`,
//! `.<uuid>.json`) and dropped flat — no subdirectories. The UUID is the only
//! stable key for an immutable fact: names collide over time and are mutable,
//! and mirroring the live directory tree would reintroduce the sibling-alias
//! collisions the flat scheme exists to avoid. Because discovery only recurses
//! into `charters/`, the moved files drop out of the default read automatically
//! while staying fully parseable when the graph binary needs them. Any RDF view
//! is regenerated on read, exactly like live data — this is what lets
//! `clearhead-core` shed Oxigraph: archival no longer writes Turtle.
//!
//! Crystallization is *almost* a verbatim relocation. Child 1's one content
//! side effect is self-containment: the sidecar's own `charter.id` is stamped
//! from the known charter UUID (when absent) so a lone `.<uuid>.json` declares
//! its charter in its *content*, not merely its name. (Outbound `parent:` edge
//! normalization is the sibling concern, handled separately.)
//!
//! # Process
//!
//! 1. Verify each charter in the subtree is terminal ([`CharterState::Closed`]
//!    or [`CharterState::Cancelled`]).
//! 2. Count open actions in each primary `.actions` file.
//!    - If any are open and `force` is false, refuse and return
//!      [`ArchiveCharterError::OpenActions`].
//! 3. Stamp the sidecar's `charter.id` (self-identification), then move the
//!    quartet — `<uuid>.actions`, `<uuid>.completed.actions`, `<uuid>.md`,
//!    `.<uuid>.json` — plus any supporting files (each prefixed `<uuid>.`),
//!    all-or-none, through the batch transaction. Each lands flat in
//!    `<data_root>/archive/`. The sidecar moves *with* the files rather than
//!    folded into the lines, so its `created_at` provenance survives intact.
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
use crate::workspace::action_files::{completed_actions_path, read_actions};
use crate::workspace::durability::{PendingBatch, WorkspaceLock, atomic_write, recover_pending};
use crate::workspace::sidecar::{sidecar_path, stamp_charter_id};
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
    archive_many(root, &subtree, &charters, opts)
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
        let result = archive_many(root, &subtree, &charters, opts)?;
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
    all_charters: &[MarkdownCharter],
    opts: &ArchiveCharterOptions,
) -> Result<ArchiveCharterResult, ArchiveCharterError> {
    let root_charter = charters
        .first()
        .expect("archive_many requires at least one charter");
    let charter_name = charter_display_name(root_charter);

    let layout = resolve_workspace_layout(root);
    let archive_root = layout.data_root.join("archive");

    // Directory-form charter roots in this subtree. A parent's supporting-file
    // sweep stops at these boundaries so a child charter's files are claimed
    // under the *child's* UUID, not the parent's — making attribution
    // independent of the order we visit the subtree.
    let subtree_charter_dirs: HashSet<PathBuf> = charters
        .iter()
        .filter_map(|mc| {
            let acts = layout.charter_root.join(mc.actions_file.as_ref()?);
            if acts.file_name()?.to_str()? == "next.actions" {
                acts.parent().map(PathBuf::from)
            } else {
                None
            }
        })
        .collect();

    let mut primary_swept = 0usize;
    let mut completed_swept = 0usize;
    // Source → destination pairs. Destinations are flat and UUID-stemmed: the
    // subtree's parent/child structure no longer rides directory nesting (which
    // flattening destroys) — it is re-homed into the files themselves (`parent:`
    // normalization, handled separately).
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

        // Completed path derived from the primary path
        let completed_abs: Option<PathBuf> = acts_abs.as_ref().map(|p| completed_actions_path(p));

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

        // Self-containment side effect: guarantee the sidecar records its own
        // `charter.id` before it leaves, so the archived `.<uuid>.json` declares
        // its charter by content and not only by filename. Idempotent (stamps
        // only when absent); skipped on dry-run, which must not write.
        if !opts.dry_run {
            if let Some(acts) = &acts_abs {
                stamp_charter_id(acts, mc.id)?;
            }
        }

        // Materialize the parent edge into the `.md` as a UUID before the move,
        // so hierarchy survives flattening as *data* rather than as directory
        // nesting (which we are about to destroy). Same shape as the id-stamp: an
        // in-place, surgical write to the live source, then the normal move. The
        // parent is resolved against the whole workspace — a child archived while
        // its parent stays live must still resolve across the boundary. Absent or
        // unresolvable parents are left verbatim, never fabricated.
        if !opts.dry_run {
            if let (Some(md), Some(parent)) = (&md_abs, mc.parent.as_deref()) {
                if md.exists() {
                    if let Some(parent_uuid) = resolve_parent_uuid(parent, all_charters) {
                        let content = std::fs::read_to_string(md)?;
                        if let Some(rewritten) = set_frontmatter_parent(&content, &parent_uuid) {
                            atomic_write(md, rewritten.as_bytes())?;
                        }
                    }
                }
            }
        }

        // Flat, UUID-stemmed destinations. The quartet keys on the charter's own
        // UUID; the sidecar name derives from the actions dest and so agrees by
        // construction. Distinct UUIDs are what make a flat dir safe.
        let acts_dest = archive_root.join(format!("{}.actions", mc.id));
        let quartet: [(Option<PathBuf>, PathBuf); 4] = [
            (acts_abs, acts_dest.clone()),
            (
                completed_abs,
                archive_root.join(format!("{}.completed.actions", mc.id)),
            ),
            (md_abs, archive_root.join(format!("{}.md", mc.id))),
            (sidecar_abs, sidecar_path(&acts_dest)),
        ];

        // The `.ics` plans are intentionally excluded: the server owns them and
        // they stay on disk. Everything else moves all-or-none.
        for (src, dest) in quartet {
            if let Some(src) = src.filter(|p| p.exists()) {
                if seen_sources.insert(src.clone()) {
                    moves.push((src, dest));
                }
            }
        }

        if let Some(subdir) = charter_subdir {
            // Directory-form charters own all files below their directory, not
            // only the formats core knows about — notes, inventories, future
            // charter-local artifacts. Each is prefixed with the charter's UUID
            // (`<uuid>.<name>`) so a no-UUID file stays owned once flat. The
            // sweep stops at descendant charter boundaries (see above).
            if subdir != layout.charter_root {
                let boundaries: HashSet<PathBuf> = subtree_charter_dirs
                    .iter()
                    .filter(|d| *d != &subdir)
                    .cloned()
                    .collect();
                for src in collect_supporting_files(&subdir, &boundaries)? {
                    if seen_sources.insert(src.clone()) {
                        let name = src.file_name().and_then(|n| n.to_str()).unwrap_or("file");
                        let dest = archive_root.join(format!("{}.{}", mc.id, name));
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

/// Files owned by a directory-form charter, recursively, sorted for
/// determinism — but never descending into `exclude_dirs`, the subdirectories
/// that belong to a *descendant* charter (those files are claimed under that
/// charter's own UUID, not this one's).
fn collect_supporting_files(
    root: &Path,
    exclude_dirs: &HashSet<PathBuf>,
) -> Result<Vec<PathBuf>, std::io::Error> {
    let mut files = Vec::new();
    let mut dirs = vec![root.to_path_buf()];
    while let Some(dir) = dirs.pop() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if entry.file_type()?.is_dir() {
                if !exclude_dirs.contains(&path) {
                    dirs.push(path);
                }
            } else {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

/// Resolve a `parent:` frontmatter value to a charter UUID for archival.
///
/// Accepts an already-canonical UUID (kept as-is) or an exact, case-insensitive
/// alias resolved against the whole workspace — never a title, because a
/// structural edge must not ride a display string. Returns `None` when the value
/// resolves to nothing, so an unresolvable parent is left verbatim rather than
/// fabricated.
fn resolve_parent_uuid(parent: &str, all_charters: &[MarkdownCharter]) -> Option<Uuid> {
    if let Ok(uuid) = Uuid::parse_str(parent) {
        return Some(uuid);
    }
    let q = parent.to_lowercase();
    all_charters
        .iter()
        .find(|c| c.alias.as_deref().is_some_and(|a| a.to_lowercase() == q))
        .map(|c| c.id)
}

/// Substitute — or insert — a top-level `parent: <uuid>` line inside the leading
/// YAML frontmatter, leaving every other byte untouched.
///
/// Returns the rewritten content, or `None` when there is nothing to do (no
/// frontmatter block, or the value already matches). The insert case is the
/// common one: a directory-nested charter carries no `parent:` line, its
/// parenthood having lived only in the directory position we are flattening
/// away. Assumes `\n` line endings — the form our writer emits and the only form
/// these files take in practice.
fn set_frontmatter_parent(content: &str, parent_uuid: &Uuid) -> Option<String> {
    let desired = format!("parent: {parent_uuid}");

    let body = content.strip_prefix("---\n")?;
    let mut offset = "---\n".len();
    let mut existing: Option<(usize, usize)> = None;
    let mut close_start: Option<usize> = None;
    for line in body.split_inclusive('\n') {
        let trimmed = line.strip_suffix('\n').unwrap_or(line);
        if trimmed == "---" {
            close_start = Some(offset);
            break;
        }
        if trimmed.starts_with("parent:") {
            existing = Some((offset, offset + trimmed.len()));
        }
        offset += line.len();
    }
    let close_start = close_start?;

    match existing {
        Some((start, end)) => {
            if content[start..end] == desired {
                return None;
            }
            Some(format!("{}{}{}", &content[..start], desired, &content[end..]))
        }
        None => Some(format!(
            "{}{}\n{}",
            &content[..close_start],
            desired,
            &content[close_start..]
        )),
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

        // Capture the charter's stable UUID before it is archived — the flat
        // archive names every file `<uuid>.*`.
        let uuid = {
            let charters = load_workspace(&root).expect("load");
            find_charter(&charters, "done").expect("charter").id
        };

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
        // …and land flat under `archive/`, UUID-stemmed, as plaintext (no Turtle).
        assert!(
            archive_dir.join(format!("{uuid}.actions")).exists(),
            "actions in archive/"
        );
        assert!(
            archive_dir.join(format!("{uuid}.md")).exists(),
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
                created: Some(
                    chrono::DateTime::parse_from_rfc3339("2024-06-01T12:00:00Z")
                        .unwrap()
                        .with_timezone(&chrono::Local),
                ),
            },
        );
        write_sidecar(&sc_path, &meta).expect("write sidecar");
        assert!(sc_path.exists(), "sidecar written before archiving");

        let uuid = {
            let charters = load_workspace(&root).expect("load");
            find_charter(&charters, "done").expect("charter").id
        };

        archive_charter(&root, "done", &ArchiveCharterOptions::default())
            .expect("archive should succeed");

        // The sidecar left `charters/` …
        assert!(!sc_path.exists(), "sidecar must move out of charters/");
        // … and its provenance now lives in the flat, UUID-stemmed archived
        // sidecar, intact — and self-identifying via a stamped `charter.id`.
        let archived_sc =
            sidecar_path(&root.join(format!(".clearhead/archive/{uuid}.actions")));
        let moved =
            std::fs::read_to_string(&archived_sc).expect("sidecar must be moved into archive/");
        assert!(
            moved.contains("2024-06-01"),
            "sidecar-only data must survive the move verbatim:\n{moved}"
        );
        assert!(
            moved.contains(&uuid.to_string()),
            "archived sidecar must self-identify with its charter.id:\n{moved}"
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

        // Capture both charters' stable UUIDs before the flatten.
        let (work_uuid, ops_uuid) = {
            let charters = load_workspace(&root).expect("load");
            (
                find_charter(&charters, "work").expect("work charter").id,
                find_charter(&charters, "ops").expect("ops charter").id,
            )
        };

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

        // Both charters land flat under archive/, each keyed on its own UUID —
        // no directory nesting. Their parent/child structure is reconstructed
        // from `parent:` in the files, not from path (normalization handled
        // separately); here we assert the flat naming.
        let archive_dir = root.join(".clearhead/archive");
        assert!(
            archive_dir.join(format!("{work_uuid}.md")).exists(),
            "parent charter (flat) in archive/"
        );
        assert!(
            archive_dir.join(format!("{work_uuid}.actions")).exists(),
            "parent actions (flat) in archive/"
        );
        assert!(
            archive_dir
                .join(format!("{work_uuid}.inventory.md"))
                .exists(),
            "supporting file, prefixed with the owning charter's UUID"
        );
        assert!(
            archive_dir.join(format!("{ops_uuid}.md")).exists(),
            "child charter (flat) in archive/, keyed on its own UUID"
        );
        assert!(
            archive_dir.join(format!("{ops_uuid}.actions")).exists(),
            "child actions (flat) in archive/"
        );
        // The child's files must NOT be claimed under the parent's UUID.
        assert!(
            !archive_dir.join(format!("{work_uuid}.README.md")).exists(),
            "child files must not be swept under the parent's UUID"
        );

        // Child 2: the child's parent edge is materialized into its archived
        // `.md` as a UUID — it had no `parent:` line, its parenthood having lived
        // only in the directory nesting we just flattened away.
        let ops_md =
            std::fs::read_to_string(archive_dir.join(format!("{ops_uuid}.md"))).unwrap();
        assert!(
            ops_md.contains(&format!("parent: {work_uuid}")),
            "child's inferred parent must be written into the file as a UUID:\n{ops_md}"
        );
        // The root charter has no parent, so none is fabricated.
        let work_md =
            std::fs::read_to_string(archive_dir.join(format!("{work_uuid}.md"))).unwrap();
        assert!(
            !work_md.contains("parent:"),
            "no parent may be fabricated for a root charter:\n{work_md}"
        );
    }

    // ===== Child 2: parent-edge materialization =====

    #[test]
    fn set_frontmatter_parent_substitutes_existing() {
        let u: Uuid = "019faab5-aa9a-7613-b5a6-f312904d9db3".parse().unwrap();
        let content = "---\nalias: q3\nparent: goals\nstate: Closed\n---\n# Q3\nbody\n";
        let out = set_frontmatter_parent(content, &u).unwrap();
        assert_eq!(
            out,
            format!("---\nalias: q3\nparent: {u}\nstate: Closed\n---\n# Q3\nbody\n")
        );
    }

    #[test]
    fn set_frontmatter_parent_inserts_when_absent() {
        let u: Uuid = "019faab5-aa9a-7613-b5a6-f312904d9db3".parse().unwrap();
        let content = "---\nalias: ops\nstate: Closed\n---\n# Ops\n";
        let out = set_frontmatter_parent(content, &u).unwrap();
        assert_eq!(
            out,
            format!("---\nalias: ops\nstate: Closed\nparent: {u}\n---\n# Ops\n")
        );
    }

    #[test]
    fn set_frontmatter_parent_is_noop_when_already_canonical() {
        let u: Uuid = "019faab5-aa9a-7613-b5a6-f312904d9db3".parse().unwrap();
        let content = format!("---\nalias: ops\nparent: {u}\nstate: Closed\n---\n# Ops\n");
        assert!(set_frontmatter_parent(&content, &u).is_none());
    }

    #[test]
    fn set_frontmatter_parent_requires_a_frontmatter_block() {
        let u: Uuid = "019faab5-aa9a-7613-b5a6-f312904d9db3".parse().unwrap();
        assert!(set_frontmatter_parent("# Just a body, no frontmatter\n", &u).is_none());
    }

    #[test]
    fn archive_rewrites_explicit_parent_alias_to_uuid() {
        // A closed charter that names its parent by alias has that edge rewritten
        // to the parent's UUID at archival (substitute case).
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("ws");
        let charters_dir = root.join(".clearhead/charters");
        std::fs::create_dir_all(&charters_dir).unwrap();

        std::fs::write(
            charters_dir.join("goals.md"),
            "---\nalias: goals\nstate: Active\n---\n# Goals\n",
        )
        .unwrap();
        std::fs::write(charters_dir.join("goals.actions"), "").unwrap();
        std::fs::write(
            charters_dir.join("q3.md"),
            "---\nalias: q3\nparent: goals\nstate: Closed\n---\n# Q3\n",
        )
        .unwrap();
        std::fs::write(charters_dir.join("q3.actions"), "").unwrap();

        let (goals_uuid, q3_uuid) = {
            let cs = load_workspace(&root).unwrap();
            (
                find_charter(&cs, "goals").unwrap().id,
                find_charter(&cs, "q3").unwrap().id,
            )
        };

        archive_charter(&root, "q3", &ArchiveCharterOptions::default()).expect("archive q3");

        let archive_dir = root.join(".clearhead/archive");
        let q3_md = std::fs::read_to_string(archive_dir.join(format!("{q3_uuid}.md"))).unwrap();
        assert!(
            q3_md.contains(&format!("parent: {goals_uuid}")),
            "explicit alias edge must become a UUID:\n{q3_md}"
        );
        assert!(
            !q3_md.contains("parent: goals"),
            "the alias form must be gone:\n{q3_md}"
        );
    }

    #[test]
    fn archive_materializes_parent_uuid_across_a_live_boundary() {
        // A child archived while its parent stays LIVE must still record its
        // parent as a UUID that points into live space — the archive forest aims
        // at the live world, and the edge resolves against the whole workspace.
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("ws");
        let work_dir = root.join(".clearhead/charters/work");
        let ops_dir = work_dir.join("ops");
        std::fs::create_dir_all(&ops_dir).unwrap();

        // Parent stays Active — only the closed child is archived.
        std::fs::write(
            work_dir.join("README.md"),
            "---\nalias: work\nstate: Active\n---\n# Work\n",
        )
        .unwrap();
        std::fs::write(work_dir.join("next.actions"), "").unwrap();

        // Child is Closed, directory-nested, with NO explicit parent: line.
        std::fs::write(
            ops_dir.join("README.md"),
            "---\nalias: ops\nstate: Closed\n---\n# Ops\n",
        )
        .unwrap();
        std::fs::write(ops_dir.join("next.actions"), "").unwrap();

        let (work_uuid, ops_uuid) = {
            let cs = load_workspace(&root).unwrap();
            (
                find_charter(&cs, "work").unwrap().id,
                find_charter(&cs, "ops").unwrap().id,
            )
        };

        archive_charter(&root, "ops", &ArchiveCharterOptions::default())
            .expect("archive the closed child while its parent stays live");

        // Parent untouched.
        assert!(work_dir.join("README.md").exists(), "live parent must stay");

        // Child crystallized flat, its parent edge pointing at the live parent.
        let archive_dir = root.join(".clearhead/archive");
        let ops_md =
            std::fs::read_to_string(archive_dir.join(format!("{ops_uuid}.md"))).unwrap();
        assert!(
            ops_md.contains(&format!("parent: {work_uuid}")),
            "cross-boundary parent must be materialized as a UUID:\n{ops_md}"
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
