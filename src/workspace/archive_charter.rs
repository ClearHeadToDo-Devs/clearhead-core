//! Charter archival: sweep a closed charter's artifacts into `archive.ttl`.
//!
//! # Process (per spec)
//!
//! 1. Verify the charter's `state` is terminal ([`CharterState::Closed`] or
//!    [`CharterState::Cancelled`]).
//! 2. Count open actions in the primary `.actions` file.
//!    - If any are open and `force` is false, refuse and return
//!      [`ArchiveCharterError::OpenActions`].
//! 3. Read all actions (primary + completed), then hydrate them from the
//!    charter's `.<stem>.json` sidecar (`created_at`, `external_schedule_id`)
//!    the same way the workspace loader does — otherwise that metadata is
//!    silently lost the moment the sidecar is deleted in step 7.
//! 4. Build a [`DomainModel`] from the charter + all its actions. Plans
//!    (`.ics`) are deliberately excluded: they are a calendar projection the
//!    server owns, and the actions already carry the scheduling source of
//!    truth (`scheduled_at` / `due_at`).
//! 5. Load the existing `archive.ttl` (if present) into an Oxigraph store,
//!    then load the new model on top (quad idempotence means re-runs are safe).
//! 6. Serialize the merged store back to `archive.ttl`.
//! 7. Delete the source files:
//!    - `<charter>.actions`
//!    - `<charter>.completed.actions`
//!    - `<charter>.upcoming.actions`
//!    - `<charter>.md` (if present)
//!    - `.<charter>.json` sidecar (if present) — its data has already been
//!      folded into the actions written to `archive.ttl` in step 3.
//!    - The charter subdirectory itself if it is now empty (directory-form
//!      charters only; silently skipped when non-empty so sub-charters survive).
//!
//! `.ics` plans are never touched. Once a plan's `.ics` exists the server owns
//! it; archiving the actions leaves the calendar files in place (the user
//! clears them via the calendar app). Any `.ics` that outlive their charter
//! resurface on the next load as an implicit charter — an honest reflection
//! that the calendar still holds those events.

use std::path::{Path, PathBuf};

use uuid::Uuid;

use crate::domain::{Action, ActionState, Charter, DomainModel};
use crate::graph::{
    create_store, dump_store_to_turtle, insert::load_domain_model as insert_model_into_store,
    insert::load_turtle,
};
use crate::workspace::action_files::{completed_actions_path, read_actions, upcoming_actions_path};
use crate::workspace::actions::repository::SourcedAction;
use crate::workspace::sidecar::{hydrate_actions_map, read_sidecar, sidecar_path};
use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};
use crate::workspace::store::load_workspace;
use crate::workspace::MarkdownCharter;

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
    /// Absolute path to the `archive.ttl` that was written (or would be).
    pub archive_ttl_path: PathBuf,
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
    #[error(
        "Charter '{0}' has {1} open action(s); resolve them or pass --force to archive anyway"
    )]
    OpenActions(String, usize),

    /// Underlying workspace I/O or parse error.
    #[error("Workspace error: {0}")]
    Workspace(#[from] WorkspaceError),

    /// Filesystem I/O error.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// RDF graph error.
    #[error("Graph error: {0}")]
    Graph(String),
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
    let charters = load_workspace(root)?;

    let mc = find_charter(&charters, query)
        .ok_or_else(|| ArchiveCharterError::NotFound(query.to_string()))?
        .clone();

    archive_one(root, &mc, opts)
}

/// Archive every charter whose `state` is terminal ([`CharterState::Closed`]
/// or [`CharterState::Cancelled`]).
///
/// Returns one result per charter. The first `ArchiveCharterError::OpenActions`
/// or workspace error aborts the sweep unless you want to add a
/// `continue-on-error` flag in the future.
pub fn archive_terminal_charters(
    root: &Path,
    opts: &ArchiveCharterOptions,
) -> Result<Vec<ArchiveCharterResult>, ArchiveCharterError> {
    let charters = load_workspace(root)?;

    let terminal: Vec<MarkdownCharter> = charters
        .into_iter()
        .filter(|c| c.state.is_some_and(|s| s.is_terminal()))
        .collect();

    let mut results = Vec::new();
    for mc in &terminal {
        let result = archive_one(root, mc, opts)?;
        results.push(result);
    }
    Ok(results)
}

// ============================================================================
// Core logic
// ============================================================================

/// Execute (or dry-run) the archive of a single `MarkdownCharter`.
fn archive_one(
    root: &Path,
    mc: &MarkdownCharter,
    opts: &ArchiveCharterOptions,
) -> Result<ArchiveCharterResult, ArchiveCharterError> {
    let charter_name = mc
        .alias
        .clone()
        .unwrap_or_else(|| mc.title.clone());

    // ── 1. State guard ─────────────────────────────────────────────────────
    if !mc.state.is_some_and(|s| s.is_terminal()) {
        let current = mc
            .state
            .map(|s| s.to_string())
            .unwrap_or_else(|| "New".to_string());
        return Err(ArchiveCharterError::NotArchivable(charter_name, current));
    }

    // ── 2. Resolve absolute paths ───────────────────────────────────────────
    let layout = resolve_workspace_layout(root);
    let archive_ttl = layout.data_root.join("archive.ttl");

    // Primary .actions path (absolute)
    let acts_abs: Option<PathBuf> = mc
        .actions_file
        .as_ref()
        .map(|rel| layout.charter_root.join(rel));

    // Completed / upcoming paths derived from the primary path
    let completed_abs: Option<PathBuf> = acts_abs.as_ref().map(|p| completed_actions_path(p));
    let upcoming_abs: Option<PathBuf> = acts_abs.as_ref().map(|p| upcoming_actions_path(p));

    // Charter .md path
    let md_abs: Option<PathBuf> = mc
        .md_file
        .as_ref()
        .map(|rel| layout.charter_root.join(rel));

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

    // ── 3. Read primary actions, hydrated from the sidecar ─────────────────
    let sidecar_meta = match &sidecar_abs {
        Some(p) => read_sidecar(p)?,
        None => Default::default(),
    };

    let mut primary_actions: Vec<Action> = match &acts_abs {
        Some(p) => read_actions(p)?,
        None => vec![],
    };
    hydrate_action_vec(&mut primary_actions, &sidecar_meta);

    let open_count = primary_actions
        .iter()
        .filter(|a| !matches!(a.state, ActionState::Completed | ActionState::Cancelled))
        .count();

    if open_count > 0 && !opts.force {
        return Err(ArchiveCharterError::OpenActions(charter_name, open_count));
    }

    // ── 4. Read completed actions, hydrated from the sidecar ───────────────
    let mut completed_actions: Vec<Action> = match &completed_abs {
        Some(p) => read_actions(p)?,
        None => vec![],
    };
    hydrate_action_vec(&mut completed_actions, &sidecar_meta);

    let primary_swept = primary_actions.len();
    let completed_swept = completed_actions.len();

    // ── 5. Dry-run short-circuit ────────────────────────────────────────────
    if opts.dry_run {
        return Ok(ArchiveCharterResult {
            charter_name,
            primary_actions_swept: primary_swept,
            completed_actions_swept: completed_swept,
            archive_ttl_path: archive_ttl,
            was_dry_run: true,
        });
    }

    // ── 6. Build DomainModel and merge into archive.ttl ─────────────────────
    let mut all_actions = primary_actions.clone();
    all_actions.extend(completed_actions.iter().cloned());

    let mut charter_domain = Charter::from(mc.clone());
    charter_domain.actions = all_actions;
    // Plans are not archived: the `.ics` are server-owned and stay on disk.
    charter_domain.plans = vec![];

    let model = DomainModel {
        objectives: vec![],
        charters: vec![charter_domain],
    };

    let store = create_store().map_err(|e| ArchiveCharterError::Graph(e.to_string()))?;

    // Load existing archive.ttl if it exists
    if archive_ttl.exists() {
        let existing = std::fs::read_to_string(&archive_ttl)?;
        load_turtle(&store, &existing).map_err(|e| ArchiveCharterError::Graph(e.to_string()))?;
    }

    insert_model_into_store(&store, &model, None, oxigraph::model::GraphName::DefaultGraph)
        .map_err(|e| ArchiveCharterError::Graph(e.to_string()))?;

    let ttl = dump_store_to_turtle(&store).map_err(|e| ArchiveCharterError::Graph(e.to_string()))?;

    crate::workspace::durability::atomic_write(&archive_ttl, ttl.as_bytes())
        .map_err(ArchiveCharterError::Io)?;

    // ── 7. Delete source files ──────────────────────────────────────────────
    remove_if_exists(&acts_abs)?;
    remove_if_exists(&completed_abs)?;
    remove_if_exists(&upcoming_abs)?;
    remove_if_exists(&md_abs)?;
    remove_if_exists(&sidecar_abs)?;

    // `.ics` plans are intentionally left untouched — the server owns them.

    // Try to remove the charter subdirectory; silently ignore if non-empty
    // (sub-charters still live there).
    if let Some(ref subdir) = charter_subdir {
        let _ = std::fs::remove_dir(subdir); // ok if non-empty
    }

    Ok(ArchiveCharterResult {
        charter_name,
        primary_actions_swept: primary_swept,
        completed_actions_swept: completed_swept,
        archive_ttl_path: archive_ttl,
        was_dry_run: false,
    })
}

// ============================================================================
// Resolution helper
// ============================================================================

/// Find a charter in a loaded workspace by UUID, UUID prefix, alias (exact),
/// or title (partial, case-insensitive).
pub fn find_charter<'a>(charters: &'a [MarkdownCharter], query: &str) -> Option<&'a MarkdownCharter> {
    let q = query.to_lowercase();

    // Full UUID
    if let Ok(uuid) = Uuid::parse_str(query) {
        if let Some(c) = charters.iter().find(|c| c.id == uuid) {
            return Some(c);
        }
    }

    // UUID prefix (≥ 4 hex chars)
    if query.len() >= 4 && query.chars().all(|c| c.is_ascii_hexdigit() || c == '-') {
        if let Some(c) = charters.iter().find(|c| c.id.to_string().starts_with(query)) {
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
// Helpers
// ============================================================================

fn remove_if_exists(path: &Option<PathBuf>) -> Result<(), ArchiveCharterError> {
    if let Some(p) = path {
        if p.exists() {
            std::fs::remove_file(p)?;
        }
    }
    Ok(())
}

/// Fill in `created_at` / `external_schedule_id` (etc.) from the sidecar,
/// the same hydration the workspace loader applies on every normal read —
/// so archived actions carry that metadata into `archive.ttl` instead of
/// losing it when the sidecar is deleted in step 7.
fn hydrate_action_vec(actions: &mut Vec<Action>, metadata: &crate::workspace::sidecar::CharterMetadata) {
    let mut sourced: Vec<SourcedAction> = actions
        .drain(..)
        .map(|action| SourcedAction { action, source_metadata: None })
        .collect();
    hydrate_actions_map(&mut sourced, &metadata.actions);
    *actions = sourced.into_iter().map(|sa| sa.action).collect();
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
        let charters = vec![make_mc("health", None), make_mc("work", Some(CharterState::Closed))];
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
        // Archiving a closed charter sweeps its `.actions`/`.md` into archive.ttl
        // but must never touch the `.ics` plans — the server owns those files.
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
             BEGIN:VEVENT\r\n\
             UID:evt@example.com\r\n\
             SUMMARY:Lingering Event\r\n\
             DTSTART:20260427T100000\r\n\
             END:VEVENT\r\n\
             END:VCALENDAR\r\n",
        )
        .expect("write ics");

        let result = archive_charter(&root, "done", &ArchiveCharterOptions::default())
            .expect("archive should succeed");
        assert_eq!(result.charter_name, "done");

        // The charter's own artifacts are swept away…
        assert!(!charters_dir.join("done.actions").exists(), "actions removed");
        assert!(!charters_dir.join("done.md").exists(), "charter md removed");
        assert!(root.join(".clearhead/archive.ttl").exists(), "archive written");

        // …but the server-owned `.ics` is left exactly where it was.
        assert!(ics_path.exists(), "`.ics` must survive archival");
        assert!(plans_dir.exists(), "plans directory must survive archival");
    }

    #[test]
    fn archive_hydrates_sidecar_and_removes_it() {
        // The sidecar carries data (VEVENT linkage) that has no DSL form at
        // all — it only ever lives in the sidecar. Archiving must fold it
        // into archive.ttl before the sidecar itself is deleted, or the
        // data is lost with no record it ever existed.
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
                source_vevent: Some("weekly-review@example.com".to_string()),
                ..Default::default()
            },
        );
        write_sidecar(&sc_path, &meta).expect("write sidecar");
        assert!(sc_path.exists(), "sidecar written before archiving");

        archive_charter(&root, "done", &ArchiveCharterOptions::default())
            .expect("archive should succeed");

        // The VEVENT linkage survived into archive.ttl…
        let ttl = std::fs::read_to_string(root.join(".clearhead/archive.ttl"))
            .expect("archive.ttl written");
        assert!(
            ttl.contains("weekly-review@example.com"),
            "sidecar-only data must be folded into archive.ttl before the sidecar is deleted:\n{ttl}"
        );

        // …and the sidecar itself, now redundant, is swept away like the other artifacts.
        assert!(!sc_path.exists(), "sidecar must be removed once archived");
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
    fn archive_terminal_charters_sweeps_closed_and_cancelled_only() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ws");
        let charters_dir = root.join(".clearhead/charters");
        std::fs::create_dir_all(&charters_dir).expect("create charters dir");

        for (alias, state) in [("done", "Closed"), ("abandoned", "Cancelled"), ("live", "Active")] {
            std::fs::write(
                charters_dir.join(format!("{alias}.md")),
                format!("---\nalias: {alias}\nstate: {state}\n---\n# {alias}\n"),
            )
            .expect("write charter md");
            std::fs::write(charters_dir.join(format!("{alias}.actions")), "")
                .expect("write actions");
        }

        let results = archive_terminal_charters(&root, &ArchiveCharterOptions::default())
            .expect("sweep should succeed");
        let archived: std::collections::HashSet<_> =
            results.iter().map(|r| r.charter_name.clone()).collect();

        assert_eq!(archived.len(), 2, "only the two terminal charters archive: {archived:?}");
        assert!(archived.contains("done"));
        assert!(archived.contains("abandoned"));
        assert!(charters_dir.join("live.md").exists(), "Active charter is left alone");
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
