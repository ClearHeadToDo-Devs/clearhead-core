//! Durability primitives for workspace writes.
//!
//! Three levels of protection:
//!
//! - [`atomic_write`] — single-file: temp + fsync + rename + dir fsync.
//! - [`PendingBatch`] — multi-file: stage all writes to temps, write a
//!   `.pending` journal, rename in order, fsync, unlink journal. A present
//!   `.pending` on startup means an interrupted batch; [`recover_pending`]
//!   replays it forward to completion.
//! - [`WorkspaceLock`] — advisory PID lock that serializes concurrent
//!   ClearHead writers. Best-effort: acquiring it can fail without data loss.

use std::io::{self, Write};
use std::path::{Path, PathBuf};

// ============================================================================
// Atomic single-file write
// ============================================================================

/// Write `content` to `path` atomically: temp file in the same directory,
/// fsync, rename, then fsync the directory.
///
/// Parent directories are created if they don't exist. On any error the
/// original file (if it existed) is untouched.
pub fn atomic_write(path: &Path, content: impl AsRef<[u8]>) -> io::Result<()> {
    let dir = path.parent().unwrap_or(Path::new("."));
    if !dir.as_os_str().is_empty() {
        std::fs::create_dir_all(dir)?;
    }

    let mut tmp = tempfile::NamedTempFile::new_in(dir)?;
    tmp.write_all(content.as_ref())?;
    tmp.flush()?;
    tmp.as_file().sync_all()?;

    // persist() is an atomic rename; on failure the NamedTempFile is returned
    // so it can be cleaned up via Drop.
    tmp.persist(path).map_err(|e| e.error)?;

    std::fs::File::open(dir)?.sync_all()?;
    Ok(())
}

// ============================================================================
// Staged batch commit
// ============================================================================

struct BatchEntry {
    /// The file to rename into `final_path` on commit. For [`stage`] this is a
    /// freshly-written temp; for [`stage_move`] it is an existing source file
    /// being relocated. Either way, commit renames `source_path -> final_path`
    /// and recovery replays that same rename.
    ///
    /// [`stage`]: PendingBatch::stage
    /// [`stage_move`]: PendingBatch::stage_move
    source_path: PathBuf,
    final_path: PathBuf,
}

/// Staged multi-file commit.
///
/// Usage:
/// 1. Call [`stage`](PendingBatch::stage) for each file to write, and/or
///    [`stage_move`](PendingBatch::stage_move) for each existing file to relocate.
/// 2. Call [`commit`](PendingBatch::commit) to write the journal, rename all
///    sources to their finals, and unlink the journal.
///
/// The `journal_dir` receives the `.pending` file; all staged files should live
/// on the same filesystem so renames are atomic.
pub struct PendingBatch {
    journal_dir: PathBuf,
    entries: Vec<BatchEntry>,
}

impl PendingBatch {
    pub fn new(journal_dir: PathBuf) -> Self {
        Self { journal_dir, entries: Vec::new() }
    }

    /// Write `content` to a temp file and record it as a pending rename to
    /// `final_path`. The temp is fsynced immediately.
    pub fn stage(&mut self, final_path: PathBuf, content: &[u8]) -> io::Result<()> {
        let dir = final_path.parent().unwrap_or(&self.journal_dir);
        std::fs::create_dir_all(dir)?;

        let tmp_path = dir.join(format!(".tmp.{}", uuid::Uuid::now_v7()));
        let mut f = std::fs::File::create(&tmp_path)?;
        f.write_all(content)?;
        f.sync_all()?;

        self.entries.push(BatchEntry { source_path: tmp_path, final_path });
        Ok(())
    }

    /// Record an *existing* file for relocation to `final_path` on commit.
    ///
    /// Unlike [`stage`](PendingBatch::stage), nothing is copied: `src` is
    /// itself the file that gets renamed into place, so this is a true move,
    /// not a copy-then-delete. `src` is assumed already durable on disk (it
    /// was written by an earlier operation), so no content fsync is needed;
    /// only the destination directory is created ahead of the rename.
    ///
    /// A crash between journal-write and completion leaves `src` in place, and
    /// [`recover_pending`] replays the rename — identical semantics to a
    /// staged write, because both reduce to "if the source still exists,
    /// rename it to its final path."
    pub fn stage_move(&mut self, src: PathBuf, final_path: PathBuf) -> io::Result<()> {
        if let Some(parent) = final_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        self.entries.push(BatchEntry { source_path: src, final_path });
        Ok(())
    }

    /// Commit the batch: write journal, rename sources, fsync dir, unlink journal.
    ///
    /// A no-op if no files were staged.
    pub fn commit(self) -> io::Result<()> {
        if self.entries.is_empty() {
            return Ok(());
        }

        // Write the journal before any rename so recovery can replay if we crash.
        let journal_path = self.journal_dir.join(".pending");
        {
            let journal_content: String = self
                .entries
                .iter()
                .map(|e| {
                    format!("{}\t{}\n", e.source_path.display(), e.final_path.display())
                })
                .collect();
            let mut jf = std::fs::File::create(&journal_path)?;
            jf.write_all(journal_content.as_bytes())?;
            jf.sync_all()?;
        }
        std::fs::File::open(&self.journal_dir)?.sync_all()?;

        // Rename in a fixed order (deterministic, matches recovery order).
        for entry in &self.entries {
            std::fs::rename(&entry.source_path, &entry.final_path)?;
        }

        std::fs::File::open(&self.journal_dir)?.sync_all()?;
        std::fs::remove_file(&journal_path)?;
        Ok(())
    }
}

// ============================================================================
// Pending-journal recovery
// ============================================================================

/// Replay an interrupted batch, if one exists.
///
/// If `journal_dir/.pending` is present, re-executes the recorded renames
/// (skipping any whose temp file is already gone — they completed before the
/// crash) and then removes the journal. Safe to call on every startup; a
/// missing journal is a no-op.
pub fn recover_pending(journal_dir: &Path) -> io::Result<()> {
    let journal_path = journal_dir.join(".pending");
    if !journal_path.exists() {
        return Ok(());
    }

    let content = std::fs::read_to_string(&journal_path)?;
    for line in content.lines() {
        let mut parts = line.splitn(2, '\t');
        let (Some(tmp_str), Some(final_str)) = (parts.next(), parts.next()) else {
            continue;
        };
        let tmp_path = Path::new(tmp_str);
        let final_path = Path::new(final_str);
        if tmp_path.exists() {
            std::fs::rename(tmp_path, final_path)?;
        }
    }

    std::fs::remove_file(&journal_path)?;
    Ok(())
}

// ============================================================================
// Advisory workspace lock
// ============================================================================

/// Advisory PID-based workspace lock.
///
/// Acquired via [`WorkspaceLock::try_acquire`]; released on `Drop`.
/// If acquisition fails (another ClearHead process holds the lock), callers
/// should decide whether to wait/retry or proceed without exclusion.
pub struct WorkspaceLock {
    path: PathBuf,
}

impl WorkspaceLock {
    /// Try to acquire the lock. Returns `None` if already held.
    pub fn try_acquire(data_root: &Path) -> io::Result<Option<Self>> {
        let path = data_root.join(".clearhead.lock");
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        {
            Ok(mut f) => {
                write!(f, "{}", std::process::id())?;
                Ok(Some(Self { path }))
            }
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => Ok(None),
            Err(e) => Err(e),
        }
    }
}

impl Drop for WorkspaceLock {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // ── atomic_write ─────────────────────────────────────────────────────────

    #[test]
    fn atomic_write_creates_file() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.actions");
        atomic_write(&path, b"[ ] hello\n").unwrap();
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "[ ] hello\n");
    }

    #[test]
    fn atomic_write_overwrites_existing() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.actions");
        std::fs::write(&path, "old content").unwrap();
        atomic_write(&path, b"new content").unwrap();
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "new content");
    }

    #[test]
    fn atomic_write_creates_parent_dirs() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("sub/dir/test.actions");
        atomic_write(&path, b"content").unwrap();
        assert!(path.exists());
    }

    // ── PendingBatch happy path ───────────────────────────────────────────────

    #[test]
    fn batch_commits_all_staged_files() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();

        let mut batch = PendingBatch::new(root.clone());
        batch.stage(root.join("a.actions"), b"[ ] A\n").unwrap();
        batch.stage(root.join("b.actions"), b"[ ] B\n").unwrap();
        batch.commit().unwrap();

        assert_eq!(std::fs::read_to_string(root.join("a.actions")).unwrap(), "[ ] A\n");
        assert_eq!(std::fs::read_to_string(root.join("b.actions")).unwrap(), "[ ] B\n");
        assert!(!root.join(".pending").exists());
    }

    #[test]
    fn empty_batch_commit_is_noop() {
        let dir = tempfile::tempdir().unwrap();
        PendingBatch::new(dir.path().to_path_buf()).commit().unwrap();
        assert!(!dir.path().join(".pending").exists());
    }

    // ── PendingBatch::stage_move ──────────────────────────────────────────────

    /// `stage_move` relocates an existing file (no copy), creating the
    /// destination's parent directory as needed.
    #[test]
    fn stage_move_relocates_existing_file() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        let src = root.join("charters/done.actions");
        std::fs::create_dir_all(src.parent().unwrap()).unwrap();
        std::fs::write(&src, "[x] done\n").unwrap();

        let dest = root.join("archive/done.actions");
        let mut batch = PendingBatch::new(root.join("charters"));
        batch.stage_move(src.clone(), dest.clone()).unwrap();
        batch.commit().unwrap();

        assert!(!src.exists(), "source is moved, not copied");
        assert_eq!(std::fs::read_to_string(&dest).unwrap(), "[x] done\n");
        assert!(!root.join("charters/.pending").exists());
    }

    /// A crash after the journal but before the move completes: recovery
    /// replays the rename, because a staged move reduces to the same
    /// "if the source still exists, rename it" contract as a staged write.
    #[test]
    fn stage_move_recovers_after_crash() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        let src = root.join("charters/done.actions");
        let dest = root.join("archive/done.actions");
        std::fs::create_dir_all(src.parent().unwrap()).unwrap();
        std::fs::create_dir_all(dest.parent().unwrap()).unwrap();
        std::fs::write(&src, "[x] done\n").unwrap();

        // Journal written (source still in place), then "crash" before rename.
        let journal = format!("{}\t{}\n", src.display(), dest.display());
        std::fs::write(root.join(".pending"), &journal).unwrap();

        recover_pending(root).unwrap();

        assert!(!src.exists(), "recovery completes the move");
        assert_eq!(std::fs::read_to_string(&dest).unwrap(), "[x] done\n");
        assert!(!root.join(".pending").exists());
    }

    // ── recover_pending ───────────────────────────────────────────────────────

    /// Crash before journal → prior state intact (no journal, no renames).
    #[test]
    fn crash_before_journal_leaves_prior_state() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();

        // Prior content exists.
        std::fs::write(root.join("a.actions"), "old A").unwrap();

        // "Crash": a temp was written but no journal was recorded.
        std::fs::write(root.join(".tmp.orphan"), "new A").unwrap();

        // Recovery finds no journal — no-op.
        recover_pending(root).unwrap();

        // Prior content untouched.
        assert_eq!(std::fs::read_to_string(root.join("a.actions")).unwrap(), "old A");
    }

    /// The A+B seam: action file (A) and sidecar (B) staged together.
    /// Crash before the journal is written — neither file advances, prior content survives.
    #[test]
    fn ab_crash_before_journal_leaves_both_files_at_prior_state() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();

        // Prior state: action file and sidecar both exist.
        std::fs::write(root.join("health.actions"), "[ ] old action\n").unwrap();
        std::fs::write(root.join(".health.json"), r#"{"actions":{}}"#).unwrap();

        // Temps written (staged), but process dies before the journal is recorded.
        std::fs::write(root.join(".tmp.action"), "[ ] new action\n").unwrap();
        std::fs::write(root.join(".tmp.sidecar"), r#"{"actions":{"id":"stamp"}}"#).unwrap();

        // No journal → recovery is a no-op.
        recover_pending(root).unwrap();

        // Both files remain at prior content — workspace is coherent.
        assert_eq!(
            std::fs::read_to_string(root.join("health.actions")).unwrap(),
            "[ ] old action\n"
        );
        assert_eq!(
            std::fs::read_to_string(root.join(".health.json")).unwrap(),
            r#"{"actions":{}}"#
        );
    }

    /// Crash after journal but before all renames → recovery completes the batch.
    #[test]
    fn crash_after_journal_recovery_completes_batch() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();

        // Prior content for both files.
        std::fs::write(root.join("a.actions"), "old A").unwrap();
        std::fs::write(root.join("b.actions"), "old B").unwrap();

        // Simulate: temps written, journal written.
        let tmp_a = root.join(".tmp.aaa");
        let tmp_b = root.join(".tmp.bbb");
        std::fs::write(&tmp_a, "new A").unwrap();
        std::fs::write(&tmp_b, "new B").unwrap();

        let journal = format!(
            "{}\t{}\n{}\t{}\n",
            tmp_a.display(), root.join("a.actions").display(),
            tmp_b.display(), root.join("b.actions").display(),
        );
        std::fs::write(root.join(".pending"), &journal).unwrap();

        // Simulate: A was renamed before the crash; B's temp still exists.
        std::fs::rename(&tmp_a, root.join("a.actions")).unwrap();

        // Recovery replays remaining renames.
        recover_pending(root).unwrap();

        assert_eq!(std::fs::read_to_string(root.join("a.actions")).unwrap(), "new A");
        assert_eq!(std::fs::read_to_string(root.join("b.actions")).unwrap(), "new B");
        assert!(!root.join(".pending").exists());
    }

    #[test]
    fn recover_pending_noop_when_no_journal() {
        let dir = tempfile::tempdir().unwrap();
        recover_pending(dir.path()).unwrap();
    }

    // ── WorkspaceLock ─────────────────────────────────────────────────────────

    #[test]
    fn lock_acquire_and_release() {
        let dir = tempfile::tempdir().unwrap();
        let lock_path = dir.path().join(".clearhead.lock");

        let lock = WorkspaceLock::try_acquire(dir.path()).unwrap();
        assert!(lock.is_some());
        assert!(lock_path.exists());

        drop(lock);
        assert!(!lock_path.exists());
    }

    #[test]
    fn second_acquire_returns_none_while_held() {
        let dir = tempfile::tempdir().unwrap();
        let _lock = WorkspaceLock::try_acquire(dir.path()).unwrap();
        let second = WorkspaceLock::try_acquire(dir.path()).unwrap();
        assert!(second.is_none());
    }
}
