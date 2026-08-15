//! One locked, journaled mutation seam.
//!
//! Every durable action mutation — close and archive today, `add`/`update`/
//! `delete` as the durable-verbs charter routes them through core — shares one
//! envelope: acquire the workspace lock, recover any pending intent, run a body
//! that reads trusted state and produces the files to write, then stage and
//! commit them in one [`PendingBatch`]. A body that produces no writes commits
//! nothing.
//!
//! The seam is named for *what it commits* — file writes — not for *why* the
//! caller made them. The three loaded words stay reserved: `Plan` is the
//! schedule entity, `transaction` is the CLI batch surface, `update` is a field
//! edit. This seam is deliberately dumber than all three: it does not know why
//! the files changed, only how to commit them atomically.

use std::path::PathBuf;

use crate::workspace::durability::{PendingBatch, WorkspaceLock, recover_pending};
use crate::workspace::store::{WorkspaceError, WorkspaceLayout};

/// One rendered file to be written atomically as part of a mutation.
#[derive(Debug, Clone)]
pub(crate) struct FileWrite {
    pub(crate) path: PathBuf,
    pub(crate) contents: String,
}

/// The set of file writes a mutation body stages for one atomic commit.
///
/// An empty set means the mutation is a no-op: nothing is committed. This is how
/// an archive with nothing to move, or a close that finds its target already
/// completed, records "resolved successfully, wrote nothing".
#[derive(Debug, Clone, Default)]
pub(crate) struct WriteSet(Vec<FileWrite>);

impl WriteSet {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    /// Stage one rendered file for the commit.
    pub(crate) fn stage(&mut self, path: PathBuf, contents: String) {
        self.0.push(FileWrite { path, contents });
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Run one mutation body under the workspace lock with crash recovery.
///
/// Ordering is load-bearing and matches the hand-written closers this replaces:
/// the charter root is ensured, the lock is acquired, and pending intent is
/// recovered *before* `body` runs — so the body reads trusted, recovered state.
/// `body` is pure with respect to the filesystem: it reads and computes,
/// returning the [`WriteSet`] to commit plus its own outcome value. The seam
/// owns all staging and the single commit. A body returning an empty `WriteSet`
/// commits nothing, so no-op mutations never open a batch.
pub(crate) fn with_locked_mutation<T>(
    layout: &WorkspaceLayout,
    body: impl FnOnce(&WorkspaceLayout) -> Result<(WriteSet, T), WorkspaceError>,
) -> Result<T, WorkspaceError> {
    std::fs::create_dir_all(&layout.charter_root)?;
    let _lock = WorkspaceLock::try_acquire(&layout.data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(layout.data_root.clone()))?;
    recover_pending(&layout.charter_root)?;

    let (writes, outcome) = body(layout)?;

    if !writes.is_empty() {
        let mut batch = PendingBatch::new(layout.charter_root.clone());
        for FileWrite { path, contents } in &writes.0 {
            batch.stage(path.clone(), contents.as_bytes())?;
        }
        batch.commit()?;
    }

    Ok(outcome)
}
