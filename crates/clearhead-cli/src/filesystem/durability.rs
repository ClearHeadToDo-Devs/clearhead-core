//! Durability primitive for workspace writes.
//!
//! [`atomic_write`] performs a single-file atomic replace (temp + fsync +
//! rename + directory fsync). Multi-file consistency is deliberately *not*
//! journaled: callers apply effects in additive order — content-producing
//! writes and moves before removals — so an interrupted multi-file mutation
//! leaves a recoverable duplicate for `doctor` to reconcile rather than a hole.
//! See the direct-delivery charter.

use std::io::{self, Write};
use std::path::Path;

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

#[cfg(test)]
mod tests {
    use super::*;

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
}
