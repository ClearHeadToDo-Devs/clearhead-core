//! Observe replacement permissions before atomic_write accesses private content.
#![cfg(unix)]

use clearhead_cli::filesystem::durability::atomic_write;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::Command;

struct PrivateContent<'a> {
    destination: &'a Path,
}

impl AsRef<[u8]> for PrivateContent<'_> {
    fn as_ref(&self) -> &[u8] {
        let replacements: Vec<_> = std::fs::read_dir(self.destination.parent().unwrap())
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| path != self.destination)
            .collect();
        assert_eq!(replacements.len(), 1);
        let metadata = replacements[0].metadata().unwrap();
        assert_eq!(metadata.len(), 0, "content must not yet have been written");
        let original_mode = self.destination.metadata().unwrap().permissions().mode();
        let temporary_mode = metadata.permissions().mode();
        assert_eq!(
            temporary_mode & !original_mode & 0o777,
            0,
            "temporary file must not grant permissions absent from the original"
        );
        b"private replacement"
    }
}

#[test]
fn replacement_is_private_before_content_access() {
    // Fix the mask in a child only, without mutating this test process's umask.
    let output = Command::new("sh")
        .arg("-c")
        .arg("umask 022; exec \"$1\" --exact private_replacement_child --nocapture")
        .arg("private-replacement-regression")
        .arg(std::env::current_exe().unwrap())
        .env("CLEARHEAD_PRIVATE_TEMP_CHILD", "1")
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn private_replacement_child() {
    if std::env::var_os("CLEARHEAD_PRIVATE_TEMP_CHILD").is_none() {
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("next.actions");
    std::fs::write(&path, "private original").unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
    atomic_write(&path, PrivateContent { destination: &path }).unwrap();
    assert_eq!(std::fs::read(&path).unwrap(), b"private replacement");
    assert_eq!(path.metadata().unwrap().permissions().mode() & 0o777, 0o600);
}
