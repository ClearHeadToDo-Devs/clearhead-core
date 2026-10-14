//! Mode regressions are separate from the durability implementation so a
//! tests-first commit can lock this file without locking production code.
#![cfg(unix)]

use clearhead_cli::filesystem::durability::atomic_write;
use std::os::unix::fs::PermissionsExt;
use std::process::Command;

#[test]
fn rewrite_preserves_existing_mode() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("next.actions");
    for mode in [0o664, 0o640, 0o600, 0o755] {
        std::fs::write(&path, "old").unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(mode)).unwrap();
        atomic_write(&path, "new").unwrap();
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "new");
        assert_eq!(path.metadata().unwrap().permissions().mode() & 0o7777, mode);
    }
}

#[test]
fn new_file_respects_umask() {
    // Set each mask only in a child shell, never in this multithreaded test
    // process. The exec'd helper inherits it just like the CLI would.
    for (mask, expected) in [("002", "664"), ("022", "644"), ("077", "600")] {
        let output = Command::new("sh")
            .arg("-c")
            .arg("umask \"$1\"; exec \"$2\" --exact new_file_child --nocapture")
            .arg("mode-regression")
            .arg(mask)
            .arg(std::env::current_exe().unwrap())
            .env("CLEARHEAD_MODE_TEST_EXPECTED", expected)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "umask {mask}:\n{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn new_file_child() {
    let Ok(expected) = std::env::var("CLEARHEAD_MODE_TEST_EXPECTED") else {
        return;
    };
    let expected = u32::from_str_radix(&expected, 8).unwrap();
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("nested/next.actions");
    atomic_write(&path, "new").unwrap();
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "new");
    assert_eq!(
        path.metadata().unwrap().permissions().mode() & 0o7777,
        expected
    );
}

#[test]
fn metadata_errors_are_not_treated_as_missing_files() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("loop");
    std::os::unix::fs::symlink("loop", &path).unwrap();
    assert!(atomic_write(&path, "must not replace the symlink").is_err());
    assert_eq!(
        std::fs::read_link(&path).unwrap(),
        std::path::Path::new("loop")
    );
}
