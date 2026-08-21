//! Guards the write-durability seam (core-seam charter): every write in
//! `src/commands/` should route through core's atomic write primitives
//! (`action_files::write_actions`, `durability::atomic_write`), not raw
//! `std::fs::write` — a crash mid-write can truncate a source-of-truth file.
//! New call sites must either route through those primitives or be added to
//! `EXEMPT` below with a one-line reason, never silently.

use std::fs;
use std::path::Path;

/// (file name, line substring) pairs already reviewed and known not to touch
/// source-of-truth workspace data — user-directed export/print output or
/// one-time bootstrap during `init`.
const EXEMPT: &[(&str, &str)] = &[
    ("init.rs", "fs::write(&gitignore_path"),
    ("init.rs", "fs::write(&config_path, json_str)"),
    ("mod.rs", "fs::write(path, content)"), // write_or_print: user-directed output
    ("plan.rs", "fs::write(output_path, icalendar)"), // export --output: user-directed output
];

#[test]
fn no_naive_fs_write_outside_exemptions() {
    let commands_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/commands");
    let mut violations = Vec::new();
    visit(&commands_dir, &mut violations);
    assert!(
        violations.is_empty(),
        "found fs::write outside the durability seam's exemption list — route through \
         action_files::write_actions / durability::atomic_write, or add a reviewed \
         exemption to EXEMPT in tests/no_naive_writes.rs with a reason:\n{}",
        violations.join("\n")
    );
}

fn visit(dir: &Path, violations: &mut Vec<String>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            visit(&path, violations);
            continue;
        }
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }
        let content = fs::read_to_string(&path).unwrap();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        for (i, line) in content.lines().enumerate() {
            if !(line.contains("fs::write(") || line.contains("std::fs::write(")) {
                continue;
            }
            let exempt = EXEMPT
                .iter()
                .any(|(f, snippet)| *f == file_name && line.contains(snippet));
            if !exempt {
                violations.push(format!("{}:{}: {}", path.display(), i + 1, line.trim()));
            }
        }
    }
}
