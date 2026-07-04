//! Workspace-level findings — plain data describing what a read observed.
//!
//! The file-level analogue is `LintDiagnostic` (code, severity, message,
//! range within one file); a [`Finding`] is the same shape one level up:
//! per-file and cross-file observations made while reading a workspace.
//! Produced by `read_workspace_with_plans` (per-file read failures) and,
//! eventually, `doctor` (cross-file coherence checks). See Decision 34.

use serde::Serialize;
use std::path::PathBuf;

/// How bad a finding is. `Warning` means data loaded but something is off;
/// `Violation` means data was skipped or is incoherent. Ordered so that
/// `Violation > Warning` (doctor sorts most-severe first).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum FindingSeverity {
    Warning,
    Violation,
}

/// One observation about the workspace, tied to the file it concerns.
///
/// `code` is a stable kebab-case slug (e.g. `sidecar-corrupt`,
/// `syntax-errors`) so scripts can match on it; `message` is for humans.
#[derive(Debug, Clone, Serialize)]
pub struct Finding {
    pub code: String,
    pub severity: FindingSeverity,
    /// Path relative to the root the file was discovered under
    /// (charter root for `.actions`/`.md`/sidecars, plans root for `.ics`).
    pub path: PathBuf,
    pub message: String,
}

impl Finding {
    pub fn warning(code: &str, path: impl Into<PathBuf>, message: impl Into<String>) -> Self {
        Self {
            code: code.to_string(),
            severity: FindingSeverity::Warning,
            path: path.into(),
            message: message.into(),
        }
    }

    pub fn violation(code: &str, path: impl Into<PathBuf>, message: impl Into<String>) -> Self {
        Self {
            code: code.to_string(),
            severity: FindingSeverity::Violation,
            path: path.into(),
            message: message.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finding_serializes_with_lowercase_severity() {
        let f = Finding::violation("sidecar-corrupt", ".inbox.json", "bad JSON");
        let json = serde_json::to_value(&f).unwrap();
        assert_eq!(json["severity"], "violation");
        assert_eq!(json["code"], "sidecar-corrupt");
    }
}
