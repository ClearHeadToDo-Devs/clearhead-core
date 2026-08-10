//! Typed read model for archived action facts.
//!
//! Archive storage is deliberately plaintext (`*.completed.actions` plus sidecar
//! JSON), but consumers should not need to learn those file/layout rules. This
//! module is the core-owned adapter from archive files into data.

use std::path::{Path, PathBuf};

use crate::domain::Action;
use crate::workspace::action_files::read_actions;
use crate::workspace::sidecar::{OccurrenceSnapshot, read_sidecar, sidecar_path};
use crate::workspace::store::{WorkspaceError, resolve_workspace_layout};

/// One terminal action loaded from archive storage.
#[derive(Debug, Clone, PartialEq)]
pub struct ArchivedActionFact {
    /// The archived terminal action exactly as represented in plaintext history.
    pub action: Action,
    /// Workspace-root-relative path of the completed actions file that carried it.
    pub source_path: PathBuf,
    /// Frozen recurring occurrence lineage, when this action is the root of a
    /// materialized recurring occurrence archived at crystallization time.
    pub occurrence: Option<OccurrenceSnapshot>,
}

/// Read archived action facts from every completed-actions store core owns.
///
/// This covers both active charter-local `*.completed.actions` files and charter
/// histories that have moved into `.clearhead/archive/*.completed.actions`.
/// Callers get data, not filesystem conventions.
pub fn read_archived_action_facts(root: &Path) -> Result<Vec<ArchivedActionFact>, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    let mut files = Vec::new();
    collect_completed_actions_files(&layout.charter_root, &mut files)?;
    collect_completed_actions_files(&layout.data_root.join("archive"), &mut files)?;
    files.sort();
    files.dedup();

    let mut facts = Vec::new();
    for path in files {
        let metadata = read_sidecar(&sidecar_path(&path))?;
        let source_path = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
        for action in read_actions(&path)? {
            let occurrence = metadata
                .actions
                .get(&action.id.to_string())
                .and_then(|meta| meta.occurrence.clone());
            facts.push(ArchivedActionFact {
                action,
                source_path: source_path.clone(),
                occurrence,
            });
        }
    }
    Ok(facts)
}

fn collect_completed_actions_files(
    dir: &Path,
    out: &mut Vec<PathBuf>,
) -> Result<(), WorkspaceError> {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Ok(());
    };
    for entry in entries {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_completed_actions_files(&path, out)?;
        } else if path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.ends_with(".completed.actions"))
        {
            out.push(path);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::sidecar::{ActionMeta, CharterMetadata, write_sidecar};
    use chrono::TimeZone;
    use uuid::Uuid;

    #[test]
    fn reads_completed_actions_with_occurrence_snapshot() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        let charters = root.join(".clearhead/charters");
        std::fs::create_dir_all(&charters).unwrap();
        let completed = charters.join("health.completed.actions");
        let action_id = Uuid::parse_str("019f733d-45b2-7f21-bcad-5610887b7230").unwrap();
        let plan_id = Uuid::parse_str("019f733d-45c2-7dd2-91dc-8631f33c6b77").unwrap();
        std::fs::write(&completed, format!("[x] Run #{}\n", action_id)).unwrap();

        let snapshot = OccurrenceSnapshot {
            plan_id,
            plan_uid: Some("run@example.com".to_string()),
            occurrence_key: "20260101T080000Z".to_string(),
            plan_title: "Run".to_string(),
            scheduled_at: Some(chrono::Local.with_ymd_and_hms(2026, 1, 1, 8, 0, 0).unwrap()),
            rrule: Some("FREQ=DAILY".to_string()),
            template: None,
        };
        let mut meta = CharterMetadata::default();
        meta.actions.insert(
            action_id.to_string(),
            ActionMeta {
                occurrence: Some(snapshot.clone()),
                ..Default::default()
            },
        );
        write_sidecar(&sidecar_path(&completed), &meta).unwrap();

        let facts = read_archived_action_facts(root).unwrap();
        assert_eq!(facts.len(), 1);
        assert_eq!(facts[0].action.id, action_id);
        assert_eq!(facts[0].occurrence, Some(snapshot));
        assert_eq!(
            facts[0].source_path,
            PathBuf::from(".clearhead/charters/health.completed.actions")
        );
    }

    #[test]
    fn also_reads_completed_actions_after_charter_archive() {
        let temp = tempfile::tempdir().unwrap();
        let archive = temp.path().join(".clearhead/archive");
        std::fs::create_dir_all(&archive).unwrap();
        let completed = archive.join("019f733d-45b2-7f21-bcad-5610887b7230.completed.actions");
        std::fs::write(
            &completed,
            "[x] Archived #019f733d-45c2-7dd2-91dc-8631f33c6b77\n",
        )
        .unwrap();

        let facts = read_archived_action_facts(temp.path()).unwrap();
        assert_eq!(facts.len(), 1);
        assert_eq!(facts[0].action.name, "Archived");
    }
}
