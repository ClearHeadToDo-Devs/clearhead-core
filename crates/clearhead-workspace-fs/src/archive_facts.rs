//! Typed read model for archived action facts.
//!
//! Archive storage is deliberately plaintext (`*.completed.actions` plus sidecar
//! JSON), but consumers should not need to learn those file/layout rules. This
//! native adapter translates archive files into Core's host-neutral fact DTO.

use std::path::{Path, PathBuf};

use clearhead_core::ArchivedActionFact;
use clearhead_core::workspace::sidecar::sidecar_path;
use clearhead_core::workspace::store::{WorkspaceError, charter_root, workspace_data_root};

/// Read archived action facts from every completed-actions store the native workspace owns.
///
/// This covers both active charter-local `*.completed.actions` files and charter
/// histories that have moved into `.clearhead/archive/*.completed.actions`.
/// Callers get data, not filesystem conventions.
pub fn read_archived_action_facts(root: &Path) -> Result<Vec<ArchivedActionFact>, WorkspaceError> {
    let mut files = Vec::new();
    collect_completed_actions_files(&charter_root(root), &mut files)?;
    collect_completed_actions_files(&workspace_data_root(root).join("archive"), &mut files)?;
    files.sort();
    files.dedup();

    let mut facts = Vec::new();
    for path in files {
        let metadata = crate::sidecar::read_sidecar(&sidecar_path(&path))?;
        let source_path = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
        for action in crate::action_files::read_actions(&path)? {
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
    use crate::sidecar::write_sidecar;
    use chrono::TimeZone;
    use clearhead_core::workspace::sidecar::{
        ActionMeta, CharterMetadata, OccurrenceSnapshot, sidecar_path,
    };
    use uuid::Uuid;

    #[test]
    fn reads_completed_actions_with_occurrence_snapshot() {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join(".clearhead/charters");
        std::fs::create_dir_all(&charters).unwrap();
        let completed = charters.join("health.completed.actions");
        let action_id: Uuid = "019f733d-45b2-7f21-bcad-5610887b7230".parse().unwrap();
        let plan_id: Uuid = "019f733d-45c2-7dd2-91dc-8631f33c6b77".parse().unwrap();
        std::fs::write(&completed, format!("[x] Run #{action_id}\n")).unwrap();
        let occurrence = OccurrenceSnapshot {
            plan_id,
            plan_uid: Some("run@example.com".into()),
            occurrence_key: "20260820T080000Z".into(),
            plan_title: "Run".into(),
            scheduled_at: Some(
                chrono::Local
                    .with_ymd_and_hms(2026, 8, 20, 8, 0, 0)
                    .unwrap(),
            ),
            rrule: Some("FREQ=DAILY".into()),
            template: None,
        };
        let mut metadata = CharterMetadata::default();
        metadata.actions.insert(
            action_id.to_string(),
            ActionMeta {
                created: None,
                plan: None,
                occurrence: Some(occurrence.clone()),
            },
        );
        write_sidecar(&sidecar_path(&completed), &metadata).unwrap();
        let facts = read_archived_action_facts(temp.path()).unwrap();
        assert_eq!(facts.len(), 1);
        assert_eq!(facts[0].occurrence, Some(occurrence));
    }
}
