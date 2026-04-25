use std::collections::HashMap;
use std::path::{Path, PathBuf};

use uuid::Uuid;

use crate::workspace::actions::{Action, ActionList};
use crate::workspace::store::WorkspaceError;

/// Resolve a template file by name, searching charter-local then data-root.
///
/// Returns `Ok(None)` if the template is not found in either location.
pub fn resolve_template(
    charter_dir: &Path,
    data_root: &Path,
    name: &str,
) -> Result<Option<PathBuf>, WorkspaceError> {
    let filename = format!("{}.actions", name);

    let local = charter_dir.join("templates").join(&filename);
    if local.is_file() {
        return Ok(Some(local));
    }

    let root = data_root.join("templates").join(&filename);
    if root.is_file() {
        return Ok(Some(root));
    }

    Ok(None)
}

/// Instantiate a template, remapping all UUIDs and optionally reparenting root acts.
///
/// `id_for(template_id)` maps each original action UUID to a new instance UUID.
/// `parent_override` sets `parent_id` on template root acts (those with no parent in
/// the template) — used by `expand acts` to attach template children under a
/// VEVENT-generated root act.
pub fn instantiate_template(
    template: &ActionList,
    id_for: impl Fn(Uuid) -> Uuid,
    parent_override: Option<Uuid>,
) -> ActionList {
    let id_map: HashMap<Uuid, Uuid> = template.iter().map(|a| (a.id, id_for(a.id))).collect();

    template
        .iter()
        .map(|a| {
            let new_parent = match a.parent_id {
                Some(old_parent) => Some(*id_map.get(&old_parent).unwrap_or(&old_parent)),
                None => parent_override,
            };
            Action {
                id: id_map[&a.id],
                parent_id: new_parent,
                ..a.clone()
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::ActionState;
    use std::fs;

    #[test]
    fn resolve_finds_charter_local_first() {
        let tmp = tempfile::tempdir().unwrap();
        let charter_dir = tmp.path().join("health");
        let data_root = tmp.path().join("root");

        fs::create_dir_all(charter_dir.join("templates")).unwrap();
        fs::create_dir_all(data_root.join("templates")).unwrap();

        let local = charter_dir.join("templates/weekly-review.actions");
        let root = data_root.join("templates/weekly-review.actions");
        fs::write(&local, "[ ] Step one\n").unwrap();
        fs::write(&root, "[ ] Root version\n").unwrap();

        let result = resolve_template(&charter_dir, &data_root, "weekly-review").unwrap();
        assert_eq!(result, Some(local));
    }

    #[test]
    fn resolve_falls_back_to_data_root() {
        let tmp = tempfile::tempdir().unwrap();
        let charter_dir = tmp.path().join("health");
        let data_root = tmp.path().join("root");

        fs::create_dir_all(&charter_dir).unwrap();
        fs::create_dir_all(data_root.join("templates")).unwrap();

        let root = data_root.join("templates/weekly-review.actions");
        fs::write(&root, "[ ] Step one\n").unwrap();

        let result = resolve_template(&charter_dir, &data_root, "weekly-review").unwrap();
        assert_eq!(result, Some(root));
    }

    #[test]
    fn resolve_returns_none_when_missing() {
        let tmp = tempfile::tempdir().unwrap();
        let result =
            resolve_template(&tmp.path().join("a"), &tmp.path().join("b"), "nonexistent").unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn instantiate_remaps_uuids_and_preserves_hierarchy() {
        let parent_id = Uuid::now_v7();
        let child_id = Uuid::now_v7();

        let template = vec![
            Action {
                id: parent_id,
                name: "Parent".into(),
                state: ActionState::NotStarted,
                ..Default::default()
            },
            Action {
                id: child_id,
                name: "Child".into(),
                state: ActionState::NotStarted,
                parent_id: Some(parent_id),
                ..Default::default()
            },
        ];

        let counter = std::sync::atomic::AtomicU64::new(1);
        let result = instantiate_template(
            &template,
            |_| {
                let n = counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                Uuid::from_u128(n as u128)
            },
            None,
        );

        assert_eq!(result.len(), 2);
        assert_ne!(result[0].id, parent_id);
        assert_ne!(result[1].id, child_id);
        assert_eq!(result[1].parent_id, Some(result[0].id));
        assert!(result[0].parent_id.is_none());
    }

    #[test]
    fn instantiate_applies_parent_override_to_root_acts() {
        let root_id = Uuid::now_v7();
        let override_parent = Uuid::now_v7();

        let template = vec![Action {
            id: root_id,
            name: "Root act".into(),
            state: ActionState::NotStarted,
            ..Default::default()
        }];

        let result = instantiate_template(&template, |_| Uuid::now_v7(), Some(override_parent));

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].parent_id, Some(override_parent));
        assert_ne!(result[0].id, root_id);
    }
}
