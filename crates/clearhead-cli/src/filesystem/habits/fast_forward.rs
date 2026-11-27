use std::path::Path;

use chrono::{DateTime, FixedOffset};
use clearhead_core::workspace::habit_stamp::{FastForward, StampSource, fast_forward_text_habit};
use clearhead_core::{ReferenceSelection, select_reference};

use super::{
    EffectBatch, NativeWorkspaceMounts, ObservedFiles, WorkspaceError, actions_anchor, deliver,
    discover, render_sidecar, sidecar_path, validate_source_path,
};

/// Skip the due, unmaterialized slots of exactly one Habit. Only its sidecar
/// can change; existing occurrence rows and history are revision evidence.
pub fn fast_forward(
    workspace_root: &Path,
    reference: &str,
    now: DateTime<FixedOffset>,
    dry_run: bool,
) -> Result<FastForward, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root);
    let (definitions, history) = discover(&mounts)?;
    let mut files = ObservedFiles::new(&mounts.workspace);
    // A quarantined definition could contain a second match. The skip must
    // never guess a target from an incomplete set of Habit roots.
    let definitions = definitions
        .iter()
        .map(|path| files.document(path).map(|doc| (path, doc)))
        .collect::<Result<Vec<_>, _>>()?;
    let candidates: Vec<_> = definitions
        .iter()
        .flat_map(|(path, doc)| {
            doc.actions
                .iter()
                .filter(|action| action.parent_id.is_none())
                .map(move |action| (*path, doc, action))
        })
        .collect();
    let roots: Vec<_> = candidates.iter().map(|(_, _, root)| *root).collect();
    let (path, doc, root) = match select_reference(&roots, reference) {
        ReferenceSelection::Unique { index, .. } => candidates[index],
        ReferenceSelection::NotFound => {
            return Err(WorkspaceError::Actions(format!(
                "no Habit matches '{reference}'; use its alias or UUID"
            )));
        }
        ReferenceSelection::Ambiguous { indices, .. } => {
            let ids: Vec<_> = indices
                .iter()
                .map(|&index| roots[index].id.to_string())
                .collect();
            return Err(WorkspaceError::Actions(format!(
                "Habit reference '{reference}' is ambiguous; candidates: {}",
                ids.join(", ")
            )));
        }
    };
    let occupied = files.occupied(history)?;
    if occupied.contains(&root.id)
        || definitions
            .iter()
            .flat_map(|(_, doc)| &doc.actions)
            .filter(|action| action.id == root.id)
            .nth(1)
            .is_some()
    {
        return Err(WorkspaceError::Actions(format!(
            "{}: Habit UUID {} is also used on another line [duplicate-uuid]",
            path.display(),
            root.id
        )));
    }
    let anchor = actions_anchor(path).expect("classified Habit path");
    validate_source_path(&anchor, &mounts.charter_root())?;
    let sidecar = sidecar_path(&anchor);
    let mut metadata = files.metadata(&sidecar)?;
    let result = fast_forward_text_habit(
        StampSource::new(path, doc),
        root.id,
        &now,
        &occupied,
        &mut metadata,
    )?;
    if !dry_run && result.skipped != 0 {
        let write = files.write(&sidecar, render_sidecar(&metadata)?.into_bytes())?;
        let batch = EffectBatch::new(vec![write], files.preconditions())
            .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
        let _lock = crate::filesystem::sidecar::lock_sidecar(&sidecar)?;
        deliver(&mounts, &batch)?;
    }
    Ok(result)
}
