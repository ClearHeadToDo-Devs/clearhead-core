//! Native template discovery using Core's ordered candidate policy.

use std::path::{Path, PathBuf};

use clearhead_core::workspace::WorkspaceError;

pub fn resolve_template(
    charter_dir: &Path,
    data_root: &Path,
    name: &str,
) -> Result<Option<PathBuf>, WorkspaceError> {
    Ok(
        clearhead_core::workspace::templates::template_candidates(charter_dir, data_root, name)
            .into_iter()
            .find(|path| path.is_file()),
    )
}
