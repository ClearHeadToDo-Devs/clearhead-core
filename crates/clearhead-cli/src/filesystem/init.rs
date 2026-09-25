//! Native delivery for `clearhead init`: snapshot the bootstrap resources, let
//! Core plan, and deliver the batch.

use std::path::Path;

use clearhead_core::workspace::init::{
    MANIFEST_PATH, ROOT_ACTIONS_PATH, ROOT_README_PATH, ROOT_SIDECAR_PATH,
};
use clearhead_core::workspace::resource::ExpectedResource;
use clearhead_core::workspace::{InitPlan, InitRequest, InitSnapshot, plan_workspace_init};

use crate::filesystem::{NativeWorkspaceMounts, WorkspaceError, deliver, snapshot};

/// Bootstrap or repair the workspace at `root` and return the delivered plan.
///
/// `root` is a project directory whose `.clearhead/` already exists, or the
/// user workspace's data dir; both resolve to their data root the same way.
pub fn init_workspace(root: &Path, request: &InitRequest) -> Result<InitPlan, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(root);
    let read = |path: &str| {
        let (resource, expected) = snapshot(&mounts.workspace, &mounts.workspace.join(path))?;
        Ok::<_, WorkspaceError>((expected != ExpectedResource::Missing).then_some(resource))
    };
    let found = InitSnapshot {
        manifest: read(MANIFEST_PATH)?,
        readme: read(ROOT_README_PATH)?,
        root_actions: read(ROOT_ACTIONS_PATH)?,
        sidecar: read(ROOT_SIDECAR_PATH)?,
    };
    let plan = plan_workspace_init(&found, request)?;
    deliver(&mounts, &plan.batch)?;
    Ok(plan)
}
