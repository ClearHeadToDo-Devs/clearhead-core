use std::path::{Path, PathBuf};
use uuid::Uuid;

use clearhead_core::ReferenceTarget;

use crate::commands::CommandContext;

pub enum ResolvedScope {
    Charter { file_path: PathBuf },
    Plan { file_path: PathBuf },
    Action { file_path: PathBuf },
}

/// Resolve a domain reference string (e.g. `"health"`, `"health/exercise"`,
/// `"c:clearhead-cli"`, `"a:0e8dda27"`) to a concrete file path.
///
/// Searches all configured workspaces in primary-first order; returns the
/// first match. The [`ReferenceOptions`] default applies: `c:`/`p:`/`a:`
/// prefixes allowed, alias and UUID matching only (no title fuzzing).
pub fn resolve_domain_ref(ctx: &CommandContext, ref_str: &str) -> anyhow::Result<ResolvedScope> {
    let models = ctx.all_domain_models()?;

    let workspace_refs: Vec<(&str, &clearhead_core::DomainModel)> = models
        .iter()
        .map(|(name, model)| (name.as_str(), model))
        .collect();

    let (ws_name, target) = clearhead_core::resolve_reference_in_workspaces(
        &workspace_refs,
        ref_str,
        &clearhead_core::ReferenceOptions::default(),
    )?;

    let ws_dir = ctx
        .workspace_dirs()
        .into_iter()
        .find(|(name, _)| *name == ws_name)
        .map(|(_, dir)| dir)
        .ok_or_else(|| anyhow::anyhow!("Internal: workspace '{}' missing from dirs", ws_name))?;

    let model = models
        .iter()
        .find(|(name, _)| *name == ws_name)
        .map(|(_, m)| m)
        .ok_or_else(|| anyhow::anyhow!("Internal: model for workspace '{}' missing", ws_name))?;

    match target {
        ReferenceTarget::Charter(id) => {
            let file_path = charter_actions_path(model, id, &ws_dir)?;
            Ok(ResolvedScope::Charter { file_path })
        }
        ReferenceTarget::Plan(plan_id) => {
            let charter_id = model
                .charters
                .iter()
                .find(|c| c.plans.iter().any(|p| p.id == plan_id))
                .map(|c| c.id)
                .ok_or_else(|| anyhow::anyhow!("Plan {} not found in model", plan_id))?;
            let file_path = charter_actions_path(model, charter_id, &ws_dir)?;
            Ok(ResolvedScope::Plan { file_path })
        }
        ReferenceTarget::Action(action_id) => {
            let charter_id = model
                .charters
                .iter()
                .find(|c| c.actions.iter().any(|a| a.id == action_id))
                .map(|c| c.id)
                .ok_or_else(|| anyhow::anyhow!("Action {} not found in model", action_id))?;
            let file_path = charter_actions_path(model, charter_id, &ws_dir)?;
            Ok(ResolvedScope::Action { file_path })
        }
    }
}

fn charter_actions_path(
    model: &clearhead_core::DomainModel,
    charter_id: Uuid,
    ws_dir: &Path,
) -> anyhow::Result<PathBuf> {
    let charter = model
        .charters
        .iter()
        .find(|c| c.id == charter_id)
        .ok_or_else(|| anyhow::anyhow!("Charter {} not found in model", charter_id))?;
    let key = charter.alias.as_deref().unwrap_or(&charter.title);
    crate::commands::charter_to_file_path(ws_dir, key)
}
