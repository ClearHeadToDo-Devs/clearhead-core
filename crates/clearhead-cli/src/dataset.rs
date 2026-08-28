//! Whole-workspace RDF dataset assembly — the one load→project path behind
//! `clearhead export workspace` and the `sparql` feature's ephemeral store.
//!
//! Every quad comes from Core's canonical projection: [`rdf::project_domain`]
//! for domain semantics, [`rdf::project_workspace_snapshot`] for the `ws:`
//! workspace-snapshot layer built from host-supplied filesystem evidence. This
//! module only orchestrates workspace loading and merges the per-graph results
//! into one canonical set — nothing here builds an RDF term.

use anyhow::{Context as _, anyhow};
use clearhead_core::rdf::{self, WorkspaceSnapshot};
use clearhead_core::workspace::store::Workspace;
use oxrdf::Quad;

use crate::commands::CommandContext;

/// Load every selected workspace and return the merged canonical dataset: one
/// `urn:clearhead:workspace:<uuid>` named graph per workspace, canonicalized
/// so downstream serialization is byte-deterministic (for workspaces with
/// durable manifest identity — an identity-less workspace's ephemeral graph
/// name is intentionally unstable, see `Workspace::ephemeral_id`).
///
/// The primary workspace honors `plan_path` and contributes the configured
/// context hierarchy; additional workspaces warn and are skipped on error so
/// one bad workspace never blocks the others.
pub fn assemble_dataset(ctx: &CommandContext) -> anyhow::Result<Vec<Quad>> {
    let config = ctx.workspace_config();
    let mut quads = Vec::new();

    for (_name, path) in ctx.workspace_dirs() {
        let is_primary = path == ctx.data_dir;
        let loaded = if is_primary {
            clearhead_workspace_fs::load_workspace_model(&path, ctx.plan_override().as_deref())
        } else {
            clearhead_workspace_fs::load_workspace_model(&path, None)
        };
        let workspace = match loaded {
            Ok(workspace) => workspace,
            Err(error) if is_primary => {
                return Err(error).context("Failed to load workspace");
            }
            Err(error) => {
                tracing::warn!("Skipping workspace '{}': {error}", path.display());
                continue;
            }
        };

        let graph = rdf::workspace_graph_name(&workspace.effective_id());
        let snapshot = workspace_snapshot(&workspace);
        let model = clearhead_core::DomainModel::from(workspace);
        quads.extend(
            rdf::project_domain(&model, is_primary.then_some(&config), graph.clone())
                .map_err(|e| anyhow!("Failed to project workspace '{}': {e}", path.display()))?,
        );
        quads.extend(
            rdf::project_workspace_snapshot(&snapshot, graph)
                .map_err(|e| anyhow!("Failed to project workspace snapshot: {e}"))?,
        );
    }

    rdf::canonicalize(&mut quads);
    Ok(quads)
}

/// Assemble the host evidence for Core's pure workspace-snapshot projection:
/// workspace identity plus per-charter / per-action source locations, with
/// paths canonicalized here at the filesystem boundary.
fn workspace_snapshot(workspace: &Workspace) -> WorkspaceSnapshot {
    let root = workspace
        .root
        .canonicalize()
        .unwrap_or_else(|_| workspace.root.clone());
    WorkspaceSnapshot {
        workspace_id: workspace.effective_id(),
        workspace_name: workspace.effective_name(),
        root: root.to_string_lossy().into_owned(),
        charter_root: clearhead_workspace_fs::charter_root(&root)
            .to_string_lossy()
            .into_owned(),
        charter_files: workspace
            .charters
            .iter()
            .filter_map(|charter| {
                charter
                    .md_file
                    .as_deref()
                    .map(|p| (charter.id, p.to_string_lossy().into_owned()))
            })
            .collect(),
        action_sources: workspace
            .charters
            .iter()
            .flat_map(|charter| {
                let source_file = charter
                    .actions_file
                    .as_deref()
                    .map(|p| p.to_string_lossy().into_owned())
                    .unwrap_or_default();
                charter.actions.iter().filter_map(move |sourced| {
                    sourced.source_metadata.as_ref().map(|meta| {
                        (
                            sourced.action.id,
                            source_file.clone(),
                            // Published lines are 1-based; tree-sitter rows are 0-based.
                            meta.root.start_row as u32 + 1,
                        )
                    })
                })
            })
            .collect(),
    }
}
