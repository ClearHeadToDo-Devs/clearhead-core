//! Whole-workspace RDF dataset assembly — the one load→project path behind
//! `clearhead export workspace` and the `sparql` feature's ephemeral store.
//!
//! Every quad comes from Core's application-graph projection
//! ([`app::project_app`], specifications `ontology.md`), with host-supplied
//! locations and the viewer's zone (the local zone). This module only
//! orchestrates workspace loading and merges the per-graph results into one
//! canonical set — nothing here builds an RDF term.

use anyhow::{Context as _, anyhow};
use chrono::Local;
use clearhead_core::rdf::{self, app};
use clearhead_core::workspace::store::Workspace;
use oxrdf::Quad;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use uuid::Uuid;

use crate::cli::CommandContext;

/// The merged dataset, and each entity's absolute data root. The graph names
/// no root (rule 3), so a row that locates an entity gets it from here.
pub struct Dataset {
    pub quads: Vec<Quad>,
    pub data_roots: HashMap<Uuid, PathBuf>,
}

/// Load every selected workspace and return the merged canonical dataset: one
/// `urn:clearhead:workspace:<uuid>` named graph per workspace, with charters
/// that declare no document id as blank nodes, canonicalized
/// so downstream serialization is byte-deterministic (for workspaces with
/// durable manifest identity — an identity-less workspace's ephemeral graph
/// name is intentionally unstable, see `Workspace::ephemeral_id`).
///
/// The primary workspace contributes the configured context hierarchy.
pub fn assemble_dataset(ctx: &CommandContext) -> anyhow::Result<Dataset> {
    project_dataset(ctx, load_workspaces(ctx)?)
}

/// Load every selected workspace once. The primary must load; additional
/// workspaces warn and are skipped on error so one bad workspace never blocks
/// the others.
pub fn load_workspaces(ctx: &CommandContext) -> anyhow::Result<Vec<Workspace>> {
    let mut workspaces = Vec::new();
    for (_name, path) in ctx.workspace_dirs() {
        match clearhead_cli::filesystem::load_workspace_model(&path) {
            Ok(workspace) => workspaces.push(workspace),
            Err(error) if path == ctx.data_dir => {
                return Err(error).context("Failed to load workspace");
            }
            Err(error) => tracing::warn!("Skipping workspace '{}': {error}", path.display()),
        }
    }
    Ok(workspaces)
}

/// Project already loaded workspaces into the merged canonical dataset.
pub fn project_dataset(
    ctx: &CommandContext,
    workspaces: Vec<Workspace>,
) -> anyhow::Result<Dataset> {
    let config = ctx.workspace_config();
    let mut quads = Vec::new();
    let mut unpublished = HashSet::new();
    let mut data_roots = HashMap::new();

    for workspace in workspaces {
        let is_primary = workspace.root == ctx.data_dir;
        let path = workspace.root.clone();
        let data_root = clearhead_cli::filesystem::workspace_data_root(&path);
        let data_root = data_root.canonicalize().unwrap_or(data_root);
        let graph = rdf::workspace_graph_name(&workspace.effective_id());
        let locations = app::Locations::of(&workspace);
        data_roots.extend(locations.files.keys().map(|id| (*id, data_root.clone())));
        unpublished.extend(workspace.unpublished_charter_ids());
        let model = clearhead_core::DomainModel::from(workspace);
        quads.extend(
            app::project_app(
                &model,
                &locations,
                is_primary.then_some(&config),
                &Local,
                graph,
            )
            .map_err(|e| anyhow!("Failed to project workspace '{}': {e}", path.display()))?,
        );
    }

    rdf::canonicalize(&mut quads);
    Ok(Dataset {
        quads: rdf::anonymize_charters(quads, &unpublished),
        data_roots,
    })
}
