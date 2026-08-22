//! Whole-workspace RDF export: `clearhead export workspace`.
//!
//! The exported bytes are Core's canonical projection of the validated
//! plaintext workspace (assembled by [`crate::dataset`]) — a deterministic,
//! replaceable publication snapshot. This module owns only invocation
//! concerns: format selection, stdout-vs-file output, and failure context.
//! No semantic mapping lives here.

use std::path::Path;

use anyhow::{Context as _, anyhow};
use clearhead_core::rdf::{self, RdfFormat};

use crate::argparser::RdfExportFormat;
use crate::commands::CommandContext;

/// Export the workspace's canonical RDF dataset. TriG is the default: a
/// dataset syntax that preserves each workspace's stable named graph, so the
/// export round-trips into any RDF dataset-aware store.
pub fn workspace(
    ctx: &CommandContext,
    format: Option<RdfExportFormat>,
    output: Option<&Path>,
) -> anyhow::Result<()> {
    let rdf_format = match format.unwrap_or(RdfExportFormat::Trig) {
        RdfExportFormat::Trig => RdfFormat::TriG,
        RdfExportFormat::Nquads => RdfFormat::NQuads,
        RdfExportFormat::Jsonld => RdfFormat::JsonLd,
        RdfExportFormat::Turtle => RdfFormat::Turtle,
    };
    let quads = crate::dataset::assemble_dataset(ctx)?;
    let text = rdf::serialize(&quads, rdf_format)
        .map_err(|e| anyhow!("Failed to serialize the workspace dataset: {e}"))?;
    match output {
        Some(path) => std::fs::write(path, &text)
            .with_context(|| format!("Failed to write {}", path.display())),
        None => crate::stdout::write_stdout(text.as_bytes()),
    }
}
