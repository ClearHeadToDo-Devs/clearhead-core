use crate::domain::Action;
use super::source::SourceMetadata;

/// An action paired with its file-layer source metadata.
///
/// `source_metadata` carries line/column positions for LSP diagnostics;
/// absent when loaded from disk rather than from a live parse. File
/// provenance is *not* stored per action — it derives from the enclosing
/// file/charter group (see [`ActionsFile.path`] / [`MarkdownCharter.actions_file`]),
/// which every consumer already iterates.
#[derive(Debug, Clone)]
pub struct SourcedAction {
    pub action: Action,
    pub source_metadata: Option<SourceMetadata>,
}
