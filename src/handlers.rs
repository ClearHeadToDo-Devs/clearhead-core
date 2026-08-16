use std::collections::HashMap;

use crate::telemetry::{
    TelemetryEvent, Tool, emit_event, event_from_field_change, event_from_state_change,
};
use chrono::Local;
use clearhead_core::workspace::actions::format::FormatConfig;
use clearhead_core::workspace::actions::{Diff, FieldChange, diff_actions, get_node_text};
use clearhead_core::{ParsedDocument, TrustedDocument, format_trusted_source};
use tower_lsp_server::LanguageServer;
use tower_lsp_server::jsonrpc::{Error, Result};
use tower_lsp_server::ls_types::*;
use tracing::{debug, info, warn};

use super::Backend;
use super::providers::*;

// =============================================================================
// LanguageServer trait — entry points (what the LSP does)
// =============================================================================

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let roots: HashMap<Uri, std::path::PathBuf> = params
            .workspace_folders
            .unwrap_or_default()
            .into_iter()
            .filter_map(|wf| {
                let path = wf.uri.to_file_path()?.to_path_buf();
                Some((wf.uri, path))
            })
            .collect();
        let _ = self.workspace_roots.set(roots);

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "clearhead-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["@".to_string(), "%".to_string()]),
                    ..Default::default()
                }),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            // This legend is currently inert: `compute_semantic_tokens`
                            // emits nothing (see its doc comment). The augmentation
                            // charter will redefine these types + add modifiers so the
                            // overlay reinforces meaning tree-sitter can't see.
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    SemanticTokenType::VARIABLE, // id
                                    SemanticTokenType::KEYWORD,  // priority, state
                                    SemanticTokenType::STRING,   // name, description
                                    SemanticTokenType::COMMENT,  // story
                                    SemanticTokenType::FUNCTION, // context/tags
                                    SemanticTokenType::MACRO,    // date
                                ],
                                token_modifiers: vec![],
                            },
                            ..Default::default()
                        },
                    ),
                ),
                inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
                    InlayHintOptions {
                        resolve_provider: Some(false),
                        ..Default::default()
                    },
                ))),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.update_document(params.text_document.uri, params.text_document.text, true)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            self.update_document(params.text_document.uri, change.text, false)
                .await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!(uri = ?uri, "Processing didSave notification");

        // Snapshot the saved document and emit telemetry. Archival deliberately
        // does not happen in didSave: the editor owns its open buffer, so a
        // client must save, invoke the CLI's durable workspace mutation, and
        // reload only after that process succeeds.
        if let Some(mut doc) = self.documents.get_mut(&uri) {
            if let (Some(current), Some(last)) = (&doc.parsed, &doc.last_saved_parsed)
                && TrustedDocument::try_from(current.clone()).is_ok()
                && TrustedDocument::try_from(last.clone()).is_ok()
            {
                let diff = diff_actions(&last.actions, &current.actions);
                let file_path = uri
                    .to_file_path()
                    .map(|p| p.to_string_lossy().into_owned())
                    .unwrap_or_default();
                emit_diff_telemetry(&diff, current, &file_path);
            }
            doc.last_saved_parsed = doc.parsed.clone();
        }

        // Stamp created timestamps in sidecar for any new actions (best-effort).
        if let Some(path) = uri.to_file_path()
            && let Some(doc) = self.documents.get(&uri)
            && let Some(ref parsed) = doc.parsed
            && let Ok(trusted) = TrustedDocument::try_from(parsed.clone())
        {
            let actions = trusted.actions().clone();
            drop(doc);
            if let Err(e) =
                clearhead_core::workspace::sidecar::stamp_sidecar_entries(&path, &actions)
            {
                warn!(error = %e, "Failed to update sidecar on save");
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!(uri = ?uri, "Releasing document state on didClose");

        // Drop the in-memory state so the DashMap doesn't grow unbounded across
        // an editing session. No telemetry is emitted: any changes since the
        // last save were never persisted, so there is nothing durable to record.
        self.documents.remove(&uri);

        // Clear diagnostics. Once a document closes the server no longer owns its
        // truth (the on-disk version may differ from the buffer we last saw), so
        // stale squiggles must not linger in the client.
        self.client
            .publish_diagnostics(uri, Vec::new(), None)
            .await;
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri;
        if let Some(doc) = self.documents.get(&uri)
            && let Some(ref parsed) = doc.parsed
        {
            return Ok(Some(compute_code_actions(parsed, &uri, params.range)));
        }
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        if let Some(doc) = self.documents.get(&uri) {
            let tokens = compute_semantic_tokens(&doc.tree);
            let mut last_line = 0;
            let mut last_start = 0;
            let data = tokens
                .into_iter()
                .map(|token| {
                    let delta_line = token.delta_line - last_line;
                    let delta_start = if delta_line == 0 {
                        token.delta_start - last_start
                    } else {
                        token.delta_start
                    };
                    last_line = token.delta_line;
                    last_start = token.delta_start;
                    SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.length,
                        token_type: token.token_type,
                        token_modifiers_bitset: 0,
                    }
                })
                .collect();

            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data,
            })));
        }
        Ok(None)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        if let Some(doc) = self.documents.get(&uri)
            && let Some(ref parsed) = doc.parsed
        {
            return Ok(Some(compute_inlay_hints(parsed, None)));
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let (node_kind, ref_text, same_file_range) = {
            let Some(doc) = self.documents.get(&uri) else {
                return Ok(None);
            };
            let Some(node) = get_node_at_position(&doc.tree, position) else {
                return Ok(None);
            };
            let kind = node.kind().to_string();
            let text = get_node_text(&node, &doc.text);

            // Same-file fast path: tags and UUID id nodes resolved from in-memory state
            let same_file = match kind.as_str() {
                // Context/story tags — existing behaviour
                "story" | "context" => doc
                    .parsed
                    .as_ref()
                    .and_then(|p| p.tag_index.get(&text))
                    .and_then(|ranges| ranges.first().copied())
                    .map(source_range_to_lsp_range),
                // The `#uuid` id field — jump to the action itself (same file)
                "uuid_value" => doc
                    .parsed
                    .as_ref()
                    .and_then(|p| {
                        uuid::Uuid::parse_str(&text)
                            .ok()
                            .and_then(|id| p.source_map.get(&id))
                    })
                    .map(|m| source_range_to_lsp_range(m.root)),
                _ => None,
            };

            (kind, text, same_file)
        };

        if let Some(range) = same_file_range {
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            })));
        }

        // Cross-file path: predecessor references, alias lookups, short UUIDs
        let needs_workspace_lookup = matches!(
            node_kind.as_str(),
            "uuid_value"
                | "short_uuid_value"
                | "predecessor_reference"
                | "predecessor_name"
                | "alias_name"
        );

        if needs_workspace_lookup {
            let workspace_root = self
                .workspace_for_uri(&uri)
                .ok_or_else(|| internal_error("No workspace for URI"))?;

            let result = tokio::task::spawn_blocking(move || {
                find_definition_in_workspace(&workspace_root, &ref_text)
            })
            .await
            .map_err(|e| internal_error(format!("Definition lookup panicked: {e}")))?;

            if let Some((file_path, range)) = result {
                let target_uri = Uri::from_file_path(&file_path)
                    .ok_or_else(|| internal_error("Could not build URI from path"))?;
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: target_uri,
                    range,
                })));
            }
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let (node_kind, ref_text, same_file_locations) = {
            let Some(doc) = self.documents.get(&uri) else {
                return Ok(None);
            };
            let Some(node) = get_node_at_position(&doc.tree, position) else {
                return Ok(None);
            };
            let kind = node.kind().to_string();
            let text = get_node_text(&node, &doc.text);

            // Same-file: tags/contexts — existing behaviour
            let same_file = match kind.as_str() {
                "story" | "context" => doc
                    .parsed
                    .as_ref()
                    .and_then(|p| p.tag_index.get(&text))
                    .map(|ranges| {
                        ranges
                            .iter()
                            .map(|r| Location {
                                uri: uri.clone(),
                                range: source_range_to_lsp_range(*r),
                            })
                            .collect::<Vec<_>>()
                    }),
                _ => None,
            };

            (kind, text, same_file)
        };

        if let Some(locs) = same_file_locations {
            return Ok(Some(locs));
        }

        // Cross-file: find all predecessor references to this UUID across the workspace
        let needs_workspace_lookup = matches!(
            node_kind.as_str(),
            "uuid_value" | "short_uuid_value" | "predecessor_name" | "predecessor_reference"
        );

        if needs_workspace_lookup {
            let workspace_root = self
                .workspace_for_uri(&uri)
                .ok_or_else(|| internal_error("No workspace for URI"))?;

            let result = tokio::task::spawn_blocking(move || {
                find_references_in_workspace(&workspace_root, &ref_text)
            })
            .await
            .map_err(|e| internal_error(format!("References lookup panicked: {e}")))?;

            if let Some(locations) = result {
                return Ok(Some(locations));
            }
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc) = self.documents.get(&uri) {
            let line_idx = position.line as usize;
            let lines: Vec<&str> = doc.text.lines().collect();

            if line_idx < lines.len() {
                let line = lines[line_idx];
                let char_idx = position.character as usize;

                if char_idx > 0
                    && let Some('@' | '%') = line.chars().nth(char_idx - 1)
                {
                    return Ok(Some(CompletionResponse::Array(date_completion_items(
                        Local::now(),
                    ))));
                }
            }
        }
        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        if let Some(doc) = self.documents.get(&uri)
            && let Some(ref parsed) = doc.parsed
        {
            let config = FormatConfig {
                indent_width: params.options.tab_size as usize,
                ..Default::default()
            };
            let trusted = match TrustedDocument::try_from(parsed.clone()) {
                Ok(document) => document,
                Err(error) => {
                    self.client
                        .log_message(
                            MessageType::WARNING,
                            format!(
                                "Formatting refused: fix {} parser integrity issue(s) first",
                                error.issues.len()
                            ),
                        )
                        .await;
                    return Ok(None);
                }
            };
            match format_trusted_source(&trusted, Some(config)) {
                Ok(new_text) => {
                    return Ok(Some(vec![full_replace_text_edit(new_text)]));
                }
                Err(e) => {
                    self.client
                        .log_message(MessageType::ERROR, format!("Formatting failed: {e}"))
                        .await;
                    return Ok(None);
                }
            }
        }
        Ok(None)
    }
}

/// A TextEdit that replaces the entire content of a document.
fn full_replace_text_edit(text: String) -> TextEdit {
    TextEdit {
        range: Range::new(Position::new(0, 0), Position::new(u32::MAX, 0)),
        new_text: text,
    }
}

fn internal_error(msg: impl std::fmt::Display) -> Error {
    Error {
        code: tower_lsp_server::jsonrpc::ErrorCode::InternalError,
        message: msg.to_string().into(),
        data: None,
    }
}

fn emit_diff_telemetry(diff: &Diff, current: &ParsedDocument, file_path: &str) {
    if !diff.is_empty() {
        info!(
            added = diff.added.len(),
            removed = diff.removed.len(),
            modified = diff.modified.len(),
            "Changes detected on save"
        );
    }

    for action in &diff.added {
        debug!(id = %action.id, name = %action.name, "Emitting action_created event");
        if let Err(e) = emit_event(
            Tool::Lsp,
            Some(action.id.to_string()),
            TelemetryEvent::ActionCreated {
                name: action.name.clone(),
                file_path: file_path.to_string(),
            },
        ) {
            warn!(error = %e, "Failed to emit action_created event");
        }
    }

    for action in &diff.removed {
        debug!(id = %action.id, name = %action.name, "Emitting action_deleted event");
        if let Err(e) = emit_event(
            Tool::Lsp,
            Some(action.id.to_string()),
            TelemetryEvent::ActionDeleted {
                name: action.name.clone(),
            },
        ) {
            warn!(error = %e, "Failed to emit action_deleted event");
        }
    }

    for mod_action in &diff.modified {
        let id = Some(mod_action.id.to_string());
        let name = current
            .actions
            .iter()
            .find(|a| a.id == mod_action.id)
            .map(|a| a.name.as_str())
            .unwrap_or("");

        for change in &mod_action.changes {
            let event = match change {
                FieldChange::State { old, new } => {
                    debug!(id = %mod_action.id, old = ?old, new = ?new, "Emitting state change event");
                    event_from_state_change(*old, *new, name)
                }
                _ => event_from_field_change(change),
            };
            if let Some(evt) = event
                && let Err(e) = emit_event(Tool::Lsp, id.clone(), evt)
            {
                warn!(error = %e, "Failed to emit property change event");
            }
        }
    }
}

// =============================================================================
// Workspace reference resolution helpers
// =============================================================================

/// Resolve a reference string to a `(file_path, Range)` using the core reference resolver.
///
/// Handles full UUIDs, short UUID prefixes, alias names, and path notation.
/// Returns `None` when the reference cannot be resolved or the workspace is unavailable.
/// Read and parse every action file in the workspace, skipping files that
/// fail to read or parse.
fn parsed_action_files(
    workspace_root: &std::path::Path,
) -> impl Iterator<Item = (std::path::PathBuf, clearhead_core::ParsedDocument)> {
    use clearhead_core::{list_action_files, parse_document};

    list_action_files(workspace_root)
        .unwrap_or_default()
        .into_iter()
        .filter_map(|path| {
            let content = std::fs::read_to_string(&path).ok()?;
            let parsed = parse_document(&content).ok()?;
            Some((path, parsed))
        })
}

fn find_definition_in_workspace(
    workspace_root: &std::path::Path,
    ref_text: &str,
) -> Option<(std::path::PathBuf, tower_lsp_server::ls_types::Range)> {
    use clearhead_core::{ReferenceOptions, ReferenceTarget, load_domain_model, resolve_reference};

    let model = load_domain_model(workspace_root).ok()?;
    let opts = ReferenceOptions::default();
    let target = resolve_reference(&model, ref_text, &opts).ok()?;

    let target_uuid = match target {
        ReferenceTarget::Action(id) | ReferenceTarget::Charter(id) | ReferenceTarget::Plan(id) => {
            id
        }
    };

    parsed_action_files(workspace_root).find_map(|(file_path, parsed)| {
        let meta = parsed.source_map.get(&target_uuid)?;
        Some((file_path, source_range_to_lsp_range(meta.root)))
    })
}

/// Find all locations in the workspace that reference a given UUID as a predecessor.
///
/// Returns `None` when the workspace is unavailable or no references exist.
fn find_references_in_workspace(
    workspace_root: &std::path::Path,
    ref_text: &str,
) -> Option<Vec<tower_lsp_server::ls_types::Location>> {
    let target_uuid = uuid::Uuid::parse_str(ref_text).ok();

    let mut locations = Vec::new();

    for (file_path, parsed) in parsed_action_files(workspace_root) {
        let file_uri = match Uri::from_file_path(&file_path) {
            Some(u) => u,
            None => continue,
        };

        for action in &parsed.actions {
            let refs_target = action.predecessors.as_ref().is_some_and(|preds| {
                preds.iter().any(|pred| match target_uuid {
                    Some(uuid) => pred.resolved_uuid == Some(uuid),
                    None => pred.raw_ref.starts_with(ref_text),
                })
            });

            if refs_target && let Some(meta) = parsed.source_map.get(&action.id) {
                locations.push(tower_lsp_server::ls_types::Location {
                    uri: file_uri.clone(),
                    range: source_range_to_lsp_range(meta.root),
                });
            }
        }
    }

    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}
