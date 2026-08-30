use chrono::{DateTime, Local};
use clearhead_core::workspace::actions::{LintDiagnostic, LintSeverity, lint_document};
use clearhead_core::workspace::{Finding, FindingSeverity};
use clearhead_core::{ParsedDocument, SourceRange};
use tower_lsp_server::ls_types::*;
use tree_sitter::Tree;
use uuid::Uuid;

pub fn source_range_to_lsp_range(src: SourceRange) -> Range {
    Range {
        start: Position::new(src.start_row as u32, src.start_col as u32),
        end: Position::new(src.end_row as u32, src.end_col as u32),
    }
}

fn lint_severity_to_lsp(severity: LintSeverity) -> DiagnosticSeverity {
    match severity {
        LintSeverity::Error => DiagnosticSeverity::ERROR,
        LintSeverity::Warning => DiagnosticSeverity::WARNING,
        LintSeverity::Info => DiagnosticSeverity::INFORMATION,
    }
}

fn lint_diagnostic_to_lsp(diag: LintDiagnostic) -> Diagnostic {
    Diagnostic {
        range: source_range_to_lsp_range(diag.range),
        severity: Some(lint_severity_to_lsp(diag.severity)),
        code: Some(NumberOrString::String(diag.code)),
        source: Some("clearhead-lsp".to_string()),
        message: diag.message,
        ..Default::default()
    }
}

pub fn compute_diagnostics(doc: &ParsedDocument) -> Vec<Diagnostic> {
    lint_document(doc)
        .into_iter()
        .map(lint_diagnostic_to_lsp)
        .collect()
}

pub fn finding_to_lsp(finding: &Finding) -> Diagnostic {
    Diagnostic {
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
        severity: Some(match finding.severity {
            FindingSeverity::Warning => DiagnosticSeverity::WARNING,
            FindingSeverity::Violation => DiagnosticSeverity::ERROR,
        }),
        code: Some(NumberOrString::String(finding.code.clone())),
        source: Some("clearhead-workspace".to_string()),
        message: finding.message.clone(),
        ..Default::default()
    }
}

pub fn date_completion_items(now: DateTime<Local>) -> Vec<CompletionItem> {
    let make_item = |label: String, detail: &str| CompletionItem {
        label: label.clone(),
        kind: Some(CompletionItemKind::VALUE),
        detail: Some(detail.to_string()),
        insert_text: Some(label),
        ..Default::default()
    };

    vec![
        make_item(now.format("%Y-%m-%dT%H:%M").to_string(), "Now"),
        make_item(now.format("%Y-%m-%d").to_string(), "Today"),
        make_item(
            (now + chrono::Duration::days(1))
                .format("%Y-%m-%d")
                .to_string(),
            "Tomorrow",
        ),
    ]
}

fn create_quick_fix(
    uri: Uri,
    pos: Position,
    new_text: String,
    title: String,
) -> CodeActionOrCommand {
    let mut changes = std::collections::HashMap::new();
    changes.insert(
        uri,
        vec![TextEdit {
            range: Range::new(pos, pos),
            new_text,
        }],
    );

    CodeActionOrCommand::CodeAction(CodeAction {
        title,
        kind: Some(CodeActionKind::QUICKFIX),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }),
        ..Default::default()
    })
}

pub fn compute_code_actions(
    doc: &ParsedDocument,
    uri: &Uri,
    range: Range,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Parser recovery remains diagnostic-only, but deterministic repairs can
    // be offered as surgical source edits. A Tree-sitter MISSING close carries
    // the exact insertion point and never requires whole-document formatting.
    for issue in &doc.syntax_errors {
        let issue_range = source_range_to_lsp_range(issue.range);
        if issue.message == "missing ']]'"
            && range.start.line <= issue_range.end.line
            && range.end.line >= issue_range.start.line
        {
            actions.push(create_quick_fix(
                uri.clone(),
                issue_range.start,
                "]]".to_string(),
                "Close incomplete link".to_string(),
            ));
        }
    }

    for action in &doc.actions {
        if let Some(metadata) = doc.source_map.get(&action.id) {
            let action_range = source_range_to_lsp_range(metadata.root);

            if range.start.line <= action_range.end.line
                && range.end.line >= action_range.start.line
            {
                let insert_pos = source_range_to_lsp_range(metadata.line_range).end;

                // 1. Hydrate UUID
                if metadata.is_id_generated {
                    let uuid = Uuid::now_v7();
                    actions.push(create_quick_fix(
                        uri.clone(),
                        insert_pos,
                        format!(" #{}", uuid),
                        "Hydrate Action (Add UUID)".to_string(),
                    ));
                }

                // 2. Add Completion Date
                if action.state == clearhead_core::ActionState::Completed
                    && action.completed_at.is_none()
                {
                    let now = Local::now();
                    actions.push(create_quick_fix(
                        uri.clone(),
                        insert_pos,
                        format!(" %{}", now.format("%Y-%m-%dT%H:%M")),
                        "Set Completion Date (Today)".to_string(),
                    ));
                }
            }
        }
    }
    actions
}

pub fn compute_inlay_hints(
    doc: &ParsedDocument,
    base_time: Option<DateTime<Local>>,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    let now = base_time.unwrap_or_else(Local::now);

    for action in &doc.actions {
        if let Some(metadata) = doc.source_map.get(&action.id) {
            // Do Date Hint
            if let (Some(dt), Some(range)) = (action.scheduled_at, metadata.do_date) {
                let diff = dt.signed_duration_since(now);
                let label = if diff.num_days() > 0 {
                    format!(" (due in {}d)", diff.num_days())
                } else if diff.num_days() < 0 {
                    format!(" ({}d ago)", -diff.num_days())
                } else {
                    " (due today)".to_string()
                };

                let lsp_range = source_range_to_lsp_range(range);
                hints.push(InlayHint {
                    position: lsp_range.end,
                    label: InlayHintLabel::String(label),
                    kind: Some(InlayHintKind::PARAMETER),
                    text_edits: None,
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: Some(false),
                    data: None,
                });
            }

            // Completed Date Hint
            if let (Some(dt), Some(range)) = (action.completed_at, metadata.completed_date) {
                let diff = now.signed_duration_since(dt);
                let label = format!(" (done {}d ago)", diff.num_days());

                let lsp_range = source_range_to_lsp_range(range);
                hints.push(InlayHint {
                    position: lsp_range.end,
                    label: InlayHintLabel::String(label),
                    kind: Some(InlayHintKind::PARAMETER),
                    text_edits: None,
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: Some(false),
                    data: None,
                });
            }
        }
    }
    hints
}

/// Semantic tokens are intentionally empty for now.
///
/// Every token this used to emit (id, priority, name, description, story,
/// context, dates) merely restated what the tree-sitter grammar already
/// highlights from syntax. That overlay was net-negative: it sits at the LSP
/// priority (125), *above* tree-sitter (100), so it clobbered finer grammar
/// highlights -- most visibly, links nested inside `name`/`description` were
/// repainted as plain strings.
///
/// The right role for semantic tokens is to *augment* the grammar with meaning
/// it cannot compute from syntax alone (overdue dates, dangling predecessor
/// references, blocked actions). That work is scoped in the
/// `semantic-token-augmentation` charter and will need the resolved
/// `DomainModel`, not just the syntax `Tree`. Until then we emit nothing and
/// let tree-sitter own highlighting.
pub fn compute_semantic_tokens(_tree: &Tree) -> Vec<SemanticToken> {
    Vec::new()
}

pub fn get_node_at_position(tree: &Tree, position: Position) -> Option<tree_sitter::Node<'_>> {
    let point = tree_sitter::Point::new(position.line as usize, position.character as usize);
    tree.root_node()
        .named_descendant_for_point_range(point, point)
}

#[cfg(test)]
mod tests {
    use super::*;
    use clearhead_core::parse_document as get_parsed_document;
    use tree_sitter::Parser;

    #[test]
    fn test_lsp_adapter_converts_diagnostics() {
        let text = "[ ] This action has no ID";
        let parsed = get_parsed_document(text).unwrap();

        let diagnostics = compute_diagnostics(&parsed);
        assert_eq!(diagnostics.len(), 1);

        assert!(
            diagnostics
                .iter()
                .all(|d| d.source == Some("clearhead-lsp".to_string()))
        );
        assert!(diagnostics.iter().all(|d| d.code.is_some()));
    }

    #[test]
    fn test_date_completion_items() {
        use chrono::TimeZone;

        let now = Local.with_ymd_and_hms(2026, 1, 15, 10, 30, 0).unwrap();

        let items = date_completion_items(now);
        let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
        let details: Vec<_> = items.iter().filter_map(|i| i.detail.as_deref()).collect();

        assert_eq!(labels, vec!["2026-01-15T10:30", "2026-01-15", "2026-01-16"]);
        assert_eq!(details, vec!["Now", "Today", "Tomorrow"]);
    }

    // Unit tests for compute_code_actions

    #[test]
    fn test_code_action_closes_incomplete_link_without_formatting() {
        let text = concat!(
            "[ ] Read [[docs|https://example.com\n",
            "[ ] Next #019f0000-0000-7000-8000-000000000001",
        );
        let parsed = get_parsed_document(text).unwrap();
        let uri = Uri::from_file_path("/test.actions").unwrap();
        let range = Range::new(Position::new(1, 0), Position::new(1, 0));

        let actions = compute_code_actions(&parsed, &uri, range);
        let close = actions
            .iter()
            .find_map(|action| match action {
                CodeActionOrCommand::CodeAction(action)
                    if action.title == "Close incomplete link" =>
                {
                    Some(action)
                }
                _ => None,
            })
            .expect("missing-link diagnostic should offer a narrow repair");
        let edit = close
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .and_then(|changes| changes.get(&uri))
            .and_then(|edits| edits.first())
            .expect("quick fix edit");
        assert_eq!(edit.range.start, Position::new(1, 0));
        assert_eq!(edit.range.end, Position::new(1, 0));
        assert_eq!(edit.new_text, "]]");
    }

    #[test]
    fn test_code_actions_hydrate_uuid() {
        let text = "[ ] Task without ID";
        let parsed = get_parsed_document(text).unwrap();
        let uri = Uri::from_file_path("/test.actions").unwrap();
        let range = Range::new(Position::new(0, 0), Position::new(0, 0));

        let actions = compute_code_actions(&parsed, &uri, range);

        let titles: Vec<_> = actions
            .iter()
            .filter_map(|a| match a {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.as_str()),
                _ => None,
            })
            .collect();

        assert!(
            titles.contains(&"Hydrate Action (Add UUID)"),
            "Expected hydrate action, got: {:?}",
            titles
        );
    }

    #[test]
    fn test_code_actions_completion_date() {
        let text = "[x] Completed task #019baaec-00b6-7991-be34-94b68212619a";
        let parsed = get_parsed_document(text).unwrap();
        let uri = Uri::from_file_path("/test.actions").unwrap();
        let range = Range::new(Position::new(0, 0), Position::new(0, 0));

        let actions = compute_code_actions(&parsed, &uri, range);

        let titles: Vec<_> = actions
            .iter()
            .filter_map(|a| match a {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.as_str()),
                _ => None,
            })
            .collect();

        assert!(
            titles.contains(&"Set Completion Date (Today)"),
            "Expected completion date action, got: {:?}",
            titles
        );
    }

    #[test]
    fn test_code_actions_cursor_outside_action() {
        let text = "[ ] Task on line 0";
        let parsed = get_parsed_document(text).unwrap();
        let uri = Uri::from_file_path("/test.actions").unwrap();
        let range = Range::new(Position::new(5, 0), Position::new(5, 0));

        let actions = compute_code_actions(&parsed, &uri, range);

        assert!(
            actions.is_empty(),
            "Expected no actions when cursor is outside, got: {:?}",
            actions.len()
        );
    }

    #[test]
    fn test_code_actions_completed_with_date_no_suggestion() {
        let text = "[x] Done task %2026-01-15T10:00 #019baaec-00b6-7991-be34-94b68212619a";
        let parsed = get_parsed_document(text).unwrap();
        let uri = Uri::from_file_path("/test.actions").unwrap();
        let range = Range::new(Position::new(0, 0), Position::new(0, 0));

        let actions = compute_code_actions(&parsed, &uri, range);

        let titles: Vec<_> = actions
            .iter()
            .filter_map(|a| match a {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.as_str()),
                _ => None,
            })
            .collect();

        assert!(
            !titles.contains(&"Set Completion Date (Today)"),
            "Should not suggest completion date when already present"
        );
    }

    // Unit tests for compute_inlay_hints

    #[test]
    fn test_inlay_hints_due_in_future() {
        let text = "[ ] Future task @2026-01-20T10:00 #019baaec-00b6-7991-be34-94b68212619a";
        let parsed = get_parsed_document(text).unwrap();
        let base_time = DateTime::parse_from_rfc3339("2026-01-15T10:00:00+00:00")
            .unwrap()
            .with_timezone(&Local);

        let hints = compute_inlay_hints(&parsed, Some(base_time));

        assert_eq!(hints.len(), 1);
        match &hints[0].label {
            InlayHintLabel::String(s) => {
                assert!(s.contains("due in"), "Expected 'due in', got: {}", s)
            }
            _ => panic!("Expected string label"),
        }
    }

    #[test]
    fn test_inlay_hints_due_in_past() {
        let text = "[ ] Overdue task @2026-01-10T10:00 #019baaec-00b6-7991-be34-94b68212619a";
        let parsed = get_parsed_document(text).unwrap();
        let base_time = DateTime::parse_from_rfc3339("2026-01-15T10:00:00+00:00")
            .unwrap()
            .with_timezone(&Local);

        let hints = compute_inlay_hints(&parsed, Some(base_time));

        assert_eq!(hints.len(), 1);
        match &hints[0].label {
            InlayHintLabel::String(s) => assert!(s.contains("ago"), "Expected 'ago', got: {}", s),
            _ => panic!("Expected string label"),
        }
    }

    #[test]
    fn test_inlay_hints_due_today() {
        let text = "[ ] Today task @2026-01-15T10:00 #019baaec-00b6-7991-be34-94b68212619a";
        let parsed = get_parsed_document(text).unwrap();
        let base_time = DateTime::parse_from_rfc3339("2026-01-15T12:00:00+00:00")
            .unwrap()
            .with_timezone(&Local);

        let hints = compute_inlay_hints(&parsed, Some(base_time));

        assert_eq!(hints.len(), 1);
        match &hints[0].label {
            InlayHintLabel::String(s) => {
                assert!(s.contains("due today"), "Expected 'due today', got: {}", s)
            }
            _ => panic!("Expected string label"),
        }
    }

    #[test]
    fn test_inlay_hints_completed_date() {
        let text = "[x] Done task %2026-01-10T10:00 #019baaec-00b6-7991-be34-94b68212619a";
        let parsed = get_parsed_document(text).unwrap();
        let base_time = DateTime::parse_from_rfc3339("2026-01-15T10:00:00+00:00")
            .unwrap()
            .with_timezone(&Local);

        let hints = compute_inlay_hints(&parsed, Some(base_time));

        assert_eq!(hints.len(), 1);
        match &hints[0].label {
            InlayHintLabel::String(s) => assert!(
                s.contains("done") && s.contains("ago"),
                "Expected 'done X ago', got: {}",
                s
            ),
            _ => panic!("Expected string label"),
        }
    }

    #[test]
    fn test_inlay_hints_no_dates_no_hints() {
        let text = "[ ] Plain task #019baaec-00b6-7991-be34-94b68212619a";
        let parsed = get_parsed_document(text).unwrap();

        let hints = compute_inlay_hints(&parsed, None);

        assert!(hints.is_empty(), "Expected no hints for task without dates");
    }

    // Unit tests for compute_semantic_tokens

    fn get_tree(text: &str) -> Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_actions::LANGUAGE.into())
            .unwrap();
        parser.parse(text, None).unwrap()
    }

    // Semantic tokens are intentionally inert until the augmentation charter
    // lands: they must add meaning tree-sitter can't (overdue/dangling/blocked),
    // not re-emit syntax tree-sitter already highlights. Guard against a
    // regression that reintroduces redundant, link-clobbering overlays.
    #[test]
    fn test_semantic_tokens_are_empty_until_augmentation() {
        let text = "[ ] Task !2 +home @2026-01-20T10:00 #019baaec-00b6-7991-be34-94b68212619a";
        let tree = get_tree(text);

        let tokens = compute_semantic_tokens(&tree);

        assert!(
            tokens.is_empty(),
            "Expected no semantic tokens (grammar owns highlighting), got {} tokens",
            tokens.len()
        );
    }
}
