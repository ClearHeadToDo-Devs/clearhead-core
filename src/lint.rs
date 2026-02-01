use crate::entities::{parse_action_recursive, Action, ActionState};
use crate::treesitter::{create_node_wrapper, TreeWrapper};
use crate::{ParsedDocument, SourceMetadata, SourceRange};
use chrono::Local;
use std::collections::HashMap;
use std::collections::HashSet;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LintSeverity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LintDiagnostic {
    pub code: String,
    pub severity: LintSeverity,
    pub message: String,
    pub range: SourceRange,
}

impl LintDiagnostic {
    pub fn new(code: &str, severity: LintSeverity, message: String, range: SourceRange) -> Self {
        Self {
            code: code.to_string(),
            severity,
            message,
            range,
        }
    }

    pub fn error(code: &str, message: String, range: SourceRange) -> Self {
        Self::new(code, LintSeverity::Error, message, range)
    }

    pub fn warning(code: &str, message: String, range: SourceRange) -> Self {
        Self::new(code, LintSeverity::Warning, message, range)
    }

    pub fn info(code: &str, message: String, range: SourceRange) -> Self {
        Self::new(code, LintSeverity::Info, message, range)
    }
}

/// Structured lint results grouped by severity
#[derive(Debug, Clone, Default)]
pub struct LintResults {
    pub errors: Vec<LintDiagnostic>,
    pub warnings: Vec<LintDiagnostic>,
    pub info: Vec<LintDiagnostic>,
}

/// IntoIterator for ergonomic `for diag in results` usage
impl IntoIterator for LintResults {
    type Item = LintDiagnostic;
    type IntoIter = std::vec::IntoIter<LintDiagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        let mut all = Vec::with_capacity(self.errors.len() + self.warnings.len() + self.info.len());
        all.extend(self.errors);
        all.extend(self.warnings);
        all.extend(self.info);
        all.into_iter()
    }
}

// ============================================================================
// Check Types
// ============================================================================

/// Action-level check: operates on a single action and its metadata
pub type ActionCheck = fn(&Action, &SourceMetadata) -> Option<LintDiagnostic>;

/// Document-level check: operates on the entire parsed document
pub type DocumentCheck = fn(&ParsedDocument) -> Vec<LintDiagnostic>;

// ============================================================================
// Check Registries - Indexed by Spec Code
// ============================================================================

/// Action-level error checks (E001-E007)
/// Gaps: E004 (orphaned child), E005 (skipped level), E007 (no state) - parser handles these
pub const ACTION_ERROR_CHECKS: &[ActionCheck] = &[
    check_duration_without_do_date,   // E001
    check_recurrence_without_do_date, // E002
    check_empty_context,              // E003
    // E004, E005 - parser/grammar level
    check_invalid_uuid, // E006
    // E007 - parser level
    check_orphaned_child, // E010
    check_skipped_level,  // E011
];

/// Action-level warning checks (W004-W006)
/// W001-W003 are either not implemented or document-level
pub const ACTION_WARNING_CHECKS: &[ActionCheck] = &[
    // W001 (hierarchy depth) - not implemented
    // W002, W003 - document-level (check_tree_consistency)
    check_missing_creation_date,      // W004
    check_future_creation_date,       // W005
    check_completion_before_creation, // W006
];

/// Action-level info checks (I001-I003)
/// I004 (duplicate ID) is document-level
pub const ACTION_INFO_CHECKS: &[ActionCheck] = &[
    check_missing_completion_date,       // I001
    check_completion_date_without_state, // I002
    check_priority_range,                // I003
    check_missing_id,                    // missing-id (style check)
    check_blocked_without_description,   // I008
    check_excessive_duration,            // I010
];

/// Document-level checks (operate on whole document)
pub const DOCUMENT_CHECKS: &[DocumentCheck] = &[
    check_tree_consistency, // W002, W003
    check_duplicate_ids,    // I004
    check_hierarchy_levels, // E010, E011, W001
];

// ============================================================================
// Action-Level Checks
// ============================================================================

/// Check if duration is present without a do-date (E001)
fn check_duration_without_do_date(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    if action.do_duration.is_some() && action.do_date_time.is_none() {
        Some(LintDiagnostic::error(
            "E001",
            "Duration requires a do-date to be meaningful (E001).".to_string(),
            metadata.root,
        ))
    } else {
        None
    }
}

/// Placeholder for orphaned child check (E010) - implemented at document level
fn check_orphaned_child(_action: &Action, _metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    None
}

/// Placeholder for skipped level check (E011) - implemented at document level
fn check_skipped_level(_action: &Action, _metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    None
}

/// Check if action is blocked but missing description (I008)
fn check_blocked_without_description(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    if action.state == ActionState::BlockedorAwaiting && action.description.is_none() {
        Some(LintDiagnostic::info(
            "I008",
            "Blocked actions should have a description explaining why (I008).".to_string(),
            metadata.root,
        ))
    } else {
        None
    }
}

/// Check if duration is excessive (I010)
fn check_excessive_duration(action: &Action, metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    const MAX_DURATION: u32 = 480; // 8 hours
    if let Some(duration) = action.do_duration {
        if duration > MAX_DURATION {
            Some(LintDiagnostic::info(
                "I010",
                format!(
                    "Suspiciously long duration ({} minutes). Is this correct? (I010)",
                    duration
                ),
                metadata.do_date.unwrap_or(metadata.root),
            ))
        } else {
            None
        }
    } else {
        None
    }
}

/// Check hierarchy levels (E010, E011, W001)
fn check_hierarchy_levels(doc: &ParsedDocument) -> Vec<LintDiagnostic> {
    let mut diagnostics = Vec::new();

    for action in &doc.actions {
        if let Some(metadata) = doc.source_map.get(&action.id) {
            let depth = action.depth(&doc.actions);

            // W001: Hierarchy Depth Exceeded (max 5)
            if depth > 5 {
                diagnostics.push(LintDiagnostic::warning(
                    "W001",
                    format!(
                        "Action depth {} exceeds recommended maximum of 5 (W001).",
                        depth
                    ),
                    metadata.root,
                ));
            }

            // E011: Skipped Hierarchy Level
            if let Some(parent_id) = action.parent_id {
                if let Some(parent) = doc.actions.iter().find(|a| a.id == parent_id) {
                    let parent_depth = parent.depth(&doc.actions);
                    if depth > parent_depth + 1 {
                        diagnostics.push(LintDiagnostic::error(
                            "E011",
                            format!(
                                "Skipped hierarchy level: parent is depth {}, child is depth {} (E011).",
                                parent_depth, depth
                            ),
                            metadata.root,
                        ));
                    }
                }
            }
        }
    }
    diagnostics
}
impl TryFrom<TreeWrapper> for ParsedDocument {
    type Error = String;
    fn try_from(value: TreeWrapper) -> Result<Self, Self::Error> {
        let root = value.tree.root_node();
        let mut action_list = Vec::new();
        let mut source_map = HashMap::new();
        let mut tag_index = HashMap::new();
        let mut syntax_errors = Vec::new();
        let mut cursor = root.walk();

        // Collect syntax errors (ERROR and MISSING nodes)
        if root.has_error() {
            let mut stack = vec![root];
            while let Some(node) = stack.pop() {
                if node.is_error() || node.is_missing() {
                    let start = node.start_position();
                    let end = node.end_position();
                    let message = if node.is_missing() {
                        format!("missing '{}'", node.kind())
                    } else {
                        "unexpected token".to_string()
                    };
                    syntax_errors.push(LintDiagnostic::error(
                        "syntax-error",
                        message,
                        SourceRange {
                            start_row: start.row,
                            start_col: start.column,
                            end_row: end.row,
                            end_col: end.column,
                        },
                    ));
                }
                // Don't recurse into errors themselves, just find the top-most ones
                if !node.is_error() {
                    for child in node.children(&mut cursor) {
                        stack.push(child);
                    }
                }
            }
        }

        // Iterate through all root actions
        for root_action in root.children(&mut cursor) {
            if root_action.kind() == "root_action" {
                let wrapper = create_node_wrapper(root_action, value.source.clone());
                action_list.extend(
                    parse_action_recursive(wrapper, None, &mut source_map, &mut tag_index)
                        .map_err(|e| e.to_string())?,
                );
            }
        }

        Ok(ParsedDocument {
            actions: action_list,
            source_map,
            tag_index,
            syntax_errors,
        })
    }
}

/// Check if recurrence is present without a do-date (E002)
fn check_recurrence_without_do_date(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    if action.recurrence.is_some() && action.do_date_time.is_none() {
        Some(LintDiagnostic::error(
            "E002",
            "Recurrence requires a do-date as the starting point (E002).".to_string(),
            metadata.root,
        ))
    } else {
        None
    }
}

/// Check if action is missing a UUID (Info - style preference)
fn check_missing_id(_action: &Action, metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    if metadata.is_id_generated && metadata.raw_id.is_none() {
        Some(LintDiagnostic::info(
            "missing-id",
            "Action is missing a UUID. Use 'Hydrate Action' to add one.".to_string(),
            metadata.root,
        ))
    } else {
        None
    }
}

/// Check if UUID format is invalid (E006)
fn check_invalid_uuid(_action: &Action, metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    metadata.raw_id.as_ref().map(|raw_id| {
        LintDiagnostic::error(
            "E006",
            format!(
                "Invalid UUID format: '{}'. UUIDs must follow standard format (E006).",
                raw_id
            ),
            metadata.root,
        )
    })
}

// ============================================================================
// Document-Level Checks
// ============================================================================

/// Check for duplicate UUIDs across the document (I004)
fn check_duplicate_ids(doc: &ParsedDocument) -> Vec<LintDiagnostic> {
    let mut diagnostics = Vec::new();
    let mut seen_ids = HashSet::<Uuid>::new();

    for action in &doc.actions {
        if let Some(metadata) = doc.source_map.get(&action.id) {
            if !metadata.is_id_generated && !seen_ids.insert(action.id) {
                diagnostics.push(LintDiagnostic::info(
                    "I004",
                    format!("Duplicate action ID found: {} (I004)", action.id),
                    metadata.root,
                ));
            }
        }
    }
    diagnostics
}

/// Check tree consistency rules across the document (W002, W003)
fn check_tree_consistency(doc: &ParsedDocument) -> Vec<LintDiagnostic> {
    let mut diagnostics = Vec::new();

    for action in &doc.actions {
        if let Some(metadata) = doc.source_map.get(&action.id) {
            let children: Vec<_> = doc
                .actions
                .iter()
                .filter(|a| a.parent_id == Some(action.id))
                .collect();

            if children.is_empty() {
                continue;
            }

            let all_children_completed = children.iter().all(|c| c.state == ActionState::Completed);
            let any_children_uncompleted =
                children.iter().any(|c| c.state != ActionState::Completed);
            let is_completed = action.state == ActionState::Completed;

            // W002: Completed parent with uncompleted children
            if is_completed && any_children_uncompleted {
                diagnostics.push(LintDiagnostic::warning(
                    "W002",
                    "Parent is completed but some children are still active (W002).".to_string(),
                    metadata.root,
                ));
            }

            // W003: Uncompleted parent with all children completed
            if !is_completed && all_children_completed {
                diagnostics.push(LintDiagnostic::warning(
                    "W003",
                    "All children are completed. Should this parent be completed too? (W003)"
                        .to_string(),
                    metadata.root,
                ));
            }
        }
    }
    diagnostics
}

// ============================================================================
// Action-Level Checks (continued)
// ============================================================================

/// Check if completed action is missing completion date (I001)
fn check_missing_completion_date(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    if action.state == ActionState::Completed && action.completed_date_time.is_none() {
        Some(LintDiagnostic::info(
            "I001",
            "Completed action is missing a completion date (I001).".to_string(),
            metadata.root,
        ))
    } else {
        None
    }
}

/// Check if action has completion date but isn't completed (I002)
fn check_completion_date_without_state(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    if action.state != ActionState::Completed && action.completed_date_time.is_some() {
        Some(LintDiagnostic::info(
            "I002",
            "Action has a completion date but is not marked as completed (I002).".to_string(),
            metadata.completed_date.unwrap_or(metadata.root),
        ))
    } else {
        None
    }
}

/// Check if priority is in valid range 1-5 (I003)
fn check_priority_range(action: &Action, metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    action.priority.and_then(|priority| {
        if priority == 0 || priority > 5 {
            Some(LintDiagnostic::info(
                "I003",
                format!("Priority must be 1-5 (got {}) (I003).", priority),
                metadata.root,
            ))
        } else {
            None
        }
    })
}

/// Check for empty context tags (E003)
fn check_empty_context(action: &Action, metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    action.context_list.as_ref().and_then(|contexts| {
        if contexts.iter().any(|c| c.is_empty()) {
            Some(LintDiagnostic::error(
                "E003",
                "Context tags cannot be empty (E003).".to_string(),
                metadata.root,
            ))
        } else {
            None
        }
    })
}

/// Check if action is missing creation date (W004)
fn check_missing_creation_date(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    let is_v7 = !metadata.is_id_generated
        && action.id.get_variant() == uuid::Variant::RFC4122
        && action.id.get_version_num() == 7;

    if action.created_date_time.is_none() && !is_v7 {
        Some(LintDiagnostic::warning(
            "W004",
            "Action is missing a creation date. Consider adding ^ or using UUIDv7 (W004)."
                .to_string(),
            metadata.root,
        ))
    } else {
        None
    }
}

// ============================================================================
// Action-Level Checks (continued)
// ============================================================================

/// Check if creation date is in the future (W005)
fn check_future_creation_date(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    action.created_date_time.and_then(|created| {
        if created > Local::now() {
            Some(LintDiagnostic::warning(
                "W005",
                "Creation date cannot be in the future (W005).".to_string(),
                metadata.created_date.unwrap_or(metadata.root),
            ))
        } else {
            None
        }
    })
}

/// Check if completion date is before creation date (W006)
fn check_completion_before_creation(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    match (action.created_date_time, action.completed_date_time) {
        (Some(created), Some(completed)) if completed < created => Some(LintDiagnostic::warning(
            "W006",
            "Completion date cannot be before creation date (W006).".to_string(),
            metadata.completed_date.unwrap_or(metadata.root),
        )),
        _ => None,
    }
}

/// Lint a parsed document and return structured results grouped by severity
pub fn lint_document(doc: &ParsedDocument) -> LintResults {
    let mut results = LintResults::default();

    // Include syntax errors from parsing phase
    results.errors.extend(doc.syntax_errors.clone());

    // Action-level checks: iterate over actions, filter_map through check registries
    for action in &doc.actions {
        if let Some(metadata) = doc.source_map.get(&action.id) {
            results.errors.extend(
                ACTION_ERROR_CHECKS
                    .iter()
                    .filter_map(|check| check(action, metadata)),
            );
            results.warnings.extend(
                ACTION_WARNING_CHECKS
                    .iter()
                    .filter_map(|check| check(action, metadata)),
            );
            results.info.extend(
                ACTION_INFO_CHECKS
                    .iter()
                    .filter_map(|check| check(action, metadata)),
            );
        }
    }

    // Document-level checks: flat_map through registry, route by severity
    for check in DOCUMENT_CHECKS {
        for diag in check(doc) {
            match diag.severity {
                LintSeverity::Error => results.errors.push(diag),
                LintSeverity::Warning => results.warnings.push(diag),
                LintSeverity::Info => results.info.push(diag),
            }
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::get_parsed_document;

    #[test]
    fn test_lint_missing_id() {
        let text = "[ ] This action has no ID";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        // Only missing-id (Info) - created date is auto-injected when ID is generated
        assert_eq!(results.info.len(), 1);
        assert!(results.errors.is_empty());
        assert!(results.warnings.is_empty());

        // Find the missing-id diagnostic
        let uuid_diag = results
            .info
            .iter()
            .find(|d| d.code == "missing-id")
            .unwrap();
        assert_eq!(uuid_diag.severity, LintSeverity::Info);
        assert!(uuid_diag.message.contains("missing a UUID"));
    }

    #[test]
    fn test_lint_has_id() {
        let text = "[ ] This action has an ID #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.errors.is_empty());
        assert!(results.warnings.is_empty());
        assert!(results.info.is_empty());
    }

    // ========================================================================
    // Individual Rule Tests
    // ========================================================================

    #[test]
    fn test_check_missing_completion_date_info() {
        let text =
            "[x] Completed task with no completion date #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.info.iter().any(|d| d.code == "I001"));
        let diag = results.info.iter().find(|d| d.code == "I001").unwrap();
        assert_eq!(diag.severity, LintSeverity::Info);
    }

    #[test]
    fn test_check_missing_completion_date_ok() {
        let text = "[x] Completed task %2025-01-09T14:00 #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(!results.info.iter().any(|d| d.code == "I001"));
    }

    #[test]
    fn test_check_invalid_uuid_format() {
        // Test the check_invalid_uuid function directly since grammar won't parse invalid formats
        use crate::entities::Action;
        let action = Action {
            id: uuid::Uuid::nil(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Test".to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: None,
            do_duration: None,
            recurrence: None,
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        };
        let metadata = SourceMetadata {
            root: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 10,
            },
            line_range: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 10,
            },
            do_date: None,
            completed_date: None,
            created_date: None,
            is_id_generated: false,
            raw_id: Some("invalid-uuid-format".to_string()),
        };

        let result = check_invalid_uuid(&action, &metadata);
        assert!(result.is_some());
        assert_eq!(result.unwrap().code, "E006");
    }

    #[test]
    fn test_check_duplicate_id_detection() {
        let text = "[ ] Task 1 #01942d99-4c27-77f6-9316-107024843939\n[ ] Task 2 #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.info.iter().any(|d| d.code == "I004"));
        let diag = results.info.iter().find(|d| d.code == "I004").unwrap();
        assert_eq!(diag.severity, LintSeverity::Info);
    }

    #[test]
    fn test_check_tree_consistency_parent_completed() {
        let text = "[x] Parent task #01942d99-4c27-77f6-9316-107024843939 %2025-01-09T14:00\n>[ ] Child task still open #01942d99-4c27-77f6-9316-107024843999";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.warnings.iter().any(|d| d.code == "W002"));
        let diag = results.warnings.iter().find(|d| d.code == "W002").unwrap();
        assert_eq!(diag.severity, LintSeverity::Warning);
    }

    #[test]
    fn test_check_tree_consistency_children_completed() {
        let text = "[ ] Parent task #01942d99-4c27-77f6-9316-107024843939\n>[x] Child task completed #01942d99-4c27-77f6-9316-107024843998 %2025-01-09T14:00\n>[x] Another child completed #01942d99-4c27-77f6-9316-107024843999 %2025-01-09T14:00";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.warnings.iter().any(|d| d.code == "W003"));
        let diag = results.warnings.iter().find(|d| d.code == "W003").unwrap();
        assert_eq!(diag.severity, LintSeverity::Warning);
    }

    #[test]
    fn test_check_priority_range_invalid_zero() {
        let text = "[ ] Task with priority 0 !0 #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.info.iter().any(|d| d.code == "I003"));
        let diag = results.info.iter().find(|d| d.code == "I003").unwrap();
        assert_eq!(diag.severity, LintSeverity::Info);
    }

    #[test]
    fn test_check_priority_range_invalid_high() {
        let text = "[ ] Task with priority 6 !6 #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.info.iter().any(|d| d.code == "I003"));
    }

    #[test]
    fn test_check_priority_range_valid() {
        let text = "[ ] Task with priority 3 !3 #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(!results.info.iter().any(|d| d.code == "I003"));
    }

    #[test]
    fn test_check_empty_context_tag() {
        // Test the check_empty_context function directly since grammar may prevent empty tags
        use crate::entities::Action;
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Test".to_string(),
            description: None,
            priority: None,
            context_list: Some(vec!["work".to_string(), "".to_string()]),
            do_date_time: None,
            do_duration: None,
            recurrence: None,
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        };
        let metadata = SourceMetadata {
            root: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 10,
            },
            line_range: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 10,
            },
            do_date: None,
            completed_date: None,
            created_date: None,
            is_id_generated: false,
            raw_id: None,
        };

        let result = check_empty_context(&action, &metadata);
        assert!(result.is_some());
        let diag = result.unwrap();
        assert_eq!(diag.code, "E003");
        assert_eq!(diag.severity, LintSeverity::Error);
    }

    #[test]
    fn test_check_creation_date_future() {
        use chrono::Duration;
        let future = Local::now() + Duration::days(1);
        let text = format!(
            "[ ] Task from the future ^{} #01942d99-4c27-77f6-9316-107024843939",
            future.format("%Y-%m-%dT%H:%M")
        );
        let parsed = get_parsed_document(&text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.warnings.iter().any(|d| d.code == "W005"));
        let diag = results.warnings.iter().find(|d| d.code == "W005").unwrap();
        assert_eq!(diag.severity, LintSeverity::Warning);
    }

    #[test]
    fn test_check_completion_before_creation() {
        let text = "[ ] Task with wrong dates ^2025-01-10T14:00 %2025-01-09T14:00 #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.warnings.iter().any(|d| d.code == "W006"));
        let diag = results.warnings.iter().find(|d| d.code == "W006").unwrap();
        assert_eq!(diag.severity, LintSeverity::Warning);
    }

    // ========================================================================
    // New E001/E002 Tests (Duration and Recurrence without Do-Date)
    // ========================================================================

    #[test]
    fn test_check_duration_without_do_date() {
        // Test the function directly since parsing D without @ may not be valid in grammar
        use crate::entities::Action;
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Meeting".to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: None,    // No do-date
            do_duration: Some(60), // Has duration
            recurrence: None,
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        };
        let metadata = SourceMetadata {
            root: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            line_range: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            do_date: None,
            completed_date: None,
            created_date: None,
            is_id_generated: false,
            raw_id: None,
        };

        let result = check_duration_without_do_date(&action, &metadata);
        assert!(result.is_some());
        let diag = result.unwrap();
        assert_eq!(diag.code, "E001");
        assert_eq!(diag.severity, LintSeverity::Error);
    }

    #[test]
    fn test_check_duration_with_do_date_ok() {
        use crate::entities::Action;
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Meeting".to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: Some(Local::now()), // Has do-date
            do_duration: Some(60),            // Has duration
            recurrence: None,
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        };
        let metadata = SourceMetadata {
            root: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            line_range: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            do_date: None,
            completed_date: None,
            created_date: None,
            is_id_generated: false,
            raw_id: None,
        };

        let result = check_duration_without_do_date(&action, &metadata);
        assert!(result.is_none());
    }

    #[test]
    fn test_check_recurrence_without_do_date() {
        use crate::entities::{Action, Recurrence};
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Daily standup".to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: None, // No do-date
            do_duration: None,
            recurrence: Some(Recurrence {
                // Has recurrence
                frequency: "daily".to_string(),
                interval: None,
                count: None,
                until: None,
                by_second: None,
                by_minute: None,
                by_hour: None,
                by_day: None,
                by_month_day: None,
                by_year_day: None,
                by_week_no: None,
                by_month: None,
                by_set_pos: None,
                week_start: None,
            }),
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        };
        let metadata = SourceMetadata {
            root: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            line_range: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            do_date: None,
            completed_date: None,
            created_date: None,
            is_id_generated: false,
            raw_id: None,
        };

        let result = check_recurrence_without_do_date(&action, &metadata);
        assert!(result.is_some());
        let diag = result.unwrap();
        assert_eq!(diag.code, "E002");
        assert_eq!(diag.severity, LintSeverity::Error);
    }

    #[test]
    fn test_check_recurrence_with_do_date_ok() {
        use crate::entities::{Action, Recurrence};
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Daily standup".to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: Some(Local::now()), // Has do-date
            do_duration: None,
            recurrence: Some(Recurrence {
                // Has recurrence
                frequency: "daily".to_string(),
                interval: None,
                count: None,
                until: None,
                by_second: None,
                by_minute: None,
                by_hour: None,
                by_day: None,
                by_month_day: None,
                by_year_day: None,
                by_week_no: None,
                by_month: None,
                by_set_pos: None,
                week_start: None,
            }),
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        };
        let metadata = SourceMetadata {
            root: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            line_range: SourceRange {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 20,
            },
            do_date: None,
            completed_date: None,
            created_date: None,
            is_id_generated: false,
            raw_id: None,
        };

        let result = check_recurrence_without_do_date(&action, &metadata);
        assert!(result.is_none());
    }
}
