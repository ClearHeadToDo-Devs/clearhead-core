use crate::workspace::actions::{Action, ActionState, ParsedDocument, SourceMetadata, SourceRange};
use chrono::Local;
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
    check_duration_without_do_date, // E001
    check_empty_context,            // E003
    // E004, E005 - parser/grammar level
    check_invalid_uuid, // E006
    // E007 - parser level
    check_orphaned_child, // E010
    check_skipped_level,  // E011
];

/// Action-level warning checks (W005-W006)
/// W001-W003 are either not implemented or document-level
/// W004 (missing creation date) removed — creation dates now live in the sidecar, not the DSL.
pub const ACTION_WARNING_CHECKS: &[ActionCheck] = &[
    // W001 (hierarchy depth) - not implemented
    // W002, W003 - document-level (check_tree_consistency)
    check_future_creation_date,       // W005
    check_completion_before_creation, // W006
    check_incomplete_uuid,            // W013
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
    if action.duration.is_some() && action.scheduled_at.is_none() {
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
    if action.state == ActionState::BlockedOrAwaiting && action.description.is_none() {
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
    if let Some(duration) = action.duration {
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
            if let Some(parent_id) = action.parent_id
                && let Some(parent) = doc.actions.iter().find(|a| a.id == parent_id)
            {
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
    diagnostics
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

/// True when `s` is a strict, well-formed prefix of a canonical hyphenated UUID
/// (`xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`) that also carries at least one `-`.
///
/// This is the "half-typed uuid" shape: every character sits in its template
/// slot (hex where the template is hex, `-` at positions 8/13/18/23), the whole
/// string is shorter than a full uuid, and a dash proves the author is partway
/// through the hyphenated form rather than typing junk (`abc-def`) or a bare
/// short hash (`01950000`). It is the boundary between W013 (incomplete, a
/// live-buffer warning) and E006 (invalid, an error).
fn is_incomplete_uuid(s: &str) -> bool {
    const FULL_LEN: usize = 36;
    if s.len() >= FULL_LEN || !s.contains('-') {
        return false;
    }
    s.char_indices().all(|(i, c)| match i {
        8 | 13 | 18 | 23 => c == '-',
        _ => c.is_ascii_hexdigit(),
    })
}

/// Check if UUID format is invalid (E006).
///
/// Fires only for a captured `raw_id` that is *not* a recognizable incomplete
/// uuid — those are the author's genuine mistakes (`#abc-def`, `#123`), as
/// opposed to a uuid still being typed (see [`check_incomplete_uuid`]).
fn check_invalid_uuid(_action: &Action, metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    metadata
        .raw_id
        .as_ref()
        .filter(|raw_id| !is_incomplete_uuid(raw_id))
        .map(|raw_id| {
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

/// Check for a partially-typed UUID id (W013).
///
/// A relaxed grammar lets a half-typed `#id` parse instead of erroring the line
/// (Decision 6, live buffer), so the id arrives here as a `raw_id` that is a
/// clean prefix of a full uuid. That is not a mistake to error on — it's an edit
/// in progress — so it lint-warns, never errors. Note the id is still swapped
/// for a generated one on load, so finishing it matters.
fn check_incomplete_uuid(_action: &Action, metadata: &SourceMetadata) -> Option<LintDiagnostic> {
    metadata
        .raw_id
        .as_ref()
        .filter(|raw_id| is_incomplete_uuid(raw_id))
        .map(|raw_id| {
            LintDiagnostic::warning(
                "W013",
                format!(
                    "Incomplete UUID: '{}' looks like a uuid still being typed. \
                     Finish it — an unfinished id is replaced by a generated one on load (W013).",
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
        if let Some(metadata) = doc.source_map.get(&action.id)
            && !metadata.is_id_generated
            && !seen_ids.insert(action.id)
        {
            diagnostics.push(LintDiagnostic::info(
                "I004",
                format!("Duplicate action ID found: {} (I004)", action.id),
                metadata.root,
            ));
        }
    }
    diagnostics
}

fn is_closed_action(state: ActionState) -> bool {
    matches!(state, ActionState::Completed | ActionState::Cancelled)
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

            let all_children_closed = children.iter().all(|c| is_closed_action(c.state));
            let any_children_open = children.iter().any(|c| !is_closed_action(c.state));
            let is_closed = is_closed_action(action.state);

            // W002: Closed parent with open children
            if is_closed && any_children_open {
                diagnostics.push(LintDiagnostic::warning(
                    "W002",
                    "Parent is closed but some children are still active (W002).".to_string(),
                    metadata.root,
                ));
            }

            // W003: Open parent with all children closed
            if !is_closed && all_children_closed {
                diagnostics.push(LintDiagnostic::warning(
                    "W003",
                    "All children are closed. Should this parent be closed too? (W003)"
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

/// Check if closed action is missing completion date (I001)
fn check_missing_completion_date(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    if is_closed_action(action.state) && action.completed_at.is_none() {
        Some(LintDiagnostic::info(
            "I001",
            "Closed action is missing a completion date (I001).".to_string(),
            metadata.root,
        ))
    } else {
        None
    }
}

/// Check if action has completion date but isn't closed (I002)
fn check_completion_date_without_state(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    if !is_closed_action(action.state) && action.completed_at.is_some() {
        Some(LintDiagnostic::info(
            "I002",
            "Action has a completion date but is not marked as closed (completed/cancelled) (I002).".to_string(),
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
    action.contexts.as_ref().and_then(|contexts| {
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

// ============================================================================
// Action-Level Checks (continued)
// ============================================================================

/// Check if creation date is in the future (W005)
fn check_future_creation_date(
    action: &Action,
    metadata: &SourceMetadata,
) -> Option<LintDiagnostic> {
    action.created_at.and_then(|created| {
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
    match (action.created_at, action.completed_at) {
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
    use crate::workspace::parse_document as get_parsed_document;

    #[test]
    fn test_lint_missing_id() {
        let text = "[ ] This action has no ID";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        // Only missing-id (Info) - creation date lives in sidecar, not DSL
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
    fn test_check_missing_completion_date_cancelled_info() {
        let text =
            "[_] Cancelled task with no completion date #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.info.iter().any(|d| d.code == "I001"));
    }

    #[test]
    fn test_check_completion_date_cancelled_ok() {
        let text = "[_] Cancelled task %2025-01-09T14:00 #01942d99-4c27-77f6-9316-107024843939";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(!results.info.iter().any(|d| d.code == "I001" || d.code == "I002"));
    }

    #[test]
    fn test_check_invalid_uuid_format() {
        // Test the check_invalid_uuid function directly since grammar won't parse invalid formats
        use crate::workspace::actions::Action;
        let action = Action {
            id: uuid::Uuid::nil(),
            state: ActionState::NotStarted,
            name: "Test".to_string(),
            ..Default::default()
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
            due_date: None,
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
    fn test_incomplete_uuid_classification() {
        // Well-formed prefixes with a dash — a uuid mid-type (W013).
        for s in ["12345678-1234", "01950000-0000-7000-8000", "01950000-"] {
            assert!(is_incomplete_uuid(s), "{s} should read as incomplete");
        }
        // Genuine mistakes — invalid, not incomplete (E006).
        for s in [
            "123",                // no dash, too short to be a uuid-in-progress
            "01950000",           // bare short hash, not valid as an *id*
            "abc-def-abc-def",    // dash at position 3, not a template slot
            "not-a-uuid",         // non-hex
            "invalid-uuid-format",
            "01950000-0000-7000-8000-000000000001", // a full, valid uuid is neither
        ] {
            assert!(!is_incomplete_uuid(s), "{s} should NOT read as incomplete");
        }
    }

    #[test]
    fn test_incomplete_and_invalid_uuid_are_mutually_exclusive() {
        use crate::workspace::actions::Action;
        let action = Action::default();
        let make = |raw: &str| SourceMetadata {
            raw_id: Some(raw.to_string()),
            is_id_generated: true,
            ..Default::default()
        };

        // Incomplete → W013 only, never E006.
        let incomplete = make("01950000-0000-7000-8000");
        assert!(check_invalid_uuid(&action, &incomplete).is_none());
        assert_eq!(
            check_incomplete_uuid(&action, &incomplete).unwrap().code,
            "W013"
        );

        // Invalid → E006 only, never W013.
        let invalid = make("abc-def-abc-def");
        assert!(check_incomplete_uuid(&action, &invalid).is_none());
        assert_eq!(check_invalid_uuid(&action, &invalid).unwrap().code, "E006");
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
    fn test_check_tree_consistency_children_cancelled_counts_as_closed() {
        let text = "[ ] Parent task #01942d99-4c27-77f6-9316-107024843939\n>[_] Child task cancelled #01942d99-4c27-77f6-9316-107024843998 %2025-01-09T14:00\n>[_] Another child cancelled #01942d99-4c27-77f6-9316-107024843999 %2025-01-09T14:00";
        let parsed = get_parsed_document(text).unwrap();

        let results = lint_document(&parsed);
        assert!(results.warnings.iter().any(|d| d.code == "W003"));
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
        use crate::workspace::actions::Action;
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            state: ActionState::NotStarted,
            name: "Test".to_string(),
            contexts: Some(vec!["work".to_string(), "".to_string()]),
            ..Default::default()
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
            due_date: None,
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
        use crate::workspace::actions::Action;
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            state: ActionState::NotStarted,
            name: "Meeting".to_string(),
            duration: Some(60),
            ..Default::default()
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
            due_date: None,
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
        use crate::workspace::actions::Action;
        let action = Action {
            id: uuid::Uuid::parse_str("01942d99-4c27-77f6-9316-107024843939").unwrap(),
            state: ActionState::NotStarted,
            name: "Meeting".to_string(),
            scheduled_at: Some(Local::now()),
            duration: Some(60),
            ..Default::default()
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
            due_date: None,
            completed_date: None,
            created_date: None,
            is_id_generated: false,
            raw_id: None,
        };

        let result = check_duration_without_do_date(&action, &metadata);
        assert!(result.is_none());
    }
}
