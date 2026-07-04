use super::source::{
    NodeWrapper, SourceMetadata, SourceRange, create_node_wrapper, get_node_text, get_prefixed_text,
};
use crate::domain::{Action, ActionState, PredecessorRef};
use chrono::{DateTime, Local};
use std::collections::HashMap;
use std::fmt;
use tree_sitter::Tree;
use uuid::Uuid;

/// A collection of [`Action`]s, typically representing a parsed `.actions` document.
pub type ActionList = Vec<Action>;

/// Sigils the formatter escapes inside a freeform `safe_text` field (name,
/// story/charter ref, predecessor ref) so they read back as literal text
/// rather than starting a metadata field. Brackets are absent — they belong to
/// `[[link]]` syntax, which [`escape_field`] copies verbatim.
const NAME_ESCAPE: &[char] = &[
    '\\', '$', '!', '*', '+', '@', '%', '^', '#', '>', '<', '~', '=', ':',
];

/// Sigils a description body escapes: only `$` (its block delimiter) and the
/// backslash itself — a description already tolerates every other sigil.
const DESC_ESCAPE: &[char] = &['\\', '$'];

/// Whether a backslash may escape `c` when reading a field back — the grammar's
/// escape set: every metadata sigil, both brackets, and the backslash itself.
/// One uniform set is safe for every field: the per-field escaped forms are all
/// subsets of it, so unescaping only ever undoes a backslash we (or the grammar)
/// legitimately put there.
fn is_escapable(c: char) -> bool {
    matches!(
        c,
        '\\' | '$' | '!' | '*' | '+' | '@' | '%' | '^' | '#' | '>' | '<' | '~' | '=' | ':' | '[' | ']'
    )
}

/// Escape `reserved` sigils for the on-disk form, leaving `[[link]]` spans
/// untouched (a URL's `:` or `=` must not be escaped or the link breaks).
fn escape_field(s: &str, reserved: &[char]) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        // A `[[` opens a link — copy through the closing `]]` verbatim.
        if c == '[' && chars.peek() == Some(&'[') {
            out.push('[');
            out.push('[');
            chars.next();
            while let Some(n) = chars.next() {
                out.push(n);
                if n == ']' && chars.peek() == Some(&']') {
                    out.push(']');
                    chars.next();
                    break;
                }
            }
            continue;
        }
        if reserved.contains(&c) {
            out.push('\\');
        }
        out.push(c);
    }
    out
}

/// Escape a `safe_text` field — name, story/charter ref, predecessor ref.
pub(crate) fn escape_name(s: &str) -> String {
    escape_field(s, NAME_ESCAPE)
}

/// Escape a description body (only `$` and the backslash itself need it).
pub(crate) fn escape_description(s: &str) -> String {
    escape_field(s, DESC_ESCAPE)
}

/// Inverse of the escape functions: drop a backslash that escapes an escapable
/// char, keeping the char literally. A backslash before a non-escapable char
/// stays (a lenient read, matching the grammar). Safe to run over a whole field
/// — link spans hold no backslashes to disturb.
pub(crate) fn unescape_field(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' && chars.peek().is_some_and(|&n| is_escapable(n)) {
            out.push(chars.next().unwrap());
        } else {
            out.push(c);
        }
    }
    out
}

impl Action {
    /// Serialize the action content (state, name, metadata) to a formatter.
    pub fn fmt_content(&self, f: &mut fmt::Formatter<'_>, include_id: bool) -> fmt::Result {
        write!(f, "[{}] {}", self.state, escape_name(&self.name))?;
        if let Some(alias) = &self.alias {
            write!(f, " ={}", alias)?;
        }
        if self.is_sequential == Some(true) {
            write!(f, " ~")?;
        }
        if let Some(description) = &self.description {
            write!(f, " $ {} $", escape_description(description))?;
        }
        if let Some(priority) = &self.priority {
            write!(f, " !{}", priority)?;
        }
        if let Some(charter) = &self.charter {
            write!(f, " *{}", escape_name(charter))?;
        }
        if let Some(contexts) = &self.contexts {
            write!(f, " +{}", contexts.join(","))?;
        }
        if let Some(scheduled_at) = &self.scheduled_at {
            write!(f, " @{}", scheduled_at.format("%Y-%m-%dT%H:%M"))?;
            if let Some(duration) = self.duration {
                write!(f, " D{}", duration)?;
            }
        }
        if let Some(due_date) = &self.due_date {
            write!(f, " :{}", due_date.format("%Y-%m-%dT%H:%M"))?;
        }
        if let Some(completed_at) = &self.completed_at {
            write!(f, " %{}", completed_at.format("%Y-%m-%dT%H:%M"))?;
        }
        if let Some(predecessors) = &self.predecessors {
            for pred in predecessors {
                write!(f, " <{}", escape_name(&pred.raw_ref))?;
            }
        }
        if include_id {
            write!(f, " #{}", self.id)?;
        }
        Ok(())
    }

    /// Compute the depth of this action by walking up the parent chain.
    pub fn depth(&self, action_list: &ActionList) -> u32 {
        let mut depth = 0;
        let mut current_id = self.parent_id;
        while let Some(pid) = current_id {
            depth += 1;
            current_id = action_list
                .iter()
                .find(|a| a.id == pid)
                .and_then(|a| a.parent_id);
        }
        depth
    }
}

impl fmt::Display for ActionState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            ActionState::NotStarted => " ",
            ActionState::InProgress => "-",
            ActionState::Completed => "x",
            ActionState::BlockedOrAwaiting => "=",
            ActionState::Cancelled => "_",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_content(f, true)
    }
}

/// Parse ISO 8601 datetime string to DateTime<Local>
/// Supports formats: YYYY-MM-DD, YYYY-MM-DDTHH:MM, YYYY-MM-DDTHH:MM:SS
/// with optional timezone (Z or +/-HH:MM)
pub fn parse_iso8601_datetime(datetime_str: &str) -> Option<DateTime<Local>> {
    use chrono::{NaiveDate, NaiveDateTime, NaiveTime, TimeZone};

    let trimmed = datetime_str.trim();

    // Try parsing with timezone first
    if let Ok(dt) = DateTime::parse_from_rfc3339(trimmed) {
        return Some(dt.with_timezone(&Local));
    }

    // Try YYYY-MM-DDTHH:MM:SS format (without timezone)
    if let Ok(naive_dt) = NaiveDateTime::parse_from_str(trimmed, "%Y-%m-%dT%H:%M:%S") {
        return Local.from_local_datetime(&naive_dt).earliest();
    }

    // Try YYYY-MM-DDTHH:MM format (without timezone, no seconds)
    if let Ok(naive_dt) = NaiveDateTime::parse_from_str(trimmed, "%Y-%m-%dT%H:%M") {
        return Local.from_local_datetime(&naive_dt).earliest();
    }

    // Try YYYY-MM-DD format (date only, default to start of day)
    if let Ok(naive_date) = NaiveDate::parse_from_str(trimmed, "%Y-%m-%d") {
        let naive_dt = naive_date.and_time(NaiveTime::from_hms_opt(0, 0, 0)?);
        return Local.from_local_datetime(&naive_dt).earliest();
    }

    None
}

/// Recursively parse an action node and all its children into a flat list
pub fn parse_action_recursive(
    node: NodeWrapper,
    parent_id: Option<Uuid>,
    source_map: &mut HashMap<Uuid, SourceMetadata>,
    tag_index: &mut HashMap<String, Vec<SourceRange>>,
) -> Result<Vec<Action>, &'static str> {
    let mut actions = Vec::new();
    let action_range = SourceRange::from_node(&node.node);

    // Parse state
    let state_node = node.require_field("state")?;
    let state_value_node = state_node
        .child_by_field_name("value")
        .ok_or("Missing state value")?;
    let state = parse_state_kind(state_value_node.kind())?;

    // Parse name
    let name_node = node.require_field("name")?;
    let name = unescape_field(get_node_text(&name_node, &node.source).trim());
    let mut line_end_pos = name_node.end_position();

    // Parse metadata fields
    let mut description = None;
    let mut priority = None;
    let mut context_list = None;
    let mut id = None;
    let mut charter = None;
    let mut do_date_time = None;
    let mut do_duration = None;
    let mut completed_date_time = None;
    let mut created_date_time = None;
    let mut predecessors = Vec::new();
    let mut alias = None;
    let mut is_sequential = None;

    let mut do_date_range = None;
    let mut due_date_time = None;
    let mut due_date_range = None;
    let mut completed_date_range = None;
    let mut created_date_range = None;
    let mut raw_id = None;

    let mut metadata_cursor = node.node.walk();
    for meta in node
        .node
        .children_by_field_name("metadata", &mut metadata_cursor)
    {
        // Track line end position for source mapping
        if meta.end_position().row > line_end_pos.row
            || (meta.end_position().row == line_end_pos.row
                && meta.end_position().column > line_end_pos.column)
        {
            line_end_pos = meta.end_position();
        }

        match meta.kind() {
            "description" => {
                // Extract just the 'text' field content, not the $ markers
                description = meta
                    .child_by_field_name("text")
                    .map(|text_node| unescape_field(get_node_text(&text_node, &node.source).trim()));
            }
            "priority" => {
                priority = get_prefixed_text(&meta, &node.source, '!').and_then(|s| s.parse().ok());
            }
            "story" => {
                let text = get_node_text(&meta, &node.source);
                charter = get_prefixed_text(&meta, &node.source, '*').map(|s| unescape_field(&s));
                index_tag(tag_index, text, &meta);
            }
            "context" => {
                let text = get_node_text(&meta, &node.source);
                if let Some(inner) = text.strip_prefix('+') {
                    // Collect tags from every context node rather than overwriting.
                    // The grammar emits one node per `+` group, so space-separated
                    // `+work +meeting +client` (spec-documented) arrives as several
                    // nodes; assigning here would silently keep only the last.
                    let tags = inner.split(',').map(|s| s.trim().to_string());
                    context_list.get_or_insert_with(Vec::new).extend(tags);
                    index_tag(tag_index, text, &meta);
                }
            }
            "do_date" => {
                (do_date_time, do_date_range) = parse_date_field(&meta, &node.source);
                do_duration = parse_duration_field(&meta, &node.source);
            }
            "due_date" => {
                (due_date_time, due_date_range) = parse_date_field(&meta, &node.source);
            }
            "completed_date" => {
                (completed_date_time, completed_date_range) = parse_date_field(&meta, &node.source);
            }
            "created_date" => {
                (created_date_time, created_date_range) = parse_date_field(&meta, &node.source);
            }
            "id" => {
                if let Some(id_val) = get_prefixed_text(&meta, &node.source, '#') {
                    match Uuid::parse_str(&id_val) {
                        Ok(uuid) => id = Some(uuid),
                        Err(_) => raw_id = Some(id_val),
                    }
                }
            }
            "predecessor" => {
                if let Some(pred_val) = get_prefixed_text(&meta, &node.source, '<') {
                    let pred_val = unescape_field(&pred_val);
                    predecessors.push(PredecessorRef {
                        resolved_uuid: Uuid::parse_str(&pred_val).ok(),
                        raw_ref: pred_val,
                    });
                }
            }
            "alias" => {
                alias = get_prefixed_text(&meta, &node.source, '=').filter(|s| !s.is_empty());
            }
            "sequential" => {
                is_sequential = Some(true);
            }
            _ => {}
        }
    }

    let is_id_generated = id.is_none();

    // Generate ID if not present (using UUIDv7 for embedded timestamp)
    let action_id = id.unwrap_or_else(Uuid::now_v7);

    // Record metadata
    source_map.insert(
        action_id,
        SourceMetadata {
            root: action_range,
            line_range: SourceRange {
                start_row: action_range.start_row,
                start_col: action_range.start_col,
                end_row: line_end_pos.row,
                end_col: line_end_pos.column,
            },
            do_date: do_date_range,
            due_date: due_date_range,
            completed_date: completed_date_range,
            created_date: created_date_range,
            is_id_generated,
            raw_id,
        },
    );

    // Create the action
    actions.push(Action {
        id: action_id,
        parent_id,
        state,
        name,
        description,
        priority,
        contexts: context_list,
        scheduled_at: do_date_time,
        duration: do_duration,
        due_date: due_date_time,
        completed_at: completed_date_time,
        created_at: created_date_time,
        predecessors: if predecessors.is_empty() { None } else { Some(predecessors) },
        charter,
        alias,
        is_sequential,
        ..Default::default()
    });

    // Recursively parse children using field access
    let mut child_cursor = node.node.walk();
    for child_node in node.node.children_by_field_name("child", &mut child_cursor) {
        let child_wrapper = create_node_wrapper(child_node, node.source.clone());
        actions.extend(parse_action_recursive(
            child_wrapper,
            Some(action_id),
            source_map,
            tag_index,
        )?);
    }

    Ok(actions)
}

/// Lifecycle state of an [`Action`].
///
/// Maps to the visual representation in the `.actions` file:
/// - `NotStarted` -> `[ ]`
/// - `Completed` -> `[x]`
/// - `InProgress` -> `[-]`
fn parse_state_kind(kind: &str) -> Result<ActionState, &'static str> {
    match kind {
        "state_not_started" => Ok(ActionState::NotStarted),
        "state_completed" => Ok(ActionState::Completed),
        "state_in_progress" => Ok(ActionState::InProgress),
        "state_blocked" => Ok(ActionState::BlockedOrAwaiting),
        "state_cancelled" => Ok(ActionState::Cancelled),
        _ => Err("Unknown state type"),
    }
}

/// Add a tag reference to the index
fn index_tag(
    tag_index: &mut HashMap<String, Vec<SourceRange>>,
    text: String,
    node: &tree_sitter::Node,
) {
    tag_index
        .entry(text)
        .or_default()
        .push(SourceRange::from_node(node));
}

/// Parse a date field (do_date, completed_date, created_date) returning both the datetime and source range
fn parse_date_field(
    node: &tree_sitter::Node,
    source: &str,
) -> (Option<DateTime<Local>>, Option<SourceRange>) {
    if let Some(datetime_node) = node.child_by_field_name("datetime") {
        let datetime_str = get_node_text(&datetime_node, source);
        let datetime = parse_iso8601_datetime(&datetime_str);
        let range = Some(SourceRange::from_node(node));
        (datetime, range)
    } else {
        (None, None)
    }
}

/// Parse duration from a do_date node
fn parse_duration_field(node: &tree_sitter::Node, source: &str) -> Option<u32> {
    node.child_by_field_name("duration")
        .and_then(|d| d.child_by_field_name("minutes"))
        .and_then(|m| get_node_text(&m, source).parse().ok())
}

/// Parse raw text into a tree-sitter Tree
pub fn parse_tree(input: &str) -> Result<Tree, String> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_actions::LANGUAGE.into())
        .expect("Failed to set language for tree-sitter parser");
    parser
        .parse(input, None)
        .ok_or("Failed to parse tree".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::source::TreeWrapper;

    fn parse_actions(source: &str) -> ActionList {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_actions::LANGUAGE.into())
            .expect("Failed to set language");

        let tree = parser.parse(source, None).expect("Failed to parse");
        let tree_wrapper = TreeWrapper {
            tree,
            source: source.to_string(),
        };

        tree_wrapper
            .try_into()
            .expect("Failed to convert to ActionList")
    }

    #[test]
    fn test_parse_simple_action() {
        let source = "[ ] Buy milk";
        let actions = parse_actions(source);

        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].name, "Buy milk");
        assert_eq!(actions[0].state, ActionState::NotStarted);
        assert_eq!(actions[0].parent_id, None);
    }

    #[test]
    fn test_parse_with_metadata() {
        let source = "[x] Buy groceries $from the store$ !1 +shopping";
        let actions = parse_actions(source);

        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].name, "Buy groceries");
        assert_eq!(actions[0].state, ActionState::Completed);
        assert_eq!(
            actions[0].description.as_ref().map(|s| s.trim()),
            Some("from the store")
        );
        assert_eq!(actions[0].priority, Some(1));
        assert!(actions[0].contexts.is_some());
    }

    #[test]
    fn test_parse_with_children() {
        let source = "[ ] Parent action\n> [ ] Child action\n>> [ ] Grandchild action";
        let actions = parse_actions(source);

        assert_eq!(actions.len(), 3);

        // Check parent
        assert_eq!(actions[0].name, "Parent action");
        assert_eq!(actions[0].parent_id, None);

        // Check child
        assert_eq!(actions[1].name, "Child action");
        assert_eq!(actions[1].parent_id, Some(actions[0].id));

        // Check grandchild
        assert_eq!(actions[2].name, "Grandchild action");
        assert_eq!(actions[2].parent_id, Some(actions[1].id));
    }

    #[test]
    fn test_parse_with_predecessors_uuid() {
        let pred_uuid = "01951111-cfa6-718d-b303-d7107f4005b3";
        let source = format!("[ ] Task B < {}", pred_uuid);
        let actions = parse_actions(&source);

        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].name, "Task B");
        assert!(actions[0].predecessors.is_some());
        let preds = actions[0].predecessors.as_ref().unwrap();
        assert_eq!(preds.len(), 1);
        assert_eq!(preds[0].raw_ref, pred_uuid);
        assert_eq!(preds[0].resolved_uuid.unwrap().to_string(), pred_uuid);
    }

    #[test]
    fn test_parse_with_multiple_predecessors() {
        let uuid1 = "01951111-cfa6-718d-b303-d7107f4005b3";
        let uuid2 = "02961111-cfa6-718d-b303-d7107f4005b3";
        let source = format!("[ ] Deploy < {} < {}", uuid1, uuid2);
        let actions = parse_actions(&source);

        assert_eq!(actions.len(), 1);
        assert!(actions[0].predecessors.is_some());
        let preds = actions[0].predecessors.as_ref().unwrap();
        assert_eq!(preds.len(), 2);
    }

    #[test]
    fn test_parse_with_predecessor_name() {
        let source = "[ ] Task B < Task A";
        let actions = parse_actions(source);

        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].name, "Task B");
        assert!(actions[0].predecessors.is_some());
        let preds = actions[0].predecessors.as_ref().unwrap();
        assert_eq!(preds.len(), 1);
        assert_eq!(preds[0].raw_ref, "Task A");
        assert_eq!(preds[0].resolved_uuid, None); // Name doesn't resolve to UUID yet

        // Verify round-trip formatting preserves the name
        let formatted = format!("{}", actions[0]);
        assert!(formatted.contains("<Task A"));
    }

    #[test]
    fn test_format_with_predecessors() {
        let pred_uuid = Uuid::parse_str("01951111-cfa6-718d-b303-d7107f4005b3").unwrap();
        let pred_ref = PredecessorRef {
            raw_ref: pred_uuid.to_string(),
            resolved_uuid: Some(pred_uuid),
        };
        let action = Action {
            id: Uuid::new_v4(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Task B".to_string(),
            predecessors: Some(vec![pred_ref]),
            ..Default::default()
        };

        let formatted = format!("{}", action);
        assert!(formatted.contains(&pred_uuid.to_string()));
        assert!(formatted.contains("<"));
    }

    #[test]
    fn test_parse_space_separated_contexts_preserves_all_tags() {
        // Regression for the multi-tag data-loss bug (cli charter 019f268f-eb4e):
        // the spec documents space-separated tags (action_file_format.md line 145,
        // `+work +meeting +client`), which parse as separate context nodes. The
        // parser must collect every node, not silently keep only the last —
        // silent loss on a source-of-truth file is the trust-destroying event.
        let source = "[ ] Prepare deck +work +meeting +client";
        let actions = parse_actions(source);

        assert_eq!(actions.len(), 1);
        let contexts = actions[0]
            .contexts
            .as_ref()
            .expect("space-separated tags should parse to Some");
        assert_eq!(
            contexts,
            &vec![
                "work".to_string(),
                "meeting".to_string(),
                "client".to_string()
            ],
            "every space-separated tag must survive parsing, not just the last"
        );
    }

    #[test]
    fn test_parse_empty_context_behavior() {
        // This test confirms that the new permissive grammar/parser treats a standalone "+"
        // as a valid context node with an empty tag string.
        let source = "[ ] Task +";
        let actions = parse_actions(source);
        assert_eq!(actions.len(), 1);

        // The permissive grammar accepts "+", resulting in Some([""])
        assert!(actions[0].contexts.is_some());
        let contexts = actions[0].contexts.as_ref().unwrap();
        assert_eq!(contexts.len(), 1);
        assert_eq!(contexts[0], "");
    }

    #[test]
    fn test_escape_unescape_are_inverse() {
        // escape_name -> unescape_name must be the identity for names carrying
        // reserved sigils, a literal backslash, and a link (left untouched).
        for name in [
            "save $500 for the trip",
            "review PR #42 before <friday",
            "ratio 3:1 and 50% off *now*",
            r"path C:\temp\logs",
            "see [[docs|http://example.com/a?x=1]] and $5",
            "plain title, no sigils",
        ] {
            assert_eq!(
                unescape_field(&escape_name(name)),
                name,
                "escape/unescape not inverse for {name:?}"
            );
        }
    }

    #[test]
    fn test_escape_name_leaves_links_intact() {
        let name = "see [[docs|http://example.com/a?x=1]] and $5";
        let escaped = escape_name(name);
        assert!(
            escaped.contains("[[docs|http://example.com/a?x=1]]"),
            "link internals must not be escaped: {escaped:?}"
        );
        assert!(
            escaped.contains(r"\$5"),
            "a sigil outside the link must still be escaped: {escaped:?}"
        );
    }

    #[test]
    fn test_reserved_chars_in_name_survive_full_format_pipeline() {
        // The Decision-33 residue: a name with reserved sigils must survive the
        // real formatter -> Topiary -> parser trip, not just the raw Display.
        use crate::workspace::actions::format::{OutputFormat, format};
        for raw in [
            "save $500 for the trip",
            "review PR #42 before <friday",
            "ratio 3:1 and 50% off",
            r"path C:\temp\logs",
        ] {
            let action = Action::new(raw);
            let text = format(&vec![action.clone()], OutputFormat::Actions, None, None)
                .expect("format should succeed");
            let parsed = parse_actions(&text);
            assert_eq!(parsed.len(), 1, "one action for {raw:?}; text was {text:?}");
            assert_eq!(
                parsed[0].name, raw,
                "name did not survive the format->Topiary->parse trip; text was {text:?}"
            );
        }
    }

    #[test]
    fn test_reserved_chars_survive_in_description_and_refs() {
        // Description bodies and predecessor refs are freeform too — a literal
        // `$` in a description must not close the block, and a name-ref
        // predecessor must keep its sigils.
        use crate::workspace::actions::format::{OutputFormat, format};
        let mut action = Action::new("headline with $ and #42");
        action.description = Some("budget is $500; ratio 3:1; keep notes".to_string());
        action.predecessors = Some(vec![PredecessorRef {
            raw_ref: "buy $5 widget".to_string(),
            resolved_uuid: None,
        }]);

        let text = format(&vec![action.clone()], OutputFormat::Actions, None, None).unwrap();
        let parsed = parse_actions(&text);

        assert_eq!(parsed.len(), 1, "text was {text:?}");
        assert_eq!(parsed[0].name, action.name, "name; text {text:?}");
        assert_eq!(
            parsed[0].description, action.description,
            "description with literal $ did not round-trip; text {text:?}"
        );
        let preds = parsed[0].predecessors.as_ref().expect("predecessors");
        assert_eq!(
            preds[0].raw_ref, "buy $5 widget",
            "predecessor ref sigils did not round-trip; text {text:?}"
        );
    }
}
