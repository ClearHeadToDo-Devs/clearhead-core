use crate::actions::source::{
    NodeWrapper, SourceMetadata, SourceRange, create_node_wrapper, get_node_text, get_prefixed_text,
};
use crate::domain::Recurrence;
use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use uuid::Uuid;

pub type ActionList = Vec<Action>;

/// Reference to a predecessor action, which can be either a UUID or a name reference
#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct PredecessorRef {
    /// The raw reference text from the source (could be a UUID string or an action name)
    pub raw_ref: String,
    /// The resolved UUID if we were able to parse/resolve it
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolved_uuid: Option<Uuid>,
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Action {
    pub id: Uuid,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<Uuid>,
    pub state: ActionState,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub priority: Option<u32>,
    #[serde(rename = "contexts", skip_serializing_if = "Option::is_none")]
    pub context_list: Option<Vec<String>>,
    #[serde(rename = "doDateTime", skip_serializing_if = "Option::is_none")]
    pub do_date_time: Option<DateTime<Local>>,
    #[serde(rename = "doDuration", skip_serializing_if = "Option::is_none")]
    pub do_duration: Option<u32>, // Duration in minutes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recurrence: Option<Recurrence>,
    #[serde(rename = "completedDate", skip_serializing_if = "Option::is_none")]
    pub completed_date_time: Option<DateTime<Local>>,
    #[serde(rename = "createdDate", skip_serializing_if = "Option::is_none")]
    pub created_date_time: Option<DateTime<Local>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub predecessors: Option<Vec<PredecessorRef>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub story: Option<String>,
    /// Alias for stable references that persist even when action name changes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alias: Option<String>,
    /// When true, all direct children are sequential (each depends on previous sibling)
    #[serde(rename = "isSequential", skip_serializing_if = "Option::is_none")]
    pub is_sequential: Option<bool>,
}

impl Default for Action {
    fn default() -> Self {
        Self {
            id: Uuid::now_v7(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: String::new(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: None,
            do_duration: None,
            recurrence: None,
            completed_date_time: None,
            created_date_time: Some(Local::now()),
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        }
    }
}

impl Action {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Serialize the action content (state, name, metadata) to a formatter
    pub fn fmt_content(&self, f: &mut fmt::Formatter<'_>, include_id: bool) -> fmt::Result {
        // State and name (required)
        write!(f, "[{}] {}", self.state, self.name)?;

        // Alias (=alias_name)
        if let Some(alias) = &self.alias {
            write!(f, " ={}", alias)?;
        }

        // Sequential marker (~) - marks children as sequential
        if self.is_sequential == Some(true) {
            write!(f, " ~")?;
        }

        // Add metadata with spec-compliant spacing:
        // - Description enclosed in $ ... $
        // - No space after other icons
        if let Some(description) = &self.description {
            write!(f, " $ {} $", description)?;
        }
        if let Some(priority) = &self.priority {
            write!(f, " !{}", priority)?;
        }
        if let Some(story) = &self.story {
            write!(f, " *{}", story)?;
        }
        if let Some(context_list) = &self.context_list {
            write!(f, " +{}", context_list.join(","))?;
        }
        if let Some(do_date_time) = &self.do_date_time {
            write!(f, " @{}", do_date_time.format("%Y-%m-%dT%H:%M"))?;
            if let Some(duration) = self.do_duration {
                write!(f, " D{}", duration)?;
            }
            if let Some(recurrence) = &self.recurrence {
                write!(f, " {}", recurrence)?;
            }
        }
        if let Some(completed_date_time) = &self.completed_date_time {
            write!(f, " %{}", completed_date_time.format("%Y-%m-%dT%H:%M"))?;
        }
        if let Some(created_date_time) = &self.created_date_time {
            write!(f, " ^{}", created_date_time.format("%Y-%m-%dT%H:%M"))?;
        }
        if let Some(predecessors) = &self.predecessors {
            for pred in predecessors {
                write!(f, " <{}", pred.raw_ref)?;
            }
        }
        if include_id {
            write!(f, " #{}", self.id)?;
        }
        Ok(())
    }

    /// Expand recurrence rule into a list of occurrence dates.
    ///
    /// Uses `do_date_time` as DTSTART and `recurrence` as RRULE.
    /// Returns an empty vector if no recurrence or do_date_time is present.
    pub fn expand_occurrences(&self, limit: u16) -> Vec<DateTime<rrule::Tz>> {
        let dtstart = match self.do_date_time {
            Some(dt) => dt,
            None => return Vec::new(),
        };

        let recurrence = match &self.recurrence {
            Some(r) => r,
            None => return Vec::new(),
        };

        let recurrence_str = recurrence.to_string();
        let clean_recurrence = recurrence_str.strip_prefix("R:").unwrap_or(&recurrence_str);

        let rrule_str = format!(
            "DTSTART:{}\nRRULE:{}",
            dtstart.format("%Y%m%dT%H%M%S"),
            clean_recurrence
        );

        match rrule_str.parse::<rrule::RRuleSet>() {
            Ok(rrule_set) => rrule_set.all(limit).dates,
            Err(e) => {
                eprintln!("Failed to parse recurrence rule: {}", e);
                Vec::new()
            }
        }
    }

    /// Compute the depth of this action by walking up the parent chain
    pub fn depth(&self, action_list: &ActionList) -> u32 {
        let mut depth = 0;
        let mut current_id = self.parent_id;
        while let Some(parent_id) = current_id {
            depth += 1;
            current_id = action_list
                .iter()
                .find(|a| a.id == parent_id)
                .and_then(|a| a.parent_id);
        }
        depth
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
    let name = get_node_text(&name_node, &node.source).trim().to_string();
    let mut line_end_pos = name_node.end_position();

    // Parse metadata fields
    let mut description = None;
    let mut priority = None;
    let mut context_list = None;
    let mut id = None;
    let mut story = None;
    let mut do_date_time = None;
    let mut do_duration = None;
    let mut recurrence = None;
    let mut completed_date_time = None;
    let mut created_date_time = None;
    let mut predecessors = Vec::new();
    let mut alias = None;
    let mut is_sequential = None;

    let mut do_date_range = None;
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
                    .map(|text_node| get_node_text(&text_node, &node.source).trim().to_string());
            }
            "priority" => {
                priority = get_prefixed_text(&meta, &node.source, '!').and_then(|s| s.parse().ok());
            }
            "story" => {
                let text = get_node_text(&meta, &node.source);
                story = get_prefixed_text(&meta, &node.source, '*');
                index_tag(tag_index, text, &meta);
            }
            "context" => {
                let text = get_node_text(&meta, &node.source);
                if let Some(inner) = text.strip_prefix('+') {
                    context_list = Some(inner.split(',').map(|s| s.trim().to_string()).collect());
                    index_tag(tag_index, text, &meta);
                }
            }
            "do_date" => {
                (do_date_time, do_date_range) = parse_date_field(&meta, &node.source);
                do_duration = parse_duration_field(&meta, &node.source);
                recurrence = parse_recurrence_field(&meta, &node.source);
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

    // When generating an ID, also inject created_date if missing
    // This makes logical sense: the action is "created" when it gets its formal ID
    if is_id_generated && created_date_time.is_none() {
        created_date_time = Some(chrono::Local::now());
    }

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
        context_list,
        do_date_time,
        do_duration,
        recurrence,
        completed_date_time,
        created_date_time,
        predecessors: if predecessors.is_empty() {
            None
        } else {
            Some(predecessors)
        },
        story,
        alias,
        is_sequential,
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

#[derive(
    Default, Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize,
)]
#[serde(rename_all = "snake_case")]
pub enum ActionState {
    #[default]
    NotStarted,
    Completed,
    InProgress,
    #[serde(rename = "blocked")]
    BlockedorAwaiting,
    Cancelled,
}

impl fmt::Display for ActionState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let state_char = match self {
            ActionState::NotStarted => " ",
            ActionState::Completed => "x",
            ActionState::InProgress => "-",
            ActionState::BlockedorAwaiting => "=",
            ActionState::Cancelled => "_",
        };
        write!(f, "{}", state_char)
    }
}

fn parse_state_kind(kind: &str) -> Result<ActionState, &'static str> {
    match kind {
        "state_not_started" => Ok(ActionState::NotStarted),
        "state_completed" => Ok(ActionState::Completed),
        "state_in_progress" => Ok(ActionState::InProgress),
        "state_blocked" => Ok(ActionState::BlockedorAwaiting),
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

/// Parse recurrence from a do_date node
fn parse_recurrence_field(node: &tree_sitter::Node, source: &str) -> Option<Recurrence> {
    node.child_by_field_name("recurrence")
        .and_then(|r| r.child_by_field_name("rrule"))
        .and_then(|rrule| parse_rrule(&get_node_text(&rrule, source)))
}

fn parse_rrule(rrule_str: &str) -> Option<Recurrence> {
    let mut frequency = String::new();
    let mut interval = None;
    let mut count = None;
    let mut until = None;
    let mut by_second = None;
    let mut by_minute = None;
    let mut by_hour = None;
    let mut by_day = None;
    let mut by_month_day = None;
    let mut by_year_day = None;
    let mut by_week_no = None;
    let mut by_month = None;
    let mut by_set_pos = None;
    let mut week_start = None;

    // Remove "R:" prefix if present (it should be handled by tree-sitter but just in case)
    let clean_rrule = if rrule_str.trim().starts_with("R:") {
        &rrule_str.trim()[2..]
    } else {
        rrule_str.trim()
    };

    for part in clean_rrule.split(';') {
        let (key, value) = part.split_once('=')?;

        match key {
            "FREQ" => frequency = value.to_lowercase(),
            "INTERVAL" => interval = value.parse().ok(),
            "COUNT" => count = value.parse().ok(),
            "UNTIL" => until = Some(value.to_string()),
            "BYSECOND" => by_second = Some(parse_int_list(value)),
            "BYMINUTE" => by_minute = Some(parse_int_list(value)),
            "BYHOUR" => by_hour = Some(parse_int_list(value)),
            "BYDAY" => by_day = Some(value.split(',').map(|s| s.to_string()).collect()),
            "BYMONTHDAY" => by_month_day = Some(parse_int_list(value)),
            "BYYEARDAY" => by_year_day = Some(parse_int_list(value)),
            "BYWEEKNO" => by_week_no = Some(parse_int_list(value)),
            "BYMONTH" => by_month = Some(parse_int_list(value)),
            "BYSETPOS" => by_set_pos = Some(parse_int_list(value)),
            "WKST" => week_start = Some(value.to_string()),
            _ => {}
        }
    }

    if frequency.is_empty() {
        return None;
    }

    Some(Recurrence {
        frequency,
        interval,
        count,
        until,
        by_second,
        by_minute,
        by_hour,
        by_day,
        by_month_day,
        by_year_day,
        by_week_no,
        by_month,
        by_set_pos,
        week_start,
    })
}

fn parse_int_list<T: std::str::FromStr>(s: &str) -> Vec<T> {
    s.split(',').filter_map(|x| x.parse().ok()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::actions::source::TreeWrapper;

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
        assert!(actions[0].context_list.is_some());
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
    fn test_parse_recurrence_and_duration() {
        let source = "[ ] Recurring Task @2025-01-20T14:00 D60 R:FREQ=WEEKLY;BYDAY=MO;INTERVAL=2";
        let actions = parse_actions(source);

        assert_eq!(actions.len(), 1);
        let action = &actions[0];

        assert_eq!(action.name, "Recurring Task");
        assert!(action.do_date_time.is_some());
        assert_eq!(action.do_duration, Some(60));

        let recurrence = action
            .recurrence
            .as_ref()
            .expect("Recurrence should be present");
        assert_eq!(recurrence.frequency, "weekly");
        assert_eq!(recurrence.by_day, Some(vec!["MO".to_string()]));
        assert_eq!(recurrence.interval, Some(2));
    }

    #[test]
    fn test_format_recurrence_and_duration() {
        let recurrence = Recurrence {
            frequency: "weekly".to_string(),
            interval: Some(2),
            count: None,
            until: None,
            by_second: None,
            by_minute: None,
            by_hour: None,
            by_day: Some(vec!["MO".to_string()]),
            by_month_day: None,
            by_year_day: None,
            by_week_no: None,
            by_month: None,
            by_set_pos: None,
            week_start: None,
        };

        let action = Action {
            id: Uuid::new_v4(),
            parent_id: None,
            state: ActionState::NotStarted,
            name: "Recurring".to_string(),
            description: None,
            priority: None,
            context_list: None,
            do_date_time: Some(Local::now()),
            do_duration: Some(60),
            recurrence: Some(recurrence),
            completed_date_time: None,
            created_date_time: None,
            predecessors: None,
            story: None,
            alias: None,
            is_sequential: None,
        };

        let formatted = format!("{}", action);
        assert!(formatted.contains("D60"));
        assert!(formatted.contains("R:FREQ=WEEKLY"));
        assert!(formatted.contains(";INTERVAL=2"));
        assert!(formatted.contains(";BYDAY=MO"));
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
            description: None,
            priority: None,
            context_list: None,
            do_date_time: None,
            do_duration: None,
            recurrence: None,
            completed_date_time: None,
            created_date_time: None,
            predecessors: Some(vec![pred_ref]),
            story: None,
            alias: None,
            is_sequential: None,
        };

        let formatted = format!("{}", action);
        assert!(formatted.contains(&pred_uuid.to_string()));
        assert!(formatted.contains("<"));
    }

    #[test]
    fn test_parse_empty_context_behavior() {
        // This test confirms that the new permissive grammar/parser treats a standalone "+"
        // as a valid context node with an empty tag string.
        let source = "[ ] Task +";
        let actions = parse_actions(source);
        assert_eq!(actions.len(), 1);

        // The permissive grammar accepts "+", resulting in Some([""])
        assert!(actions[0].context_list.is_some());
        let contexts = actions[0].context_list.as_ref().unwrap();
        assert_eq!(contexts.len(), 1);
        assert_eq!(contexts[0], "");
    }
}
