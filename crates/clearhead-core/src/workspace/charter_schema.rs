//! Source-aware, pure projection of charter facts into charters.schema.json.
//! Actions and plans deliberately remain in their own projections.

use serde_json::{Map, Value, json};
use uuid::Uuid;

use super::charter::{MarkdownCharter, split_frontmatter};

/// Project a charter from its loaded model and its document text (if any).
/// Only an id matching the document declaration is published. A sidecar or
/// shell identity is an in-process join key, not a serialized charter fact.
pub fn project_charter_schema(
    charter: &MarkdownCharter,
    source: Option<&str>,
) -> Result<Value, String> {
    let (frontmatter, body) = source.map(split_frontmatter).unwrap_or((None, ""));
    let frontmatter: Map<String, Value> = frontmatter
        .map(|yaml| serde_yaml_ng::from_str(yaml).map_err(|error| error.to_string()))
        .transpose()?
        .unwrap_or_default();
    let mut row = Map::new();
    if frontmatter
        .get("id")
        .and_then(Value::as_str)
        .and_then(|id| Uuid::parse_str(id).ok())
        == Some(charter.id)
    {
        row.insert("id".into(), json!(charter.id));
    }
    let state = charter.state.unwrap_or_default();
    row.insert(
        "state".into(),
        json!(
            serde_json::to_value(state)
                .map_err(|error| error.to_string())?
                .as_str()
                .ok_or("state must be a string")?
                .to_ascii_lowercase()
        ),
    );
    if charter.title.is_empty() {
        return Err("charter title cannot be empty".into());
    }
    row.insert("title".into(), json!(charter.title));
    if let Some(alias) = &charter.alias {
        if alias.is_empty() {
            return Err("charter alias cannot be empty".into());
        }
        row.insert("alias".into(), json!(alias));
    }
    if let Some(parent) = &charter.parent {
        if parent.is_empty() {
            return Err("charter parent cannot be empty".into());
        }
        row.insert("parent".into(), json!(parent));
    }
    if let Some(objectives) = &charter.objectives {
        if objectives.is_empty() || objectives.iter().any(String::is_empty) {
            return Err("charter objectives must be nonempty references".into());
        }
        row.insert("objectives".into(), json!(objectives));
    }
    if let Some(defaults) = frontmatter.get("defaults") {
        if !defaults.is_object() {
            return Err("charter defaults must be a map".into());
        }
        row.insert("defaults".into(), defaults.clone());
    }

    let body = if source.is_some() {
        body
    } else {
        charter.description.as_deref().unwrap_or("")
    };
    let body = body.trim_start_matches(['\r', '\n']);
    let body = body
        .strip_prefix("# ")
        .map(|text| text.split_once('\n').map(|(_, rest)| rest).unwrap_or(""))
        .unwrap_or(body);
    let mut heading: Option<String> = None;
    let mut lines = Vec::new();
    let mut sections = Vec::new();
    let mut logs = Vec::new();
    let mut description = None;
    let mut fence: Option<(u8, usize)> = None;
    for line in body.lines() {
        if let Some((kind, width)) = fence {
            if let Some((end_kind, end_width)) = fence_marker(line)
                && kind == end_kind
                && end_width >= width
                && line
                    .trim_start_matches(' ')
                    .trim_start_matches(kind as char)
                    .trim()
                    .is_empty()
            {
                fence = None;
            }
        } else {
            fence = fence_marker(line);
        }
        if fence.is_none() && line.starts_with("## ") && !line[3..].trim().is_empty() {
            flush_section(
                heading.take(),
                &lines,
                &mut description,
                &mut logs,
                &mut sections,
            );
            heading = Some(line[3..].trim().to_string());
            lines.clear();
        } else {
            lines.push(line);
        }
    }
    flush_section(heading, &lines, &mut description, &mut logs, &mut sections);
    if let Some(description) = description {
        row.insert("description".into(), json!(description));
    }
    if !logs.is_empty() {
        row.insert("log".into(), json!(logs));
    }
    if !sections.is_empty() {
        row.insert("sections".into(), json!(sections));
    }
    Ok(Value::Object(row))
}

fn fence_marker(line: &str) -> Option<(u8, usize)> {
    let indent = line.bytes().take_while(|byte| *byte == b' ').count();
    if indent > 3 {
        return None;
    }
    let bytes = &line.as_bytes()[indent..];
    let marker = *bytes.first()?;
    if marker != b'`' && marker != b'~' {
        return None;
    }
    let width = bytes.iter().take_while(|byte| **byte == marker).count();
    (width >= 3).then_some((marker, width))
}

fn flush_section(
    heading: Option<String>,
    lines: &[&str],
    description: &mut Option<String>,
    logs: &mut Vec<Value>,
    sections: &mut Vec<Value>,
) {
    let text = lines.join("\n").trim().to_string();
    match heading {
        None => {
            if !text.is_empty() {
                *description = Some(text);
            }
        }
        Some(name) if name.eq_ignore_ascii_case("Log") => parse_log(&text, logs),
        Some(name) => sections.push(json!({"heading": name, "body": text})),
    }
}

fn parse_log(body: &str, rows: &mut Vec<Value>) {
    for line in body.lines() {
        let text = line
            .trim()
            .strip_prefix("- ")
            .or_else(|| line.trim().strip_prefix("* "));
        let Some(text) = text else {
            // Legacy heading-style and freeform log text must not disappear.
            if !line.trim().is_empty() {
                rows.push(json!({"text": line.trim()}));
            }
            continue;
        };
        if let Some((at, content)) = text.split_once(" — ")
            && valid_log_date(at)
            && !content.trim().is_empty()
        {
            rows.push(json!({"at": at, "text": content.trim()}));
            continue;
        }
        if !text.trim().is_empty() {
            rows.push(json!({"text": text.trim()}));
        }
    }
}

fn valid_log_date(at: &str) -> bool {
    if !at.is_ascii() {
        return false;
    }
    let bytes = at.as_bytes();
    if bytes.len() < 10
        || bytes[4] != b'-'
        || bytes[7] != b'-'
        || !bytes[..4].iter().all(u8::is_ascii_digit)
        || !bytes[5..7].iter().all(u8::is_ascii_digit)
        || !bytes[8..10].iter().all(u8::is_ascii_digit)
    {
        return false;
    }
    if bytes.len() == 10 {
        return chrono::NaiveDate::parse_from_str(at, "%Y-%m-%d").is_ok();
    }
    if bytes.len() < 16
        || bytes[10] != b'T'
        || bytes[13] != b':'
        || !bytes[11..13].iter().all(u8::is_ascii_digit)
        || !bytes[14..16].iter().all(u8::is_ascii_digit)
    {
        return false;
    }
    let has_seconds = bytes.get(16) == Some(&b':');
    if has_seconds && (bytes.len() < 19 || !bytes[17..19].iter().all(u8::is_ascii_digit)) {
        return false;
    }
    let end = if has_seconds { 19 } else { 16 };
    let suffix = &at[end..];
    if suffix.is_empty() {
        return chrono::NaiveDateTime::parse_from_str(
            at,
            if has_seconds {
                "%Y-%m-%dT%H:%M:%S"
            } else {
                "%Y-%m-%dT%H:%M"
            },
        )
        .is_ok();
    }
    if suffix != "Z"
        && !(suffix.len() == 6
            && matches!(suffix.as_bytes()[0], b'+' | b'-')
            && suffix.as_bytes()[3] == b':'
            && suffix.as_bytes()[1..3].iter().all(u8::is_ascii_digit)
            && suffix.as_bytes()[4..].iter().all(u8::is_ascii_digit))
    {
        return false;
    }
    let expanded = if has_seconds {
        at.to_string()
    } else {
        format!("{}:00{suffix}", &at[..16])
    };
    chrono::DateTime::parse_from_rfc3339(&expanded).is_ok()
}

pub fn project_charters_schema(rows: Vec<Value>) -> Value {
    json!({"charters": rows})
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::charter::parse_charter;
    use uuid::Uuid;

    #[test]
    fn projects_declared_identity_defaults_sections_and_log_without_embedded_work() {
        let source = "---\nid: 01951111-0000-7000-8000-0000000000aa\nalias: reno\ndefaults:\n  contexts: [home]\n---\n# Renovation\n\nCore paragraph.\n\n### Details\n\nKeep this.\n\n## Budget\n\nUnder $20k.\n\n## Log\n\n- 2026-09-17 — quote arrived\n- undated note\n\n## Notes\n\nLast section.\n";
        let doc = parse_charter(source).unwrap();
        let charter = MarkdownCharter::from(doc.into_charter(Uuid::nil()));
        let row = project_charter_schema(&charter, Some(source)).unwrap();
        assert_eq!(row["id"], "01951111-0000-7000-8000-0000000000aa");
        assert_eq!(row["state"], "new");
        assert_eq!(
            row["description"],
            "Core paragraph.\n\n### Details\n\nKeep this."
        );
        assert_eq!(row["defaults"]["contexts"][0], "home");
        assert_eq!(
            row["sections"][0],
            json!({"heading":"Budget", "body":"Under $20k."})
        );
        assert_eq!(
            row["sections"][1],
            json!({"heading":"Notes", "body":"Last section."})
        );
        assert_eq!(
            row["log"][0],
            json!({"at":"2026-09-17", "text":"quote arrived"})
        );
        assert_eq!(row["log"][1], json!({"text":"undated note"}));
        assert!(row.get("actions").is_none() && row.get("plans").is_none());
        assert!(!serde_json::to_string(&row).unwrap().contains("null"));
    }

    #[test]
    fn uppercase_declared_uuid_and_fenced_headings_survive_projection() {
        let source = "---\nid: 01951111-0000-7000-8000-0000000000AA\n---\n# Work\n\n```md\n## Not a section\n```\n\n## Notes\n\nReal section.\n";
        let charter =
            MarkdownCharter::from(parse_charter(source).unwrap().into_charter(Uuid::nil()));
        let row = project_charter_schema(&charter, Some(source)).unwrap();
        assert_eq!(row["id"], "01951111-0000-7000-8000-0000000000aa");
        assert_eq!(row["description"], "```md\n## Not a section\n```");
        assert_eq!(row["sections"].as_array().unwrap().len(), 1);
    }

    #[test]
    fn unfinished_and_indented_fences_keep_all_content() {
        for source in [
            "# Work\n\nImportant text.\n\n```text\nunfinished\n",
            "# Work\n\n    ```text\n    ## indented content\n\n## Notes\n\nVisible section.\n",
            "# Work\n\n````text\n## not a section\n```\n## still not a section\n````\n\n## Notes\n\nVisible section.\n",
        ] {
            let charter =
                MarkdownCharter::from(parse_charter(source).unwrap().into_charter(Uuid::nil()));
            let row = project_charter_schema(&charter, Some(source)).unwrap();
            assert!(
                row["description"].as_str().unwrap().contains("text"),
                "{row}"
            );
            if source.contains("## Notes") {
                assert_eq!(row["sections"].as_array().unwrap().len(), 1, "{row}");
                assert_eq!(row["sections"][0]["heading"], "Notes");
            }
            if source.contains("still not a section") {
                assert!(
                    row["description"]
                        .as_str()
                        .unwrap()
                        .contains("## still not a section")
                );
            }
            if source.contains("unfinished") {
                assert!(row["description"].as_str().unwrap().contains("unfinished"));
            }
        }
    }

    #[test]
    fn unsupported_timestamp_is_kept_as_undated_text() {
        assert!(!valid_log_date("2026-09-17T23:28:00.123-07:00"));
        assert!(!valid_log_date("2026-09-17t23:28z"));
        assert!(!valid_log_date("2026- 9-17"));
        assert!(!valid_log_date("2026- 9-17T23:28"));
        assert!(valid_log_date("2026-09-17T23:28-07:00"));
        assert!(valid_log_date("2026-09-17T23:28:00Z"));
        let source = "# Work\n\n## Log\n\n- 2026-09-17T23:28:00.123-07:00 — precise entry\n";
        let charter =
            MarkdownCharter::from(parse_charter(source).unwrap().into_charter(Uuid::nil()));
        let row = project_charter_schema(&charter, Some(source)).unwrap();
        assert_eq!(
            row["log"][0],
            json!({"text": "2026-09-17T23:28:00.123-07:00 — precise entry"})
        );
    }

    #[test]
    fn schema_required_strings_and_nonempty_objectives_fail_explicitly() {
        let original =
            MarkdownCharter::from(parse_charter("# Work\n").unwrap().into_charter(Uuid::nil()));
        let mut charter = original.clone();
        charter.title.clear();
        assert!(
            project_charter_schema(&charter, None)
                .unwrap_err()
                .contains("title")
        );
        let mut charter = original.clone();
        charter.alias = Some(String::new());
        assert!(
            project_charter_schema(&charter, None)
                .unwrap_err()
                .contains("alias")
        );
        let mut charter = original.clone();
        charter.parent = Some(String::new());
        assert!(
            project_charter_schema(&charter, None)
                .unwrap_err()
                .contains("parent")
        );
        for objectives in [vec![], vec![String::new()]] {
            let mut charter = original.clone();
            charter.objectives = Some(objectives);
            assert!(
                project_charter_schema(&charter, None)
                    .unwrap_err()
                    .contains("objectives")
            );
        }
    }

    #[test]
    fn sidecar_identity_not_emitted_without_document_declaration() {
        let source = "---\nalias: reno\n---\n# Renovation\n";
        let doc = parse_charter(source).unwrap();
        let charter = MarkdownCharter::from(doc.into_charter(Uuid::now_v7()));
        assert!(
            project_charter_schema(&charter, Some(source))
                .unwrap()
                .get("id")
                .is_none()
        );
    }
}
