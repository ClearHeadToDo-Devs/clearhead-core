//! Source-aware, pure projection of charter facts into charters.schema.json.
//! Actions and plans deliberately remain in their own projections.

use serde_json::{Map, Value, json};

use super::charter::{CharterIdSource, MarkdownCharter, is_log_heading, split_frontmatter};
use super::markdown;
use regex::Regex;
use std::sync::LazyLock;

/// Project a charter from its loaded model and its document text (if any).
/// Only a document-declared id is published; see [`CharterIdSource`].
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
    if charter.id_source == CharterIdSource::Document {
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
    let (preamble, mut parts) = markdown::sections(body, 2);
    // The title heading opens the document; its section is the core description.
    let core = if preamble.trim().is_empty() && parts.first().is_some_and(|s| s.heading.level == 1)
    {
        parts.remove(0).body
    } else {
        preamble
    };
    let core = text(core);
    if !core.is_empty() {
        row.insert("description".into(), json!(core));
    }
    let mut logs = Vec::new();
    let mut sections = Vec::new();
    for section in parts {
        if is_log_heading(&section.heading) {
            parse_log(&text(section.body), &mut logs);
        } else {
            sections.push(json!({"heading": section.heading.text, "body": text(section.body)}));
        }
    }
    if !logs.is_empty() {
        row.insert("log".into(), json!(logs));
    }
    if !sections.is_empty() {
        row.insert("sections".into(), json!(sections));
    }
    Ok(Value::Object(row))
}

/// Trimmed section text with LF line endings.
fn text(section: &str) -> String {
    section.trim().replace("\r\n", "\n")
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

/// `logEntry.at` in charters.schema.json: the same pattern, with its parts
/// captured as date, hours:minutes, seconds and UTC offset.
static LOG_DATE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(\d{4}-\d{2}-\d{2})(?:T(\d{2}:\d{2})(?::(\d{2}))?(Z|[+-]\d{2}:\d{2})?)?$")
        .expect("schema pattern is a valid regex")
});

/// The schema's shape, plus a real calendar date, time and offset.
fn valid_log_date(at: &str) -> bool {
    let Some(parts) = LOG_DATE.captures(at) else {
        return false;
    };
    let date = &parts[1];
    match parts.get(2) {
        None => chrono::NaiveDate::parse_from_str(date, "%Y-%m-%d").is_ok(),
        Some(clock) => {
            let seconds = parts.get(3).map_or("00", |m| m.as_str());
            let offset = parts.get(4).map_or("Z", |m| m.as_str());
            let stamp = format!("{date}T{}:{seconds}{offset}", clock.as_str());
            chrono::DateTime::parse_from_rfc3339(&stamp).is_ok()
        }
    }
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
        let mut charter = MarkdownCharter::from(doc.into_charter(Uuid::nil()));
        charter.id_source = CharterIdSource::Document;
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
        let mut charter =
            MarkdownCharter::from(parse_charter(source).unwrap().into_charter(Uuid::nil()));
        charter.id_source = CharterIdSource::Document;
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
        let mut charter = MarkdownCharter::from(doc.into_charter(Uuid::now_v7()));
        charter.id_source = CharterIdSource::Sidecar;
        assert!(
            project_charter_schema(&charter, Some(source))
                .unwrap()
                .get("id")
                .is_none()
        );
    }

    #[test]
    fn log_dates_follow_the_schema_and_the_calendar() {
        for valid in [
            "2026-09-17",
            "2026-09-17T23:28",
            "2026-09-17T23:28:05",
            "2026-09-17T23:28Z",
            "2026-09-17T23:28-07:00",
        ] {
            assert!(valid_log_date(valid), "{valid}");
        }
        for invalid in [
            "2026-9-17",
            " 2026-09-17",
            "2026-09-17T23:28:05.5Z",
            "2026-13-01",
            "2026-02-30",
            "2026-09-17T24:00",
            "2026-09-17T23:28+24:00",
        ] {
            assert!(!valid_log_date(invalid), "{invalid}");
        }
    }
}
