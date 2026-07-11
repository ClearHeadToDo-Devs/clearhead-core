//! Charter parsing and formatting.
//!
//! Charters are markdown files with optional YAML frontmatter that define
//! high-level directives organizing plans under a shared purpose.
//!
//! Format:
//! ```text
//! ---
//! id: <uuid>
//! alias: health
//! parent: lifestyle
//! objectives:
//!   - lose-weight
//! ---
//! # Health & Fitness
//!
//! Description of the charter goes here.
//! ```

use serde::Deserialize;
use std::path::PathBuf;
use uuid::Uuid;

use crate::domain::{Charter, CharterState};
use crate::workspace::actions::repository::SourcedAction;
use crate::workspace::calendar::ics::ICSPlan;

/// A charter as it exists in the workspace — carries file paths alongside domain data.
///
/// Use `From<MarkdownCharter> for Charter` to obtain a pure domain object.
#[derive(Debug, Clone)]
pub struct MarkdownCharter {
    pub id: Uuid,
    pub title: String,
    pub description: Option<String>,
    pub alias: Option<String>,
    pub parent: Option<String>,
    pub objectives: Option<Vec<String>>,
    /// Lifecycle state parsed from the charter `.md` frontmatter.
    /// `None` means the state has not been explicitly set (treated as `New`).
    pub state: Option<CharterState>,
    pub plans: Vec<ICSPlan>,
    pub actions: Vec<SourcedAction>,

    pub md_file: Option<PathBuf>,
    pub actions_file: Option<PathBuf>,
    pub plans_dir: Option<PathBuf>,
}

impl From<MarkdownCharter> for Charter {
    fn from(mc: MarkdownCharter) -> Charter {
        Charter {
            id: mc.id,
            title: mc.title,
            description: mc.description,
            alias: mc.alias,
            parent: mc.parent,
            objectives: mc.objectives,
            state: mc.state,
            plans: mc.plans.into_iter().map(|ip| ip.plan).collect(),
            actions: mc.actions.into_iter().map(|sa| sa.action).collect(),
        }
    }
}

impl From<Charter> for MarkdownCharter {
    fn from(c: Charter) -> MarkdownCharter {
        MarkdownCharter {
            id: c.id,
            title: c.title,
            description: c.description,
            alias: c.alias,
            parent: c.parent,
            objectives: c.objectives,
            state: c.state,
            plans: c.plans.into_iter().map(|plan| ICSPlan { path: PathBuf::new(), plan }).collect(),
            actions: c.actions.into_iter().map(|action| SourcedAction {
                action,
                source_metadata: None,
            }).collect(),
            md_file: None,
            actions_file: None,
            plans_dir: None,
        }
    }
}

/// Namespace UUID for deterministic charter IDs (v5).
const CHARTER_NS: Uuid = Uuid::from_bytes([
    0x63, 0x68, 0x61, 0x72, 0x74, 0x65, 0x72, 0x2d, 0x6e, 0x73, 0x2d, 0x75, 0x75, 0x69, 0x64, 0x00,
]);

/// Internal frontmatter representation.
#[derive(Deserialize, Default)]
struct CharterFrontmatter {
    id: Option<Uuid>,
    title: Option<String>,
    alias: Option<String>,
    parent: Option<String>,
    objectives: Option<Vec<String>>,
    state: Option<CharterState>,
}

/// Parse a [`Charter`] from markdown content with optional YAML frontmatter.
///
/// Title resolution order:
/// 1. `title` field in frontmatter
/// 2. First H1 header (`# ...`)
/// 3. Error
///
/// ID resolution:
/// 1. `id` field in frontmatter
/// 2. Deterministic v5 UUID from title
pub fn parse_charter(content: &str) -> Result<Charter, String> {
    let (frontmatter, body) = split_frontmatter(content);

    let fm: CharterFrontmatter = match frontmatter {
        Some(yaml) => {
            serde_yaml_ng::from_str(yaml).map_err(|e| format!("Invalid charter frontmatter: {}", e))?
        }
        None => CharterFrontmatter::default(),
    };

    let (h1_title, description) = extract_title_and_description(body);

    let title = fm.title.or(h1_title).ok_or_else(|| {
        "Charter must have a title (frontmatter `title` or H1 header)".to_string()
    })?;

    let id = fm
        .id
        .unwrap_or_else(|| Uuid::new_v5(&CHARTER_NS, title.as_bytes()));

    Ok(Charter {
        id,
        title,
        description,
        alias: fm.alias,
        parent: fm.parent,
        objectives: fm.objectives,
        state: fm.state,
        plans: vec![],
        actions: vec![],
    })
}

/// Create a minimal implicit charter from a name.
///
/// Uses a deterministic v5 UUID so the same name always produces the same ID.
pub fn implicit_charter(name: &str) -> Charter {
    Charter {
        id: Uuid::new_v5(&CHARTER_NS, name.as_bytes()),
        title: name.to_string(),
        description: None,
        alias: Some(name.to_string()),
        parent: None,
        objectives: None,
        state: None,
        plans: vec![],
        actions: vec![],
    }
}

/// Render a charter back to markdown with YAML frontmatter.
pub fn format_charter(charter: &Charter) -> String {
    let mut out = String::new();

    // Frontmatter
    out.push_str("---\n");
    out.push_str(&format!("id: {}\n", charter.id));
    if let Some(ref alias) = charter.alias {
        out.push_str(&format!("alias: {}\n", alias));
    }
    if let Some(ref parent) = charter.parent {
        out.push_str(&format!("parent: {}\n", parent));
    }
    if let Some(ref objectives) = charter.objectives {
        out.push_str("objectives:\n");
        for obj in objectives {
            out.push_str(&format!("  - {}\n", obj));
        }
    }
    if let Some(ref state) = charter.state {
        out.push_str(&format!("state: {}\n", state));
    }
    out.push_str("---\n");

    // Title
    out.push_str(&format!("# {}\n", charter.title));

    // Description
    if let Some(ref desc) = charter.description {
        out.push('\n');
        out.push_str(desc);
        if !desc.ends_with('\n') {
            out.push('\n');
        }
    }

    out
}

/// Split content into optional YAML frontmatter and body.
///
/// Frontmatter must start on line 1 with `---` and close with `---`.
fn split_frontmatter(content: &str) -> (Option<&str>, &str) {
    let trimmed = content.trim_start();
    if !trimmed.starts_with("---") {
        return (None, content);
    }

    // Find the opening delimiter end
    let after_first = &trimmed[3..];
    let after_first = after_first.trim_start_matches(['\r', '\n']);

    // Find closing ---
    if let Some(close_pos) = after_first.find("\n---") {
        let yaml = &after_first[..close_pos];
        let rest_start = close_pos + 4; // skip \n---
        let rest = if rest_start < after_first.len() {
            after_first[rest_start..].trim_start_matches(['\r', '\n'])
        } else {
            ""
        };
        (Some(yaml), rest)
    } else {
        // No closing delimiter — treat entire content as body
        (None, content)
    }
}

/// Return true if the markdown frontmatter contains an explicit `parent:` key.
///
/// Parses the YAML block into a raw map to distinguish a present `parent: ~`
/// (explicit null → no parent) from an absent `parent:` key (implicit parenting
/// still applies). Called by the workspace loader to decide whether path-inferred
/// parent hints should be suppressed for this charter.
pub(crate) fn frontmatter_has_parent_key(content: &str) -> bool {
    let (frontmatter, _) = split_frontmatter(content);
    frontmatter
        .and_then(|yaml| {
            serde_yaml_ng::from_str::<std::collections::HashMap<String, serde_yaml_ng::Value>>(yaml).ok()
        })
        .map(|map| map.contains_key("parent"))
        .unwrap_or(false)
}

/// Return true if the markdown frontmatter contains an explicit `id:` key.
///
/// Lets the loader tell a *declared* charter identity from a `v5(title)` seed:
/// only a declared id is authoritative, so a charter without this key may have
/// its id superseded by a recorded sidecar `charter.id`.
pub(crate) fn frontmatter_has_id_key(content: &str) -> bool {
    let (frontmatter, _) = split_frontmatter(content);
    frontmatter
        .and_then(|yaml| {
            serde_yaml_ng::from_str::<std::collections::HashMap<String, serde_yaml_ng::Value>>(yaml).ok()
        })
        .map(|map| map.contains_key("id"))
        .unwrap_or(false)
}

/// Extract the first H1 title and remaining description from markdown body.
fn extract_title_and_description(body: &str) -> (Option<String>, Option<String>) {
    let mut title = None;
    let mut desc_lines = Vec::new();
    let mut found_title = false;
    let mut past_title_blank = false;

    for line in body.lines() {
        if !found_title {
            if let Some(h1) = line.strip_prefix("# ") {
                title = Some(h1.trim().to_string());
                found_title = true;
                continue;
            }
            // Skip blank lines before title
            if line.trim().is_empty() {
                continue;
            }
            // Non-H1, non-blank line before we find a title — no H1 present
            break;
        }

        // After title: skip the first blank line, then collect description
        if !past_title_blank && line.trim().is_empty() {
            past_title_blank = true;
            continue;
        }
        if found_title {
            past_title_blank = true;
            desc_lines.push(line);
        }
    }

    let description = if desc_lines.is_empty() {
        None
    } else {
        let desc = desc_lines.join("\n").trim().to_string();
        if desc.is_empty() { None } else { Some(desc) }
    };

    (title, description)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_full_frontmatter() {
        let content = r#"---
id: 01234567-89ab-cdef-0123-456789abcdef
title: Health & Fitness
alias: health
parent: lifestyle
objectives:
  - lose-weight
  - run-marathon
---
# Health & Fitness

Stay healthy and fit through regular exercise and diet.
"#;
        let charter = parse_charter(content).unwrap();
        assert_eq!(charter.title, "Health & Fitness");
        assert_eq!(
            charter.id,
            Uuid::parse_str("01234567-89ab-cdef-0123-456789abcdef").unwrap()
        );
        assert_eq!(charter.alias, Some("health".to_string()));
        assert_eq!(charter.parent, Some("lifestyle".to_string()));
        assert_eq!(
            charter.objectives,
            Some(vec!["lose-weight".to_string(), "run-marathon".to_string()])
        );
        assert_eq!(
            charter.description,
            Some("Stay healthy and fit through regular exercise and diet.".to_string())
        );
    }

    #[test]
    fn test_parse_no_frontmatter() {
        let content = "# My Project\n\nThis is a project charter.\n";
        let charter = parse_charter(content).unwrap();
        assert_eq!(charter.title, "My Project");
        assert_eq!(
            charter.description,
            Some("This is a project charter.".to_string())
        );
        // ID should be deterministic from title
        assert_eq!(charter.id, Uuid::new_v5(&CHARTER_NS, b"My Project"));
    }

    #[test]
    fn test_parse_title_from_frontmatter_overrides_h1() {
        let content = "---\ntitle: Official Title\n---\n# Markdown Title\n";
        let charter = parse_charter(content).unwrap();
        assert_eq!(charter.title, "Official Title");
    }

    #[test]
    fn test_parse_missing_title_errors() {
        let content = "Some random text without a heading.\n";
        let result = parse_charter(content);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("title"));
    }

    #[test]
    fn test_implicit_charter() {
        let charter = implicit_charter("health");
        assert_eq!(charter.title, "health");
        assert!(charter.description.is_none());
        assert_eq!(charter.alias.unwrap(), "health");
    }

    #[test]
    fn test_deterministic_uuid_stability() {
        let c1 = implicit_charter("health");
        let c2 = implicit_charter("health");
        assert_eq!(c1.id, c2.id);

        let c3 = implicit_charter("work");
        assert_ne!(c1.id, c3.id);
    }

    #[test]
    fn test_format_charter_round_trip() {
        let charter = Charter {
            id: Uuid::new_v5(&CHARTER_NS, b"Test"),
            title: "Test".to_string(),
            description: Some("A test charter.".to_string()),
            alias: Some("test".to_string()),
            parent: None,
            objectives: Some(vec!["obj1".to_string()]),
            state: None,
            plans: vec![],
            actions: vec![],
        };

        let formatted = format_charter(&charter);
        let parsed = parse_charter(&formatted).unwrap();

        assert_eq!(parsed.id, charter.id);
        assert_eq!(parsed.title, charter.title);
        assert_eq!(parsed.description, charter.description);
        assert_eq!(parsed.alias, charter.alias);
        assert_eq!(parsed.objectives, charter.objectives);
    }

    #[test]
    fn test_parse_h1_only_no_description() {
        let content = "# Just a Title\n";
        let charter = parse_charter(content).unwrap();
        assert_eq!(charter.title, "Just a Title");
        assert!(charter.description.is_none());
    }
}
