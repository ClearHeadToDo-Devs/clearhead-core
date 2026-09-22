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

use crate::domain::update::CharterUpdate;
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
    /// Canonical collection path relative to the configured plans root.
    /// Calculated from the charter anchor; the directory need not exist.
    pub plans_dir: PathBuf,
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
        let collection_key = c.alias.as_deref().unwrap_or(&c.title);
        let plans_dir = match c.parent.as_deref() {
            Some(parent) => PathBuf::from(format!(
                "{}-{}",
                crate::workspace::slugify(parent),
                crate::workspace::slugify(collection_key)
            )),
            None => PathBuf::from(crate::workspace::slugify(collection_key)),
        };
        MarkdownCharter {
            id: c.id,
            title: c.title,
            description: c.description,
            alias: c.alias,
            parent: c.parent,
            objectives: c.objectives,
            state: c.state,
            plans: c
                .plans
                .into_iter()
                .map(|plan| ICSPlan {
                    path: PathBuf::new(),
                    plan,
                    component_kind: crate::config::PlanComponentKind::default(),
                    schedule_end: None,
                    task_fields: None,
                    exdates: Default::default(),
                    overrides: Default::default(),
                })
                .collect(),
            actions: c
                .actions
                .into_iter()
                .map(|action| SourcedAction {
                    action,
                    source_metadata: None,
                })
                .collect(),
            md_file: None,
            actions_file: None,
            plans_dir,
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

/// A charter document as it is written on disk: the frontmatter plus the
/// resolved title and body description, with an OPTIONAL id (I2).
///
/// A person may not have written an `id` yet, so the document view must not
/// mint one. Strictness increases only at the conversion into the domain
/// [`Charter`], which takes the id as an argument supplied by the shell —
/// Core never reads a clock or RNG here.
#[derive(Debug, Clone)]
pub struct CharterDocument {
    pub id: Option<Uuid>,
    pub title: String,
    pub description: Option<String>,
    pub alias: Option<String>,
    pub parent: Option<String>,
    pub objectives: Option<Vec<String>>,
    pub state: Option<CharterState>,
}

impl CharterDocument {
    /// Convert into a domain [`Charter`], using `fallback_id` when the document
    /// declares none (I2, I3).
    ///
    /// A declared frontmatter id always wins; otherwise the fallback is the
    /// *ephemeral* id the shell minted for this load. It is never persisted by
    /// a read, and never derived from a title or path.
    pub fn into_charter(self, fallback_id: Uuid) -> Charter {
        Charter {
            id: self.id.unwrap_or(fallback_id),
            title: self.title,
            description: self.description,
            alias: self.alias,
            parent: self.parent,
            objectives: self.objectives,
            state: self.state,
            plans: vec![],
            actions: vec![],
        }
    }
}

/// Parse a [`CharterDocument`] from markdown content with optional YAML frontmatter.
///
/// Title resolution order:
/// 1. `title` field in frontmatter
/// 2. First H1 header (`# ...`)
/// 3. Error
///
/// The document's `id` is whatever the frontmatter declares — `None` when a
/// person has not written one. Identity is never minted here and never
/// recomputed from mutable content (specifications/workspace.md, Concept
/// Identity): a title-derived id would silently change on every retitle and
/// orphan the references to it. Callers convert with
/// [`CharterDocument::into_charter`], supplying an id for the missing case; the
/// workspace loader then adopts a `charter.id` recorded in the sidecar when the
/// document declares none.
pub fn parse_charter(content: &str) -> Result<CharterDocument, String> {
    let (frontmatter, body) = split_frontmatter(content);

    let fm: CharterFrontmatter = match frontmatter {
        Some(yaml) => serde_yaml_ng::from_str(yaml)
            .map_err(|e| format!("Invalid charter frontmatter: {}", e))?,
        None => CharterFrontmatter::default(),
    };

    let (h1_title, description) = extract_title_and_description(body);

    let title = fm.title.or(h1_title).ok_or_else(|| {
        "Charter must have a title (frontmatter `title` or H1 header)".to_string()
    })?;

    Ok(CharterDocument {
        id: fm.id,
        title,
        description,
        alias: fm.alias,
        parent: fm.parent,
        objectives: fm.objectives,
        state: fm.state,
    })
}

/// Read a charter's declared frontmatter `id` without minting one.
///
/// Returns `None` when no `id` is declared, so a caller can tell a persisted
/// identity apart from the shell-supplied ephemeral fallback. [`parse_charter`]
/// exposes the same declared id as part of its [`CharterDocument`]; this helper
/// exists for callers that only need the id and never the rest of the document.
pub fn charter_frontmatter_id(content: &str) -> Result<Option<Uuid>, String> {
    match split_frontmatter(content).0 {
        Some(yaml) => serde_yaml_ng::from_str::<CharterFrontmatter>(yaml)
            .map(|fm| fm.id)
            .map_err(|e| format!("Invalid charter frontmatter: {}", e)),
        None => Ok(None),
    }
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

/// Apply charter metadata edits directly to the source document.
///
/// This preserves every byte outside the touched frontmatter field or title
/// line, including unmodeled frontmatter. Missing fields are inserted without
/// inventing an `id`; document creation is the caller's responsibility.
pub fn edit_charter_document(content: &str, update: &CharterUpdate) -> Result<String, String> {
    // Validate the source before attempting surgical edits.
    parse_charter(content)?;

    let mut edited = content.to_string();
    if let Some(alias) = &update.alias {
        edited = set_frontmatter_field(&edited, "alias", &yaml_string(alias));
    }
    if let Some(state) = update.state {
        edited = set_frontmatter_field(&edited, "state", &state.to_string());
    }
    if let Some(title) = &update.title {
        if frontmatter_has_key(&edited, "title") {
            edited = set_frontmatter_field(&edited, "title", &yaml_string(title));
            // Keep a present display heading aligned with the authoritative
            // frontmatter title; a title-only document need not grow an H1.
            if let Some(with_heading) = replace_h1_title_if_present(&edited, title) {
                edited = with_heading;
            }
        } else {
            edited = replace_h1_title(&edited, title)?;
        }
    }

    // Ensure the edited document still satisfies the charter codec.
    parse_charter(&edited)?;
    Ok(edited)
}

fn yaml_string(value: &str) -> String {
    serde_yaml_ng::to_string(value)
        .expect("serializing a string as YAML cannot fail")
        .trim_end()
        .to_string()
}

fn line_spans(content: &str) -> Vec<(usize, usize, usize)> {
    let mut offset = 0;
    content
        .split_inclusive('\n')
        .map(|line| {
            let start = offset;
            offset += line.len();
            let text_end = offset - usize::from(line.ends_with('\n'));
            let text_end = text_end
                - usize::from(content.as_bytes().get(text_end.wrapping_sub(1)) == Some(&b'\r'));
            (start, text_end, offset)
        })
        .collect()
}

fn frontmatter_span(content: &str) -> Option<(usize, usize)> {
    let spans = line_spans(content);
    let first = spans.first()?;
    if &content[first.0..first.1] != "---" {
        return None;
    }
    spans
        .iter()
        .skip(1)
        .find(|span| &content[span.0..span.1] == "---")
        .map(|closing| (first.2, closing.0))
}

fn frontmatter_has_key(content: &str, key: &str) -> bool {
    let Some((start, end)) = frontmatter_span(content) else {
        return false;
    };
    line_spans(content).into_iter().any(|span| {
        span.0 >= start
            && span.0 < end
            && content[span.0..span.1]
                .strip_prefix(key)
                .is_some_and(|rest| rest.starts_with(':'))
    })
}

fn set_frontmatter_field(content: &str, key: &str, value: &str) -> String {
    let newline = if content.contains("\r\n") {
        "\r\n"
    } else {
        "\n"
    };
    let replacement = format!("{key}: {value}");

    let Some((start, end)) = frontmatter_span(content) else {
        return format!("---{newline}{replacement}{newline}---{newline}{content}");
    };
    if let Some(span) = line_spans(content).into_iter().find(|span| {
        span.0 >= start
            && span.0 < end
            && content[span.0..span.1]
                .strip_prefix(key)
                .is_some_and(|rest| rest.starts_with(':'))
    }) {
        return format!(
            "{}{}{}",
            &content[..span.0],
            replacement,
            &content[span.1..]
        );
    }

    format!(
        "{}{}{newline}{}",
        &content[..end],
        replacement,
        &content[end..]
    )
}

fn h1_span(content: &str) -> Option<(usize, usize, usize)> {
    let body_start = frontmatter_span(content).map_or(0, |(_, end)| {
        line_spans(content)
            .into_iter()
            .find(|span| span.0 == end)
            .map_or(end, |span| span.2)
    });
    line_spans(content)
        .into_iter()
        .find(|span| span.0 >= body_start && content[span.0..span.1].starts_with("# "))
}

fn replace_h1_title_if_present(content: &str, title: &str) -> Option<String> {
    let span = h1_span(content)?;
    Some(format!(
        "{}# {}{}",
        &content[..span.0],
        title,
        &content[span.1..]
    ))
}

fn replace_h1_title(content: &str, title: &str) -> Result<String, String> {
    replace_h1_title_if_present(content, title)
        .ok_or_else(|| "Charter must have an H1 title to update".to_string())
}

/// Append a single log entry as a bullet under the charter's `## Log` section,
/// returning the new markdown.
///
/// This is a *surgical* text edit: every byte of `content` outside the touched
/// region is preserved. It deliberately does not round-trip through
/// [`parse_charter`]/[`format_charter`], which regenerate the file from the
/// parsed model and would silently drop unmodeled frontmatter (e.g. `defaults`)
/// and normalize whitespace — wrong for an append-only log.
///
/// If a `## Log` section exists, the bullet is inserted after the last non-blank
/// line of that section (before trailing blank lines or a following heading).
/// Otherwise a `## Log` section is created at the end of the document.
pub fn append_log_entry(content: &str, entry: &str) -> String {
    let bullet = format!("- {}", entry.trim());
    let lines: Vec<&str> = content.lines().collect();

    let Some(start) = lines.iter().position(|line| line.trim() == "## Log") else {
        // No log section: create one at the end, separated from prior content.
        let mut result = content.trim_end().to_string();
        if !result.is_empty() {
            result.push_str("\n\n");
        }
        result.push_str("## Log\n\n");
        result.push_str(&bullet);
        result.push('\n');
        return result;
    };

    // The section runs until the next heading (level 1 or 2) or end of file.
    let end = lines[start + 1..]
        .iter()
        .position(|line| line.starts_with("# ") || line.starts_with("## "))
        .map(|rel| start + 1 + rel)
        .unwrap_or(lines.len());
    // Insert right after the last non-blank line inside the section, so the
    // bullet joins the list rather than landing past trailing blank lines.
    let insert_at = (start + 1..end)
        .rev()
        .find(|&i| !lines[i].trim().is_empty())
        .map(|i| i + 1)
        .unwrap_or(end);

    let mut out: Vec<&str> = Vec::with_capacity(lines.len() + 1);
    out.extend_from_slice(&lines[..insert_at]);
    out.push(bullet.as_str());
    out.extend_from_slice(&lines[insert_at..]);
    let mut result = out.join("\n");
    if content.ends_with('\n') {
        result.push('\n');
    }
    result
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
            serde_yaml_ng::from_str::<std::collections::HashMap<String, serde_yaml_ng::Value>>(yaml)
                .ok()
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
            serde_yaml_ng::from_str::<std::collections::HashMap<String, serde_yaml_ng::Value>>(yaml)
                .ok()
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
        let document = parse_charter(content).unwrap();
        assert_eq!(document.title, "Health & Fitness");
        assert_eq!(
            document.id,
            Some(Uuid::parse_str("01234567-89ab-cdef-0123-456789abcdef").unwrap())
        );
        assert_eq!(document.alias, Some("health".to_string()));
        assert_eq!(document.parent, Some("lifestyle".to_string()));
        assert_eq!(
            document.objectives,
            Some(vec!["lose-weight".to_string(), "run-marathon".to_string()])
        );
        assert_eq!(
            document.description,
            Some("Stay healthy and fit through regular exercise and diet.".to_string())
        );
    }

    #[test]
    fn test_parse_no_frontmatter() {
        let content = "# My Project\n\nThis is a project charter.\n";
        let document = parse_charter(content).unwrap();
        assert_eq!(document.title, "My Project");
        assert_eq!(
            document.description,
            Some("This is a project charter.".to_string())
        );
        // No declared id: parse never mints one (deterministic), so the id is
        // simply absent until the shell supplies one at conversion time.
        assert_eq!(document.id, None);
        assert_eq!(parse_charter(content).unwrap().id, None);
    }

    #[test]
    fn test_declared_id_survives_a_retitle() {
        let declared = "---\nid: 01234567-89ab-cdef-0123-456789abcdef\n---\n# Original\n";
        let retitled = "---\nid: 01234567-89ab-cdef-0123-456789abcdef\n---\n# Renamed\n";
        assert_eq!(
            parse_charter(declared).unwrap().id,
            parse_charter(retitled).unwrap().id
        );
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

        assert_eq!(parsed.id, Some(charter.id));
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

    #[test]
    fn test_parse_never_mints_an_id() {
        // An id-less document parses to `None` every time — deterministic, no
        // clock or RNG in core. The shell supplies the ephemeral id at
        // conversion time instead.
        let content = "# My Project\n\nBody.\n";
        assert_eq!(parse_charter(content).unwrap().id, None);
        assert_eq!(parse_charter(content).unwrap().id, None);
    }

    #[test]
    fn test_undeclared_id_gets_the_supplied_id() {
        let content = "# My Project\n";
        let supplied = Uuid::now_v7();
        let document = parse_charter(content).unwrap();
        assert_eq!(document.id, None);
        assert_eq!(document.into_charter(supplied).id, supplied);
    }

    #[test]
    fn test_declared_id_wins_over_supplied() {
        let declared = Uuid::parse_str("01234567-89ab-cdef-0123-456789abcdef").unwrap();
        let content = "---\nid: 01234567-89ab-cdef-0123-456789abcdef\n---\n# T\n";
        let document = parse_charter(content).unwrap();
        assert_eq!(document.id, Some(declared));
        assert_eq!(document.into_charter(Uuid::now_v7()).id, declared);
    }

    #[test]
    fn surgical_edit_preserves_unknown_frontmatter_and_does_not_mint_id() {
        let source = "---\ndefaults:\n  context: work\nalias: old\n---\n# Old title\n\nBody.\n";
        let edited = edit_charter_document(
            source,
            &CharterUpdate {
                title: Some("New title".to_string()),
                alias: Some("new".to_string()),
                state: Some(CharterState::Closed),
            },
        )
        .unwrap();

        assert!(edited.contains("defaults:\n  context: work\n"));
        assert!(edited.contains("alias: new\n"));
        assert!(edited.contains("state: Closed\n"));
        assert!(edited.contains("# New title\n\nBody.\n"));
        assert!(!edited.contains("id:"));
    }

    #[test]
    fn surgical_edit_updates_a_frontmatter_title_in_place() {
        let source = "---\ntitle: Old title\ncustom: keep\n---\n# Display heading\n";
        let edited = edit_charter_document(
            source,
            &CharterUpdate {
                title: Some("New: title".to_string()),
                ..Default::default()
            },
        )
        .unwrap();

        assert!(edited.contains("title: 'New: title'\n"));
        assert!(edited.contains("custom: keep\n"));
        assert!(edited.contains("# New: title\n"));
    }

    #[test]
    fn append_log_creates_section_when_absent() {
        let content = "---\nid: x\n---\n# Title\n\nSome description.\n";
        let out = append_log_entry(content, "first finding");
        assert_eq!(
            out,
            "---\nid: x\n---\n# Title\n\nSome description.\n\n## Log\n\n- first finding\n"
        );
    }

    #[test]
    fn append_log_adds_bullet_to_existing_section() {
        let content = "# Title\n\n## Log\n\n- old entry\n";
        let out = append_log_entry(content, "new entry");
        assert_eq!(out, "# Title\n\n## Log\n\n- old entry\n- new entry\n");
    }

    #[test]
    fn append_log_inserts_before_following_section_and_preserves_it() {
        // The log is not the last section; the bullet must land inside it and
        // the trailing sibling section must be untouched.
        let content = "# T\n\n## Log\n\n- a\n\n## Notes\n\nkeep me\n";
        let out = append_log_entry(content, "b");
        assert_eq!(out, "# T\n\n## Log\n\n- a\n- b\n\n## Notes\n\nkeep me\n");
    }

    #[test]
    fn append_log_preserves_unmodeled_frontmatter() {
        // The whole point of the surgical edit: `defaults` survives, where a
        // format_charter round-trip would drop it.
        let content = "---\nid: x\ndefaults:\n  priority: 3\n---\n# T\n";
        let out = append_log_entry(content, "note");
        assert!(out.contains("defaults:\n  priority: 3"), "{out}");
        assert!(out.ends_with("## Log\n\n- note\n"), "{out}");
    }
}
