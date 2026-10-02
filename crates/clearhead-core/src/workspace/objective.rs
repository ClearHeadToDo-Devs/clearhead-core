//! Objective parsing (specifications/objectives.md).
//!
//! An objective is a markdown file under `objectives/` with YAML frontmatter:
//! ```text
//! ---
//! id: <uuid>
//! alias: keep-food
//! parent: eat-well
//! metrics:
//!   - name: fridge stocked
//!     target: milk, eggs and vegetables on hand
//! ---
//! # Keep food in the house
//!
//! Description of the objective goes here.
//! ```

use serde::Deserialize;
use uuid::Uuid;

use super::charter::{extract_title_and_description, split_frontmatter};
use crate::domain::{Metric, Objective};

#[derive(Deserialize, Default)]
struct ObjectiveFrontmatter {
    id: Option<Uuid>,
    title: Option<String>,
    alias: Option<String>,
    parent: Option<String>,
    metrics: Option<Vec<Metric>>,
}

/// Parse an objective document. `stem` is the file name without `.md`.
///
/// `id` is the one required field: an objective without one is an error, never
/// given an id here. The title is the frontmatter `title`, else the first H1,
/// else the stem; the alias is the frontmatter `alias`, else the stem, since
/// the file name is the objective's name when no alias is written.
pub fn parse_objective(content: &str, stem: &str) -> Result<Objective, String> {
    let (frontmatter, body) = split_frontmatter(content);
    let fm: ObjectiveFrontmatter = match frontmatter {
        Some(yaml) => serde_yaml_ng::from_str(yaml)
            .map_err(|e| format!("Invalid objective frontmatter: {e}"))?,
        None => ObjectiveFrontmatter::default(),
    };
    let id = fm.id.ok_or_else(|| {
        "objective declares no id; add an `id:` (a UUIDv7) to its frontmatter".to_string()
    })?;
    let (h1_title, description) = extract_title_and_description(body);
    Ok(Objective {
        id,
        title: Some(fm.title.or(h1_title).unwrap_or_else(|| stem.to_string())),
        description,
        alias: Some(fm.alias.unwrap_or_else(|| stem.to_string())),
        parent: fm.parent,
        metrics: fm.metrics,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    const ID: &str = "01a0fb10-0000-7000-8000-0000000000b1";

    #[test]
    fn every_field_is_read() {
        let doc = format!(
            "---\nid: {ID}\nalias: keep-food\nparent: eat-well\nmetrics:\n  - name: fridge stocked\n    target: milk on hand\n---\n# Keep food in the house\n\nNobody opens an empty fridge.\n"
        );
        let objective = parse_objective(&doc, "file-stem").unwrap();
        assert_eq!(objective.id.to_string(), ID);
        assert_eq!(objective.title.as_deref(), Some("Keep food in the house"));
        assert_eq!(
            objective.description.as_deref(),
            Some("Nobody opens an empty fridge.")
        );
        assert_eq!(objective.alias.as_deref(), Some("keep-food"));
        assert_eq!(objective.parent.as_deref(), Some("eat-well"));
        let metrics = objective.metrics.unwrap();
        assert_eq!(metrics[0].name, "fridge stocked");
        assert_eq!(metrics[0].target.as_deref(), Some("milk on hand"));
    }

    #[test]
    fn alias_and_title_fall_back_to_the_file_stem() {
        let objective = parse_objective(&format!("---\nid: {ID}\n---\n"), "keep-food").unwrap();
        assert_eq!(objective.alias.as_deref(), Some("keep-food"));
        assert_eq!(objective.title.as_deref(), Some("keep-food"));
    }

    #[test]
    fn an_objective_without_an_id_is_an_error() {
        let error = parse_objective("# Eat well\n", "eat-well").unwrap_err();
        assert!(error.contains("no id"), "{error}");
    }

    #[test]
    fn unknown_frontmatter_keys_are_ignored() {
        let doc = format!("---\nid: {ID}\nstate: Active\n---\n# Eat well\n");
        assert!(parse_objective(&doc, "eat-well").is_ok());
    }
}
