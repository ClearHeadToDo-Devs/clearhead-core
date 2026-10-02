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

/// Parse an objective document. `file_name` is the name its file gives it
/// ([`objective_file_name`]); the root objective has none.
///
/// `id` is the one required field: an objective without one is an error, never
/// given an id here. The alias is the frontmatter `alias`, else the file name;
/// the title is the frontmatter `title`, else the first H1, else the file name.
pub fn parse_objective(content: &str, file_name: Option<&str>) -> Result<Objective, String> {
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
        title: fm
            .title
            .or(h1_title)
            .or_else(|| file_name.map(str::to_string)),
        description,
        alias: fm.alias.or_else(|| file_name.map(str::to_string)),
        parent: fm.parent,
        metrics: fm.metrics,
    })
}

/// A new objective document declaring only its identity and name, as `init`
/// writes the root objective.
pub fn format_new_objective(id: Uuid, alias: &str) -> String {
    let alias_yaml = serde_yaml_ng::to_string(alias).unwrap_or_else(|_| format!("{alias}\n"));
    format!("---\nid: {id}\nalias: {alias_yaml}---\n# {alias}\n")
}

/// The name an objective's file gives it, from its path under `objectives/`:
/// `<name>.md` and `<name>/README.md` are `<name>`; the root objective,
/// `README.md` itself, takes its name only from its frontmatter.
pub fn objective_file_name(relative: &str) -> Option<&str> {
    let (parent, file) = relative.rsplit_once('/').unwrap_or(("", relative));
    if file == "README.md" {
        (!parent.is_empty()).then(|| parent.rsplit('/').next().unwrap_or(parent))
    } else {
        file.strip_suffix(".md")
    }
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
        let objective = parse_objective(&doc, Some("file-stem")).unwrap();
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
        let objective =
            parse_objective(&format!("---\nid: {ID}\n---\n"), Some("keep-food")).unwrap();
        assert_eq!(objective.alias.as_deref(), Some("keep-food"));
        assert_eq!(objective.title.as_deref(), Some("keep-food"));
    }

    #[test]
    fn file_names_follow_the_workspace_layout() {
        assert_eq!(objective_file_name("eat-well.md"), Some("eat-well"));
        assert_eq!(objective_file_name("health/README.md"), Some("health"));
        assert_eq!(objective_file_name("health/sleep.md"), Some("sleep"));
        assert_eq!(objective_file_name("README.md"), None);
    }

    #[test]
    fn the_root_objective_is_named_only_by_its_frontmatter() {
        let unnamed = parse_objective(&format!("---\nid: {ID}\n---\n"), None).unwrap();
        assert_eq!((unnamed.alias, unnamed.title), (None, None));
        let named =
            parse_objective(&format!("---\nid: {ID}\nalias: home\n---\n# Home\n"), None).unwrap();
        assert_eq!(named.alias.as_deref(), Some("home"));
    }

    #[test]
    fn a_new_objective_parses_back_with_its_id_and_alias() {
        let id: Uuid = ID.parse().unwrap();
        let objective = parse_objective(&format_new_objective(id, "home"), None).unwrap();
        assert_eq!(objective.id, id);
        assert_eq!(objective.alias.as_deref(), Some("home"));
        assert_eq!(objective.title.as_deref(), Some("home"));
    }

    #[test]
    fn an_objective_without_an_id_is_an_error() {
        let error = parse_objective("# Eat well\n", Some("eat-well")).unwrap_err();
        assert!(error.contains("no id"), "{error}");
    }

    #[test]
    fn unknown_frontmatter_keys_are_ignored() {
        let doc = format!("---\nid: {ID}\nstate: Active\n---\n# Eat well\n");
        assert!(parse_objective(&doc, Some("eat-well")).is_ok());
    }
}
