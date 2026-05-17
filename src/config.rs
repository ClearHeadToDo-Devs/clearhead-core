//! Semantic workspace configuration.
//!
//! [`WorkspaceConfig`] carries the settings that have semantic meaning — they
//! affect the data model and graph behaviour, not presentation or UI.
//!
//! **Core defines this struct. Tools own loading it.**
//!
//! `clearhead-core` never reads `config.json` from disk. Each tool
//! (CLI, nvim, etc.) loads configuration from the appropriate source,
//! constructs a `WorkspaceConfig` from the shared fields, and passes it
//! into core functions that need it. Tool-specific settings (`cli_*`,
//! `nvim_*`) never enter core.

use std::collections::{HashMap, HashSet};

/// Semantic workspace configuration shared across all ClearHead tools.
///
/// Corresponds to the core settings in `config.schema.json` in the
/// specifications repository. Tools are responsible for loading and
/// constructing this from `~/.config/clearhead/config.json`; core only
/// consumes it.
#[derive(Debug, Clone, Default)]
pub struct WorkspaceConfig {
    /// Tag parent→children relationships for implicit context inheritance.
    ///
    /// Maps a parent tag to a list of direct child tags. Traversal is
    /// transitive: if `terminal` is a child of `computer` and `neovim` is a
    /// child of `terminal`, then `+neovim` implicitly includes both ancestors.
    ///
    /// Used by the graph layer to emit `contextBroader`/`contextNarrower`
    /// triples and by query expansion to match ancestor tags.
    pub tag_hierarchies: HashMap<String, Vec<String>>,

    /// When true, workspace discovery ignores project-scoped `.clearhead`
    /// directories and only loads user-scoped data from `data_dir`.
    pub default_to_user_scope: bool,

    /// Additional workspace directories to merge into the domain model.
    /// Each path should follow the `.clearhead` directory layout.
    pub additional_workspaces: Vec<String>,
}

impl WorkspaceConfig {
    /// Create an empty config (same as `Default`).
    pub fn new() -> Self {
        Self::default()
    }

    /// Return all ancestor tags for `tag` in order from immediate parent to
    /// root. Cycle-safe.
    ///
    /// ```
    /// use clearhead_core::WorkspaceConfig;
    /// use std::collections::HashMap;
    ///
    /// let mut h = HashMap::new();
    /// h.insert("computer".to_string(), vec!["terminal".to_string()]);
    /// h.insert("terminal".to_string(), vec!["neovim".to_string()]);
    /// let cfg = WorkspaceConfig { tag_hierarchies: h, ..Default::default() };
    ///
    /// assert_eq!(cfg.get_tag_ancestors("neovim"), vec!["terminal", "computer"]);
    /// assert_eq!(cfg.get_tag_ancestors("computer"), Vec::<String>::new());
    /// ```
    pub fn get_tag_ancestors(&self, tag: &str) -> Vec<String> {
        let tag_lower = tag.to_lowercase();
        let mut ancestors = Vec::new();
        let mut visited = HashSet::new();

        // Build child → parent reverse map (single pass)
        let mut child_to_parent: HashMap<String, String> = HashMap::new();
        for (parent, children) in &self.tag_hierarchies {
            let parent_lower = parent.to_lowercase();
            for child in children {
                child_to_parent.insert(child.to_lowercase(), parent_lower.clone());
            }
        }

        let mut current = tag_lower;
        while let Some(parent) = child_to_parent.get(&current) {
            if !visited.insert(parent.clone()) {
                break; // cycle guard
            }
            ancestors.push(parent.clone());
            current = parent.clone();
        }
        ancestors
    }

    /// Expand `tag` to itself plus all ancestor tags.
    pub fn expand_tag(&self, tag: &str) -> Vec<String> {
        let mut expanded = vec![tag.to_lowercase()];
        expanded.extend(self.get_tag_ancestors(tag));
        expanded
    }

    /// Expand a slice of tags to include all ancestor tags.
    /// Returns a deduplicated, unsorted list.
    pub fn expand_tags(&self, tags: &[String]) -> Vec<String> {
        let mut all: HashSet<String> = HashSet::new();
        for tag in tags {
            for t in self.expand_tag(tag) {
                all.insert(t);
            }
        }
        all.into_iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hierarchy() -> WorkspaceConfig {
        let mut h = HashMap::new();
        h.insert("computer".to_string(), vec!["terminal".to_string(), "browser".to_string()]);
        h.insert("terminal".to_string(), vec!["neovim".to_string(), "tmux".to_string()]);
        h.insert("driving".to_string(), vec!["grocery_store".to_string()]);
        WorkspaceConfig { tag_hierarchies: h, ..Default::default() }
    }

    #[test]
    fn ancestors_single_level() {
        let cfg = hierarchy();
        assert_eq!(cfg.get_tag_ancestors("terminal"), vec!["computer"]);
        assert_eq!(cfg.get_tag_ancestors("browser"), vec!["computer"]);
    }

    #[test]
    fn ancestors_multi_level() {
        let cfg = hierarchy();
        assert_eq!(cfg.get_tag_ancestors("neovim"), vec!["terminal", "computer"]);
    }

    #[test]
    fn ancestors_root_is_empty() {
        let cfg = hierarchy();
        assert!(cfg.get_tag_ancestors("computer").is_empty());
    }

    #[test]
    fn ancestors_unknown_tag_is_empty() {
        let cfg = hierarchy();
        assert!(cfg.get_tag_ancestors("unknown").is_empty());
    }

    #[test]
    fn ancestors_case_insensitive() {
        let cfg = hierarchy();
        assert_eq!(cfg.get_tag_ancestors("NEOVIM"), vec!["terminal", "computer"]);
    }

    #[test]
    fn expand_tag_includes_self() {
        let cfg = hierarchy();
        let mut got = cfg.expand_tag("neovim");
        got.sort();
        assert_eq!(got, vec!["computer", "neovim", "terminal"]);
    }

    #[test]
    fn expand_tags_deduplicates() {
        let cfg = hierarchy();
        let mut got = cfg.expand_tags(&["neovim".to_string(), "grocery_store".to_string()]);
        got.sort();
        assert_eq!(got, vec!["computer", "driving", "grocery_store", "neovim", "terminal"]);
    }

    #[test]
    fn cycle_guard_does_not_loop() {
        let mut h = HashMap::new();
        h.insert("a".to_string(), vec!["b".to_string()]);
        h.insert("b".to_string(), vec!["a".to_string()]); // cycle
        let cfg = WorkspaceConfig { tag_hierarchies: h, ..Default::default() };
        // must terminate
        let _ = cfg.get_tag_ancestors("a");
        let _ = cfg.get_tag_ancestors("b");
    }
}
