/// Built-in Tree-sitter queries embedded into the binary via the tree-sitter-actions crate.
/// These allow users to run `clearhead_cli read --query p1` without needing external files.

pub fn get_builtin_query(name: &str) -> Option<&'static str> {
    match name {
        "p1" | "p1-actions" => Some(tree_sitter_actions::queries::P1_ACTIONS),
        "completed" | "completed-actions" => Some(tree_sitter_actions::queries::COMPLETED_ACTIONS),
        "in-progress" => Some(tree_sitter_actions::queries::IN_PROGRESS_ACTIONS),
        "blocked" | "blocked-actions" => Some(tree_sitter_actions::queries::BLOCKED_ACTIONS),
        "not-started" => Some(tree_sitter_actions::queries::NOT_STARTED_ACTIONS),
        "with-children" => Some(tree_sitter_actions::queries::WITH_CHILDREN),
        _ => None,
    }
}

pub fn list_builtins() -> Vec<&'static str> {
    vec![
        "p1",
        "completed",
        "in-progress",
        "blocked",
        "not-started",
        "with-children",
    ]
}
