//! File-role semantics shared by assembly, lint and editor diagnostics.
use super::action_files::{ActionFileRole, classify_action_file};
use super::actions::{LintDiagnostic, ParsedDocument};
use crate::domain::{Action, Habit, collect_subtree_ids};
use std::path::Path;

pub fn habits_from_actions(actions: &[Action]) -> Vec<Habit> {
    actions
        .iter()
        .filter(|action| action.parent_id.is_none())
        .map(|root| {
            let ids = collect_subtree_ids(actions, root.id);
            let mut definition = root.clone();
            let recurrence_rule = definition
                .recurrence_rule
                .take()
                .unwrap_or_default()
                .trim()
                .to_string();
            let (template, description) = super::calendar::ics::parse_description_directives(
                definition.description.as_deref().unwrap_or(""),
            );
            definition.description = description;
            Habit {
                definition,
                recurrence_rule,
                template,
                children: actions
                    .iter()
                    .filter(|a| a.id != root.id && ids.contains(&a.id))
                    .cloned()
                    .collect(),
            }
        })
        .collect()
}

/// Semantic diagnostics never turn syntactically valid R: into parser recovery.
pub fn lint_action_file(doc: &ParsedDocument, path: &Path) -> Vec<LintDiagnostic> {
    let template =
        super::action_files::resource_role(path) == super::action_files::ResourceRole::Template;
    let habits = !template
        && matches!(
            classify_action_file(path),
            Some((_, ActionFileRole::Habits))
        );
    let range = |id| {
        doc.source_map
            .get(&id)
            .map(|source| source.root)
            .unwrap_or_default()
    };
    let mut diagnostics = Vec::new();
    if !habits {
        for action in &doc.actions {
            if action.recurrence_rule.is_some() {
                diagnostics.push(LintDiagnostic::warning(
                    "habit-scope",
                    "R: belongs in the charter's .habits.actions file; source rule retained".into(),
                    range(action.id),
                ));
            }
        }
        return diagnostics;
    }
    for habit in habits_from_actions(&doc.actions) {
        let root_range = range(habit.definition.id);
        if let Err(error) = habit.validate() {
            diagnostics.push(LintDiagnostic::error("E002", error, root_range));
        }
        if habit.template.is_some() && !habit.children.is_empty() {
            diagnostics.push(LintDiagnostic::warning(
                "habit-template-shadowed",
                "inline children win over the named template".into(),
                root_range,
            ));
        }
        for action in std::iter::once(&habit.definition).chain(habit.children.iter()) {
            let source = doc.source_map.get(&action.id);
            if source.is_some_and(|source| source.is_id_generated) {
                diagnostics.push(LintDiagnostic::warning(
                    "habit-id-missing",
                    "persist Habit and template-step ids before stamping".into(),
                    range(action.id),
                ));
            }
            if action.parent_id.is_some() && action.recurrence_rule.is_some() {
                diagnostics.push(LintDiagnostic::error(
                    "E002",
                    "recurrence is only valid on Habit roots".into(),
                    range(action.id),
                ));
            }
        }
    }
    diagnostics
}
