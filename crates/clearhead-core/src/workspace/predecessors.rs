//! Resolve predecessor references against a workspace, then check ancestry at the source.

use std::collections::{HashMap, HashSet};

use uuid::Uuid;

use crate::domain::{Action, Charter, DomainModel};
use crate::reference::{
    LocalActions, ReferenceError, ReferenceOptions, ReferenceTarget,
    resolve_reference_with_local_actions,
};

use super::actions::{LintDiagnostic, ParsedDocument, SourceRange, SourcedAction};
use super::charter::MarkdownCharter;

/// The active Action and Charter candidates for `<`, without calendar Plans.
/// Unstamped template steps are visible only while checking their own source.
#[derive(Default)]
pub struct PredecessorScope {
    model: DomainModel,
}

impl PredecessorScope {
    pub fn new(charters: &[MarkdownCharter]) -> Self {
        Self {
            model: DomainModel {
                charters: charters
                    .iter()
                    .map(|charter| Charter {
                        id: charter.id,
                        alias: charter.alias.clone(),
                        title: charter.title.clone(),
                        parent: charter.parent.clone(),
                        actions: charter
                            .actions
                            .iter()
                            .map(|source| source.action.clone())
                            .collect(),
                        ..Default::default()
                    })
                    .collect(),
                objectives: Vec::new(),
            },
        }
    }

    pub(super) fn resolve(&self, reference: &str) -> Result<ReferenceTarget, ReferenceError> {
        self.resolve_with_local(reference, LocalActions::default())
    }

    pub(super) fn resolve_with_local(
        &self,
        reference: &str,
        local: LocalActions<'_>,
    ) -> Result<ReferenceTarget, ReferenceError> {
        resolve_reference_with_local_actions(
            &self.model,
            reference,
            &ReferenceOptions::default(),
            local,
        )
    }

    /// Check a template or standalone file, which is not among the active candidates.
    pub fn lint_document(&self, doc: &ParsedDocument) -> Vec<LintDiagnostic> {
        if !doc.syntax_errors.is_empty() {
            return Vec::new();
        }
        self.lint_local(&doc.actions, None, |id| {
            doc.source_map.get(&id).map(|s| s.root)
        })
    }

    pub(super) fn lint_local(
        &self,
        actions: &[Action],
        charter_id: Option<Uuid>,
        source_range: impl Fn(Uuid) -> Option<SourceRange>,
    ) -> Vec<LintDiagnostic> {
        self.lint(
            actions
                .iter()
                .map(|action| (action, source_range(action.id))),
            LocalActions {
                actions,
                charter_id,
            },
        )
    }

    pub(super) fn lint_active(&self, actions: &[SourcedAction]) -> Vec<LintDiagnostic> {
        self.lint(
            actions
                .iter()
                .map(|s| (&s.action, s.source_metadata.as_ref().map(|m| m.root))),
            LocalActions::default(),
        )
    }

    fn lint<'a>(
        &self,
        actions: impl Iterator<Item = (&'a Action, Option<SourceRange>)>,
        local: LocalActions<'_>,
    ) -> Vec<LintDiagnostic> {
        let actions: Vec<_> = actions.collect();
        let by_id: HashMap<_, _> = actions
            .iter()
            .map(|(action, _)| (action.id, *action))
            .collect();
        let mut diagnostics = Vec::new();
        for (action, range) in actions {
            let (Some(predecessors), Some(range), Some(mut parent)) =
                (&action.predecessors, range, action.parent_id)
            else {
                continue;
            };
            let mut ancestors = HashSet::new();
            while let Some(ancestor) = by_id.get(&parent) {
                if ancestor.id == action.id || !ancestors.insert(parent) {
                    break;
                }
                let Some(next) = ancestor.parent_id else {
                    break;
                };
                parent = next;
            }
            for predecessor in predecessors {
                let Ok(ReferenceTarget::Action(id)) =
                    self.resolve_with_local(&predecessor.raw_ref, local)
                else {
                    continue;
                };
                if ancestors.contains(&id) {
                    diagnostics.push(LintDiagnostic::error(
                        "E009",
                        format!(
                            "predecessor '{}' resolves to ancestor '{}'",
                            predecessor.raw_ref, by_id[&id].name
                        ),
                        range,
                    ));
                }
            }
        }
        diagnostics
    }
}
