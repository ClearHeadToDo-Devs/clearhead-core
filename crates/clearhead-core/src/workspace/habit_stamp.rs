//! Pure generation of native Habit occurrences; no slot discovery or writes.

mod catch_up;
mod fast_forward;
mod schedule;

pub use catch_up::{
    CatchUpBatch, CatchUpInput, plan_catch_up, plan_text_catch_up, text_exclusions,
};
pub use fast_forward::{FastForward, fast_forward_text_habit};

use std::collections::{HashMap, HashSet};
use std::path::Path;

use uuid::Uuid;

use crate::domain::habit_slots::{NativeSlot, occurrence_id, step_id};
use crate::domain::{Action, ActionState, NativeOccurrence};
use crate::reference::{LocalActions, ReferenceTarget};

use super::WorkspaceError;
use super::actions::{ActionList, LintDiagnostic, ParsedDocument, SourceRange};
use super::habit_files::{habit_from_root, template_step_recurrence};
use super::predecessors::PredecessorScope;
use schedule::SlotShift;

#[derive(Clone, Copy)]
pub struct StampSource<'a> {
    path: &'a Path,
    doc: &'a ParsedDocument,
}

impl<'a> StampSource<'a> {
    pub fn new(path: &'a (impl AsRef<Path> + ?Sized), doc: &'a ParsedDocument) -> Self {
        Self {
            path: path.as_ref(),
            doc,
        }
    }

    fn root(self, habit_id: Uuid) -> Result<&'a Action, WorkspaceError> {
        self.check_syntax()?;
        let mut matching = self
            .doc
            .actions
            .iter()
            .filter(|action| action.id == habit_id);
        let root = matching
            .next()
            .filter(|root| root.parent_id.is_none())
            .ok_or_else(|| {
                WorkspaceError::Actions(format!(
                    "{} has no Habit root {habit_id}",
                    self.path.display()
                ))
            })?;
        if matching.next().is_some() {
            return Err(WorkspaceError::Actions(self.message(
                self.range(habit_id),
                "duplicate-uuid",
                "Habit UUID is also used on another line",
            )));
        }
        Ok(root)
    }

    fn has_written_id(self, id: Uuid) -> bool {
        self.doc
            .source_map
            .get(&id)
            .is_some_and(|source| !source.is_id_generated)
    }

    fn range(self, id: Uuid) -> SourceRange {
        self.doc
            .source_map
            .get(&id)
            .map_or_default(|source| source.root)
    }

    fn message(self, range: SourceRange, code: &str, message: &str) -> String {
        format!(
            "{}:{}: {message} [{code}]",
            self.path.display(),
            range.start_row + 1
        )
    }

    fn diagnostic(self, diagnostic: &LintDiagnostic) -> String {
        self.message(diagnostic.range, &diagnostic.code, &diagnostic.message)
    }

    fn check_syntax(self) -> Result<(), WorkspaceError> {
        if self.doc.syntax_errors.is_empty() {
            return Ok(());
        }
        Err(WorkspaceError::Actions(
            self.doc
                .syntax_errors
                .iter()
                .map(|diagnostic| self.diagnostic(diagnostic))
                .collect::<Vec<_>>()
                .join("\n"),
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct StampedOccurrence(ActionList);

impl StampedOccurrence {
    /// Actions in source order, with the occurrence itself first.
    pub fn actions(&self) -> &[Action] {
        &self.0
    }

    /// Append the whole tree or nothing. A present root owns the slot, even
    /// when its name, state, or children no longer match the template.
    pub fn insert_into(self, actions: &mut ActionList) -> Result<bool, WorkspaceError> {
        let existing: HashSet<_> = actions.iter().map(|action| action.id).collect();
        if existing.contains(&self.0[0].id) {
            return Ok(false);
        }
        self.require_absent(|id| existing.contains(&id))?;
        actions.extend(self.0);
        Ok(true)
    }

    fn require_absent(&self, occupied: impl Fn(Uuid) -> bool) -> Result<(), WorkspaceError> {
        if let Some(collision) = self.0.iter().find(|action| occupied(action.id)) {
            return Err(WorkspaceError::Actions(format!(
                "Habit step {} already exists without its occurrence root",
                collision.id
            )));
        }
        Ok(())
    }
}

/// Stamp one original slot. Inline children, if present, shadow a named
/// template. The host supplies that document and the owning charter's scope.
/// Closed Habits return no occurrence. This function does not check whether a
/// slot has arrived or is excluded; the caller owns slot selection.
pub fn stamp_occurrence(
    source: StampSource<'_>,
    habit_id: Uuid,
    named_template: Option<StampSource<'_>>,
    slot: &NativeSlot,
    scope: &PredecessorScope,
    charter_id: Option<Uuid>,
) -> Result<Option<StampedOccurrence>, WorkspaceError> {
    prepare_stamp(source, habit_id, named_template, scope, charter_id)?
        .map(|stamp| stamp.at(slot))
        .transpose()
}

struct PreparedStamp<'a> {
    source: StampSource<'a>,
    step_source: StampSource<'a>,
    tree: Vec<Action>,
    binding: Option<String>,
    recurrence_rule: String,
    scope: &'a PredecessorScope,
    charter_id: Option<Uuid>,
}

fn prepare_stamp<'a>(
    source: StampSource<'a>,
    habit_id: Uuid,
    named_template: Option<StampSource<'a>>,
    scope: &'a PredecessorScope,
    charter_id: Option<Uuid>,
) -> Result<Option<PreparedStamp<'a>>, WorkspaceError> {
    let root = source.root(habit_id)?;
    if root.state.is_terminal() {
        return Ok(None);
    }
    let habit = habit_from_root(root, &source.doc.actions);
    let root_error = |code, message: &str| {
        WorkspaceError::Actions(source.message(source.range(habit_id), code, message))
    };
    habit
        .validate()
        .map_err(|error| root_error("E002", &error))?;
    let binding = habit.template.filter(|_| habit.children.is_empty());
    let (step_source, steps) = if let Some(name) = &binding {
        let template = named_template.ok_or_else(|| {
            root_error(
                "template-missing",
                &format!("template '{name}' was not supplied"),
            )
        })?;
        template.check_syntax()?;
        (template, template.doc.actions.clone())
    } else {
        (source, habit.children)
    };

    let mut tree = vec![habit.definition];
    tree.extend(steps.into_iter().map(|mut action| {
        action.parent_id = action.parent_id.or(Some(habit_id));
        action
    }));
    let location = |index| if index == 0 { source } else { step_source };
    validate_tree(&tree, scope, charter_id, location, binding.is_some())?;

    Ok(Some(PreparedStamp {
        source,
        step_source,
        tree,
        binding,
        recurrence_rule: habit.recurrence_rule,
        scope,
        charter_id,
    }))
}

impl PreparedStamp<'_> {
    fn at(&self, slot: &NativeSlot) -> Result<StampedOccurrence, WorkspaceError> {
        let tree = &self.tree;
        let habit_id = tree[0].id;
        let shift = SlotShift::new(tree[0].planned.expect("validated Habit").start, slot).map_err(
            |error| {
                WorkspaceError::Actions(self.source.message(
                    self.source.range(habit_id),
                    "slot-time",
                    error,
                ))
            },
        )?;
        let occurrence = occurrence_id(habit_id, &slot.key);
        let id_map: HashMap<_, _> = std::iter::once((habit_id, occurrence))
            .chain(
                tree[1..]
                    .iter()
                    .map(|step| (step.id, step_id(occurrence, step.id))),
            )
            .collect();
        let local = LocalActions {
            actions: tree,
            charter_id: self.charter_id,
        };
        let mut stamped = Vec::with_capacity(tree.len());
        for (index, original) in tree.iter().enumerate() {
            let mut action = original.clone();
            action.id = id_map[&action.id];
            action.parent_id = action.parent_id.map(|parent| id_map[&parent]);
            for predecessor in action.predecessors.iter_mut().flatten() {
                // Persist an internal wait as the stamped UUID; the template alias
                // or original UUID would no longer name the step after a reload.
                predecessor.resolved_uuid =
                    match self.scope.resolve_with_local(&predecessor.raw_ref, local) {
                        Ok(ReferenceTarget::Action(id)) => Some(match id_map.get(&id) {
                            Some(stamped_id) => {
                                predecessor.raw_ref = stamped_id.to_string();
                                *stamped_id
                            }
                            None => id,
                        }),
                        Ok(ReferenceTarget::Charter(id)) => Some(id),
                        _ => None,
                    };
            }
            shift.apply(&mut action).map_err(|error| {
                let location = if index == 0 {
                    self.source
                } else {
                    self.step_source
                };
                WorkspaceError::Actions(location.message(
                    location.range(original.id),
                    "slot-time",
                    error,
                ))
            })?;
            stamped.push(action);
        }

        let root = &mut stamped[0];
        root.state = ActionState::NotStarted;
        root.created_at = None;
        root.completed_at = None;
        // The native address is not a legacy Plan link or a transport slot key.
        root.native_occurrence = Some(NativeOccurrence {
            habit_id,
            occurrence_key: slot.key.as_str().to_owned(),
            plan_title: root.name.clone(),
            plan_uid: None,
            scheduled_at: root.planned.map(|planned| planned.start.to_string()),
            rrule: Some(self.recurrence_rule.clone()),
            template: self.binding.clone(),
            extra: Default::default(),
        });
        Ok(StampedOccurrence(stamped))
    }
}

fn validate_tree<'a>(
    tree: &[Action],
    scope: &PredecessorScope,
    charter_id: Option<Uuid>,
    location: impl Fn(usize) -> StampSource<'a>,
    named: bool,
) -> Result<(), WorkspaceError> {
    let step_source = location(1);
    let mut issues: Vec<_> = tree[1..]
        .iter()
        .filter_map(|step| template_step_recurrence(step_source.doc, step, named))
        .chain(scope.lint_local(tree, charter_id, |id| Some(step_source.range(id))))
        .map(|diagnostic| step_source.diagnostic(&diagnostic))
        .collect();
    let mut ids = HashSet::new();
    for (index, action) in tree.iter().enumerate() {
        let source = location(index);
        let range = source.range(action.id);
        if !source.has_written_id(action.id) {
            issues.push(source.message(
                range,
                "habit-id-missing",
                "persist Habit and template-step ids before stamping",
            ));
        }
        if !ids.insert(action.id) {
            issues.push(source.message(
                range,
                "duplicate-uuid",
                "stamp source uses this UUID more than once",
            ));
        }
    }
    if issues.is_empty() {
        Ok(())
    } else {
        Err(WorkspaceError::Actions(issues.join("\n")))
    }
}
