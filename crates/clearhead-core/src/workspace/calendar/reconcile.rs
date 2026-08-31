//! Field-wise three-way reconciliation between Actions and the configured plans vdir.
//!
//! The plans vdir is the complete integration boundary. No server, account,
//! href, ETag, or transport-specific metadata enters this module. Each owned
//! VTODO field is merged independently against its last-agreed value so a
//! conflict in one field never blocks safe changes in another.

use chrono::{DateTime, Local, Utc};
use icalendar::{Calendar, CalendarComponent, Component, EventLike, Todo, TodoStatus};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use uuid::Uuid;

use super::expand::{next_active_slot, render_occurrence};
use super::ics::{
    ICSPlan, OccurrenceOp, VTodoAction, action_to_vtodo, canonical_occurrence_key, parse_ics,
    render_master_rollforward, render_occurrence_deviation,
};
use super::sync_store::{
    CONTEXTS_FIELD, DESCRIPTION_FIELD, DUE_DATE_FIELD, MASTER_DTSTART_FIELD, PRIORITY_FIELD,
    PlansSyncStore, SCHEDULED_AT_FIELD, STATE_FIELD, TITLE_FIELD, UID_FIELD,
    serialize_plans_sync_store,
};
use crate::config::PlanComponentKind;
use crate::domain::{Action, ActionState, DomainModel};
use crate::workspace::actions::format::require_actions_formatting;
use crate::workspace::charter::MarkdownCharter;
use crate::workspace::resource::{
    Effect, EffectBatch, ExpectedResource, PreparedMutation, ResourceLocation, ResourcePrecondition,
};
use crate::workspace::sidecar::{ActionMeta, CharterMetadata, OccurrenceSnapshot, render_sidecar};
use crate::workspace::store::{Workspace, WorkspaceError};
use crate::workspace::templates::instantiate_template;
use crate::workspace::{OutputFormat, SourcedAction, format};

type Time = Option<DateTime<Local>>;

/// A conventional three-way merge result for one field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reconcile<T> {
    NoOp,
    TakeAction(T),
    TakeCalendar(T),
    Converged(T),
    Conflict { action: T, calendar: T },
}

/// Merge one field. A missing base means first sync. A missing calendar value
/// means the complete VTODO resource is absent, so ClearHead recreates its
/// projection rather than treating deletion as an instruction to erase Action
/// data. Nullable fields use `T = Option<_>`, preserving the distinction
/// between an absent resource and an explicitly absent DTSTART/DUE/DESCRIPTION.
pub fn reconcile<T: PartialEq + Clone>(
    action: &T,
    base: Option<&T>,
    calendar: Option<&T>,
) -> Reconcile<T> {
    let Some(calendar) = calendar else {
        return Reconcile::TakeAction(action.clone());
    };
    let Some(base) = base else {
        return if action == calendar {
            Reconcile::Converged(action.clone())
        } else {
            Reconcile::Conflict {
                action: action.clone(),
                calendar: calendar.clone(),
            }
        };
    };

    match (action != base, calendar != base) {
        (false, false) => Reconcile::NoOp,
        (true, false) => Reconcile::TakeAction(action.clone()),
        (false, true) => Reconcile::TakeCalendar(calendar.clone()),
        (true, true) if action == calendar => Reconcile::Converged(action.clone()),
        (true, true) => Reconcile::Conflict {
            action: action.clone(),
            calendar: calendar.clone(),
        },
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyncField {
    ScheduledAt,
    DueDate,
    State,
    Title,
    Description,
    Priority,
    Contexts,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyncEntry {
    pub action_id: Uuid,
    /// Original interoperable UID, which may not itself be a UUID.
    pub uid: String,
    pub name: String,
    pub scheduled_at: Reconcile<Time>,
    pub due_date: Reconcile<Time>,
    pub state: Reconcile<ActionState>,
    pub title: Reconcile<String>,
    pub description: Reconcile<Option<String>>,
    pub priority: Reconcile<Option<u32>>,
    pub contexts: Reconcile<Option<Vec<String>>>,
    /// Auxiliary RFC 5545 completion timestamp used when calendar STATUS wins.
    pub calendar_completed_at: Time,
}

impl SyncEntry {
    pub fn outcomes(&self) -> [(SyncField, OutcomeKind); 7] {
        [
            (SyncField::ScheduledAt, kind(&self.scheduled_at)),
            (SyncField::DueDate, kind(&self.due_date)),
            (SyncField::State, kind(&self.state)),
            (SyncField::Title, kind(&self.title)),
            (SyncField::Description, kind(&self.description)),
            (SyncField::Priority, kind(&self.priority)),
            (SyncField::Contexts, kind(&self.contexts)),
        ]
    }

    fn is_noop(&self) -> bool {
        self.outcomes()
            .iter()
            .all(|(_, outcome)| *outcome == OutcomeKind::NoOp)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutcomeKind {
    NoOp,
    TakeAction,
    TakeCalendar,
    Converged,
    Conflict,
}

fn resolve_one<T: Clone>(outcome: &mut Reconcile<T>, choice: SyncConflictResolution) {
    let Reconcile::Conflict { action, calendar } = outcome else {
        return;
    };
    *outcome = match choice {
        SyncConflictResolution::PreferAction => Reconcile::TakeAction(action.clone()),
        SyncConflictResolution::PreferCalendar => Reconcile::TakeCalendar(calendar.clone()),
    };
}

fn kind<T>(outcome: &Reconcile<T>) -> OutcomeKind {
    match outcome {
        Reconcile::NoOp => OutcomeKind::NoOp,
        Reconcile::TakeAction(_) => OutcomeKind::TakeAction,
        Reconcile::TakeCalendar(_) => OutcomeKind::TakeCalendar,
        Reconcile::Converged(_) => OutcomeKind::Converged,
        Reconcile::Conflict { .. } => OutcomeKind::Conflict,
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SyncTally {
    /// Actions with at least one field in this outcome category.
    pub take_action: usize,
    pub take_calendar: usize,
    pub converged: usize,
    pub conflict: usize,
}

/// A calendar-created VTODO that will become a new Action in the charter
/// selected by its containing vdir directory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyncImport {
    pub action: VTodoAction,
    pub plans_dir: PathBuf,
    pub charter_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VTodoResource {
    pub action: VTodoAction,
    pub path: PathBuf,
    pub plans_dir: PathBuf,
    pub charter_name: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SyncReport {
    pub entries: Vec<SyncEntry>,
    pub imports: Vec<SyncImport>,
    pub warnings: Vec<String>,
}

/// Optional policy for resolving every remaining field conflict in one sync run.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncConflictResolution {
    PreferAction,
    PreferCalendar,
}

impl SyncReport {
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty() && self.imports.is_empty() && self.warnings.is_empty()
    }

    pub fn resolve_conflicts(mut self, choice: Option<SyncConflictResolution>) -> Self {
        let Some(choice) = choice else {
            return self;
        };
        for entry in &mut self.entries {
            resolve_one(&mut entry.scheduled_at, choice);
            resolve_one(&mut entry.due_date, choice);
            resolve_one(&mut entry.state, choice);
            resolve_one(&mut entry.title, choice);
            resolve_one(&mut entry.description, choice);
            resolve_one(&mut entry.priority, choice);
            resolve_one(&mut entry.contexts, choice);
        }
        self
    }

    pub fn tally(&self) -> SyncTally {
        let mut tally = SyncTally {
            take_calendar: self.imports.len(),
            ..SyncTally::default()
        };
        for entry in &self.entries {
            let outcomes = entry.outcomes();
            tally.take_action += usize::from(
                outcomes
                    .iter()
                    .any(|(_, value)| *value == OutcomeKind::TakeAction),
            );
            tally.take_calendar += usize::from(
                outcomes
                    .iter()
                    .any(|(_, value)| *value == OutcomeKind::TakeCalendar),
            );
            let has_conflict = outcomes
                .iter()
                .any(|(_, value)| *value == OutcomeKind::Conflict);
            let has_transfer = outcomes.iter().any(|(_, value)| {
                matches!(value, OutcomeKind::TakeAction | OutcomeKind::TakeCalendar)
            });
            tally.converged += usize::from(
                !has_transfer
                    && !has_conflict
                    && outcomes
                        .iter()
                        .any(|(_, value)| *value == OutcomeKind::Converged),
            );
            tally.conflict += usize::from(has_conflict);
        }
        tally
    }
}

/// Plan a field-wise sync without touching disk.
pub fn plan_sync(
    model: &DomainModel,
    store: &PlansSyncStore,
    calendar: &HashMap<Uuid, VTodoResource>,
) -> Result<SyncReport, WorkspaceError> {
    let scheduled_bases: HashMap<Uuid, Time> = store.field_bases(SCHEDULED_AT_FIELD)?;
    let due_bases: HashMap<Uuid, Time> = store.field_bases(DUE_DATE_FIELD)?;
    let state_bases: HashMap<Uuid, ActionState> = store.field_bases(STATE_FIELD)?;
    let title_bases: HashMap<Uuid, String> = store.field_bases(TITLE_FIELD)?;
    let description_bases: HashMap<Uuid, Option<String>> = store.field_bases(DESCRIPTION_FIELD)?;
    let priority_bases: HashMap<Uuid, Option<u32>> = store.field_bases(PRIORITY_FIELD)?;
    let contexts_bases: HashMap<Uuid, Option<Vec<String>>> = store.field_bases(CONTEXTS_FIELD)?;
    let uid_bases: HashMap<Uuid, String> = store.field_bases(UID_FIELD)?;

    let mut report = SyncReport::default();
    let existing_ids: HashSet<_> = model
        .all_actions()
        .into_iter()
        .map(|action| action.id)
        .collect();

    // A materialized occurrence token is represented on the calendar by its
    // master's RRULE + deviations, never as a standalone VTODO — and its grafted
    // template steps stay local in `.actions` entirely. Both are ordinary
    // materialized lines a window-0 load keeps, so without this they would push
    // as duplicate standalone todos on the next sync (the double-vision the master
    // already covers). This is the materialized-token analog of the window-0 seal
    // that removes *projected* occurrences before they ever reach here.
    let occurrence_owned = occurrence_subtree_ids(model, store);

    for action in model.all_actions() {
        if occurrence_owned.contains(&action.id) {
            continue;
        }
        let calendar_action = calendar.get(&action.id).map(|resource| &resource.action);
        let action_contexts = normalized_contexts(action.contexts.clone());
        let entry = SyncEntry {
            action_id: action.id,
            uid: calendar_action
                .map(|value| value.uid.clone())
                .or_else(|| uid_bases.get(&action.id).cloned())
                .unwrap_or_else(|| action.id.to_string()),
            name: action.name.clone(),
            scheduled_at: reconcile(
                &action.scheduled_at,
                scheduled_bases.get(&action.id),
                calendar_action.map(|value| &value.scheduled_at),
            ),
            due_date: reconcile(
                &action.due_date,
                due_bases.get(&action.id),
                calendar_action.map(|value| &value.due_date),
            ),
            state: reconcile(
                &action.state,
                state_bases.get(&action.id),
                calendar_action.map(|value| &value.state),
            ),
            title: reconcile(
                &action.name,
                title_bases.get(&action.id),
                calendar_action.map(|value| &value.title),
            ),
            description: reconcile(
                &action.description,
                description_bases.get(&action.id),
                calendar_action.map(|value| &value.description),
            ),
            priority: reconcile(
                &action.priority,
                priority_bases.get(&action.id),
                calendar_action.map(|value| &value.priority),
            ),
            contexts: reconcile(
                &action_contexts,
                contexts_bases.get(&action.id),
                calendar_action.map(|value| &value.contexts),
            ),
            calendar_completed_at: calendar_action.and_then(|value| value.completed_at),
        };
        if !entry.is_noop() {
            report.entries.push(entry);
        }
    }

    for resource in calendar.values() {
        if !existing_ids.contains(&resource.action.id) {
            report.imports.push(SyncImport {
                action: resource.action.clone(),
                plans_dir: resource.plans_dir.clone(),
                charter_name: resource.charter_name.clone(),
            });
        }
    }
    report.imports.sort_by_key(|import| import.action.id);
    Ok(report)
}

/// Plan one-off Action/Plan reconciliation by the durable semantic relation.
///
/// This is the replacement seam for identity-coupled standalone-VTODO sync.
/// It deliberately covers only already-linked pairs in this first slice;
/// native creation, adoption, deletion, and delivery remain on the later
/// transaction cutover. Recurring Plans and occurrence-linked Actions are
/// excluded structurally.
pub fn plan_one_off_sync(
    model: &DomainModel,
    store: &PlansSyncStore,
    plans: &[ICSPlan],
) -> Result<SyncReport, WorkspaceError> {
    let scheduled_bases: HashMap<Uuid, Time> = store.field_bases(SCHEDULED_AT_FIELD)?;
    let due_bases: HashMap<Uuid, Time> = store.field_bases(DUE_DATE_FIELD)?;
    let state_bases: HashMap<Uuid, ActionState> = store.field_bases(STATE_FIELD)?;
    let title_bases: HashMap<Uuid, String> = store.field_bases(TITLE_FIELD)?;
    let description_bases: HashMap<Uuid, Option<String>> = store.field_bases(DESCRIPTION_FIELD)?;
    let priority_bases: HashMap<Uuid, Option<u32>> = store.field_bases(PRIORITY_FIELD)?;
    let contexts_bases: HashMap<Uuid, Option<Vec<String>>> = store.field_bases(CONTEXTS_FIELD)?;

    let mut one_off = HashMap::new();
    for plan in plans.iter().filter(|plan| plan.plan.recurrence.is_none()) {
        if one_off.insert(plan.plan.id, plan).is_some() {
            return Err(WorkspaceError::Parse(format!(
                "one-off Plan {} appears more than once",
                plan.plan.id
            )));
        }
    }

    let mut report = SyncReport::default();
    for action in model.all_actions() {
        if action.external_occurrence_key.is_some() {
            continue;
        }
        let Some(plan_id) = action.plan_id else {
            continue;
        };
        let Some(resource) = one_off.get(&plan_id) else {
            continue;
        };
        let uid = resource
            .plan
            .external_id
            .clone()
            .unwrap_or_else(|| resource.plan.id.to_string());
        let action_contexts = normalized_contexts(action.contexts.clone());
        let (state, title, description, priority, contexts, completed_at) =
            match (resource.component_kind, resource.task_fields.as_ref()) {
                (PlanComponentKind::VTodo, Some(task)) => (
                    reconcile(
                        &action.state,
                        state_bases.get(&action.id),
                        Some(&task.state),
                    ),
                    reconcile(
                        &action.name,
                        title_bases.get(&action.id),
                        Some(&resource.plan.name),
                    ),
                    reconcile(
                        &action.description,
                        description_bases.get(&action.id),
                        Some(&resource.plan.description),
                    ),
                    reconcile(
                        &action.priority,
                        priority_bases.get(&action.id),
                        Some(&task.priority),
                    ),
                    reconcile(
                        &action_contexts,
                        contexts_bases.get(&action.id),
                        Some(&task.contexts),
                    ),
                    task.completed_at,
                ),
                _ => (
                    Reconcile::NoOp,
                    Reconcile::NoOp,
                    Reconcile::NoOp,
                    Reconcile::NoOp,
                    Reconcile::NoOp,
                    None,
                ),
            };
        let entry = SyncEntry {
            action_id: action.id,
            uid,
            name: action.name.clone(),
            scheduled_at: reconcile(
                &action.scheduled_at,
                scheduled_bases.get(&action.id),
                Some(&resource.plan.dtstart),
            ),
            due_date: reconcile(
                &action.due_date,
                due_bases.get(&action.id),
                Some(&resource.schedule_end),
            ),
            state,
            title,
            description,
            priority,
            contexts,
            calendar_completed_at: completed_at,
        };
        if !entry.is_noop() {
            report.entries.push(entry);
        }
    }
    Ok(report)
}

/// The ids of every materialized occurrence token **and its grafted subtree**.
///
/// A token root is any action carrying an occurrence link in `store`
/// ([`stamp_occurrence_link`](PlansSyncStore::stamp_occurrence_link)); its grafted
/// template steps are its descendants by `parent_id`. Together they are the actions
/// the plans vdir represents through the master (RRULE occurrence + completion
/// deviations) or keeps purely local — so [`plan_sync`] excludes them from
/// standalone reconciliation. Returns an empty set when no tokens are stamped, the
/// non-recurring common case.
fn occurrence_subtree_ids(model: &DomainModel, store: &PlansSyncStore) -> HashSet<Uuid> {
    let roots = store.occurrence_links();
    if roots.is_empty() {
        return HashSet::new();
    }
    let actions = model.all_actions();
    let parent_of: HashMap<Uuid, Option<Uuid>> =
        actions.iter().map(|a| (a.id, a.parent_id)).collect();

    let mut owned = HashSet::new();
    for action in &actions {
        // Walk the parent chain; if it reaches a token root the action is part of
        // that occurrence's subtree. The chain is a finite tree, so this terminates.
        let mut cursor = Some(action.id);
        while let Some(id) = cursor {
            if roots.contains_key(&id) {
                owned.insert(action.id);
                break;
            }
            cursor = parent_of.get(&id).copied().flatten();
        }
    }
    owned
}

fn normalized_contexts(mut contexts: Option<Vec<String>>) -> Option<Vec<String>> {
    if let Some(values) = &mut contexts {
        values.retain(|value| !value.is_empty());
        values.sort();
        values.dedup();
        if values.is_empty() {
            return None;
        }
    }
    contexts
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct AppliedSync {
    pub take_action: usize,
    pub take_calendar: usize,
    pub converged: usize,
    pub conflict: usize,
}

#[derive(Debug, Clone)]
struct PendingActionMirror {
    action_id: Uuid,
    uid: String,
    action: Action,
    fields: Vec<SyncField>,
}

#[derive(Debug)]
struct AppliedReport {
    dirty_actions: HashSet<PathBuf>,
    mirrors: Vec<PendingActionMirror>,
}

fn apply_report(
    workspace: &mut Workspace,
    store: &mut PlansSyncStore,
    report: &SyncReport,
) -> Result<AppliedReport, WorkspaceError> {
    let mut dirty_actions = HashSet::new();
    let mut mirrors = Vec::new();
    let mut applied = AppliedSync::default();

    for import in &report.imports {
        let charter_idx = locate_import_charter(&workspace.charters, import)?;
        let actions_relative = import_actions_file(&mut workspace.charters[charter_idx], import);
        let action = action_from_vtodo(&import.action);
        workspace.charters[charter_idx].actions.push(SourcedAction {
            action,
            source_metadata: None,
        });
        dirty_actions.insert(actions_relative);
        stamp_projection(store, &import.action)?;
        applied.take_calendar += 1;
    }

    for entry in &report.entries {
        let Some((charter_idx, action_idx)) = locate_action(&workspace.charters, entry.action_id)
        else {
            return Err(WorkspaceError::Parse(format!(
                "sync action not found in workspace: {}",
                entry.action_id
            )));
        };
        store.stamp(entry.action_id, UID_FIELD, &entry.uid)?;
        let actions_relative = workspace.charters[charter_idx]
            .actions_file
            .clone()
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "sync charter for action {} has no actions_file",
                    entry.action_id
                ))
            })?;

        let (push_fields, action_for_calendar) = {
            let mut push_fields = Vec::new();
            let action = &mut workspace.charters[charter_idx].actions[action_idx].action;
            apply_time_outcome(
                &entry.scheduled_at,
                &mut action.scheduled_at,
                entry.action_id,
                SCHEDULED_AT_FIELD,
                SyncField::ScheduledAt,
                &mut push_fields,
                store,
                &mut applied,
            )?;
            apply_time_outcome(
                &entry.due_date,
                &mut action.due_date,
                entry.action_id,
                DUE_DATE_FIELD,
                SyncField::DueDate,
                &mut push_fields,
                store,
                &mut applied,
            )?;
            apply_state_outcome(
                &entry.state,
                entry.calendar_completed_at,
                action,
                entry.action_id,
                &mut push_fields,
                store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.title,
                &mut action.name,
                entry.action_id,
                TITLE_FIELD,
                SyncField::Title,
                &mut push_fields,
                store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.description,
                &mut action.description,
                entry.action_id,
                DESCRIPTION_FIELD,
                SyncField::Description,
                &mut push_fields,
                store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.priority,
                &mut action.priority,
                entry.action_id,
                PRIORITY_FIELD,
                SyncField::Priority,
                &mut push_fields,
                store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.contexts,
                &mut action.contexts,
                entry.action_id,
                CONTEXTS_FIELD,
                SyncField::Contexts,
                &mut push_fields,
                store,
                &mut applied,
            )?;
            (push_fields, action.clone())
        };

        if entry
            .outcomes()
            .iter()
            .any(|(_, outcome)| *outcome == OutcomeKind::TakeCalendar)
        {
            dirty_actions.insert(actions_relative);
        }
        if !push_fields.is_empty() {
            mirrors.push(PendingActionMirror {
                action_id: entry.action_id,
                uid: entry.uid.clone(),
                action: action_for_calendar,
                fields: push_fields,
            });
        }
    }

    Ok(AppliedReport {
        dirty_actions,
        mirrors,
    })
}

/// Immutable Action-file evidence supplied by a host for sync preparation.
#[derive(Clone, Debug)]
pub struct SyncActionResourceState {
    pub actions_file: PathBuf,
    pub location: ResourceLocation,
    pub expected: ExpectedResource,
}

/// Immutable calendar-mirror evidence supplied by a host for sync preparation.
#[derive(Clone, Debug)]
pub struct SyncMirrorResourceState {
    pub action_id: Uuid,
    pub location: ResourceLocation,
    pub expected: ExpectedResource,
    pub source: Option<String>,
}

/// One pure calendar-resource rewrite prepared before field reconciliation.
#[derive(Clone, Debug)]
pub struct SyncCalendarWrite {
    pub location: ResourceLocation,
    pub content: String,
}

/// Parsed template steps and host-supplied identities for one possible Plan token.
#[derive(Clone, Debug)]
pub struct SyncPlanTemplate {
    pub plan_id: Uuid,
    pub steps: Vec<Action>,
    pub generated_ids: Vec<Uuid>,
}

/// Immutable host evidence and explicit nondeterministic inputs for sync preparation.
pub struct CalendarSyncPreparationInput {
    pub workspace: Workspace,
    pub store: PlansSyncStore,
    pub action_resources: Vec<SyncActionResourceState>,
    pub mirror_resources: Vec<SyncMirrorResourceState>,
    pub calendar_writes: Vec<SyncCalendarWrite>,
    pub templates: Vec<SyncPlanTemplate>,
    pub observed_resources: Vec<ResourcePrecondition>,
    pub now: DateTime<Local>,
    pub store_location: ResourceLocation,
    pub store_expected: ExpectedResource,
}

/// Optional completed-occurrence metadata evidence for lineage crystallization.
pub struct MaterializedOccurrenceArchiveState {
    pub action: Action,
    pub metadata: CharterMetadata,
    pub sidecar_location: ResourceLocation,
    pub sidecar_expected: ExpectedResource,
}

/// Immutable evidence and explicit inputs for resolving one materialized token.
pub struct MaterializedOccurrencePreparationInput {
    pub workspace: Workspace,
    pub store: PlansSyncStore,
    pub occurrence_id: Uuid,
    pub operation: OccurrenceOp,
    pub now: DateTime<Local>,
    pub plan_resources: Vec<PlanResourceState>,
    pub action_resources: Vec<SyncActionResourceState>,
    pub templates: Vec<SyncPlanTemplate>,
    pub archive: Option<MaterializedOccurrenceArchiveState>,
    pub observed_resources: Vec<ResourcePrecondition>,
    pub store_location: ResourceLocation,
    pub store_expected: ExpectedResource,
}

/// Speculative workspace and merge-base state produced by pure sync preparation.
pub struct CalendarSyncState {
    pub workspace: Workspace,
    pub store: PlansSyncStore,
}

/// Apply a resolved sync report to immutable host evidence and prepare one effect batch.
///
/// The host owns locking, recovery, inventory, reads, stale validation, and delivery.
/// Core owns field reconciliation, Action/calendar rendering, and merge-base updates.
pub fn prepare_sync(
    input: CalendarSyncPreparationInput,
    report: &SyncReport,
) -> Result<PreparedMutation<CalendarSyncState, AppliedSync>, WorkspaceError> {
    require_actions_formatting().map_err(WorkspaceError::Actions)?;
    let CalendarSyncPreparationInput {
        mut workspace,
        mut store,
        action_resources,
        mirror_resources,
        calendar_writes,
        templates,
        observed_resources,
        now,
        store_location,
        store_expected,
    } = input;
    let mut changes = apply_report(&mut workspace, &mut store, report)?;
    ensure_active_occurrences_prepared(
        &mut workspace.charters,
        &mut store,
        &mut changes.dirty_actions,
        &templates,
        now,
    )?;
    let mut effects = Vec::new();
    let mut expected_by_location = BTreeMap::new();
    for (location, expected) in action_resources
        .iter()
        .map(|resource| (&resource.location, &resource.expected))
        .chain(
            mirror_resources
                .iter()
                .map(|resource| (&resource.location, &resource.expected)),
        )
        .chain(
            observed_resources
                .iter()
                .map(|resource| (&resource.path, &resource.expected)),
        )
        .chain(std::iter::once((&store_location, &store_expected)))
    {
        if let Some(previous) = expected_by_location.insert(location.clone(), expected.clone())
            && previous != *expected
        {
            return Err(WorkspaceError::Parse(format!(
                "sync resource has inconsistent revision evidence: {location}"
            )));
        }
    }
    let preconditions = expected_by_location
        .into_iter()
        .map(|(path, expected)| ResourcePrecondition { path, expected })
        .collect::<Vec<_>>();

    let mut dirty_actions = changes.dirty_actions.into_iter().collect::<Vec<_>>();
    dirty_actions.sort();
    for actions_file in dirty_actions {
        let resource = action_resources
            .iter()
            .find(|resource| resource.actions_file == actions_file)
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "sync Action resource evidence is missing: {}",
                    actions_file.display()
                ))
            })?;
        let charter = workspace
            .charters
            .iter()
            .find(|charter| charter.actions_file.as_deref() == Some(actions_file.as_path()))
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "dirty Action file has no owning charter: {}",
                    actions_file.display()
                ))
            })?;
        effects.push(Effect::Write {
            path: resource.location.clone(),
            bytes: render_actions(&charter.actions)?.into_bytes(),
        });
    }

    let mut rendered_mirrors = calendar_writes
        .into_iter()
        .map(|write| (write.location, write.content))
        .collect::<BTreeMap<ResourceLocation, String>>();
    for mirror in changes.mirrors {
        let resource = mirror_resources
            .iter()
            .find(|resource| resource.action_id == mirror.action_id)
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "sync calendar resource evidence is missing for Action {}",
                    mirror.action_id
                ))
            })?;
        let source = rendered_mirrors
            .get(&resource.location)
            .map(String::as_str)
            .or(resource.source.as_deref());
        let rendered = render_action_mirror(source, &mirror.uid, &mirror.action, &mirror.fields)?;
        rendered_mirrors.insert(resource.location.clone(), rendered);
    }
    effects.extend(
        rendered_mirrors
            .into_iter()
            .map(|(path, content)| Effect::Write {
                path,
                bytes: content.into_bytes(),
            }),
    );

    effects.push(Effect::Write {
        path: store_location,
        bytes: serialize_plans_sync_store(&store)?.into_bytes(),
    });
    let batch = EffectBatch::new(effects, preconditions)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    let tally = report.tally();
    Ok(PreparedMutation::with_outcome(
        CalendarSyncState { workspace, store },
        batch,
        AppliedSync {
            take_action: tally.take_action,
            take_calendar: tally.take_calendar,
            converged: tally.converged,
            conflict: tally.conflict,
        },
    ))
}

/// Resolve a materialized recurring token without touching a host.
pub fn prepare_materialized_occurrence_resolution(
    input: MaterializedOccurrencePreparationInput,
) -> Result<PreparedMutation<CalendarSyncState, bool>, WorkspaceError> {
    require_actions_formatting().map_err(WorkspaceError::Actions)?;
    let MaterializedOccurrencePreparationInput {
        mut workspace,
        mut store,
        occurrence_id,
        operation,
        now,
        plan_resources,
        action_resources,
        templates,
        mut archive,
        observed_resources,
        store_location,
        store_expected,
    } = input;
    let Some((plan_id, slot_key)) = store.occurrence_link(occurrence_id) else {
        let batch = EffectBatch::new(Vec::new(), observed_resources)
            .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
        return Ok(PreparedMutation::with_outcome(
            CalendarSyncState { workspace, store },
            batch,
            false,
        ));
    };

    let mut matched = None;
    for resource in &plan_resources {
        for plan in parse_ics(&resource.source, Path::new(resource.location.path.as_str()))? {
            if plan.plan.id != plan_id {
                continue;
            }
            if matched.is_some() {
                return Err(WorkspaceError::Parse(format!(
                    "recurring plan {plan_id} appears more than once in the configured plans vdir"
                )));
            }
            matched = Some((resource, plan));
        }
    }
    let Some((plan_resource, plan)) = matched else {
        return Err(WorkspaceError::Parse(format!(
            "recurring plan {plan_id} not found in the configured plans vdir"
        )));
    };
    let uid = plan.plan.external_id.as_deref().ok_or_else(|| {
        WorkspaceError::Parse(format!("recurring plan {plan_id} has no UID to key on"))
    })?;
    let rendered_plan =
        render_occurrence_deviation(&plan_resource.source, uid, &slot_key, &operation)?;

    if let Some(archive) = &mut archive {
        let entry = archive
            .metadata
            .actions
            .entry(occurrence_id.to_string())
            .or_insert_with(ActionMeta::default);
        if entry.occurrence.is_none() {
            entry.occurrence = Some(OccurrenceSnapshot {
                plan_id,
                plan_uid: plan.plan.external_id.clone(),
                occurrence_key: slot_key.clone(),
                plan_title: plan.plan.name.clone(),
                scheduled_at: archive.action.scheduled_at,
                rrule: plan.plan.recurrence.as_ref().map(|recurrence| {
                    let text = recurrence.to_string();
                    text.strip_prefix("R:").unwrap_or(&text).to_string()
                }),
                template: plan.plan.template_name.clone(),
            });
        }
    }
    store.clear_occurrence_link(occurrence_id);

    let mut dirty_actions = HashSet::new();
    let floor = parse_occurrence_key(&slot_key);
    if let Some(charter_idx) = workspace
        .charters
        .iter()
        .position(|charter| charter.plans.iter().any(|value| value.plan.id == plan_id))
        && let Some(workspace_plan) = workspace.charters[charter_idx]
            .plans
            .iter()
            .find(|value| value.plan.id == plan_id)
            .cloned()
        && let Some(actions_file) = stage_prepared_plan_token(
            &mut workspace.charters[charter_idx],
            &mut store,
            &workspace_plan,
            floor,
            &templates,
            now,
        )?
    {
        dirty_actions.insert(actions_file);
    }

    let mut effects = vec![Effect::Write {
        path: plan_resource.location.clone(),
        bytes: rendered_plan.into_bytes(),
    }];
    for actions_file in dirty_actions {
        let resource = action_resources
            .iter()
            .find(|resource| resource.actions_file == actions_file)
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "occurrence Action resource evidence is missing: {}",
                    actions_file.display()
                ))
            })?;
        let charter = workspace
            .charters
            .iter()
            .find(|charter| charter.actions_file.as_deref() == Some(actions_file.as_path()))
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "occurrence Action file has no owning charter: {}",
                    actions_file.display()
                ))
            })?;
        effects.push(Effect::Write {
            path: resource.location.clone(),
            bytes: render_actions(&charter.actions)?.into_bytes(),
        });
    }
    if let Some(archive) = &archive {
        effects.push(Effect::Write {
            path: archive.sidecar_location.clone(),
            bytes: render_sidecar(&archive.metadata)?.into_bytes(),
        });
    }
    effects.push(Effect::Write {
        path: store_location.clone(),
        bytes: serialize_plans_sync_store(&store)?.into_bytes(),
    });

    let mut expected_by_location = BTreeMap::new();
    for precondition in observed_resources
        .into_iter()
        .chain(
            action_resources
                .iter()
                .map(|resource| ResourcePrecondition {
                    path: resource.location.clone(),
                    expected: resource.expected.clone(),
                }),
        )
        .chain(archive.iter().map(|archive| ResourcePrecondition {
            path: archive.sidecar_location.clone(),
            expected: archive.sidecar_expected.clone(),
        }))
        .chain(std::iter::once(ResourcePrecondition {
            path: store_location,
            expected: store_expected,
        }))
    {
        if let Some(previous) =
            expected_by_location.insert(precondition.path.clone(), precondition.expected.clone())
            && previous != precondition.expected
        {
            return Err(WorkspaceError::Parse(format!(
                "occurrence resource has inconsistent revision evidence: {}",
                precondition.path
            )));
        }
    }
    let batch = EffectBatch::new(
        effects,
        expected_by_location
            .into_iter()
            .map(|(path, expected)| ResourcePrecondition { path, expected })
            .collect(),
    )
    .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    Ok(PreparedMutation::with_outcome(
        CalendarSyncState { workspace, store },
        batch,
        true,
    ))
}

/// A resolved occurrence no longer holds the token — the next may be stamped.
fn is_resolved(state: ActionState) -> bool {
    matches!(state, ActionState::Completed | ActionState::Cancelled)
}

/// Ensure every recurring plan carries exactly one live (unresolved) materialized
/// occurrence — the single token.
///
/// For a plan with no live token, render its active slot ([`next_active_slot`],
/// the next upcoming occurrence) and stage it: a real `.actions` line under the
/// plan's own charter, plus its `(plan_id, slot)` link in `store` so the completion
/// hook can later target the master deviation. Mutated action files are recorded in
/// `dirty_actions`; the caller stages them and `store` into one atomic batch.
///
/// Idempotent by construction: a plan whose token already exists unresolved is
/// skipped, and a deterministic occurrence id already present (in any state) is
/// never duplicated. A token resolved *outside* the completion hook (e.g. a raw
/// `[x]` edit) reads as not-live, so sync re-stamps the next slot — the safety net
/// under the eager completion path. Returns the number of tokens stamped.
fn ensure_active_occurrences_prepared(
    charters: &mut [MarkdownCharter],
    store: &mut PlansSyncStore,
    dirty_actions: &mut HashSet<PathBuf>,
    templates: &[SyncPlanTemplate],
    now: DateTime<Local>,
) -> Result<usize, WorkspaceError> {
    let links = store.occurrence_links();
    let mut stamped = 0;

    for charter_idx in 0..charters.len() {
        let plans = charters[charter_idx].plans.clone();
        for plan in &plans {
            let has_live_token = links.iter().any(|(occ_id, (plan_id, _slot))| {
                *plan_id == plan.plan.id
                    && charters.iter().any(|charter| {
                        charter.actions.iter().any(|action| {
                            action.action.id == *occ_id && !is_resolved(action.action.state)
                        })
                    })
            });
            if has_live_token {
                continue;
            }
            if let Some(actions_file) = stage_prepared_plan_token(
                &mut charters[charter_idx],
                store,
                plan,
                None,
                templates,
                now,
            )? {
                dirty_actions.insert(actions_file);
                stamped += 1;
            }
        }
    }
    Ok(stamped)
}

fn stage_prepared_plan_token(
    charter: &mut MarkdownCharter,
    store: &mut PlansSyncStore,
    plan: &super::ics::ICSPlan,
    floor: Option<DateTime<Local>>,
    templates: &[SyncPlanTemplate],
    now: DateTime<Local>,
) -> Result<Option<PathBuf>, WorkspaceError> {
    let Some(uid) = plan.plan.external_id.as_deref() else {
        return Ok(None);
    };
    let Some(slot) = next_active_slot(plan, floor, now) else {
        return Ok(None);
    };
    let occurrence = render_occurrence(plan, uid, slot);
    let occurrence_id = occurrence.id;
    if charter
        .actions
        .iter()
        .any(|action| action.action.id == occurrence_id)
    {
        return Ok(None);
    }
    let slot_key = occurrence.external_occurrence_key.clone().ok_or_else(|| {
        WorkspaceError::Parse(format!(
            "rendered Plan {} occurrence has no occurrence key",
            plan.plan.id
        ))
    })?;
    let actions_file = charter.actions_file.clone().ok_or_else(|| {
        WorkspaceError::Parse(format!(
            "charter {} carries plans but has no actions_file to stamp into",
            charter.id
        ))
    })?;
    charter.actions.push(SourcedAction {
        action: occurrence,
        source_metadata: None,
    });

    if plan.plan.template_name.is_some()
        && let Some(template) = templates
            .iter()
            .find(|template| template.plan_id == plan.plan.id)
    {
        if template.steps.len() != template.generated_ids.len() {
            return Err(WorkspaceError::Parse(format!(
                "Plan {} template identity count does not match its steps",
                plan.plan.id
            )));
        }
        let generated_ids = template
            .steps
            .iter()
            .map(|step| step.id)
            .zip(template.generated_ids.iter().copied())
            .collect::<HashMap<_, _>>();
        for step in instantiate_template(
            &template.steps,
            |source_id| generated_ids[&source_id],
            Some(occurrence_id),
        ) {
            charter.actions.push(SourcedAction {
                action: step,
                source_metadata: None,
            });
        }
    }
    store.stamp_occurrence_link(occurrence_id, plan.plan.id, &slot_key)?;
    Ok(Some(actions_file))
}

fn parse_occurrence_key(key: &str) -> Option<DateTime<Local>> {
    chrono::NaiveDateTime::parse_from_str(key, "%Y%m%dT%H%M%SZ")
        .ok()
        .map(|naive| naive.and_utc().with_timezone(&Local))
}

fn locate_import_charter(
    charters: &[MarkdownCharter],
    import: &SyncImport,
) -> Result<usize, WorkspaceError> {
    charters
        .iter()
        .position(|charter| charter.plans_dir == import.plans_dir)
        .ok_or_else(|| {
            WorkspaceError::Parse(format!(
                "calendar collection '{}' has no owning charter; run `clearhead doctor`",
                import.plans_dir.display()
            ))
        })
}

/// Select the Action resource that owns a calendar import without touching the host.
pub fn sync_import_actions_file(charter: &MarkdownCharter, import: &SyncImport) -> PathBuf {
    charter.actions_file.clone().unwrap_or_else(|| {
        charter
            .md_file
            .as_ref()
            .map(|path| path.with_extension("actions"))
            .unwrap_or_else(|| {
                if import.plans_dir == Path::new("next") {
                    PathBuf::from("next.actions")
                } else {
                    PathBuf::from(format!("{}.actions", import.charter_name))
                }
            })
    })
}

fn import_actions_file(charter: &mut MarkdownCharter, import: &SyncImport) -> PathBuf {
    let path = sync_import_actions_file(charter, import);
    charter.actions_file.get_or_insert_with(|| path.clone());
    path
}

fn action_from_vtodo(source: &VTodoAction) -> Action {
    Action {
        id: source.id,
        state: source.state,
        name: source.title.clone(),
        description: source.description.clone(),
        priority: source.priority,
        contexts: normalized_contexts(source.contexts.clone()),
        scheduled_at: source.scheduled_at,
        due_date: source.due_date,
        completed_at: (source.state == ActionState::Completed)
            .then_some(source.completed_at)
            .flatten(),
        ..Action::default()
    }
}

fn stamp_projection(
    store: &mut PlansSyncStore,
    source: &VTodoAction,
) -> Result<(), WorkspaceError> {
    store.stamp(source.id, UID_FIELD, &source.uid)?;
    store.stamp(source.id, SCHEDULED_AT_FIELD, &source.scheduled_at)?;
    store.stamp(source.id, DUE_DATE_FIELD, &source.due_date)?;
    store.stamp(source.id, STATE_FIELD, &source.state)?;
    store.stamp(source.id, TITLE_FIELD, &source.title)?;
    store.stamp(source.id, DESCRIPTION_FIELD, &source.description)?;
    store.stamp(source.id, PRIORITY_FIELD, &source.priority)?;
    store.stamp(
        source.id,
        CONTEXTS_FIELD,
        &normalized_contexts(source.contexts.clone()),
    )?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn apply_value_outcome<T: Clone + serde::Serialize>(
    outcome: &Reconcile<T>,
    target: &mut T,
    id: Uuid,
    field_name: &str,
    field: SyncField,
    pushes: &mut Vec<SyncField>,
    store: &mut PlansSyncStore,
    applied: &mut AppliedSync,
) -> Result<(), WorkspaceError> {
    match outcome {
        Reconcile::NoOp => {}
        Reconcile::TakeAction(value) => {
            pushes.push(field);
            store.stamp(id, field_name, value)?;
            applied.take_action += 1;
        }
        Reconcile::TakeCalendar(value) => {
            *target = value.clone();
            store.stamp(id, field_name, value)?;
            applied.take_calendar += 1;
        }
        Reconcile::Converged(value) => {
            store.stamp(id, field_name, value)?;
            applied.converged += 1;
        }
        Reconcile::Conflict { .. } => applied.conflict += 1,
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn apply_time_outcome(
    outcome: &Reconcile<Time>,
    target: &mut Time,
    id: Uuid,
    field_name: &str,
    field: SyncField,
    pushes: &mut Vec<SyncField>,
    store: &mut PlansSyncStore,
    applied: &mut AppliedSync,
) -> Result<(), WorkspaceError> {
    apply_value_outcome(
        outcome, target, id, field_name, field, pushes, store, applied,
    )
}

fn apply_state_outcome(
    outcome: &Reconcile<ActionState>,
    calendar_completed_at: Time,
    action: &mut Action,
    id: Uuid,
    pushes: &mut Vec<SyncField>,
    store: &mut PlansSyncStore,
    applied: &mut AppliedSync,
) -> Result<(), WorkspaceError> {
    apply_value_outcome(
        outcome,
        &mut action.state,
        id,
        STATE_FIELD,
        SyncField::State,
        pushes,
        store,
        applied,
    )?;
    if matches!(outcome, Reconcile::TakeCalendar(_)) {
        // COMPLETED is auxiliary VTODO lifecycle data, not an independent
        // ClearHead sync field. Preserve the client's timestamp when present;
        // never invent one merely because sync happened now.
        action.completed_at = if action.state == ActionState::Completed {
            calendar_completed_at
        } else {
            None
        };
    }
    Ok(())
}

/// Render one standalone Action mirror from host-supplied source bytes.
pub fn render_action_mirror(
    content: Option<&str>,
    uid: &str,
    action: &Action,
    fields: &[SyncField],
) -> Result<String, WorkspaceError> {
    let Some(content) = content else {
        let mut calendar = Calendar::new().name("ClearHead Actions").done();
        let mut todo = action_to_vtodo(action);
        todo.uid(uid);
        calendar.push(todo);
        return Ok(calendar.to_string());
    };

    let mut calendar: Calendar = content
        .parse()
        .map_err(|error: String| WorkspaceError::Parse(error))?;
    let mut found = false;
    for component in &mut calendar.components {
        let CalendarComponent::Todo(todo) = component else {
            continue;
        };
        if todo.get_uid() == Some(uid) && todo.property_value("RRULE").is_none() {
            patch_todo(todo, action, fields);
            found = true;
            break;
        }
    }
    if !found {
        return Err(WorkspaceError::Parse(format!(
            "action mirror does not contain standalone VTODO UID {uid}"
        )));
    }
    Ok(calendar.to_string())
}

fn patch_todo(todo: &mut Todo, action: &Action, fields: &[SyncField]) {
    let fields: HashSet<_> = fields.iter().copied().collect();
    if fields.contains(&SyncField::ScheduledAt) {
        todo.remove_starts();
        if let Some(value) = action.scheduled_at {
            todo.starts(value.with_timezone(&Utc));
        }
    }
    if fields.contains(&SyncField::DueDate) {
        todo.remove_due();
        if let Some(value) = action.due_date {
            todo.due(value.with_timezone(&Utc));
        }
    }
    if fields.contains(&SyncField::State) {
        todo.remove_status().remove_property("X-CLEARHEAD-STATUS");
        let status = match action.state {
            ActionState::NotStarted | ActionState::BlockedOrAwaiting => TodoStatus::NeedsAction,
            ActionState::InProgress => TodoStatus::InProcess,
            ActionState::Completed => TodoStatus::Completed,
            ActionState::Cancelled => TodoStatus::Cancelled,
        };
        todo.status(status);
        if action.state == ActionState::BlockedOrAwaiting {
            todo.add_property("X-CLEARHEAD-STATUS", "blocked");
        }
        todo.remove_completed();
        if action.state == ActionState::Completed
            && let Some(value) = action.completed_at
        {
            todo.completed(value.with_timezone(&Utc));
        }
    }
    if fields.contains(&SyncField::Title) {
        todo.summary(&action.name);
    }
    if fields.contains(&SyncField::Description) {
        todo.remove_description();
        if let Some(value) = &action.description {
            todo.description(value);
        }
    }
    if fields.contains(&SyncField::Priority) {
        todo.remove_priority();
        if let Some(value) = action.priority {
            todo.priority(value);
        }
    }
    if fields.contains(&SyncField::Contexts) {
        todo.remove_property("CATEGORIES")
            .remove_multi_property("CATEGORIES");
        if let Some(contexts) = &action.contexts {
            for context in contexts {
                todo.add_multi_property("CATEGORIES", context);
            }
        }
    }
}

fn locate_action(charters: &[MarkdownCharter], id: Uuid) -> Option<(usize, usize)> {
    charters
        .iter()
        .enumerate()
        .find_map(|(charter_idx, charter)| {
            charter
                .actions
                .iter()
                .position(|action| action.action.id == id)
                .map(|action_idx| (charter_idx, action_idx))
        })
}

fn render_actions(actions: &[SourcedAction]) -> Result<String, WorkspaceError> {
    let actions = actions
        .iter()
        .map(|action| action.action.clone())
        .collect::<Vec<_>>();
    format(&actions, OutputFormat::Actions, None, None).map_err(WorkspaceError::Actions)
}

/// Host-supplied recurring-plan resource used by pure calendar preparation.
#[derive(Clone, Debug)]
pub struct PlanResourceState {
    pub location: ResourceLocation,
    pub source: String,
    pub expected: ExpectedResource,
}

/// Prepare recurring-master roll-forward normalization without touching a host.
pub struct MasterRollforwardChanges {
    pub store: PlansSyncStore,
    pub calendar_writes: Vec<SyncCalendarWrite>,
    pub recorded: usize,
    pub store_changed: bool,
}

/// Interpret recurring-master anchor advances without assigning persistence.
pub fn prepare_master_rollforward_changes(
    mut store: PlansSyncStore,
    resources: &[PlanResourceState],
) -> Result<MasterRollforwardChanges, WorkspaceError> {
    let bases: HashMap<Uuid, DateTime<Local>> = store.field_bases(MASTER_DTSTART_FIELD)?;
    let mut calendar_writes = Vec::new();
    let mut recorded = 0usize;
    let mut store_changed = false;

    for resource in resources {
        let plans = parse_ics(&resource.source, Path::new(resource.location.path.as_str()))?;
        let mut rendered = resource.source.clone();
        let mut resource_dirty = false;
        for ics in plans {
            // Master advancement is a VTODO client compatibility behavior.
            // VEVENT DTSTART edits are ordinary calendar-side reschedules and
            // must never be reinterpreted as completed Action occurrences.
            if ics.component_kind != crate::config::PlanComponentKind::VTodo {
                continue;
            }
            let (Some(plan_uid), Some(current)) =
                (ics.plan.external_id.as_deref(), ics.plan.dtstart)
            else {
                continue;
            };
            if ics.plan.recurrence.is_none() {
                continue;
            }
            let plan_id = ics.plan.id;
            let Some(&base) = bases.get(&plan_id) else {
                store.stamp(plan_id, MASTER_DTSTART_FIELD, &current)?;
                store_changed = true;
                continue;
            };
            if current == base {
                continue;
            }
            let grid: Vec<DateTime<Local>> = ics
                .plan
                .expand_occurrences(base, 1000)
                .into_iter()
                .map(|date| date.with_timezone(&Local))
                .collect();
            let Some(index) = grid
                .iter()
                .position(|&date| date == current)
                .filter(|&index| index >= 1)
            else {
                store.stamp(plan_id, MASTER_DTSTART_FIELD, &current)?;
                store_changed = true;
                continue;
            };
            let completed_slots = grid[..index]
                .iter()
                .map(|&slot| (canonical_occurrence_key(slot), slot))
                .filter(|(key, _)| !ics.exdates.contains(key) && !ics.overrides.contains_key(key))
                .collect::<Vec<_>>();
            rendered = render_master_rollforward(&rendered, plan_uid, base, &completed_slots)?;
            resource_dirty = true;
            recorded += completed_slots.len();
        }
        if resource_dirty {
            calendar_writes.push(SyncCalendarWrite {
                location: resource.location.clone(),
                content: rendered,
            });
        }
    }
    Ok(MasterRollforwardChanges {
        store,
        calendar_writes,
        recorded,
        store_changed,
    })
}

pub fn prepare_master_rollforwards(
    store: PlansSyncStore,
    store_location: ResourceLocation,
    store_expected: ExpectedResource,
    resources: &[PlanResourceState],
) -> Result<PreparedMutation<PlansSyncStore, usize>, WorkspaceError> {
    let changes = prepare_master_rollforward_changes(store, resources)?;
    let mut effects = changes
        .calendar_writes
        .iter()
        .map(|write| Effect::Write {
            path: write.location.clone(),
            bytes: write.content.as_bytes().to_vec(),
        })
        .collect::<Vec<_>>();
    if changes.store_changed {
        effects.push(Effect::Write {
            path: store_location.clone(),
            bytes: serialize_plans_sync_store(&changes.store)?.into_bytes(),
        });
    }
    let mut preconditions = resources
        .iter()
        .map(|resource| ResourcePrecondition {
            path: resource.location.clone(),
            expected: resource.expected.clone(),
        })
        .collect::<Vec<_>>();
    preconditions.push(ResourcePrecondition {
        path: store_location,
        expected: store_expected,
    });
    let batch = EffectBatch::new(effects, preconditions)
        .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
    Ok(PreparedMutation::with_outcome(
        changes.store,
        batch,
        changes.recorded,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    fn t(day: u32) -> DateTime<Local> {
        Local.with_ymd_and_hms(2026, 4, day, 10, 0, 0).unwrap()
    }

    #[test]
    fn generic_three_way_table_and_first_sync() {
        assert_eq!(reconcile(&"a", Some(&"a"), Some(&"a")), Reconcile::NoOp);
        assert_eq!(
            reconcile(&"b", Some(&"a"), Some(&"a")),
            Reconcile::TakeAction("b")
        );
        assert_eq!(
            reconcile(&"a", Some(&"a"), Some(&"b")),
            Reconcile::TakeCalendar("b")
        );
        assert_eq!(
            reconcile(&"b", Some(&"a"), Some(&"b")),
            Reconcile::Converged("b")
        );
        assert!(matches!(
            reconcile(&"b", Some(&"a"), Some(&"c")),
            Reconcile::Conflict { .. }
        ));
        assert_eq!(reconcile(&"a", None, None), Reconcile::TakeAction("a"));
    }

    #[test]
    fn nullable_field_distinguishes_resource_absence_from_missing_value() {
        let none: Time = None;
        assert_eq!(
            reconcile(&none, None, Some(&none)),
            Reconcile::Converged(None)
        );
        assert_eq!(reconcile(&none, None, None), Reconcile::TakeAction(None));
    }

    #[test]
    fn fields_reconcile_independently() {
        let id = Uuid::new_v4();
        let action = Action {
            id,
            name: "base title".into(),
            scheduled_at: Some(t(28)),
            ..Default::default()
        };
        let model = DomainModel {
            objectives: vec![],
            charters: vec![crate::domain::Charter {
                actions: vec![action],
                ..Default::default()
            }],
        };
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        store.stamp(id, SCHEDULED_AT_FIELD, &Some(t(27))).unwrap();
        store.stamp(id, TITLE_FIELD, &"base title").unwrap();
        let calendar = HashMap::from([(
            id,
            VTodoResource {
                action: VTodoAction {
                    id,
                    uid: id.to_string(),
                    scheduled_at: Some(t(27)),
                    due_date: None,
                    state: ActionState::NotStarted,
                    title: "calendar title".into(),
                    description: None,
                    priority: None,
                    contexts: None,
                    completed_at: None,
                },
                path: PathBuf::from("/tmp/plans/work/item.ics"),
                plans_dir: PathBuf::from("work"),
                charter_name: "work".into(),
            },
        )]);
        let report = plan_sync(&model, &store, &calendar).unwrap();
        assert_eq!(
            report.entries[0].scheduled_at,
            Reconcile::TakeAction(Some(t(28)))
        );
        assert_eq!(
            report.entries[0].title,
            Reconcile::TakeCalendar("calendar title".into())
        );
    }

    fn model_with(action: Action) -> DomainModel {
        DomainModel {
            objectives: vec![],
            charters: vec![crate::domain::Charter {
                actions: vec![action],
                ..Default::default()
            }],
        }
    }

    #[test]
    fn one_off_join_uses_plan_identity_not_foreign_uid_as_action_identity() {
        let uid = "foreign-plan@example.com";
        let plan = parse_ics(
            &format!(
                "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:{uid}\r\nSUMMARY:Foreign\r\nDTSTART:20260420T100000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
            ),
            Path::new("foreign.ics"),
        )
        .unwrap()
        .remove(0);
        let action_id = Uuid::now_v7();
        assert_ne!(action_id.to_string(), uid);
        let action = Action {
            id: action_id,
            plan_id: Some(plan.plan.id),
            name: "Foreign".into(),
            scheduled_at: plan.plan.dtstart,
            ..Default::default()
        };

        let report = plan_one_off_sync(
            &model_with(action),
            &PlansSyncStore::new(Path::new("/tmp/plans")),
            &[plan],
        )
        .unwrap();

        assert_eq!(report.entries.len(), 1);
        assert_eq!(report.entries[0].action_id, action_id);
        assert_eq!(report.entries[0].uid, uid);
    }

    #[test]
    fn one_off_vtodo_reconciles_the_full_task_profile() {
        let uid = "task@example.com";
        let plan = parse_ics(
            &format!(
                "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{uid}\r\nSUMMARY:Calendar title\r\nDESCRIPTION:Calendar description\r\nDTSTART:20260421T100000Z\r\nDUE:20260422T100000Z\r\nSTATUS:COMPLETED\r\nCOMPLETED:20260422T110000Z\r\nPRIORITY:2\r\nCATEGORIES:calendar,home\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
            ),
            Path::new("task.ics"),
        )
        .unwrap()
        .remove(0);
        let id = Uuid::now_v7();
        let base_time = Some(t(20));
        let action = Action {
            id,
            plan_id: Some(plan.plan.id),
            name: "Base title".into(),
            description: Some("Base description".into()),
            scheduled_at: base_time,
            due_date: base_time,
            state: ActionState::NotStarted,
            priority: Some(5),
            contexts: Some(vec!["base".into()]),
            ..Default::default()
        };
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        store.stamp(id, SCHEDULED_AT_FIELD, &base_time).unwrap();
        store.stamp(id, DUE_DATE_FIELD, &base_time).unwrap();
        store
            .stamp(id, STATE_FIELD, &ActionState::NotStarted)
            .unwrap();
        store.stamp(id, TITLE_FIELD, &"Base title").unwrap();
        store
            .stamp(id, DESCRIPTION_FIELD, &Some("Base description".to_string()))
            .unwrap();
        store.stamp(id, PRIORITY_FIELD, &Some(5_u32)).unwrap();
        store
            .stamp(id, CONTEXTS_FIELD, &Some(vec!["base".to_string()]))
            .unwrap();

        let entry = plan_one_off_sync(&model_with(action), &store, &[plan])
            .unwrap()
            .entries
            .remove(0);
        assert!(matches!(entry.scheduled_at, Reconcile::TakeCalendar(_)));
        assert!(matches!(entry.due_date, Reconcile::TakeCalendar(_)));
        assert_eq!(entry.state, Reconcile::TakeCalendar(ActionState::Completed));
        assert_eq!(
            entry.title,
            Reconcile::TakeCalendar("Calendar title".into())
        );
        assert_eq!(
            entry.description,
            Reconcile::TakeCalendar(Some("Calendar description".into()))
        );
        assert_eq!(entry.priority, Reconcile::TakeCalendar(Some(2)));
        assert_eq!(
            entry.contexts,
            Reconcile::TakeCalendar(Some(vec!["calendar".into(), "home".into()]))
        );
        assert!(entry.calendar_completed_at.is_some());
    }

    #[test]
    fn one_off_vevent_reconciles_schedule_only() {
        let uid = "event@example.com";
        let plan = parse_ics(
            &format!(
                "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:{uid}\r\nSUMMARY:Calendar display\r\nDESCRIPTION:Calendar display description\r\nDTSTART:20260421T100000Z\r\nDTEND:20260422T100000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
            ),
            Path::new("event.ics"),
        )
        .unwrap()
        .remove(0);
        let id = Uuid::now_v7();
        let base_time = Some(t(20));
        let action = Action {
            id,
            plan_id: Some(plan.plan.id),
            name: "Local title".into(),
            description: Some("Local description".into()),
            scheduled_at: base_time,
            due_date: base_time,
            state: ActionState::InProgress,
            priority: Some(1),
            contexts: Some(vec!["local".into()]),
            ..Default::default()
        };
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        store.stamp(id, SCHEDULED_AT_FIELD, &base_time).unwrap();
        store.stamp(id, DUE_DATE_FIELD, &base_time).unwrap();

        let entry = plan_one_off_sync(&model_with(action), &store, &[plan])
            .unwrap()
            .entries
            .remove(0);
        assert!(matches!(entry.scheduled_at, Reconcile::TakeCalendar(_)));
        assert!(matches!(entry.due_date, Reconcile::TakeCalendar(_)));
        assert_eq!(entry.state, Reconcile::NoOp);
        assert_eq!(entry.title, Reconcile::NoOp);
        assert_eq!(entry.description, Reconcile::NoOp);
        assert_eq!(entry.priority, Reconcile::NoOp);
        assert_eq!(entry.contexts, Reconcile::NoOp);
    }

    #[test]
    fn one_off_planner_excludes_recurring_plans_and_occurrence_actions() {
        let mut recurring = parse_ics(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:series@example.com\r\nSUMMARY:Series\r\nDTSTART:20260421T100000Z\r\nRRULE:FREQ=DAILY\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n",
            Path::new("series.ics"),
        )
        .unwrap()
        .remove(0);
        let recurring_action = Action {
            plan_id: Some(recurring.plan.id),
            name: "Series".into(),
            scheduled_at: recurring.plan.dtstart,
            ..Default::default()
        };
        assert!(
            plan_one_off_sync(
                &model_with(recurring_action),
                &PlansSyncStore::new(Path::new("/tmp/plans")),
                &[recurring.clone()],
            )
            .unwrap()
            .entries
            .is_empty()
        );

        recurring.plan.recurrence = None;
        let occurrence = Action {
            plan_id: Some(recurring.plan.id),
            external_occurrence_key: Some("20260421T100000Z".into()),
            name: "Occurrence".into(),
            scheduled_at: recurring.plan.dtstart,
            ..Default::default()
        };
        assert!(
            plan_one_off_sync(
                &model_with(occurrence),
                &PlansSyncStore::new(Path::new("/tmp/plans")),
                &[recurring],
            )
            .unwrap()
            .entries
            .is_empty()
        );
    }

    #[test]
    fn patch_preserves_vendor_properties_and_alarms() {
        let id = Uuid::new_v4();
        let mut calendar: Calendar = format!(
            "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:{id}\r\nSUMMARY:Old\r\nX-APPLE-SORT-ORDER:7\r\nBEGIN:VALARM\r\nACTION:DISPLAY\r\nTRIGGER:-PT5M\r\nDESCRIPTION:Alarm\r\nEND:VALARM\r\nEND:VTODO\r\nEND:VCALENDAR\r\n"
        ).parse().unwrap();
        let action = Action {
            id,
            name: "New".into(),
            ..Default::default()
        };
        let CalendarComponent::Todo(todo) = &mut calendar.components[0] else {
            panic!()
        };
        patch_todo(todo, &action, &[SyncField::Title]);
        let output = calendar.to_string();
        assert!(output.contains("SUMMARY:New"));
        assert!(output.contains("X-APPLE-SORT-ORDER:7"));
        assert!(output.contains("BEGIN:VALARM"));
    }

    #[test]
    fn imported_vtodo_preserves_every_owned_action_field() {
        let completed_at = t(30);
        let source = VTodoAction {
            id: Uuid::now_v7(),
            uid: "foreign@example.com".to_string(),
            scheduled_at: Some(t(20)),
            due_date: Some(t(25)),
            state: ActionState::Completed,
            title: "Imported title".to_string(),
            description: Some("Imported description".to_string()),
            priority: Some(3),
            contexts: Some(vec!["errands".to_string(), "home".to_string()]),
            completed_at: Some(completed_at),
        };

        let action = action_from_vtodo(&source);

        assert_eq!(
            action,
            Action {
                id: source.id,
                state: source.state,
                name: source.title,
                description: source.description,
                priority: source.priority,
                contexts: source.contexts,
                scheduled_at: source.scheduled_at,
                due_date: source.due_date,
                completed_at: Some(completed_at),
                ..Action::default()
            }
        );
    }

    #[test]
    fn calendar_imports_contribute_to_the_take_calendar_tally() {
        let report = SyncReport {
            imports: vec![SyncImport {
                action: VTodoAction {
                    id: Uuid::now_v7(),
                    uid: "foreign@example.com".to_string(),
                    scheduled_at: None,
                    due_date: None,
                    state: ActionState::NotStarted,
                    title: "Imported".to_string(),
                    description: None,
                    priority: None,
                    contexts: None,
                    completed_at: None,
                },
                plans_dir: PathBuf::from("next"),
                charter_name: "workspace".to_string(),
            }],
            ..SyncReport::default()
        };

        assert_eq!(
            report.tally(),
            SyncTally {
                take_calendar: 1,
                ..SyncTally::default()
            }
        );
    }

    // ---- ensure_active_occurrences: single-token stamping ----

    /// One charter holding one weekly recurring plan, no materialized actions yet.
    /// Returns the charters, the plan id, and its UID.
    #[cfg(feature = "formatting")]
    fn weekly_charter(dtstart: DateTime<Local>) -> (Vec<MarkdownCharter>, Uuid, String) {
        use crate::domain::{Plan, Recurrence};
        use std::collections::{BTreeMap, BTreeSet};

        let plan_id = Uuid::now_v7();
        let uid = "review@example.com".to_string();
        let plan = Plan {
            id: plan_id,
            name: "weekly review".into(),
            external_id: Some(uid.clone()),
            dtstart: Some(dtstart),
            recurrence: Some(Recurrence {
                frequency: "weekly".into(),
                ..Default::default()
            }),
            ..Default::default()
        };
        let ics = crate::workspace::calendar::ics::ICSPlan {
            path: PathBuf::from("review.ics"),
            plan,
            component_kind: crate::config::PlanComponentKind::VTodo,
            schedule_end: None,
            task_fields: None,
            exdates: BTreeSet::new(),
            overrides: BTreeMap::new(),
        };
        let charter = MarkdownCharter {
            id: Uuid::now_v7(),
            title: "health".into(),
            description: None,
            alias: None,
            parent: None,
            objectives: None,
            state: None,
            plans: vec![ics],
            actions: Vec::new(),
            md_file: None,
            actions_file: Some(PathBuf::from("health.actions")),
            plans_dir: PathBuf::from("weekly"),
        };
        (vec![charter], plan_id, uid)
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn stamps_one_token_then_is_idempotent() {
        let dtstart = t(5);
        let now = t(20);
        let (mut charters, plan_id, uid) = weekly_charter(dtstart);
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        let mut dirty = HashSet::new();
        let n = ensure_active_occurrences_prepared(&mut charters, &mut store, &mut dirty, &[], now)
            .unwrap();
        assert_eq!(n, 1, "a fresh recurring plan gets exactly one token");
        assert_eq!(charters[0].actions.len(), 1);

        let occ = &charters[0].actions[0].action;
        assert!(!is_resolved(occ.state));
        let slot = occ.scheduled_at.unwrap();
        assert!(
            slot >= now,
            "the token is the next upcoming slot, never a past one"
        );
        let key = canonical_occurrence_key(slot);
        assert_eq!(
            occ.id,
            crate::workspace::calendar::ics::occurrence_action_id(&uid, &key)
        );
        assert_eq!(store.occurrence_link(occ.id), Some((plan_id, key)));
        assert!(dirty.contains(Path::new("health.actions")));

        // Second run while the token is live and unresolved → nothing new.
        let again =
            ensure_active_occurrences_prepared(&mut charters, &mut store, &mut dirty, &[], now)
                .unwrap();
        assert_eq!(again, 0, "idempotent while the token is live");
        assert_eq!(charters[0].actions.len(), 1);
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn each_recurring_plan_gets_its_own_live_token() {
        let (mut charters, first_plan_id, _) = weekly_charter(t(5));
        let mut second_plan = charters[0].plans[0].clone();
        second_plan.plan.id = Uuid::now_v7();
        second_plan.plan.external_id = Some("second-review@example.com".to_string());
        second_plan.path = PathBuf::from("second-review.ics");
        let second_plan_id = second_plan.plan.id;
        charters[0].plans.push(second_plan);

        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        let mut dirty = HashSet::new();
        let first_plan = charters[0].plans[0].clone();
        stage_prepared_plan_token(&mut charters[0], &mut store, &first_plan, None, &[], t(20))
            .unwrap()
            .expect("first plan token should stamp");

        let stamped =
            ensure_active_occurrences_prepared(&mut charters, &mut store, &mut dirty, &[], t(20))
                .unwrap();

        assert_eq!(stamped, 1, "only the second plan still needs a token");
        let linked_plans: HashSet<_> = store
            .occurrence_links()
            .into_values()
            .map(|(plan_id, _)| plan_id)
            .collect();
        assert_eq!(linked_plans, HashSet::from([first_plan_id, second_plan_id]));
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn resolved_token_advances_by_jump_forward() {
        // Safety net: a token resolved outside the completion hook (a raw `[x]`
        // edit) reads as not-live, so a later sync stamps the next slot >= now,
        // jumping past whatever was missed. Exactly one live token at all times.
        let dtstart = t(5);
        let (mut charters, _plan_id, _uid) = weekly_charter(dtstart);
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        let mut dirty = HashSet::new();
        ensure_active_occurrences_prepared(&mut charters, &mut store, &mut dirty, &[], t(6))
            .unwrap();
        let first_slot = charters[0].actions[0].action.scheduled_at.unwrap();
        charters[0].actions[0].action.state = ActionState::Completed; // resolved by hand
        charters[0].actions.push(SourcedAction {
            action: Action {
                name: "unrelated open action".to_string(),
                ..Action::default()
            },
            source_metadata: None,
        });

        let now_later = first_slot + chrono::Duration::days(1);
        let n = ensure_active_occurrences_prepared(
            &mut charters,
            &mut store,
            &mut dirty,
            &[],
            now_later,
        )
        .unwrap();
        assert_eq!(n, 1, "no live token → the next slot is stamped");
        assert_eq!(charters[0].actions.len(), 3);
        let live_tokens: Vec<_> = charters[0]
            .actions
            .iter()
            .filter(|sa| sa.action.scheduled_at.is_some() && !is_resolved(sa.action.state))
            .collect();
        assert_eq!(live_tokens.len(), 1, "exactly one live token at any time");
        assert!(
            live_tokens[0].action.scheduled_at.unwrap() > first_slot,
            "advanced forward, not backward"
        );
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn occurrence_tokens_and_grafted_steps_are_excluded_from_standalone_sync() {
        // The materialized-token seal: an occurrence token (has a store link) and
        // its grafted template steps (its subtree) must never push as standalone
        // VTODOs — the master + deviations already represent the slot. An ordinary
        // dated action alongside them still syncs.
        let token = Uuid::now_v7();
        let step = Uuid::now_v7();
        let standalone = Uuid::now_v7();
        let plan_id = Uuid::now_v7();

        let model = DomainModel {
            objectives: vec![],
            charters: vec![crate::domain::Charter {
                actions: vec![
                    Action {
                        id: token,
                        name: "Weekly Review".into(),
                        scheduled_at: Some(t(20)),
                        ..Default::default()
                    },
                    Action {
                        id: step,
                        parent_id: Some(token),
                        name: "Review the inbox".into(),
                        ..Default::default()
                    },
                    Action {
                        id: standalone,
                        name: "buy milk".into(),
                        scheduled_at: Some(t(21)),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            }],
        };

        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        store
            .stamp_occurrence_link(token, plan_id, "20260420T170000Z")
            .unwrap();

        // Empty vdir: without the seal every action here would push as a new VTODO.
        let report = plan_sync(&model, &store, &HashMap::new()).unwrap();
        let pushed: HashSet<Uuid> = report.entries.iter().map(|e| e.action_id).collect();

        assert!(
            !pushed.contains(&token),
            "the occurrence token must not leak to the vdir"
        );
        assert!(
            !pushed.contains(&step),
            "the grafted step must stay local, not leak to the vdir"
        );
        assert!(
            pushed.contains(&standalone),
            "an ordinary dated action still reconciles"
        );
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn templated_plan_stamps_root_plus_grafted_steps() {
        // The templated lane: `template:` adds a step-forest beneath the same
        // synthesized occurrence root the atomic lane stamps. One root token
        // (carrying the occurrence identity + store link), with the template's own
        // roots grafted as its children.
        let (mut charters, plan_id, uid) = weekly_charter(t(5));
        charters[0].plans[0].plan.template_name = Some("weekly-review".into());
        let steps = crate::parse_actions(
            "[ ] Review the inbox #01970000-0000-7000-0000-000000000001\n\
             [ ] Reflect on the week #01970000-0000-7000-0000-000000000002\n",
        )
        .unwrap();
        let template = SyncPlanTemplate {
            plan_id,
            generated_ids: vec![Uuid::now_v7(), Uuid::now_v7()],
            steps,
        };

        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        let mut dirty = HashSet::new();
        let n = ensure_active_occurrences_prepared(
            &mut charters,
            &mut store,
            &mut dirty,
            &[template],
            t(20),
        )
        .unwrap();
        assert_eq!(n, 1, "one plan → one token stamped");

        // Root token: carries the occurrence identity + store link, is parentless.
        let acts = &charters[0].actions;
        assert_eq!(acts.len(), 3, "one synthesized root + two grafted steps");
        let root = acts
            .iter()
            .find(|sa| sa.action.plan_id == Some(plan_id))
            .expect("the stamped occurrence root");
        assert!(
            root.action.parent_id.is_none(),
            "the occurrence root is a root"
        );
        let slot = root.action.scheduled_at.unwrap();
        let key = canonical_occurrence_key(slot);
        assert_eq!(
            root.action.id,
            crate::workspace::calendar::ics::occurrence_action_id(&uid, &key)
        );
        assert_eq!(store.occurrence_link(root.action.id), Some((plan_id, key)));

        // The template's roots graft *beneath* the occurrence root.
        let steps: Vec<_> = acts
            .iter()
            .filter(|sa| sa.action.parent_id == Some(root.action.id))
            .collect();
        assert_eq!(steps.len(), 2, "both template steps grafted under the root");
        let names: HashSet<&str> = steps.iter().map(|sa| sa.action.name.as_str()).collect();
        assert!(names.contains("Review the inbox"));
        assert!(names.contains("Reflect on the week"));
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn atomic_plan_stamps_a_childless_root() {
        // The atomic lane is the templated lane minus the template: a plan with no
        // `template_name` stamps exactly the synthesized root, no children — and a
        // *named-but-missing* template must degrade to the same, never fail the sync.
        let (mut charters, plan_id, _uid) = weekly_charter(t(5));
        charters[0].plans[0].plan.template_name = Some("does-not-exist".into());

        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        let mut dirty = HashSet::new();
        let n =
            ensure_active_occurrences_prepared(&mut charters, &mut store, &mut dirty, &[], t(20))
                .unwrap();

        assert_eq!(n, 1, "a missing template still stamps the root token");
        assert_eq!(charters[0].actions.len(), 1, "no phantom children grafted");
        assert_eq!(charters[0].actions[0].action.plan_id, Some(plan_id));
    }
}
