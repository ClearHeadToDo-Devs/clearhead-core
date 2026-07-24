//! Field-wise three-way reconciliation between Actions and the configured plans vdir.
//!
//! The plans vdir is the complete integration boundary. No server, account,
//! href, ETag, or transport-specific metadata enters this module. Each owned
//! VTODO field is merged independently against its last-agreed value so a
//! conflict in one field never blocks safe changes in another.

use chrono::{DateTime, Local, Utc};
use icalendar::{Calendar, CalendarComponent, Component, EventLike, Todo, TodoStatus};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use uuid::Uuid;

use super::ics::{VTodoAction, action_to_vtodo, parse_vtodo_actions};
use super::plans::{action_mirror_path, charter_plans_dir_relative, collect_plan_files_in};
use super::sync_store::{
    CONTEXTS_FIELD, DESCRIPTION_FIELD, DUE_DATE_FIELD, PRIORITY_FIELD, PlansSyncStore,
    SCHEDULED_AT_FIELD, STATE_FIELD, TITLE_FIELD, UID_FIELD, plans_sync_store_path,
    read_plans_sync_store, serialize_plans_sync_store,
};
use crate::domain::{Action, ActionState, DomainModel};
use crate::workspace::charter::{MarkdownCharter, implicit_charter};
use crate::workspace::durability::{PendingBatch, WorkspaceLock, atomic_write, recover_pending};
use crate::workspace::store::{Workspace, WorkspaceError, resolve_workspace_layout};
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

impl SyncReport {
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty() && self.imports.is_empty() && self.warnings.is_empty()
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

/// Read all standalone VTODO projections in the vdir, keyed by RFC 5545 UID.
/// File names and vendor properties are irrelevant. Duplicate UIDs are rejected
/// rather than resolved by traversal order.
pub fn read_vtodo_actions(
    plans_root: &Path,
) -> Result<HashMap<Uuid, VTodoResource>, WorkspaceError> {
    let mut actions = HashMap::new();
    for entry in collect_plan_files_in(plans_root, None)? {
        let plans_dir = entry
            .relative_path
            .parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| WorkspaceError::InvalidPath(entry.relative_path.clone()))?;
        for action in parse_vtodo_actions(&entry.path)? {
            let resource = VTodoResource {
                action: action.clone(),
                path: entry.path.clone(),
                plans_dir: plans_dir.clone(),
                charter_name: entry.charter_name.clone(),
            };
            if actions.insert(action.id, resource).is_some() {
                return Err(WorkspaceError::Parse(format!(
                    "duplicate standalone VTODO Action identity {} in configured plans vdir",
                    action.id
                )));
            }
        }
    }
    Ok(actions)
}

/// Compatibility helper for callers interested only in DTSTART.
pub fn read_ics_dates(plans_root: &Path) -> Result<HashMap<Uuid, Time>, WorkspaceError> {
    Ok(read_vtodo_actions(plans_root)?
        .into_iter()
        .map(|(id, resource)| (id, resource.action.scheduled_at))
        .collect())
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
    for action in model.all_actions() {
        if action.external_schedule_id.is_some() {
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

/// Apply a report under the workspace lock. Action files and merge bases share
/// one pending batch. VTODO files are updated first, preserving properties and
/// child components not owned by ClearHead.
pub fn apply_sync(
    root: &Path,
    plan_override: Option<&Path>,
    report: &SyncReport,
) -> Result<AppliedSync, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    std::fs::create_dir_all(&layout.charter_root)?;
    let _lock = WorkspaceLock::try_acquire(&layout.data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(layout.data_root.clone()))?;
    recover_pending(&layout.charter_root)?;

    let mut workspace = Workspace::load_with_plans(root, plan_override)?;
    let plans_root = plan_override.unwrap_or(&layout.plans_root);
    let mut store = read_plans_sync_store(root, plans_root)?;
    // Preserve the actual vdir resource path and UID chosen by external tools;
    // only newly emitted resources use ClearHead's canonical UUID identity.
    let resources = read_vtodo_actions(plans_root)?;
    let mut dirty_actions = HashSet::new();
    let mut applied = AppliedSync::default();

    for import in &report.imports {
        let charter_idx = locate_or_create_import_charter(&mut workspace.charters, import);
        let actions_relative = import_actions_file(&mut workspace.charters[charter_idx], import);
        let action = action_from_vtodo(&import.action);
        workspace.charters[charter_idx].actions.push(SourcedAction {
            action,
            source_metadata: None,
        });
        dirty_actions.insert(layout.charter_root.join(actions_relative));
        stamp_projection(&mut store, &import.action)?;
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
                &mut store,
                &mut applied,
            )?;
            apply_time_outcome(
                &entry.due_date,
                &mut action.due_date,
                entry.action_id,
                DUE_DATE_FIELD,
                SyncField::DueDate,
                &mut push_fields,
                &mut store,
                &mut applied,
            )?;
            apply_state_outcome(
                &entry.state,
                entry.calendar_completed_at,
                action,
                entry.action_id,
                &mut push_fields,
                &mut store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.title,
                &mut action.name,
                entry.action_id,
                TITLE_FIELD,
                SyncField::Title,
                &mut push_fields,
                &mut store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.description,
                &mut action.description,
                entry.action_id,
                DESCRIPTION_FIELD,
                SyncField::Description,
                &mut push_fields,
                &mut store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.priority,
                &mut action.priority,
                entry.action_id,
                PRIORITY_FIELD,
                SyncField::Priority,
                &mut push_fields,
                &mut store,
                &mut applied,
            )?;
            apply_value_outcome(
                &entry.contexts,
                &mut action.contexts,
                entry.action_id,
                CONTEXTS_FIELD,
                SyncField::Contexts,
                &mut push_fields,
                &mut store,
                &mut applied,
            )?;
            (push_fields, action.clone())
        };

        if entry
            .outcomes()
            .iter()
            .any(|(_, outcome)| *outcome == OutcomeKind::TakeCalendar)
        {
            dirty_actions.insert(layout.charter_root.join(&actions_relative));
        }
        if !push_fields.is_empty() {
            let resource = resources.get(&entry.action_id);
            let path = resource
                .map(|resource| resource.path.clone())
                .unwrap_or_else(|| {
                    action_mirror_path(
                        plans_root,
                        &workspace.charters[charter_idx],
                        &action_for_calendar,
                    )
                });
            patch_action_mirror(&path, &entry.uid, &action_for_calendar, &push_fields)?;
        }
    }

    let mut batch = PendingBatch::new(layout.charter_root.clone());
    let mut paths: Vec<_> = dirty_actions.into_iter().collect();
    paths.sort();
    for action_path in paths {
        let relative = action_path
            .strip_prefix(&layout.charter_root)
            .unwrap_or(&action_path);
        let charter = workspace
            .charters
            .iter()
            .find(|charter| charter.actions_file.as_deref() == Some(relative))
            .ok_or_else(|| {
                WorkspaceError::Parse(format!(
                    "dirty action file missing charter: {}",
                    action_path.display()
                ))
            })?;
        let content = render_actions(&charter.actions)?;
        batch.stage(action_path, content.as_bytes())?;
    }
    let sync_content = serialize_plans_sync_store(&store)?;
    batch.stage(plans_sync_store_path(root), sync_content.as_bytes())?;
    batch.commit()?;
    let tally = report.tally();
    Ok(AppliedSync {
        take_action: tally.take_action,
        take_calendar: tally.take_calendar,
        converged: tally.converged,
        conflict: tally.conflict,
    })
}

fn locate_or_create_import_charter(
    charters: &mut Vec<MarkdownCharter>,
    import: &SyncImport,
) -> usize {
    if let Some(index) = charters.iter().position(|charter| {
        charter_plans_dir_relative(charter) == import.plans_dir
            || charter.alias.as_deref() == Some(&import.charter_name)
            || charter.title == import.charter_name
    }) {
        return index;
    }

    let mut charter = MarkdownCharter::from(implicit_charter(&import.charter_name));
    charter.plans_dir = Some(import.plans_dir.clone());
    charters.push(charter);
    charters.len() - 1
}

fn import_actions_file(charter: &mut MarkdownCharter, import: &SyncImport) -> PathBuf {
    if let Some(path) = &charter.actions_file {
        return path.clone();
    }
    let path = charter
        .md_file
        .as_ref()
        .map(|path| path.with_extension("actions"))
        .unwrap_or_else(|| {
            if import.plans_dir == Path::new("next") {
                PathBuf::from("next.actions")
            } else {
                PathBuf::from(format!("{}.actions", import.charter_name))
            }
        });
    charter.actions_file = Some(path.clone());
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

fn patch_action_mirror(
    path: &Path,
    uid: &str,
    action: &Action,
    fields: &[SyncField],
) -> Result<(), WorkspaceError> {
    if !path.exists() {
        let mut calendar = Calendar::new().name("ClearHead Actions").done();
        let mut todo = action_to_vtodo(action);
        todo.uid(uid);
        calendar.push(todo);
        return atomic_write(path, calendar.to_string().as_bytes()).map_err(WorkspaceError::Io);
    }

    let content = std::fs::read_to_string(path)?;
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
            "action mirror {} does not contain standalone VTODO UID {}",
            path.display(),
            uid
        )));
    }
    atomic_write(path, calendar.to_string().as_bytes()).map_err(WorkspaceError::Io)
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
}
