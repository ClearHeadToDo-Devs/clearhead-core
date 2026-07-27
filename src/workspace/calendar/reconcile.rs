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

use super::ics::{
    VTodoAction, action_to_vtodo, canonical_occurrence_key, parse_ics_file, parse_vtodo_actions,
    write_master_rollforward,
};
use super::expand::{next_active_slot, render_occurrence};
use super::plans::{action_mirror_path, charter_plans_dir_relative, collect_plan_files_in};
use super::sync_store::{
    CONTEXTS_FIELD, DESCRIPTION_FIELD, DUE_DATE_FIELD, MASTER_DTSTART_FIELD, PRIORITY_FIELD,
    PlansSyncStore, SCHEDULED_AT_FIELD, STATE_FIELD, TITLE_FIELD, UID_FIELD, plans_sync_store_path,
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

    // Single-token stamping rides the same lock and batch: ensure every recurring
    // plan has one live materialized occurrence before we stage.
    ensure_active_occurrences(
        &mut workspace.charters,
        &mut store,
        &mut dirty_actions,
        &layout.charter_root,
        Local::now(),
    )?;

    commit_actions_and_store(
        &layout.charter_root,
        plans_sync_store_path(root),
        &workspace.charters,
        &store,
        dirty_actions,
    )?;
    let tally = report.tally();
    Ok(AppliedSync {
        take_action: tally.take_action,
        take_calendar: tally.take_calendar,
        converged: tally.converged,
        conflict: tally.conflict,
    })
}

/// Stage every dirty `.actions` file (re-rendered from its charter) plus the sync
/// store into one atomic [`PendingBatch`] and commit. The shared tail of every
/// write touching both layers, so `.actions` and merge bases never diverge.
fn commit_actions_and_store(
    charter_root: &Path,
    store_path: PathBuf,
    charters: &[MarkdownCharter],
    store: &PlansSyncStore,
    dirty_actions: HashSet<PathBuf>,
) -> Result<(), WorkspaceError> {
    let mut batch = PendingBatch::new(charter_root.to_path_buf());
    let mut paths: Vec<_> = dirty_actions.into_iter().collect();
    paths.sort();
    for action_path in paths {
        let relative = action_path.strip_prefix(charter_root).unwrap_or(&action_path);
        let charter = charters
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
    batch.stage(store_path, serialize_plans_sync_store(store)?.as_bytes())?;
    batch.commit()?;
    Ok(())
}

/// Resolve a *materialized* recurring occurrence (`complete`/`cancel` on its
/// `.actions` line): record the deviation on its master, drop its store link, and
/// eagerly stamp the plan's next token. Returns `Ok(false)` when `occurrence_id`
/// carries no occurrence link — an ordinary action, so the caller's normal close
/// was all that was needed.
///
/// The line's own state is set by that normal close; this adds only the calendar
/// round-trip and the single-token advance. Advance reuses
/// [`ensure_active_occurrences`]: with the link cleared the plan has no live token,
/// so the stamper produces the next. (One imperfection: completing a *future* slot
/// early re-selects that same slot, whose `[x]` line already exists, so no token is
/// stamped until `now` passes it — it self-heals on a later sync.)
pub fn resolve_materialized_occurrence(
    root: &Path,
    plan_override: Option<&Path>,
    occurrence_id: Uuid,
    op: &super::ics::OccurrenceOp,
    now: DateTime<Local>,
) -> Result<bool, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    let plans_root = plan_override.unwrap_or(&layout.plans_root).to_path_buf();

    let (plan_id, slot_key) =
        match read_plans_sync_store(root, &plans_root)?.occurrence_link(occurrence_id) {
            Some(link) => link,
            None => return Ok(false),
        };

    // 1. Calendar round-trip: the deviation on the master (the proven write path).
    super::plans::apply_occurrence_op(root, plan_override, plan_id, &slot_key, op)?;

    // 2. Clear the link and advance the token, atomic with the store. Reload after
    //    the deviation so the stamper sees the fresh EXDATE / completed override.
    let _lock = WorkspaceLock::try_acquire(&layout.data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(layout.data_root.clone()))?;
    recover_pending(&layout.charter_root)?;

    let mut workspace = Workspace::load_with_plans(root, plan_override)?;
    let mut store = read_plans_sync_store(root, &plans_root)?;
    store.clear_occurrence_link(occurrence_id);

    // Advance: stamp the plan's next token, using the resolved slot as the floor so
    // an on-time or early completion advances rather than re-selecting it.
    let mut dirty_actions = HashSet::new();
    let floor = parse_occurrence_key(&slot_key);
    if let Some(charter_idx) = workspace
        .charters
        .iter()
        .position(|c| c.plans.iter().any(|p| p.plan.id == plan_id))
        && let Some(plan) = workspace.charters[charter_idx]
            .plans
            .iter()
            .find(|p| p.plan.id == plan_id)
            .cloned()
        && let Some(path) = stage_plan_token(
            &mut workspace.charters[charter_idx],
            &mut store,
            &plan,
            floor,
            now,
            &layout.charter_root,
        )?
    {
        dirty_actions.insert(path);
    }

    commit_actions_and_store(
        &layout.charter_root,
        plans_sync_store_path(root),
        &workspace.charters,
        &store,
        dirty_actions,
    )?;
    Ok(true)
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
fn ensure_active_occurrences(
    charters: &mut Vec<MarkdownCharter>,
    store: &mut PlansSyncStore,
    dirty_actions: &mut HashSet<PathBuf>,
    charter_root: &Path,
    now: DateTime<Local>,
) -> Result<usize, WorkspaceError> {
    let links = store.occurrence_links();
    let mut stamped = 0;

    for charter_idx in 0..charters.len() {
        // Clone the plan list so we can mutate this charter's actions in the loop.
        let plans = charters[charter_idx].plans.clone();
        for plan in &plans {
            if plan.plan.external_id.is_none() || plan.plan.recurrence.is_none() {
                continue; // need a UID for identity; one-shots aren't tokened series
            }

            // Skip if this plan already has a live (unresolved) token anywhere.
            let has_live_token = links.iter().any(|(occ_id, (pid, _slot))| {
                *pid == plan.plan.id
                    && charters.iter().any(|c| {
                        c.actions
                            .iter()
                            .any(|sa| sa.action.id == *occ_id && !is_resolved(sa.action.state))
                    })
            });
            if has_live_token {
                continue;
            }

            // No live token → stamp the next upcoming slot (no floor).
            if let Some(path) =
                stage_plan_token(&mut charters[charter_idx], store, plan, None, now, charter_root)?
            {
                dirty_actions.insert(path);
                stamped += 1;
            }
        }
    }

    Ok(stamped)
}

/// Render `plan`'s token at `next_active_slot(plan, floor, now)` and stage it into
/// `charter`: a real `.actions` line plus its `(plan_id, slot)` link in `store`.
/// Returns the actions file to mark dirty, or `None` when there is no such slot or
/// it is already materialized in `charter`.
///
/// `floor` is the exclusive lower bound the next slot must exceed: `None` for a
/// plan's first token, `Some(resolved_slot)` when advancing past a just-resolved
/// one (so on-time and early completions advance instead of re-selecting it).
fn stage_plan_token(
    charter: &mut MarkdownCharter,
    store: &mut PlansSyncStore,
    plan: &super::ics::ICSPlan,
    floor: Option<DateTime<Local>>,
    now: DateTime<Local>,
    charter_root: &Path,
) -> Result<Option<PathBuf>, WorkspaceError> {
    let Some(uid) = plan.plan.external_id.as_deref() else {
        return Ok(None);
    };
    let Some(slot) = next_active_slot(plan, floor, now) else {
        return Ok(None); // series exhausted or no anchor
    };
    let occurrence = render_occurrence(plan, uid, slot);
    let occ_id = occurrence.id;
    if charter.actions.iter().any(|sa| sa.action.id == occ_id) {
        return Ok(None); // this slot is already materialized
    }
    let slot_key = occurrence
        .external_occurrence_key
        .clone()
        .expect("render_occurrence always sets the occurrence key");
    let actions_relative = charter.actions_file.clone().ok_or_else(|| {
        WorkspaceError::Parse(format!(
            "charter {} carries plans but has no actions_file to stamp into",
            charter.id
        ))
    })?;
    charter.actions.push(SourcedAction {
        action: occurrence,
        source_metadata: None,
    });
    store.stamp_occurrence_link(occ_id, plan.plan.id, &slot_key)?;
    Ok(Some(charter_root.join(actions_relative)))
}

/// Parse a [`canonical_occurrence_key`] (`%Y%m%dT%H%M%SZ`, UTC) back to a local
/// instant, for use as an advance floor. `None` if it is not our canonical form.
fn parse_occurrence_key(key: &str) -> Option<DateTime<Local>> {
    chrono::NaiveDateTime::parse_from_str(key, "%Y%m%dT%H%M%SZ")
        .ok()
        .map(|naive| naive.and_utc().with_timezone(&Local))
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

/// Ingest foreign roll-forwards on recurring masters in the configured plans vdir.
///
/// Camp-B clients (Apple Reminders, etc.) complete a recurring VTODO by *advancing
/// the master `DTSTART`* with no override. This pass detects that — the master's
/// `DTSTART` moved forward onto a later point of its own recurrence grid, relative
/// to the origin we hold in [`MASTER_DTSTART_FIELD`] — and translates it into
/// ClearHead's canonical form: reset the anchor to the origin and record each
/// passed slot as a completed occurrence (a `RECURRENCE-ID` override). See
/// [`write_master_rollforward`] for the idempotency/spec rationale.
///
/// - **First sight** of a master establishes its origin; nothing is recorded.
/// - An **off-grid** new `DTSTART` is a genuine series reschedule, not a
///   roll-forward: the origin is updated and no completions are recorded.
/// - Runs under the workspace lock. Returns the number of occurrences recorded.
pub fn sync_master_rollforwards(
    root: &Path,
    plan_override: Option<&Path>,
) -> Result<usize, WorkspaceError> {
    let layout = resolve_workspace_layout(root);
    let _lock = WorkspaceLock::try_acquire(&layout.data_root)?
        .ok_or_else(|| WorkspaceError::WorkspaceLocked(layout.data_root.clone()))?;

    let plans_root = plan_override.unwrap_or(&layout.plans_root);
    let mut store = read_plans_sync_store(root, plans_root)?;
    let bases: HashMap<Uuid, DateTime<Local>> = store.field_bases(MASTER_DTSTART_FIELD)?;

    let mut recorded = 0usize;
    let mut store_dirty = false;

    for entry in collect_plan_files_in(plans_root, None)? {
        for ics in parse_ics_file(&entry.path)? {
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
                // First sight: establish the canonical origin, detect nothing.
                store.stamp(plan_id, MASTER_DTSTART_FIELD, &current)?;
                store_dirty = true;
                continue;
            };
            if current == base {
                continue;
            }

            // Is `current` a later point on the recurrence grid anchored at the
            // origin? If not, it's a genuine reschedule — accept the new origin.
            let grid: Vec<DateTime<Local>> = ics
                .plan
                .expand_occurrences(base, 1000)
                .into_iter()
                .map(|dt| dt.with_timezone(&Local))
                .collect();
            let Some(k) = grid.iter().position(|&d| d == current).filter(|&k| k >= 1) else {
                store.stamp(plan_id, MASTER_DTSTART_FIELD, &current)?;
                store_dirty = true;
                continue;
            };

            // The passed slots grid[0..k] were completed by the advance. Skip any
            // already excluded or overridden — recording stays idempotent.
            let completed_slots: Vec<(String, DateTime<Local>)> = grid[..k]
                .iter()
                .map(|&slot| (canonical_occurrence_key(slot), slot))
                .filter(|(key, _)| !ics.exdates.contains(key) && !ics.overrides.contains_key(key))
                .collect();

            // Reset the anchor to the origin (always) and record completions. The
            // origin itself is unchanged, so the stored base is not restamped.
            write_master_rollforward(&ics.path, plan_uid, base, &completed_slots)?;
            recorded += completed_slots.len();
        }
    }

    if store_dirty {
        let content = serialize_plans_sync_store(&store)?;
        atomic_write(&plans_sync_store_path(root), content.as_bytes()).map_err(WorkspaceError::Io)?;
    }
    Ok(recorded)
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

    // ---- ensure_active_occurrences: single-token stamping ----

    /// One charter holding one weekly recurring plan, no materialized actions yet.
    /// Returns the charters, the plan id, and its UID.
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
            plans_dir: None,
        };
        (vec![charter], plan_id, uid)
    }

    #[test]
    fn stamps_one_token_then_is_idempotent() {
        let dtstart = t(5);
        let now = t(20);
        let (mut charters, plan_id, uid) = weekly_charter(dtstart);
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        let mut dirty = HashSet::new();
        let root = Path::new("/ws/.clearhead/charters");

        let n =
            ensure_active_occurrences(&mut charters, &mut store, &mut dirty, root, now).unwrap();
        assert_eq!(n, 1, "a fresh recurring plan gets exactly one token");
        assert_eq!(charters[0].actions.len(), 1);

        let occ = &charters[0].actions[0].action;
        assert!(!is_resolved(occ.state));
        let slot = occ.scheduled_at.unwrap();
        assert!(slot >= now, "the token is the next upcoming slot, never a past one");
        let key = canonical_occurrence_key(slot);
        assert_eq!(occ.id, crate::workspace::calendar::ics::occurrence_action_id(&uid, &key));
        assert_eq!(store.occurrence_link(occ.id), Some((plan_id, key)));
        assert!(dirty.contains(&root.join("health.actions")));

        // Second run while the token is live and unresolved → nothing new.
        let again =
            ensure_active_occurrences(&mut charters, &mut store, &mut dirty, root, now).unwrap();
        assert_eq!(again, 0, "idempotent while the token is live");
        assert_eq!(charters[0].actions.len(), 1);
    }

    #[test]
    fn resolved_token_advances_by_jump_forward() {
        // Safety net: a token resolved outside the completion hook (a raw `[x]`
        // edit) reads as not-live, so a later sync stamps the next slot >= now,
        // jumping past whatever was missed. Exactly one live token at all times.
        let dtstart = t(5);
        let (mut charters, _plan_id, _uid) = weekly_charter(dtstart);
        let mut store = PlansSyncStore::new(Path::new("/tmp/plans"));
        let mut dirty = HashSet::new();
        let root = Path::new("/ws/.clearhead/charters");

        ensure_active_occurrences(&mut charters, &mut store, &mut dirty, root, t(6)).unwrap();
        let first_slot = charters[0].actions[0].action.scheduled_at.unwrap();
        charters[0].actions[0].action.state = ActionState::Completed; // resolved by hand

        let now_later = first_slot + chrono::Duration::days(1);
        let n = ensure_active_occurrences(&mut charters, &mut store, &mut dirty, root, now_later)
            .unwrap();
        assert_eq!(n, 1, "no live token → the next slot is stamped");
        assert_eq!(charters[0].actions.len(), 2);
        let live: Vec<_> = charters[0]
            .actions
            .iter()
            .filter(|sa| !is_resolved(sa.action.state))
            .collect();
        assert_eq!(live.len(), 1, "exactly one live token at any time");
        assert!(
            live[0].action.scheduled_at.unwrap() > first_slot,
            "advanced forward, not backward"
        );
    }
}
