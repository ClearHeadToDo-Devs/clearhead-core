//! The calendar projection: the bridge between ClearHead actions and the
//! configured vdir of `.ics` plans.
//!
//! Everything here is about that one boundary — parsing and emitting `.ics`,
//! expanding recurring plans into actions, locating plan files on disk, and
//! reconciling an action's schedule against its calendar event:
//!
//! - [`ics`] — parse and emit Plan and Action VTODO projections.
//! - [`expand`] — expand a recurring plan into concrete actions.
//! - [`plans`] — discover `.ics` plan files under a plans root.
//! - [`reconcile`] — the three-way (action / merge-base / `.ics`) sync decision.

pub mod expand;
pub mod ics;
pub mod plans;
pub mod reconcile;
pub mod sync_store;

pub use expand::render_occurrences;
pub use ics::{
    ICSPlan, OccurrenceOp, OccurrenceOverride, VTodoAction, action_id_from_vtodo_uid,
    action_to_vtodo, actions_to_icalendar, canonical_occurrence_key, occurrence_action_id,
    parse_vtodo_actions, plan_to_vtodo, plans_to_icalendar, write_master_rollforward,
    write_occurrence_deviation,
};
pub use plans::{
    PlanFileEntry, action_mirror_path, apply_occurrence_op, charter_plans_dir_relative,
    collect_plan_files, collect_plan_files_with_plans, infer_plan_charter_name, infer_plan_parent,
    plan_file_name, plan_output_path, slugify,
};
pub use reconcile::{
    AppliedSync, CalendarSyncPreparationInput, CalendarSyncState, OutcomeKind, Reconcile,
    SyncActionResourceState, SyncConflictResolution, SyncEntry, SyncField, SyncImport,
    SyncMirrorResourceState, SyncPlanTemplate, SyncReport, SyncTally, VTodoResource, plan_sync,
    prepare_sync, read_ics_dates, read_vtodo_actions, reconcile, sync_import_actions_file,
    sync_master_rollforwards,
};
pub use sync_store::{PlansSyncStore, plans_sync_store_path, read_plans_sync_store};
