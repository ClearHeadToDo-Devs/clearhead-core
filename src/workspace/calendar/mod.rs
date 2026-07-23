//! The calendar projection: the bridge between ClearHead actions and the
//! configured vdir of `.ics` plans.
//!
//! Everything here is about that one boundary — parsing and emitting `.ics`,
//! expanding recurring plans into actions, locating plan files on disk, and
//! reconciling an action's schedule against its calendar event:
//!
//! - [`ics`] — parse `.ics` into [`Plan`](crate::Plan)s; emit an
//!   [`Action`](crate::Action) as a VTODO.
//! - [`expand`] — expand a recurring plan into concrete actions.
//! - [`plans`] — discover `.ics` plan files under a plans root.
//! - [`reconcile`] — the three-way (action / merge-base / `.ics`) sync decision.

pub mod expand;
pub mod ics;
pub mod plans;
pub mod reconcile;
pub mod sync_store;

pub use expand::{ExpandResult, ExpansionConfig, expand_plans_into_actions};
pub use ics::{ICSPlan, action_to_vtodo, actions_to_icalendar, occurrence_action_id};
pub use plans::{
    PlanFileEntry, action_mirror_path, charter_plans_dir_relative, collect_plan_files,
    collect_plan_files_with_plans, infer_plan_charter_name, infer_plan_parent, plan_file_name,
    plan_output_path,
};
pub use reconcile::{
    AppliedSync, Reconcile, SyncEntry, SyncReport, SyncTally, apply_sync, plan_sync,
    read_ics_dates, reconcile,
};
pub use sync_store::{
    PlansSyncStore, plans_sync_store_path, read_plans_sync_store,
};
