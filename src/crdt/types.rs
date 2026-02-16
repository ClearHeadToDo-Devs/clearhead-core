//! CRDT-optimized mirror types for Automerge synchronization.
//!
//! These types mirror the domain model (`crate::domain`) but are specifically
//! designed for CRDT storage via Automerge/Autosurgeon. They derive
//! `Reconcile` and `Hydrate` — traits that the domain model should NOT need.
//!
//! The separation allows:
//! - Domain types to evolve without CRDT schema concerns
//! - CRDT types to use storage-optimal representations (e.g., dates as strings)
//! - The `sync` feature to be optional without polluting the domain
//!
//! Conversion between domain and CRDT types is handled by `crdt::convert`.

use autosurgeon::{Hydrate, Reconcile};
use std::collections::HashMap;
use uuid::Uuid;

use super::sync_utils::{hydrate_date, reconcile_date};

/// Root CRDT document state.
///
/// Mirrors the workspace-level container. Each file in the workspace
/// maps to a `SyncModel`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncWorkspaceState {
    /// Schema version — detect stale CRDT files after struct changes
    pub version: u32,
    /// Map of file path (relative to workspace root) → sync model
    pub files: HashMap<String, SyncModel>,
}

/// CRDT mirror of `domain::DomainModel`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncModel {
    pub objectives: Vec<SyncObjective>,
    pub charters: Vec<SyncCharter>,
}

/// CRDT mirror of `domain::Objective`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncObjective {
    pub id: Uuid,
    pub title: Option<String>,
    pub description: Option<String>,
    pub alias: Option<String>,
    pub parent: Option<String>,
    pub metrics: Option<Vec<SyncMetric>>,
}

/// CRDT mirror of `domain::Metric`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncMetric {
    pub name: String,
    pub description: Option<String>,
    pub target: Option<String>,
    pub review_date: Option<String>,
}

/// CRDT mirror of `domain::Reference`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub enum SyncReference {
    UUID(Uuid),
    Prefix(String),
    Name(String),
    Alias(String),
}

/// CRDT mirror of `domain::Charter`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncCharter {
    pub id: Uuid,
    pub title: String,
    pub description: Option<String>,
    pub alias: Option<String>,
    pub parent: Option<String>,
    pub objectives: Option<Vec<String>>,
    pub plans: Vec<SyncPlan>,
}

/// CRDT mirror of `domain::Plan`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncPlan {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub priority: Option<u32>,
    pub contexts: Option<Vec<String>>,
    pub recurrence: Option<SyncRecurrence>,
    pub parent: Option<Uuid>,
    pub objective: Option<String>,
    pub alias: Option<String>,
    pub is_sequential: Option<bool>,
    pub duration: Option<u32>,
    pub depends_on: Option<Vec<Uuid>>,
    pub acts: Vec<SyncPlannedAct>,
}

/// CRDT mirror of `domain::PlannedAct`.
///
/// Dates are stored as RFC 3339 strings for Automerge compatibility.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncPlannedAct {
    pub id: Uuid,
    pub plan_id: Uuid,
    pub phase: SyncActPhase,
    #[autosurgeon(reconcile = "reconcile_date", hydrate = "hydrate_date")]
    pub scheduled_at: Option<chrono::DateTime<chrono::Local>>,
    pub duration: Option<u32>,
    #[autosurgeon(reconcile = "reconcile_date", hydrate = "hydrate_date")]
    pub completed_at: Option<chrono::DateTime<chrono::Local>>,
    #[autosurgeon(reconcile = "reconcile_date", hydrate = "hydrate_date")]
    pub created_at: Option<chrono::DateTime<chrono::Local>>,
}

/// CRDT mirror of `domain::ActPhase`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Reconcile, Hydrate)]
pub enum SyncActPhase {
    #[default]
    NotStarted,
    InProgress,
    Completed,
    Blocked,
    Cancelled,
}

/// CRDT mirror of `domain::Recurrence`.
#[derive(Debug, Clone, Reconcile, Hydrate)]
pub struct SyncRecurrence {
    pub frequency: String,
    pub interval: Option<u32>,
    pub count: Option<u32>,
    pub until: Option<String>,
    pub by_second: Option<Vec<u32>>,
    pub by_minute: Option<Vec<u32>>,
    pub by_hour: Option<Vec<u32>>,
    pub by_day: Option<Vec<String>>,
    pub by_month_day: Option<Vec<i32>>,
    pub by_year_day: Option<Vec<i32>>,
    pub by_week_no: Option<Vec<i32>>,
    pub by_month: Option<Vec<u32>>,
    pub by_set_pos: Option<Vec<i32>>,
    pub week_start: Option<String>,
}
