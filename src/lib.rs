//! ClearHead Core Library
//!
//! This library provides the core domain model and logic for the ClearHead framework.
//! It is designed to be consumed by multiple frontends (CLI, LSP, web services, etc.)
//! without containing any frontend-specific concerns.
//!
//! # Modules
//!
//! - `workspace`: DSL projection — `.actions` parsing/formatting, charters, store
//! - `domain`: Higher-level domain models (Objectives, Charters, Plan, Act, phases, Recurrence)
//! - `sync`: Semantic comparison and sync decision logic
//! - `document`: Document save pipeline orchestration
//! - `crdt`: CRDT operations for distributed synchronization
//! - `graph`: RDF/SPARQL integration for semantic queries

pub mod workspace;
pub use workspace::{
    Action, ActionList, ActionState, Diff, DiscoveredCharter, FsWorkspaceStore, InMemoryStore,
    ObjectiveRef, OutputFormat, ParsedDocument, SourceMetadata, SourceRange, WorkspaceStore,
    closed_acts_path, diff_actions, format, format_charter, implicit_charter, merge_acts_into_model,
    open_acts_path, parse_actions, parse_charter, parse_document, parse_domain_model, parse_tree,
    patch_action_list, read_acts, write_acts, write_acts_for_plans,
};

pub mod diff;
pub use diff::diff_domain_models;

pub mod domain;
pub use domain::{
    ActDiff, ActFieldChange, ActPhase, Charter, DomainDiff, DomainModel, Metric, Objective, Plan,
    PlanDiff, PlanFieldChange, PlannedAct, Recurrence,
};

pub mod crdt;

pub mod sync;
pub use sync::DomainSyncDecision;

pub mod document;
pub use document::DomainSaveResult;

pub mod graph;

pub mod display;
pub use display::format_domain_as_table;

pub mod reference;
pub use reference::{
    MatchMode,
    ReferenceError,
    ReferenceOptions,
    ReferenceTarget,
    filter_model_for_act,
    filter_model_for_charter,
    filter_model_for_plan,
    resolve_reference,
};

pub mod telemetry;
pub use telemetry::{
    NoopEmitter, TelemetryEmitter, TelemetryEvent, TelemetryRecord, Tool, event_from_field_change,
    event_from_state_change, noop_emitter,
};
