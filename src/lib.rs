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
    diff_actions, format, format_charter, implicit_charter, parse_actions, parse_charter,
    parse_document, parse_domain_model, parse_tree, patch_action_list,
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
