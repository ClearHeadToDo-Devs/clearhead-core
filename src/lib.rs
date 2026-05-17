//! ClearHead Core Library
//!
//! This library provides the core domain model and logic for the ClearHead framework,
//! aligned with the Actions Vocabulary v4 ontology. It is designed to be
//! frontend-agnostic, supporting CLI, LSP, and web services.
//!
//! # Core Concepts
//!
//! - **Ontology Aligned**: Follows BFO/CCO principles, distinguishing between
//!   Information Content ([`Plan`][domain::Plan]) and Occurrences ([`PlannedAct`][domain::PlannedAct]).
//! - **DSL Driven**: Primary interaction via the `.actions` DSL, parsed and
//!   managed by the [`workspace`] module.
//! - **Semantic**: Integrated RDF/SPARQL support in the [`graph`] module for
//!   complex queries and linked-data exports.
//!
//! # Module Hierarchy
//!
//! - [`workspace`]: DSL projection — `.actions` parsing/formatting, charters, store.
//! - [`domain`]: Higher-level domain models (Objectives, Charters, Plans, Acts).
//! - [`sync`]: Semantic comparison and sync decision logic.
//! - [`graph`]: RDF/SPARQL integration for semantic queries.
//! - [`crdt`]: CRDT operations (Deferred for future implementation).

pub mod workspace;
pub use workspace::store::{ManifestSourceType, WorkspaceManifestEntry};

pub mod config;
pub use config::WorkspaceConfig;
#[doc(inline)]
pub use workspace::{
    Action, ActionList, ActionState, Diff, MarkdownCharter, OutputFormat, ParseFailure, ParseMode,
    ParseOutcome, ParsedDocument, RecoveryReport, SourceMetadata, SourceRange, collect_plan_files,
    collect_workspace_manifest, completed_acts_path, diff_actions, format, format_charter,
    implicit_charter, infer_charter_name, infer_charter_name_for_workspace,
    infer_parent_charter_name, infer_parent_charter_name_for_workspace, infer_plan_charter_name,
    infer_plan_charter_name_for_workspace, infer_plan_parent, infer_plan_parent_for_workspace,
    expand_plans_into_acts, list_action_files, load_domain_model, load_workspace,
    occurrence_act_id, parse_actions,
    parse_actions_with_mode, parse_charter, parse_document, parse_tree, patch_action_list,
    charter_root, plans_root, read_acts, save_domain_model, workspace_data_root, write_acts,
};

pub mod domain;
#[doc(inline)]
pub use domain::diff::{
    ActDiff, ActFieldChange, CharterDiff, CharterFieldChange, DomainDiff, PlanDiff,
    PlanFieldChange, diff_domain_models,
};
#[doc(inline)]
pub use domain::{ActPhase, Charter, DomainModel, Metric, Objective, Plan, PlannedAct, Recurrence};

pub mod crdt;

pub mod sync;
#[doc(inline)]
pub use sync::DomainSyncDecision;

pub mod graph;

pub mod display;
#[doc(inline)]
pub use display::format_domain_as_table;

pub mod reference;
#[doc(inline)]
pub use reference::{
    MatchMode, ReferenceError, ReferenceOptions, ReferenceTarget, filter_model_for_act,
    filter_model_for_charter, filter_model_for_plan, resolve_reference,
};

pub mod telemetry;
#[doc(inline)]
pub use telemetry::{
    NoopEmitter, TelemetryEmitter, TelemetryEvent, TelemetryRecord, Tool, event_from_field_change,
    event_from_state_change, noop_emitter,
};
