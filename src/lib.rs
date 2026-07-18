//! ClearHead Core Library
//!
//! Pure domain model and business logic for the ClearHead framework, aligned
//! with the Actions Vocabulary v4 ontology. Designed to be environment-agnostic:
//! no filesystem access, no network, no configuration loading. Every tool (CLI,
//! LSP, nvim plugin) constructs this layer and passes context in.
//!
//! # Domain Model
//!
//! The hierarchy is: [`Objective`] → [`Charter`] → [`Plan`] / [`Action`]
//!
//! - [`Objective`] — high-level goal that organises charters.
//! - [`Charter`] — domain of concern; owns a list of [`Plan`]s and [`Action`]s.
//! - [`Plan`] — recurring schedule definition (RRULE + DTSTART). Produces [`Action`]s
//!   via the expansion workflow. Does not carry execution state.
//! - [`Action`] — atomic executable work item. The primary unit users interact with.
//!   May be created directly (ad-hoc) or generated from a [`Plan`].
//! - [`ActionState`] — lifecycle state that inheres in an [`Action`]:
//!   `NotStarted` → `InProgress` → `Completed` (or `Cancelled` / `BlockedOrAwaiting`).
//!
//! # Module Hierarchy
//!
//! - [`workspace`]: DSL projection — `.actions` parsing/formatting, charter discovery,
//!   ICS plan loading, expansion, and workspace store.
//! - [`domain`]: Core structs ([`Action`], [`Plan`], [`Charter`], [`Objective`], etc.)
//!   and the [`DomainModel`] aggregate.
//! - [`reference`]: String-based reference resolution across the domain model
//!   (UUID, short-prefix, alias, and path-style `charter/plan`).
//! - [`config`]: [`WorkspaceConfig`] — semantic settings (tag hierarchies, expansion
//!   counts) passed in by tools; core never reads disk config itself.
//! - [`sync`]: [`DomainSyncDecision`] for CRDT merge/save orchestration.
//! Graph execution and linked-data export deliberately live in the separate
//! `clearhead-graphd` crate, which consumes this crate's domain/workspace API.
//! - [`telemetry`]: Structured event emission for action lifecycle observability.
//! - [`crdt`]: CRDT operations (in progress).

pub mod workspace;
pub use workspace::store::{ManifestSourceType, WorkspaceManifestEntry};

pub mod config;
pub use config::WorkspaceConfig;
#[doc(inline)]
pub use workspace::{
    Action, ActionList, AppliedSync, Diff, Workspace, ICSPlan, MarkdownCharter, OutputFormat,
    ParseFailure, ParseMode, ParseOutcome, ParsedDocument, RecoveryReport, SourceMetadata,
    SourceRange, collect_plan_files, collect_plan_files_with_plans,
    collect_workspace_manifest, completed_actions_path, diff_actions, format, format_charter,
    implicit_charter, infer_charter_name, infer_parent_charter_name,
    infer_plan_charter_name, infer_plan_parent, action_mirror_path, charter_plans_dir_relative,
    plan_file_name, plan_output_path,
    expand_plans_into_actions, upcoming_actions_path, list_action_files, load_domain_model,
    load_domain_model_with_plans, load_workspace, load_workspace_with_plans, load_workspaces,
    action_to_vevent, actions_to_icalendar, occurrence_action_id, parse_actions,
    parse_actions_with_mode, parse_charter, parse_document, parse_tree, patch_action_list,
    charter_root, plans_root, read_action_file, read_actions, save_domain_model,
    apply_sync, plan_sync, read_ics_dates, reconcile,
    workspace_data_root, write_actions,
    ArchiveCharterError, ArchiveCharterOptions, ArchiveCharterResult,
    archive_charter, archive_terminal_charters, find_markdown_charter,
};
pub use workspace::calendar::expand::{ExpandResult, ExpansionConfig};

pub mod domain;
#[doc(inline)]
pub use domain::diff::{
    ActionDiff, ActionFieldChange, CharterDiff, CharterFieldChange, DomainDiff, PlanDiff,
    PlanFieldChange, diff_domain_models,
};
#[doc(inline)]
pub use domain::{ActionState, PredecessorRef, Charter, CharterState, DomainModel, Metric, Objective, Plan, Recurrence, WorkspaceCharter, close_subtree, collect_subtree_ids};
pub use workspace::{Reconcile, SyncEntry, SyncReport, SyncTally};
#[doc(inline)]
pub use domain::filter::{ActionFilter, apply_filter};

pub mod crdt;

pub mod sync;
#[doc(inline)]
pub use sync::DomainSyncDecision;


pub mod reference;
#[doc(inline)]
pub use reference::{
    MatchMode, ReferenceError, ReferenceOptions, ReferenceTarget, filter_model_for_action,
    filter_model_for_charter, filter_model_for_plan, resolve_reference,
    resolve_reference_in_workspaces,
};

pub mod telemetry;
#[doc(inline)]
pub use telemetry::{
    NoopEmitter, TelemetryEmitter, TelemetryEvent, TelemetryRecord, Tool, event_from_field_change,
    event_from_state_change, noop_emitter,
};
