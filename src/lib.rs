//! ClearHead Core Library
//!
//! Shared domain and workspace services for the ClearHead framework, aligned
//! with the Actions Vocabulary v4 ontology. The [`domain`] module contains the
//! in-memory model and pure algorithms; [`workspace`] and [`config`] own the
//! canonical local-file layout, persistence, and configuration semantics used
//! consistently by the CLI, LSP, graphd, and other clients. Network transport
//! and client-specific user interfaces remain outside this crate.
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
//! - [`config`]: Shared semantic settings plus the canonical config source and
//!   precedence stack; clients may extend these settings with tool-specific fields.
//! - [`telemetry`]: Structured event emission for action lifecycle observability.
//!
//! The canonical RDF *publication* of the domain model lives in [`rdf`] — a
//! database-free projection to quads and their serializations, depending only on
//! `oxrdf` and `oxttl`. SPARQL evaluation and any Oxigraph store deliberately
//! stay out of this crate (they belong to the optional CLI `sparql` feature).

pub mod workspace;
pub use workspace::store::{ManifestSourceType, WorkspaceManifestEntry};

pub mod config;
pub use config::WorkspaceConfig;
pub use workspace::calendar::expand::render_occurrences;
#[doc(inline)]
pub use workspace::transaction::{TransactionOutcome, TransactionRequest};
#[doc(inline)]
pub use workspace::{
    Action, ActionArchivePlan, ActionList, ActionSelector, AppliedMutation, AppliedSync,
    ArchivePolicyError, ArchivedActionFact, CalendarSyncPreparationInput, CalendarSyncState,
    DeliveryError, Diff, Effect, EffectBatch, EffectBatchError, ExpectedResource, ICSPlan,
    IntegrityError, MarkdownCharter, OccurrenceOp, OccurrenceSnapshot, OutcomeKind, OutputFormat,
    ParseFailure, ParseMode, ParseOutcome, ParsedDocument, PreparedMutation, ReadPlan,
    RecoveryReport, ResourceConflict, ResourcePrecondition, ResourceRevision, ResourceSnapshot,
    SnapshotError, SourceMetadata, SourceRange, SyncActionResourceState, SyncConflictResolution,
    SyncField, SyncImport, SyncMirrorResourceState, SyncPlanTemplate, TrustedDocument, VTodoAction,
    VTodoResource, Workspace, WorkspaceInventory, WorkspacePath, WorkspacePathError,
    WorkspaceSnapshot, action_id_from_vtodo_uid, action_mirror_path, action_to_vtodo,
    actions_to_icalendar, apply_occurrence_op, archive_charter_name, archive_charter_subtree,
    canonical_occurrence_key, charter_collection_from_anchor, charter_plans_dir_relative,
    charter_root, collect_plan_files, collect_plan_files_with_plans, collect_workspace_manifest,
    completed_actions_path, diff_actions, format, format_charter, format_trusted_source,
    has_terminal_archive_ancestor, implicit_charter, infer_charter_name, infer_parent_charter_name,
    infer_plan_charter_name, infer_plan_parent, list_action_files, load_domain_model,
    load_domain_model_with_plans, load_workspace, load_workspace_with_plans, load_workspaces,
    occurrence_action_id, parse_actions, parse_actions_with_mode, parse_charter, parse_document,
    parse_tree, parse_trusted_document, parse_vtodo_actions, patch_action_list,
    plan_action_archive, plan_file_name, plan_output_path, plan_sync, plan_to_vtodo, plans_root,
    plans_sync_store_path, plans_to_icalendar, prepare_sync, read_action_file, read_actions,
    read_ics_dates, read_plans_sync_store, read_vtodo_actions, reconcile,
    resolve_materialized_occurrence, resolve_template, save_domain_model, slugify,
    sync_master_rollforwards, workspace_data_root, write_actions, write_master_rollforward,
    write_occurrence_deviation,
};

pub mod domain;
#[doc(inline)]
pub use domain::diff::{
    ActionDiff, ActionFieldChange, CharterDiff, CharterFieldChange, DomainDiff, PlanDiff,
    PlanFieldChange, diff_domain_models,
};
#[doc(inline)]
pub use domain::filter::{ActionFilter, apply_filter};
#[doc(inline)]
pub use domain::update::{
    ActionUpdate, CharterUpdate, apply_charter_update, apply_updates, disallowed_terminal_update,
};
#[doc(inline)]
pub use domain::{
    ActionState, Charter, CharterState, DomainModel, Metric, Objective, Plan, PredecessorRef,
    Recurrence, WorkspaceCharter, close_subtree, collect_subtree_ids,
};
pub use workspace::{PlansSyncStore, Reconcile, SyncEntry, SyncReport, SyncTally};

/// Canonical JSON projection of domain actions, matching the specifications
/// repo's `schemas/actions.schema.json`.
pub mod schema_export;

pub mod verb_result;
#[doc(inline)]
pub use verb_result::{VerbError, VerbOutcome, canonical_id};

pub mod reference;
#[doc(inline)]
pub use reference::{
    MatchMode, ReferenceEntity, ReferenceError, ReferenceErrorKind, ReferenceMatch,
    ReferenceOptions, ReferenceSelection, ReferenceTarget, filter_model_for_action,
    filter_model_for_charter, filter_model_for_plan, match_entity_reference, match_uuid_reference,
    resolve_reference, resolve_reference_in_workspaces, select_reference, select_reference_where,
};

/// Canonical RDF publication of the domain model: `DomainModel` → quads and
/// their TriG / N-Quads / Turtle / JSON-LD serializations. Database-free.
pub mod rdf;

pub mod telemetry;
#[doc(inline)]
pub use telemetry::{
    NoopEmitter, TelemetryEmitter, TelemetryEvent, TelemetryRecord, Tool, event_from_field_change,
    event_from_state_change, noop_emitter,
};
