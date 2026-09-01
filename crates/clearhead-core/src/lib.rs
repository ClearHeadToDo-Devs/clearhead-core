//! ClearHead Core Library
//!
//! Pure domain library for the ClearHead framework, aligned with the Actions
//! Vocabulary v4 ontology. Core holds the in-memory model and the algorithms and
//! *decides* what a workspace mutation should do, but it performs no I/O: reading
//! bytes off disk and durably writing them is the job of a delivery adapter (the
//! native one is `clearhead-workspace-fs`). The [`domain`] module contains the
//! model and pure algorithms; [`workspace`] defines the DSL projection and the
//! host-neutral delivery protocol; [`config`] defines the shared semantic config
//! schema (a delivery adapter resolves the actual files and environment). This
//! keeps one dialect of layout and configuration across the CLI, LSP, and any
//! other host — native or WebAssembly. Network transport and client-specific
//! user interfaces remain outside this crate.
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
//! - [`workspace`]: DSL projection and delivery protocol — `.actions`
//!   parsing/formatting, recurrence expansion, mutation planning, and the
//!   host-neutral resource/effect contract adapters execute. Loading bytes from
//!   disk lives in a delivery adapter, not here.
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

pub mod config;
pub use config::{PlanComponentKind, WorkspaceConfig};
pub use workspace::calendar::expand::render_occurrences;
#[doc(inline)]
pub use workspace::transaction::{TransactionOutcome, TransactionRequest};
#[doc(inline)]
pub use workspace::{
    Action, ActionArchivePlan, ActionList, ActionPlanLink, ActionSelector, AppliedMutation,
    AppliedSync, ArchivePolicyError, ArchivedActionFact, CalendarSyncPreparationInput,
    CalendarSyncState, DeliveryError, Diff, Effect, EffectBatch, EffectBatchError,
    ExpectedResource, ICSPlan, IntegrityError, MarkdownCharter, OccurrenceActionFields,
    OccurrenceOp, OccurrenceSnapshot, OutcomeKind, OutputFormat, ParseFailure, ParseMode,
    ParseOutcome, ParsedDocument, PlanActionProjection, PreparedMutation, ReadPlan, RecoveryReport,
    ResourceConflict, ResourcePrecondition, ResourceRevision, ResourceSnapshot, SnapshotError,
    SourceMetadata, SourceRange, SyncActionResourceState, SyncCodecMigration,
    SyncConflictResolution, SyncField, SyncImport, SyncLifecycleEntry, SyncLifecycleKind,
    SyncMirrorResourceState, SyncPlanTemplate, TrustedDocument, Workspace, WorkspaceInventory,
    WorkspacePath, WorkspacePathError, WorkspaceSnapshot, action_mirror_path, action_to_vtodo,
    actions_to_icalendar, archive_charter_name, archive_charter_subtree, canonical_occurrence_key,
    charter_collection_from_anchor, charter_plans_dir_relative, completed_actions_path,
    append_log_entry, diff_actions, format, format_charter, format_trusted_source,
    has_terminal_archive_ancestor,
    implicit_charter, infer_charter_name, infer_parent_charter_name, infer_plan_charter_name,
    infer_plan_parent, occurrence_action_id, parse_actions, parse_actions_with_mode, parse_charter,
    parse_document, parse_tree, parse_trusted_document, patch_action_list, plan_action_archive,
    plan_file_name, plan_one_off_sync, plan_output_path, plan_recurring_occurrence_sync,
    plan_to_vevent, plan_to_vtodo, plans_to_icalendar, plans_to_icalendar_with_component,
    prepare_sync, reconcile, render_master_rollforward, render_occurrence_action,
    render_occurrence_deviation, render_plan_resource_with_component, slugify,
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
    Recurrence, WorkspaceCharter, close_subtree, collect_subtree_ids, reopen_subtree,
};
pub use workspace::{
    PlansSyncStore, Reconcile, SyncEntry, SyncReport, SyncTally, decode_plans_sync_store,
    encode_plans_sync_store,
};

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
