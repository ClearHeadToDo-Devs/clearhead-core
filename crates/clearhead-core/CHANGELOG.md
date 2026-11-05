# Changelog

All notable changes to clearhead-core will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.0] - 2026-11-05

Included in the clearhead 0.4.0 binary release. Declares specification v0.5.0 and pins registry tree-sitter-actions `=0.11.1`; schema URLs move to `v0.5.0`. **v0.5.0 Habits are only partly implemented:** the domain and graph below, with no habits-file reader, occurrence generation or calendar migration yet.

### Added
- `domain::Habit` and `NativeOccurrence`: native Habit definitions using the Action state vocabulary, a required rule and recurrence anchor, closed dates only on terminal states, and separate template children; canonical Habit JSON export. The application graph projects them with `app:occurrenceOf` and canonical STRING slots, independent of transport identifiers.
- `action_lifecycle`: the shared close, reopen and move transitions, with `expand_cascade`, `tree_root`, `prepare_action_move` and `MoveMode`; `prepare_transaction_with_occurrences` and `fold_materialized_occurrence_resolution` for occurrence closure.
- `ActionState::is_terminal`, `VerbError::already_closed` and `require_open_action`; `unique_selector_match` and `unique_selector_match_pair` resolve one selector across the active and completed files.

### Changed
- Closing is a state change in place and archival moves whole root trees (platform Decisions 58 and 59; Core Decision 2 amended for `--cascade`). `plan_action_archive` takes root trees.
- A recurrence (`R:`) in an actions file is an `action-parse-error` for its root tree, so no write drops a rule `Action` cannot hold.

### Removed
- `close_subtree`, `reopen_subtree`, `prepare_close_action_subtree`, `prepare_reopen_action_subtree`, `prepare_action_archive` and their `PreparedCloseOutcome` and `PreparedReopenOutcome`: superseded by `action_lifecycle`.

## [0.3.1] - 2026-10-14

Source snapshot included in the clearhead 0.3.2 binary release; crates.io publication is not part of this delivery.

### Added
- `grammar_version()`: the linked tree-sitter-actions release, read from the parser's ABI 15 metadata (`v0.10.0`), beside `SPECIFICATION`.

### Changed
- Implements specification v0.3.0 (was v0.2.0): `SPECIFICATION` and the workspace and charter-metadata schema URLs move to `v0.3.0`. The schemas differ from v0.2.0 only in their `$id`s. This adopts the existing specification release without adding semantics.

## [0.3.0] - 2026-10-06

The accumulated entries below were already included in the published Core 0.3.0 source.

### Added
- `rdf::app::project_app`: the application graph (`app:` vocabulary, specifications `ontology.md`, Decision 45), projected from a `DomainModel`, host-supplied `app::Locations` (`Locations::of(&Workspace)`), the workspace config and the viewer's zone. Written times are as written; `notBefore`, `lateFrom` and `durationMinutes` are derived in the zone. The graph fixture's `expected-app.ttl` is its conformance test. The CLI's queries read it.
- `Bound::xsd`, `Bound::end_instant_in`, `Planned::duration_in`: the as-written XSD form and zone-aware instants.
- `app:plannedFrom`, the planned start's instant in the viewer's zone, so queries compare `@` without reading the written value. The Turtle, TriG and JSON-LD serializers declare the `app:` and `skos:` prefixes.
- `WorkspaceRead::objective_files` and `Workspace::objective_files`: each objective's data-root-relative file. `Workspace::with_objectives` takes them.

### Changed
- `Action::due_date` is a `domain::time::Due` window (`:end` or `:start/end`, platform Decision 48), not a `DateTime`. Each `Bound` keeps the precision it was written at, so a date-only deadline is written back as a date instead of `T00:00`. `Due::late_from` and `Due::not_before` give the half-open window's instants. Calendar sync sees only the deadline; the window's start is never placed.
- `parse_iso8601_datetime` moved from `workspace::actions::parser` to `domain::time`.
- `Bound` is its written form (platform Decision 52): `local`, the written `offset` if any, and `precision`. The `at` field is now `at()`, resolved in the local zone, or `at_in(zone)`; `resolve_local` resolves a local time as RFC 5545 does (first occurrence when it occurs twice, the offset before the gap when it does not occur). Text round-trips exactly, so a written offset is no longer rewritten as local time, and a time with an offset but no seconds, or in a spring-forward gap, no longer fails to parse and drops its field.

- `Action::created_at` and `completed_at` are `Option<domain::time::Bound>` (Decision 52): `^` and `%` are kept as written, so `format` no longer rewrites a written offset into local time, a date into `T00:00`, or drops seconds. Times the host stamps or imports are `Bound::minute` of the local instant, written as before; `.at()` gives the instant. The canonical JSON and `app:closed`/`dcterms:created` carry the written form.

- `rdf::app::APP_NS` is `https://clearhead.dev/vocab/app/v1#` (platform Decision 49); saved queries declaring the old `app:` IRI must update it.

### Removed
- The v4 projection (`rdf::project_domain`, `rdf::serialize_domain`), the `ws:` workspace-snapshot layer (`rdf::project_workspace_snapshot`, `rdf::WorkspaceSnapshot`) and the v4 namespace constants and fixtures (platform `retire-v4`). The application graph (`rdf::app`) is the only projection; the serializers declare only its prefixes.
- `Metric::review_date`: reviewing a metric is an action (specifications, 2026-10-03). Frontmatter that still has `review_date` loads unchanged; the key is ignored.

## [0.1.0] - 2026-02-01

### Added
- Initial extraction of core business logic from clearhead-cli
- Core domain models: `Action`, `ActionList`, `ActionState`, `Recurrence`
- Higher-level domain models: `Plan`, `PlannedAct`, `ActPhase`, `DomainModel`
- Tree-sitter parsing integration for `.actions` DSL format
- Format conversions: DSL, JSON, RDF/TTL, Table
- CRDT synchronization algorithms for distributed conflict resolution
- SPARQL query engine integration via Oxigraph
- Document save pipeline with diff detection and sync decisions
- Comprehensive linting and validation rules
- Pure business logic with zero environmental dependencies

### Design Principles
- Environment-agnostic: no filesystem, network, or configuration dependencies
- Can run in WASM, embedded systems, web services, or any Rust environment
- Pure functions where possible, explicit parameters over implicit config
- Suitable as a library for multiple frontend implementations

[Unreleased]: https://github.com/ClearHeadToDo-Devs/clearhead-core/compare/v0.1.0...HEAD [0.1.0]: https://github.com/ClearHeadToDo-Devs/clearhead-core/releases/tag/v0.1.0
