# Changelog

All notable changes to clearhead-core will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `rdf::app::project_app`: the application graph (`app:` vocabulary, specifications `ontology.md`, Decision 45), projected from a `DomainModel`, host-supplied `app::Locations` (`Locations::of(&Workspace)`), the workspace config and the viewer's zone. Written times are as written; `notBefore`, `lateFrom` and `durationMinutes` are derived in the zone. The graph fixture's `expected-app.ttl` is its conformance test. The CLI's queries read it.
- `Bound::xsd`, `Bound::end_instant_in`, `Planned::duration_in`: the as-written XSD form and zone-aware instants.
- `app:plannedFrom`, the planned start's instant in the viewer's zone, so queries compare `@` without reading the written value. The Turtle, TriG and JSON-LD serializers declare the `app:` and `skos:` prefixes.
- `WorkspaceRead::objective_files` and `Workspace::objective_files`: each objective's data-root-relative file. `Workspace::with_objectives` takes them.

### Changed
- `Action::due_date` is a `domain::time::Due` window (`:end` or `:start/end`, platform Decision 48), not a `DateTime`. Each `Bound` keeps the precision it was written at, so a date-only deadline is written back as a date instead of `T00:00`. `Due::late_from` and `Due::not_before` give the half-open window's instants. Calendar sync sees only the deadline; the window's start is never placed.
- `parse_iso8601_datetime` moved from `workspace::actions::parser` to `domain::time`.
- `Bound` is its written form (platform Decision 52): `local`, the written `offset` if any, and `precision`. The `at` field is now `at()`, resolved in the local zone, or `at_in(zone)`; `resolve_local` resolves a local time as RFC 5545 does (first occurrence when it occurs twice, the offset before the gap when it does not occur). Text round-trips exactly, so a written offset is no longer rewritten as local time, and a time with an offset but no seconds, or in a spring-forward gap, no longer fails to parse and drops its field.

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
