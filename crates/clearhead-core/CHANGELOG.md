# Changelog

All notable changes to clearhead-core will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- `Action::due_date` is a `domain::time::Due` window (`:end` or `:start/end`, platform Decision 48), not a `DateTime`. Each `Bound` keeps the precision it was written at, so a date-only deadline is written back as a date instead of `T00:00`. `Due::late_from` and `Due::not_before` give the half-open window's instants. Calendar sync sees only the deadline; the window's start is never placed.
- `parse_iso8601_datetime` moved from `workspace::actions::parser` to `domain::time`.

### Removed
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
