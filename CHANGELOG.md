# Changelog

All notable changes to clearhead-core will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/ClearHeadToDo-Devs/clearhead-core/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/ClearHeadToDo-Devs/clearhead-core/releases/tag/v0.1.0
