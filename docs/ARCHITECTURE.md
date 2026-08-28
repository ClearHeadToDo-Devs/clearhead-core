# ClearHead Core architecture

This document describes the internal architecture of the `clearhead_core` crate.
For platform-level systems, executable composition, external tools, and durable
stores, see the platform's [Structurizr workspace][Structurizr]. For runtime
event order, see the platform [workflow diagrams][Workflows].

## Boundary

Core is a host-neutral decision library. It owns the domain model, semantic
configuration, representation codecs, reference resolution, publication, and
the decisions behind workspace mutations and calendar reconciliation. It does
not discover files, inspect directories, read environment variables, acquire
locks, recover journals, or deliver effects.

The governing rule is:

> **Core decides; adapters observe and deliver.**

A host supplies explicit evidence:

- resource inventories and immutable byte snapshots;
- resource revisions and expected-presence facts;
- workspace scope and mount identity;
- typed mutation or synchronization requests.

Core parses that evidence, applies policy, and returns typed outcomes plus an
`EffectBatch`. An adapter validates the preconditions and delivers the effects.
A `PreparedMutation` remains speculative until the adapter reports successful
delivery; conflicts or delivery failures discard it.

The native implementation of observation and delivery is the sibling
`clearhead-workspace-fs` crate. CLI, LSP, Neovim, operating-system paths,
calendar networking, and query-engine hosting are outside this crate.

## Internal organization

| Responsibility | Primary source |
|---|---|
| Entities, lifecycle, diff, filter, and update semantics | `src/domain/` |
| UUID, prefix, alias, and path reference policy | `src/reference.rs` |
| Host-neutral semantic configuration | `src/config.rs` |
| `.actions` parsing, trust, linting, formatting, and patching | `src/workspace/actions/` |
| Charter, sidecar, template, and archive representations | `src/workspace/` |
| Recurrence, VTODO codecs, reconciliation, and deviations | `src/workspace/calendar/` |
| Snapshot-to-workspace assembly and diagnosis | `src/workspace/store/` |
| Logical resources, revisions, preconditions, and effects | `src/workspace/resource.rs` |
| Multi-operation transaction planning | `src/workspace/transaction.rs` |
| Deterministic RDF dataset projection and serialization | `src/rdf/` |
| Host-neutral telemetry records | `src/telemetry.rs` |

These are ownership roots, not an exhaustive file index. Module declarations
and generated Rust documentation are the detailed source map.

## Domain and representations

`DomainModel` is Core's canonical in-memory semantic interpretation. Durable
authority remains outside Core in the plaintext workspace observed by a host.
Those statements are complementary: the workspace owns persisted facts, while
the typed model owns their meaning during computation.

Core maps between the model and several representations:

- `.actions` source for executable Action facts;
- charter Markdown and JSON sidecar content;
- iCalendar components for Plan and Action interoperability;
- deterministic RDF quads and serializations for publication.

A representation is not automatically authoritative. In particular, RDF is a
replaceable publication, while calendar resources participate through explicit
reconciliation policy. Normative format and workspace behavior belongs in the
[specifications][Specifications], not in this implementation document.

## Resource/effect protocol

Core and delivery adapters communicate through
`clearhead_core::workspace::resource`:

- `WorkspacePath` and `ResourceLocation` identify logical resources without
  probing a host filesystem.
- `ResourceSnapshot` contains bytes already observed by a host.
- `ResourceRevision` is opaque concurrency evidence.
- `ResourcePrecondition` records the revision or absence expected at delivery.
- `Effect` describes a logical write, remove, or move.
- `EffectBatch` groups effects and validates their internal consistency.
- `PreparedMutation` pairs speculative next state with effects and adopts the
  outcome only after successful delivery.

Path values may occur as inert source-location or representation data. Calling
filesystem APIs on those values is an adapter responsibility.

## Dependency direction

`clearhead_core` must not depend on the native adapter or either executable.
The intended dependency direction is:

```text
clearhead-cli ───────────────┐
clearhead-lsp ───────────────┼─> clearhead-workspace-fs ─> clearhead_core
other host adapter ──────────┘                           └> clearhead_core
```

Hosts may also invoke Core directly for pure calculations. Core must never name
or invoke those downstream hosts.

## Fitness checks

The boundary is protected by repository gates:

- `scripts/pure-core-source-gate.sh` rejects production filesystem observation
  or delivery APIs in Core source;
- `scripts/wasm-dependency-gate.sh` rejects native-only dependencies from
  Core's portable dependency graph;
- the workspace build checks Core without default features;
- tests exercise decisions from supplied snapshots and effects rather than
  requiring native delivery.

When new behavior needs filesystem state, the adapter should observe that state
and pass a typed fact into Core. Do not add a convenient probe to Core and treat
it as an exception to the boundary.

[Specifications]: https://github.com/ClearHeadToDo-Devs/specifications
[Structurizr]: https://github.com/ClearHeadToDo-Devs/platform/tree/main/structurizr
[Workflows]: https://github.com/ClearHeadToDo-Devs/platform/blob/main/docs/workflows.md
