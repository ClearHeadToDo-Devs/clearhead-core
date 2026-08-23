# ClearHead Core

Shared Rust library implementing ClearHead specifications for reuse by clients without requiring implementation coordination.

## Overview

ClearHead Core is the foundational library for the ClearHead ecosystem. It
provides the shared domain model and logic for managing actions, plans, charters,
and objectives.

Core is a **pure domain library**: it holds the in-memory model and the
algorithms, and it *decides* what a workspace mutation should do — but it
performs no I/O. Reading and durably writing files is the job of a *delivery
adapter*. The native adapter, `clearhead-workspace-fs`, implements that boundary
against a POSIX filesystem; a different host (for example a WASM runtime) can
supply its own adapter without Core changing. See
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) for the full seam.

## Repository layout

This repository is a single Cargo workspace. The root package is
`clearhead_core` (this pure library); the delivery adapter and native host
binaries live in `crates/`:

- `crates/clearhead-workspace-fs` — native filesystem delivery adapter (the I/O
  half of the seam; loading, durable writes, calendar sync)
- `crates/clearhead-cli` — the `clearhead` command-line client
- `crates/clearhead-lsp` — the editor protocol server

Members use ordinary sibling path dependencies, so this checkout builds and
tests standalone: `cargo test --workspace`.

## Features

- Domain model structs and logic
  - strongly typed representations of the framework's entities
- pure algorithms over the model
  - `.actions` parsing/formatting, diff, filter, recurrence expansion,
    reference resolution, and the RDF publication (a database-free quad
    projection)
- the host-neutral delivery protocol
  - logical resource paths, snapshots, and the `EffectBatch` a host executes —
    the contract every delivery adapter implements
- the shared semantic config schema (`WorkspaceConfig`)
  - Core defines the settings and precedence; a delivery adapter (e.g.
    `clearhead-workspace-fs`) reads the actual files and environment

Loading the workspace from disk and durably persisting mutations are **not**
Core's responsibility — they belong to a delivery adapter. This makes Core
suitable for use in:

- CLI tools such as [clearhead-cli](https://github.com/ClearHeadToDo-Devs/clearhead-core/tree/main/crates/clearhead-cli)
- LSP servers
- Web services and APIs
- WebAssembly (WASM) environments
- Embedded systems
- Any Rust project needing action/plan management

## Cargo features

The default `formatting` feature provides canonical `.actions` source formatting through Topiary. Clients that write or format action files, including the CLI and LSP, should enable it explicitly. Read-only consumers can disable default features to avoid Topiary and its async runtime dependency closure:

```toml
clearhead_core = { version = "0.2.0", default-features = false }
```

Without `formatting`, parsing, workspace loading, and domain operations remain available, while attempts to format `.actions` source return a capability error. JSON and XML serialization do not require the feature.

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
clearhead_core = { version = "0.2.0", features = ["formatting"] }
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Related Projects

- [clearhead-cli](https://github.com/ClearHeadToDo-Devs/clearhead-cli) — CLI client
- [clearhead-lsp](https://github.com/ClearHeadToDo-Devs/clearhead-lsp) — LSP server
- [tree-sitter-actions](https://github.com/ClearHeadToDo-Devs/tree-sitter-actions) — `.actions` parser
- [ontology](https://github.com/ClearHeadToDo-Devs/ontology) — BFO/CCO-aligned ontology
- [specifications](https://github.com/ClearHeadToDo-Devs/specifications) — file format specifications

## Links

- [Repository](https://github.com/ClearHeadToDo-Devs/clearhead-core)
- [Crates.io](https://crates.io/crates/clearhead_core) (coming soon)
- [Documentation](https://docs.rs/clearhead_core) (coming soon)
