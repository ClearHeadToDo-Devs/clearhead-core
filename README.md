# ClearHead Core

Shared Rust library implementing ClearHead specifications for reuse by clients
without requiring implementation coordination.

## Overview

ClearHead Core is the foundational library for the ClearHead ecosystem. It
provides shared logic for managing actions, plans, charters, and objectives.

The workspace backend is currently the only implementation. Future backends,
such as SQLite, can be introduced as explicit capabilities.

## Features

- Domain model structs and logic
  - strongly typed representations of the framework's entities
- workspace loading
  - domain-model loading from the canonical plaintext workspace
- config reading and layering
  - this includes environment variables

This makes it suitable for use in:

- CLI tools such as
  [clearhead-cli](https://github.com/ClearHeadToDo-Devs/clearhead-cli)
- LSP servers
- Web services and APIs
- WebAssembly (WASM) environments
- Embedded systems
- Any Rust project needing action/plan management

## Cargo features

The default `formatting` feature provides canonical `.actions` source formatting
through Topiary. Clients that write or format action files, including the CLI
and LSP, should enable it explicitly. Read-only consumers can disable default
features to avoid Topiary and its async runtime dependency closure:

```toml
clearhead_core = { version = "0.2.0", default-features = false }
```

Without `formatting`, parsing, workspace loading, and domain operations remain
available, while attempts to format `.actions` source return a capability
error. JSON and XML serialization do not require the feature.

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
- [tree-sitter-actions](https://github.com/ClearHeadToDo-Devs/tree-sitter-actions)
  — `.actions` parser
- [ontology](https://github.com/ClearHeadToDo-Devs/ontology) — BFO/CCO-aligned
  ontology
- [specifications](https://github.com/ClearHeadToDo-Devs/specifications) — file
  format specifications

## Links

- [Repository](https://github.com/ClearHeadToDo-Devs/clearhead-core)
- [Crates.io](https://crates.io/crates/clearhead_core) (coming soon)
- [Documentation](https://docs.rs/clearhead_core) (coming soon)
