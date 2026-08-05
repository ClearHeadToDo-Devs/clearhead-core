# ClearHead Core

Shared rust library that implements many of the clearhead specifications in a way that can be leveraged by several clients without coordination

## Overview

ClearHead Core is the foundational library for the ClearHead ecosystem. It provides all the business logic for managing actions, plans, charters, and objectives.

Currently, we only support the workspace backend but there is potential to implement other backends if people wanted to try an entirely `sqlite` backend in the form of a feature flag

## Features

- Domain model structs and logic
  - these serve as the fundamental units handling the various entities in a strongly-typed fashion
- workspace loading
  - the process of loading said domain model using entirely files on disk and a dedicated workspace model
- config reading and layering
  - this includes environment variables

This makes it suitable for use in:

- CLI tools (like [clearhead-cli](https://github.com/ClearHeadToDo-Devs/clearhead-cli))
- LSP servers
- Web services and APIs
- WebAssembly (WASM) environments
- Embedded systems
- Any Rust project needing action/plan management

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
clearhead_core = "0.2.0"
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Related Projects

- [clearhead-cli](https://github.com/ClearHeadToDo-Devs/clearhead-cli) - CLI and LSP server
- [tree-sitter-actions](https://github.com/ClearHeadToDo-Devs/tree-sitter-actions) - Parser for .actions DSL
- [ontology](https://github.com/ClearHeadToDo-Devs/ontology) - BFO/CCO-aligned ontology
- [specifications](https://github.com/ClearHeadToDo-Devs/specifications) - File format specs

## Links

- [Repository](https://github.com/ClearHeadToDo-Devs/clearhead-core)
- [Crates.io](https://crates.io/crates/clearhead_core) (coming soon)
- [Documentation](https://docs.rs/clearhead_core) (coming soon)
