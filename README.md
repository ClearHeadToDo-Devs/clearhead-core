# ClearHead Core

Pure Rust library providing the domain model and business logic for the ClearHead personal data platform.

## Overview

ClearHead Core is the foundational library for the ClearHead ecosystem. It provides all the business logic for managing actions, plans, and workflows without making any assumptions about the environment it runs in.

## Features

- **Domain Models**: Plans, PlannedActs (action processes), Actions, and Act phases
- **Tree-sitter Parser Integration**: Parse `.actions` DSL files into structured data
- **Format Conversions**: Convert between DSL, JSON, calendar, and table formats
- **Integration Surface**: Serde domain/workspace types consumed by external integrations such as `clearhead-graphd`
- **Document Pipeline**: Save orchestration with diff detection and sync decisions
- **Validation & Linting**: Comprehensive linting rules for action files
- **Diff & Sync Logic**: Semantic comparison and merge algorithms

## Design Philosophy

ClearHead Core is **environment-agnostic** by design. It contains:

- Pure business logic  
- Domain models and algorithms  
- Data transformations  
- Validation rules  

- No filesystem access  
-No network operations  
-No configuration management  
-No environment variables  

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
clearhead_core = "0.1.0"
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
