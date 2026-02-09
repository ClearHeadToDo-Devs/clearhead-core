# ClearHead Core

Pure Rust library providing the domain model and business logic for the ClearHead personal data platform.

## Overview

ClearHead Core is the foundational library for the ClearHead ecosystem. It provides all the business logic for managing actions, plans, and workflows without making any assumptions about the environment it runs in.

## Features

- **Domain Models**: Plans, PlannedActs (action processes), Actions, and Act phases
- **Tree-sitter Parser Integration**: Parse `.actions` DSL files into structured data
- **Format Conversions**: Convert between DSL, JSON, RDF/TTL, and table formats
- **CRDT Synchronization**: Conflict-free replicated data types for distributed sync
- **SPARQL Query Engine**: Semantic queries over action data using RDF graphs
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

### Basic Example

```rust
use clearhead_core::{parse_actions, format, OutputFormat};

fn main() -> Result<(), String> {
    // Parse an actions DSL string
    let dsl = r#"
    - [ ] Write documentation @docs
    - [x] Implement parser @dev
    "#;
    
    let actions = parse_actions(dsl)?;
    
    // Convert to JSON
    let json = format(&actions, OutputFormat::Json, None)?;
    println!("{}", json);
    
    Ok(())
}
```

### SPARQL Queries

```rust
use clearhead_core::{parse_actions, graph};

fn main() -> Result<(), String> {
    let actions = parse_actions("- [ ] Task @work")?;
    
    // Load into RDF store
    let store = graph::create_database()?;
    graph::load_actions(&store, &actions)?;
    
    // Query with SPARQL
    let sparql = r#"
        SELECT ?id WHERE {
            ?action <http://clearhead.invalid/has_tag> "work" .
            ?action <http://clearhead.invalid/id> ?id .
        }
    "#;
    
    let result\s = graph::query_actions(&store, sparql)?;
    println!("Matching actions: {:?}", results);
    
    Ok(())
}
```

## Architecture

### Module Structure

```
clearhead_core/
├── entities.rs       - Core domain types (Action, ActionList, ActionState)
├── domain.rs         - Higher-level models (Plan, PlannedAct, ActPhase)
├── treesitter.rs     - Tree-sitter parsing integration
├── format.rs         - Format conversions (DSL, JSON, TTL, Table)
├── diff.rs           - Diffing algorithms for action lists
├── sync.rs           - Sync decision logic and conflict resolution
├── document.rs       - Document save pipeline orchestration
├── crdt.rs           - CRDT operations for distributed sync
├── graph.rs          - RDF/SPARQL integration
├── lint.rs           - Validation and linting rules
└── sync_utils.rs     - Utility functions for synchronization
```

### Data Flow

```mmd
graph TD
    DSL[Actions DSL] -->|parse file| actions[ActionList]
    actions -->|Transform| domain[Domain Structs]
    domain -->|autosurgeon| crdt[CRDT State]
    domain -->|oxigraph| graph[Graph Store]
    domain -->|format| output[Formatted Output]
```

### Core Types

- **`Action`**: Individual task or plan with state, tags, dependencies
- **`ActionList`**: Collection of actions (alias for `Vec<Action>`)
- **`Plan`**: Template for an action (what it is)
- **`PlannedAct`**: Execution of a plan (when/how it happened)
- **`Charter`**: Scope of concern for a set of plans
- **`Objective`**: Desired outcome that plans serve
- **`ActionState`**: State enum (Todo, InProgress, Done, Cancelled, etc.)

## Boundary Principles

To maintain environment-agnostic design, core follows these rules:

1. **No I/O**: All functions take strings or data structures as input
2. **No side effects**: Functions are pure where possible
3. **No assumptions**: Don't assume filesystem paths, URLs, or system calls exist

For environment integration (filesystem, config, network), use [clearhead-cli](https://github.com/ClearHeadToDo-Devs/clearhead-cli) or build your own wrapper.

## Testing

Run tests:

```bash
cargo test
```

Run with verbose output:
c
```bash
cargo test -- --nocapture
```

## Contributing

Contributions are welcome! Please ensure:

1. All tests pass (`cargo test`)
2. Code is formatted (`cargo fmt`)
3. No clippy warnings (`cargo clippy`)
4. No environment dependencies (filesystem, network, config)

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
