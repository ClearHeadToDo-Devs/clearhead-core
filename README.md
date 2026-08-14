# clearhead-lsp

Standalone Language Server Protocol runtime for ClearHead.

The server speaks standard LSP JSON-RPC over stdio:

```sh
clearhead-lsp
```

`clearhead-lsp` owns the asynchronous editor runtime and depends directly on `clearhead-core` for parsing and domain/workspace behavior. It does not depend on `clearhead-cli`.

The protocol runtime has moved here from `clearhead-cli`. This crate now owns Tokio, Tower LSP, DashMap, tree-sitter document state, stdio startup, workspace routing, diagnostics, code actions, completion, inlay hints, semantic tokens, definition, references, formatting, protocol conversions, provider tests, and its NDJSON telemetry adapter.

Archive mutations are intentionally absent from the LSP surface. Editor clients save their buffers, invoke the CLI's durable workspace operation, and reload or close the affected buffer only after success.

## Ownership and releases

The LSP is released independently from `clearhead-cli` at `ClearHeadToDo-Devs/clearhead-lsp`. Its public process contract is standard LSP over stdio; provider changes and async-runtime upgrades follow this repository's own release history.

## Development

Sibling checkouts are used for path development:

```sh
cargo test --manifest-path clearhead-lsp/Cargo.toml
```
