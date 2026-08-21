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

## Dependencies and builds

`clearhead-core` and `tree-sitter-actions` are declared from their canonical
Git repositories, so a standalone clone of this repository builds and tests
without any sibling checkout:

```sh
git clone https://github.com/ClearHeadToDo-Devs/clearhead-lsp.git
cd clearhead-lsp
cargo test
```

Both dependencies are pinned to a specific revision. Bump the `rev` fields in
`Cargo.toml` to consume a newer published `clearhead-core` or
`tree-sitter-actions`; promote those pins to version tags as a release step.

### Local development with sibling checkouts

Inside the platform workspace, the super-repo's `.cargo/config.toml` carries a
`[patch]` table that redirects those Git dependencies back to the adjacent
submodule checkouts, so local edits to `clearhead-core` and
`tree-sitter-actions` propagate and `scripts/validate-pinned` verifies the exact
pinned submodule composition rather than a fetched revision. Cargo discovers
that file by walking up from the working directory, so it is active for any
build inside the platform tree and inert for a standalone clone.

```sh
cargo test --manifest-path clearhead-lsp/Cargo.toml
```
