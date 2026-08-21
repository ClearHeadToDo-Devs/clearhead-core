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

The LSP is a member of the `clearhead-core` repository's Cargo workspace at `crates/clearhead-lsp`, versioned and built independently of the other members. Its public process contract is standard LSP over stdio; provider changes and async-runtime upgrades follow its own release history.

## Dependencies and builds

`clearhead-core` is an ordinary sibling path dependency, so a clone of the
workspace repository builds and tests without any other checkout:

```sh
git clone https://github.com/ClearHeadToDo-Devs/clearhead-core.git
cd clearhead-core
cargo test -p clearhead-lsp
```

The `tree-sitter-actions` grammar is declared from its canonical Git
repository, pinned to a specific revision. Bump the `rev` field in
`Cargo.toml` to consume a newer published grammar; promote the pin to a
version tag as a release step. Inside the platform workspace, the super-repo's
`.cargo/config.toml` carries a `[patch]` table that redirects that Git
dependency to the adjacent submodule checkout, so local grammar edits
propagate and `scripts/validate-pinned` verifies the exact pinned composition.
Cargo discovers that file by walking up from the working directory, so it is
active for any build inside the platform tree and inert for a standalone
clone.
