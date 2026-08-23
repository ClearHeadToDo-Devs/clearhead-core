# clearhead-rs

The Rust codebase for the [ClearHead](https://github.com/ClearHeadToDo-Devs)
platform: a single Cargo workspace holding the pure domain core, the native
filesystem delivery adapter, and the user-facing hosts.

> The repository is named `clearhead-core` for historical reasons; the Cargo
> workspace it contains is `clearhead-rs`. (A virtual workspace has no Cargo
> `name` field, so that identity lives here and in the root `Cargo.toml`
> metadata.)

## Architecture in one paragraph

The organising principle is a single seam: **a pure domain core that decides,
and host adapters that deliver.** `clearhead_core` owns the model and the
algorithms and *decides* what a mutation should change, but performs no I/O; it
runs natively and on `wasm32`. A *delivery adapter* turns those decisions into
real reads and durable writes. `clearhead-workspace-fs` is the native
filesystem adapter; the CLI and LSP compose the two. The full account —
including the resource/effect boundary — is in
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md).

## Workspace members

| Crate | Role |
|---|---|
| [`crates/clearhead-core`](crates/clearhead-core) | pure domain library — model, algorithms, and the host-neutral effect protocol (no I/O) |
| [`crates/clearhead-workspace-fs`](crates/clearhead-workspace-fs) | native filesystem delivery adapter — loading, durable writes, calendar sync |
| [`crates/clearhead-cli`](crates/clearhead-cli) | the `clearhead` command-line client |
| [`crates/clearhead-lsp`](crates/clearhead-lsp) | the standalone editor protocol server |

The dependency arrows all point *into* core: the adapter and hosts depend on
`clearhead_core`, never the reverse. That is why core is a plain member under
`crates/` rather than the workspace root — it must not name or "parent" its own
downstream.

## Building

Members use ordinary sibling path dependencies, so this checkout builds and
tests standalone:

```bash
cargo build --workspace
cargo test  --workspace
```

Select a single crate with `-p`, e.g. `cargo test -p clearhead_core`.

When developing inside the ClearHead platform super-repo, a `.cargo/config.toml`
`[patch]` redirects the tree-sitter grammar dependency to the adjacent submodule
checkout; a standalone clone builds the pinned grammar from Git instead.

## License

MIT License — see [LICENSE](LICENSE).
