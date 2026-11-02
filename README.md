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
real reads and durable writes. The native filesystem adapter and both hosts —
the CLI and the LSP — live in `clearhead_cli` (its `filesystem`, `cli`, and
`lsp` modules). The full account — including the resource/effect boundary — is in
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md).

## Workspace members

| Crate | Role |
|---|---|
| [`crates/clearhead-core`](crates/clearhead-core) | pure domain library — model, algorithms, and the host-neutral effect protocol (no I/O) |
| [`crates/clearhead-cli`](crates/clearhead-cli) | the `clearhead` client and the effectful runtime — `filesystem` adapter, `cli` and `lsp` frontends, `query` engine |

The dependency arrows all point *into* core: the effectful crate depends on
`clearhead_core`, never the reverse. That is why core is a plain member under
`crates/` rather than the workspace root — it must not name or "parent" its own
downstream.

## Closing, reopening, and storing history

Closure is a state change, not a file move:

```bash
clearhead complete action <id>                 # this line only, in place
clearhead cancel action <id>                   # this line only, in place
clearhead complete action <id> --cascade       # target and every open descendant
clearhead reopen action <id>                   # this active line, date cleared
clearhead reopen action <id> --cascade         # every closed member, through open ones
clearhead archive action <root-id>             # whole fully closed root tree
clearhead archive actions                      # sweep every eligible root tree
clearhead unarchive action <root-id>            # whole tree, states/dates unchanged
clearhead unarchive action <root-id> --reopen    # move tree; reopen root only
clearhead unarchive action <root-id> --reopen --cascade
```

Single complete/cancel/update refuse a closed target (`already-closed`). A close
result reports `open_children`, not `children`. A close cascade forces its target
to the requested state (a matching target keeps its date); descendants already
closed keep their states and dates. Cancel mirrors complete. Reopen cascades
preserve open members. A cascade with nothing to do succeeds without writes.
`transact` folds single `complete-action`, `cancel-action`, `reopen-action`, and
`update-action` operations in order and commits all their effects together,
including occurrence deviations, frozen Plan links, and the next token.
Reopening does not undo an occurrence's deviation.

Only archive/unarchive move lines and their sidecar entries, always by whole
root tree. Both refuse a child and name its root. Reopen refuses archived lines
and names `unarchive` and the root. `--cascade` on unarchive requires `--reopen`.
`archive charter` sweeps eligible closed trees to completed history before
relocating the charter; `orient` reads closures in both files.

Neovim's `nvim_archive_on_save` is unchanged: when enabled, plain unarchive lasts
only until the next save if the tree is still fully closed. Use `--reopen` or
reopen before saving to keep it active. A reopened root with closed children can
produce W003; closed roots with open children can produce W002. Both remain
warnings, not closure prohibitions.

## Building

Members use ordinary sibling path dependencies, so this checkout builds and
tests standalone:

```bash
cargo build --workspace
cargo test  --workspace
```

Select a single crate with `-p`, e.g. `cargo test -p clearhead_core`.

The tree-sitter grammar comes from crates.io at the version in `Cargo.toml`, in
every checkout, as any consumer gets it. To try an unpublished grammar change,
point the build at a local checkout for that invocation only:

```bash
cargo test --workspace \
  --config 'patch.crates-io.tree-sitter-actions.path="../tree-sitter-actions"'
```

Publish the grammar and bump the version here before committing; the lockfile
must name the registry crate, or `--locked` fails in CI.

## License

MIT License — see [LICENSE](LICENSE).
