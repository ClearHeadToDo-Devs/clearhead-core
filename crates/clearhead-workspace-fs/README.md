# clearhead-workspace-fs

**The native filesystem delivery adapter for ClearHead workspaces.**

`clearhead_core` is a *pure* domain library: it holds the in-memory model,
runs the algorithms, and **decides** what a workspace mutation should do — but
it performs no I/O. This crate is the other half of that seam. It reads bytes
off disk, hands them to Core, and durably executes the changes Core decides on.

If Core is the brain, `clearhead-workspace-fs` is the hands.

## Why it exists

The domain core was deliberately made host-neutral so the same decision logic
can run natively *and* in a WebAssembly host (a browser, an embedded runtime)
where there is no filesystem. Everything that assumes a POSIX filesystem —
paths, `fs::read`, locking, `fsync`, atomic rename — lives here instead of in
Core. A different host (say, a browser talking to a vault API) would provide
its *own* adapter implementing the same boundary; Core would not change.

## The delivery boundary

Core speaks in host-neutral terms defined in
[`clearhead_core::workspace::resource`](../clearhead-core/src/workspace/resource.rs):

- **`WorkspacePath` / `ResourceLocation`** — logical, `/`-separated workspace
  paths with no OS, symlink, or root-directory semantics. This adapter maps
  them to real paths (see `mounts.rs`).
- **`ResourceSnapshot`** — immutable bytes *this adapter has already read*.
  Core never opens a file; it consumes snapshots.
- **`ResourceRevision`** — opaque per-resource evidence Core compares but never
  interprets. Natively this is a BLAKE3 content digest.
- **`Effect`** (`Write` / `Remove` / `Move`) bundled into an **`EffectBatch`**
  with a precondition (`ExpectedResource::Missing` or `Revision`) per affected
  resource. Core emits the batch; this adapter executes it.
- **`PreparedMutation`** — speculative next state that is adopted **only** after
  this adapter confirms the batch was delivered successfully. On conflict or
  failure the speculative state is discarded and the caller reloads.

The round trip is: **inventory → read bytes into snapshots → Core prepares an
`EffectBatch` → validate preconditions → execute durably → adopt outcome.**

## What it provides

- **Loading** (`mounts.rs`): `load_workspace`, `read_workspace`,
  `load_domain_model` — inventory the workspace mount (and any external plans
  mount), read the bytes, and assemble a `DomainModel` via Core.
- **Mutation execution** (`lib.rs`): `insert`/`update`/`delete`/`close`/
  `archive` action helpers, each threading Core's prepared outcome through the
  locked, journaled write seam.
- **Durability** (`durability.rs`): three layers of protection —
  - `atomic_write` — temp + fsync + rename + directory fsync for single files.
  - `PendingBatch` — a `.pending` journal that stages a multi-file batch and
    converges it in order; an interrupted batch is replayed forward by
    `recover_pending` on the next load.
  - `WorkspaceLock` — an OS-backed exclusive lock that serializes writers and
    is released by the kernel after a crash.
- **Calendar sync** (`calendar.rs`): reading/writing `.ics` plan files and
  reconciling occurrences against Core's sync plan.
- **Doctor** (`doctor.rs`), **manifest** (`manifest.rs`), **discovery**
  (`discovery.rs`), **sidecar** (`sidecar.rs`), **templates** (`templates.rs`),
  and host **config** resolution (`config.rs`).

## Who uses it

The native hosts compose Core (decisions) with this crate (delivery):

- [`clearhead-cli`](../clearhead-cli) — the `clearhead` command-line client
- [`clearhead-lsp`](../clearhead-lsp) — the editor protocol server

Both depend on `clearhead_core` with `default-features = false, features =
["formatting"]` and on this crate for everything that touches the disk.

## Building

This crate is a member of the `clearhead-core` workspace:

```bash
cargo build -p clearhead-workspace-fs
cargo test  -p clearhead-workspace-fs
```

## License

MIT
