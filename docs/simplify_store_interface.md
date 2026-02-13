# Simplify Store Interface + Eliminate Parallel Loading Paths

## Context

The DomainModel hierarchy refactor (Objectives → Charters → Plans → Acts) is complete, but the storage layer didn't follow. There are currently **three parallel systems** for loading workspace data:

1. **`WorkspaceStore` trait** (store.rs) — has 6 methods but only `discover_charters()` and `save_charter()` are used in production. `load_domain_model`, `list_objectives`, `save_domain_model`, `load_charter` are dead code outside tests.
2. **`workspace.rs`** — its own file walker, its own types (`WorkspaceActions`, `SourcedAction`, `SourcedCharter`, `CharterSource`), its own `discover_charters()` that duplicates store logic. Used by `read_plans` and `show_charter`.
3. **`ActionRepository`** (crdt.rs) — CRDT-backed, single-file scope. Used by `add_plan`, `update_plan`, `complete_plan`, `delete_plan` via `load_repo`/`save_repo`.

The goal: one way to read workspace-wide data (store), one way to mutate single files (CRDT). Eliminate the duplication.

## What Changes

### Phase 1: Simplify WorkspaceStore trait

**File: `clearhead-core/src/store.rs`**

Collapse trait to two methods:
```rust
pub trait WorkspaceStore {
    type Error: fmt::Display;
    fn read_model(&self) -> Result<DomainModel, Self::Error>;
    fn sync_model(&mut self, model: &DomainModel) -> Result<(), Self::Error>;
}
```

Remove from trait surface: `list_objectives`, `load_domain_model`, `save_domain_model`, `load_charter`, `save_charter`, `discover_charters`.

Keep `discover_charters`, `save_charter`, `load_charter`, `resolve_charter_path` as **private/inherent methods** on `FsWorkspaceStore` — they're implementation details of `read_model`/`sync_model`.

Keep `discover_action_files` and `infer_project_name` as internal helpers.

**`FsWorkspaceStore::read_model` implementation:**
1. Discover charters (existing `discover_charters` logic, now private)
2. For each discovered charter, find associated `.actions` files by source_key:
   - Explicit charter `health.md` → look for `health.actions`
   - Implicit charter from `health.actions` → parse that file
   - Implicit charter from directory `project/` → parse `project/next.actions` + any other `.actions` files in that dir
3. Parse each `.actions` file → ActionList → `from_actions` → populate charter's `plans` vec
4. `inbox.actions` → synthetic inbox charter (deterministic UUIDv5, already implemented in convert.rs)
5. Return assembled `DomainModel { objectives: vec![], charters }`

**`FsWorkspaceStore::sync_model` implementation:**
1. For each charter in model:
   - If charter has associated `.md` file (explicit), write charter metadata via `format_charter`
   - Convert charter's plans → ActionList → format → write to associated `.actions` file
2. Inbox charter → write to `inbox.actions`

**`InMemoryStore`:** Simplify to hold a single `DomainModel`:
```rust
pub struct InMemoryStore {
    model: DomainModel,
}
```

### Phase 2: Remove `ObjectiveRef` and `DiscoveredCharter` from public API

**File: `clearhead-core/src/store.rs`**
- `ObjectiveRef` → delete entirely. It was a routing key for the per-objective methods we're removing. Internal charter→file mapping is handled by `FsWorkspaceStore` using filename conventions directly.
- `DiscoveredCharter` → keep as `pub(crate)` or private on `FsWorkspaceStore`. The `is_explicit` / `source_key` metadata is needed internally for `read_model` assembly but shouldn't be in the public API.

**File: `clearhead-core/src/lib.rs`**
- Remove `ObjectiveRef`, `DiscoveredCharter` from re-exports.
- Keep exporting `FsWorkspaceStore`, `InMemoryStore`, `WorkspaceStore`.

### Phase 3: Merge workspace.rs functionality into store

**File: `clearhead-cli/src/workspace.rs`**

Types to remove:
- `WorkspaceActions` — replaced by `DomainModel` (plans live in charters)
- `SourcedAction` / `ActionSource` — the charter→plan hierarchy carries the "where did this come from" information now. A plan's parent charter tells you which file/project it belongs to.
- `SourcedCharter` / `CharterSource` — replaced by `DiscoveredCharter` (internal to store) or just `Charter` in the model

Functions to remove:
- `discover_charters()` — duplicates store.rs logic
- `discover_action_files()` — duplicates store.rs logic
- `load_workspace_actions()` — replaced by `store.read_model().to_action_list()`
- `load_workspace_with_sources()` — replaced by `store.read_model()`
- `load_workspace()` — replaced by `store.read_model()`
- `resolve_charter()` — move to core (on DomainModel or as standalone in charter.rs)
- `infer_project_name()` — already exists in store.rs

What remains in workspace.rs (or moves elsewhere):
- `resolve_charter()` logic → move to `clearhead-core/src/charter.rs` or as a method on `DomainModel`, since it's pure domain logic (search by UUID/alias/name)

### Phase 4: Update CLI consumers

**File: `clearhead-cli/src/commands/charter.rs`**

`read_charters`:
```rust
// Before: FsWorkspaceStore::new → discover_charters() → iterate DiscoveredCharter
// After:  FsWorkspaceStore::new → read_model() → iterate model.charters
```
- The `is_explicit` flag and `source_key` are no longer exposed. If the table display needs "explicit vs implicit", we could either:
  - (a) Add an `is_explicit: bool` field to `Charter` itself, or
  - (b) Keep a separate internal method `discover_charters_with_metadata()` on FsWorkspaceStore for this one command
  - Recommendation: option (b) — `is_explicit` is a storage concern, not a domain property

`show_charter`:
```rust
// Before: discover_charters + resolve + load_workspace for plan count
// After:  store.read_model() → find charter → charter.plans.len()
```
- No more separate plan-counting — the charter already contains its plans.

`add_charter`:
```rust
// Before: FsWorkspaceStore::new → save_charter(ObjectiveRef, Charter)
// After:  FsWorkspaceStore::new → read_model → push charter → sync_model
// Or: keep a dedicated add_charter method on FsWorkspaceStore (simpler)
```
- Recommendation: keep a concrete `FsWorkspaceStore::write_charter(&mut self, charter: &Charter)` method. Writing a single charter file doesn't need full model round-trip.

**File: `clearhead-cli/src/commands/plan.rs`**

`read_plans`:
```rust
// Before: workspace::load_workspace_actions() or load_workspace_with_sources()
// After:  FsWorkspaceStore::new → read_model() → model.to_action_list()
```
- SPARQL/WHERE filtering: pass the DomainModel's action list to existing SQL query functions
- Single-file reads (`--file` flag): still parse directly, no store needed

Mutation commands (`add_plan`, `update_plan`, `complete_plan`, `delete_plan`):
- **No changes** — these go through `ActionRepository` (CRDT), which is a separate single-file concern

**File: `clearhead-cli/src/commands/mod.rs`**
- `load_repo`/`save_repo` — unchanged (CRDT path)

### Phase 5: Update graph.rs SPARQL integration

**File: `clearhead-cli/src/graph.rs`**
- `run_workspace_sql_query` and `run_workspace_sql_where` currently take `&WorkspaceActions`
- Change to take `&ActionList` or `&DomainModel` directly
- The `SourcedAction` wrapping was needed for project name context — with the hierarchy, we can get project name from the charter title instead

## What Does NOT Change
- `ActionRepository` / CRDT path — stays as-is for single-file mutations
- `.actions` file format and tree-sitter grammar
- `Action` struct and `ActionList` type
- Formatting module (`format.rs`)
- Lint module
- Charter `.md` file format
- `load_repo` / `save_repo` in commands/mod.rs

## Key Design Decision: `is_explicit` for charters

The `read_charters` command currently shows whether each charter is "explicit" (from .md file) or "implicit" (inferred). Two options:

- **(a)** Add `is_explicit` to the `Charter` domain type — makes the info available everywhere but mixes storage concerns into the domain
- **(b)** Keep an internal `FsWorkspaceStore` method that returns metadata alongside charters — cleaner separation but slightly more code for this one command

Recommendation: **(b)** — the domain model shouldn't know about filesystem discovery semantics. `FsWorkspaceStore` can have a `pub fn discover_charters_detailed(&self) -> Vec<DiscoveredCharter>` for the `read_charters` command specifically, while `read_model()` strips that metadata.

## Verification
1. `cd clearhead-core && cargo build` — compiles clean
2. `cd clearhead-core && cargo test` — all non-pre-existing tests pass
3. `cd clearhead-cli && cargo build` — compiles clean
4. `cd clearhead-cli && cargo test` — all non-pre-existing tests pass
5. Manual: `clearhead read plans` returns same output as before
6. Manual: `clearhead read charters` returns same output as before (including explicit/implicit column)
7. Manual: `clearhead show charter health` shows charter + plan count
8. Manual: `clearhead add plan "test" -w` still works (CRDT path unchanged)
