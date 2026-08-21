# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

## 2026-01-25

### Added
- **V4 Domain Model (`domain.rs`):** New module with CCO-aligned structs that separate task definition from execution:
- `Plan` - information content (cco:Plan) - holds name, description, priority, contexts, and hierarchy (with schedule metadata sourced from `.ics`)
  - `PlannedAct` - occurrence (cco:PlannedAct) - holds phase, scheduled time, completion time
  - `ActPhase` - lifecycle state enum (NotStarted, InProgress, Completed, Blocked, Cancelled)
  - `DomainModel` - container with `from_actions()` conversion from ActionList
  - This implements the BFO insight: Plans are continuants (persist), PlannedActs are occurrents (unfold in time)

- **V4 Graph Loading (`graph.rs`):** New functions for loading domain model into Oxigraph with v4 ontology:
  - `load_domain_model(&store, &model)` - inserts Plans and PlannedActs as separate RDF entities
  - `insert_plan()` / `insert_planned_act()` - v4 predicates with CCO types
  - Uses `https://clearhead.us/vocab/actions/v4#` namespace

### Changed
- **Renamed `sql.rs` → `graph.rs`:** Better reflects that this is RDF/SPARQL graph operations, not SQL.
  - Legacy alias `pub use graph as sql` preserved for compatibility during migration.

- **Fixed Deprecated Oxigraph APIs:**
  - `Subject` → `NamedOrBlankNode`
  - `store.query()` → `SparqlEvaluator::new().parse_query().on_store().execute()`
  - Removed unnecessary `mut` on closures

- **Refactored Tree-sitter Parsing (`entities.rs`, `treesitter.rs`):** Cleaner helper functions reduce boilerplate:
  - `get_prefixed_text(node, source, prefix)` - strips prefix char and trims
  - `SourceRange::from_node(node)` - extract range from any node
  - `parse_date_field()`, `parse_duration_field()` - focused helpers
  - Metadata parsing loop reduced from ~180 lines to ~65 lines

### Architecture Note
The data flow is now:
```
.actions file → Parse Tree → ActionList → DomainModel → Oxigraph (v4)
                                       ↘ (v3 legacy path still works)
```

---

## Roadmap: CRUD Operations

The following work remains to enable full Create/Read/Update/Delete operations via the CLI:

### 1. Create (Add new actions)
- [ ] **`add` command**: Add new action to a file
  - Parse existing file → ActionList
  - Create new Action with provided name/metadata
  - Append to list (or insert at position)
  - Write back to file using formatter
- [ ] **Interactive mode**: Prompt for name, priority, contexts, etc.
- [ ] **Stdin support**: `echo "Buy milk" | clearhead add --file inbox.actions`

### 2. Read (Already mostly complete)
- [x] `read` command reads workspace or specific files
- [x] Multiple output formats (actions, json, xml, table)
- [ ] **Query by Plan vs PlannedAct**: Extend SPARQL queries to use v4 model
- [ ] **Fix failing integration tests**: The `build_where_query()` generates malformed SPARQL

### 3. Update (Modify existing actions)
- [ ] **`update` command**: Modify action by ID
  - `clearhead update <uuid> --state completed`
  - `clearhead update <uuid> --priority 1 --context work`
- [ ] **Surgical file edits**: Use source ranges from `ParsedDocument.source_map` to edit in-place
  - Avoids full reparse/reformat cycle
  - Preserves user formatting where possible
- [ ] **Batch updates**: Update multiple actions matching a query
  - `clearhead update --where "state = 'not_started'" --state in_progress`

### 4. Delete (Remove actions)
- [ ] **`delete` command**: Remove action by ID
  - `clearhead delete <uuid>`
  - Use source ranges for surgical removal
- [ ] **Archive vs Delete**: Option to move to archive file instead of permanent delete
- [ ] **Cascade option**: Delete action and all children

### 5. Supporting Infrastructure
- [ ] **File write-back**: Safe atomic writes (write to temp, rename)
- [ ] **Conflict detection**: Check file hasn't changed since read (mtime or hash)
- [ ] **Undo/history**: Integration with events.rs for operation logging
- [ ] **CRDT sync**: Use crdt.rs for multi-device merge after local edits

### 6. Graph/Query Improvements
- [ ] **Migrate queries to v4**: Update `query_actions()` to query Plans/PlannedActs
- [ ] **Fix `build_where_query()`**: Current SPARQL template has formatting issues
- [ ] **Query by phase**: `clearhead read --phase completed`
- [ ] **Query by plan hierarchy**: Find all acts under a plan subtree

---

## 2026-01-17

### Changed
- **Workspace-First Read Command:** The `read` command now reads ALL `.actions` files from the workspace by default, enabling cross-file queries.
  - `read` (no args) - reads entire workspace
  - `read file <path>` - reads specific file
  - `read stdio` - reads from stdin
  - This is a **breaking change** from the previous behavior where `read` defaulted to `inbox.actions`.

### Added
- **Workspace Discovery:** New `workspace` module that recursively discovers all `.actions` files in the data directory.
  - Skips hidden directories (like `.clearhead`)
  - Infers project names from directory structure (e.g., `work/next.actions` → project "work")
- **Cross-File SQL Queries:** SQL schema now includes `file_path` and `project` columns for filtering across files.
  - `--where "project = 'work'"` - filter by inferred project name
  - `--where "file_path LIKE '%inbox%'"` - filter by source file path
- **Source Tracking:** Actions loaded from workspace retain their source file metadata for accurate cross-file queries.

### Removed
- **SQL flags on file/stdio subcommands:** The `--where` and `--sql` flags are now only available for workspace-wide reads. This simplifies the interface:
  - Workspace reads = query the database (SQL supported)
  - File/stdio reads = parse input (no SQL, just format conversion)

## 2026-01-03

### Changed
- **Formatter Now Handles All Horizontal Spacing:** Updated `format_as_actions_basic()` to add spec-compliant spacing when serializing from IR to `.actions` format.
  - Space after state brackets: `[x] Task` not `[x]Task`
  - Space before all metadata tokens: `Task $ Desc !1` not `Task$Desc!1`
  - Space after description icon only: `$ Description` not `$Description`
  - No space after value icons: `!1`, `*Story`, `+Work`, etc.
  - This change makes the formatter fully compliant with `formatting_specification.md` v1.1.0.

- **Topiary Temporarily Disabled:** Disabled `format_with_topiary()` in the main formatting pipeline because Topiary was stripping the horizontal spacing we're adding.
  - Root cause: Tree-sitter grammar uses `extras: [/\s/]` which makes whitespace invisible to the AST.
  - Topiary cannot preserve or control horizontal spacing due to this grammar limitation.
  - Our IR-based formatter now handles both horizontal and vertical spacing directly.
  - Note: `format_with_topiary()` still exists and is used by LSP (see "Known Issues" below).

### Fixed
- **Formatter Now Normalizes Spacing:** Badly spaced input like `[x]Task$desc!1*Story` now correctly formats to `[x] Task $ desc !1 *Story #<uuid>`.
- **Updated Integration Test:** Fixed `test_actions_with_hierarchy` to expect properly spaced output (with space after state, no space after child markers).

### Known Issues & Decisions Needed

#### 1. Formatting Strategy: Topiary Adoption
- **Decision:** Topiary is now the sole engine for `.actions` file formatting.
- **Change:** Replaced the custom `format_as_actions_basic()` implementation with `format_with_topiary()`.
- **Impact:**
    - Vertical spacing is strictly enforced by the Topiary query (`actions.scm`).
    - Horizontal spacing and indentation are currently flat (whitespace stripped) due to Topiary grammar limitations (`extras`).
    - `List` style support is effectively removed; output is always "Compact" style (metadata on same line).
    - `IndentStyle` and `IndentWidth` settings are currently ignored by the formatter.


#### 2. LSP Formatting is Broken
**Current State:**
- `src/lsp.rs:426` still calls `format_with_topiary(&doc.text, &config)` directly
- This means LSP format command strips horizontal spacing
- LSP is not using the new spec-compliant formatter

**Decision Needed:**
- [ ] Fix LSP to use IR-based formatter
  - Parse text → ActionList IR → format_as_actions() → formatted text
  - Or create a new `format_text()` helper that does this pipeline
  - Benefits: LSP formatting would actually work correctly

#### 3. List Mode Not Implemented
**Current State:**
- `FormatConfig` has a `style: FormatStyle` field with `Compact` and `List` variants
- List mode is supposed to put metadata on separate indented lines
- Currently, both modes produce identical output (compact format)
- Topiary query never implemented list mode correctly

**Decision Needed:**
- [ ] Implement list mode in `format_as_actions_basic()`
  - Check `config.style` and format accordingly
  - Compact: `[x] Task $ Desc !1 *Story`
  - List:
    ```
    [x] Task
        $ Desc
        !1
        *Story
    ```
  - Would require modifying the serialization logic to add newlines and indentation

- [ ] Or remove list mode entirely if not needed
  - Remove `FormatStyle` enum
  - Simplify `FormatConfig` to just `indent_width` and `include_id`
  - Update specs to only define compact mode

## 2026-01-01

### Added
- **Topiary Formatting Integration:** Integrated Topiary as the official formatter for `.actions` files, implementing a "cargo fmt" equivalent.
  - Added `topiary-core` and `topiary-tree-sitter-facade` dependencies.
  - Created `queries/actions/topiary.scm` query file defining formatting rules (currently implements compact mode).
  - Added `FormatConfig` with `style` (Compact/List), `indent_width`, and `include_id` options.
  - **New `format` command:** Pure formatting without UUID modification. Preserves existing UUIDs, applies spacing rules.
  - Created comprehensive snapshot tests in `tests/formatting.rs` for both formatting modes.
  - Note: List mode infrastructure exists but currently produces same output as compact. Indentation scoping in Topiary query needed for full list mode support.

### Changed
- **Separated `format` and `normalize` commands:** Clarified conceptual distinction between formatting (cosmetic) and normalization (data integrity).
  - `format`: Formats `.actions` files with proper spacing, preserves existing UUIDs (doesn't add new ones).
  - `normalize`: Ensures all actions have UUIDs, formats by default (use `--no-format` to skip formatting).
- **Grammar Improvements:** Refactored `tree-sitter-actions` grammar to improve formatter compatibility.
  - Made `priority_level` and `story_name` into named node types (previously anonymous regex nodes).
  - This enables Topiary to preserve priority numbers and story names during formatting.

### Fixed
- **UUID Side-Effect Control:** Added `include_id: bool` field to `FormatConfig` to control UUID output.
  - Parser auto-generates UUIDs for all actions (useful for normal pipeline).
  - Formatter can now omit UUIDs via `include_id: false` (useful for testing stable output).
  - This eliminates the need for UUID redaction in snapshot tests.

## 2025-12-30

### Added
- **Automerge Integration:** Added `automerge` and `autosurgeon` dependencies to enable CRDT-based state synchronization.
- **Sync Utilities:** Created `src/sync_utils.rs` with helpers (`reconcile_date`, `hydrate_date`) to handle `chrono::DateTime` conversion for Automerge.
- **Snapshot Testing:** Implemented a rigorous test harness using `insta` and RON (Rusty Object Notation).
  - Tests now automatically generate/verify snapshots for all examples provided by `tree-sitter-actions`.
  - Added "Golden Data" files in `tests/snapshots/` to serve as the source of truth for the IR.
- **LSP Scaffolding:** Added `tower-lsp` and `tokio` dependencies.
  - Implemented `src/lsp.rs` with a basic Language Server loop.
  - Added `Lsp` command to the CLI, running on an isolated local Tokio runtime to preserve the synchronous nature of other CLI commands.
  - **Surgical Hydration:** Implemented the first LSP feature: a `Code Action` that detects actions missing an ID and surgically injects a `#uuid` (UUIDv7) without rewriting the rest of the file.

### Changed
- **Architecture:** Formalized the "Action-centric" Hub-and-Spoke model. The `Action` struct is now the primary Intermediate Representation (IR) for all sync and query operations.
- **Entities:**
  - `Action` struct now derives `Reconcile` and `Hydrate` for seamless Automerge sync.
  - Changed `priority` field from `usize` to `u32` for better compatibility with Automerge types.
  - Flattened `do_date_time` and `do_duration` fields in the `Action` struct (and Schema) to better match the flat-file format and simplfy sync logic.
- **Schema:** Updated `tree-sitter-actions/schema/actions.schema.json`:
  - Added `parent_id` to the allowed properties (supporting the Adjacency List model).
  - Flattened the `doDate` object properties into top-level fields (`doDateTime`, `doDuration`).
- **SQL:** Updated `src/sql.rs` to handle `priority` as `u32`.

### Fixed
- **JSON Validation:** Fixed regression where the CLI's JSON output (flat list) conflicted with the previous Schema expectation (nested tree). The Schema was updated to match the implementation.
