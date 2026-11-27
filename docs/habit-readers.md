# Habit reader caller inventory

Audited 2026-11-05 for `=core-habit-readers`, against specification v0.5.0 and
pinned grammar v0.11.1. The starting inventory is platform
`specifications/docs/habit-transition-review.md`. The sweep searched all Rust,
Lua, shell, Python and JavaScript sources for `.actions`, `read_actions`,
`parse_actions` and `parse_document`, then followed their callers. VS Code has
no reader implementation. Historical platform scripts and parser/debug examples
are inert codec consumers, not workspace discovery.

## Classification boundary

`workspace/action_files.rs` classifies the longest known suffix first:
Habit definitions, completed history, retired upcoming materializations, ordinary
Actions. It derives the ordinary anchor without inventing a second charter.
Templates and archive regions are not live executable work. `habit_files.rs`
lowers roots to native Habits and leaves descendants as template data. It owns
file-role diagnostics shared by assembly, CLI lint and LSP. The parser retains
RRULE text even outside a habits file: it is inert there, warns, and every
Action serializer carries it forward. The pinned Topiary layout query is extended
with an opaque recurrence leaf so even whitespace inside `R:` is not normalized.
An unparseable anchor or ambiguous duplicate planned blocks refuses a destructive
rewrite; it never drops the rule. RFC validation uses the existing `rrule` crate;
no dependency was added. No native occurrences are generated here.

## Callers and disposition

Paths below are relative to the relevant crate's `src/`, unless stated otherwise.
The machine inventory lists **every matching production source file**, including
syntax-only consumers, inline tests and incidental documentation matches.

| Area | Callers | Role / disposition |
| --- | --- | --- |
| Core discovery | `workspace/resource.rs`, `store/assembly.rs`, `store/pathing.rs` | Read-plan includes Habit files and templates; assembly lowers only live charter roots, not templates/history. All suffixes pair with the same charter/document/collection. |
| Core file layer | `workspace/action_files.rs`, `charter.rs`, `sidecar.rs`, `store/load.rs`, `store/mod.rs` | Habits survive conversion to/from domain Charters, with their source locations. Habits share the ordinary anchor's sidecar; completed-history sidecars remain distinct. Workspace note logs are unioned by entity UUID independently of Action metadata, without lowering Habits to executable Actions. |
| Core syntax | `workspace/actions/{parser,source,repository,format,lint,convert,merge,mod}.rs`, `lib.rs`, `schema_export.rs` | Syntax documents may contain Habit/template lines. Parsing, formatting and source merging are filename-neutral and preserve R:. Merging uses syntax UUIDs and nesting, not live workspace discovery or Habit lowering. Semantic assembly or `lint_action_file` supplies the file role. Schema export also retains the rule. |
| Core mutation | `workspace/{mutate_actions,action_lifecycle,transaction}.rs` | Explicitly reject Habit/template resources as executable Action mutations. Ordinary R: survives insert/update/delete/close/move/transaction formatting. |
| Core Habit stamping | `workspace/habit_stamp.rs`, `workspace/habit_stamp/catch_up.rs` | Consumes already parsed Habit and optional named-template documents with their source maps. Validates the selected tree before stamping. Plans bounded batches against live/history IDs supplied by the host; does not discover or write files. |
| Core history | `workspace/{archive_actions,archive_facts,archive_charter}.rs`, `store/doctor.rs` | Action collections exclude Habits. Doctor knows Habit/step ids for duplicate, sidecar and note-ownership checks. Archived Habit definitions supply note locations but are excluded from archived Action state resolution. |
| Core projections | `rdf/app.rs`, `schema_export.rs`, `reference.rs`, `domain/{mod,update,diff,filter}.rs` | Native Habits project as Habits, never Actions; template children are not published as executable nodes. Locations point to the habits file and root line. Domain-level action algorithms remain syntax/path independent. |
| Legacy calendars/templates | Core `workspace/calendar/{ics,expand,reconcile,sync_store}.rs`, `workspace/templates.rs`; CLI `filesystem/{calendar,templates}.rs`, `cli/template.rs` | Native Habit loading does not enter legacy Plan expansion. Template reads use the explicit template-data reader, not the executable-Action adapter. Existing manual template application and legacy calendars remain intact. |
| CLI discovery | `filesystem/{discovery,mounts}.rs`, `environment_reader.rs`, `cli/{resolver,init,orient,debug,query}.rs` | Ordinary Action listings use the shared live predicate. Manifests include Habit-only owners and coalesce ordinary/Habit anchors without duplicate charters. Downstream workspace consumers see assembled native types. |
| CLI source reads/writes | `filesystem/{action_files,mod,durability}.rs`, `cli/{mod,action,file,merge,plan,service}.rs` | Explicit Action APIs refuse Habit/template paths rather than lowering them as work. Formatting and lint also operate on source. The merge driver takes three standalone inputs, bypasses workspace discovery, and never calls an executable-Action API. The Action-only Plan-export file path refuses Habit input rather than exporting template children as work. |
| CLI Habit delivery | `cli/habits.rs`, `filesystem/habits.rs` | `apply habits` reads native definitions and their selected templates; reads live/completed/archived rows and matching sidecars only as identity evidence; appends whole trees to ordinary Action anchors. No history is added to live reads. |
| CLI archive/doctor | `filesystem/{archive_charter,archive_facts,doctor}.rs`, `cli/charter.rs` | Charter lookup accepts the habits file. Known Habit file relocates byte for byte alongside its charter, before supporting-file discovery. The terminal Action sweep never consumes it. Completed-history facts remain Action-only. |
| CLI publication/display | `export.rs`, `query/sparql/index.rs`, `display/{tree,detail}.rs`, `lib.rs`, `argparser.rs` | Consumers of native models / classified collections; no independent Habit-to-Action lowering. |
| LSP | `lsp/{mod,providers,handlers}.rs` | Syntax ranges remain usable for all suffixes. Diagnostics use URI-aware file roles. Workspace diagnostics and navigation consume the assembled model. |
| nvim | `lua/clearhead/init.lua` | Complete-suffix stem inference; live picker excludes definitions/templates/history; save autocmd never sweeps Habits; explicit Habit archive routes to charter archive. Format/reload autocmds still apply to Habit syntax. |
| Other nvim suffix matches | `lua/clearhead/{config,health}.lua`, `ftdetect/actions.lua` | Configuration/default capture, grammar compatibility and syntax filetype detection; no semantic Action discovery. Remaining Lua modules consume classified CLI views or edit syntax. |

## Named-template identity records (2026-11-07)

`filesystem/templates.rs::read_template_actions` returns the strict parser's
`ParsedDocument`, including `source_map`: a step's `is_id_generated` record
identifies ids absent from the written source. A generated UUID in `actions`
alone is not evidence of a persisted identity. Before manual application, charter
creation or legacy calendar expansion writes generated Actions, this reader
rejects template recurrence (E002) and ancestor dependencies (E009) with the
file and line. The ancestry check uses the same workspace reference rules as
`doctor` and `lint file`: a step's own template adds candidates but display
names and ambiguous references never resolve. The editor reports E009 from the
saved workspace, not from a partial buffer. Missing written ids do not block
these generation paths;
`habit_files::lint_action_file` reports `habit-id-missing` with the step's source
range. `habit_template_ids` integration tests in Core and CLI cover the report,
reader records, written-id round trips and strict syntax rejection.

Native stamping also refuses missing written Habit and template-step IDs. It
writes internal predecessor references as the stamped step UUIDs so they survive
a file round trip. `habit_step_identity` covers inline and named source roles,
file/line refusals, deterministic IDs, unchanged retry, and DST boundaries.
`habit_catchup` exercises the batch budget, arrival cutoff in the text anchor's
time frame, exclusions, archived IDs, and retries using re-parsed output. Batch
planning never advances a cursor; the set of durable occurrences determines
what the next run still needs. `apply_habits` tests the native delivery path:
preview, bounded retries, refusal, live and archive dedup, and lineage after
re-reading or archiving. Delivery publishes lineage before atomically replacing
the Action file; a retry after only the sidecar was written sees no Action row
and can still publish the tree. Legacy calendar sync does not project a native
occurrence tree as one-off Plans.

## Guard and evidence

`python3 scripts/habit-reader-gate.py` compares source-match counts with
`docs/habit-reader-callers.txt`. It runs in `scripts/gate.sh` and covers only
Core's own Rust sources: the gate is independent of sibling checkout contents.
The cross-repository nvim token inventory was dropped; nvim's semantic tests
remain owned and run by that repository. A new caller or a new matching read
inside an existing file requires an explicit inventory update after auditing
its role. Comment-only lines (including block comments) are excluded; inline
comments and inline tests still count. The gate runs a regression test for this
filter before checking the inventory. This conservative guard is not a substitute
for semantic tests.

Storage roles are determined by the first component of a workspace-data-root
relative path, through Core's `resource_role`: only `archive/` and `templates/`
at that root are special. Native CLI and LSP paths are relativized by the shared
`filesystem::action_files::role_path` adapter with the caller's explicit loaded
workspace root; it never probes ancestors for directory names. Workspace reads
and writes use `read_actions_in` / `write_actions_in`; standalone codec wrappers
have suffix semantics only. Directory-form charters such as `charters/archive/`
and workspace ancestors named `templates` or `charters` remain ordinary.

`habit_reader_conformance` and `habit_reader_paths` execute real integration
tests: root/flat/directory files, Habit-only owners, named templates, terminal
files, invalid rules/children, scope warnings, source-preserving CLI mutations,
locations, sidecar ownership and byte-preserving archive moves/supporting files.
`git_merge_driver` merges two edits to a Habit's source line and checks that
both the exact recurrence text and the nested template step survive.
`clearhead.nvim/tests/habit_reader_conformance_spec.lua` covers stems, picker
eligibility, save suppression and explicit charter archive routing. Run its
README's `busted tests` with `parser/actions.so` built from the pinned grammar;
pending parser-dependent specs do not count.

## Known archive divergence retained by dispatcher decision

The 2026-11-05 launcher answer explicitly excluded replacing the pre-existing
charter archive contract from this increment. In CLI
`filesystem/archive_charter.rs::archive_many` (the internal archive worker),
terminal Actions are swept, destination names are UUID-flat, sidecars crystallize
charter identity, and frontmatter parent links may be rewritten. Supporting
files use existing UUID-prefixed names. These differ from workspace.md's
verbatim, path-preserving archival with the sidecar moved unchanged. Only the
newly classified habits resource is guaranteed to move byte for byte here.
The old archive behavior and its tests are intentionally retained; its replacement
is separate work, not an interpretation left open by the published spec.
