# ClearHead Core decisions

Decisions that only this repository's code must honor. Decisions that span
repositories (the specification, repository topology, shared tooling) live in
the platform's `docs/DECISIONS.md`. Each entry states the choice, the
alternatives rejected, and the trade-off accepted.

---

## Decision 3: Single Closure in Place; Explicit Cascades Force Their Target

Decided by Darrion on 2026-11-02 (platform Decisions 58 and 59). This amends
Decision 2: **single** complete/cancel still reject a closed target, but an
explicit `--cascade` forces its target to the requested terminal state. A target
already in that state keeps its date; an oppositely closed target receives the
new state and date. Every open descendant closes, including below closed
members; closed descendants keep their factual states and dates. Cancel mirrors
complete. An empty expansion succeeds without writing.

Close and reopen change only the selected active line. Reopen clears its date;
`reopen --cascade` reopens all closed members and preserves open members.
Transactions fold these single operations, including `reopen-action`, in order.
The shared close resolves occurrences only on the open-to-closed transition:
state, deviation, frozen Plan link in the line's sidecar, and next token commit
together. Forcing a closed target closed never resolves again; reopen does not
undo a deviation. Update rejects closed active lines in either delivery path.

Archive and unarchive alone move whole root trees and their sidecar entries,
without changing states or dates. A single archive requires a fully closed tree;
the sweep skips ineligible roots. Both forms refuse a child and name its root.
Reopen refuses archived lines and points to `unarchive` and the root.
`unarchive --reopen` reopens just the root; adding `--cascade` reopens closed
members too. `--cascade` alone is a usage error. Charter archival sweeps eligible
trees into completed history first, and orient reads both active and completed
closures. Neovim archive-on-save is not changed: a plain unarchive of a still
closed tree is swept back on save.

**Alternatives rejected:** implicit subtree closure; making a cascade silently
ignore an oppositely closed target; erasing descendants' historical closure
facts; treating an archived child as a detached new root.

**Trade-off:** a closed line may remain under an open root indefinitely. W002
and W003 remain warnings. Explicit storage moves and an optional reopen make
the behavior composable without adding hidden sweep-exemption state.

## Decision 2: Explicit Cascade Closure Preserves Existing Closures

Decided by Darrion on 2026-11-01: “if i complete in a casacding fashion any
existing closures or cancells stay so if a chid has a closed state dont overwrite
it.” This extends cancellation's history-preservation rule to completion.

An explicitly requested completion or cancellation cascade closes only open
members. Already completed or cancelled members retain both their state and
`completed_at`, including an absent timestamp. Traversal still reaches open
members below terminal descendants, and archival retains the subtree's internal
parent links. A terminal target in the active file is rejected as `AlreadyClosed`
by ordinary verbs and transactions alike, rather than restamping its subtree.
The terminal-target guard is shared by both paths.

`specifications/process.md`, “On Closure” and “On Child Actions”, separates
closure from archival, keeps states independently asserted, and permits an
explicit reviewed batch transition. Preserving an existing closure within that
batch does not change the specification; no implicit cascade is introduced.
Reopen behavior was not decided here; Decision 3 supersedes that behavior and
amends the terminal-target rule specifically for explicit cascades.

**Alternative rejected:** replacing all descendant states and timestamps with
those of the new cascade, which erases factual completion and cancellation
history.

**Trade-off:** an archived subtree can contain both completed and cancelled
members, and missing historical timestamps stay missing rather than acquiring
an invented closure time.

## Decision 1: Ephemeral Identity Never Leaves the Process

Platform Decision 40 gives a charter with no declared id an ephemeral id,
supplied by the delivery shell on each load. That id is an in-memory join key
only. No output surface emits it, because anything outside the process that
sees it will take it for the real one:

- canonical JSON omits `id` (the specification's `charters.schema.json` makes it
  optional for this reason);
- `--format ids` skips the charter;
- RDF output (JSON-LD reads, `export`, `query`) represents the charter as a
  blank node, RDF's own term for a thing with no stable identity;
- human views (`show`, `read charters`, `orient`) omit the
  id or say it is not declared.

The rule is applied from `CharterIdSource`, recorded once at load; no surface
re-derives it from the document.

**Alternatives rejected:** stamping the id on read (reads become writes: a
`read` produces a git diff, the LSP rewrites a buffer being edited, and two
concurrent readers mint conflicting ids; a load would return effects, breaking
the pure core); deriving a stable id from the path (forbidden by the
specification's Concept Identity rules); emitting the id with an `ephemeral`
flag (consumers ignore the flag and persist the id anyway).

**Trade-off:** exported data cannot reference an id-less charter unless it has
an alias, since `parent` and an action's `charter` accept only an alias or a
UUID. That gap is deliberate pressure toward `normalize`, which stamps a
durable id.
