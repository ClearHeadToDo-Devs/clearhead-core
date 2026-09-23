# ClearHead Core decisions

Decisions that only this repository's code must honor. Decisions that span
repositories (the specification, repository topology, shared tooling) live in
the platform's `docs/DECISIONS.md`. Each entry states the choice, the
alternatives rejected, and the trade-off accepted.

---

## Decision 1: Ephemeral Identity Never Leaves the Process

Platform Decision 40 gives a charter with no declared id an ephemeral id,
supplied by the delivery shell on each load. That id is an in-memory join key
only. No output surface emits it, because anything outside the process that
sees it will take it for the real one:

- canonical JSON omits `id` (the specification's `charters.schema.json` makes it
  optional for this reason);
- `--format ids` skips the charter;
- JSON-LD represents the charter as a blank node, RDF's own term for a thing
  with no stable identity.

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
