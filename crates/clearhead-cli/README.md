# clearhead

**Command-line client for the ClearHead action management framework.**

Work items live in plain-text `.actions` files that any editor can read and write. Recurring schedules live in `.ics` vdir files, and archived charter files remain plaintext under the workspace `archive/` directory. `clearhead` provides synchronous command and mutation workflows over `clearhead-core`, and — with its default `sparql` feature — evaluates ad-hoc and saved SPARQL queries in-process over the workspace's published RDF dataset (standard SPARQL, no query server). The saved presentation query families (`index`, `tree`, `graph`, `chain`) run in-process too; editor intelligence belongs to [`clearhead-lsp`](https://github.com/ClearHeadToDo-Devs/clearhead-lsp).

## Installation

```bash
cargo install clearhead
```

Or build from source:

```bash
git clone https://github.com/ClearHeadToDo-Devs/clearhead-core
cd clearhead-core
cargo build --release -p clearhead_cli
```

## Quick start

```bash
# Add an action to your inbox
clearhead add action "Buy oat milk" --charter inbox

# List open actions
clearhead read actions --open-only

# Complete it
clearhead complete action "Buy oat milk"

# Changed your mind? Reopen it (moves the whole subtree back, all NotStarted)
clearhead reopen action "Buy oat milk"

# Jot a timestamped finding into a charter's ## Log (frictionless capture)
clearhead jot "range reads need a cache keyed on Last-Modified" --charter inbox

# Archive completed actions out of active files
clearhead archive actions

# Show resolved config and workspace layout
clearhead debug
```

## Documentation

Full reference documentation is in the man page:

```bash
man clearhead
```

Every subcommand also has inline help:

```bash
clearhead --help
clearhead read --help
clearhead archive charter --help
```

Concrete deployment and tool-composition recipes live in the [CLI cookbook](./docs/cookbook/README.md), beginning with [Radicale and vdirsyncer](./docs/cookbook/radicale-vdirsyncer.md).

## Graph queries

Ad-hoc and saved SPARQL run in-process against the workspace's published RDF
dataset (an ephemeral in-memory store; queries are verbatim standard SPARQL
that also run unchanged in independent tooling):

```bash
clearhead query raw 'SELECT ?s WHERE { ?s ?p ?o }' --format json   # SPARQL Results JSON
clearhead query named my-saved-query        # .clearhead/queries/my-saved-query.sparql
```

Machine output is standard SPARQL Results JSON / RDF serializations. The saved
presentation views (`index`, `tree`, `graph`, `chain`) run in-process as well:

```bash
clearhead query index agenda
clearhead query tree
```

## Governed work selection

`clearhead query index unscheduled` is the trusted next-work view; `agenda`
selects dated work that is actionable now. Both derive eligibility from Action
state and schedule constraints plus the state of the owning Charter and its
ancestors. Run `clearhead doctor` when expected work is absent: it reports
cross-level state contradictions instead of silently normalizing source data.

The normative readiness and Charter-state semantics live in the
[process specification](https://github.com/ClearHeadToDo-Devs/specifications/blob/master/process.md).
The CLI only evaluates and presents that shared contract.

## Editor integration

The official Neovim plugin provides LSP setup, syntax highlighting, state cycling, depth hotkeys, workspace pickers, and archiving commands:

- **[clearhead.nvim](https://github.com/ClearHeadToDo-Devs/clearhead.nvim)**

For other LSP-compatible editors, use the canonical standalone server command:

```bash
clearhead-lsp
```

`clearhead start lsp` remains a temporary compatibility shim that execs the standalone binary. Set `CLEARHEAD_LSP` to an explicit executable path when validating that transition.

## Specifications

The file format, workspace layout, and process model are defined in the [ClearHead specifications](https://github.com/ClearHeadToDo-Devs/specifications):

- [Action file format](https://github.com/ClearHeadToDo-Devs/specifications/blob/master/action_file_format.md)
- [Naming conventions and workspace layout](https://github.com/ClearHeadToDo-Devs/specifications/blob/master/naming_conventions.md)
- [Process](https://github.com/ClearHeadToDo-Devs/specifications/blob/master/process.md)

## License

MIT
