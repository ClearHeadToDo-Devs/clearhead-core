# clearhead

**Command-line client for the ClearHead action management framework.**

Work items live in plain-text `.actions` files that any editor can read and write. Recurring schedules live in `.ics` vdir files, and archived charter files remain plaintext under the workspace `archive/` directory. `clearhead` provides synchronous command and mutation workflows over `clearhead-core`, and — with its default `sparql` feature — evaluates ad-hoc and saved SPARQL queries in-process over the workspace's published RDF dataset (standard SPARQL, no query server). The saved presentation query families (`index`, `tree`, `graph`, `chain`) run in-process too; editor intelligence belongs to the `lsp` module, shipped as the standalone `clearhead-lsp` binary.

## Installation

Each [GitHub release](https://github.com/ClearHeadToDo-Devs/clearhead-core/releases) carries a prebuilt x86_64 Linux archive holding `clearhead` and `clearhead-lsp`. It needs glibc 2.34 or later (Debian 12, Ubuntu 22.04, Arch).

With [cargo-binstall](https://github.com/cargo-bins/cargo-binstall), which downloads that archive rather than compiling:

```bash
cargo binstall clearhead
```

Without Rust, as in a container: download the archive and check it against the `.sha256` published beside it. Pin both, so a rebuild installs the same bytes (`--build-arg CLEARHEAD_VERSION=… --build-arg CLEARHEAD_SHA256=…`):

```dockerfile
# Pass both: a release version and the sum published beside its archive, at
# .../releases/download/v<version>/clearhead-x86_64-unknown-linux-gnu.tar.xz.sha256
ARG CLEARHEAD_VERSION
ARG CLEARHEAD_SHA256
RUN curl -sSfL https://github.com/ClearHeadToDo-Devs/clearhead-core/releases/download/v${CLEARHEAD_VERSION}/clearhead-x86_64-unknown-linux-gnu.tar.xz -o /tmp/clearhead.tar.xz \
    && echo "${CLEARHEAD_SHA256}  /tmp/clearhead.tar.xz" | sha256sum -c - \
    && tar -xJf /tmp/clearhead.tar.xz -C /usr/local/bin --strip-components=1 \
        clearhead-x86_64-unknown-linux-gnu/clearhead clearhead-x86_64-unknown-linux-gnu/clearhead-lsp \
    && rm /tmp/clearhead.tar.xz
```

From source, on any platform Rust builds for:

```bash
cargo install clearhead
cargo install clearhead --no-default-features   # without the SPARQL query engine
```

## Quick start

```bash
# Capture an action (lands in the root charter's charters/next.actions)
clearhead add action "Buy oat milk"

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

## Charter identity normalization

`clearhead normalize file path/to/charter.md --write` stamps a missing
frontmatter `id` without rewriting the charter body or other metadata. It reuses
an existing sidecar charter id when present; repeated runs preserve the id.
`clearhead doctor` reports documents still missing one. Existing charter
`update`, `close`, and `jot` do **not** stamp an id; `close` and `jot` stamp one
only when creating a new document.

## Charter lifecycle state

A Charter with no declared `state` — a fresh `add charter`, a root `init`
just wrote, or a Charter known only through its `.actions` file — is `New`:
defined, but not yet admitted for engagement. `New` is deliberately the
planning state; a Charter's open Actions stay hidden from engagement queries
until it is explicitly activated:

```bash
clearhead update charter <name> --state active
```

`init`, `add charter`, and `add action` all write `state: New` (or leave it
unset, which reads the same way) rather than defaulting a fresh Charter to
`Active`. `add action` prints a reminder naming the exact activation command
when the Action it just added landed in a `New` Charter, and `clearhead
doctor` reports every `New` Charter that still owns open Actions — the
catch-all that covers every path into that state, including a `jot`-created
document (`jot` never sets or changes state itself). Engagement also requires
every ancestor Charter to be `Active`; `doctor` warns when an `Active`
Charter sits beneath one that is not.

`normalize file --write` only stamps a missing `id`; it never touches
`state`. Because a Charter's own state never cascades, activating a parent
does not activate its children — each still needs its own explicit
`clearhead update charter <name> --state active`.

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

### Locating results

The graph says what the work is, not where it is stored, so query results
carry identities and `locate` answers where each one lives: the absolute file
and 1-based line, one JSON array in input order, across every configured
workspace. Pipe any `--format ids` output in, or pass ids as arguments; an
unknown id gets a null location and a warning on stderr.

```bash
clearhead query index agenda --format ids | clearhead locate
clearhead locate urn:uuid:01a0faa1-ca17-72a9-8a6b-f05b6bd6dbd3
```

## Governed work selection

`clearhead query index unscheduled` is the trusted next-work view; `agenda`
selects dated work that is actionable now. Both derive eligibility from Action
state and schedule constraints plus the state of the owning Charter and its
ancestors. Run `clearhead doctor` when expected work is absent: it reports
cross-level state contradictions instead of silently normalizing source data.

The normative readiness and Charter-state semantics live in the
[process specification](https://github.com/ClearHeadToDo-Devs/specifications/blob/main/process.md).
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

- [Action file format](https://github.com/ClearHeadToDo-Devs/specifications/blob/main/action_file_format.md)
- [Workspace layout and naming](https://github.com/ClearHeadToDo-Devs/specifications/blob/main/workspace.md)
- [Process](https://github.com/ClearHeadToDo-Devs/specifications/blob/main/process.md)

## License

MIT
