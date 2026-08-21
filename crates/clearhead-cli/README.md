# clearhead

**Command-line client for the ClearHead action management framework.**

Work items live in plain-text `.actions` files that any editor can read and write. Recurring schedules live in `.ics` vdir files, and archived charter files remain plaintext under the workspace `archive/` directory. `clearhead` provides synchronous command and mutation workflows over `clearhead-core`. Graph reads and SPARQL export belong to the separate [`clearhead-graphd`](https://github.com/ClearHeadToDo-Devs/clearhead-graphd) tool; editor intelligence belongs to [`clearhead-lsp`](https://github.com/ClearHeadToDo-Devs/clearhead-lsp). The CLI does not proxy either public interface.

## Installation

```bash
cargo install clearhead
```

Or build from source:

```bash
git clone https://github.com/ClearHeadToDo-Devs/clearhead-cli
cd clearhead-cli
cargo build --release
```

## Quick start

```bash
# Add an action to your inbox
clearhead add action "Buy oat milk" --charter inbox

# List open actions
clearhead read actions --open-only

# Complete it
clearhead complete action "Buy oat milk"

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

Call graphd directly for saved views and ad-hoc SPARQL:

```bash
clearhead-graphd query index agenda
clearhead-graphd query raw 'SELECT ?s WHERE { ?s ?p ?o }' --format json
```

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
