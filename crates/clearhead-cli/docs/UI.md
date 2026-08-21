# User Interface

The other major Concern of the tool is building the proper user interface for both tools and users.

As mentioned in the README, we adhere to a verb-noun interface where different commands are structured as `clearhead <verb> <noun> [options]`. This allows us to have a consistent and intuitive command structure that can be easily extended as we add more functionality.

Specifically, the use of subcommands allows for a clear separation of different functionalities and makes it easier for users to discover and use the various features of the tool. For example, we can have subcommands for managing objectives, charters, plans, and acts, each with their own set of options and flags.

## Nouns

We use the above file formats as our primary nouns so that each verb can operate on the noun in a consistent way. For example, we can have commands like:

- Objectives
- Charters
- Plans
- Planned Acts
- LSP
- Config

### Read

`clearhead read` reads canonical workspace entities through core. Semantic queries are a separate tool boundary: `clearhead-graphd` owns SPARQL, query families, validation, rendering, and graph export, and clients invoke it directly. The CLI does not proxy graphd.

## Verbs

The verbs are mostly standard CRUD operations:

- Create
- Read
- Update
- Delete
- Start
- Stop

the point is to keep the interface as simple and intuitive as possiible so that users can easily see and compose interactions together

## Output

CLI output covers canonical entities and mutation outcomes. graphd independently owns query output; it is not a CLI presentation layer.

### Destination is binary: human or machine

The only distinction is `isatty(stdout)` — a terminal is a human, everything else is a machine. A pipe and a file redirect are the *same* machine destination, deliberately: branching on FIFO-vs-file would make `cmd > f` and `cmd | tee f` disagree, breaking the composability that makes pipes worth having. Intent finer than "human or machine" is carried by a **flag**, never by the kind of file descriptor.

### Verb picks the machine format, because representability forces it

| Verb           | Terminal (human)          | Machine (pipe / redirect) |
|----------------|---------------------------|---------------------------|
| `read <noun>`  | static table / tree render| **native on-disk format** |

- **`read <noun>`** targets one entity type, and each has a home file: actions → `.actions` DSL, charters → Markdown, plans → vdir/iCal. So its machine output *is* that format. `read actions > inbox.actions` yields a valid workspace file, and `read actions | clearhead update …` round-trips clearhead-to-clearhead — no conversion.

### Explicit format flags override both

Format flags are tool-local. The CLI's entity reads support their documented native, JSON-LD, table, and ID modes; graphd separately owns query formats such as NDJSON, nested JSON, and RDF serializations. `--ids` emits one UUID per line for xargs-style batch work:

```
clearhead read actions --charter lsp --ids | xargs -I{} clearhead update action {} --state in-progress
```

Mutations accept their entity's native format on stdin, which is what makes the round-trip pipelines above work:

```
clearhead read actions --charter inbox | clearhead update action --state in-progress
```

### Interactive UI is out of scope (for now)

The terminal renders above are **static** — format a result, print it, exit; `display/tree.rs` is a pure `DomainModel → String` with no event loop. A live, navigable, keybound view is a genuinely separate tool, and — as graphd is to the graph engine — it would be its own binary consuming this output, not a mode of the CLI. It is deferred, not designed here; nothing in this contract assumes it.

## Flags

Common flags shared across subcommands:

- `--charter`: Scope to a specific charter (name, alias, or UUID)
- `--workspace`: Restrict to a named workspace (for multi-workspace setups)
- `--file`: Target a specific `.actions` file directly
- `--dry-run`: Preview what would change without writing
- `--help`: Display help for the command

by composing these various verbs, nouns, and flags together we can create a powerful and flexible command-line interface that allows users to easily manage their objectives, charters, plans, and acts while also providing the necessary tools for querying and manipulating the underlying data.

## Examples

Now that we have covered the structure i want to cover how specific functionality is covered without needing to implement a new verb or noun

### Create Items

Any item can be created using simple flags around the noun. For example, to create a new objective, you can use the following command:

```
clearhead create objective "my new objective" --description "this is a description of my new objective" --alias "new-objective"
```

however, we can also create a new objective by reading in a file that contains the objective definition:

```clearhead create objective --file objective.md
```

or even add a list of objectives using a directory of markdown files:

```clearhead create objective --dir objectives/
```

### Deleting and Updting Items

to delete an item you can use any of the four reference types (id, file, name, alias) to specify the item you want to delete:

```clearhead delete objective --id 123
clearhead delete objective --file objective.md
clearhead delete objective --name "my new objective"
clearhead delete objective --alias "new-objective"
```

We can also leverage the power of SPARQL queries to delete multiple items at once. For example, to delete all objectives that have a certain tag, you can use the following command:

```
clearhead delete objective --where "{ ?objective a :Objective ; :hasTag :someTag . }"
```

the same can be said for updating items. For example, to update the description of an objective, you can use the following command:

```clearhead update objective --id 123 --description "this is an updated description"
```

or to update multiple items at once using a SPARQL query:

```
clearhead update objective --where "{ ?objective a :Objective ; :hasTag :someTag . }" --description "this is an updated description for all objectives with someTag"
```

### Calendar Tasks

One core use case is projecting Actions and recurring Plans into interoperable calendar task resources. ClearHead uses VTODO for both standalone Actions and RRULE-bearing Plan masters. This can be done with a simple command like:

```
clearhead export plans --open-only
```

we also provide helper scripts to do these sorts of operations quickly and easily without needing

### Runtime Configuration

Finally, we also want to allow users to configure certain aspects of the CLI at runtime without needing to edit configuration files. For example, we can allow users to set the default output format for all commands using a simple command like:

```
clearhead update config default_format json
```

This would set the default output format to JSON for all commands that support the `--format` flag. We can also allow users to view their current configuration settings with a command like:

```
clearhead read config
```
