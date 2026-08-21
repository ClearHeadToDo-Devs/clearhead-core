# clearhead-graphd

Standalone graph/query tool and graph-runtime library for ClearHead. It owns Oxigraph, RDF materialization, SPARQL execution, query registration, shape validation, and JSON-LD serialization. `clearhead-core` remains the graph-neutral domain/workspace substrate. graphd disables Core's `formatting` feature because its read-only runtime neither writes `.actions` files nor needs Topiary or Tokio.

The first implementation is deliberately one-shot rather than a resident daemon. It discovers ClearHead configuration through core and can be installed and used without the `clearhead` CLI.

## Query interface

Run a built-in index view:

```sh
clearhead-graphd --workspace /path/to/project query index unscheduled
clearhead-graphd --workspace /path/to/project query index unscheduled --format ids
clearhead-graphd --workspace /path/to/project query tree work-map
clearhead-graphd --workspace /path/to/project query graph dependencies \
  --format jsonld
clearhead-graphd --workspace /path/to/project query graph dependencies \
  --format dot
```

Inspect and run registered queries:

```sh
clearhead-graphd --workspace /path/to/project query list
clearhead-graphd --workspace /path/to/project query show agenda
clearhead-graphd --workspace /path/to/project query named high-priority
```

Run ad-hoc SPARQL or a raw `WHERE` clause:

```sh
clearhead-graphd --workspace /path/to/project query raw \
  'SELECT ?name WHERE { ?action rdfs:label ?name }'
clearhead-graphd --workspace /path/to/project query raw \
  --where '?action rdfs:label ?name'
```

Use `query <command> --help` for parameters such as `--status`, `--target`, and `--format`.

### Query discovery

Queries are resolved from these layers, with the more local definition taking precedence:

- graphd's built-in registry
- the user's ClearHead config directory under `queries/`
- `<workspace>/.clearhead/queries/`

Index queries live in an `index/` subdirectory. graphd loads the primary workspace plus configured `additional_workspaces` into separate named graphs.

### Output

Output is destination-aware. A terminal defaults to a human rendering. A pipe uses the query family's machine projection: index views emit NDJSON, while unrestricted `SELECT` queries emit JSON row arrays. Index views validate their addressable-row contract before any projection. Explicit `--format table`, `json`, `ndjson`, `jsonld`, `ids`, `turtle`, and `dot` override detection where supported. The index-only `ids` projection emits each canonical identity unchanged, one per line in query order, for JSON-parser-free composition.

The semantic rules for index output are documented in [`docs/query_contract.md`](docs/query_contract.md). Exact JSON-LD fields are in [`docs/jsonld_export_contract.md`](docs/jsonld_export_contract.md).

Clients invoke this public command interface directly; the `clearhead` CLI does not proxy graph queries.

## Domain JSON to JSON-LD export

`export-jsonld` is the remaining stdin protocol. It reads a JSON-encoded `DomainModel` and writes canonical JSON-LD:

```sh
clearhead-graphd export-jsonld < domain-model.json
```

Warnings and errors go to stderr. A failed command exits non-zero, and callers must not consume stdout.

## Development

```sh
cargo test --manifest-path clearhead-graphd/Cargo.toml
```
