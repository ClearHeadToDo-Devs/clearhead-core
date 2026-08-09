# Query Output Contract

**Status:** Stable

## Purpose

A query contract is a guarantee between applications and agents. It says that a
query in a named family returns enough standard SPARQL structure for any
consumer of that family to use it without inspecting or rewriting the query.
It is not a ClearHead query language.

Every saved query remains a complete, portable `.sparql` file. It can be opened,
inspected, tested, and run unchanged with ordinary SPARQL tooling. ClearHead
adds conventions around those queries: family placement, result validation,
and convenient output projections.

## Query families

A query opts into a contract by living in the corresponding directory and being
run through that family:

```text
queries/index/agenda.sparql
queries/tree/work-map.sparql
queries/graph/dependencies.sparql
```

No YAML header, custom directive, embedded schema, or proprietary query syntax
is required.

### `index`

An index is an ordered `SELECT` result whose rows are addressable and
navigable. Every row guarantees:

- `id` — canonical identity used by mutation verbs
- `name` — display label
- `status` — application state
- `source_file`, `source_line`, `charter_root` — local projection locator

A query may project additional fields such as `priority`, `scheduled_at`,
`due_date`, or `parent`. Its `ORDER BY` determines row order. graphd validates
the required bindings after execution and fails clearly rather than repairing a
query that does not satisfy the family.

This is the contract consumed by agenda views, Neovim quickfix lists, and agents
performing read → act-by-id → re-read loops.

### `tree`

A tree is an ordered `SELECT` result of identifiable nodes with a hierarchical
identity edge. Every row guarantees `id`, `name`, and `kind`; non-root rows also
bind `parent` to the canonical `id` of another row in the same result. Duplicate
identities, missing parents, self-parenting, and cycles fail validation. The
query remains responsible for membership and ordering; graphd projects the
flat bindings as nested JSON or an indented terminal tree. The built-in
`work-map.sparql` proves the family over charters and actions.

### `graph`

A graph is a standard `CONSTRUCT` query. Its returned RDF graph is the contract:
subjects, predicates, and objects preserve their ontology meaning without being
flattened into an application-specific row schema. Machine output defaults to
JSON-LD; explicit Turtle is available for standard RDF interchange, and a
terminal receives a triple/subject/predicate summary. DOT is an explicit
visualization projection: graphd builds a petgraph network from the CONSTRUCT
result and serializes it for Graphviz without replacing RDF as the semantic
contract. A `SELECT` saved in the graph family fails clearly. The built-in
`dependencies.sparql` proves the family over actions, states, charter membership,
and predecessor edges.

## Unrestricted queries

Raw SPARQL and ordinary named queries do not opt into one of these application
contracts. A `SELECT` returns its projected bindings, including aggregates; a
`CONSTRUCT` returns RDF. The query itself defines the content.

An aggregate is not a separate shape. It is still a row/list result for
presentation purposes, but unlike an index it does not promise canonical
identity or locators.

## Shape and destination

The query family determines the useful machine representation. Destination
selects human rendering versus machine emission:

| Family | Terminal | Pipe or redirect |
| --- | --- | --- |
| `index` | table | NDJSON, one entry per line |
| `tree` | indented tree | nested JSON |
| `graph` | graph summary | JSON-LD |
| unrestricted `SELECT` | table | JSON row array |

Explicit format flags override destination detection. `jsonld` preserves the
semantic document for consumers that need it; `json`, `ndjson`, `ids`, `dot`,
and `table` provide shallower projections where the selected query form
supports them. `ids` is defined for the index family and emits each row's
canonical `id` binding unchanged, one per line in query order. It is the small
composition interface for consumers that need identity without parsing JSON.
Empty machine results remain valid structured output: no NDJSON or ID records,
`[]` for JSON rows, or an empty JSON-LD graph.

## Identity and ordering

Contracted nodes carry canonical identity. In JSON-LD this is semantically
`@id`; contexts may compact it to the client-facing `id` key. It is never a
presentation-local identifier. The `ids` projection preserves this complete
value—for example, `urn:uuid:…`—rather than shortening it to a UUID or alias.

Ordered `SELECT` families preserve SPARQL `ORDER BY` in their emitted sequence.
Queries must also project the relevant sort keys when consumers need to recover
order after loading the result into an RDF store, where array order is not
semantic.

## Semantic depth

JSON-LD is graphd's semantic representation and the federation boundary where
meaning must travel with the data. It is not mandatory stdout for every
consumer: validated index and tree results may be projected into NDJSON or
nested JSON without changing the underlying query contract. Consumers choose
the least depth they need.

## The application seam

graphd is stateless. It executes and validates queries; mutation remains a
separate set of verbs addressed by canonical identity. A living view is client
composition:

```text
read → act by canonical id → re-read
```

graphd does not know about quickfix lists, pickers, widgets, or agent protocols.
Those clients depend on family guarantees and map the same result onto their own
interfaces.
