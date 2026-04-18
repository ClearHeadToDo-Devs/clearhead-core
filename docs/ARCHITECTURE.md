# Architecture

The core library is primarily concerned with the in-memory represenation of the ontology defined in the ontology repo.

That is:
- Objectives
- Charters
- Plans
- Planned Acts

These are the core domain objects and everything we do speaks in terms of these Planned Acts

the rest is various formats that are able to leverage the core domain model to provide different functionality and to keep the domain model separated from the various needs of our usecases

There are three primary modules:
- domain: the in-memory representation of the domain model, including the core structs and the various algorithms that operate on them
- workspace: the various formats that we use to interact with the domain model, including the actions files and the markdown files for charters and objectives
- graph: the graph layer that converts the domain model into RDF triples that can be stored in a graph database and queried with SPARQL all according to the [ontology](https://github.com/ClearHeadToDo-Devs/ontology/blob/main/README.md)
- crdt: the CRDT layer for syncing changes, which is responsible for converting the domain
  - currently deferred in leu of getting the others done first but the skeleton is still there for future contributors if the need for a mobile app arises

## Workspaces

The workspace is the collection of files that are serving as the user interface for the domain model.

now discipline is required to keep this library from taking on more responsibility than needed so what this workspace module does is work on converting the text of these workspace files into and out of the domain model.

However is does NOT handle any of the actual file reading and writing, that is the responsibility of the CLI layer. This allows us to keep the core library focused on the domain model and the various formats that we need to support without getting bogged down in system-level interactions.

This also allows implementors the freedom to implement the structure that makes the most sense for them.

### Actions files

In particular the actions DSL has a few pieces of extra functionality that are worth noting here.

- Formatter to have consistent formatting of the actions files
- Parser to convert the actions files into the domain model and back using tree-sitter and the custom grammar defined in the specifications repo.
- Linter to ensure that the actions files are adhering to the linting specification defined in the specifications repo. this is important to ensure that the actions files are consistent and adhere to the expected structure, while not making the parser overly strict and allowing for some flexibility in how users write their plans.

### Markdown files

Charters and Objectives are assumed to be markdown files that follow the specifications defined in the specifications repo. this library is responsible for parsing those markdown files into the domain model and for converting the domain model back into markdown when needed.
## CRDT for syncing

automerge serves as the CRDT layer for syncing changes. again, a new layer was created to ensure that the requirements of the CRDT are met without polluting the core library with concerns around syncing and merging. and again this layer does not concern itself with file reading and writing, that is the responsibility of the sync servers that will be responsible for implementing the actual syncing functionality.

instead, this library is responsible for converting the domain model into a format that can be easily synced and merged by automerge, and for applying changes from the CRDT back into the domain model.

## RDF and SPARQL

Finally, the RDF layer allows us to translate the domain model into RDF triples that can be stored in an graph database. This allows us to leverage SPARQL for querying the data in a flexible and powerful way, and also to perform SHACL validation against the data to ensure it conforms to our ontology.

the choice of graph db is left up to the implementors but the important part is making sure that the domain model can be easily translated into RDF and that we have a clear way to apply changes from the graph database back into the domain model.
### Module Structure


### Data Flow

```mmd
graph TD
    DSL[Actions DSL] -->|parse file| actions[ActionList]
    actions -->|Transform| domain[Domain Structs]
    domain -->|autosurgeon| crdt[CRDT State]
    domain -->|oxigraph| graph[Graph Store]
    domain -->|format| output[Formatted Output]
```

## Core Types

The Domain model is the core struct of the entire library.

### Testing

Run tests:

```bash
cargo test
```

Run with verbose output:
c
```bash
cargo test -- --nocapture
```

### Snapshots
In addition, RON snapshots are the recommended method for structuring tests such that different datasets can be created according to a golden format

please see the `tests` directory where we load some example fixtures and generates RON snapshots, this allows us to have end-to-end testing persistently on-disk

for changes to the structure of the structs themselves be sure to update the RON

## Contributing

Contributions are welcome! Please ensure:

1. All tests pass (`cargo test`)
2. Code is formatted (`cargo fmt`)
3. No clippy warnings (`cargo clippy`)
4. No environment dependencies (filesystem, network, config)

