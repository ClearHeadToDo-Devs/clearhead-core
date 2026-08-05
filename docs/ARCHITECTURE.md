# architecture overview

Attempts have been made to make the CLI as thin as possible.

We do this by making many of the _core_ functionalities availabile through the `clearhead-core` crate, which is a shared Rust library that handles all shared concerns between this project and other projects that work with the workspaces. This allows us to keep the CLI focused on the user interface and how it leverages core functionality as it becomes available.

This allows for a layer of control between core and the cli but also allows the
cli to own how the different possibilities become well

## Conceptual Model

The **Rust struct (IR) is the canonical representation**. Everything else is a view or persistence mechanism.

- Workspace is all the plaintext files that users interact with directly. These files are the durable source of truth and are parsed into the IR.
- analytics tools such as our own graphd are expected to read the workspace
files like any other consumer to get their information, which is why much of
this functionality is within core so that all tools can leverage the same
language of configuration and workspace layout

## Workspace Architecture

Per the [Process Specification][Process Specification], cli leverages our [Naming Conventions][Naming Conventions] to actually discover and build the proper domain model.

```mmd
---
config:
  theme: redux
---
flowchart BT
    charters[Charters] <--many-to-many--> objectives[Objectives]
    actions[Actions] --References--> charters 
    plans[Plans] --References--> charters 
    actions[Actions] <--May Reference--> plans
    sidecar[JSON Sidecar]--watches-->actions
    sidecar[JSON Sidecar]--watches-->charters
```

### File Format Distinctions

In particular, its important to know how the different domain models translate to different files within the workspace:

- `Objectives` -> `.md` files within the `objectives/` directory per our and following the [Objectives File Specification][Objectives File Specification]
- `Charters` -> `.md` files within the workspace root or any subdirectory and is written according to the [Charter File Specification][Charter File Specification]
- `Plans` -> `.ics` files that conform to the VTODO standard
- `Planned Acts` -> `.actions` files that conform to the action file specification
- `Sidecar` -> finally, we have a data sidecar in the form of a JSON that is
intended to capture data that does not currently belong in the Actions DSL and
scoped to one sidecar per charter

These four file formats come together to allow us to form and update the `DomainModel` in memory, which will enable the core of what we are doing here

Everything uses this model, from the CLI to the LSP server and UI rendering. This is the heart of our architecture, and it all relies on these file formats being properly defined and adhered to.

#### File Conversions

What needs to be known is that converting between different file formats is a core part of the architecture and we are intending to participate in delivering functionality by supporting the following conversions:

- Plan DSL (Action files)
- Markdown (Objectives and Charters)
- VTODO (for recurring Plans and standalone Action projections)
- JSON for sidecars

By getting these structures in place we can easily deliver functionality by simply making different structures available in different formats

#### Calendar Export Boundary

When plans are exported as `.ics` files, the CLI writes them to:

```text
$XDG_DATA_HOME/clearhead/plans/<charter-slug>/<plan-uid>.ics
```

This is an **output boundary** — the CLI's responsibility ends at writing a valid iCalendar file to that path. Sync to external calendar systems (Google Calendar, CalDAV servers, etc.) is handled entirely by external tooling (e.g., vdirsyncer). The CLI has no dependency on any sync tool and makes no assumptions about what, if anything, consumes these files.

This is intentional. Keeping sync out of the CLI means:

- the CLI remains testable without network or credentials
- operators choose their own sync strategy
- the `.ics` files are usable standalone by any CalDAV-aware tool

Project-local workspaces (those with a `.clearhead/` directory at the project root) write plans to `.clearhead/plans/` within that project. These are development workspace files and are not expected to be in the personal calendar sync path.

## Reference

[Process Specification]: https://github.com/ClearHeadToDo-Devs/specifications/blob/master/process.md
[Naming Conventions]: https://github.com/ClearHeadToDo-Devs/specifications/blob/master/naming_conventions.md
[Charter File Specification]: https://github.com/ClearHeadToDo-Devs/specifications/blob/master/charters.md
[Objectives File Specification]: https://github.com/ClearHeadToDo-Devs/specifications/blob/master/objectives.md
