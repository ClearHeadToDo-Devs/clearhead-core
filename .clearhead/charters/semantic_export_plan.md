---
title: Semantic Export Plan
alias: semantic_export_plan
---

# Semantic Export Plan

## What This Is Actually About

The graph is now close enough to the v4 ontology contract that we can define a stable
data output format. This charter is about hardening that contract — validation, canonical
JSON-LD shape, and making JSON-LD the default data output when clearhead is used in a
pipeline.

It is not about routing the visual display layer through JSON-LD. The visual layer
(trees, tables) consumes `DomainModel` directly and lives in `clearhead-cli`, not here.

## The Output Architecture

There are two distinct output modes and they serve different consumers:

**Data output** — when stdout is a pipe or redirect:
- Format: canonical JSON-LD
- Consumers: shell pipelines, external tools, web UI, semantic tooling, scripts
- This is where JSON-LD earns its place — rich semantic shape, ontology-aligned,
  useful to anything that can consume JSON

**Visual output** — when stdout is a TTY:
- Format: tree (hierarchy view) or table (list view) depending on command
- Consumers: humans at a terminal
- Comes straight from `DomainModel` — no intermediate format
- Lives entirely in `clearhead-cli`, not in core

The switch between them is TTY detection via `std::io::IsTerminal`. No flags required
for the common case. `--output json` exists as an explicit override.

## The Remaining Core Work

### 1. Expand Validation

The validation that runs before export needs to cover what the graph actually claims.
Minimum useful expansion:

1. Successor-cycle detection
2. Recurrence requires a scheduled act anchor
3. Completed acts require completion dates
4. Context deferral is explicit — contexts are not modeled as proper nodes yet and the
   export should say so rather than silently emitting partial data

The goal is not a generic SHACL engine. The goal is enough coverage that exported data
is trustworthy against the contract we actually claim.

### 2. Declare the JSON-LD Shape as Stable

The export is implemented but not declared. This means:

1. Write down the canonical field names, cardinality rules, and ordering rules as a
   spec, not just tests
2. Snapshot representative example payloads for each major entity type
3. Make the vendored schema the normative reference, not an afterthought

### 3. Context Deferral

Contexts exist in the DSL (`+tag`) and flow through to the graph, but they are not
yet modeled as proper ontology nodes with real identity. Until they are:

- The export contract explicitly defers full context semantics
- `urn:context:<name>` identifiers are provisional
- Do not emit context nodes as if the ontology support is complete

## What Lives in the CLI, Not Here

These are related but belong in `clearhead-cli`:

- TTY detection and output dispatch
- Moving `comfy-table` out of `clearhead-core` (it has no business being in a library)
- Tree renderer for `DomainModel` hierarchy (charter → plan → action → child actions)
- Table renderer for list views
- Wiring JSON-LD as the default pipe output

## Non-Goals

1. Full context modeling in this slice
2. Generic SHACL engine in core
3. Display polish
4. Solving every legacy path at once

## Standard For Success

This phase is done when:

1. Core can export canonical JSON-LD and the shape is a declared contract, not just
   passing tests
2. Validation covers what the contract claims — incomplete data fails loudly
3. The CLI emits JSON-LD by default when piped and the visual layer never leaks into
   the data path
