---
title: Semantic Export Plan
alias: semantic_export_plan
---

# Semantic Export Plan

## Why This Exists

We have spent the recent core and ontology work moving the internal graph closer to the actual v4 ontology contract.

That work matters because the next layer should not be built on top of Rust struct dumps or ad hoc formatter assumptions. It should be built on top of a semantic seam that is already close to ontology truth.

The immediate purpose of this note is to capture the next phase clearly:

1. harden the graph-backed semantic seam
2. define the canonical JSON-LD export contract
3. build display on top of that export instead of directly on `DomainModel`

## What Is True Now

The following semantic cleanups are already in place:

1. plan-level objective holdover removed
2. duration moved from `Plan` to `PlannedAct`
3. dependency semantics moved to `cco:is_successor_of`
4. recurrence semantics moved to `actions:hasRecurrenceRule`
5. sequential-children semantics moved to `actions:hasSequentialChildren`
6. graph writes use canonical UUID / alias / label / containment predicates

This means the graph is finally becoming a real candidate for the stable semantic seam.

## The Next Phase

### 1. Expand Validation

Before export becomes public contract, validation needs to cover the semantics we now claim to support.

The minimum useful expansion is:

1. successor-cycle detection
2. recurrence requires a scheduled act anchor
3. completed acts require completion dates
4. UUID and alias shape checks
5. sequential-children boolean checks

The goal is not full generic SHACL execution inside core. The goal is enough parity that exported semantics are trustworthy.

### 2. Define Canonical JSON-LD

We want one semantic JSON, not competing JSON formats.

The export should be:

1. graph-derived
2. compacted through the ontology context
3. deterministic in ordering
4. pleasant enough for both tests and downstream code

That means we should not just dump triples or arbitrary JSON-LD serializer output and call it done.

We need to settle:

1. graph ordering rules
2. field cardinality rules
3. reference shape conventions
4. what subset is canonical now versus deferred

### 3. Make Context Deferral Explicit

Contexts are clearly real domain concepts, but they are not yet first-class in core.

Until they are modeled properly, the export contract should say so plainly.

The key rule is:

Do not fake ontology support just because the DSL has `+context` shorthand.

We either model contexts as proper nodes and references, or we defer them from the canonical export surface.

### 4. Build Display On Top Of Export

Once JSON-LD is stable, display work should consume that exported semantic shape.

That gives us:

1. one semantic contract
2. display decoupled from Rust internals
3. more meaningful e2e tests
4. less formatter-specific drift over time

## Proposed Delivery Order

1. expand validation in core
2. write the canonical JSON-LD contract down with sample payloads
3. implement graph -> JSON-LD export in core
4. snapshot semantic examples
5. build `outline` and `table` over the export shape

## Non-Goals For This Slice

1. full context modeling
2. generic SHACL engine in core
3. final display polish
4. solving every legacy path at once

## Standard For Success

We should consider this phase successful when:

1. core can export one canonical JSON-LD view of the semantic graph
2. that export validates against the ontology contract we actually claim to support
3. display can start consuming that export without needing Rust-specific knowledge

At that point, the system will finally have a clean semantic waist between storage/runtime code and human-facing CLI display.
