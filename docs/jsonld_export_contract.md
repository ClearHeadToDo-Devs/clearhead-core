# Canonical JSON-LD Export Contract

**Status:** Stable  
**Version:** v4 (aligned with `actions.context.v4.json` and `actions.schema.v4.json`)

## Overview

`serialize_domain_to_jsonld(model: &DomainModel) -> Result<String>` in
`clearhead-core/src/graph/jsonld.rs` is the single authoritative export path.
Output is compact JSON-LD validated against the vendored schema at test time.

## Document Shape

```json
{
  "@context": { ... },  // vendored actions.context.v4.json @context value
  "_meta": {            // present only when context nodes exist (see Deferral below)
    "context_nodes": {
      "status": "provisional",
      "count": <n>,
      "note": "Context nodes use urn:context:<name> provisional URNs. ..."
    }
  },
  "@graph": [           // array of typed nodes, deterministically sorted
    { Charter nodes },
    { Context nodes },
    { Plan nodes },
    { Action nodes }
  ]
}
```

## Node Types and Required Fields

### Charter

| Field | Value |
|---|---|
| `@id` | `urn:uuid:<uuid>` |
| `@type` | `Charter` |
| `name` | string |
| `description` | string (optional) |
| `alias` | string (optional) |
| `hasPart` | array of child charter `@id`s (optional) |

### Plan

| Field | Value |
|---|---|
| `@id` | `urn:uuid:<uuid>` |
| `@type` | `Plan` |
| `name` | string |
| `description` | string (optional) |
| `partOf` | charter `@id` |
| `prescribes` | array of action `@id`s (optional) |
| `hasRecurrenceRule` | RRULE string without `R:` prefix (optional) |

### Action

| Field | Value |
|---|---|
| `@id` | `urn:uuid:<uuid>` |
| `@type` | `Action` |
| `name` | string |
| `description` | string (optional) |
| `hasStatus` | one of `NotStarted` · `InProgress` · `Completed` · `Blocked` · `Cancelled` |
| `hasScheduledDateTime` | ISO 8601 string (optional) |
| `hasDueDateTime` | ISO 8601 string (optional) |
| `hasCompletedDateTime` | ISO 8601 string — **required when status is `Completed`** |
| `hasPriority` | integer 1–5 (optional) |
| `hasAlias` | string (optional, must be unique per graph) |
| `isSequential` | boolean (optional) |
| `requiresContext` | array of context `@id`s (optional) |
| `is_successor_of` | array of predecessor action `@id`s (optional) |

### Context (provisional)

| Field | Value |
|---|---|
| `@id` | `urn:context:<normalized-name>` — **provisional, not ontology-declared** |
| `@type` | `Context` |
| `name` | string |
| `contextIdentifier` | string |

## Sort Order

Nodes in `@graph` are sorted by type priority, then by `@id` within each type:

```
Charter → Context → Plan → Action
```

## Graph Validation Contract

`validate_actions_vocabulary(store: &Store)` runs the following checks. Each
failure appends a human-readable violation string.

| Shape | Rule |
|---|---|
| `ActionStatusShape` | Every `Action` must have a `hasStatus` value |
| `ActionStatusShape (sh:in)` | Status must be one of the five valid values |
| `PlanPrescribesShape` | `prescribes` target must be typed as `Action` |
| `UUIDShape` | Every `Action` and `Plan` must carry a `hasUUID` literal |
| `CompletedDateShape` | `Action` with status `Completed` must have `hasCompletedDateTime` |
| `RecurrenceAnchorShape` | `Plan` with `hasRecurrenceRule` must have `hasScheduledDateTime` |
| `SuccessorCycleShape` | An `Action` must not be its own successor (direct self-loop) |
| `AliasUniquenessShape` | Two actions in the same named graph must not share an alias |

## Context Node Deferral

Context tags (`+tag` in the DSL) are exported as provisional `Context` nodes
with `urn:context:<name>` identifiers. These URNs are **not declared in the v4
ontology** — full context semantics (SKOS concept scheme, real IRIs, class
hierarchy) are explicitly deferred.

When context nodes are present, the export includes a `_meta.context_nodes`
block describing this deferral. Consumers **must not** treat `urn:context:*`
identifiers as stable cross-document identity.

## Snapshots and Testing

- `clearhead-core/src/resources/actions.context.v4.json` — vendored context
- `clearhead-core/src/resources/actions.schema.v4.json` — vendored JSON Schema
- `clearhead-core/src/resources/ontology-out.example.v4.jsonld` — example output

The test `exported_jsonld_validates_against_vendored_schema` in `graph::jsonld`
validates real export output against the vendored schema at every test run.
