//! End-to-end RDF roundtrip tests for the Actions Vocabulary v4.1.0.
//!
//! These tests verify that:
//! 1. Parsing an `.actions` DSL string and loading it into an RDF store produces
//!    valid RDF that passes key SHACL constraints (implemented as SPARQL).
//! 2. Serializing to Turtle and reloading preserves the constraint-valid structure.
//! 3. Vendored example TTL files from the ontology repository also pass the same
//!    SHACL-as-SPARQL checks.
//!
//! # SHACL validation approach
//!
//! Oxigraph does not support SHACL natively. The shapes from `shapes.ttl` are
//! implemented as SPARQL SELECT queries in `graph::validate_actions_vocabulary`.
//! A passing store produces an empty violations list.
//!
//! # Vendored fixtures
//!
//! `tests/fixtures/` contains copies of selected example TTL files from the
//! `ontology/examples/v4/valid/` directory. Keeping them alongside the tests
//! ensures the test suite is self-contained and doesn't depend on the submodule path.

use clearhead_core::{graph, parse_domain_model};

// ---------------------------------------------------------------------------
// Vendored fixture files
// ---------------------------------------------------------------------------

/// `ontology/examples/v4/valid/simple-plan-and-act.ttl`
/// Minimal Plan + PlannedAct with cco:prescribes and cco:is_measured_by_nominal.
const SIMPLE_PLAN_TTL: &str = include_str!("fixtures/simple-plan-and-act.ttl");

/// `ontology/examples/v4/valid/all-phase-states.ttl`
/// One Plan+Act pair per status (NotStarted, InProgress, Completed, Blocked, Cancelled).
const ALL_PHASES_TTL: &str = include_str!("fixtures/all-phase-states.ttl");

// ---------------------------------------------------------------------------
// DSL → Store → validate (no serialization step)
// ---------------------------------------------------------------------------

#[test]
fn test_basic_plan_passes_shacl() {
    let model = parse_domain_model("[ ] Buy milk").unwrap();
    let store = graph::create_store().unwrap();
    graph::load_domain_model(&store, &model).unwrap();

    let violations = graph::validate_actions_vocabulary(&store).unwrap();
    assert!(
        violations.is_empty(),
        "Expected no SHACL violations, got:\n  - {}",
        violations.join("\n  - ")
    );
}

#[test]
fn test_all_phase_states_pass_shacl() {
    // DSL symbols: [ ] = NotStarted, [-] = InProgress, [x] = Completed, [=] = Blocked, [_] = Cancelled
    let dsl =
        "[ ] Not started task\n[-] In progress task\n[x] Completed task\n[=] Blocked task\n[_] Cancelled task";
    let model = parse_domain_model(dsl).unwrap();
    let store = graph::create_store().unwrap();
    graph::load_domain_model(&store, &model).unwrap();

    let violations = graph::validate_actions_vocabulary(&store).unwrap();
    assert!(
        violations.is_empty(),
        "Expected no SHACL violations for all 5 phases, got:\n  - {}",
        violations.join("\n  - ")
    );
}

#[test]
fn test_multiple_plans_pass_shacl() {
    let dsl = "[ ] Task one\n[ ] Task two\n[ ] Task three";
    let model = parse_domain_model(dsl).unwrap();
    let store = graph::create_store().unwrap();
    graph::load_domain_model(&store, &model).unwrap();

    let violations = graph::validate_actions_vocabulary(&store).unwrap();
    assert!(
        violations.is_empty(),
        "SHACL violations for multiple plans:\n  - {}",
        violations.join("\n  - ")
    );
}

// ---------------------------------------------------------------------------
// Full roundtrip: DSL → DomainModel → Turtle → reload → validate
// ---------------------------------------------------------------------------

#[test]
fn test_roundtrip_basic() {
    let model = parse_domain_model("[ ] Buy milk").unwrap();

    // Step 1: serialize to Turtle
    let turtle = graph::serialize_acts_to_turtle(&model).unwrap();
    assert!(!turtle.is_empty(), "Serialized Turtle should not be empty");

    // Step 2: reload the Turtle into a fresh store
    let store2 = graph::create_store().unwrap();
    graph::load_turtle(&store2, &turtle).unwrap();

    // Step 3: validate the reloaded store
    let violations = graph::validate_actions_vocabulary(&store2).unwrap();
    assert!(
        violations.is_empty(),
        "Roundtrip violations for basic plan:\n  - {}",
        violations.join("\n  - ")
    );
}

#[test]
fn test_roundtrip_all_phases() {
    let dsl = "[ ] Not started\n[-] In progress\n[x] Completed\n[=] Blocked\n[_] Cancelled";
    let model = parse_domain_model(dsl).unwrap();

    let turtle = graph::serialize_acts_to_turtle(&model).unwrap();
    let store2 = graph::create_store().unwrap();
    graph::load_turtle(&store2, &turtle).unwrap();

    let violations = graph::validate_actions_vocabulary(&store2).unwrap();
    assert!(
        violations.is_empty(),
        "Roundtrip violations for all-phases DSL:\n  - {}",
        violations.join("\n  - ")
    );
}

#[test]
fn test_roundtrip_turtle_contains_correct_uris() {
    let model = parse_domain_model("[ ] Verify URIs").unwrap();
    let turtle = graph::serialize_acts_to_turtle(&model).unwrap();

    // CCO Plan class
    assert!(
        turtle.contains("ont00000974"),
        "Turtle missing CCO Plan URI (ont00000974)"
    );
    // CCO PlannedAct class
    assert!(
        turtle.contains("ont00000228"),
        "Turtle missing CCO PlannedAct URI (ont00000228)"
    );
    // CCO prescribes property
    assert!(
        turtle.contains("ont00001942"),
        "Turtle missing CCO prescribes URI (ont00001942)"
    );
    // CCO is_measured_by_nominal property
    assert!(
        turtle.contains("ont00001868"),
        "Turtle missing CCO status property URI (ont00001868)"
    );
    // NotStarted status individual
    assert!(
        turtle.contains("NotStarted"),
        "Turtle missing actions:NotStarted individual"
    );
    // Actions vocabulary namespace
    assert!(
        turtle.contains("clearhead.us/vocab/actions/v4"),
        "Turtle missing actions vocabulary namespace"
    );
    // CCO namespace
    assert!(
        turtle.contains("commoncoreontologies.org"),
        "Turtle missing CCO namespace"
    );
}

// ---------------------------------------------------------------------------
// Vendored example TTL files — validate against SHACL-as-SPARQL
// ---------------------------------------------------------------------------

#[test]
fn test_vendored_simple_example_passes_shacl() {
    let store = graph::create_store().unwrap();
    graph::load_turtle(&store, SIMPLE_PLAN_TTL).unwrap();

    let violations = graph::validate_actions_vocabulary(&store).unwrap();
    assert!(
        violations.is_empty(),
        "SHACL violations in simple-plan-and-act.ttl:\n  - {}",
        violations.join("\n  - ")
    );
}

#[test]
fn test_vendored_all_phases_example_passes_shacl() {
    let store = graph::create_store().unwrap();
    graph::load_turtle(&store, ALL_PHASES_TTL).unwrap();

    let violations = graph::validate_actions_vocabulary(&store).unwrap();
    assert!(
        violations.is_empty(),
        "SHACL violations in all-phase-states.ttl:\n  - {}",
        violations.join("\n  - ")
    );
}

// ---------------------------------------------------------------------------
// Structural: verify Turtle output has the right content on reload
// ---------------------------------------------------------------------------

#[test]
fn test_vendored_all_phases_fixture_covers_all_statuses() {
    // Sanity check: the fixture file itself mentions each of the 5 status values.
    // Combined with test_vendored_all_phases_example_passes_shacl, this confirms
    // the fixture is a complete, valid example of all status states.
    for status in ["NotStarted", "InProgress", "Completed", "Blocked", "Cancelled"] {
        assert!(
            ALL_PHASES_TTL.contains(status),
            "Fixture file all-phase-states.ttl is missing status '{status}'"
        );
    }
}

#[test]
fn test_roundtrip_preserves_plan_name() {
    let plan_name = "My specific plan name";
    let dsl = format!("[ ] {plan_name}");
    let model = parse_domain_model(&dsl).unwrap();

    let turtle = graph::serialize_acts_to_turtle(&model).unwrap();
    assert!(
        turtle.contains(plan_name),
        "Serialized Turtle should contain the plan name '{plan_name}'"
    );

    // Load the Turtle back and verify through SHACL (structure is preserved)
    let store2 = graph::create_store().unwrap();
    graph::load_turtle(&store2, &turtle).unwrap();
    let violations = graph::validate_actions_vocabulary(&store2).unwrap();
    assert!(violations.is_empty(), "Violations after name roundtrip: {:?}", violations);
}
