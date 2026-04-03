//! RDF graph module for storing and querying actions using Oxigraph.
//!
//! Implements the RDF schema from the Actions Vocabulary v4 ontology.
//! The domain model (Plan, PlannedAct, Charter) maps to CCO-aligned classes.
//!
//! # Submodules
//!
//! - [`insert`] — load domain objects into a store
//! - [`query`]  — SPARQL queries + reconstruction from the store
//! - [`serialize`] — serialize a store or model to Turtle output

pub mod insert;
pub mod query;
pub mod serialize;

pub use insert::{load_acts_into_store, load_domain_model, load_turtle};
pub use oxigraph::store::Store;
pub use query::{
    build_raw_where_query, build_where_query, query_actions, query_acts, query_plans, query_raw,
    validate_actions_vocabulary,
};
pub use serialize::{
    dump_store_to_turtle, serialize_acts_to_turtle, serialize_closed_acts_to_turtle,
    serialize_open_acts_to_turtle,
};

use oxigraph::model::NamedNode;

// ============================================================================
// Namespace constants
// ============================================================================

pub(crate) const ACTIONS_NS: &str = "https://clearhead.us/vocab/actions/v4#";
pub(crate) const CCO_NS: &str = "https://www.commoncoreontologies.org/";
pub(crate) const BFO_NS: &str = "http://purl.obolibrary.org/obo/";
pub(crate) const SCHEMA_NS: &str = "http://schema.org/";
pub(crate) const RDF_NS: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
pub(crate) const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";
pub(crate) const SKOS_NS: &str = "http://www.w3.org/2004/02/skos/core#";

// BFO property identifiers
pub(crate) const BFO_HAS_PART: &str = "BFO_0000051";

// CCO class and property identifiers
pub(crate) const CCO_PLAN: &str = "ont00000974";
pub(crate) const CCO_PLANNED_ACT: &str = "ont00000228";
pub(crate) const CCO_PRESCRIBES: &str = "ont00001942";
pub(crate) const CCO_STATUS_PROP: &str = "ont00001868";

// ============================================================================
// Shared NamedNode helpers (used by all submodules)
// ============================================================================

pub(crate) fn ns(base: &str, name: &str) -> NamedNode {
    NamedNode::new(format!("{}{}", base, name)).unwrap()
}

pub(crate) fn actions_pred(name: &str) -> NamedNode {
    ns(ACTIONS_NS, name)
}

pub(crate) fn cco_node(id: &str) -> NamedNode {
    ns(CCO_NS, id)
}

pub(crate) fn schema_pred(name: &str) -> NamedNode {
    ns(SCHEMA_NS, name)
}

pub(crate) fn bfo_pred(name: &str) -> NamedNode {
    ns(BFO_NS, name)
}

pub(crate) fn rdf_type() -> NamedNode {
    ns(RDF_NS, "type")
}

pub(crate) fn phase_node(phase: &crate::domain::ActPhase) -> NamedNode {
    use crate::domain::ActPhase;
    let name = match phase {
        ActPhase::NotStarted => "NotStarted",
        ActPhase::InProgress => "InProgress",
        ActPhase::Completed => "Completed",
        ActPhase::Blocked => "Blocked",
        ActPhase::Cancelled => "Cancelled",
    };
    actions_pred(name)
}

// ============================================================================
// Store creation
// ============================================================================

/// Create an in-memory Oxigraph store.
pub fn create_store() -> Result<Store, String> {
    Store::new().map_err(|e| e.to_string())
}

/// Legacy alias for `create_store`.
pub fn create_database() -> Result<Store, String> {
    create_store()
}
