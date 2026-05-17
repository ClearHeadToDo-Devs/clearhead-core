//! RDF graph module for storing and querying actions using Oxigraph.
//!
//! Implements the RDF schema from the Actions Vocabulary v4 ontology.
//! The domain model (Plan, Action, Charter) maps to CCO-aligned classes.
//!
//! # Submodules
//!
//! - [`insert`] — load domain objects into a store
//! - [`jsonld`] — export canonical compact JSON-LD
//! - [`query`]  — SPARQL queries + reconstruction from the store
//! - [`serialize`] — serialize a store or model to Turtle output

pub mod insert;
pub mod jsonld;
pub mod query;
pub mod serialize;

pub use insert::{load_acts_into_store, load_domain_model, load_turtle};
pub use jsonld::{serialize_domain_to_jsonld, serialize_store_to_jsonld};
pub use oxigraph::store::Store;
pub use query::{
    build_raw_where_query, build_where_query, load_domain_model_from_store,
    load_planned_acts_from_store, query_action_ids, query_raw, validate_actions_vocabulary,
};
pub use serialize::{
    dump_store_to_turtle, serialize_acts_to_turtle, serialize_closed_acts_to_turtle,
    serialize_open_acts_to_turtle,
};

/// Result type for graph operations.
pub type Result<T> = std::result::Result<T, GraphError>;

/// Errors that can occur during RDF/SPARQL operations.
#[derive(thiserror::Error, Debug)]
pub enum GraphError {
    /// Error from the underlying Oxigraph store.
    #[error("Database error: {0}")]
    Store(String),
    /// Error parsing RDF (e.g., Turtle).
    #[error("RDF syntax error: {0}")]
    Syntax(String),
    /// Error executing a SPARQL query.
    #[error("SPARQL error: {0}")]
    Query(String),
    /// Error during domain model hydration/mapping.
    #[error("Domain mapping error: {0}")]
    Domain(String),
}

use oxigraph::model::NamedNode;

// ============================================================================
// Namespace constants
// ============================================================================

pub(crate) const ACTIONS_NS: &str = "https://clearhead.us/vocab/actions/v4#";
pub(crate) const CCO_NS: &str = "https://www.commoncoreontologies.org/";
pub(crate) const BFO_NS: &str = "http://purl.obolibrary.org/obo/";
pub(crate) const RDF_NS: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
pub(crate) const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";
pub(crate) const SKOS_NS: &str = "http://www.w3.org/2004/02/skos/core#";

// BFO property identifiers
pub(crate) const BFO_HAS_PART: &str = "BFO_0000051";
pub(crate) const BFO_PART_OF: &str = "BFO_0000050";

// CCO class and property identifiers
pub(crate) const CCO_PLAN: &str = "ont00000974";
pub(crate) const ACTIONS_ACTION: &str = "Action";
pub(crate) const CCO_IS_SUCCESSOR_OF: &str = "ont00001775";
pub(crate) const CCO_PRESCRIBES: &str = "ont00001942";
pub(crate) const CCO_PRESCRIBED_BY: &str = "ont00001920";
pub(crate) const CCO_STATUS_PROP: &str = "ont00001868";

// RDFS property identifiers
pub(crate) const RDFS_LABEL: &str = "label";
pub(crate) const RDFS_COMMENT: &str = "comment";

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

pub(crate) fn rdfs_pred(name: &str) -> NamedNode {
    ns("http://www.w3.org/2000/01/rdf-schema#", name)
}

pub(crate) fn bfo_pred(name: &str) -> NamedNode {
    ns(BFO_NS, name)
}

pub(crate) fn rdf_type() -> NamedNode {
    ns(RDF_NS, "type")
}

pub(crate) fn phase_node(phase: &crate::domain::ActionState) -> NamedNode {
    use crate::domain::ActionState;
    let name = match phase {
        ActionState::NotStarted => "NotStarted",
        ActionState::InProgress => "InProgress",
        ActionState::Completed => "Completed",
        ActionState::BlockedOrAwaiting => "Blocked",
        ActionState::Cancelled => "Cancelled",
    };
    actions_pred(name)
}

// ============================================================================
// Store creation
// ============================================================================

/// Create an in-memory Oxigraph store.
pub fn create_store() -> Result<Store> {
    Store::new().map_err(|e| GraphError::Store(e.to_string()))
}

/// Legacy alias for `create_store`.
pub fn create_database() -> Result<Store> {
    create_store()
}
