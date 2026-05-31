//! RDF graph module — the bridge between the domain model and Oxigraph.
//!
//! This module owns the "put it in / take it out / ask it questions" layer:
//! [`insert`] loads domain objects as RDF triples, [`query`] runs SPARQL and
//! reconstructs domain objects, [`serialize`] writes Turtle for archival, and
//! [`jsonld`] produces canonical JSON-LD interchange.
//!
//! # Named Graph Architecture
//!
//! **All workspace data lives in a named graph, never in `DefaultGraph`.**
//!
//! Every workspace is assigned a stable UUID by `clearhead init`, stored in
//! `.clearhead/config.json`.  That UUID is the workspace's durable identity;
//! the corresponding RDF named graph URI is:
//!
//! ```text
//! urn:clearhead:workspace:<uuid>
//! ```
//!
//! Use [`workspace_graph_uri`] to derive this node and pass it as the
//! `graph_name` argument to [`load_domain_model`].  For ad-hoc or test stores
//! that have no real workspace UUID, use [`TRANSIENT_GRAPH_URI`] — it is a
//! valid named graph URI so `GRAPH ?g` patterns still find data.
//!
//! `GraphName::DefaultGraph` is **only** legitimate in two places:
//!
//! | Call site | Why DefaultGraph is correct |
//! |-----------|-----------------------------|
//! | [`load_acts_into_store`] | Single-use store written directly to Turtle (archival).  Never queried via SPARQL. |
//! | [`serialize`] module internals | Same — transient TTL-output stores. |
//!
//! Everywhere else — CLI query paths, tests, multi-workspace loading — use a
//! named graph.  Tests that load into `DefaultGraph` and query via SPARQL are
//! testing a configuration that never occurs in production and will give false
//! greens for the class of bug fixed in commit `bab5769`.
//!
//! # SPARQL Evaluator Behaviour
//!
//! Oxigraph's `SparqlEvaluator::new()` defaults to querying only
//! `GraphName::DefaultGraph`, which is always empty in workspace stores.
//! [`query_raw`] and friends call
//! `prepared.dataset_mut().set_default_graph_as_union()` before binding to
//! the store, so **triple patterns without an explicit `GRAPH` clause match
//! across all named graphs** (the union default graph).
//!
//! This has two practical consequences for query authors:
//!
//! ## Omitting `GRAPH ?g`
//!
//! ```sparql
//! SELECT ?name WHERE {
//!     ?act a actions:Action ; rdfs:label ?name .
//! }
//! ```
//!
//! This works and is the recommended style for single-workspace queries.  All
//! named queries in `clearhead-cli/src/queries/*.sparql` are written this way.
//!
//! ## Using `GRAPH ?g` explicitly
//!
//! ```sparql
//! SELECT ?g ?name WHERE {
//!     GRAPH ?g {
//!         ?act a actions:Action ; rdfs:label ?name .
//!     }
//! }
//! ```
//!
//! Use this when you need the graph URI in results (e.g. to identify which
//! workspace an action came from in a multi-workspace query) or when you want
//! to scope a query to one specific workspace by binding `?g` to a known URI.
//!
//! ## `FROM` / `FROM NAMED`
//!
//! When a query declares its own dataset (`FROM <uri>` or `FROM NAMED <uri>`),
//! the evaluator honours those clauses and does **not** apply the union default
//! graph override — `is_default_dataset()` returns false and the explicit
//! declaration is used as-is.
//!
//! # Multi-Workspace Stores
//!
//! When `WorkspaceConfig::additional_workspaces` is non-empty, the CLI loads
//! each additional workspace into the **same store** under its own named graph.
//! Because each workspace occupies a separate `urn:clearhead:workspace:<uuid>`
//! named graph, queries without `GRAPH` automatically span all workspaces
//! (union default graph), and queries that bind `?g` can distinguish sources.
//!
//! # Writing Tests
//!
//! Tests that exercise SPARQL queries must use a named graph — they should
//! mirror the production code path:
//!
//! ```rust
//! # use clearhead_core::graph::{create_store, load_domain_model, GraphName, TRANSIENT_GRAPH_URI};
//! # fn make_model() -> clearhead_core::DomainModel { clearhead_core::DomainModel { objectives: vec![], charters: vec![] } }
//! let store = create_store().unwrap();
//! let graph = GraphName::NamedNode(
//!     oxigraph::model::NamedNode::new(TRANSIENT_GRAPH_URI).unwrap()
//! );
//! let model = make_model();
//! load_domain_model(&store, &model, None, graph).unwrap();
//! // now query with query_raw / query_action_ids — they use union default graph
//! ```
//!
//! Using `GraphName::DefaultGraph` in tests is only correct when testing the
//! archive/serialization path (`load_acts_into_store` / `serialize` module).
//!
//! # Submodules
//!
//! - [`insert`]    — domain model → RDF triples; loads into named graph
//! - [`query`]     — SPARQL query execution and result extraction
//! - [`jsonld`]    — canonical compact JSON-LD export
//! - [`serialize`] — Turtle serialization for archival (uses DefaultGraph internally)

pub mod insert;
pub mod jsonld;
pub mod query;
pub mod serialize;

pub use insert::{insert_workspace_metadata, load_acts_into_store, load_domain_model, load_turtle, load_turtle_into_graph};
pub use oxigraph::model::GraphName;
pub use jsonld::{serialize_domain_to_jsonld, serialize_workspace_to_jsonld};
pub use oxigraph::store::Store;
pub use query::{
    build_raw_where_query, build_where_query, query_action_ids, query_raw,
    validate_actions_vocabulary,
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
pub(crate) const WORKSPACE_NS: &str = "https://clearhead.us/vocab/workspace/v1#";
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

/// URI prefix for all workspace named graphs.
pub const WORKSPACE_GRAPH_PREFIX: &str = "urn:clearhead:workspace:";

/// Derive the named graph URI for a workspace from its UUID string.
///
/// The resulting URI is `urn:clearhead:workspace:<uuid>`.  Every workspace
/// must have a stable UUID (written to `.clearhead/config.json` by
/// `clearhead init`) so that its named graph identity is durable.
pub fn workspace_graph_uri(uuid: &str) -> NamedNode {
    NamedNode::new(format!("{}{}", WORKSPACE_GRAPH_PREFIX, uuid)).unwrap()
}

/// Named graph used by transient in-memory stores (ad-hoc queries, tests).
/// Not persisted; exists only so GRAPH ?g queries find data in single-use stores.
pub const TRANSIENT_GRAPH_URI: &str = "urn:clearhead:workspace:transient";

/// Create an in-memory Oxigraph store.
pub fn create_store() -> Result<Store> {
    Store::new().map_err(|e| GraphError::Store(e.to_string()))
}

