//! Canonical RDF projection of the domain model.
//!
//! RDF is the deterministic *publication* of the validated plaintext workspace,
//! not a second write model or an embedded database. This module owns the one
//! direction that matters here — domain model → RDF quads — plus the
//! serializations of that one dataset. It depends only on [`oxrdf`] (term model)
//! and [`oxttl`] (Turtle/TriG/N-Quads text); the Oxigraph store and SPARQL live
//! outside Core, behind the optional CLI `sparql` feature.
//!
//! # Named graphs
//!
//! All workspace data is published into a named graph, never the default graph.
//! A workspace's stable UUID (from `.clearhead/config.json`) names its graph as
//! `urn:clearhead:workspace:<uuid>` — see [`workspace_graph_name`]. Entity IRIs
//! are `urn:uuid:<uuid>` for Charters/Plans/Actions and `urn:context:<slug>` for
//! Contexts, so identity is stable across exports.
//!
//! # Vocabulary
//!
//! Terms follow the v4 Actions Vocabulary (a CCO extension). The ontology is the
//! arbiter of what is published; this projection emits exactly the vocabulary it
//! names — no derivable inverses, no un-modelled predicates. See
//! `specifications/ontology.md` and the `ontology/v4` artifacts.

pub mod project;
pub mod serialize;
pub mod snapshot;

pub use project::project_domain;
pub use serialize::{RdfFormat, serialize, serialize_domain};
pub use snapshot::{WorkspaceSnapshot, project_workspace_snapshot};

use oxrdf::{GraphName, Literal, NamedNode, Quad, Term};
use uuid::Uuid;

/// Result type for RDF projection and serialization.
pub type Result<T> = std::result::Result<T, RdfError>;

/// Errors that can occur while projecting or serializing the canonical dataset.
#[derive(thiserror::Error, Debug)]
pub enum RdfError {
    /// A domain value could not be projected into a well-formed RDF term (for
    /// example a context tag that does not sanitize to a valid IRI).
    #[error("RDF projection error: {0}")]
    Projection(String),
    /// A serializer failed to write the dataset.
    #[error("RDF serialization error: {0}")]
    Serialize(String),
}

// ============================================================================
// Namespace constants
// ============================================================================

pub(crate) const ACTIONS_NS: &str = "https://clearhead.us/vocab/actions/v4#";
pub(crate) const CCO_NS: &str = "https://www.commoncoreontologies.org/";
pub(crate) const BFO_NS: &str = "http://purl.obolibrary.org/obo/";
pub(crate) const RDF_NS: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
pub(crate) const RDFS_NS: &str = "http://www.w3.org/2000/01/rdf-schema#";
pub(crate) const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";
pub(crate) const DCTERMS_NS: &str = "http://purl.org/dc/terms/";
/// Workspace-snapshot vocabulary (identity, source provenance for editors).
pub(crate) const WORKSPACE_NS: &str = "https://clearhead.us/vocab/workspace/v1#";

// BFO property identifiers. Containment is published in the upward (part_of)
// direction only; the inverse has_part (BFO_0000051) is derivable and omitted.
pub(crate) const BFO_PART_OF: &str = "BFO_0000050";

// CCO class and property identifiers.
pub(crate) const CCO_PLAN: &str = "ont00000974";
pub(crate) const CCO_IS_SUCCESSOR_OF: &str = "ont00001775";
pub(crate) const CCO_PRESCRIBES: &str = "ont00001942";
pub(crate) const CCO_STATUS_PROP: &str = "ont00001868";

// ============================================================================
// Shared NamedNode helpers
// ============================================================================

pub(crate) fn ns(base: &str, name: &str) -> NamedNode {
    NamedNode::new(format!("{base}{name}")).expect("static namespace + name is a valid IRI")
}

pub(crate) fn actions_pred(name: &str) -> NamedNode {
    ns(ACTIONS_NS, name)
}

pub(crate) fn cco_node(id: &str) -> NamedNode {
    ns(CCO_NS, id)
}

pub(crate) fn bfo_pred(name: &str) -> NamedNode {
    ns(BFO_NS, name)
}

pub(crate) fn rdfs_pred(name: &str) -> NamedNode {
    ns(RDFS_NS, name)
}

pub(crate) fn dcterms_pred(name: &str) -> NamedNode {
    ns(DCTERMS_NS, name)
}

pub(crate) fn ws_pred(name: &str) -> NamedNode {
    ns(WORKSPACE_NS, name)
}

pub(crate) fn rdf_type() -> NamedNode {
    ns(RDF_NS, "type")
}

// ============================================================================
// Shared term helpers
// ============================================================================

/// Canonical order for a quad set: sorted by N-Quads spelling, deduplicated.
///
/// Both projection entry points ([`project_domain`],
/// [`project_workspace_snapshot`]) emit in this order; hosts merging multiple
/// workspaces' quads (whole-workspace export, multi-graph query datasets)
/// re-canonicalize through here so serialization stays byte-deterministic.
pub fn canonicalize(quads: &mut Vec<Quad>) {
    quads.sort_by_key(|q| q.to_string());
    quads.dedup();
}

/// The canonical `urn:uuid:<uuid>` entity node (Charter/Plan/Action).
pub(crate) fn uuid_node(id: Uuid) -> NamedNode {
    NamedNode::new(format!("urn:uuid:{id}")).expect("uuid yields a valid IRI")
}

/// An RDF plain literal.
pub(crate) fn simple(value: impl Into<String>) -> Term {
    Term::Literal(Literal::new_simple_literal(value))
}

/// An `xsd:`-typed literal.
pub(crate) fn typed(value: impl Into<String>, xsd_type: &str) -> Term {
    Term::Literal(Literal::new_typed_literal(value, ns(XSD_NS, xsd_type)))
}

/// The v4 individual for an Action's lifecycle phase (linked via
/// `cco:is_measured_by_nominal`, [`CCO_STATUS_PROP`]).
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

/// The v4 individual for a Charter's lifecycle status (linked via
/// `actions:hasCharterState`). Kept distinct from the Action phase individuals
/// so the two lifecycles never share a status value.
pub(crate) fn charter_state_node(state: &crate::domain::CharterState) -> NamedNode {
    use crate::domain::CharterState;
    let name = match state {
        CharterState::New => "CharterNew",
        CharterState::Active => "CharterActive",
        CharterState::Blocked => "CharterBlocked",
        CharterState::Closed => "CharterClosed",
        CharterState::Cancelled => "CharterCancelled",
    };
    actions_pred(name)
}

// ============================================================================
// Named graph identity
// ============================================================================

/// URI prefix for every workspace named graph.
pub const WORKSPACE_GRAPH_PREFIX: &str = "urn:clearhead:workspace:";

/// The named graph for a workspace, derived from its stable UUID string.
pub fn workspace_graph_name(uuid: &str) -> GraphName {
    GraphName::NamedNode(workspace_node(uuid))
}

/// The workspace entity node: the same `urn:clearhead:workspace:<uuid>` IRI
/// that names the workspace's named graph.
pub(crate) fn workspace_node(id: &str) -> NamedNode {
    NamedNode::new(format!("{WORKSPACE_GRAPH_PREFIX}{id}"))
        .expect("workspace UUID yields a valid IRI")
}

/// Named graph for transient, workspace-less datasets (ad-hoc projection, tests).
/// A real URI so `GRAPH ?g` patterns still find the data if it is later queried.
pub const TRANSIENT_GRAPH_URI: &str = "urn:clearhead:workspace:transient";

/// The transient named graph as a [`GraphName`].
pub fn transient_graph_name() -> GraphName {
    GraphName::NamedNode(NamedNode::new(TRANSIENT_GRAPH_URI).expect("static transient IRI"))
}
