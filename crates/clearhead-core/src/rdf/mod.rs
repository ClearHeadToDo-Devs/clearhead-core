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
//! A workspace's stable UUID (from its `workspace.json`) names its graph as
//! `urn:clearhead:workspace:<uuid>` — see [`workspace_graph_name`].
//!
//! # Vocabulary
//!
//! The application graph ([`app`]): the `app:` vocabulary the specification
//! defines in `ontology.md`, whose meaning is the specification's mapping to
//! CCO and IAO (Decisions 45 and 50).

pub mod anonymize;
pub mod app;
pub mod serialize;

pub use anonymize::anonymize_charters;
pub use serialize::{RdfFormat, serialize};

use oxrdf::{GraphName, Literal, NamedNode, Quad, Term};
use uuid::Uuid;

/// Result type for RDF projection and serialization.
pub type Result<T> = std::result::Result<T, RdfError>;

/// Errors that can occur while projecting or serializing the canonical dataset.
#[derive(thiserror::Error, Debug)]
pub enum RdfError {
    /// A domain value could not be projected into a well-formed RDF term.
    #[error("RDF projection error: {0}")]
    Projection(String),
    /// A serializer failed to write the dataset.
    #[error("RDF serialization error: {0}")]
    Serialize(String),
}

// ============================================================================
// Namespaces and term helpers
// ============================================================================

pub(crate) const RDF_NS: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
pub(crate) const RDFS_NS: &str = "http://www.w3.org/2000/01/rdf-schema#";
pub(crate) const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";
pub(crate) const DCTERMS_NS: &str = "http://purl.org/dc/terms/";

pub(crate) fn ns(base: &str, name: &str) -> NamedNode {
    NamedNode::new(format!("{base}{name}")).expect("static namespace + name is a valid IRI")
}

pub(crate) fn rdf_type() -> NamedNode {
    ns(RDF_NS, "type")
}

/// Canonical order for a quad set: sorted by N-Quads spelling, deduplicated.
///
/// The projection emits in this order; hosts merging multiple workspaces'
/// quads (whole-workspace export, multi-graph query datasets) re-canonicalize
/// through here so serialization stays byte-deterministic.
pub fn canonicalize(quads: &mut Vec<Quad>) {
    quads.sort_by_key(|q| q.to_string());
    quads.dedup();
}

/// The canonical `urn:uuid:<uuid>` entity node.
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

// ============================================================================
// Named graph identity
// ============================================================================

/// URI prefix for every workspace named graph.
pub const WORKSPACE_GRAPH_PREFIX: &str = "urn:clearhead:workspace:";

/// The named graph for a workspace, derived from its stable UUID string.
pub fn workspace_graph_name(uuid: &str) -> GraphName {
    GraphName::NamedNode(
        NamedNode::new(format!("{WORKSPACE_GRAPH_PREFIX}{uuid}"))
            .expect("workspace UUID yields a valid IRI"),
    )
}

/// Named graph for transient, workspace-less datasets (ad-hoc projection, tests).
/// A real URI so `GRAPH ?g` patterns still find the data if it is later queried.
pub const TRANSIENT_GRAPH_URI: &str = "urn:clearhead:workspace:transient";

/// The transient named graph as a [`GraphName`].
pub fn transient_graph_name() -> GraphName {
    GraphName::NamedNode(NamedNode::new(TRANSIENT_GRAPH_URI).expect("static transient IRI"))
}
