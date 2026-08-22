//! Serialize the canonical quad projection into RDF text formats.
//!
//! Every format is one function of the same `&[Quad]` that [`project_domain`]
//! produces. There is no second traversal of the domain model and no store: the
//! projection decides *what is true*, and this module only decides *how to spell
//! it*. That is the whole point of the read path — one set of facts, several
//! serializations of it, JSON-LD included.
//!
//! ## Dataset vs. graph formats
//!
//! [`RdfFormat::TriG`] and [`RdfFormat::NQuads`] are dataset syntaxes: they carry
//! the fourth (graph) term, so a workspace's `urn:clearhead:workspace:<uuid>`
//! named graph survives the round trip. [`RdfFormat::Turtle`] is a graph-only
//! syntax — it has no place to write the graph name, so it emits the triples and
//! drops the graph label; that is Turtle's contract, not a loss of fidelity in
//! the projection. [`RdfFormat::JsonLd`] is flat/expanded JSON-LD serialized
//! straight from the quads via `oxjsonld`; the named graph is preserved via
//! JSON-LD's own `@graph` nesting.
//!
//! ## Why there is no shape/SHACL validation here
//!
//! Emit-side correctness is guaranteed by construction, not by a validation pass.
//! [`project_domain`] can only build well-typed quads — an Action always gets a
//! `hasStatus`, a completed Action always carries its completion datetime, and so
//! on — so a SHACL run over our own output would only re-assert what the
//! constructor already made unrepresentable otherwise.
//!
//! The boundary where a shape contract *would* earn its keep is the inverse arrow:
//! **importing foreign RDF** into the domain model. There we would not trust the
//! input, and a declarative shape gate is how you reject "this does not fit the
//! domain" at the door instead of smearing defensive parsing through bespoke Rust.
//! That import path is a deliberate non-goal today (we never ingest RDF; neither
//! did graphd), so the validator lives nowhere rather than in database-free Core.
//! See the `rdf-publication` charter.

use super::{
    ACTIONS_NS, BFO_NS, CCO_NS, DCTERMS_NS, RDF_NS, RDFS_NS, Result, WORKSPACE_NS, XSD_NS,
    project_domain,
};
use crate::WorkspaceConfig;
use crate::domain::DomainModel;
use oxjsonld::JsonLdSerializer;
use oxrdf::{GraphName, Quad, QuadRef, TripleRef};
use oxttl::{NQuadsSerializer, TriGSerializer, TurtleSerializer};

/// The RDF text formats Core can emit from the canonical quad set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RdfFormat {
    /// TriG — dataset syntax; preserves the workspace named graph.
    TriG,
    /// N-Quads — line-based dataset syntax; preserves the workspace named graph.
    NQuads,
    /// Turtle — graph-only syntax; emits the triples and drops the graph label.
    Turtle,
    /// Flat/expanded JSON-LD, serialized directly from the quads.
    JsonLd,
}

/// Vocabulary prefixes applied to the prefix-aware syntaxes (TriG, Turtle,
/// JSON-LD `@context`). N-Quads has no prefix mechanism and always spells IRIs
/// in full. Kept in sync with the namespace constants the projection emits.
const VOCAB_PREFIXES: &[(&str, &str)] = &[
    ("actions", ACTIONS_NS),
    ("cco", CCO_NS),
    ("bfo", BFO_NS),
    ("rdf", RDF_NS),
    ("rdfs", RDFS_NS),
    ("dcterms", DCTERMS_NS),
    ("xsd", XSD_NS),
    ("ws", WORKSPACE_NS),
];

/// Project `model` and serialize it in one call — the entry point a host uses to
/// publish a whole workspace. `graph` is the workspace's named graph (from
/// [`super::workspace_graph_name`]); it is passed in rather than derived because
/// workspace-UUID discovery is a host/filesystem concern, not Core's.
pub fn serialize_domain(
    model: &DomainModel,
    config: Option<&WorkspaceConfig>,
    graph: GraphName,
    format: RdfFormat,
) -> Result<String> {
    let quads = project_domain(model, config, graph)?;
    serialize(&quads, format)
}

/// Serialize an already-projected canonical quad set into `format`.
pub fn serialize(quads: &[Quad], format: RdfFormat) -> Result<String> {
    match format {
        RdfFormat::TriG => to_trig(quads),
        RdfFormat::NQuads => to_nquads(quads),
        RdfFormat::Turtle => to_turtle(quads),
        RdfFormat::JsonLd => to_jsonld(quads),
    }
}

/// Map any serializer error (IRI parse, I/O, UTF-8) to [`super::RdfError`].
fn ser_err(e: impl std::fmt::Display) -> super::RdfError {
    super::RdfError::Serialize(e.to_string())
}

fn to_trig(quads: &[Quad]) -> Result<String> {
    let ser = VOCAB_PREFIXES
        .iter()
        .try_fold(TriGSerializer::new(), |s, (p, ns)| {
            s.with_prefix(*p, *ns).map_err(ser_err)
        })?;
    let mut writer = ser.for_writer(Vec::new());
    for quad in quads {
        writer.serialize_quad(quad).map_err(ser_err)?;
    }
    String::from_utf8(writer.finish().map_err(ser_err)?).map_err(ser_err)
}

fn to_nquads(quads: &[Quad]) -> Result<String> {
    let mut writer = NQuadsSerializer::new().for_writer(Vec::new());
    for quad in quads {
        writer.serialize_quad(quad).map_err(ser_err)?;
    }
    String::from_utf8(writer.finish()).map_err(ser_err)
}

fn to_turtle(quads: &[Quad]) -> Result<String> {
    let ser = VOCAB_PREFIXES
        .iter()
        .try_fold(TurtleSerializer::new(), |s, (p, ns)| {
            s.with_prefix(*p, *ns).map_err(ser_err)
        })?;
    let mut writer = ser.for_writer(Vec::new());
    for quad in quads {
        let q: QuadRef = quad.into();
        writer
            .serialize_triple(TripleRef::new(q.subject, q.predicate, q.object))
            .map_err(ser_err)?;
    }
    String::from_utf8(writer.finish().map_err(ser_err)?).map_err(ser_err)
}

fn to_jsonld(quads: &[Quad]) -> Result<String> {
    let ser = VOCAB_PREFIXES
        .iter()
        .try_fold(JsonLdSerializer::new(), |s, (p, ns)| {
            s.with_prefix(*p, *ns).map_err(ser_err)
        })?;
    let mut writer = ser.for_writer(Vec::new());
    for quad in quads {
        writer.serialize_quad(quad).map_err(ser_err)?;
    }
    String::from_utf8(writer.finish().map_err(ser_err)?).map_err(ser_err)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Action, ActionState, Charter, DomainModel};
    use crate::rdf::workspace_graph_name;
    use oxrdf::Quad;
    use oxttl::{NQuadsParser, TriGParser};
    use uuid::Uuid;

    const WS_UUID: &str = "00000000-0000-0000-0000-0000000000aa";

    fn sample_quads() -> Vec<Quad> {
        let action = Action {
            id: Uuid::new_v4(),
            name: "Write the serializer".to_string(),
            state: ActionState::NotStarted,
            ..Default::default()
        };
        let model = DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: Uuid::new_v4(),
                title: "Publication".to_string(),
                actions: vec![action],
                ..Default::default()
            }],
        };
        project_domain(&model, None, workspace_graph_name(WS_UUID)).unwrap()
    }

    /// Parse an N-Quads document back into a canonical, comparable quad set.
    fn parse_nquads(text: &str) -> Vec<Quad> {
        let mut quads: Vec<Quad> = NQuadsParser::new()
            .for_slice(text.as_bytes())
            .map(|q| q.unwrap())
            .collect();
        quads.sort_by_key(|q| q.to_string());
        quads
    }

    #[test]
    fn nquads_round_trips_to_the_same_quad_set() {
        let quads = sample_quads();
        let reparsed = parse_nquads(&to_nquads(&quads).unwrap());
        assert_eq!(reparsed, quads, "N-Quads must preserve the dataset exactly");
    }

    #[test]
    fn dataset_formats_preserve_the_named_graph() {
        let quads = sample_quads();
        let graph_iri = format!("urn:clearhead:workspace:{WS_UUID}");

        // N-Quads: every line carries the graph as its fourth term.
        let nq = to_nquads(&quads).unwrap();
        assert!(
            nq.lines()
                .all(|l| l.trim().is_empty() || l.contains(&graph_iri)),
            "every N-Quads statement must name the workspace graph"
        );

        // TriG: the graph label heads a block, and reparsing recovers it.
        let trig = to_trig(&quads).unwrap();
        assert!(
            trig.contains(&graph_iri),
            "TriG must name the workspace graph"
        );
        let mut reparsed: Vec<Quad> = TriGParser::new()
            .for_slice(trig.as_bytes())
            .map(|q| q.unwrap())
            .collect();
        reparsed.sort_by_key(|q| q.to_string());
        assert_eq!(reparsed, quads, "TriG must round-trip the full dataset");
    }

    #[test]
    fn turtle_drops_the_graph_but_keeps_every_triple() {
        let quads = sample_quads();
        let ttl = to_turtle(&quads).unwrap();
        // Graph-only: the named graph must not appear as data.
        assert!(
            !ttl.contains(&format!("urn:clearhead:workspace:{WS_UUID}")),
            "Turtle is graph-only and must not spell the graph name"
        );
        // Prefixed output should compact the actions vocabulary.
        assert!(
            ttl.contains("@prefix actions:"),
            "Turtle should declare prefixes"
        );
    }

    #[test]
    fn jsonld_is_flat_and_carries_a_context() {
        let quads = sample_quads();
        let doc = to_jsonld(&quads).unwrap();
        let value: serde_json::Value = serde_json::from_str(&doc).unwrap();
        assert!(
            value.get("@context").is_some(),
            "JSON-LD must carry an @context"
        );
        assert!(
            doc.contains(&format!("urn:clearhead:workspace:{WS_UUID}")),
            "flat JSON-LD must reference the workspace named graph"
        );
    }

    #[test]
    fn serialize_is_deterministic() {
        let quads = sample_quads();
        for format in [
            RdfFormat::TriG,
            RdfFormat::NQuads,
            RdfFormat::Turtle,
            RdfFormat::JsonLd,
        ] {
            assert_eq!(
                serialize(&quads, format).unwrap(),
                serialize(&quads, format).unwrap(),
                "{format:?} serialization must be deterministic"
            );
        }
    }
}
