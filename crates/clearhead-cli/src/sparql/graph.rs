//! The `graph` family: ontology-preserving CONSTRUCT results.
//!
//! A graph query is a complete standard CONSTRUCT/DESCRIBE whose RDF triples
//! are the semantic result. This module renders them: RDF serializations
//! (Turtle, JSON-LD) are the contract; the Graphviz DOT projection and the
//! terminal summary are presentation. RDF stays the source of truth.

use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::io::IsTerminal;

use anyhow::anyhow;
use oxigraph::io::{JsonLdProfileSet, RdfFormat};
use oxigraph::model::{NamedOrBlankNode, Term, Triple};
use petgraph::Graph;
use petgraph::dot::{Config, Dot};

use super::{build_store, construct_triples, emit_rdf};
use crate::argparser::QueryFormat;
use crate::commands::CommandContext;
use crate::stdout::{write_stdout, write_stdout_line};

const RDF_NS: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
const RDFS_NS: &str = "http://www.w3.org/2000/01/rdf-schema#";
const ACTIONS_NS: &str = "https://clearhead.us/vocab/actions/v4#";
const CCO_NS: &str = "https://www.commoncoreontologies.org/";
const BFO_NS: &str = "http://purl.obolibrary.org/obo/";
const CCO_IS_SUCCESSOR_OF: &str = "ont00001775";
const CCO_STATUS_PROP: &str = "ont00001868";

/// Run a named graph view: resolve, execute the CONSTRUCT, render.
pub fn run(
    ctx: &CommandContext,
    name: Option<&str>,
    format: Option<QueryFormat>,
) -> anyhow::Result<()> {
    let name = name.unwrap_or("dependencies");
    let sparql =
        super::registry::resolve_family(ctx, "graph", name, super::registry::BUILT_IN_GRAPH)
            .ok_or_else(|| {
                anyhow!(
                    "No graph query named '{name}'. Save a .sparql file to \
                 <config>/queries/graph/ or <workspace>/.clearhead/queries/graph/"
                )
            })?;

    let store = build_store(ctx)?;
    let triples = construct_triples(&store, &sparql)?;
    match format.unwrap_or_else(default_graph_format) {
        QueryFormat::Table => emit_summary(&triples),
        QueryFormat::Jsonld => emit_rdf(
            &triples,
            RdfFormat::JsonLd {
                profile: JsonLdProfileSet::empty(),
            },
        ),
        QueryFormat::Turtle => emit_rdf(&triples, RdfFormat::Turtle),
        QueryFormat::Dot => write_stdout(frame_dot(&triples).as_bytes()),
        QueryFormat::Json | QueryFormat::Ndjson => {
            anyhow::bail!("graph queries require an RDF format: use jsonld or turtle")
        }
        QueryFormat::Ids => anyhow::bail!("--format ids is defined for index queries"),
    }
}

/// Graph results default to a DOT-adjacent RDF (JSON-LD) when piped and a
/// summary at a terminal.
fn default_graph_format() -> QueryFormat {
    if std::io::stdout().is_terminal() {
        QueryFormat::Table
    } else {
        QueryFormat::Jsonld
    }
}

fn emit_summary(triples: &[Triple]) -> anyhow::Result<()> {
    let subjects: HashSet<_> = triples.iter().map(|t| t.subject.to_string()).collect();
    let predicates: HashSet<_> = triples.iter().map(|t| t.predicate.to_string()).collect();
    write_stdout_line(&format!(
        "{} triples, {} subjects, {} predicates",
        triples.len(),
        subjects.len(),
        predicates.len()
    ))
}

// ============================================================================
// Graphviz DOT projection
// ============================================================================

#[derive(Debug, Clone, Default)]
struct Entity {
    id: String,
    label: String,
    kind: String,
    status: Option<String>,
    priority: Option<String>,
}

#[derive(Debug, Clone)]
struct Relation {
    predicate: String,
}

/// Project an RDF graph into deterministic Graphviz DOT.
///
/// Typed subjects become nodes; object relations between them become edges;
/// literal label/status/priority triples become node attributes. The `action
/// is-successor-of predecessor` assertion is reversed for display so work flows
/// prerequisite → dependent.
fn frame_dot(triples: &[Triple]) -> String {
    let rdf_type = format!("{RDF_NS}type");
    let rdfs_label = format!("{RDFS_NS}label");
    let status_predicate = format!("{CCO_NS}{CCO_STATUS_PROP}");
    let priority_predicate = format!("{ACTIONS_NS}hasPriority");
    let predecessor_predicate = format!("{CCO_NS}{CCO_IS_SUCCESSOR_OF}");
    let has_part_predicate = format!("{BFO_NS}BFO_0000051");

    let mut entities: BTreeMap<String, Entity> = BTreeMap::new();
    for triple in triples {
        let Some(subject) = named_subject(&triple.subject) else {
            continue;
        };
        if triple.predicate.as_str() == rdf_type
            && let Term::NamedNode(kind) = &triple.object
        {
            let entity = entities.entry(subject.clone()).or_default();
            entity.id = subject;
            entity.kind = compact_iri(kind.as_str());
        }
    }

    for triple in triples {
        let Some(subject) = named_subject(&triple.subject) else {
            continue;
        };
        let Some(entity) = entities.get_mut(&subject) else {
            continue;
        };
        match triple.predicate.as_str() {
            p if p == rdfs_label => {
                if let Term::Literal(value) = &triple.object {
                    entity.label = value.value().to_string();
                }
            }
            p if p == status_predicate => entity.status = Some(term_label(&triple.object)),
            p if p == priority_predicate => entity.priority = Some(term_label(&triple.object)),
            _ => {}
        }
    }

    let mut graph = Graph::<Entity, Relation>::new();
    let mut indices = BTreeMap::new();
    for (id, entity) in &entities {
        let mut entity = entity.clone();
        if entity.label.is_empty() {
            entity.label = compact_iri(id);
        }
        indices.insert(id.clone(), graph.add_node(entity));
    }

    let mut edges = BTreeSet::new();
    for triple in triples {
        let Some(subject) = named_subject(&triple.subject) else {
            continue;
        };
        let Term::NamedNode(object) = &triple.object else {
            continue;
        };
        let object = object.as_str().to_string();
        if !entities.contains_key(&subject) || !entities.contains_key(&object) {
            continue;
        }
        let predicate = triple.predicate.as_str();
        if predicate == rdf_type || predicate == status_predicate {
            continue;
        }
        let (from, to) = if predicate == predecessor_predicate {
            (object, subject)
        } else {
            (subject, object)
        };
        edges.insert((from, to, predicate.to_string()));
    }
    for (from, to, predicate) in edges {
        graph.add_edge(indices[&from], indices[&to], Relation { predicate });
    }

    let edge_attributes = |_, edge: petgraph::graph::EdgeReference<'_, Relation>| {
        let predicate = edge.weight().predicate.as_str();
        if predicate == has_part_predicate {
            "style=\"dashed\",color=\"#6b7280\",label=\"contains\"".to_string()
        } else if predicate == predecessor_predicate {
            "color=\"#60a5fa\",penwidth=\"2\"".to_string()
        } else {
            format!("label=\"{}\"", escape_dot(&compact_iri(predicate)))
        }
    };
    let node_attributes = |_, (_, node): (_, &Entity)| node_attributes(node);
    let dot = Dot::with_attr_getters(
        &graph,
        &[Config::NodeNoLabel, Config::EdgeNoLabel],
        &edge_attributes,
        &node_attributes,
    );
    format!("{dot:?}\n")
}

fn named_subject(subject: &NamedOrBlankNode) -> Option<String> {
    match subject {
        NamedOrBlankNode::NamedNode(node) => Some(node.as_str().to_string()),
        NamedOrBlankNode::BlankNode(_) => None,
    }
}

fn term_label(term: &Term) -> String {
    match term {
        Term::NamedNode(node) => compact_iri(node.as_str()),
        Term::Literal(value) => value.value().to_string(),
        Term::BlankNode(node) => node.as_str().to_string(),
    }
}

fn compact_iri(iri: &str) -> String {
    iri.rsplit(['#', '/', ':'])
        .next()
        .unwrap_or(iri)
        .to_string()
}

fn node_attributes(node: &Entity) -> String {
    let mut label = node.label.clone();
    if let Some(status) = &node.status {
        label.push_str("\\n[");
        label.push_str(status);
        label.push(']');
    }
    if let Some(priority) = &node.priority {
        label.push_str(" !");
        label.push_str(priority);
    }
    let (shape, fill) = if node.kind == "Charter" {
        ("box", "#1f2937")
    } else {
        match node.status.as_deref() {
            Some("Blocked") => ("ellipse", "#7f1d1d"),
            Some("InProgress") => ("ellipse", "#78350f"),
            _ => ("ellipse", "#1e3a5f"),
        }
    };
    format!(
        "label=\"{}\",shape=\"{shape}\",style=\"filled\",fillcolor=\"{fill}\",fontcolor=\"white\",tooltip=\"{}\"",
        escape_dot(&label),
        escape_dot(&node.id)
    )
}

fn escape_dot(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxigraph::model::{Literal, NamedNode};

    fn triple(s: &str, p: &str, o: Term) -> Triple {
        Triple::new(NamedNode::new(s).unwrap(), NamedNode::new(p).unwrap(), o)
    }

    #[test]
    fn typed_subjects_become_labelled_dot_nodes() {
        let s = "urn:uuid:a";
        let triples = vec![
            triple(
                s,
                &format!("{RDF_NS}type"),
                Term::NamedNode(NamedNode::new(format!("{ACTIONS_NS}Action")).unwrap()),
            ),
            triple(
                s,
                &format!("{RDFS_NS}label"),
                Term::Literal(Literal::new_simple_literal("Do it")),
            ),
        ];
        let dot = frame_dot(&triples);
        assert!(dot.starts_with("digraph"), "{dot}");
        assert!(dot.contains("Do it"), "node label present: {dot}");
    }
}
