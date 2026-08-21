//! Graphviz DOT projection for graph-family query results.
//!
//! RDF remains the semantic contract. This module selects the entity and
//! relationship structure from a CONSTRUCT result, builds a petgraph graph,
//! and emits DOT as a visualization projection.

use std::collections::{BTreeMap, BTreeSet};

use oxigraph::model::{NamedOrBlankNode, Term, Triple};
use petgraph::Graph;
use petgraph::dot::{Config, Dot};

use super::{ACTIONS_NS, BFO_NS, CCO_IS_SUCCESSOR_OF, CCO_NS, CCO_STATUS_PROP, RDF_NS, RDFS_LABEL};

const RDFS_NS: &str = "http://www.w3.org/2000/01/rdf-schema#";

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
/// Typed subjects become nodes. Object relations between those subjects become
/// edges; literal label/status/priority triples become node attributes. The
/// ontology's `action has predecessor predecessor` assertion is reversed for
/// display so work flows naturally from prerequisite to dependent action.
pub fn frame_dot(triples: &[Triple]) -> String {
    let rdf_type = format!("{RDF_NS}type");
    let rdfs_label = format!("{RDFS_NS}{RDFS_LABEL}");
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
            predicate if predicate == rdfs_label => {
                if let Term::Literal(value) = &triple.object {
                    entity.label = value.value().to_string();
                }
            }
            predicate if predicate == status_predicate => {
                entity.status = Some(term_label(&triple.object));
            }
            predicate if predicate == priority_predicate => {
                entity.priority = Some(term_label(&triple.object));
            }
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
    use oxigraph::model::{NamedNode, Triple};

    use super::*;

    fn nn(value: &str) -> NamedNode {
        NamedNode::new(value).unwrap()
    }

    #[test]
    fn dependency_edges_flow_from_predecessor_to_dependent() {
        let action = nn("urn:uuid:dependent");
        let predecessor = nn("urn:uuid:predecessor");
        let action_type = nn(&format!("{ACTIONS_NS}Action"));
        let triples = vec![
            Triple::new(
                action.clone(),
                nn(&format!("{RDF_NS}type")),
                action_type.clone(),
            ),
            Triple::new(
                predecessor.clone(),
                nn(&format!("{RDF_NS}type")),
                action_type,
            ),
            Triple::new(
                action,
                nn(&format!("{CCO_NS}{CCO_IS_SUCCESSOR_OF}")),
                predecessor,
            ),
        ];

        let dot = frame_dot(&triples);
        assert!(dot.contains("digraph"), "{dot}");
        assert!(dot.contains("predecessor"), "{dot}");
        assert!(dot.contains("dependent"), "{dot}");
    }
}
