//! Keep unpublished charter identity out of the canonical dataset.
//!
//! A charter whose document declares no id still needs a UUID in process, as a
//! join key. Publishing it would let consumers persist an id that is not the
//! charter's (clearhead-core docs/DECISIONS.md Decision 1), so every surface
//! runs its fully assembled dataset through [`anonymize_charters`].

use super::{canonicalize, uuid_node};
use oxrdf::{BlankNode, NamedOrBlankNode, Quad, Term};
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

/// Rewrite each charter in `ids` to a blank node.
///
/// Both subjects and objects are rewritten, so edges from other entities
/// still join. Run it once over the whole
/// dataset: blank labels are dataset-scoped, sequential, and never contain the
/// UUID they replace.
pub fn anonymize_charters(quads: Vec<Quad>, ids: &HashSet<Uuid>) -> Vec<Quad> {
    if ids.is_empty() {
        return quads;
    }
    let mut sorted: Vec<_> = ids.iter().copied().collect();
    sorted.sort();
    let blanks: HashMap<_, _> = sorted
        .into_iter()
        .enumerate()
        .map(|(index, id)| {
            let blank = BlankNode::new(format!("charter{index}"))
                .expect("sequential blank-node label is valid");
            (uuid_node(id), blank)
        })
        .collect();
    let mut quads: Vec<Quad> = quads
        .into_iter()
        .map(|quad| {
            let subject = match quad.subject {
                NamedOrBlankNode::NamedNode(node) => match blanks.get(&node) {
                    Some(blank) => NamedOrBlankNode::BlankNode(blank.clone()),
                    None => NamedOrBlankNode::NamedNode(node),
                },
                blank => blank,
            };
            let object = match quad.object {
                Term::NamedNode(node) => match blanks.get(&node) {
                    Some(blank) => Term::BlankNode(blank.clone()),
                    None => Term::NamedNode(node),
                },
                other => other,
            };
            Quad::new(subject, quad.predicate, object, quad.graph_name)
        })
        .collect();
    canonicalize(&mut quads);
    quads
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rdf::app::{APP_NS, Locations, project_app};
    use crate::rdf::{ns, transient_graph_name};
    use crate::workspace::implicit_charter;
    use crate::{Action, DomainModel};

    const HIDDEN: &str = "01951111-0000-7000-8000-0000000000aa";

    fn model() -> DomainModel {
        let mut hidden = implicit_charter("hidden");
        hidden.id = Uuid::parse_str(HIDDEN).unwrap();
        let mut child = implicit_charter("child");
        child.parent = Some("hidden".into());
        let mut action = Action::new("task");
        action.charter = Some("hidden".into());
        hidden.actions.push(action);
        DomainModel {
            objectives: vec![],
            charters: vec![hidden, child],
        }
    }

    fn projected() -> Vec<Quad> {
        let locations = Locations::default();
        project_app(
            &model(),
            &locations,
            None,
            &chrono::Utc,
            transient_graph_name(),
        )
        .unwrap()
    }

    fn anonymized() -> Vec<Quad> {
        let quads = projected();
        anonymize_charters(quads, &HashSet::from([Uuid::parse_str(HIDDEN).unwrap()]))
    }

    #[test]
    fn hidden_charter_uuid_appears_nowhere() {
        let text = format!("{:?}", anonymized());
        assert!(!text.contains(HIDDEN), "leaked: {text}");
    }

    #[test]
    fn edges_to_and_from_the_hidden_charter_still_join() {
        let quads = anonymized();
        let points_at_hidden = |object: &Term| matches!(object, Term::BlankNode(_));
        let part_of = ns(APP_NS, "partOf");
        let parts: Vec<_> = quads
            .iter()
            .filter(|q| q.predicate == part_of && points_at_hidden(&q.object))
            .collect();
        assert_eq!(
            parts.len(),
            2,
            "the child charter and the action keep their whole: {parts:?}"
        );
        assert!(
            quads
                .iter()
                .any(|q| matches!(q.subject, NamedOrBlankNode::BlankNode(_))),
            "the hidden charter keeps its own facts"
        );
    }

    #[test]
    fn declared_charters_are_untouched() {
        let quads = projected();
        assert_eq!(anonymize_charters(quads.clone(), &HashSet::new()), quads);
        let child = uuid_node(implicit_charter("child").id);
        assert!(
            anonymized()
                .iter()
                .any(|q| q.subject == child.clone().into())
        );
    }
}
