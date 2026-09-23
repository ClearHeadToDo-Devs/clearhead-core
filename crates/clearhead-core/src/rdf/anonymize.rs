//! Keep unpublished charter identity out of the canonical dataset.
//!
//! A charter whose document declares no id still needs a UUID in process, as a
//! join key. Publishing it would let consumers persist an id that is not the
//! charter's (clearhead-core docs/DECISIONS.md Decision 1), so every surface
//! runs its fully assembled dataset through [`anonymize_charters`].

use super::{actions_pred, canonicalize, uuid_node};
use oxrdf::{BlankNode, NamedOrBlankNode, Quad, Term};
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

/// Rewrite each charter in `ids` to a blank node and drop its `hasUUID`.
///
/// Both subjects and objects are rewritten, so edges from other entities and
/// from the workspace-snapshot layer still join. Run it once over the whole
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
    let has_uuid = actions_pred("hasUUID");

    let mut quads: Vec<Quad> = quads
        .into_iter()
        .filter_map(|quad| {
            let subject = match quad.subject {
                NamedOrBlankNode::NamedNode(node) => match blanks.get(&node) {
                    Some(_) if quad.predicate == has_uuid => return None,
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
            Some(Quad::new(subject, quad.predicate, object, quad.graph_name))
        })
        .collect();
    canonicalize(&mut quads);
    quads
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rdf::{project_domain, transient_graph_name};
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

    fn anonymized() -> Vec<Quad> {
        let quads = project_domain(&model(), None, transient_graph_name()).unwrap();
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
        let is_hidden =
            |subject: &NamedOrBlankNode| matches!(subject, NamedOrBlankNode::BlankNode(_));
        let points_at_hidden = |object: &Term| matches!(object, Term::BlankNode(_));
        let sub_charter = actions_pred("hasSubCharter");
        let part_of = crate::rdf::bfo_pred(crate::rdf::BFO_PART_OF);
        assert!(
            quads
                .iter()
                .any(|q| is_hidden(&q.subject) && q.predicate == sub_charter),
            "parent edge lost"
        );
        assert!(
            quads
                .iter()
                .any(|q| q.predicate == part_of && points_at_hidden(&q.object)),
            "action containment lost"
        );
    }

    #[test]
    fn declared_charters_are_untouched() {
        let quads = project_domain(&model(), None, transient_graph_name()).unwrap();
        assert_eq!(anonymize_charters(quads.clone(), &HashSet::new()), quads);
        let child = uuid_node(implicit_charter("child").id);
        assert!(
            anonymized()
                .iter()
                .any(|q| q.subject == child.clone().into())
        );
    }
}
