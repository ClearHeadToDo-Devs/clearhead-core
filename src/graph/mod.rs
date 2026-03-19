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
    build_raw_where_query, build_where_query, query_acts, query_actions, query_plans, query_raw,
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

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use super::{
        ACTIONS_NS, BFO_HAS_PART, BFO_NS, CCO_NS, CCO_PLANNED_ACT, CCO_PLAN, CCO_PRESCRIBES,
        CCO_STATUS_PROP, SCHEMA_NS,
    };
    use crate::workspace::actions::{Action, ActionState, convert};
    use oxigraph::sparql::{QueryResults, SparqlEvaluator};
    use oxigraph::model::Term;

    #[test]
    fn test_load_domain_model() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Test task")];
        let model = convert::from_actions(&actions);
        load_domain_model(&store, &model).unwrap();

        let plan_query = format!(
            "SELECT ?name WHERE {{ ?s a <{}{}> . ?s <{}name> ?name }}",
            CCO_NS, CCO_PLAN, SCHEMA_NS
        );
        let results = SparqlEvaluator::new()
            .parse_query(&plan_query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let names: Vec<_> = solutions
                .filter_map(|s| s.ok())
                .filter_map(|s| s.get("name").cloned())
                .collect();
            assert_eq!(names.len(), 1);
        }
    }

    #[test]
    fn test_serialize_acts_to_turtle() {
        let actions = vec![Action::new("Turtle task")];
        let model = convert::from_actions(&actions);
        let turtle = serialize_acts_to_turtle(&model).unwrap();
        assert!(turtle.contains(CCO_PLAN));
        assert!(turtle.contains(CCO_PLANNED_ACT));
        assert!(!turtle.is_empty());
    }

    #[test]
    fn test_serialize_open_closed_split() {
        let action_open = Action::new("Open task");
        let mut action_done = Action::new("Done task");
        action_done.state = ActionState::Completed;
        let model = convert::from_actions(&vec![action_open, action_done]);

        let open_turtle = serialize_open_acts_to_turtle(&model).unwrap();
        let closed_turtle = serialize_closed_acts_to_turtle(&model).unwrap();

        assert!(open_turtle.contains("NotStarted"));
        assert!(!open_turtle.contains("Completed"));
        assert!(closed_turtle.contains("Completed"));
        assert!(!closed_turtle.contains("NotStarted"));
    }

    #[test]
    fn test_query_plans_from_store() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Queryable plan")];
        let model = convert::from_actions(&actions);
        load_domain_model(&store, &model).unwrap();

        let sparql = format!(
            "SELECT ?id WHERE {{ ?s a <{}{}> . ?s <{}id> ?id }}",
            CCO_NS, CCO_PLAN, ACTIONS_NS
        );
        let plans = query_plans(&store, &sparql).unwrap();
        assert_eq!(plans.len(), 1);
        assert_eq!(plans[0].name, "Queryable plan");
    }

    #[test]
    fn test_query_acts_from_store() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Queryable act")];
        let model = convert::from_actions(&actions);
        load_domain_model(&store, &model).unwrap();

        let sparql = format!(
            "SELECT ?id WHERE {{ ?s a <{}{}> . ?s <{}id> ?id }}",
            CCO_NS, CCO_PLANNED_ACT, ACTIONS_NS
        );
        let acts = query_acts(&store, &sparql).unwrap();
        assert_eq!(acts.len(), 1);
        assert_eq!(acts[0].plan_id, model.all_plans()[0].id);
    }

    #[test]
    fn test_plan_and_act_linked() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Linked task")];
        let model = convert::from_actions(&actions);
        let plan_id = model.all_plans()[0].id;
        load_domain_model(&store, &model).unwrap();

        let query = format!(
            "SELECT ?act WHERE {{ ?act <{}prescribedBy> <urn:uuid:{}> }}",
            ACTIONS_NS, plan_id
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let acts: Vec<_> = solutions.filter_map(|s| s.ok()).collect();
            assert_eq!(acts.len(), 1);
        }
    }

    #[test]
    fn test_plan_prescribes_act() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Prescribed task")];
        let model = convert::from_actions(&actions);
        let plan_id = model.all_plans()[0].id;
        load_domain_model(&store, &model).unwrap();

        let query = format!(
            "SELECT ?act WHERE {{ <urn:uuid:{}> <{}{}> ?act }}",
            plan_id, CCO_NS, CCO_PRESCRIBES
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let acts: Vec<_> = solutions.filter_map(|s| s.ok()).collect();
            assert_eq!(acts.len(), 1, "Plan should prescribe exactly one act");
        }
    }

    #[test]
    fn test_act_status_uses_cco_property() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Status task")];
        let model = convert::from_actions(&actions);
        load_domain_model(&store, &model).unwrap();

        let query = format!(
            "SELECT ?status WHERE {{ ?act a <{}{}> . ?act <{}{}> ?status }}",
            CCO_NS, CCO_PLANNED_ACT, CCO_NS, CCO_STATUS_PROP
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let statuses: Vec<_> = solutions.filter_map(|s| s.ok()).collect();
            assert_eq!(statuses.len(), 1, "PlannedAct should have exactly one status");
        }
    }

    #[test]
    fn test_charter_stored_with_type_and_name() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Charter plan")];
        let model =
            convert::from_actions_with_charter(&actions, Some("build_clearhead".to_string()));
        load_domain_model(&store, &model).unwrap();

        let query = format!(
            "SELECT ?name WHERE {{ ?c a <{}Charter> . ?c <{}name> ?name }}",
            ACTIONS_NS, SCHEMA_NS
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let names: Vec<String> = solutions
                .filter_map(|s| s.ok())
                .filter_map(|s| s.get("name").cloned())
                .filter_map(|t| {
                    if let Term::Literal(l) = t {
                        Some(l.value().to_string())
                    } else {
                        None
                    }
                })
                .collect();
            assert_eq!(names, vec!["build_clearhead"]);
        } else {
            panic!("Expected query solutions");
        }
    }

    #[test]
    fn test_charter_has_part_links_to_plans() {
        let store = create_store().unwrap();
        let actions = vec![Action::new("Plan A"), Action::new("Plan B")];
        let model =
            convert::from_actions_with_charter(&actions, Some("my_project".to_string()));
        load_domain_model(&store, &model).unwrap();

        let query = format!(
            "SELECT ?id WHERE {{ \
                ?charter a <{actions}Charter> . \
                ?charter <{schema}name> \"my_project\" . \
                ?charter <{bfo}{has_part}> ?plan . \
                ?plan <{actions}id> ?id \
            }}",
            actions = ACTIONS_NS,
            schema = SCHEMA_NS,
            bfo = BFO_NS,
            has_part = BFO_HAS_PART,
        );
        let results = SparqlEvaluator::new()
            .parse_query(&query)
            .unwrap()
            .on_store(&store)
            .execute()
            .unwrap();

        if let QueryResults::Solutions(solutions) = results {
            let ids: Vec<_> = solutions.filter_map(|s| s.ok()).collect();
            assert_eq!(ids.len(), 2, "Charter should link to both plans via bfo:has_part");
        } else {
            panic!("Expected query solutions");
        }
    }
}
