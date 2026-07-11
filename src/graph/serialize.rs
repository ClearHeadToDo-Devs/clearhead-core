//! Serialize an Oxigraph store or DomainModel to Turtle output.
//!
//! This module owns the "turn a store into text" direction.

use super::{GraphError, Result, Store, create_store};
use crate::domain::{ActionState, Action, DomainModel};
use oxigraph::io::RdfFormat;
use oxigraph::model::GraphNameRef;

/// Serialize all Actions from a `DomainModel` to Turtle format.
///
/// Loads the full model (Plans + Actions) into a temporary store,
/// then serializes the default graph to Turtle.
pub fn serialize_acts_to_turtle(model: &DomainModel) -> Result<String> {
    let store = create_store()?;
    super::load_domain_model(&store, model, None, oxigraph::model::GraphName::DefaultGraph)?;
    store_to_turtle(&store)
}

/// Serialize only completed/cancelled actions (and their plans) to Turtle format.
///
/// Useful for generating a "closed actions" archive file.
pub fn serialize_closed_acts_to_turtle(model: &DomainModel) -> Result<String> {
    let filtered = filter_model_by_phase(model, |phase| {
        matches!(phase, ActionState::Completed | ActionState::Cancelled)
    });
    let store = create_store()?;
    super::load_domain_model(&store, &filtered, None, oxigraph::model::GraphName::DefaultGraph)?;
    store_to_turtle(&store)
}

/// Serialize only open (non-completed, non-cancelled) actions to Turtle format.
///
/// Useful for generating an "upcoming actions" file.
pub fn serialize_open_acts_to_turtle(model: &DomainModel) -> Result<String> {
    let filtered = filter_model_by_phase(model, |phase| {
        !matches!(phase, ActionState::Completed | ActionState::Cancelled)
    });
    let store = create_store()?;
    super::load_domain_model(&store, &filtered, None, oxigraph::model::GraphName::DefaultGraph)?;
    store_to_turtle(&store)
}

/// Serialize an Oxigraph store's default graph to Turtle.
///
/// Companion to `load_actions_into_store` for the archive workflow:
/// load existing archive + new actions → call this → write back to `archive.ttl`.
pub fn dump_store_to_turtle(store: &Store) -> Result<String> {
    store_to_turtle(store)
}

// ============================================================================
// Private helpers
// ============================================================================

fn store_to_turtle(store: &Store) -> Result<String> {
    let mut buffer = Vec::new();
    store
        .dump_graph_to_writer(GraphNameRef::DefaultGraph, RdfFormat::Turtle, &mut buffer)
        .map_err(|e| GraphError::Syntax(e.to_string()))?;
    String::from_utf8(buffer).map_err(|e| GraphError::Syntax(e.to_string()))
}

/// Filter a `DomainModel` to only include actions matching `predicate`,
/// preserving the charter hierarchy.
fn filter_model_by_phase(
    model: &DomainModel,
    predicate: impl Fn(&ActionState) -> bool,
) -> DomainModel {
    let mut filtered_charters = Vec::new();

    for charter in &model.charters {
        let filtered_acts: Vec<Action> = charter
            .actions
            .iter()
            .filter(|a| predicate(&a.state))
            .cloned()
            .collect();
        if !filtered_acts.is_empty() {
            let mut filtered_charter = charter.clone();
            filtered_charter.actions = filtered_acts;
            filtered_charters.push(filtered_charter);
        }
    }

    DomainModel {
        objectives: model.objectives.clone(),
        charters: filtered_charters,
    }
}
