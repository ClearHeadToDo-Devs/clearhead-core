//! Serialize an Oxigraph store or DomainModel to Turtle output.
//!
//! This module owns the "turn a store into text" direction.

use super::{Store, create_store};
use crate::domain::{ActPhase, DomainModel, PlannedAct};
use oxigraph::io::RdfFormat;
use oxigraph::model::GraphNameRef;

/// Serialize all PlannedActs from a `DomainModel` to Turtle format.
///
/// Loads the full model (Plans + Acts) into a temporary store,
/// then serializes the default graph to Turtle.
pub fn serialize_acts_to_turtle(model: &DomainModel) -> Result<String, String> {
    let store = create_store()?;
    super::load_domain_model(&store, model)?;
    store_to_turtle(&store)
}

/// Serialize only completed/cancelled acts (and their plans) to Turtle format.
///
/// Useful for generating a "closed acts" archive file.
pub fn serialize_closed_acts_to_turtle(model: &DomainModel) -> Result<String, String> {
    let filtered = filter_model_by_phase(model, |phase| {
        matches!(phase, ActPhase::Completed | ActPhase::Cancelled)
    });
    let store = create_store()?;
    super::load_domain_model(&store, &filtered)?;
    store_to_turtle(&store)
}

/// Serialize only open (non-completed, non-cancelled) acts to Turtle format.
///
/// Useful for generating an "upcoming acts" file.
pub fn serialize_open_acts_to_turtle(model: &DomainModel) -> Result<String, String> {
    let filtered = filter_model_by_phase(model, |phase| {
        !matches!(phase, ActPhase::Completed | ActPhase::Cancelled)
    });
    let store = create_store()?;
    super::load_domain_model(&store, &filtered)?;
    store_to_turtle(&store)
}

/// Serialize an Oxigraph store's default graph to Turtle.
///
/// Companion to `load_acts_into_store` for the archive workflow:
/// load existing archive + new acts → call this → write back to `archive.ttl`.
pub fn dump_store_to_turtle(store: &Store) -> Result<String, String> {
    store_to_turtle(store)
}

// ============================================================================
// Private helpers
// ============================================================================

fn store_to_turtle(store: &Store) -> Result<String, String> {
    let mut buffer = Vec::new();
    store
        .dump_graph_to_writer(GraphNameRef::DefaultGraph, RdfFormat::Turtle, &mut buffer)
        .map_err(|e| format!("Failed to serialize to Turtle: {}", e))?;
    String::from_utf8(buffer).map_err(|e| format!("Invalid UTF-8 in Turtle output: {}", e))
}

/// Filter a `DomainModel` to only include acts matching `predicate`,
/// preserving the charter → plan → act hierarchy.
fn filter_model_by_phase(
    model: &DomainModel,
    predicate: impl Fn(&ActPhase) -> bool,
) -> DomainModel {
    let mut filtered_charters = Vec::new();

    for charter in &model.charters {
        let mut filtered_plans = Vec::new();
        for plan in &charter.plans {
            let filtered_acts: Vec<PlannedAct> = plan
                .acts
                .iter()
                .filter(|a| predicate(&a.phase))
                .cloned()
                .collect();
            if !filtered_acts.is_empty() {
                let mut filtered_plan = plan.clone();
                filtered_plan.acts = filtered_acts;
                filtered_plans.push(filtered_plan);
            }
        }
        if !filtered_plans.is_empty() {
            let mut filtered_charter = charter.clone();
            filtered_charter.plans = filtered_plans;
            filtered_charters.push(filtered_charter);
        }
    }

    DomainModel {
        objectives: model.objectives.clone(),
        charters: filtered_charters,
    }
}
