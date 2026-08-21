//! Graph runtime and graph-shaped projections for ClearHead.
//!
//! This crate owns Oxigraph, RDF insertion/query logic, response-shape
//! validation, and JSON-LD export, plus the human/agent-facing `query`
//! interface. `clearhead-core` supplies the graph-neutral domain and workspace
//! model it operates on.

pub mod graph;
pub mod query;
