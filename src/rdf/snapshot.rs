//! Workspace-snapshot projection: host-supplied filesystem facts as RDF quads.
//!
//! The canonical dataset has two layers. [`project_domain`](super::project_domain)
//! publishes the *semantic* domain model — reconstructable, portable identity.
//! This module publishes the *workspace snapshot* layered on top: which files
//! and lines currently embody the domain, plus the workspace's own identity
//! node. Per `specifications/ontology.md` these `ws:` properties are valid for
//! the current filesystem state and exist for editor integration (quickfix,
//! jump-to-source); they are not portable cross-machine identity.
//!
//! Core stays pure: it owns the statement shapes, IRIs, and datatypes (so every
//! ClearHead RDF statement still comes from one projection), while the host
//! owns the *values* — absolute paths and line numbers it read from disk,
//! supplied here as plain evidence strings.

use super::{
    Result, actions_pred, rdf_type, rdfs_pred, simple, typed, uuid_node, workspace_node, ws_pred,
};
use oxrdf::{GraphName, Quad, Term};

/// Project host-supplied workspace-snapshot evidence into quads for `graph`.
///
/// Emits, into the workspace's named graph:
/// - the workspace node (`urn:clearhead:workspace:<id>`) as `ws:Workspace`
///   with `rdfs:label`, `actions:hasAlias`, `ws:root`, and `ws:charterRoot`;
/// - `ws:hasSourceFile` for each charter Markdown file;
/// - `ws:hasSourceFile` / `ws:hasSourceLine` for each sourced action.
///
/// The returned quads are deduplicated and sorted into the same canonical
/// order as [`project_domain`](super::project_domain), so the union of the two
/// stays deterministic for a given input.
pub fn project_workspace_snapshot(
    snapshot: &WorkspaceSnapshot,
    graph: GraphName,
) -> Result<Vec<Quad>> {
    let mut quads = Vec::new();
    let mut add = |subject: oxrdf::NamedNode, predicate: oxrdf::NamedNode, object: Term| {
        quads.push(Quad::new(subject, predicate, object, graph.clone()));
    };

    let ws = workspace_node(&snapshot.workspace_id);
    add(
        ws.clone(),
        rdf_type(),
        Term::NamedNode(ws_pred("Workspace")),
    );
    add(
        ws.clone(),
        rdfs_pred("label"),
        simple(&snapshot.workspace_name),
    );
    add(
        ws.clone(),
        actions_pred("hasAlias"),
        simple(&snapshot.workspace_name),
    );
    add(ws.clone(), ws_pred("root"), typed(&snapshot.root, "string"));
    add(
        ws,
        ws_pred("charterRoot"),
        typed(&snapshot.charter_root, "string"),
    );

    for (charter_id, file) in &snapshot.charter_files {
        add(
            uuid_node(*charter_id),
            ws_pred("hasSourceFile"),
            typed(file, "string"),
        );
    }
    for (action_id, file, line) in &snapshot.action_sources {
        let subject = uuid_node(*action_id);
        add(
            subject.clone(),
            ws_pred("hasSourceFile"),
            typed(file, "string"),
        );
        add(
            subject,
            ws_pred("hasSourceLine"),
            typed(line.to_string(), "integer"),
        );
    }

    super::canonicalize(&mut quads);
    Ok(quads)
}

/// Host-supplied workspace-snapshot evidence: the filesystem facts Core turns
/// into the `ws:`-vocabulary layer of the canonical dataset.
///
/// All paths are absolute, plain-string host values (the host canonicalizes);
/// Core never touches the filesystem.
#[derive(Debug, Clone, Default)]
pub struct WorkspaceSnapshot {
    /// The workspace's effective identity (`Workspace::effective_id()`): the
    /// durable manifest UUID when present, else the host's per-load ephemeral
    /// id, so snapshot queries never silently drop their workspace join.
    pub workspace_id: String,
    /// Display name (`Workspace::effective_name()`), published as label/alias.
    pub workspace_name: String,
    /// Canonicalized absolute workspace root.
    pub root: String,
    /// Absolute charter tree root for resolving relative `hasSourceFile`s.
    pub charter_root: String,
    /// `(charter id, absolute .md path)` for charters with a Markdown file.
    pub charter_files: Vec<(uuid::Uuid, String)>,
    /// `(action id, absolute .actions path, 1-based line)` per sourced action.
    pub action_sources: Vec<(uuid::Uuid, String, u32)>,
}

// Identity evidence values are host-supplied but always uuid-shaped (manifest
// UUID or the host's per-load ephemeral id), so the IRI constructors here stay
// infallible like the rest of the projection.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rdf::workspace_graph_name;
    use uuid::Uuid;

    const WS_UUID: &str = "00000000-0000-0000-0000-0000000000aa";

    fn evidence() -> WorkspaceSnapshot {
        WorkspaceSnapshot {
            workspace_id: WS_UUID.to_string(),
            workspace_name: "test".to_string(),
            root: "/ws".to_string(),
            charter_root: "/ws/charters".to_string(),
            charter_files: vec![(Uuid::new_v4(), "/ws/charters/work.md".to_string())],
            action_sources: vec![(Uuid::new_v4(), "/ws/charters/work.actions".to_string(), 3)],
        }
    }

    #[test]
    fn emits_workspace_node_with_identity_and_roots() {
        let quads = project_workspace_snapshot(&evidence(), workspace_graph_name(WS_UUID)).unwrap();
        let text: String = quads
            .iter()
            .map(|q| q.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        let ws = format!("urn:clearhead:workspace:{WS_UUID}");
        assert!(
            text.contains(&format!("<{ws}>")),
            "workspace subject: {text}"
        );
        assert!(
            text.contains("vocab/workspace/v1#Workspace"),
            "type: {text}"
        );
        assert!(text.contains("\"test\""), "label: {text}");
        assert!(
            text.contains("workspace/v1#charterRoot"),
            "charterRoot: {text}"
        );
        // RDF 1.1: `\"foo\"^^xsd:string` is the same term as the simple literal
        // `"foo"`, and oxrdf normalizes to the simple form (as did graphd).
        assert!(text.contains("\"/ws/charters\""), "root literal: {text}");
    }

    #[test]
    fn emits_source_provenance_with_typed_datatypes() {
        let quads = project_workspace_snapshot(&evidence(), workspace_graph_name(WS_UUID)).unwrap();
        let text: String = quads
            .iter()
            .map(|q| q.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("workspace/v1#hasSourceFile"), "file: {text}");
        assert!(text.contains("workspace/v1#hasSourceLine"), "line: {text}");
        // Source lines are xsd:integer; paths are simple literals (RDF 1.1
        // xsd:string, normalized by oxrdf exactly as in the graphd-era store).
        assert!(
            text.contains("\"3\"^^<http://www.w3.org/2001/XMLSchema#integer>"),
            "line is xsd:integer: {text}"
        );
    }

    #[test]
    fn projection_is_deterministic_and_canonical() {
        let graph = workspace_graph_name(WS_UUID);
        let evidence = evidence();
        let a = project_workspace_snapshot(&evidence, graph.clone()).unwrap();
        let b = project_workspace_snapshot(&evidence, graph).unwrap();
        assert_eq!(a, b, "same evidence must yield identical quads");
        let mut sorted = a.clone();
        sorted.sort_by_key(|q| q.to_string());
        assert_eq!(a, sorted, "quads are returned in canonical order");
    }

    #[test]
    fn every_quad_lands_in_the_workspace_graph() {
        let quads = project_workspace_snapshot(&evidence(), workspace_graph_name(WS_UUID)).unwrap();
        let graph_iri = format!("urn:clearhead:workspace:{WS_UUID}");
        assert!(
            quads
                .iter()
                .all(|q| q.graph_name.to_string().contains(&graph_iri)),
            "snapshot quads belong to the workspace named graph"
        );
    }
}
