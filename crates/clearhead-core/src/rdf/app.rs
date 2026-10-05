//! Project the domain model into the application graph (specifications
//! `ontology.md`, Decision 45): the `app:` vocabulary that queries read and
//! exports write. Its meaning is the specification's mapping to CCO, which is
//! not Core's concern.
//!
//! Pure: the host supplies where entities are kept ([`Locations`]) and the
//! viewer's zone, in which derived instants are resolved. Written times are
//! emitted as written (Decision 52).

use std::collections::HashMap;
use std::fmt::Display;
use std::path::Path;

use chrono::{DateTime, SecondsFormat, TimeZone};
use oxrdf::{GraphName, NamedNode, Quad, Term};
use uuid::Uuid;

use super::{DCTERMS_NS, RDFS_NS, Result, ns, rdf_type, simple, typed, uuid_node};
use crate::WorkspaceConfig;
use crate::domain::time::Bound;
use crate::domain::{Action, ActionState, Charter, CharterState, DomainModel, Objective};
use crate::reference::match_entity_reference;
use crate::workspace::store::Workspace;

pub const APP_NS: &str = "https://clearhead.dev/vocab/app/v1#";
const SKOS_NS: &str = "http://www.w3.org/2004/02/skos/core#";

/// Names every context node (ontology.md, Context terms).
pub const CONTEXT_NS: Uuid = Uuid::from_u128(0x0d8937ce_eb24_52d2_9532_39ea299f888b);

fn app(name: &str) -> NamedNode {
    ns(APP_NS, name)
}

/// Where each entity is kept, relative to the data root with `/` separators
/// (rule 3): a file per charter, objective and action, a 1-based line per
/// action. Empty for a backend without files, and then nothing is emitted.
#[derive(Debug, Clone, Default)]
pub struct Locations {
    pub files: HashMap<Uuid, String>,
    pub lines: HashMap<Uuid, u32>,
}

impl Locations {
    /// The locations a loaded workspace read its entities from. A charter is
    /// kept in its document, or its actions file when it has none.
    pub fn of(workspace: &Workspace) -> Self {
        let mut locations = Self::default();
        let in_charters = |file: &Path| format!("charters/{}", slashed(file));
        for charter in &workspace.charters {
            if let Some(file) = charter
                .md_file
                .as_deref()
                .or(charter.actions_file.as_deref())
            {
                locations.files.insert(charter.id, in_charters(file));
            }
            let Some(actions_file) = charter.actions_file.as_deref() else {
                continue;
            };
            for sourced in &charter.actions {
                let id = sourced.action.id;
                locations.files.insert(id, in_charters(actions_file));
                if let Some(source) = &sourced.source_metadata {
                    locations.lines.insert(id, source.root.start_row as u32 + 1);
                }
            }
        }
        for (id, file) in &workspace.objective_files {
            locations.files.insert(*id, slashed(file));
        }
        locations
    }
}

fn slashed(path: &Path) -> String {
    path.components()
        .map(|part| part.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

/// Project `model` into the application graph's quads for `graph`, resolving
/// derived instants in `zone`. `config` supplies the context hierarchy.
pub fn project_app<Tz>(
    model: &DomainModel,
    locations: &Locations,
    config: Option<&WorkspaceConfig>,
    zone: &Tz,
    graph: GraphName,
) -> Result<Vec<Quad>>
where
    Tz: TimeZone,
    Tz::Offset: Display,
{
    let mut out = Graph {
        graph,
        quads: Vec::new(),
        locations,
    };
    for charter in &model.charters {
        project_charter(&mut out, charter, model);
    }
    for objective in &model.objectives {
        project_objective(&mut out, objective, &model.objectives);
    }
    for charter in &model.charters {
        let tree = Tree::of(&charter.actions);
        for action in &charter.actions {
            project_action(&mut out, action, charter, &tree, zone);
        }
    }
    if let Some(config) = config {
        for (parent, children) in &config.tag_hierarchies {
            let parent = context(&mut out, parent);
            for child in children {
                let child = context(&mut out, child);
                out.node(&child, ns(SKOS_NS, "broader"), parent.clone());
            }
        }
    }
    super::canonicalize(&mut out.quads);
    Ok(out.quads)
}

struct Graph<'a> {
    graph: GraphName,
    quads: Vec<Quad>,
    locations: &'a Locations,
}

impl Graph<'_> {
    fn add(&mut self, subject: &NamedNode, predicate: NamedNode, object: Term) {
        self.quads.push(Quad::new(
            subject.clone(),
            predicate,
            object,
            self.graph.clone(),
        ));
    }

    fn node(&mut self, subject: &NamedNode, predicate: NamedNode, object: NamedNode) {
        self.add(subject, predicate, Term::NamedNode(object));
    }

    fn text(&mut self, subject: &NamedNode, predicate: NamedNode, value: Option<&str>) {
        if let Some(value) = value.filter(|value| !value.trim().is_empty()) {
            self.add(subject, predicate, simple(value));
        }
    }

    /// `app:file`, and `app:line` for an action (rule 3).
    fn location(&mut self, subject: &NamedNode, id: Uuid) {
        if let Some(file) = self.locations.files.get(&id).cloned() {
            self.add(subject, app("file"), simple(file));
        }
        if let Some(line) = self.locations.lines.get(&id) {
            self.add(subject, app("line"), typed(line.to_string(), "integer"));
        }
    }
}

fn label(name: &str) -> NamedNode {
    ns(RDFS_NS, name)
}

fn dcterms(name: &str) -> NamedNode {
    ns(DCTERMS_NS, name)
}

fn project_charter(out: &mut Graph, charter: &Charter, model: &DomainModel) {
    let subject = uuid_node(charter.id);
    out.node(&subject, rdf_type(), app("Charter"));
    out.text(&subject, label("label"), Some(&charter.title));
    out.text(
        &subject,
        dcterms("description"),
        charter.description.as_deref(),
    );
    out.text(&subject, app("alias"), charter.alias.as_deref());
    out.node(
        &subject,
        app("state"),
        charter_state(charter.effective_state()),
    );
    let parent = charter.parent.as_deref().and_then(|reference| {
        model
            .charters
            .iter()
            .find(|c| match_entity_reference(c.id, c.alias.as_deref(), reference).is_some())
    });
    if let Some(parent) = parent {
        out.node(&subject, app("partOf"), uuid_node(parent.id));
    }
    for reference in charter.objectives.iter().flatten() {
        if let Some(objective) = model.objectives.iter().find(|o| o.is_named_by(reference)) {
            out.node(&subject, app("serves"), uuid_node(objective.id));
        }
    }
    out.location(&subject, charter.id);
}

fn project_objective(out: &mut Graph, objective: &Objective, objectives: &[Objective]) {
    let subject = uuid_node(objective.id);
    out.node(&subject, rdf_type(), app("Objective"));
    out.text(&subject, label("label"), objective.title.as_deref());
    out.text(
        &subject,
        dcterms("description"),
        objective.description.as_deref(),
    );
    out.text(&subject, app("alias"), objective.alias.as_deref());
    let parent = objective
        .parent
        .as_deref()
        .and_then(|reference| objectives.iter().find(|o| o.is_named_by(reference)));
    if let Some(parent) = parent {
        out.node(&subject, app("partOf"), uuid_node(parent.id));
    }
    for metric in objective.metrics.iter().flatten() {
        let name = format!("metric/{}", slug(&metric.name));
        let node = uuid_node(Uuid::new_v5(&objective.id, name.as_bytes()));
        out.node(&subject, app("metric"), node.clone());
        out.node(&node, rdf_type(), app("Metric"));
        out.text(&node, label("label"), Some(&metric.name));
        out.text(&node, dcterms("description"), metric.description.as_deref());
        out.text(&node, app("target"), metric.target.as_deref());
    }
    out.location(&subject, objective.id);
}

/// One charter's actions as a tree, in document order.
struct Tree<'a> {
    by_id: HashMap<Uuid, &'a Action>,
    children: HashMap<Uuid, Vec<Uuid>>,
}

impl<'a> Tree<'a> {
    fn of(actions: &'a [Action]) -> Self {
        let mut children: HashMap<Uuid, Vec<Uuid>> = HashMap::new();
        for action in actions {
            if let Some(parent) = action.parent_id {
                children.entry(parent).or_default().push(action.id);
            }
        }
        Self {
            by_id: actions.iter().map(|action| (action.id, action)).collect(),
            children,
        }
    }

    /// The action and its ancestor actions, nearest first.
    fn lineage(&self, action: &'a Action) -> impl Iterator<Item = &'a Action> + '_ {
        std::iter::successors(Some(action), |a| {
            a.parent_id.and_then(|p| self.by_id.get(&p).copied())
        })
    }

    /// Everything that must close before `action` can start (ontology.md,
    /// Waits): its own predecessors, the sibling before it under a sequential
    /// parent, and its parent's waits.
    fn waits_on(&self, action: &Action) -> Vec<Uuid> {
        let mut waits = action.depends_on();
        let Some(parent) = action.parent_id.and_then(|p| self.by_id.get(&p)) else {
            return waits;
        };
        if parent.is_sequential == Some(true) {
            let siblings = &self.children[&parent.id];
            let index = siblings.iter().position(|id| *id == action.id);
            if let Some(previous) = index.and_then(|i| i.checked_sub(1)) {
                waits.push(siblings[previous]);
            }
        }
        waits.extend(self.waits_on(parent));
        waits
    }
}

fn project_action<Tz>(out: &mut Graph, action: &Action, charter: &Charter, tree: &Tree, zone: &Tz)
where
    Tz: TimeZone,
    Tz::Offset: Display,
{
    let subject = uuid_node(action.id);
    out.node(&subject, rdf_type(), app("Action"));
    out.text(&subject, label("label"), Some(&action.name));
    out.text(
        &subject,
        dcterms("description"),
        action.description.as_deref(),
    );
    out.text(&subject, app("alias"), action.alias.as_deref());
    out.node(&subject, app("state"), action_state(&action.state));
    if let Some(priority) = action.priority {
        out.add(
            &subject,
            app("priority"),
            typed(priority.to_string(), "integer"),
        );
    }
    for tag in action.contexts.iter().flatten() {
        let node = context(out, tag);
        out.node(&subject, app("context"), node);
    }

    if let Some(planned) = &action.planned {
        out.add(&subject, app("plannedStart"), written(&planned.start));
        out.add(
            &subject,
            app("plannedFrom"),
            instant(planned.start.at_in(zone)),
        );
        if let Some(end) = &planned.end {
            out.add(&subject, app("plannedEnd"), written(end));
        }
        let minutes = planned.duration_in(zone).map(|d| d.num_minutes());
        if let Some(minutes) = minutes.filter(|m| *m >= 1) {
            out.add(
                &subject,
                app("durationMinutes"),
                typed(minutes.to_string(), "integer"),
            );
        }
    }
    if let Some(due) = &action.due_date {
        if let Some(start) = &due.start {
            out.add(&subject, app("availableFrom"), written(start));
        }
        out.add(&subject, app("due"), written(&due.end));
    }
    let windows = || tree.lineage(action).filter_map(|a| a.due_date.as_ref());
    if let Some(open) = windows()
        .filter_map(|due| due.start.map(|s| s.at_in(zone)))
        .max()
    {
        out.add(&subject, app("notBefore"), instant(open));
    }
    if let Some(late) = windows().map(|due| due.end.end_instant_in(zone)).min() {
        out.add(&subject, app("lateFrom"), instant(late));
    }
    if let Some(closed) = &action.completed_at {
        out.add(&subject, app("closed"), written(closed));
    }
    if let Some(created) = &action.created_at {
        out.add(&subject, dcterms("created"), written(created));
    }

    for predecessor in action.depends_on() {
        out.node(&subject, app("after"), uuid_node(predecessor));
    }
    for target in tree.waits_on(action) {
        out.node(&subject, app("waitsOn"), uuid_node(target));
    }
    if action.is_sequential == Some(true) {
        out.add(&subject, app("sequential"), typed("true", "boolean"));
    }
    let whole = action.parent_id.unwrap_or(charter.id);
    out.node(&subject, app("partOf"), uuid_node(whole));
    out.location(&subject, action.id);
}

/// A bound as written: `xsd:date`, or `xsd:dateTime` with an offset only if
/// the file wrote one.
fn written(bound: &Bound) -> Term {
    let (value, xsd_type) = bound.xsd();
    typed(value, xsd_type)
}

/// A derived instant, with the viewer zone's offset (`Z` for UTC).
fn instant<Tz>(at: DateTime<Tz>) -> Term
where
    Tz: TimeZone,
    Tz::Offset: Display,
{
    typed(at.to_rfc3339_opts(SecondsFormat::Secs, true), "dateTime")
}

/// A context's slug: leading `+` stripped, trimmed, lowercased, spaces to `-`.
fn slug(tag: &str) -> String {
    tag.trim_start_matches('+')
        .trim()
        .to_lowercase()
        .replace(' ', "-")
}

/// Emit the context node for `tag` and return its IRI.
fn context(out: &mut Graph, tag: &str) -> NamedNode {
    let slug = slug(tag);
    let node = uuid_node(Uuid::new_v5(&CONTEXT_NS, slug.as_bytes()));
    out.node(&node, rdf_type(), app("Context"));
    out.add(&node, label("label"), simple(slug));
    node
}

fn action_state(state: &ActionState) -> NamedNode {
    app(match state {
        ActionState::NotStarted => "NotStarted",
        ActionState::InProgress => "InProgress",
        ActionState::BlockedOrAwaiting => "Blocked",
        ActionState::Completed => "Completed",
        ActionState::Cancelled => "Cancelled",
    })
}

fn charter_state(state: CharterState) -> NamedNode {
    app(match state {
        CharterState::New => "New",
        CharterState::Active => "Active",
        CharterState::Blocked => "Blocked",
        CharterState::Closed => "Closed",
        CharterState::Cancelled => "Cancelled",
    })
}
