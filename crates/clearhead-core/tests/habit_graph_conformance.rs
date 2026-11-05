//! Released Habit values enter through Core's domain model, never RDF input.
#![cfg(feature = "spec-conformance")]

use std::{collections::BTreeSet, fs, path::PathBuf};

use clearhead_core::{
    Action, DomainModel,
    domain::ActionState,
    rdf::{self, app},
};
use serde_json::{Value, json};
use uuid::Uuid;

const HABIT: &str = "01951111-cfa6-718d-b303-d7107f4005b4";
const CHARTER: &str = "01951111-cfa6-718d-b303-d7107f4005b3";
const OBJECTIVE: &str = "01951111-cfa6-718d-b303-d7107f4005b2";
const OCCURRENCE: &str = "8e337bcd-ad84-5158-baec-ac9b81c5ab0f";

fn spec() -> PathBuf {
    std::env::var_os("CLEARHEAD_SPEC_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../specifications")
        })
}

fn action(id: &str) -> Value {
    serde_json::to_value(Action {
        id: id.parse().unwrap(),
        name: "Daily review".into(),
        ..Action::default()
    })
    .unwrap()
}

fn input() -> Value {
    let mut definition = action(HABIT);
    definition["planned"] = serde_json::to_value(
        "2026-10-26"
            .parse::<clearhead_core::domain::time::Planned>()
            .unwrap(),
    )
    .unwrap();
    let mut occurrence = action(OCCURRENCE);
    occurrence["native_occurrence"] = json!({"habit_id": HABIT, "occurrence_key": "D:2026-10-26", "plan_title": "Daily review", "plan_uid": "unrelated-transport@example.org"});
    json!({"objectives": [{"id": OBJECTIVE, "title": "Keep work current"}], "charters": [{
        "id": CHARTER, "title": "Review", "state": "Active", "objectives": [OBJECTIVE],
        "description": null, "alias": null, "parent": null, "plans": [], "actions": [occurrence],
        "habits": [{"definition": definition, "recurrence_rule": "FREQ=DAILY", "template": null, "children": []}]
    }]})
}

fn project(value: Value) -> rdf::Result<BTreeSet<String>> {
    let model: DomainModel = serde_json::from_value(value).unwrap();
    let mut locations = app::Locations::default();
    for (id, path) in [
        (OBJECTIVE, "objectives/keep-work-current.md"),
        (CHARTER, "charters/review.md"),
        (HABIT, "charters/review.habits.actions"),
        (OCCURRENCE, "charters/review.actions"),
    ] {
        locations.files.insert(id.parse().unwrap(), path.into());
    }
    for id in [HABIT, OCCURRENCE] {
        locations.lines.insert(id.parse().unwrap(), 1);
    }
    Ok(app::project_app(
        &model,
        &locations,
        None,
        &chrono::Utc,
        rdf::workspace_graph_name(&Uuid::nil().to_string()),
    )?
    .into_iter()
    .map(|q| oxrdf::Triple::from(q).to_string())
    .collect())
}

#[test]
fn habit_graph_conformance_released_exact_triples() {
    let bytes = fs::read(spec().join("examples/conformance/habits/app.ttl")).unwrap();
    let expected: BTreeSet<_> = oxttl::TurtleParser::new()
        .for_slice(&bytes)
        .map(|t| t.unwrap().to_string())
        .collect();
    let actual = project(input()).unwrap();
    // The released minimal fixture omits this derived value, required by ontology.md.
    let derived = format!(
        "<urn:uuid:{HABIT}> <{}plannedFrom> \"2026-10-26T00:00:00Z\"^^<http://www.w3.org/2001/XMLSchema#dateTime>",
        app::APP_NS
    );
    assert!(actual.contains(&derived));
    assert_eq!(
        actual
            .into_iter()
            .filter(|t| t != &derived)
            .collect::<BTreeSet<_>>(),
        expected
    );
}

#[test]
fn habit_graph_conformance_all_states_and_closed_dates() {
    for (state, term, closed) in [
        (ActionState::NotStarted, "NotStarted", false),
        (ActionState::InProgress, "InProgress", false),
        (ActionState::BlockedOrAwaiting, "Blocked", false),
        (ActionState::Completed, "Completed", true),
        (ActionState::Cancelled, "Cancelled", true),
    ] {
        let mut value = input();
        value["charters"][0]["habits"][0]["definition"]["state"] =
            serde_json::to_value(state).unwrap();
        if closed {
            value["charters"][0]["habits"][0]["definition"]["completed_at"] = serde_json::to_value(
                "2026-11-03"
                    .parse::<clearhead_core::domain::time::Bound>()
                    .unwrap(),
            )
            .unwrap();
        }
        let actual = project(value.clone()).unwrap();
        assert!(actual.contains(&format!(
            "<urn:uuid:{HABIT}> <{}state> <{}{term}>",
            app::APP_NS,
            app::APP_NS
        )));
        assert_eq!(
            actual
                .iter()
                .filter(|t| t.starts_with(&format!("<urn:uuid:{HABIT}>")) && t.contains("#closed>"))
                .count(),
            usize::from(closed)
        );
        value["charters"][0]["habits"][0]["definition"]["completed_at"] = if closed {
            Value::Null
        } else {
            serde_json::to_value(
                "2026-11-03"
                    .parse::<clearhead_core::domain::time::Bound>()
                    .unwrap(),
            )
            .unwrap()
        };
        assert!(
            project(value).is_err(),
            "Habit closure must agree with state {state:?}"
        );
    }
}

#[test]
fn habit_graph_conformance_rule_required() {
    let mut value = input();
    value["charters"][0]["habits"][0]
        .as_object_mut()
        .unwrap()
        .remove("recurrence_rule");
    assert!(serde_json::from_value::<DomainModel>(value).is_err());
    let mut value = input();
    value["charters"][0]["habits"][0]["recurrence_rule"] = json!("");
    assert!(project(value).is_err());
}

#[test]
fn habit_graph_conformance_template_is_not_live_work() {
    let mut value = input();
    let child = action("01951111-cfa6-718d-b303-d7107f4005b5");
    value["charters"][0]["habits"][0]["children"] = json!([child]);
    value["charters"][0]["habits"][0]["template"] = json!("named-review");
    let actual = project(value).unwrap();
    assert!(actual.iter().any(|t| t.contains("#Habit>")));
    assert!(!actual.iter().any(|t| t.contains("01951111-cfa6-718d-b303-d7107f4005b5") || t.contains("#template>")));
    let mut value = input();
    value["charters"][0]["habits"][0]["template"] = json!("named-review");
    assert!(
        project(value)
            .unwrap()
            .iter()
            .any(|t| t.ends_with("#template> \"named-review\""))
    );
}

#[test]
fn habit_graph_conformance_native_slots_are_strings_not_transport_keys() {
    for slot in [
        "D:2026-10-26",
        "F:2026-10-26T09:00:00",
        "U:2026-10-26T09:00:00Z",
        "F:2026-10-26T09:00:00.125",
        "U:2026-10-26T09:00:00.125Z",
    ] {
        let mut value = input();
        value["charters"][0]["actions"][0]["native_occurrence"]["occurrence_key"] = json!(slot);
        value["charters"][0]["actions"][0]["external_occurrence_key"] = json!("transport-only");
        let actual = project(value).unwrap();
        assert!(actual.contains(&format!(
            "<urn:uuid:{OCCURRENCE}> <{}slot> \"{slot}\"",
            app::APP_NS
        )));
        assert!(
            !actual
                .iter()
                .any(|t| t.contains("transport-only") || t.contains("unrelated-transport"))
        );
    }
}

#[test]
fn plain_action_rule_export_matches_published_schema() {
    let mut action = clearhead_core::parse_actions("[ ] Ordinary @2026-11-05 R:FREQ=DAILY\n")
        .unwrap()
        .remove(0);
    assert_eq!(action.recurrence_rule.as_deref(), Some("FREQ=DAILY"));
    let schema: Value =
        serde_json::from_slice(&fs::read(spec().join("schemas/actions.schema.json")).unwrap())
            .unwrap();
    let validator = jsonschema::JSONSchema::compile(&schema).unwrap();
    let document = serde_json::to_value(clearhead_core::schema_export::to_schema_document(&[
        action.clone(),
    ]))
    .unwrap();
    assert_eq!(document["actions"][0]["recurrenceRule"], "FREQ=DAILY");
    assert!(document["actions"][0].get("recurrence_rule").is_none());
    assert!(validator.is_valid(&document), "{document}");
    action.recurrence_rule = None;
    let document =
        serde_json::to_value(clearhead_core::schema_export::to_schema_document(&[action])).unwrap();
    assert!(document["actions"][0].get("recurrenceRule").is_none());
    assert!(validator.is_valid(&document));
}

#[test]
fn habit_graph_conformance_canonical_json_and_sidecar() {
    let model: DomainModel = serde_json::from_value(input()).unwrap();
    let habit = &model.charters[0].habits[0];
    let exported = clearhead_core::schema_export::SchemaAction::from_habit(habit).unwrap();
    let document = json!({"actions": [exported]});
    assert_eq!(document["actions"][0]["recurrenceRule"], "FREQ=DAILY");
    assert_eq!(document["actions"][0]["scheduledDateTime"], "2026-10-26");
    assert_eq!(document["actions"][0]["state"], "not_started");
    let schema: Value =
        serde_json::from_slice(&fs::read(spec().join("schemas/actions.schema.json")).unwrap())
            .unwrap();
    jsonschema::JSONSchema::compile(&schema)
        .unwrap()
        .validate(&document)
        .unwrap_or_else(|errors| {
            panic!(
                "{}",
                errors.map(|e| e.to_string()).collect::<Vec<_>>().join("\n")
            )
        });
    let native = model.charters[0].actions[0]
        .native_occurrence
        .as_ref()
        .unwrap();
    let sidecar = json!({"actions": {OCCURRENCE: {"occurrence": native}}});
    let schema: Value = serde_json::from_slice(
        &fs::read(spec().join("schemas/charter_metadata.schema.json")).unwrap(),
    )
    .unwrap();
    jsonschema::JSONSchema::compile(&schema)
        .unwrap()
        .validate(&sidecar)
        .unwrap_or_else(|errors| {
            panic!(
                "{}",
                errors.map(|e| e.to_string()).collect::<Vec<_>>().join("\n")
            )
        });
    assert_eq!(
        sidecar["actions"][OCCURRENCE]["occurrence"]["habit_id"],
        HABIT
    );
    assert_eq!(
        sidecar["actions"][OCCURRENCE]["occurrence"]["occurrence_key"],
        "D:2026-10-26"
    );
    assert!(
        sidecar["actions"][OCCURRENCE]["occurrence"]
            .get("plan_id")
            .is_none()
    );
}

#[test]
fn habit_graph_conformance_real_projection_passes_shacl_and_cco_mapping() {
    use std::{
        io::Write,
        process::{Command, Stdio},
    };
    // Reuse the platform's existing external SHACL toolchain, not a Core
    // dependency or another RDF implementation in the production boundary.
    let mut child = Command::new("uv")
        .args([
            "run",
            "--quiet",
            "--with",
            "pyshacl==0.40.1",
            "python",
            "-c",
            r#"
import json, sys
from pathlib import Path
from rdflib import Graph
from rdflib.compare import isomorphic
from pyshacl import validate
root = Path(sys.argv[1])
shapes = Graph().parse(root / 'schemas/app.shapes.ttl')
for index, data in enumerate(json.load(sys.stdin)):
    graph = Graph().parse(data=data, format='nt')
    valid, _, report = validate(graph, shacl_graph=shapes, advanced=True)
    assert valid, report
    mapped = Graph()
    for path in sorted((root / 'ontology/mapping').glob('*.rq')):
        mapped += graph.query(path.read_text()).graph
    # The released minimal graph has the same mapping as Core's default graph;
    # derived plannedFrom is deliberately ignored by the published mapping.
    if index == 0:
        expected = Graph()
        fixture = Graph().parse(root / 'examples/conformance/habits/app.ttl')
        for path in sorted((root / 'ontology/mapping').glob('*.rq')):
            expected += fixture.query(path.read_text()).graph
        assert isomorphic(mapped, expected)
"#,
        ])
        .arg(spec())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut values = vec![input()];
    for state in [
        ActionState::InProgress,
        ActionState::BlockedOrAwaiting,
        ActionState::Completed,
        ActionState::Cancelled,
    ] {
        let mut value = input();
        value["charters"][0]["habits"][0]["definition"]["state"] =
            serde_json::to_value(state).unwrap();
        if matches!(state, ActionState::Completed | ActionState::Cancelled) {
            value["charters"][0]["habits"][0]["definition"]["completed_at"] = serde_json::to_value(
                "2026-11-03"
                    .parse::<clearhead_core::domain::time::Bound>()
                    .unwrap(),
            )
            .unwrap();
        }
        values.push(value);
    }
    for slot in [
        "F:2026-10-26T09:00:00",
        "U:2026-10-26T16:00:00Z",
        "F:2026-10-26T09:00:00.125",
        "U:2026-10-26T16:00:00.125Z",
    ] {
        let mut value = input();
        value["charters"][0]["actions"][0]["native_occurrence"]["occurrence_key"] = json!(slot);
        values.push(value);
    }
    let graphs: Vec<_> = values
        .into_iter()
        .map(|value| {
            project(value)
                .unwrap()
                .into_iter()
                .map(|t| format!("{t} .\n"))
                .collect::<String>()
        })
        .collect();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(serde_json::to_string(&graphs).unwrap().as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}
