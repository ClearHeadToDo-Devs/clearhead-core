//! Opt-in conformance: the specification's graph fixture workspace projects to
//! exactly its `expected-app.ttl`, the application graph (ontology.md), in a
//! UTC viewer's zone. Runs only with `--features spec-conformance`.
#![cfg(feature = "spec-conformance")]

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use clearhead_core::rdf::{self, app};

fn spec_dir() -> PathBuf {
    std::env::var("CLEARHEAD_SPEC_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../specifications")
        })
}

fn fixture() -> PathBuf {
    spec_dir().join("examples/conformance/graph")
}

/// Copy the fixture so loading can never touch the specification checkout.
fn copy_dir(from: &Path, to: &Path) {
    fs::create_dir_all(to).unwrap();
    for entry in fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let target = to.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_dir(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), target).unwrap();
        }
    }
}

/// The fixture workspace projected in `zone`, as N-Triples lines.
fn project_fixture<Tz>(zone: &Tz) -> BTreeSet<String>
where
    Tz: chrono::TimeZone,
    Tz::Offset: std::fmt::Display,
{
    let root = tempfile::tempdir().unwrap();
    copy_dir(&fixture().join("workspace"), root.path());

    let workspace = clearhead_cli::filesystem::load_workspace_model(root.path()).unwrap();
    let config: clearhead_core::WorkspaceConfig = serde_json::from_str(
        &fs::read_to_string(root.path().join(".clearhead/config.json")).unwrap(),
    )
    .unwrap();
    let graph = rdf::workspace_graph_name(&workspace.effective_id());
    let locations = app::Locations::of(&workspace);
    let model = clearhead_core::DomainModel::from(workspace);
    let quads = app::project_app(&model, &locations, Some(&config), zone, graph.clone()).unwrap();

    assert!(quads.iter().all(|quad| quad.graph_name == graph));
    quads
        .iter()
        .map(|quad| oxrdf::Triple::from(quad.clone()).to_string())
        .collect()
}

fn expected_triples(path: &Path) -> BTreeSet<String> {
    let turtle = fs::read(path).unwrap();
    oxttl::TurtleParser::new()
        .for_slice(&turtle)
        .map(|triple| triple.unwrap().to_string())
        .collect()
}

#[test]
fn the_fixture_workspace_projects_to_expected_app_ttl() {
    let actual = project_fixture(&chrono::Utc);
    let expected = expected_triples(&fixture().join("expected-app.ttl"));
    let missing: Vec<_> = expected.difference(&actual).collect();
    let extra: Vec<_> = actual.difference(&expected).collect();
    assert!(
        missing.is_empty() && extra.is_empty(),
        "missing ({}):\n{}\n\nextra ({}):\n{}",
        missing.len(),
        missing
            .iter()
            .map(|t| t.as_str())
            .collect::<Vec<_>>()
            .join("\n"),
        extra.len(),
        extra
            .iter()
            .map(|t| t.as_str())
            .collect::<Vec<_>>()
            .join("\n"),
    );
}

#[test]
fn written_values_do_not_depend_on_the_viewers_zone() {
    let is_derived =
        |triple: &String| triple.contains("#notBefore>") || triple.contains("#lateFrom>");
    let written = |set: &BTreeSet<String>| -> BTreeSet<String> {
        set.iter().filter(|t| !is_derived(t)).cloned().collect()
    };
    let utc = project_fixture(&chrono::Utc);
    let east = project_fixture(&chrono::FixedOffset::east_opt(2 * 3600).unwrap());
    assert_eq!(written(&utc), written(&east));
    assert!(
        east.iter()
            .any(|t| t.contains("#lateFrom>") && t.contains("2026-10-05T00:00:00+02:00")),
        "a date's end resolves in the viewer's zone"
    );
}
