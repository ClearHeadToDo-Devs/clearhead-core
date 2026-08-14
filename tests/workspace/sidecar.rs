use super::common::*;
use clearhead_core::load_domain_model;

#[test]
fn sidecar_hydrates_acts_on_load() {
    use uuid::Uuid;

    let uuid = "01951111-0000-7000-0000-000000000001";
    let sidecar_json =
        format!(r#"{{"acts": {{"{uuid}": {{"created": "2024-01-15T08:00:00+00:00"}}}}}}"#);
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] Task one #{uuid}\n")),
        (".work.json", &sidecar_json),
    ]);

    let model = load_domain_model(workspace.path()).unwrap();
    let action = model
        .charters
        .iter()
        .flat_map(|c| c.actions.iter())
        .find(|a| a.id == Uuid::parse_str(uuid).unwrap())
        .expect("action not found in model");

    assert!(
        action.created_at.is_some(),
        "sidecar created date should be hydrated into Action"
    );
}

#[test]
fn orphaned_sidecar_hydrates_acts_by_uuid() {
    use uuid::Uuid;

    // The action lives in work.actions, but its sidecar sits at a path matching
    // no .actions file — as if work.actions had been renamed and the sidecar left
    // behind. Hydration must still reach it by UUID.
    let uuid = "01951111-0000-7000-0000-000000000030";
    let sidecar_json =
        format!(r#"{{"acts": {{"{uuid}": {{"created": "2024-01-15T08:00:00+00:00"}}}}}}"#);
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] Task one #{uuid}\n")),
        (".stale-name.json", &sidecar_json),
    ]);

    let model = load_domain_model(workspace.path()).unwrap();
    let action = model
        .charters
        .iter()
        .flat_map(|c| c.actions.iter())
        .find(|a| a.id == Uuid::parse_str(uuid).unwrap())
        .expect("action not found in model");

    assert!(
        action.created_at.is_some(),
        "an orphaned sidecar's created should still hydrate by UUID"
    );
}

#[test]
fn sidecar_charter_id_supersedes_derived_id() {
    use uuid::Uuid;

    // An action-only charter derives its id from the filename (v5). A recorded
    // sidecar charter.id overrides that seed, so identity lives in the data and
    // survives a rename that would otherwise recompute it.
    let recorded = "01951111-0000-7000-0000-0000000000c0";
    let workspace = make_workspace(&[
        (
            "work.actions",
            "[ ] a task #01951111-0000-7000-0000-0000000000c1\n",
        ),
        (
            ".work.json",
            &format!(r#"{{"charter": {{"id": "{recorded}"}}}}"#),
        ),
    ]);

    let model = load_domain_model(workspace.path()).unwrap();
    let charter = model
        .charters
        .iter()
        .find(|c| c.title == "work" || c.alias.as_deref() == Some("work"))
        .expect("charter 'work' should load");

    assert_eq!(
        charter.id,
        Uuid::parse_str(recorded).unwrap(),
        "recorded sidecar charter.id must supersede the derived v5(name) id"
    );
}

#[test]
fn explicit_frontmatter_id_wins_over_sidecar() {
    use uuid::Uuid;

    // A declared frontmatter id is authoritative — a recorded sidecar id never
    // overrides it (the sidecar is a reference; the declaration wins).
    let front = "01951111-0000-7000-0000-0000000000d0";
    let side = "01951111-0000-7000-0000-0000000000d9";
    let workspace = make_workspace(&[
        (
            "work.actions",
            "[ ] a task #01951111-0000-7000-0000-0000000000d1\n",
        ),
        (
            "work.md",
            &format!("---\nid: {front}\nalias: work\n---\n# Work\n"),
        ),
        (
            ".work.json",
            &format!(r#"{{"charter": {{"id": "{side}"}}}}"#),
        ),
    ]);

    let model = load_domain_model(workspace.path()).unwrap();
    let charter = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("work"))
        .expect("charter 'work' should load");

    assert_eq!(
        charter.id,
        Uuid::parse_str(front).unwrap(),
        "an explicit frontmatter id must win over a recorded sidecar charter.id"
    );
}

#[test]
fn sidecar_does_not_overwrite_dsl_created() {
    use uuid::Uuid;

    let uuid = "01951111-0000-7000-0000-000000000002";
    let sidecar_json =
        format!(r#"{{"acts": {{"{uuid}": {{"created": "2020-01-01T00:00:00+00:00"}}}}}}"#);
    let workspace = make_workspace(&[
        (
            "work.actions",
            &format!("[ ] Task #{uuid}\n  ^ 2024-06-01T10:00:00\n"),
        ),
        (".work.json", &sidecar_json),
    ]);

    let model = load_domain_model(workspace.path()).unwrap();
    let action = model
        .charters
        .iter()
        .flat_map(|c| c.actions.iter())
        .find(|a| a.id == Uuid::parse_str(uuid).unwrap())
        .expect("action not found in model");

    let created = action
        .created_at
        .expect("created_at should be set from DSL ^ date");
    assert_eq!(
        created.format("%Y").to_string(),
        "2024",
        "DSL ^ date (2024) must win over sidecar date (2020)"
    );
}
