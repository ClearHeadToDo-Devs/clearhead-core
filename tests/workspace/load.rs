use super::common::*;
use clearhead_core::workspace::read_workspace;
use clearhead_core::{
    ManifestSourceType, collect_workspace_manifest, load_domain_model, load_workspace,
};
#[cfg(feature = "formatting")]
use clearhead_core::{diff_domain_models, save_domain_model};
use std::fs;
use std::path::Path;

// --- Tests ---

#[cfg(feature = "formatting")]
#[test]
fn roundtrip_preserves_model() {
    // Fixture uses explicit UUIDs so IDs are stable across loads.
    let workspace = make_workspace(&[(
        "tasks.actions",
        "[ ] Task one #01951111-0000-7000-0000-000000000001\n\
         [ ] Task two #01951111-0000-7000-0000-000000000002\n\
         > [ ] Subtask of two #01951111-0000-7000-0000-000000000003\n",
    )]);

    let model_a = load_domain_model(workspace.path()).expect("first load failed");
    save_domain_model(workspace.path(), &model_a).expect("save failed");
    let model_b = load_domain_model(workspace.path()).expect("second load failed");

    assert!(
        diff_domain_models(&model_a, &model_b).is_empty(),
        "model changed across a save/reload cycle"
    );
}

#[test]
fn load_discovers_all_action_files() {
    // Two files → two charters, three direct actions total.
    let workspace = make_workspace(&[
        (
            "work.actions",
            "[ ] Write tests #01951111-0000-7000-0000-000000000010\n\
             [ ] Review PR #01951111-0000-7000-0000-000000000011\n",
        ),
        (
            "personal.actions",
            "[ ] Buy groceries #01951111-0000-7000-0000-000000000020\n",
        ),
    ]);

    let model = load_domain_model(workspace.path()).expect("load failed");

    assert_eq!(model.charters.len(), 2, "expected 2 charters");
    assert_eq!(model.all_actions().len(), 3, "expected 3 actions total");
}

#[test]
fn load_infers_parent_charter_from_directory_structure() {
    // A file nested under a subdirectory should infer a parent charter.
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let data = dir.path().join(".clearhead").join("charters");
    let nested = data.join("project");
    fs::create_dir_all(&nested).expect("failed to create nested dir");
    fs::write(
        nested.join("tasks.actions"),
        "[ ] Nested task #01951111-0000-7000-0000-000000000030\n",
    )
    .expect("failed to write fixture");

    let model = load_domain_model(dir.path()).expect("load failed");

    let charter = model
        .charters
        .iter()
        .find(|c| c.title == "tasks")
        .expect("charter not found");
    assert!(
        charter.parent.is_some(),
        "nested charter should have a parent"
    );
}

#[test]
fn load_infers_parent_for_markdown_only_charters_from_directory_structure() {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let charters = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(charters.join("someday").join("agent-surface"))
        .expect("failed to create nested charter dirs");
    fs::write(
        charters.join("someday").join("README.md"),
        "---\nalias: someday\n---\n# Someday\n",
    )
    .expect("failed to write parent charter");
    fs::write(
        charters
            .join("someday")
            .join("agent-surface")
            .join("README.md"),
        "---\nalias: agent-surface\n---\n# Agent Surface\n",
    )
    .expect("failed to write child charter");

    let model = load_domain_model(dir.path()).expect("load failed");
    let child = model
        .charters
        .iter()
        .find(|charter| charter.alias.as_deref() == Some("agent-surface"))
        .expect("child charter not found");
    assert_eq!(child.parent.as_deref(), Some("someday"));
}

#[test]
fn explicit_markdown_parent_overrides_directory_hierarchy() {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let charters = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(charters.join("directory-parent").join("child"))
        .expect("failed to create nested charter dirs");
    fs::create_dir_all(charters.join("declared-parent"))
        .expect("failed to create declared parent dir");
    fs::write(
        charters
            .join("directory-parent")
            .join("child")
            .join("README.md"),
        "---\nalias: child\nparent: declared-parent\n---\n# Child\n",
    )
    .expect("failed to write child charter");
    fs::write(
        charters.join("declared-parent").join("README.md"),
        "---\nalias: declared-parent\n---\n# Declared Parent\n",
    )
    .expect("failed to write declared parent charter");

    let model = load_domain_model(dir.path()).expect("load failed");
    let child = model
        .charters
        .iter()
        .find(|charter| charter.alias.as_deref() == Some("child"))
        .expect("child charter not found");
    assert_eq!(child.parent.as_deref(), Some("declared-parent"));
}

#[test]
fn project_layout_next_actions_uses_project_name_as_charter() {
    // In project layout, `next.actions` at the root of `.clearhead/` is the
    // "primary" file — its charter name becomes the project directory name,
    // not "next".
    let (_outer, project) = make_named_project(
        "my-project",
        &[(
            "next.actions",
            "[ ] Root task #01951111-0000-7000-0000-000000000050\n",
        )],
    );

    let model = load_domain_model(&project).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    assert_eq!(model.charters[0].title, "my-project");
}

#[test]
fn workspace_construction_assigns_collection_ownership_without_ics_resources() {
    let (_outer, project) = make_named_project(
        "my-project",
        &[
            (
                "next.actions",
                "[ ] Root task #01951111-0000-7000-0000-000000000052\n",
            ),
            (
                "linux/next.actions",
                "[ ] Linux task #01951111-0000-7000-0000-000000000053\n",
            ),
            (
                "work/next.actions",
                "[ ] Work task #01951111-0000-7000-0000-000000000055\n",
            ),
            (
                "work/feature/next.actions",
                "[ ] Feature task #01951111-0000-7000-0000-000000000054\n",
            ),
        ],
    );

    let charters = load_workspace(&project).expect("load failed");
    let ownership: std::collections::HashMap<_, _> = charters
        .iter()
        .map(|charter| {
            (
                charter.actions_file.as_deref().unwrap(),
                charter.plans_dir.as_path(),
            )
        })
        .collect();

    assert_eq!(
        ownership.get(Path::new("next.actions")),
        Some(&Path::new("next"))
    );
    assert_eq!(
        ownership.get(Path::new("linux/next.actions")),
        Some(&Path::new("linux"))
    );
    assert_eq!(
        ownership.get(Path::new("work/feature/next.actions")),
        Some(&Path::new("work-feature"))
    );
}

#[test]
fn unowned_calendar_collection_is_quarantined_instead_of_creating_a_charter() {
    let (_outer, project) = make_named_project(
        "my-project",
        &[(
            "next.actions",
            "[ ] Root task #01951111-0000-7000-0000-000000000056\n",
        )],
    );
    let unknown = project.join(".clearhead/plans/surprise");
    fs::create_dir_all(&unknown).expect("create unowned collection");
    fs::write(
        unknown.join("task.ics"),
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:surprise\r\nSUMMARY:Surprise\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    )
    .expect("write unowned resource");

    let read = read_workspace(&project).expect("read failed");
    assert_eq!(read.charters.len(), 1, "must not invent a charter");
    assert!(
        read.findings.iter().any(|finding| {
            finding.code == "unowned-plans-collection" && finding.path == Path::new("surprise")
        }),
        "unowned collection should be a typed finding: {:?}",
        read.findings
    );
}

#[test]
fn project_layout_root_plans_dir_uses_project_name_as_charter() {
    let (_outer, project) = make_named_project(
        "my-project",
        &[(
            "next.actions",
            "[ ] Root task #01951111-0000-7000-0000-000000000051\n",
        )],
    );
    // Plans live at <data_root>/plans/ (parallel to charters/), root charter uses "next/" slug.
    let plans_dir = project.join(".clearhead").join("plans").join("next");
    fs::create_dir_all(&plans_dir).expect("create plans/next dir");
    fs::write(
        plans_dir.join("root-plan-1.ics"),
        "BEGIN:VCALENDAR\n\
VERSION:2.0\n\
PRODID:-//clearhead//NONSGML v1.0//EN\n\
BEGIN:VTODO\n\
UID:root-plan-1\n\
DTSTART:20260101T080000Z\n\
RRULE:FREQ=WEEKLY\n\
SUMMARY:Project root plan\n\
END:VTODO\n\
END:VCALENDAR\n",
    )
    .expect("write plan ics");

    // This test is about plan-to-charter attachment, not occurrence rendering;
    // the loaded model carries no projected occurrences, so it is clock-independent.
    let model = load_domain_model(&project).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    assert_eq!(model.charters[0].alias.as_deref(), Some("my-project"));
    assert_eq!(model.charters[0].plans.len(), 1);
    assert_eq!(model.charters[0].actions.len(), 1);
    assert!(
        model.charters[0]
            .plans
            .iter()
            .any(|plan| plan.name == "Project root plan"),
        "root plans/ plan should be attached to the project charter"
    );

    let manifest = collect_workspace_manifest(&project).expect("manifest failed");
    assert_eq!(manifest.len(), 1);
    assert_eq!(manifest[0].path, "next.actions");
    assert_eq!(manifest[0].charter_name, "my-project");
    assert_eq!(manifest[0].source_type, ManifestSourceType::ActionsPlusIcs);
}

#[test]
fn user_layout_uses_filename_as_charter() {
    // In user layout (no `.clearhead/`), there is no special project root —
    // every file's stem becomes the charter name directly.
    let workspace = make_user_workspace(&[(
        "next.actions",
        "[ ] User task #01951111-0000-7000-0000-000000000060\n",
    )]);

    let model = load_domain_model(workspace.path()).expect("load failed");

    assert_eq!(model.charters.len(), 1);
    assert_eq!(model.charters[0].title, "next");
}

#[cfg(feature = "formatting")]
#[test]
fn roundtrip_is_stable_across_multiple_cycles() {
    // Repeated save/reload should converge — not drift on each cycle.
    let workspace = make_workspace(&[(
        "tasks.actions",
        "[ ] Stable task #01951111-0000-7000-0000-000000000040\n",
    )]);

    let model_a = load_domain_model(workspace.path()).expect("load failed");
    save_domain_model(workspace.path(), &model_a).expect("first save failed");

    let model_b = load_domain_model(workspace.path()).expect("second load failed");
    save_domain_model(workspace.path(), &model_b).expect("second save failed");

    let model_c = load_domain_model(workspace.path()).expect("third load failed");

    assert!(
        diff_domain_models(&model_b, &model_c).is_empty(),
        "model drifted between save cycles"
    );
}

// --- Explicit charter (.md) + implicit (.actions) merge tests ---

#[test]
fn explicit_charter_title_does_not_overwrite_alias() {
    // An explicit .md file with a human-readable title and NO alias in frontmatter
    // should NOT clobber the alias set by implicit_charter() during .actions loading.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("health.actions"),
        "[ ] Morning run #01951111-0000-7000-0000-000000000100\n",
    )
    .expect("write actions");
    fs::write(
        data.join("health.md"),
        "# Health & Fitness\n\nStay healthy.\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");
    let charter = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("health"))
        .expect("health charter not found by alias");

    assert_eq!(
        charter.title, "Health & Fitness",
        "title should be human-readable"
    );
    assert_eq!(
        charter.alias,
        Some("health".to_string()),
        "alias should be the inferred filesystem name"
    );
}

#[test]
fn explicit_charter_with_alias_in_frontmatter_overrides_correctly() {
    // An explicit .md with `alias: fitness` should override the inferred alias ("h").
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("h.actions"),
        "[ ] Morning run #01951111-0000-7000-0000-000000000110\n",
    )
    .expect("write actions");
    fs::write(
        data.join("h.md"),
        "---\nalias: fitness\n---\n# Health & Fitness\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");
    let charter = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("fitness"))
        .expect("charter not found by explicit alias");

    assert_eq!(charter.title, "Health & Fitness");
    assert_eq!(charter.alias, Some("fitness".to_string()));
}

#[test]
fn alias_is_always_set_after_load() {
    // alias should be Some(...) for every charter regardless of whether it has
    // an explicit .md file, an aliased .md file, or no .md at all.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(&data).expect("create .clearhead");

    // Implicit only
    fs::write(
        data.join("implicit.actions"),
        "[ ] Task #01951111-0000-7000-0000-000000000120\n",
    )
    .expect("write actions");

    // Explicit with alias
    fs::write(
        data.join("explicit.actions"),
        "[ ] Task #01951111-0000-7000-0000-000000000121\n",
    )
    .expect("write actions");
    fs::write(
        data.join("explicit.md"),
        "---\nalias: ex\n---\n# Explicit Charter\n",
    )
    .expect("write md");

    // Explicit without alias
    fs::write(
        data.join("noalias.actions"),
        "[ ] Task #01951111-0000-7000-0000-000000000122\n",
    )
    .expect("write actions");
    fs::write(data.join("noalias.md"), "# No Alias Charter\n").expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");

    for charter in &model.charters {
        assert!(
            charter.alias.is_some(),
            "charter '{}' has no alias — invariant violated",
            charter.title
        );
    }
}

#[test]
fn parent_reference_uses_machine_key_not_title() {
    // When a parent charter has a human-readable title (from .md), child charters
    // discovered via path inference should still have parent = machine key (inferred name).
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead").join("charters");
    let work_sub = data.join("work");
    fs::create_dir_all(&work_sub).expect("create work subdir");

    fs::write(
        data.join("next.actions"),
        "[ ] Root task #01951111-0000-7000-0000-000000000130\n",
    )
    .expect("write root actions");
    fs::write(
        data.join("work.actions"),
        "[ ] Work task #01951111-0000-7000-0000-000000000131\n",
    )
    .expect("write work actions");
    // Explicit .md overwrites title with human-readable string, no alias
    fs::write(data.join("work.md"), "# Work Stuff\n\nAll work items.\n").expect("write work md");
    fs::write(
        work_sub.join("ops.actions"),
        "[ ] Ops task #01951111-0000-7000-0000-000000000132\n",
    )
    .expect("write ops actions");

    let model = load_domain_model(dir.path()).expect("load failed");

    let ops = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("ops") || c.title == "ops")
        .expect("ops charter not found");

    assert_eq!(
        ops.parent.as_deref(),
        Some("work"),
        "parent should be machine key 'work', not title 'Work Stuff'"
    );

    // Verify the work charter itself got the human-readable title
    let work = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("work"))
        .expect("work charter not found");
    assert_eq!(work.title, "Work Stuff");
}

#[test]
fn load_md_only_charter_produces_empty_plan_list() {
    // A .md file with no matching .actions file should produce a charter with zero plans/actions,
    // not be silently dropped.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("health.md"),
        "---\nalias: health\n---\n# Health & Fitness\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load failed");

    let charter = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("health"))
        .expect("health charter should be present even without .actions file");

    assert_eq!(
        charter.plans.len(),
        0,
        "charter from .md-only should have no plans"
    );
    assert_eq!(
        charter.actions.len(),
        0,
        "charter from .md-only should have no actions"
    );
    assert_eq!(charter.title, "Health & Fitness");
}

#[test]
fn load_quarantines_semantics_when_file_has_parse_issues() {
    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Valid one #01961111-0000-7000-0000-000000000001\n\
         this line is malformed and should not be parsed\n\
         [ ] Valid two #01961111-0000-7000-0000-000000000002\n",
    )]);

    let read = clearhead_core::workspace::read_workspace(workspace.path())
        .expect("diagnostic read should succeed");
    assert!(
        read.findings.iter().any(|finding| {
            finding.code == "syntax-errors" && finding.message.contains("file quarantined")
        }),
        "recovery should remain visible as a diagnostic finding"
    );
    assert!(
        read.charters
            .iter()
            .all(|charter| charter.actions.is_empty()),
        "recovered actions must not enter semantic workspace state"
    );

    let model = load_domain_model(workspace.path()).expect("quarantined load should still succeed");
    assert!(
        model
            .charters
            .iter()
            .all(|charter| charter.actions.is_empty()),
        "domain lowering must not attach recovered fields or UUIDs"
    );
}

#[test]
fn unresolvable_parent_does_not_crash_load() {
    // A charter with `parent: "Work Stuff"` (display title) should load
    // successfully — the unresolvable parent only emits a warning, it does
    // not abort. We can't easily capture stderr here, so we just assert
    // the load succeeds and the parent string is preserved as-is.
    let dir = tempfile::tempdir().expect("tempdir");
    let data = dir.path().join(".clearhead").join("charters");
    fs::create_dir_all(&data).expect("create .clearhead");

    fs::write(
        data.join("child.actions"),
        "[ ] A task #01960000-9999-7000-0000-000000000001\n",
    )
    .expect("write actions");
    fs::write(
        data.join("child.md"),
        "---\nparent: Work Stuff\n---\n# Child Charter\n",
    )
    .expect("write md");

    let model = load_domain_model(dir.path()).expect("load should succeed despite bad parent");
    let child = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("child"))
        .expect("child charter");
    assert_eq!(
        child.parent.as_deref(),
        Some("Work Stuff"),
        "bad parent should be preserved, not silently dropped"
    );
}

#[test]
fn mixed_workspace_loads_actions_and_ics_plans() {
    let root = fixture_path("project-mixed");
    let model = load_domain_model(&root).expect("load failed");

    let mut names: Vec<String> = model
        .charters
        .iter()
        .map(|c| c.alias.clone().unwrap_or_default())
        .collect();
    names.sort();
    assert_eq!(names, vec!["health", "project-mixed"]);

    let project = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("project-mixed"))
        .unwrap();
    assert_eq!(project.plans.len(), 0);
    assert_eq!(project.actions.len(), 1);
    assert_eq!(project.actions[0].name, "Buy domain name");

    let health = model
        .charters
        .iter()
        .find(|c| c.alias.as_deref() == Some("health"))
        .unwrap();
    assert_eq!(health.plans.len(), 1);

    let plan = &health.plans[0];
    assert_eq!(plan.name, "Go for a run");
    assert_eq!(plan.external_id.as_deref(), Some("health-workout-1"));
    assert_eq!(plan.template_name.as_deref(), Some("workout"));
    assert!(plan.recurrence.is_some(), "recurrence should be populated");
}

#[test]
fn flat_project_charter_owns_collection_named_by_its_workspace_anchor() {
    let dir = tempfile::tempdir().unwrap();
    let project = dir.path().join("MixedCaseProject");
    let charters = project.join(".clearhead/charters");
    let plans = project.join(".clearhead/plans").join("dogfood");
    fs::create_dir_all(&charters).unwrap();
    fs::create_dir_all(&plans).unwrap();
    fs::write(charters.join("next.actions"), "").unwrap();
    fs::write(charters.join("dogfood.actions"), "").unwrap();
    fs::write(
        plans.join("daily.ics"),
        "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VTODO\r\nUID:daily\r\nSUMMARY:Daily check\r\nDTSTART:20260730T090000Z\r\nRRULE:FREQ=DAILY\r\nEND:VTODO\r\nEND:VCALENDAR\r\n",
    )
    .unwrap();

    let model = load_domain_model(&project).unwrap();
    assert_eq!(model.charters.len(), 2, "no charterless plan owner");
    let dogfood = model
        .charters
        .iter()
        .find(|charter| charter.alias.as_deref() == Some("dogfood"))
        .unwrap();
    assert_eq!(dogfood.parent.as_deref(), Some("MixedCaseProject"));
    assert_eq!(dogfood.plans.len(), 1);
    assert_eq!(dogfood.plans[0].name, "Daily check");
}

#[test]
fn mixed_workspace_ron_snapshots() {
    let root = fixture_path("project-mixed");
    // The on-disk model carries no projected occurrences (they materialize on the
    // write path), so the golden file is deterministic without a fixed clock.
    let model = load_domain_model(&root).expect("load failed");
    let ron = model_to_ron(&model);
    assert_snapshot(&fixture_path("project-mixed.ron"), &ron);

    let manifest = collect_workspace_manifest(&root).expect("manifest failed");
    let manifest_ron = manifest_to_ron(&manifest);
    assert_snapshot(&fixture_path("project-mixed-manifest.ron"), &manifest_ron);
}
