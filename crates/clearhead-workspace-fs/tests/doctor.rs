use std::fs;
use std::path::Path;
use tempfile::TempDir;

fn make_workspace(files: &[(&str, &str)]) -> TempDir {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    let data = dir.path().join(".clearhead/charters");
    fs::create_dir_all(&data).expect("failed to create charters");
    for (name, content) in files {
        let path = data.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create fixture parent");
        }
        fs::write(path, content).expect("failed to write fixture");
    }
    dir
}

fn make_named_project(name: &str, files: &[(&str, &str)]) -> (TempDir, std::path::PathBuf) {
    let outer = tempfile::tempdir().expect("failed to create temp dir");
    let project = outer.path().join(name);
    let data = project.join(".clearhead/charters");
    fs::create_dir_all(&data).expect("failed to create project");
    for (filename, content) in files {
        let path = data.join(filename);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create fixture parent");
        }
        fs::write(path, content).expect("failed to write fixture");
    }
    (outer, project)
}
// --- Doctor: read-only cross-file fsck (Decision 34) ---

/// Stamp a durable identity onto the workspace at `root` — what `clearhead init`
/// would have written to `workspace.json`. Doctor flags a missing `workspace_id`
/// (read from the manifest), so fixtures testing *other* findings call this to
/// stay out of that check's way. Returns `root` for inline use.
fn initialized(root: &Path) -> &Path {
    let manifest = clearhead_core::workspace::WorkspaceManifest {
        workspace_id: Some("01951111-0000-7000-0000-00000000c0f9".to_string()),
        workspace_name: Some("test".to_string()),
        created_at: None,
    };
    clearhead_workspace_fs::write_workspace_manifest(root, &manifest)
        .expect("write workspace manifest");
    root
}

#[test]
fn doctor_flags_uninitialized_workspace() {
    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Task one #01951111-0000-7000-0000-000000000010\n",
    )]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(workspace.path(), None)
        .expect("diagnose failed");
    let finding = diagnosis
        .findings
        .iter()
        .find(|f| f.code == "uninitialized-workspace")
        .expect("missing workspace_id should be a finding");
    assert!(finding.message.contains("clearhead init"));
}

#[test]
fn doctor_reports_clean_on_a_coherent_workspace() {
    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Task one #01951111-0000-7000-0000-000000000010\n",
    )]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    // The tempdir root charter is inferred but has no charter file — filter to
    // real violations/warnings that concern the fixture.
    let relevant: Vec<_> = diagnosis
        .findings
        .iter()
        .filter(|f| f.code != "unresolvable-parent")
        .collect();
    assert!(relevant.is_empty(), "unexpected findings: {:?}", relevant);
    assert_eq!(diagnosis.checked_actions, 1);
}

#[test]
fn doctor_warns_about_active_work_beneath_new_ancestry() {
    let workspace = make_workspace(&[
        (
            "root.md",
            "---\nid: 01951111-0000-7000-0000-000000000020\nalias: root\nstate: New\n---\n# Root\n",
        ),
        ("root.actions", ""),
        (
            "child.md",
            "---\nid: 01951111-0000-7000-0000-000000000021\nalias: child\nparent: root\nstate: Active\n---\n# Child\n",
        ),
        (
            "child.actions",
            "[-] Doing work #01951111-0000-7000-0000-000000000022\n",
        ),
    ]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    let codes: Vec<_> = diagnosis
        .findings
        .iter()
        .map(|finding| finding.code.as_str())
        .collect();

    assert!(codes.contains(&"active-charter-under-inactive-ancestor"));
    assert!(codes.contains(&"in-progress-action-under-inactive-charter"));
}

#[test]
fn doctor_rejects_open_work_beneath_terminal_ancestry() {
    let workspace = make_workspace(&[
        (
            "root.md",
            "---\nid: 01951111-0000-7000-0000-000000000030\nalias: root\nstate: Closed\n---\n# Root\n",
        ),
        ("root.actions", ""),
        (
            "child.md",
            "---\nid: 01951111-0000-7000-0000-000000000031\nalias: child\nparent: root\n---\n# Child\n",
        ),
        (
            "child.actions",
            "[ ] Remaining work #01951111-0000-7000-0000-000000000032\n",
        ),
    ]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    let codes: Vec<_> = diagnosis
        .findings
        .iter()
        .map(|finding| finding.code.as_str())
        .collect();

    assert!(codes.contains(&"open-charter-under-terminal-ancestor"));
    assert!(codes.contains(&"open-action-under-terminal-charter"));
    assert_eq!(diagnosis.violations(), 2, "{:?}", diagnosis.findings);
}

#[test]
fn doctor_flags_duplicate_uuids_across_files() {
    let uuid = "01951111-0000-7000-0000-000000000011";
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] In work #{uuid}\n")),
        (
            "home.actions",
            &format!("[ ] Copy-pasted into home #{uuid}\n"),
        ),
    ]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    let finding = diagnosis
        .findings
        .iter()
        .find(|f| f.code == "duplicate-uuid")
        .expect("duplicate uuid should be a finding");
    assert!(finding.message.contains(uuid));
    assert!(finding.message.contains("2 times"));
}

#[test]
fn doctor_flags_dangling_predecessor_but_not_completed_one() {
    // `dangling` points at a uuid that exists nowhere; `closed-dep` points at
    // an action that lives in the completed archive — that one is coherent.
    let workspace = make_workspace(&[
        (
            "work.actions",
            "[ ] dangling <01951111-dead-7000-0000-000000000000 #01951111-0000-7000-0000-000000000012\n\
             [ ] closed-dep <01951111-0000-7000-0000-000000000014 #01951111-0000-7000-0000-000000000013\n",
        ),
        (
            "work.completed.actions",
            "[x] Done thing #01951111-0000-7000-0000-000000000014\n",
        ),
    ]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    let dangling: Vec<_> = diagnosis
        .findings
        .iter()
        .filter(|f| f.code == "dangling-predecessor")
        .collect();
    assert_eq!(dangling.len(), 1, "findings: {:?}", diagnosis.findings);
    assert!(dangling[0].message.contains("dangling"));
    assert!(dangling[0].message.contains("01951111-dead"));
}

#[test]
fn doctor_resolves_predecessors_into_the_archive_three_ways() {
    // Three live actions, each depending on a target that has left the live set:
    //   ...00a1 → archived Completed  → satisfied (no finding)
    //   ...00a2 → archived Cancelled  → abandoned (warning)
    //   ...dead → nowhere at all      → dangling  (violation)
    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] needs-satisfied <01951111-0000-7000-0000-0000000000a1 #01951111-0000-7000-0000-0000000000b1\n\
         [ ] needs-abandoned <01951111-0000-7000-0000-0000000000a2 #01951111-0000-7000-0000-0000000000b2\n\
         [ ] needs-dangling <01951111-dead-7000-0000-0000000000a3 #01951111-0000-7000-0000-0000000000b3\n",
    )]);

    // The targets live in the archive/ region as plaintext, excluded from the
    // default read but consulted for predecessor resolution.
    let archive = workspace.path().join(".clearhead").join("archive");
    fs::create_dir_all(&archive).expect("create archive region");
    fs::write(
        archive.join("proj.actions"),
        "[x] finished #01951111-0000-7000-0000-0000000000a1\n\
         [_] dropped #01951111-0000-7000-0000-0000000000a2\n",
    )
    .expect("write archived actions");

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");

    let dangling: Vec<_> = diagnosis
        .findings
        .iter()
        .filter(|f| f.code == "dangling-predecessor")
        .collect();
    assert_eq!(
        dangling.len(),
        1,
        "only the true break is dangling: {:?}",
        diagnosis.findings
    );
    assert!(dangling[0].message.contains("01951111-dead"));

    let abandoned: Vec<_> = diagnosis
        .findings
        .iter()
        .filter(|f| f.code == "abandoned-predecessor")
        .collect();
    assert_eq!(
        abandoned.len(),
        1,
        "cancelled archived target is abandoned: {:?}",
        diagnosis.findings
    );
    assert!(abandoned[0].message.contains("0000000000a2"));

    // The satisfied dependency is healthy — it produces no finding at all.
    assert!(
        !diagnosis
            .findings
            .iter()
            .any(|f| f.message.contains("0000000000a1")),
        "a completed archived dependency must be silent: {:?}",
        diagnosis.findings
    );
}

#[test]
fn doctor_flags_orphaned_sidecar_entry() {
    let live = "01951111-0000-7000-0000-000000000015";
    let gone = "01951111-0000-7000-0000-000000000016";
    let sidecar = format!(
        r#"{{"acts": {{"{live}": {{"created": "2026-01-01T00:00:00+00:00"}}, "{gone}": {{"created": "2026-01-01T00:00:00+00:00"}}}}}}"#
    );
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] Still here #{live}\n")),
        (".work.json", &sidecar),
    ]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    let orphans: Vec<_> = diagnosis
        .findings
        .iter()
        .filter(|f| f.code == "sidecar-orphan")
        .collect();
    assert_eq!(orphans.len(), 1, "findings: {:?}", diagnosis.findings);
    assert!(orphans[0].message.contains(gone));
}

#[test]
fn doctor_does_not_prune_sidecars_while_source_is_quarantined() {
    let id = "019f0000-0000-7000-8000-000000000001";
    let sidecar =
        format!(r#"{{"actions": {{"{id}": {{"created": "2026-01-01T00:00:00+00:00"}}}}}}"#);
    let workspace = make_workspace(&[
        (
            "work.actions",
            &format!("[ ] Read [[docs|https://example.com\n[ ] Next #{id}\n"),
        ),
        (".work.json", &sidecar),
    ]);

    let diagnosis =
        clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None).unwrap();
    assert!(
        diagnosis.findings.iter().any(|f| f.code == "syntax-errors"),
        "the source-integrity finding must remain visible"
    );
    assert!(
        !diagnosis.findings.iter().any(|finding| {
            finding.code == "sidecar-orphan" || finding.code == "orphaned-sidecar"
        }),
        "quarantine is not proof that sidecar provenance is stale: {:?}",
        diagnosis.findings
    );
}

#[test]
fn doctor_preserves_sidecar_metadata_after_an_action_moves_charters() {
    let moved = "01951111-0000-7000-0000-000000000019";
    let sidecar =
        format!(r#"{{"actions": {{"{moved}": {{"created": "2026-01-01T00:00:00+00:00"}}}}}}"#);
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] Moved here #{moved}\n")),
        (".old-home.json", &sidecar),
    ]);

    let diagnosis =
        clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None).unwrap();
    assert!(
        !diagnosis.findings.iter().any(|finding| {
            finding.code == "sidecar-orphan" || finding.code == "orphaned-sidecar"
        }),
        "findings: {:?}",
        diagnosis.findings
    );
}

#[test]
fn doctor_finds_project_root_history_at_project_named_completed_path() {
    let completed_id = "01951111-0000-7000-0000-000000000017";
    let legacy_id = "01951111-0000-7000-0000-000000000018";
    let sidecar = format!(
        r#"{{"actions": {{"{completed_id}": {{"created": "2026-01-01T00:00:00+00:00"}}, "{legacy_id}": {{"created": "2026-01-01T00:00:00+00:00"}}}}}}"#
    );
    let workspace = make_workspace(&[("next.actions", ""), (".next.json", &sidecar)]);
    let project_name = workspace.path().file_name().unwrap().to_string_lossy();
    let completed_name = format!("{project_name}.completed.actions");
    let charters = workspace.path().join(".clearhead/charters");
    fs::write(
        charters.join(completed_name),
        format!("[x] Completed root action #{completed_id}\n"),
    )
    .unwrap();
    fs::write(
        charters.join("next.completed.actions"),
        format!("[x] Legacy completed root action #{legacy_id}\n"),
    )
    .unwrap();

    let diagnosis =
        clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None).unwrap();
    assert!(
        !diagnosis
            .findings
            .iter()
            .any(|finding| finding.code == "sidecar-orphan"),
        "findings: {:?}",
        diagnosis.findings
    );
}

#[test]
fn doctor_flags_implausible_created_timestamp() {
    let sane = "01951111-0000-7000-0000-000000000030";
    // A v4 id whose bits were decoded as a v7 timestamp: a year-8723 date.
    let corrupt = "01951111-0000-7000-0000-000000000031";
    let sidecar = format!(
        r#"{{"acts": {{"{sane}": {{"created": "2026-01-01T00:00:00+00:00"}}, "{corrupt}": {{"created": "8723-01-03T06:19:31+00:00"}}}}}}"#
    );
    let workspace = make_workspace(&[
        (
            "work.actions",
            &format!("[ ] Sane #{sane}\n[ ] Corrupt #{corrupt}\n"),
        ),
        (".work.json", &sidecar),
    ]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    let bad: Vec<_> = diagnosis
        .findings
        .iter()
        .filter(|f| f.code == "implausible-created")
        .collect();
    assert_eq!(bad.len(), 1, "findings: {:?}", diagnosis.findings);
    assert!(bad[0].message.contains(corrupt));
}

#[test]
fn doctor_reports_pending_journal_without_replaying_it() {
    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Old #01951111-0000-7000-0000-000000000017\n",
    )]);
    let charter_root = workspace.path().join(".clearhead").join("charters");
    let tmp = charter_root.join(".tmp.staged");
    fs::write(&tmp, "[ ] New #01951111-0000-7000-0000-000000000017\n").expect("write tmp");
    fs::write(
        charter_root.join(".pending"),
        format!(
            "{}\t{}\n",
            tmp.display(),
            charter_root.join("work.actions").display()
        ),
    )
    .expect("write journal");

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    assert!(
        charter_root.join(".pending").exists(),
        "doctor must not replay the journal"
    );
    assert!(
        diagnosis
            .findings
            .iter()
            .any(|f| f.code == "pending-journal")
    );
    assert!(diagnosis.findings.iter().any(|f| f.code == "orphaned-temp"));
}

#[test]
fn doctor_flags_charter_alias_collision() {
    let workspace = make_workspace(&[
        (
            "one.actions",
            "[ ] A #01951111-0000-7000-0000-000000000018\n",
        ),
        ("one.md", "---\nalias: shared\n---\n# One\n"),
        (
            "two.actions",
            "[ ] B #01951111-0000-7000-0000-000000000019\n",
        ),
        ("two.md", "---\nalias: shared\n---\n# Two\n"),
    ]);

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(workspace.path()), None)
        .expect("diagnose failed");
    let finding = diagnosis
        .findings
        .iter()
        .find(|f| f.code == "alias-collision")
        .expect("alias collision should be a finding");
    assert!(finding.message.contains("shared"));
}

#[test]
fn doctor_flags_open_actions_under_archived_parent_charter() {
    let (_outer, project) = make_named_project(
        "workspace",
        &[(
            "work/ops.actions",
            "[ ] still open #01951111-0000-7000-0000-000000000020\n",
        )],
    );

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(initialized(&project), None)
        .expect("diagnose failed");
    let finding = diagnosis
        .findings
        .iter()
        .find(|f| f.code == "archived-parent-open-actions")
        .expect("open child work under an unresolved parent should be flagged");
    assert_eq!(finding.path, std::path::PathBuf::from("work/ops.actions"));
    assert!(finding.message.contains("parent 'work' is not loaded"));
    assert!(finding.message.contains("1 open action(s)"));
}

#[test]
fn doctor_repairs_external_plan_collections_in_their_own_mount() {
    use clearhead_core::workspace::DoctorRepair;
    use clearhead_core::workspace::resource::MountId;

    let workspace = make_workspace(&[(
        "next.actions",
        "[ ] Root #01951111-0000-7000-0000-000000000021\n",
    )]);
    let external = tempfile::tempdir().unwrap();
    fs::create_dir(external.path().join("surprise")).unwrap();

    let diagnosis = clearhead_workspace_fs::diagnose_workspace(
        initialized(workspace.path()),
        Some(external.path()),
    )
    .unwrap();
    assert!(diagnosis.repairs.iter().any(|repair| matches!(
        repair,
        DoctorRepair::RemovePlansCollection { location, .. }
            if location.mount == MountId::ExternalPlans && location.path.as_str() == "surprise"
    )));

    clearhead_workspace_fs::apply_doctor_repairs(
        workspace.path(),
        Some(external.path()),
        &diagnosis.repairs,
    )
    .unwrap();
    assert!(!external.path().join("surprise").exists());
}

#[test]
fn doctor_rejects_a_sidecar_repair_when_ownership_changed_after_diagnosis() {
    let id = "01951111-0000-7000-0000-000000000022";
    let sidecar = format!(r#"{{"actions":{{"{id}":{{}}}}}}"#);
    let workspace = make_workspace(&[("work.actions", ""), (".work.json", &sidecar)]);
    let root = initialized(workspace.path());
    let diagnosis = clearhead_workspace_fs::diagnose_workspace(root, None).unwrap();
    assert!(!diagnosis.repairs.is_empty());

    fs::write(
        root.join(".clearhead/charters/work.actions"),
        format!("[ ] Restored owner #{id}\n"),
    )
    .unwrap();
    let error =
        clearhead_workspace_fs::apply_doctor_repairs(root, None, &diagnosis.repairs).unwrap_err();
    assert!(error.to_string().contains("stale"));
    assert!(root.join(".clearhead/charters/.work.json").exists());
}

#[test]
fn doctor_rejects_an_external_collection_repair_when_contents_changed() {
    let workspace = make_workspace(&[(
        "next.actions",
        "[ ] Root #01951111-0000-7000-0000-000000000023\n",
    )]);
    let external = tempfile::tempdir().unwrap();
    let collection = external.path().join("surprise");
    fs::create_dir(&collection).unwrap();
    let root = initialized(workspace.path());
    let diagnosis =
        clearhead_workspace_fs::diagnose_workspace(root, Some(external.path())).unwrap();

    fs::write(
        collection.join("new.ics"),
        "BEGIN:VCALENDAR\nEND:VCALENDAR\n",
    )
    .unwrap();
    let error = clearhead_workspace_fs::apply_doctor_repairs(
        root,
        Some(external.path()),
        &diagnosis.repairs,
    )
    .unwrap_err();
    assert!(error.to_string().contains("stale"));
    assert!(collection.join("new.ics").exists());
}
