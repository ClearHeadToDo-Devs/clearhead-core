use super::common::*;
use std::fs;
use std::path::Path;

// --- Doctor: read-only cross-file fsck (Decision 34) ---

/// Stamp a durable identity onto the workspace at `root` — what `clearhead init`
/// would have written to `workspace.json`. Doctor flags a missing `workspace_id`
/// (read from the manifest), so fixtures testing *other* findings call this to
/// stay out of that check's way. Returns `root` for inline use.
fn initialized(root: &Path) -> &Path {
    clearhead_core::workspace::WorkspaceManifest {
        workspace_id: Some("01951111-0000-7000-0000-00000000c0f9".to_string()),
        workspace_name: Some("test".to_string()),
        created_at: None,
    }
    .write(root)
    .expect("write workspace manifest");
    root
}

#[test]
fn doctor_flags_uninitialized_workspace() {
    use clearhead_core::workspace::diagnose;

    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Task one #01951111-0000-7000-0000-000000000010\n",
    )]);

    let diagnosis = diagnose(workspace.path(), None).expect("diagnose failed");
    let finding = diagnosis
        .findings
        .iter()
        .find(|f| f.code == "uninitialized-workspace")
        .expect("missing workspace_id should be a finding");
    assert!(finding.message.contains("clearhead init"));
}

#[test]
fn doctor_reports_clean_on_a_coherent_workspace() {
    use clearhead_core::workspace::diagnose;

    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Task one #01951111-0000-7000-0000-000000000010\n",
    )]);

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");
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
fn doctor_flags_duplicate_uuids_across_files() {
    use clearhead_core::workspace::diagnose;

    let uuid = "01951111-0000-7000-0000-000000000011";
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] In work #{uuid}\n")),
        (
            "home.actions",
            &format!("[ ] Copy-pasted into home #{uuid}\n"),
        ),
    ]);

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");
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
    use clearhead_core::workspace::diagnose;

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

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");
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
    use clearhead_core::workspace::diagnose;

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

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");

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
    use clearhead_core::workspace::diagnose;

    let live = "01951111-0000-7000-0000-000000000015";
    let gone = "01951111-0000-7000-0000-000000000016";
    let sidecar = format!(
        r#"{{"acts": {{"{live}": {{"created": "2026-01-01T00:00:00+00:00"}}, "{gone}": {{"created": "2026-01-01T00:00:00+00:00"}}}}}}"#
    );
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] Still here #{live}\n")),
        (".work.json", &sidecar),
    ]);

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");
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
    use clearhead_core::workspace::diagnose;

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

    let diagnosis = diagnose(initialized(workspace.path()), None).unwrap();
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
    use clearhead_core::workspace::diagnose;

    let moved = "01951111-0000-7000-0000-000000000019";
    let sidecar =
        format!(r#"{{"actions": {{"{moved}": {{"created": "2026-01-01T00:00:00+00:00"}}}}}}"#);
    let workspace = make_workspace(&[
        ("work.actions", &format!("[ ] Moved here #{moved}\n")),
        (".old-home.json", &sidecar),
    ]);

    let diagnosis = diagnose(initialized(workspace.path()), None).unwrap();
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
    use clearhead_core::workspace::diagnose;

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

    let diagnosis = diagnose(initialized(workspace.path()), None).unwrap();
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
    use clearhead_core::workspace::diagnose;

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

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");
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
    use clearhead_core::workspace::diagnose;

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

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");
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
    use clearhead_core::workspace::diagnose;

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

    let diagnosis = diagnose(initialized(workspace.path()), None).expect("diagnose failed");
    let finding = diagnosis
        .findings
        .iter()
        .find(|f| f.code == "alias-collision")
        .expect("alias collision should be a finding");
    assert!(finding.message.contains("shared"));
}

#[test]
fn doctor_flags_open_actions_under_archived_parent_charter() {
    use clearhead_core::workspace::diagnose;

    let (_outer, project) = make_named_project(
        "workspace",
        &[(
            "work/ops.actions",
            "[ ] still open #01951111-0000-7000-0000-000000000020\n",
        )],
    );

    let diagnosis = diagnose(initialized(&project), None).expect("diagnose failed");
    let finding = diagnosis
        .findings
        .iter()
        .find(|f| f.code == "archived-parent-open-actions")
        .expect("open child work under an unresolved parent should be flagged");
    assert_eq!(finding.path, std::path::PathBuf::from("work/ops.actions"));
    assert!(finding.message.contains("parent 'work' is not loaded"));
    assert!(finding.message.contains("1 open action(s)"));
}
