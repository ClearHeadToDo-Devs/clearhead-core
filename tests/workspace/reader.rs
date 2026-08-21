use super::common::*;
use clearhead_core::load_domain_model;
use std::fs;
use std::path::Path;

// --- Relaxed reader (Decision 34): read_workspace never mutates, never refuses ---

#[test]
fn corrupt_sidecar_is_a_finding_not_a_load_failure() {
    use clearhead_core::workspace::{FindingSeverity, read_workspace};

    let workspace = make_workspace(&[
        (
            "work.actions",
            "[ ] Task one #01951111-0000-7000-0000-000000000003\n",
        ),
        (".work.json", "{ this is not json"),
    ]);

    let read = read_workspace(workspace.path()).expect("reader must not refuse the workspace");
    let work = read
        .charters
        .iter()
        .find(|c| c.title == "work")
        .expect("work charter");
    assert_eq!(
        work.actions.len(),
        1,
        "actions load even when the sidecar is corrupt"
    );

    let finding = read
        .findings
        .iter()
        .find(|f| f.code == "sidecar-corrupt")
        .expect("corrupt sidecar should be reported as a finding");
    assert_eq!(finding.severity, FindingSeverity::Violation);
    assert_eq!(finding.path, Path::new(".work.json"));

    // The healing load path also survives it (it used to hard-fail here).
    load_domain_model(workspace.path()).expect("load must survive a corrupt sidecar");
}

#[test]
fn syntax_errors_surface_as_a_warning_finding() {
    use clearhead_core::workspace::{FindingSeverity, read_workspace};

    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Valid one #01961111-0000-7000-0000-000000000001\n\
         this line is malformed and should not be parsed\n\
         [ ] Valid two #01961111-0000-7000-0000-000000000002\n",
    )]);

    let read = read_workspace(workspace.path()).expect("read failed");
    let finding = read
        .findings
        .iter()
        .find(|f| f.code == "syntax-errors")
        .expect("recoverable syntax issues should be reported as a finding");
    assert_eq!(finding.severity, FindingSeverity::Warning);
    assert_eq!(finding.path, Path::new("work.actions"));
    assert!(
        finding.message.contains("file quarantined"),
        "message should make the semantic trust boundary explicit: {}",
        finding.message
    );
}

#[test]
fn unparseable_ics_is_a_finding_and_the_rest_still_loads() {
    use clearhead_core::workspace::read_workspace;

    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Task #01951111-0000-7000-0000-000000000004\n",
    )]);
    let plans_dir = workspace
        .path()
        .join(".clearhead")
        .join("plans")
        .join("work");
    fs::create_dir_all(&plans_dir).expect("create plans dir");
    fs::write(plans_dir.join("bad.ics"), "this is not a calendar").expect("write bad ics");

    let read = read_workspace(workspace.path()).expect("reader must not refuse the workspace");
    assert!(
        read.findings.iter().any(|f| f.code == "unparseable-file"),
        "bad ics should be reported, findings were: {:?}",
        read.findings
    );
    let work = read
        .charters
        .iter()
        .find(|c| c.title == "work")
        .expect("work charter");
    assert_eq!(work.actions.len(), 1, "actions are unaffected by a bad ics");
}

#[test]
fn neither_read_nor_load_replays_pending_journal() {
    // Pending-journal recovery is the native adapter's obligation (it runs
    // recover_pending under the workspace lock before handing bytes to Core).
    // Core's readers are pure: they observe the pre-crash bytes as-is and never
    // mutate the workspace, so a `.pending` journal survives an in-Core read.
    let workspace = make_workspace(&[(
        "work.actions",
        "[ ] Old content #01951111-0000-7000-0000-000000000005\n",
    )]);
    let charter_root = workspace.path().join(".clearhead").join("charters");

    // Simulate a crash mid-batch: staged temp + journal, rename never happened.
    let tmp = charter_root.join(".tmp.staged");
    let target = charter_root.join("work.actions");
    fs::write(
        &tmp,
        "[ ] New content #01951111-0000-7000-0000-000000000005\n",
    )
    .expect("write tmp");
    fs::write(
        charter_root.join(".pending"),
        format!("{}\t{}\n", tmp.display(), target.display()),
    )
    .expect("write journal");

    let read = clearhead_core::workspace::read_workspace(workspace.path()).expect("read failed");
    assert!(
        charter_root.join(".pending").exists(),
        "the pure reader must never replay the journal"
    );
    let work = read
        .charters
        .iter()
        .find(|c| c.title == "work")
        .expect("work charter");
    assert_eq!(
        work.actions[0].action.name, "Old content",
        "read sees the pre-crash state as-is"
    );

    let model = load_domain_model(workspace.path()).expect("load failed");
    assert!(
        charter_root.join(".pending").exists(),
        "load is a pure reader too — it must not replay the journal either"
    );
    let work = model
        .charters
        .iter()
        .find(|c| c.title == "work")
        .expect("work charter");
    assert_eq!(
        work.actions[0].name, "Old content",
        "load sees the pre-crash state as-is; recovery is the adapter's job"
    );
}
