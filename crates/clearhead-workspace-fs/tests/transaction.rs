use clearhead_core::{TransactionRequest, VerbError, completed_actions_path, read_actions};
use clearhead_workspace_fs::{TransactionOutcome, transact};

const A: &str = "019f733d-4600-7000-8000-000000000001";
const B: &str = "019f733d-4600-7000-8000-000000000002";

fn workspace_with(source_body: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let temp = tempfile::tempdir().unwrap();
    let charters = temp.path().join("charters");
    std::fs::create_dir_all(&charters).unwrap();
    let source = charters.join("work.actions");
    std::fs::write(&source, source_body).unwrap();
    (temp, source)
}

#[test]
fn transact_commits_a_mixed_batch_atomically() {
    let (temp, source) = workspace_with(&format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));
    let request: TransactionRequest = serde_json::from_str(&format!(
        r#"{{"operations":[
            {{"op":"update-action","target":"urn:uuid:{A}","set":{{"priority":1}}}},
            {{"op":"complete-action","target":"urn:uuid:{B}"}}
        ]}}"#
    ))
    .unwrap();

    match transact(temp.path(), request, false).unwrap() {
        TransactionOutcome::Committed { operations, files } => {
            assert_eq!(operations.len(), 2);
            assert_eq!(files.len(), 2);
        }
        other => panic!("expected committed, got {other:?}"),
    }
    let active = read_actions(&source).unwrap();
    assert_eq!(active.len(), 1);
    assert_eq!(active[0].priority, Some(1));
    assert_eq!(
        read_actions(&completed_actions_path(&source))
            .unwrap()
            .len(),
        1
    );
    assert!(!temp.path().join("charters/.pending").exists());
}

#[test]
fn transact_rejects_the_whole_batch_and_writes_nothing() {
    let (temp, source) = workspace_with(&format!("[ ] Alpha #{A}\n"));
    let missing = "019f733d-4600-7000-8000-0000000000ff";
    let request: TransactionRequest = serde_json::from_str(&format!(
        r#"{{"operations":[
            {{"op":"update-action","target":"urn:uuid:{A}","set":{{"priority":1}}}},
            {{"op":"complete-action","target":"urn:uuid:{missing}"}}
        ]}}"#
    ))
    .unwrap();

    match transact(temp.path(), request, false).unwrap() {
        TransactionOutcome::Rejected { operation, error } => {
            assert_eq!(operation, 1);
            assert!(matches!(error, VerbError::NotFound { .. }));
        }
        other => panic!("expected rejected, got {other:?}"),
    }
    assert_eq!(read_actions(&source).unwrap()[0].priority, None);
}

#[test]
fn transact_dry_run_stages_nothing() {
    let (temp, source) = workspace_with(&format!("[ ] Alpha #{A}\n[ ] Beta #{B}\n"));
    let request: TransactionRequest = serde_json::from_str(&format!(
        r#"{{"operations":[
            {{"op":"update-action","target":"urn:uuid:{A}","set":{{"priority":1}}}},
            {{"op":"complete-action","target":"urn:uuid:{B}"}}
        ]}}"#
    ))
    .unwrap();

    match transact(temp.path(), request, true).unwrap() {
        TransactionOutcome::DryRun { operations, files } => {
            assert_eq!(operations.len(), 2);
            assert_eq!(files.len(), 2);
        }
        other => panic!("expected dry-run, got {other:?}"),
    }
    assert_eq!(read_actions(&source).unwrap().len(), 2);
}

#[test]
fn transact_recovers_an_interrupted_commit_before_folding() {
    let (temp, source) = workspace_with(&format!("[ ] Alpha #{A}\n"));
    let charters = temp.path().join("charters");
    let tmp = charters.join(".tmp.recover");
    std::fs::write(&tmp, format!("[ ] Gamma #{A}\n")).unwrap();
    std::fs::write(
        charters.join(".pending"),
        format!("{}\t{}\n", tmp.display(), source.display()),
    )
    .unwrap();
    let request: TransactionRequest = serde_json::from_str(&format!(
        r#"{{"operations":[
            {{"op":"update-action","target":"urn:uuid:{A}","set":{{"priority":1}}}}
        ]}}"#
    ))
    .unwrap();

    transact(temp.path(), request, false).unwrap();
    let active = read_actions(&source).unwrap();
    assert_eq!(active[0].name, "Gamma");
    assert_eq!(active[0].priority, Some(1));
    assert!(!charters.join(".pending").exists());
}
