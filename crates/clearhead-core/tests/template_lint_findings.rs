use clearhead_core::workspace::{
    MountInventory, MountReadEvidence, ResourceRevision, ResourceSnapshot, WorkspaceAssemblyInput,
    WorkspaceInventory, WorkspaceMounts, WorkspacePath, WorkspaceSnapshot, assemble_workspace,
};
use std::path::PathBuf;
use uuid::Uuid;

#[test]
fn template_lint_findings_keep_distinct_source_ranges_in_assembly() {
    let path = WorkspacePath::new("templates/named.actions").unwrap();
    let revision = ResourceRevision::new("template-revision");
    let snapshot = ResourceSnapshot::new(
        path.clone(),
        b"[ ] First step\n\n[ ] Second step\n".to_vec(),
        revision.clone(),
    );
    let input = WorkspaceAssemblyInput {
        root_charter: "workspace".into(),
        inventory: WorkspaceMounts {
            workspace: MountInventory {
                files: WorkspaceInventory::new([(path.clone(), revision)]),
                collections: Default::default(),
            },
            external_plans: None,
        },
        reads: WorkspaceMounts {
            workspace: MountReadEvidence {
                snapshot: WorkspaceSnapshot::new([snapshot]).unwrap(),
                failures: vec![],
            },
            external_plans: None,
        },
        charter_ids: std::collections::HashMap::<PathBuf, Uuid>::new(),
    };
    let read = assemble_workspace(&input).unwrap();
    let findings: Vec<_> = read
        .findings
        .iter()
        .filter(|finding| finding.code == "habit-id-missing")
        .collect();
    assert_eq!(findings.len(), 2);

    let serialized: Vec<_> = findings
        .iter()
        .map(|finding| serde_json::to_value(finding).unwrap())
        .collect();
    assert_eq!(serialized[0]["range"]["start_row"], 0);
    assert_eq!(serialized[1]["range"]["start_row"], 2);
}
