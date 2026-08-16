//! Ordered action transactions: parse, normalize, and fold in memory.
//!
//! A transaction is a small ordered batch of `update` / `complete` / `cancel`
//! operations that must validate and commit **together**. This module owns the
//! *pure* half:
//!
//! - the wire request shapes ([`TransactionRequest`], [`Operation`]) matching
//!   the published `transaction_request` schema;
//! - normalization of canonical `urn:uuid:` targets and the request-level guards
//!   that need no trusted state (non-empty batch, no terminal `update` state);
//! - the in-memory fold ([`apply_operations`]) that threads the operations
//!   through a per-file active/completed model in order, so a `complete` and a
//!   later `update` of the same action see each other.
//!
//! Everything above [`transact`] is pure — no filesystem. [`transact`] is the
//! one I/O entry point: it loads the touched files under the workspace lock,
//! drives the fold, and stages the result through the shared journaled commit
//! seam, so the whole batch lands atomically or not at all.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use chrono::{DateTime, Local};
use serde::Deserialize;
use uuid::Uuid;

use crate::domain::update::{ActionUpdate, apply_updates, disallowed_terminal_update};
use crate::domain::{close_subtree, collect_subtree_ids};
use crate::verb_result::{VerbError, VerbOutcome, canonical_id};
use crate::workspace::action_files::{completed_actions_path, read_actions};
use crate::workspace::actions::format::require_actions_formatting;
use crate::workspace::actions::{Action, ActionState};
use crate::workspace::mutation::{WriteSet, render, with_locked_mutation};
use crate::workspace::store::{WorkspaceError, list_action_files, resolve_workspace_layout};

// ============================================================================
// Wire request shapes (transaction_request schema)
// ============================================================================

/// A batch of operations that validate and commit together.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TransactionRequest {
    pub operations: Vec<Operation>,
}

/// One semantic operation, tagged by `op` exactly as the schema publishes it.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "op", rename_all = "kebab-case", deny_unknown_fields)]
pub enum Operation {
    UpdateAction {
        target: String,
        set: ActionUpdateSet,
    },
    CompleteAction {
        target: String,
    },
    CancelAction {
        target: String,
    },
}

/// The field-edit surface of `update-action`, mirroring core's `ActionUpdate`
/// applier. Dependency-graph edits (predecessors, is_sequential) are out of the
/// v1 operation set. `state` deserializes as core `ActionState` does (snake_case).
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ActionUpdateSet {
    pub name: Option<String>,
    pub description: Option<String>,
    pub priority: Option<u32>,
    pub context: Option<Vec<String>>,
    pub alias: Option<String>,
    pub state: Option<ActionState>,
    pub scheduled_at: Option<String>,
    pub duration: Option<u32>,
}

impl ActionUpdateSet {
    fn is_empty(&self) -> bool {
        self.name.is_none()
            && self.description.is_none()
            && self.priority.is_none()
            && self.context.is_none()
            && self.alias.is_none()
            && self.state.is_none()
            && self.scheduled_at.is_none()
            && self.duration.is_none()
    }

    /// Convert to a core [`ActionUpdate`], parsing the RFC 3339 `scheduled_at`.
    fn into_action_update(self) -> Result<ActionUpdate, TransactionError> {
        let scheduled_at = self
            .scheduled_at
            .as_deref()
            .map(|raw| {
                DateTime::parse_from_rfc3339(raw)
                    .map(|dt| dt.with_timezone(&Local))
                    .map_err(|e| {
                        TransactionError::Request(format!("invalid scheduled_at '{raw}': {e}"))
                    })
            })
            .transpose()?;

        Ok(ActionUpdate {
            name: self.name,
            description: self.description,
            priority: self.priority,
            context: self.context,
            predecessors: None,
            is_sequential: None,
            alias: self.alias,
            state: self.state,
            scheduled_at,
            duration: self.duration,
        })
    }
}

// ============================================================================
// Normalization: wire request -> validated, resolvable operations
// ============================================================================

/// A request that could not be turned into a valid batch — independent of any
/// trusted workspace state. Surfaced as a hard error before the lock is taken,
/// never as a per-operation `rejected` result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TransactionError {
    Request(String),
}

impl std::fmt::Display for TransactionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransactionError::Request(message) => {
                write!(f, "invalid transaction request: {message}")
            }
        }
    }
}

impl std::error::Error for TransactionError {}

/// A normalized operation: canonical target resolved to a UUID and, for updates,
/// the field edit validated and converted to a core [`ActionUpdate`].
#[derive(Debug, Clone)]
pub enum NormalizedOperation {
    Update { target: Uuid, update: ActionUpdate },
    Complete { target: Uuid },
    Cancel { target: Uuid },
}

impl NormalizedOperation {
    fn target(&self) -> Uuid {
        match self {
            NormalizedOperation::Update { target, .. }
            | NormalizedOperation::Complete { target }
            | NormalizedOperation::Cancel { target } => *target,
        }
    }
}

/// Parse a canonical `urn:uuid:` target into a UUID.
fn normalize_target(raw: &str) -> Result<Uuid, TransactionError> {
    let bare = raw.strip_prefix("urn:uuid:").unwrap_or(raw);
    Uuid::parse_str(bare)
        .map_err(|e| TransactionError::Request(format!("invalid canonical target '{raw}': {e}")))
}

/// Validate and normalize a whole request into resolvable operations.
///
/// Applies only the state-free guards: a non-empty batch, a well-formed target
/// per operation, a non-empty `set` with no terminal `state` (terminal
/// transitions belong to `complete`/`cancel`, which cascade and move files).
pub fn normalize_request(
    request: TransactionRequest,
) -> Result<Vec<NormalizedOperation>, TransactionError> {
    if request.operations.is_empty() {
        return Err(TransactionError::Request(
            "a transaction must contain at least one operation".to_string(),
        ));
    }

    request
        .operations
        .into_iter()
        .map(|op| match op {
            Operation::UpdateAction { target, set } => {
                if set.is_empty() {
                    return Err(TransactionError::Request(format!(
                        "update-action for '{target}' changes no fields"
                    )));
                }
                let target = normalize_target(&target)?;
                let update = set.into_action_update()?;
                if let Some(state) = disallowed_terminal_update(&update) {
                    return Err(TransactionError::Request(format!(
                        "update-action cannot set state to {state:?}; use complete-action / \
                         cancel-action, which cascade to the subtree and archive it"
                    )));
                }
                Ok(NormalizedOperation::Update { target, update })
            }
            Operation::CompleteAction { target } => Ok(NormalizedOperation::Complete {
                target: normalize_target(&target)?,
            }),
            Operation::CancelAction { target } => Ok(NormalizedOperation::Cancel {
                target: normalize_target(&target)?,
            }),
        })
        .collect()
}

// ============================================================================
// In-memory model and the ordered fold
// ============================================================================

/// One workspace file's trusted state within a transaction: its active
/// `.actions` list, its `.completed.actions` list, and whether each has changed.
#[derive(Debug, Clone)]
pub struct FileState {
    pub source_path: PathBuf,
    pub active: Vec<Action>,
    pub completed: Vec<Action>,
    pub active_dirty: bool,
    pub completed_dirty: bool,
}

impl FileState {
    pub fn new(source_path: PathBuf, active: Vec<Action>, completed: Vec<Action>) -> Self {
        Self {
            source_path,
            active,
            completed,
            active_dirty: false,
            completed_dirty: false,
        }
    }
}

/// The set of files a transaction touches, loaded under the lock and folded
/// through in operation order.
#[derive(Debug, Clone, Default)]
pub struct TransactionModel {
    pub files: Vec<FileState>,
}

impl TransactionModel {
    pub fn new(files: Vec<FileState>) -> Self {
        Self { files }
    }
}

/// Apply operations in order to the in-memory model.
///
/// Returns one [`VerbOutcome`] per operation on success. On the first operation
/// that cannot apply against the current (already-folded) state, returns its
/// 0-based index and typed [`VerbError`] and leaves the caller to discard the
/// model — nothing is committed. Pure: no filesystem access.
pub fn apply_operations(
    model: &mut TransactionModel,
    operations: &[NormalizedOperation],
    now: DateTime<Local>,
) -> Result<Vec<VerbOutcome>, (usize, VerbError)> {
    let mut outcomes = Vec::with_capacity(operations.len());
    for (index, operation) in operations.iter().enumerate() {
        match apply_one(model, operation, now) {
            Ok(outcome) => outcomes.push(outcome),
            Err(error) => return Err((index, error)),
        }
    }
    Ok(outcomes)
}

fn apply_one(
    model: &mut TransactionModel,
    operation: &NormalizedOperation,
    now: DateTime<Local>,
) -> Result<VerbOutcome, VerbError> {
    let target = operation.target();

    // The target is authoritative in exactly one active list, if anywhere.
    if let Some(file) = model
        .files
        .iter_mut()
        .find(|file| file.active.iter().any(|action| action.id == target))
    {
        return match operation {
            NormalizedOperation::Update { update, .. } => {
                let action = file
                    .active
                    .iter_mut()
                    .find(|action| action.id == target)
                    .expect("target located in this active list");
                apply_updates(action, update.clone());
                file.active_dirty = true;
                Ok(VerbOutcome::Updated {
                    id: canonical_id(target),
                })
            }
            NormalizedOperation::Complete { .. } => {
                close_in_file(file, target, ActionState::Completed, now)
            }
            NormalizedOperation::Cancel { .. } => {
                close_in_file(file, target, ActionState::Cancelled, now)
            }
        };
    }

    // Not open anywhere: distinguish already-closed from never-existed so an
    // idempotent caller can branch on "effectively done".
    if let Some(action) = model
        .files
        .iter()
        .flat_map(|file| file.completed.iter())
        .find(|action| action.id == target)
    {
        return Err(VerbError::AlreadyClosed {
            id: canonical_id(target),
            state: format!("{:?}", action.state),
            query: canonical_id(target),
        });
    }

    Err(VerbError::NotFound {
        query: canonical_id(target),
    })
}

/// Close `target`'s subtree in one file: cascade-close, move it from active to
/// completed, and stamp. Mirrors `close_action_subtree`'s in-memory core.
fn close_in_file(
    file: &mut FileState,
    target: Uuid,
    closing_state: ActionState,
    now: DateTime<Local>,
) -> Result<VerbOutcome, VerbError> {
    let subtree_ids = collect_subtree_ids(&file.active, target);
    let mut closed = close_subtree(&file.active, target, closing_state, now);
    file.active
        .retain(|action| !subtree_ids.contains(&action.id));
    let closed_count = closed.len();
    file.completed.append(&mut closed);
    file.active_dirty = true;
    file.completed_dirty = true;

    let id = canonical_id(target);
    let children = closed_count.saturating_sub(1);
    Ok(match closing_state {
        ActionState::Cancelled => VerbOutcome::Cancelled { id, children },
        _ => VerbOutcome::Completed { id, children },
    })
}

// ============================================================================
// Locked plan + journaled commit
// ============================================================================

/// The result of running a transaction, mirroring the `transaction_result`
/// schema's three shapes.
#[derive(Debug, Clone)]
pub enum TransactionOutcome {
    /// Every operation applied and the changed files were committed.
    Committed {
        operations: Vec<VerbOutcome>,
        files: Vec<PathBuf>,
    },
    /// Every operation would apply; `--dry-run` stopped before staging.
    DryRun {
        operations: Vec<VerbOutcome>,
        files: Vec<PathBuf>,
    },
    /// One operation could not apply against trusted state; nothing was written.
    Rejected { operation: usize, error: VerbError },
}

/// Execute an ordered transaction atomically.
///
/// Request-level problems (empty batch, malformed target, terminal `update`
/// state) fail fast as an `Err` before the lock is taken. Under the lock, only
/// the files holding a target are loaded, the operations are folded in order,
/// and — on success — every changed active/completed file is staged in one
/// journaled batch. A per-operation rejection or a `--dry-run` stages nothing,
/// so either commits no bytes.
pub fn transact(
    workspace_root: &Path,
    request: TransactionRequest,
    dry_run: bool,
) -> Result<TransactionOutcome, WorkspaceError> {
    let operations =
        normalize_request(request).map_err(|e| WorkspaceError::Actions(e.to_string()))?;
    require_actions_formatting().map_err(WorkspaceError::Actions)?;

    let layout = resolve_workspace_layout(workspace_root);
    let target_ids: HashSet<Uuid> = operations.iter().map(NormalizedOperation::target).collect();

    with_locked_mutation(&layout, |_layout| {
        let mut model = TransactionModel::new(load_target_files(workspace_root, &target_ids)?);

        let outcomes = match apply_operations(&mut model, &operations, Local::now()) {
            Ok(outcomes) => outcomes,
            Err((operation, error)) => {
                return Ok((
                    WriteSet::new(),
                    TransactionOutcome::Rejected { operation, error },
                ));
            }
        };

        let dirty: Vec<usize> = model
            .files
            .iter()
            .enumerate()
            .filter(|(_, file)| file.active_dirty || file.completed_dirty)
            .map(|(index, _)| index)
            .collect();
        let files: Vec<PathBuf> = dirty
            .iter()
            .map(|&index| model.files[index].source_path.clone())
            .collect();

        if dry_run {
            return Ok((
                WriteSet::new(),
                TransactionOutcome::DryRun {
                    operations: outcomes,
                    files,
                },
            ));
        }

        let mut writes = WriteSet::new();
        for &index in &dirty {
            let file = &model.files[index];
            if file.active_dirty {
                writes.stage(file.source_path.clone(), render(&file.active)?);
            }
            if file.completed_dirty {
                writes.stage(
                    completed_actions_path(&file.source_path),
                    render(&file.completed)?,
                );
            }
        }

        Ok((
            writes,
            TransactionOutcome::Committed {
                operations: outcomes,
                files,
            },
        ))
    })
}

/// Read every workspace action file that holds at least one target (active or
/// completed) into a [`FileState`]. Files no target touches are skipped.
fn load_target_files(
    workspace_root: &Path,
    target_ids: &HashSet<Uuid>,
) -> Result<Vec<FileState>, WorkspaceError> {
    let mut files = Vec::new();
    for active_path in list_action_files(workspace_root)? {
        let active = read_actions(&active_path)?;
        let completed_path = completed_actions_path(&active_path);
        let completed = read_actions(&completed_path)?;
        let touched = active
            .iter()
            .chain(completed.iter())
            .any(|action| target_ids.contains(&action.id));
        if touched {
            files.push(FileState::new(active_path, active, completed));
        }
    }
    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn action(name: &str, state: ActionState, parent: Option<Uuid>) -> Action {
        Action {
            id: Uuid::now_v7(),
            name: name.to_string(),
            state,
            parent_id: parent,
            ..Default::default()
        }
    }

    fn model_of(active: Vec<Action>, completed: Vec<Action>) -> TransactionModel {
        TransactionModel::new(vec![FileState::new(
            PathBuf::from("work.actions"),
            active,
            completed,
        )])
    }

    #[test]
    fn parses_and_normalizes_a_mixed_batch() {
        let json = r#"{
            "operations": [
                {"op": "update-action", "target": "urn:uuid:019f733d-4600-7000-8000-000000000001", "set": {"priority": 2}},
                {"op": "complete-action", "target": "urn:uuid:019f733d-4600-7000-8000-000000000002"}
            ]
        }"#;
        let request: TransactionRequest = serde_json::from_str(json).unwrap();
        let ops = normalize_request(request).unwrap();
        assert_eq!(ops.len(), 2);
        assert!(matches!(ops[0], NormalizedOperation::Update { .. }));
        assert!(matches!(ops[1], NormalizedOperation::Complete { .. }));
    }

    #[test]
    fn rejects_a_terminal_update_state_as_a_request_error() {
        let json = r#"{"operations": [
            {"op": "update-action", "target": "urn:uuid:019f733d-4600-7000-8000-000000000001", "set": {"state": "completed"}}
        ]}"#;
        let request: TransactionRequest = serde_json::from_str(json).unwrap();
        let error = normalize_request(request).unwrap_err();
        assert!(matches!(error, TransactionError::Request(_)));
    }

    #[test]
    fn rejects_an_empty_set_and_an_empty_batch() {
        let empty_set = TransactionRequest {
            operations: vec![Operation::UpdateAction {
                target: "urn:uuid:019f733d-4600-7000-8000-000000000001".into(),
                set: ActionUpdateSet::default(),
            }],
        };
        assert!(normalize_request(empty_set).is_err());

        let empty_batch = TransactionRequest { operations: vec![] };
        assert!(normalize_request(empty_batch).is_err());
    }

    #[test]
    fn update_then_complete_of_the_same_action_folds_in_order() {
        let mut target = action("Task", ActionState::NotStarted, None);
        target.id = "019f733d-4600-7000-8000-000000000001".parse().unwrap();
        let mut model = model_of(vec![target.clone()], vec![]);

        let ops = vec![
            NormalizedOperation::Update {
                target: target.id,
                update: ActionUpdate {
                    name: Some("Renamed".into()),
                    ..Default::default()
                },
            },
            NormalizedOperation::Complete { target: target.id },
        ];

        let outcomes = apply_operations(&mut model, &ops, Local::now()).unwrap();
        assert_eq!(outcomes.len(), 2);
        assert!(matches!(outcomes[0], VerbOutcome::Updated { .. }));
        assert!(matches!(outcomes[1], VerbOutcome::Completed { .. }));

        // The rename applied, then the renamed action moved active -> completed.
        let file = &model.files[0];
        assert!(file.active.is_empty());
        assert_eq!(file.completed.len(), 1);
        assert_eq!(file.completed[0].name, "Renamed");
        assert!(file.active_dirty && file.completed_dirty);
    }

    #[test]
    fn completing_a_parent_cascades_to_children() {
        let parent = action("Parent", ActionState::NotStarted, None);
        let child = action("Child", ActionState::NotStarted, Some(parent.id));
        let mut model = model_of(vec![parent.clone(), child], vec![]);

        let ops = vec![NormalizedOperation::Complete { target: parent.id }];
        let outcomes = apply_operations(&mut model, &ops, Local::now()).unwrap();

        match &outcomes[0] {
            VerbOutcome::Completed { children, .. } => assert_eq!(*children, 1),
            other => panic!("expected completed, got {other:?}"),
        }
        assert!(model.files[0].active.is_empty());
        assert_eq!(model.files[0].completed.len(), 2);
    }

    #[test]
    fn a_missing_target_rejects_with_its_index() {
        let present = action("Here", ActionState::NotStarted, None);
        let mut model = model_of(vec![present.clone()], vec![]);
        let missing: Uuid = "019f733d-4600-7000-8000-0000000000ff".parse().unwrap();

        let ops = vec![
            NormalizedOperation::Update {
                target: present.id,
                update: ActionUpdate {
                    priority: Some(1),
                    ..Default::default()
                },
            },
            NormalizedOperation::Complete { target: missing },
        ];

        let (index, error) = apply_operations(&mut model, &ops, Local::now()).unwrap_err();
        assert_eq!(index, 1, "the second operation is the one that fails");
        assert!(matches!(error, VerbError::NotFound { .. }));
    }

    #[test]
    fn a_target_already_in_completed_reports_already_closed() {
        let done = action("Done", ActionState::Completed, None);
        let mut model = model_of(vec![], vec![done.clone()]);

        let ops = vec![NormalizedOperation::Complete { target: done.id }];
        let (index, error) = apply_operations(&mut model, &ops, Local::now()).unwrap_err();
        assert_eq!(index, 0);
        assert!(matches!(error, VerbError::AlreadyClosed { .. }));
    }

    // ── locked plan + commit (end to end) ───────────────────────────────────

    const A: &str = "019f733d-4600-7000-8000-000000000001";
    const B: &str = "019f733d-4600-7000-8000-000000000002";

    #[cfg(feature = "formatting")]
    fn workspace_with(source_body: &str) -> (tempfile::TempDir, PathBuf) {
        let temp = tempfile::tempdir().unwrap();
        let charters = temp.path().join("charters");
        std::fs::create_dir_all(&charters).unwrap();
        let source = charters.join("work.actions");
        std::fs::write(&source, source_body).unwrap();
        (temp, source)
    }

    #[cfg(feature = "formatting")]
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

        let outcome = transact(temp.path(), request, false).unwrap();
        match outcome {
            TransactionOutcome::Committed { operations, files } => {
                assert_eq!(operations.len(), 2);
                assert_eq!(files.len(), 1);
            }
            other => panic!("expected committed, got {other:?}"),
        }

        let active = read_actions(&source).unwrap();
        assert_eq!(active.len(), 1, "Beta moved to completed");
        assert_eq!(active[0].name, "Alpha");
        assert_eq!(active[0].priority, Some(1), "Alpha update applied");
        let completed = read_actions(&completed_actions_path(&source)).unwrap();
        assert_eq!(completed.len(), 1);
        assert_eq!(completed[0].name, "Beta");
        assert!(!temp.path().join("charters/.pending").exists());
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn transact_rejects_the_whole_batch_and_writes_nothing() {
        let (temp, source) = workspace_with(&format!("[ ] Alpha #{A}\n"));
        let missing = "019f733d-4600-7000-8000-0000000000ff";

        // op0 would update Alpha; op1 targets a missing action. The batch rejects
        // and op0's edit must not reach disk.
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

        let active = read_actions(&source).unwrap();
        assert_eq!(
            active[0].priority, None,
            "a rejected batch leaves op0's edit uncommitted"
        );
    }

    #[cfg(feature = "formatting")]
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
                assert_eq!(operations.len(), 2, "the fold ran, validating every op");
                assert_eq!(files.len(), 1, "one file would change");
            }
            other => panic!("expected dry-run, got {other:?}"),
        }

        assert_eq!(
            read_actions(&source).unwrap().len(),
            2,
            "dry-run wrote nothing"
        );
    }

    #[cfg(feature = "formatting")]
    #[test]
    fn transact_recovers_an_interrupted_commit_before_folding() {
        let (temp, source) = workspace_with(&format!("[ ] Alpha #{A}\n"));
        let charters = temp.path().join("charters");

        // A crashed prior write that renamed Alpha -> Gamma, staged but not
        // renamed in. transact's recover_pending must complete it before it reads.
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
        assert_eq!(active.len(), 1);
        assert_eq!(active[0].name, "Gamma", "recovery applied the rename");
        assert_eq!(active[0].priority, Some(1), "then the transaction applied");
        assert!(!charters.join(".pending").exists());
    }
}
