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
//! The native adapter supplies already-loaded resource state and executes the
//! [`PreparedMutation`] returned by [`prepare_transaction`].

use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::domain::update::{ActionUpdate, apply_updates, disallowed_terminal_update};
use crate::domain::{close_subtree, collect_subtree_ids};
use crate::verb_result::{VerbError, VerbOutcome, canonical_id};
use crate::workspace::actions::format::require_actions_formatting;
use crate::workspace::actions::{Action, ActionState};
use crate::workspace::resource::{
    Effect, EffectBatch, ExpectedResource, PreparedMutation, ResourceLocation,
    ResourcePrecondition, WorkspacePath,
};

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
    pub fn target(&self) -> Uuid {
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
    pub source_path: WorkspacePath,
    pub completed_path: WorkspacePath,
    pub active: Vec<Action>,
    pub completed: Vec<Action>,
    pub active_expected: ExpectedResource,
    pub completed_expected: ExpectedResource,
    pub active_dirty: bool,
    pub completed_dirty: bool,
}

impl FileState {
    pub fn new(
        source_path: WorkspacePath,
        completed_path: WorkspacePath,
        active: Vec<Action>,
        completed: Vec<Action>,
        active_expected: ExpectedResource,
        completed_expected: ExpectedResource,
    ) -> Self {
        Self {
            source_path,
            completed_path,
            active,
            completed,
            active_expected,
            completed_expected,
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
// Pure preparation
// ============================================================================

/// Serialized transaction result populated by a delivery adapter.
///
/// File locations are host presentation strings rather than Core paths.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum TransactionOutcome {
    Committed {
        operations: Vec<VerbOutcome>,
        files: Vec<String>,
    },
    DryRun {
        operations: Vec<VerbOutcome>,
        files: Vec<String>,
    },
    Rejected {
        operation: usize,
        error: VerbError,
    },
}

/// Host-neutral result of preparing an ordered transaction.
#[derive(Debug, Clone)]
pub enum PreparedTransactionOutcome {
    Committed {
        operations: Vec<VerbOutcome>,
        files: Vec<WorkspacePath>,
    },
    DryRun {
        operations: Vec<VerbOutcome>,
        files: Vec<WorkspacePath>,
    },
    Rejected {
        operation: usize,
        error: VerbError,
    },
}

/// Fold a normalized transaction over already-loaded state and render its
/// speculative resource effects. No filesystem or ambient host path is used.
pub fn prepare_transaction(
    mut model: TransactionModel,
    operations: &[NormalizedOperation],
    now: DateTime<Local>,
    dry_run: bool,
) -> Result<PreparedMutation<TransactionModel, PreparedTransactionOutcome>, TransactionError> {
    require_actions_formatting().map_err(TransactionError::Request)?;

    let prior_model = model.clone();
    let outcomes = match apply_operations(&mut model, operations, now) {
        Ok(outcomes) => outcomes,
        Err((operation, error)) => {
            let batch =
                EffectBatch::new(Vec::new(), Vec::new()).expect("an empty effect batch is valid");
            return Ok(PreparedMutation::with_outcome(
                prior_model,
                batch,
                PreparedTransactionOutcome::Rejected { operation, error },
            ));
        }
    };

    let files: Vec<WorkspacePath> = model
        .files
        .iter()
        .flat_map(|file| {
            let mut paths = Vec::new();
            if file.active_dirty {
                paths.push(file.source_path.clone());
            }
            if file.completed_dirty {
                paths.push(file.completed_path.clone());
            }
            paths
        })
        .collect();

    if dry_run {
        let batch =
            EffectBatch::new(Vec::new(), Vec::new()).expect("an empty effect batch is valid");
        return Ok(PreparedMutation::with_outcome(
            model,
            batch,
            PreparedTransactionOutcome::DryRun {
                operations: outcomes,
                files,
            },
        ));
    }

    let mut effects = Vec::new();
    let mut preconditions = Vec::new();
    for file in &model.files {
        if file.active_dirty {
            effects.push(Effect::Write {
                path: ResourceLocation::workspace(file.source_path.clone()),
                bytes: render_actions(&file.active)?.into_bytes(),
            });
        }
        if file.completed_dirty {
            effects.push(Effect::Write {
                path: ResourceLocation::workspace(file.completed_path.clone()),
                bytes: render_actions(&file.completed)?.into_bytes(),
            });
        }

        // Protect the complete trusted read set, including a companion file
        // whose bytes influenced resolution but did not itself become dirty.
        preconditions.push(ResourcePrecondition {
            path: ResourceLocation::workspace(file.source_path.clone()),
            expected: file.active_expected.clone(),
        });
        preconditions.push(ResourcePrecondition {
            path: ResourceLocation::workspace(file.completed_path.clone()),
            expected: file.completed_expected.clone(),
        });
    }
    let batch = EffectBatch::new(effects, preconditions)
        .map_err(|error| TransactionError::Request(error.to_string()))?;
    Ok(PreparedMutation::with_outcome(
        model,
        batch,
        PreparedTransactionOutcome::Committed {
            operations: outcomes,
            files,
        },
    ))
}

fn render_actions(actions: &[Action]) -> Result<String, TransactionError> {
    crate::workspace::actions::format(
        &actions.to_vec(),
        crate::workspace::actions::OutputFormat::Actions,
        None,
        None,
    )
    .map_err(TransactionError::Request)
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
            WorkspacePath::new("charters/work.actions").unwrap(),
            WorkspacePath::new("charters/work.completed.actions").unwrap(),
            active,
            completed,
            ExpectedResource::Missing,
            ExpectedResource::Missing,
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

    #[cfg(feature = "formatting")]
    #[test]
    fn preparation_emits_logical_writes_with_snapshot_preconditions() {
        let target = action("Target", ActionState::NotStarted, None);
        let active_revision = crate::workspace::resource::ResourceRevision::new("active-r1");
        let model = TransactionModel::new(vec![FileState::new(
            WorkspacePath::new("charters/work.actions").unwrap(),
            WorkspacePath::new("charters/work.completed.actions").unwrap(),
            vec![target.clone()],
            vec![],
            ExpectedResource::Revision(active_revision.clone()),
            ExpectedResource::Missing,
        )]);
        let operations = vec![NormalizedOperation::Complete { target: target.id }];

        let prepared = prepare_transaction(model, &operations, Local::now(), false).unwrap();

        assert_eq!(prepared.effects().effects().len(), 2);
        assert_eq!(prepared.effects().preconditions().len(), 2);
        assert_eq!(
            prepared.effects().preconditions()[0].expected,
            ExpectedResource::Revision(active_revision)
        );
        assert!(matches!(
            prepared.outcome(),
            PreparedTransactionOutcome::Committed { files, .. } if files.len() == 2
        ));
    }
}
