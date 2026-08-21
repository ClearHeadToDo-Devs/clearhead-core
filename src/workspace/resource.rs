//! Host-neutral workspace resources and prepared mutation contracts.
//!
//! These types are the delivery boundary between Core's decisions and a host's
//! I/O. Paths identify logical workspace resources, snapshots contain bytes the
//! host has already read, and effects describe changes without naming an I/O
//! API. A prepared mutation remains speculative until its complete effect batch
//! has been executed successfully.
//!
//! The native adapter must still hold its workspace lock across recovery,
//! inventory, reads, preparation, precondition validation, and durable commit.
//! Per-resource revisions protect stale resources but cannot by themselves
//! detect inventory phantoms (a concurrently added resource Core never saw).

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use thiserror::Error;

/// A validated, UTF-8, workspace-relative resource path.
///
/// Logical paths always use `/` separators. They are not operating-system paths
/// and deliberately carry no canonicalization, symlink, permission, or root
/// directory semantics.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct WorkspacePath(String);

impl WorkspacePath {
    /// Validate and construct a logical workspace path.
    pub fn new(path: impl Into<String>) -> Result<Self, WorkspacePathError> {
        let path = path.into();
        validate_workspace_path(&path)?;
        Ok(Self(path))
    }

    /// Return the canonical logical representation.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for WorkspacePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl TryFrom<&str> for WorkspacePath {
    type Error = WorkspacePathError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl TryFrom<String> for WorkspacePath {
    type Error = WorkspacePathError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

/// Why a host path cannot identify a Core workspace resource.
#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum WorkspacePathError {
    #[error("workspace paths must not be empty")]
    Empty,
    #[error("workspace paths must be relative: {0:?}")]
    Absolute(String),
    #[error("workspace paths must use '/' separators: {0:?}")]
    Backslash(String),
    #[error("workspace paths must not contain empty, '.' or '..' components: {0:?}")]
    InvalidComponent(String),
    #[error("workspace paths must not contain NUL bytes")]
    Nul,
}

fn validate_workspace_path(path: &str) -> Result<(), WorkspacePathError> {
    if path.is_empty() {
        return Err(WorkspacePathError::Empty);
    }
    if path.contains('\0') {
        return Err(WorkspacePathError::Nul);
    }
    if path.starts_with('/') || has_windows_prefix(path) {
        return Err(WorkspacePathError::Absolute(path.to_owned()));
    }
    if path.contains('\\') {
        return Err(WorkspacePathError::Backslash(path.to_owned()));
    }
    if path
        .split('/')
        .any(|part| part.is_empty() || matches!(part, "." | ".."))
    {
        return Err(WorkspacePathError::InvalidComponent(path.to_owned()));
    }
    Ok(())
}

fn has_windows_prefix(path: &str) -> bool {
    let bytes = path.as_bytes();
    bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':'
}

/// Opaque revision evidence supplied and compared by a host adapter.
///
/// Core never interprets the value. A native adapter may use a content digest;
/// another host may use an API revision token. Equality means that the resource
/// observed during preparation is still the resource about to be changed.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ResourceRevision(String);

impl ResourceRevision {
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// One immutable resource already read by a host.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResourceSnapshot {
    path: WorkspacePath,
    bytes: Vec<u8>,
    revision: ResourceRevision,
}

impl ResourceSnapshot {
    pub fn new(path: WorkspacePath, bytes: Vec<u8>, revision: ResourceRevision) -> Self {
        Self {
            path,
            bytes,
            revision,
        }
    }

    pub fn path(&self) -> &WorkspacePath {
        &self.path
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn revision(&self) -> &ResourceRevision {
        &self.revision
    }
}

/// Host inventory available to Core before resource bodies are loaded.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct WorkspaceInventory(BTreeMap<WorkspacePath, ResourceRevision>);

impl WorkspaceInventory {
    pub fn new(resources: impl IntoIterator<Item = (WorkspacePath, ResourceRevision)>) -> Self {
        Self(resources.into_iter().collect())
    }

    pub fn revision(&self, path: &WorkspacePath) -> Option<&ResourceRevision> {
        self.0.get(path)
    }

    pub fn paths(&self) -> impl Iterator<Item = &WorkspacePath> {
        self.0.keys()
    }
}

/// Which host mount owns a logical resource.
///
/// The optional external plans tree is deliberately a separate namespace: an
/// adapter must never disguise it as `plans/...` inside the workspace mount.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum MountId {
    #[default]
    Workspace,
    ExternalPlans,
}

impl MountId {
    pub fn is_workspace(&self) -> bool {
        matches!(self, Self::Workspace)
    }
}

/// A logical path together with the mount that owns it.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ResourceLocation {
    pub mount: MountId,
    pub path: WorkspacePath,
}

impl ResourceLocation {
    pub fn new(mount: MountId, path: WorkspacePath) -> Self {
        Self { mount, path }
    }
}

/// The two native workspace inputs accepted by Core.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct WorkspaceMounts<T> {
    pub workspace: T,
    pub external_plans: Option<T>,
}

/// Host-neutral workspace layout evidence.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WorkspaceScope {
    Project { root_charter_name: String },
    User,
}

impl WorkspaceScope {
    pub fn project_root_charter(&self) -> Option<&str> {
        match self {
            Self::Project { root_charter_name } => Some(root_charter_name),
            Self::User => None,
        }
    }
}

/// Files and collections visible in one mount.
///
/// Collections are explicit because an empty external vdir collection still
/// has semantic ownership and quarantine consequences.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct MountInventory {
    pub files: WorkspaceInventory,
    pub collections: BTreeSet<WorkspacePath>,
}

/// A host read failure represented without an I/O error or OS path.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResourceReadFailure {
    pub path: WorkspacePath,
    pub message: String,
}

/// Successful immutable reads plus failures for one mount.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct MountReadEvidence {
    pub snapshot: WorkspaceSnapshot,
    pub failures: Vec<ResourceReadFailure>,
}

/// A pure request for the resource bodies needed for workspace assembly.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ReadPlan(Vec<WorkspacePath>);

impl ReadPlan {
    pub fn new(paths: impl IntoIterator<Item = WorkspacePath>) -> Self {
        Self(
            paths
                .into_iter()
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect(),
        )
    }

    pub fn all(inventory: &WorkspaceInventory) -> Self {
        Self(inventory.paths().cloned().collect())
    }

    pub fn paths(&self) -> &[WorkspacePath] {
        &self.0
    }
}

/// Plan the immutable reads required to assemble the supplied mounts.
///
/// Classification and narrower read planning can evolve in Core without
/// changing adapter traversal. Reading all inventoried files is the safe first
/// contract: the host still decides only how to obtain bytes, never which
/// workspace resources carry meaning.
pub fn plan_workspace_read(
    inventory: &WorkspaceMounts<MountInventory>,
) -> WorkspaceMounts<ReadPlan> {
    WorkspaceMounts {
        workspace: ReadPlan::all(&inventory.workspace.files),
        external_plans: inventory
            .external_plans
            .as_ref()
            .map(|mount| ReadPlan::all(&mount.files)),
    }
}

/// A coherent set of immutable resource snapshots.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct WorkspaceSnapshot(BTreeMap<WorkspacePath, ResourceSnapshot>);

impl WorkspaceSnapshot {
    pub fn new(
        resources: impl IntoIterator<Item = ResourceSnapshot>,
    ) -> Result<Self, SnapshotError> {
        let mut by_path = BTreeMap::new();
        for resource in resources {
            let path = resource.path.clone();
            if by_path.insert(path.clone(), resource).is_some() {
                return Err(SnapshotError::Duplicate(path));
            }
        }
        Ok(Self(by_path))
    }

    pub fn resource(&self, path: &WorkspacePath) -> Option<&ResourceSnapshot> {
        self.0.get(path)
    }

    pub fn resources(&self) -> impl Iterator<Item = &ResourceSnapshot> {
        self.0.values()
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum SnapshotError {
    #[error("snapshot contains duplicate resource {0}")]
    Duplicate(WorkspacePath),
}

/// One opaque resource change decided by Core and executed by a host.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Effect {
    Write {
        path: WorkspacePath,
        bytes: Vec<u8>,
    },
    Remove {
        path: WorkspacePath,
    },
    /// Relocate a resource without assigning filesystem rename semantics.
    Move {
        source: WorkspacePath,
        destination: WorkspacePath,
    },
}

impl Effect {
    fn affected_paths(&self) -> impl Iterator<Item = &WorkspacePath> {
        let (first, second) = match self {
            Self::Write { path, .. } | Self::Remove { path } => (path, None),
            Self::Move {
                source,
                destination,
            } => (source, Some(destination)),
        };
        std::iter::once(first).chain(second)
    }
}

/// Expected prior state checked immediately before effect execution.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExpectedResource {
    Missing,
    Revision(ResourceRevision),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResourcePrecondition {
    pub path: WorkspacePath,
    pub expected: ExpectedResource,
}

/// One semantic mutation batch that converges atomically across recovery.
///
/// Every affected resource has exactly one precondition. Additional
/// preconditions may protect read-only resources that influenced the decision.
/// A host that cannot provide recover-forward execution must report partial or
/// indeterminate delivery honestly and must not adopt the speculative state.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EffectBatch {
    effects: Vec<Effect>,
    preconditions: Vec<ResourcePrecondition>,
}

impl EffectBatch {
    pub fn new(
        effects: Vec<Effect>,
        preconditions: Vec<ResourcePrecondition>,
    ) -> Result<Self, EffectBatchError> {
        let mut affected = BTreeSet::new();
        for path in effects.iter().flat_map(Effect::affected_paths) {
            if !affected.insert(path.clone()) {
                return Err(EffectBatchError::DuplicateEffect(path.clone()));
            }
        }
        let mut expected = BTreeSet::new();
        for precondition in &preconditions {
            if !expected.insert(precondition.path.clone()) {
                return Err(EffectBatchError::DuplicatePrecondition(
                    precondition.path.clone(),
                ));
            }
        }
        if let Some(path) = affected.difference(&expected).next() {
            return Err(EffectBatchError::MissingPrecondition(path.clone()));
        }
        Ok(Self {
            effects,
            preconditions,
        })
    }

    pub fn effects(&self) -> &[Effect] {
        &self.effects
    }

    pub fn preconditions(&self) -> &[ResourcePrecondition] {
        &self.preconditions
    }

    pub fn is_empty(&self) -> bool {
        self.effects.is_empty()
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum EffectBatchError {
    #[error("resource {0} is affected more than once in one batch")]
    DuplicateEffect(WorkspacePath),
    #[error("effect on {0} has no prior-state precondition")]
    MissingPrecondition(WorkspacePath),
    #[error("resource {0} has more than one precondition")]
    DuplicatePrecondition(WorkspacePath),
}

/// A stale-state conflict found immediately before execution.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResourceConflict {
    pub path: WorkspacePath,
    pub expected: ExpectedResource,
    pub actual: Option<ResourceRevision>,
}

/// Host execution failed; Core's speculative state must not be adopted.
///
/// `E` is owned by the host (for example `std::io::Error` natively or a vault
/// API error in a browser), so Core does not acquire a host-specific error
/// dependency. `Partial` identifies effects completed by a weaker host before
/// failure; the native adapter is expected to offer all-or-nothing recovery.
#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum DeliveryError<E: fmt::Display> {
    #[error("resource revision conflict at {conflict_path}", conflict_path = .conflict.path)]
    Conflict { conflict: ResourceConflict },
    /// Failure before durable intent: no effect was applied.
    #[error("effect batch was not applied: {0}")]
    NotApplied(E),
    /// Durable intent may complete when the host recovers.
    #[error("effect batch requires recovery before its state is known: {0}")]
    RecoveryRequired(E),
    /// A weaker host applied only the listed effects and cannot recover forward.
    #[error("effect batch partially failed after effects {applied_effects:?}: {error}")]
    Partial {
        applied_effects: Vec<usize>,
        error: E,
    },
}

/// A pure mutation decision whose state remains speculative until delivery.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PreparedMutation<S, O = ()> {
    next_state: S,
    effects: EffectBatch,
    outcome: O,
}

impl<S> PreparedMutation<S> {
    pub fn new(next_state: S, effects: EffectBatch) -> Self {
        Self::with_outcome(next_state, effects, ())
    }
}

impl<S, O> PreparedMutation<S, O> {
    pub fn with_outcome(next_state: S, effects: EffectBatch, outcome: O) -> Self {
        Self {
            next_state,
            effects,
            outcome,
        }
    }

    /// Inspect, but do not adopt, the speculative state.
    pub fn next_state(&self) -> &S {
        &self.next_state
    }

    pub fn effects(&self) -> &EffectBatch {
        &self.effects
    }

    pub fn outcome(&self) -> &O {
        &self.outcome
    }

    /// Resolve host delivery and adopt the speculative state only on success.
    ///
    /// Conflicts and failures consume the prepared value without exposing its
    /// next state, forcing the caller to reload and recompute.
    pub fn adopt<E: fmt::Display>(
        self,
        delivery: Result<(), DeliveryError<E>>,
    ) -> Result<AppliedMutation<S, O>, DeliveryError<E>> {
        delivery.map(|()| AppliedMutation {
            state: self.next_state,
            outcome: self.outcome,
        })
    }
}

/// State and public outcome released only after successful host delivery.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AppliedMutation<S, O> {
    pub state: S,
    pub outcome: O,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn path(value: &str) -> WorkspacePath {
        WorkspacePath::new(value).unwrap()
    }

    #[test]
    fn logical_paths_reject_host_path_semantics() {
        for invalid in ["", "/tmp/a", "C:/tmp/a", "a\\b", "a//b", "a/./b", "a/../b"] {
            assert!(WorkspacePath::new(invalid).is_err(), "accepted {invalid:?}");
        }
        assert_eq!(
            path("charters/work/next.actions").as_str(),
            "charters/work/next.actions"
        );
    }

    #[test]
    fn read_plans_are_deterministic_and_deduplicated() {
        let plan = ReadPlan::new([path("b.actions"), path("a.actions"), path("b.actions")]);
        assert_eq!(plan.paths(), &[path("a.actions"), path("b.actions")]);
    }

    #[test]
    fn workspace_and_external_plans_keep_separate_read_namespaces() {
        let same = path("next/item.ics");
        let inventory = WorkspaceMounts {
            workspace: MountInventory {
                files: WorkspaceInventory::new([(
                    same.clone(),
                    ResourceRevision::new("workspace"),
                )]),
                collections: BTreeSet::new(),
            },
            external_plans: Some(MountInventory {
                files: WorkspaceInventory::new([(same.clone(), ResourceRevision::new("external"))]),
                collections: BTreeSet::new(),
            }),
        };

        let plan = plan_workspace_read(&inventory);
        assert_eq!(plan.workspace.paths(), std::slice::from_ref(&same));
        assert_eq!(
            plan.external_plans.unwrap().paths(),
            std::slice::from_ref(&same)
        );
    }

    #[test]
    fn snapshots_reject_duplicate_logical_resources() {
        let resource =
            || ResourceSnapshot::new(path("next.actions"), vec![], ResourceRevision::new("r1"));
        assert_eq!(
            WorkspaceSnapshot::new([resource(), resource()]),
            Err(SnapshotError::Duplicate(path("next.actions")))
        );
    }

    #[test]
    fn every_affected_resource_requires_revision_evidence() {
        let effects = vec![Effect::Move {
            source: path("charters/work.md"),
            destination: path("archive/work.md"),
        }];
        let source_only = vec![ResourcePrecondition {
            path: path("charters/work.md"),
            expected: ExpectedResource::Revision(ResourceRevision::new("r1")),
        }];
        assert_eq!(
            EffectBatch::new(effects.clone(), source_only),
            Err(EffectBatchError::MissingPrecondition(path(
                "archive/work.md"
            )))
        );

        let complete = vec![
            ResourcePrecondition {
                path: path("charters/work.md"),
                expected: ExpectedResource::Revision(ResourceRevision::new("r1")),
            },
            ResourcePrecondition {
                path: path("archive/work.md"),
                expected: ExpectedResource::Missing,
            },
        ];
        assert!(EffectBatch::new(effects, complete).is_ok());
    }

    #[test]
    fn next_state_is_adopted_only_after_delivery_succeeds() {
        let batch = EffectBatch::new(Vec::new(), Vec::new()).unwrap();
        let prepared = PreparedMutation::new("next", batch.clone());
        assert_eq!(prepared.next_state(), &"next");
        assert_eq!(
            prepared.adopt::<&str>(Ok(())),
            Ok(AppliedMutation {
                state: "next",
                outcome: (),
            })
        );

        let failed = PreparedMutation::new("must remain speculative", batch);
        assert_eq!(
            failed.adopt(Err(DeliveryError::NotApplied("host failed"))),
            Err(DeliveryError::NotApplied("host failed"))
        );
    }
}
