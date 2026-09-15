//! Planning `clearhead init`: the pure decision half of workspace bootstrap.
//!
//! A host snapshots the four bootstrap resources under a data root and delivers
//! the returned batch. Every rule about what to create, what to keep, and what
//! to leave for `doctor` lives here, identically for project and user scope.
//! See `specifications/workspace.md#initialization` and `#the-root-charter`.

use uuid::Uuid;

use super::charter::{charter_frontmatter_id, format_charter, implicit_charter};
use super::manifest::{WorkspaceManifest, parse_workspace_manifest, render_workspace_manifest};
use super::resource::{
    Effect, EffectBatch, ExpectedResource, ResourceLocation, ResourcePrecondition,
    ResourceSnapshot, WorkspacePath,
};
use super::sidecar::{CharterMetadata, parse_sidecar, record_charter_id, render_sidecar};
use super::store::WorkspaceError;
use crate::domain::{Charter, CharterState};

pub const MANIFEST_PATH: &str = "workspace.json";
pub const ROOT_README_PATH: &str = "charters/README.md";
pub const ROOT_ACTIONS_PATH: &str = "charters/next.actions";
pub const ROOT_SIDECAR_PATH: &str = "charters/.next.json";

/// The bootstrap resources as the host found them; `None` means absent.
#[derive(Clone, Debug, Default)]
pub struct InitSnapshot {
    pub manifest: Option<ResourceSnapshot>,
    pub readme: Option<ResourceSnapshot>,
    pub root_actions: Option<ResourceSnapshot>,
    pub sidecar: Option<ResourceSnapshot>,
}

/// Host-supplied values, so planning stays deterministic.
#[derive(Clone, Debug)]
pub struct InitRequest {
    /// Name persisted when the workspace has none (`--name` or the scope default).
    pub name: String,
    /// Used only when the manifest has no `workspace_id`.
    pub workspace_id: Uuid,
    /// Used only when neither root anchor carries an id.
    pub root_id: Uuid,
    /// `created_at` date for a new manifest.
    pub created_at: String,
}

/// What the root's README declares about its identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReadmeId {
    Absent,
    WithoutId,
    Id(Uuid),
}

/// The root charter's identity as decided from its two anchors.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RootId {
    Resolved(Uuid),
    /// `charters/README.md` exists without an `id`; left for `doctor`.
    MissingFromReadme,
    /// README and sidecar disagree; left for `doctor`.
    Conflict {
        readme: Uuid,
        sidecar: Uuid,
    },
}

#[derive(Clone, Debug)]
pub struct InitPlan {
    pub batch: EffectBatch,
    pub manifest: WorkspaceManifest,
    /// False when the workspace already had a `workspace_id`.
    pub minted_workspace: bool,
    pub root_id: RootId,
}

/// README frontmatter is authoritative, the sidecar mirrors it, and an id is
/// minted only when neither anchor has one — never over an existing identity.
pub fn resolve_root_id(readme: ReadmeId, sidecar: Option<Uuid>, minted: Uuid) -> RootId {
    match (readme, sidecar) {
        (ReadmeId::Id(readme), Some(sidecar)) if readme != sidecar => {
            RootId::Conflict { readme, sidecar }
        }
        (ReadmeId::Id(id), _) => RootId::Resolved(id),
        (ReadmeId::WithoutId, _) => RootId::MissingFromReadme,
        (ReadmeId::Absent, Some(id)) => RootId::Resolved(id),
        (ReadmeId::Absent, None) => RootId::Resolved(minted),
    }
}

/// Decide the writes that bring a data root to the canonical bootstrap shape.
///
/// Only missing pieces are written. Every bootstrap resource is guarded by the
/// state it was planned from, so a concurrent change fails delivery instead of
/// being overwritten.
pub fn plan_workspace_init(
    snapshot: &InitSnapshot,
    request: &InitRequest,
) -> Result<InitPlan, WorkspaceError> {
    let mut effects = Vec::new();

    let existing = match &snapshot.manifest {
        Some(resource) => parse_workspace_manifest(text(resource)?)
            .map_err(|e| WorkspaceError::Parse(format!("{MANIFEST_PATH}: {e}")))?,
        None => WorkspaceManifest::default(),
    };
    let minted_workspace = existing.workspace_id.is_none();
    let manifest = if minted_workspace {
        let manifest = WorkspaceManifest {
            workspace_id: Some(request.workspace_id.to_string()),
            workspace_name: existing
                .workspace_name
                .or_else(|| Some(request.name.clone())),
            created_at: existing
                .created_at
                .or_else(|| Some(request.created_at.clone())),
        };
        let bytes = render_workspace_manifest(&manifest)
            .map_err(|e| WorkspaceError::Parse(format!("{MANIFEST_PATH}: {e}")))?;
        effects.push(write(MANIFEST_PATH, bytes)?);
        manifest
    } else {
        existing
    };
    let name = manifest
        .workspace_name
        .clone()
        .unwrap_or_else(|| request.name.clone());

    if snapshot.root_actions.is_none() {
        effects.push(write(ROOT_ACTIONS_PATH, String::new())?);
    }

    let readme_id = match &snapshot.readme {
        None => ReadmeId::Absent,
        Some(resource) => match charter_frontmatter_id(text(resource)?)
            .map_err(|e| WorkspaceError::Parse(format!("{ROOT_README_PATH}: {e}")))?
        {
            Some(id) => ReadmeId::Id(id),
            None => ReadmeId::WithoutId,
        },
    };
    let mut sidecar = match &snapshot.sidecar {
        Some(resource) => parse_sidecar(text(resource)?)?,
        None => CharterMetadata::default(),
    };
    let sidecar_id = sidecar.charter.as_ref().and_then(|charter| charter.id);
    let root_id = resolve_root_id(readme_id, sidecar_id, request.root_id);

    if let RootId::Resolved(id) = root_id {
        if snapshot.readme.is_none() {
            let root = Charter {
                id,
                state: Some(CharterState::Active),
                ..implicit_charter(&name)
            };
            effects.push(write(ROOT_README_PATH, format_charter(&root))?);
        }
        if record_charter_id(&mut sidecar, id) {
            effects.push(write(ROOT_SIDECAR_PATH, render_sidecar(&sidecar)?)?);
        }
    }

    let preconditions = [
        (MANIFEST_PATH, &snapshot.manifest),
        (ROOT_ACTIONS_PATH, &snapshot.root_actions),
        (ROOT_README_PATH, &snapshot.readme),
        (ROOT_SIDECAR_PATH, &snapshot.sidecar),
    ]
    .into_iter()
    .map(|(path, found)| {
        Ok(ResourcePrecondition {
            path: location(path)?,
            expected: found
                .as_ref()
                .map_or(ExpectedResource::Missing, |resource| {
                    ExpectedResource::Revision(resource.revision().clone())
                }),
        })
    })
    .collect::<Result<Vec<_>, WorkspaceError>>()?;

    let batch = EffectBatch::new(effects, preconditions)
        .map_err(|e| WorkspaceError::Parse(format!("init batch: {e:?}")))?;
    Ok(InitPlan {
        batch,
        manifest,
        minted_workspace,
        root_id,
    })
}

fn location(path: &str) -> Result<ResourceLocation, WorkspaceError> {
    WorkspacePath::new(path)
        .map(ResourceLocation::workspace)
        .map_err(|e| WorkspaceError::Parse(format!("{path}: {e:?}")))
}

fn write(path: &str, content: String) -> Result<Effect, WorkspaceError> {
    Ok(Effect::Write {
        path: location(path)?,
        bytes: content.into_bytes(),
    })
}

fn text(resource: &ResourceSnapshot) -> Result<&str, WorkspaceError> {
    std::str::from_utf8(resource.bytes())
        .map_err(|e| WorkspaceError::Parse(format!("{}: {e}", resource.path().as_str())))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::resource::ResourceRevision;
    use std::collections::BTreeMap;

    const WORKSPACE: Uuid = Uuid::from_u128(1);
    const ROOT: Uuid = Uuid::from_u128(2);
    const OTHER: Uuid = Uuid::from_u128(3);

    fn request() -> InitRequest {
        InitRequest {
            name: "demo".into(),
            workspace_id: WORKSPACE,
            root_id: ROOT,
            created_at: "2026-09-15".into(),
        }
    }

    fn found(path: &str, content: &str) -> Option<ResourceSnapshot> {
        Some(ResourceSnapshot::new(
            WorkspacePath::new(path).unwrap(),
            content.as_bytes().to_vec(),
            ResourceRevision::new(content),
        ))
    }

    fn sidecar_with(id: Uuid) -> String {
        let mut metadata = CharterMetadata::default();
        record_charter_id(&mut metadata, id);
        render_sidecar(&metadata).unwrap()
    }

    fn writes(plan: &InitPlan) -> BTreeMap<&str, String> {
        plan.batch
            .effects()
            .iter()
            .map(|effect| match effect {
                Effect::Write { path, bytes } => (
                    path.path.as_str(),
                    String::from_utf8(bytes.clone()).unwrap(),
                ),
                other => panic!("init must only write, got {other:?}"),
            })
            .collect()
    }

    fn sidecar_id(content: &str) -> Option<Uuid> {
        parse_sidecar(content).unwrap().charter.and_then(|c| c.id)
    }

    #[test]
    fn fresh_workspace_gets_manifest_and_root_scaffold() {
        let plan = plan_workspace_init(&InitSnapshot::default(), &request()).unwrap();
        let writes = writes(&plan);

        assert_eq!(
            writes.keys().copied().collect::<Vec<_>>(),
            [
                ROOT_SIDECAR_PATH,
                ROOT_README_PATH,
                ROOT_ACTIONS_PATH,
                MANIFEST_PATH
            ]
        );
        assert!(plan.minted_workspace);
        assert_eq!(plan.root_id, RootId::Resolved(ROOT));
        assert_eq!(
            charter_frontmatter_id(&writes[ROOT_README_PATH]),
            Ok(Some(ROOT))
        );
        assert!(writes[ROOT_README_PATH].contains("alias: demo"));
        assert_eq!(sidecar_id(&writes[ROOT_SIDECAR_PATH]), Some(ROOT));
        assert_eq!(
            parse_workspace_manifest(&writes[MANIFEST_PATH])
                .unwrap()
                .workspace_name
                .as_deref(),
            Some("demo")
        );
    }

    #[test]
    fn complete_workspace_plans_no_writes_and_mints_nothing() {
        let first = plan_workspace_init(&InitSnapshot::default(), &request()).unwrap();
        let first = writes(&first);
        let snapshot = InitSnapshot {
            manifest: found(MANIFEST_PATH, &first[MANIFEST_PATH]),
            readme: found(ROOT_README_PATH, &first[ROOT_README_PATH]),
            root_actions: found(ROOT_ACTIONS_PATH, ""),
            sidecar: found(ROOT_SIDECAR_PATH, &first[ROOT_SIDECAR_PATH]),
        };
        let fresh_ids = InitRequest {
            workspace_id: OTHER,
            root_id: OTHER,
            ..request()
        };

        let rerun = plan_workspace_init(&snapshot, &fresh_ids).unwrap();

        assert!(rerun.batch.effects().is_empty());
        assert!(!rerun.minted_workspace);
        assert_eq!(rerun.root_id, RootId::Resolved(ROOT));
    }

    #[test]
    fn readme_id_is_mirrored_into_the_sidecar_instead_of_minting() {
        let readme = format!("---\nid: {OTHER}\nalias: platform\n---\n# Platform\n");
        let snapshot = InitSnapshot {
            readme: found(ROOT_README_PATH, &readme),
            ..Default::default()
        };

        let plan = plan_workspace_init(&snapshot, &request()).unwrap();
        let writes = writes(&plan);

        assert_eq!(plan.root_id, RootId::Resolved(OTHER));
        assert!(!writes.contains_key(ROOT_README_PATH));
        assert_eq!(sidecar_id(&writes[ROOT_SIDECAR_PATH]), Some(OTHER));
    }

    #[test]
    fn sidecar_id_is_adopted_by_a_missing_readme() {
        let snapshot = InitSnapshot {
            sidecar: found(ROOT_SIDECAR_PATH, &sidecar_with(OTHER)),
            ..Default::default()
        };

        let plan = plan_workspace_init(&snapshot, &request()).unwrap();
        let writes = writes(&plan);

        assert_eq!(plan.root_id, RootId::Resolved(OTHER));
        assert_eq!(
            charter_frontmatter_id(&writes[ROOT_README_PATH]),
            Ok(Some(OTHER))
        );
        assert!(!writes.contains_key(ROOT_SIDECAR_PATH));
    }

    #[test]
    fn disagreeing_root_anchors_are_left_for_doctor() {
        let readme = format!("---\nid: {ROOT}\n---\n# Platform\n");
        let snapshot = InitSnapshot {
            readme: found(ROOT_README_PATH, &readme),
            sidecar: found(ROOT_SIDECAR_PATH, &sidecar_with(OTHER)),
            ..Default::default()
        };

        let plan = plan_workspace_init(&snapshot, &request()).unwrap();
        let writes = writes(&plan);

        assert_eq!(
            plan.root_id,
            RootId::Conflict {
                readme: ROOT,
                sidecar: OTHER
            }
        );
        assert!(!writes.contains_key(ROOT_README_PATH));
        assert!(!writes.contains_key(ROOT_SIDECAR_PATH));
    }

    #[test]
    fn readme_without_id_is_left_for_doctor() {
        let snapshot = InitSnapshot {
            readme: found(ROOT_README_PATH, "# Platform\n"),
            ..Default::default()
        };

        let plan = plan_workspace_init(&snapshot, &request()).unwrap();

        assert_eq!(plan.root_id, RootId::MissingFromReadme);
        assert!(!writes(&plan).contains_key(ROOT_SIDECAR_PATH));
    }

    #[test]
    fn persisted_workspace_name_wins_over_the_requested_name() {
        let manifest = render_workspace_manifest(&WorkspaceManifest {
            workspace_id: Some(WORKSPACE.to_string()),
            workspace_name: Some("persisted".into()),
            created_at: Some("2026-01-01".into()),
        })
        .unwrap();
        let snapshot = InitSnapshot {
            manifest: found(MANIFEST_PATH, &manifest),
            ..Default::default()
        };

        let plan = plan_workspace_init(&snapshot, &request()).unwrap();
        let writes = writes(&plan);

        assert!(!writes.contains_key(MANIFEST_PATH));
        assert!(writes[ROOT_README_PATH].contains("alias: persisted"));
    }
}
