//! Pure workspace assembly from host-supplied inventories and immutable bytes.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};

use uuid::Uuid;

use super::WorkspaceError;
use super::findings::Finding;
use super::load::WorkspaceRead;
use super::pathing::{
    charter_collection_from_anchor, infer_charter_name_for_workspace,
    infer_parent_charter_name_for_workspace,
};
use crate::domain::{Charter, DomainModel};
use crate::workspace::actions::TrustedDocument;
use crate::workspace::actions::convert::from_actions_with_charter;
use crate::workspace::actions::repository::SourcedAction;
use crate::workspace::calendar::ics::parse_ics;
use crate::workspace::charter::{
    MarkdownCharter, frontmatter_has_id_key, frontmatter_has_parent_key, parse_charter,
};
use crate::workspace::resource::{
    MountId, MountInventory, MountReadEvidence, WorkspaceMounts, WorkspacePath, WorkspaceScope,
};
use crate::workspace::sidecar::{ActionMeta, hydrate_actions_map, parse_sidecar, sidecar_path};

/// Host-neutral evidence required for workspace assembly.
#[derive(Clone, Debug)]
pub struct WorkspaceAssemblyInput {
    pub scope: WorkspaceScope,
    pub inventory: WorkspaceMounts<MountInventory>,
    pub reads: WorkspaceMounts<MountReadEvidence>,
    /// Live occurrence lineage decoded and mount-validated by the native host.
    pub occurrence_links: HashMap<Uuid, (Uuid, String)>,
}

impl WorkspaceAssemblyInput {
    fn workspace_text(&self, path: &Path) -> Result<Option<&str>, String> {
        let logical = workspace_path(path)?;
        let Some(resource) = self.reads.workspace.snapshot.resource(&logical) else {
            return Ok(None);
        };
        std::str::from_utf8(resource.bytes())
            .map(Some)
            .map_err(|error| error.to_string())
    }

    fn plan_text(&self, path: &WorkspacePath) -> Result<Option<&str>, String> {
        let reads = self
            .reads
            .external_plans
            .as_ref()
            .unwrap_or(&self.reads.workspace);
        let Some(resource) = reads.snapshot.resource(path) else {
            return Ok(None);
        };
        std::str::from_utf8(resource.bytes())
            .map(Some)
            .map_err(|error| error.to_string())
    }

    fn workspace_files(&self) -> impl Iterator<Item = &WorkspacePath> {
        self.inventory.workspace.files.paths()
    }

    fn effective_plan_inventory(&self) -> (&MountInventory, MountId) {
        match self.inventory.external_plans.as_ref() {
            Some(inventory) => (inventory, MountId::ExternalPlans),
            None => (&self.inventory.workspace, MountId::Workspace),
        }
    }
}

/// Assemble the file-layer workspace without touching a host.
pub fn assemble_workspace(input: &WorkspaceAssemblyInput) -> Result<WorkspaceRead, WorkspaceError> {
    let project_root_charter = input.scope.project_root_charter();
    let mut findings = read_failure_findings(input);
    let mut charters: HashMap<String, MarkdownCharter> = HashMap::new();
    let mut path_for_name: HashMap<String, PathBuf> = HashMap::new();
    let global_actions = collect_sidecar_actions(input);

    for relative in action_files(input) {
        let Some(name) = infer_charter_name_for_workspace(&relative, project_root_charter) else {
            findings.push(Finding::violation(
                "charter-name-unresolved",
                &relative,
                "failed to infer a charter name from the file path; file skipped",
            ));
            continue;
        };
        let action_source = match input.workspace_text(&Path::new("charters").join(&relative)) {
            Ok(Some(source)) => source,
            Ok(None) => continue,
            Err(error) => {
                findings.push(Finding::violation(
                    "unreadable-file",
                    &relative,
                    format!("could not decode file as UTF-8: {error}; file skipped"),
                ));
                continue;
            }
        };
        let parsed_doc = match crate::workspace::parse_document(action_source) {
            Ok(doc) => doc,
            Err(error) => {
                findings.push(Finding::violation(
                    "unparseable-file",
                    &relative,
                    format!("could not parse file: {error}; file skipped"),
                ));
                continue;
            }
        };
        let parsed_doc = match TrustedDocument::try_from(parsed_doc) {
            Ok(document) => document.into_parsed(),
            Err(error) => {
                let summary = error
                    .issues
                    .first()
                    .map(|first| {
                        format!(
                            "{} parser issue(s); first at line {}, column {}: {}; file quarantined",
                            error.issues.len(),
                            first.range.start_row + 1,
                            first.range.start_col + 1,
                            first.message
                        )
                    })
                    .unwrap_or_else(|| "parser integrity issue; file quarantined".to_string());
                findings.push(Finding::warning("syntax-errors", &relative, summary));
                continue;
            }
        };

        let source_map = parsed_doc.source_map;
        let mut sourced: Vec<SourcedAction> = parsed_doc
            .actions
            .into_iter()
            .map(|mut action| {
                if action.charter.is_none() {
                    action.charter = Some(name.clone());
                }
                let metadata = source_map.get(&action.id).cloned();
                SourcedAction {
                    action,
                    source_metadata: metadata,
                }
            })
            .collect();
        let base: Charter = from_actions_with_charter(
            &sourced
                .iter()
                .map(|sourced| sourced.action.clone())
                .collect::<Vec<_>>(),
            name.clone(),
        );
        let mut charter = MarkdownCharter::from(base);
        charter.actions_file = Some(relative.clone());
        charter.plans_dir = charter_collection_from_anchor(&relative);
        let conventional_sidecar = sidecar_path(&relative);
        if let Some(source) = input
            .workspace_text(&Path::new("charters").join(&conventional_sidecar))
            .map_err(WorkspaceError::Parse)?
            && let Err(error) = parse_sidecar(source)
        {
            findings.push(Finding::violation(
                "sidecar-corrupt",
                &conventional_sidecar,
                format!("could not parse sidecar at expected path: {error}"),
            ));
        }
        hydrate_actions_map(&mut sourced, &global_actions);
        charter.actions = sourced;
        charters.entry(name.clone()).or_insert(charter);
        path_for_name.entry(name).or_insert(relative);
    }

    let mut explicit_parent_charters = HashSet::new();
    let mut explicit_id_charters = HashSet::new();
    for relative in charter_files(input) {
        let Some(name) = infer_charter_name_for_workspace(&relative, project_root_charter) else {
            findings.push(Finding::violation(
                "charter-name-unresolved",
                &relative,
                "failed to infer a charter name from the file path; file skipped",
            ));
            continue;
        };
        let content = match input.workspace_text(&Path::new("charters").join(&relative)) {
            Ok(Some(content)) => content,
            Ok(None) => continue,
            Err(error) => {
                findings.push(Finding::violation(
                    "unreadable-file",
                    &relative,
                    format!("could not decode file as UTF-8: {error}; file skipped"),
                ));
                continue;
            }
        };
        if frontmatter_has_parent_key(content) {
            explicit_parent_charters.insert(name.clone());
        }
        if frontmatter_has_id_key(content) {
            explicit_id_charters.insert(name.clone());
        }
        let explicit = match parse_charter(content) {
            Ok(charter) => charter,
            Err(error) => {
                findings.push(Finding::violation(
                    "unparseable-file",
                    &relative,
                    format!("could not parse charter frontmatter: {error}; file skipped"),
                ));
                continue;
            }
        };
        let is_readme = relative.file_name().and_then(|name| name.to_str()) == Some("README.md");
        if is_readme || relative.components().count() == 1 {
            path_for_name
                .entry(name.clone())
                .or_insert(relative.clone());
        }
        let md_relative = relative.clone();
        charters
            .entry(name)
            .and_modify(|implicit| {
                implicit.id = explicit.id;
                implicit.title = explicit.title.clone();
                implicit.description = explicit.description.clone();
                if explicit.alias.is_some() {
                    implicit.alias = explicit.alias.clone();
                }
                if explicit.parent.is_some() {
                    implicit.parent = explicit.parent.clone();
                }
                if explicit.objectives.is_some() {
                    implicit.objectives = explicit.objectives.clone();
                }
                if explicit.state.is_some() {
                    implicit.state = explicit.state;
                }
                implicit.md_file = Some(md_relative.clone());
            })
            .or_insert_with(|| {
                let mut charter = MarkdownCharter::from(explicit);
                charter.plans_dir = charter_collection_from_anchor(&md_relative);
                charter.md_file = Some(md_relative);
                charter
            });
    }

    for (name, charter) in charters.iter_mut() {
        if explicit_id_charters.contains(name) {
            continue;
        }
        let Some(actions_file) = &charter.actions_file else {
            continue;
        };
        let path = Path::new("charters").join(sidecar_path(actions_file));
        if let Ok(Some(source)) = input.workspace_text(&path)
            && let Ok(Some(id)) = parse_sidecar(source).map(|meta| meta.charter.and_then(|c| c.id))
        {
            charter.id = id;
        }
    }

    let name_to_alias: HashMap<String, String> = charters
        .iter()
        .filter_map(|(name, charter)| {
            charter
                .alias
                .as_ref()
                .map(|alias| (name.clone(), alias.clone()))
        })
        .collect();
    for (name, parent_name) in parent_hints(&path_for_name, project_root_charter) {
        if explicit_parent_charters.contains(&name) {
            continue;
        }
        let parent_alias = name_to_alias
            .get(&parent_name)
            .cloned()
            .unwrap_or(parent_name);
        if let Some(charter) = charters.get_mut(&name)
            && charter.parent.is_none()
        {
            charter.parent = Some(parent_alias);
        }
    }

    let known_aliases: HashSet<&str> = charters
        .values()
        .filter_map(|charter| charter.alias.as_deref())
        .collect();
    for (name, charter) in &charters {
        if let Some(parent) = &charter.parent
            && !known_aliases.contains(parent.as_str())
        {
            findings.push(Finding::warning(
                "unresolvable-parent",
                path_for_name
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| PathBuf::from("<unknown>")),
                format!(
                    "charter '{}' has unresolvable parent '{}' — use the alias (machine key), not the display title",
                    charter.alias.as_deref().unwrap_or(&charter.title),
                    parent
                ),
            ));
        }
    }

    attach_plans(input, &mut charters, &mut findings)?;
    let mut charters: Vec<_> = charters.into_values().collect();
    resolve_predecessor_aliases(&mut charters);
    hydrate_occurrence_links(&mut charters, &input.occurrence_links);
    Ok(WorkspaceRead { charters, findings })
}

/// Lower a pure assembled read to the domain model.
pub fn assembled_domain_model(read: WorkspaceRead) -> DomainModel {
    DomainModel {
        objectives: vec![],
        charters: read.charters.into_iter().map(Charter::from).collect(),
    }
}

fn action_files(input: &WorkspaceAssemblyInput) -> Vec<PathBuf> {
    let mut paths = input
        .workspace_files()
        .filter_map(|path| path.as_str().strip_prefix("charters/"))
        .filter(|path| !has_hidden_component(path))
        .filter(|path| path.ends_with(".actions"))
        .filter(|path| !path.ends_with(".completed.actions"))
        .filter(|path| !path.ends_with(".upcoming.actions"))
        .map(PathBuf::from)
        .collect::<Vec<_>>();
    paths.sort();
    paths
}

fn charter_files(input: &WorkspaceAssemblyInput) -> Vec<PathBuf> {
    let mut paths = input
        .workspace_files()
        .filter_map(|path| path.as_str().strip_prefix("charters/"))
        .filter(|path| !has_hidden_component(path))
        .filter(|path| path.ends_with(".md"))
        .map(PathBuf::from)
        .collect::<Vec<_>>();
    paths.sort();
    paths
}

fn collect_sidecar_actions(input: &WorkspaceAssemblyInput) -> BTreeMap<String, ActionMeta> {
    let mut union = BTreeMap::new();
    let mut sidecars = input
        .workspace_files()
        .filter(|path| {
            let path = path.as_str();
            path.starts_with("charters/")
                && !has_hidden_parent_component(path)
                && path.ends_with(".json")
                && path
                    .rsplit('/')
                    .next()
                    .is_some_and(|name| name.starts_with('.'))
        })
        .collect::<Vec<_>>();
    sidecars.sort();
    for path in sidecars {
        let Some(resource) = input.reads.workspace.snapshot.resource(path) else {
            continue;
        };
        let Ok(source) = std::str::from_utf8(resource.bytes()) else {
            continue;
        };
        let Ok(metadata) = parse_sidecar(source) else {
            continue;
        };
        for (key, action) in metadata.actions {
            let target: &mut ActionMeta = union.entry(key).or_default();
            target.created = target.created.or(action.created);
            target.occurrence = target.occurrence.clone().or(action.occurrence);
        }
    }
    union
}

fn attach_plans(
    input: &WorkspaceAssemblyInput,
    charters: &mut HashMap<String, MarkdownCharter>,
    findings: &mut Vec<Finding>,
) -> Result<(), WorkspaceError> {
    let (inventory, mount) = input.effective_plan_inventory();
    let prefix = if mount == MountId::Workspace {
        Some("plans/")
    } else {
        None
    };
    let owned: HashSet<PathBuf> = charters
        .values()
        .map(|charter| charter.plans_dir.clone())
        .collect();
    let mut reported = HashSet::new();
    for collection in &inventory.collections {
        let relative = match prefix {
            Some(prefix) => match collection.as_str().strip_prefix(prefix) {
                Some(relative) => relative,
                None => continue,
            },
            None => collection.as_str(),
        };
        if relative.is_empty() || relative.contains('/') || relative.starts_with('.') {
            continue;
        }
        let relative = PathBuf::from(relative);
        if !owned.contains(&relative) && reported.insert(relative.clone()) {
            findings.push(Finding::violation_at(
                mount,
                "unowned-plans-collection",
                relative,
                "calendar collection has no owning charter; resources are quarantined. `clearhead doctor --fix` can remove the local collection, which may propagate deletion through vdirsyncer",
            ));
        }
    }

    for logical in inventory
        .files
        .paths()
        .filter(|path| path.as_str().ends_with(".ics"))
        .filter(|path| !has_hidden_component(path.as_str()))
    {
        let relative_str = match prefix {
            Some(prefix) => match logical.as_str().strip_prefix(prefix) {
                Some(path) => path,
                None => continue,
            },
            None => logical.as_str(),
        };
        let relative = PathBuf::from(relative_str);
        let Some(collection) = relative.parent() else {
            continue;
        };
        let Some(charter) = charters
            .values_mut()
            .find(|charter| charter.plans_dir == collection)
        else {
            let collection = collection.to_path_buf();
            if reported.insert(collection.clone()) {
                findings.push(Finding::violation_at(
                    mount,
                    "unowned-plans-collection",
                    collection,
                    "calendar collection has no owning charter; resources are quarantined. `clearhead doctor --fix` can remove the local collection, which may propagate deletion through vdirsyncer",
                ));
            }
            continue;
        };
        let source = match input.plan_text(logical) {
            Ok(Some(source)) => source,
            Ok(None) => continue,
            Err(error) => {
                findings.push(Finding::violation_at(
                    mount,
                    "unreadable-file",
                    &relative,
                    format!("could not decode ics as UTF-8: {error}; file skipped"),
                ));
                continue;
            }
        };
        match parse_ics(source, &relative) {
            Ok(plans) => charter.plans.extend(plans),
            Err(error) => findings.push(Finding::violation_at(
                mount,
                "unparseable-file",
                &relative,
                format!("could not parse ics: {error}; file skipped"),
            )),
        }
    }
    Ok(())
}

fn read_failure_findings(input: &WorkspaceAssemblyInput) -> Vec<Finding> {
    let workspace = input.reads.workspace.failures.iter().map(|failure| {
        Finding::violation_at(
            MountId::Workspace,
            "unreadable-file",
            PathBuf::from(failure.path.as_str()),
            format!("could not read file: {}; file skipped", failure.message),
        )
    });
    let external = input
        .reads
        .external_plans
        .iter()
        .flat_map(|read| &read.failures)
        .map(|failure| {
            Finding::violation_at(
                MountId::ExternalPlans,
                "unreadable-file",
                PathBuf::from(failure.path.as_str()),
                format!("could not read file: {}; file skipped", failure.message),
            )
        });
    workspace.chain(external).collect()
}

fn has_hidden_component(path: &str) -> bool {
    path.split('/').any(|component| component.starts_with('.'))
}

fn has_hidden_parent_component(path: &str) -> bool {
    path.rsplit_once('/')
        .is_some_and(|(parent, _)| has_hidden_component(parent))
}

fn workspace_path(path: &Path) -> Result<WorkspacePath, String> {
    WorkspacePath::new(path.to_string_lossy().replace('\\', "/")).map_err(|error| error.to_string())
}

fn parent_hints(
    path_for_name: &HashMap<String, PathBuf>,
    project_root_charter: Option<&str>,
) -> Vec<(String, String)> {
    path_for_name
        .iter()
        .filter_map(|(name, path)| {
            infer_parent_charter_name_for_workspace(path, project_root_charter)
                .map(|parent| (name.clone(), parent))
        })
        .collect()
}

fn resolve_predecessor_aliases(charters: &mut [MarkdownCharter]) {
    let alias_to_id: HashMap<String, Uuid> = charters
        .iter()
        .flat_map(|charter| &charter.actions)
        .filter_map(|sourced| {
            sourced
                .action
                .alias
                .as_ref()
                .map(|alias| (alias.to_lowercase(), sourced.action.id))
        })
        .collect();
    for charter in charters {
        for sourced in &mut charter.actions {
            for predecessor in sourced.action.predecessors.iter_mut().flatten() {
                if predecessor.resolved_uuid.is_none() {
                    predecessor.resolved_uuid = alias_to_id
                        .get(&predecessor.raw_ref.trim().to_lowercase())
                        .copied();
                }
            }
        }
    }
}

fn hydrate_occurrence_links(
    charters: &mut [MarkdownCharter],
    links: &HashMap<Uuid, (Uuid, String)>,
) {
    for charter in charters {
        for sourced in &mut charter.actions {
            if let Some((plan_id, slot_key)) = links.get(&sourced.action.id) {
                sourced.action.plan_id = Some(*plan_id);
                sourced.action.external_occurrence_key = Some(slot_key.clone());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::resource::{
        ResourceRevision, ResourceSnapshot, WorkspaceInventory, WorkspaceSnapshot,
    };

    fn input(
        scope: WorkspaceScope,
        workspace: &[(&str, &str)],
        external: Option<&[(&str, &str)]>,
        external_collections: &[&str],
    ) -> WorkspaceAssemblyInput {
        fn mount(
            files: &[(&str, &str)],
            collections: &[&str],
        ) -> (MountInventory, MountReadEvidence) {
            let snapshots = files
                .iter()
                .map(|(path, bytes)| {
                    ResourceSnapshot::new(
                        WorkspacePath::new(*path).unwrap(),
                        bytes.as_bytes().to_vec(),
                        ResourceRevision::new(format!("{path}-rev")),
                    )
                })
                .collect::<Vec<_>>();
            let inventory = MountInventory {
                files: WorkspaceInventory::new(
                    snapshots
                        .iter()
                        .map(|snapshot| (snapshot.path().clone(), snapshot.revision().clone())),
                ),
                collections: collections
                    .iter()
                    .map(|path| WorkspacePath::new(*path).unwrap())
                    .collect(),
            };
            let reads = MountReadEvidence {
                snapshot: WorkspaceSnapshot::new(snapshots).unwrap(),
                failures: vec![],
            };
            (inventory, reads)
        }

        let (workspace_inventory, workspace_reads) = mount(workspace, &[]);
        let (external_inventory, external_reads) = external
            .map(|files| mount(files, external_collections))
            .unzip();
        WorkspaceAssemblyInput {
            scope,
            inventory: WorkspaceMounts {
                workspace: workspace_inventory,
                external_plans: external_inventory,
            },
            reads: WorkspaceMounts {
                workspace: workspace_reads,
                external_plans: external_reads,
            },
            occurrence_links: HashMap::new(),
        }
    }

    #[test]
    fn project_scope_names_the_primary_charter_without_a_host_path() {
        let id = Uuid::now_v7();
        let read = assemble_workspace(&input(
            WorkspaceScope::Project {
                root_charter_name: "platform".into(),
            },
            &[("charters/next.actions", &format!("[ ] Pure assembly #{id}"))],
            None,
            &[],
        ))
        .unwrap();
        assert_eq!(read.charters[0].alias.as_deref(), Some("platform"));
        assert_eq!(read.charters[0].actions[0].action.id, id);
    }

    #[test]
    fn external_empty_collection_is_not_flattened_into_workspace_plans() {
        let read = assemble_workspace(&input(
            WorkspaceScope::User,
            &[("charters/inbox.actions", "[ ] Inbox")],
            Some(&[]),
            &["orphan"],
        ))
        .unwrap();
        let finding = read
            .findings
            .iter()
            .find(|finding| finding.code == "unowned-plans-collection")
            .unwrap();
        assert_eq!(finding.mount, MountId::ExternalPlans);
        assert_eq!(finding.path, PathBuf::from("orphan"));
    }

    #[test]
    fn recovered_action_source_is_explicitly_quarantined() {
        let read = assemble_workspace(&input(
            WorkspaceScope::User,
            &[("charters/work.actions", "[ ] Broken [ note")],
            None,
            &[],
        ))
        .unwrap();
        assert!(read.charters.is_empty());
        assert!(
            read.findings
                .iter()
                .any(|finding| finding.code == "syntax-errors")
        );
    }
}
