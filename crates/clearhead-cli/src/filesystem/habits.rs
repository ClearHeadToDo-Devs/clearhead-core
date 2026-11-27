//! Native delivery of text-authored Habit catch-up. No calendar I/O effects.

use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};

use chrono::{DateTime, FixedOffset};
use clearhead_core::domain::Habit;
use clearhead_core::domain::habit_slots::{SlotKey, occurrence_id};
use clearhead_core::workspace::action_files::{
    ActionFileRole, actions_anchor, classify_action_file,
};
use clearhead_core::workspace::habit_files::habits_from_actions;
use clearhead_core::workspace::habit_stamp::{
    CatchUpBatch, CatchUpInput, StampSource, plan_text_catch_up, text_exclusions,
};
use clearhead_core::workspace::predecessors::PredecessorScope;
use clearhead_core::workspace::sidecar::{CharterMetadata, OccurrenceLineage, render_sidecar};
use clearhead_core::{
    ActionList, OutputFormat, ParseMode, ParsedDocument, format, parse_actions_with_mode,
};
use uuid::Uuid;

use super::{
    Effect, EffectBatch, ExpectedResource, NativeWorkspaceMounts, ResourceLocation,
    ResourcePrecondition, ResourceSnapshot, WorkspaceError, deliver, parse_sidecar_snapshot,
    read_workspace, sidecar_path, snapshot, templates::resolve_template, validate_source_path,
};

#[derive(Debug, Default)]
pub struct ApplyHabitsResult {
    pub occurrences: usize,
    pub actions: usize,
    pub has_backlog: bool,
    pub refused: Vec<String>,
}

/// Stage up to `budget` complete trees in this workspace. A refused Habit does
/// not consume the budget or prevent a different, valid Habit from generating.
pub fn apply_habits(
    workspace_root: &Path,
    now: DateTime<FixedOffset>,
    budget: usize,
    dry_run: bool,
) -> Result<ApplyHabitsResult, WorkspaceError> {
    let mounts = NativeWorkspaceMounts::resolve(workspace_root);
    let (definitions, history) = discover(&mounts)?;
    if definitions.is_empty() {
        return Ok(ApplyHabitsResult::default());
    }
    let mut files = ObservedFiles::new(&mounts.workspace);
    let mut occupied = files.occupied(history)?;
    let mut result = ApplyHabitsResult::default();
    let definitions: Vec<_> = definitions
        .into_iter()
        .filter_map(|path| match files.document(&path) {
            Ok(doc) => Some((path, doc)),
            Err(error) => {
                result.refused.push(error.to_string());
                None
            }
        })
        .collect();
    // Refuse ambiguous Habit identity before the first occurrence claims it.
    let repeated = definitions
        .iter()
        .flat_map(|(_, doc)| &doc.actions)
        .filter_map(|action| (!occupied.insert(action.id)).then_some(action.id))
        .collect();

    let workspace = read_workspace(workspace_root)?;
    let mut apply = CatchUp {
        files,
        scope: PredecessorScope::new(&workspace.charters),
        occupied,
        repeated,
        now,
        budget,
        result,
        effects: Vec::new(),
        written_sidecars: Vec::new(),
    };
    let charter_root = mounts.charter_root();
    for (path, doc) in &definitions {
        let relative = path
            .strip_prefix(&charter_root)
            .expect("inventoried charter path");
        let charter_id = workspace
            .charters
            .iter()
            .find(|charter| charter.habits_file.as_deref() == Some(relative))
            .map(|charter| charter.id);
        let target = actions_anchor(path).expect("classified Habit path");
        validate_source_path(&target, &charter_root)?;
        apply.stage_file(path, doc, &target, charter_id)?;
    }
    if !dry_run && !apply.effects.is_empty() {
        let batch = EffectBatch::new(apply.effects, apply.files.preconditions())
            .map_err(|error| WorkspaceError::Actions(error.to_string()))?;
        // Serialize delivery with other cooperating sidecar writers. Validate
        // revisions after acquiring the locks so a concurrent jot is not lost.
        let _locks = super::sidecar::lock_sidecars(apply.written_sidecars)?;
        deliver(&mounts, &batch)?;
    }
    Ok(apply.result)
}

fn discover(
    mounts: &NativeWorkspaceMounts,
) -> Result<(Vec<PathBuf>, Vec<PathBuf>), WorkspaceError> {
    let inventory = mounts.inventory()?;
    let mut definitions = Vec::new();
    let mut history = Vec::new();
    for path in inventory.workspace.files.paths() {
        let Some((region, relative)) = path.as_str().split_once('/') else {
            continue;
        };
        if !matches!(region, "charters" | "archive")
            || relative.split('/').any(|p| p.starts_with('.'))
        {
            continue;
        }
        let path = Path::new(path.as_str());
        match classify_action_file(path) {
            Some((_, ActionFileRole::Actions | ActionFileRole::Completed)) => {
                history.push(mounts.workspace.join(path))
            }
            Some((_, ActionFileRole::Habits)) if region == "charters" => {
                definitions.push(mounts.workspace.join(path))
            }
            _ => {}
        }
    }
    Ok((definitions, history))
}

struct CatchUp<'a> {
    files: ObservedFiles<'a>,
    scope: PredecessorScope,
    occupied: HashSet<Uuid>,
    repeated: HashSet<Uuid>,
    now: DateTime<FixedOffset>,
    budget: usize,
    result: ApplyHabitsResult,
    effects: Vec<Effect>,
    written_sidecars: Vec<PathBuf>,
}

impl CatchUp<'_> {
    fn stage_file(
        &mut self,
        source: &Path,
        doc: &ParsedDocument,
        target: &Path,
        charter_id: Option<Uuid>,
    ) -> Result<(), WorkspaceError> {
        let metadata_path = sidecar_path(target);
        let mut metadata = self.files.metadata(&metadata_path)?;
        let mut generated = ActionList::new();
        for habit in habits_from_actions(&doc.actions) {
            if habit.definition.state.is_terminal() {
                continue;
            }
            let batch = match self.plan_habit(
                &habit,
                StampSource::new(source, doc),
                source.parent().expect("charter directory"),
                charter_id,
                &metadata,
            ) {
                Ok(batch) => batch,
                Err(error) => {
                    let root = &habit.definition;
                    let line = doc
                        .source_map
                        .get(&root.id)
                        .map_or(1, |item| item.root.start_row + 1);
                    self.result.refused.push(format!(
                        "{}:{line}: could not apply Habit '{}': {error}",
                        source.display(),
                        root.name
                    ));
                    continue;
                }
            };
            self.result.has_backlog |= batch.has_backlog;
            self.result.occurrences += batch.occurrences.len();
            for occurrence in batch.occurrences {
                let tree = occurrence.actions();
                metadata
                    .actions
                    .entry(tree[0].id.to_string())
                    .or_default()
                    .occurrence = tree[0]
                    .native_occurrence
                    .clone()
                    .map(OccurrenceLineage::Native);
                self.occupied.extend(tree.iter().map(|action| action.id));
                generated.extend_from_slice(tree);
            }
        }
        if generated.is_empty() {
            return Ok(());
        }
        self.result.actions += generated.len();
        // Publish the sidecar first. If interrupted, the next run still sees
        // the slot as absent and can publish the whole tree at once.
        self.effects.push(
            self.files
                .write(&metadata_path, render_sidecar(&metadata)?.into_bytes())?,
        );
        let new_rows = format(&generated, OutputFormat::Actions, None, None)
            .map_err(WorkspaceError::Actions)?;
        let mut bytes = self.files.read(target)?.bytes().to_vec();
        if !bytes.is_empty() && !bytes.ends_with(b"\n") {
            bytes.push(b'\n');
        }
        bytes.extend_from_slice(new_rows.as_bytes());
        self.effects.push(self.files.write(target, bytes)?);
        self.written_sidecars.push(metadata_path);
        Ok(())
    }

    fn plan_habit(
        &mut self,
        habit: &Habit,
        source: StampSource<'_>,
        charter_dir: &Path,
        charter_id: Option<Uuid>,
        metadata: &CharterMetadata,
    ) -> Result<CatchUpBatch, WorkspaceError> {
        let root = &habit.definition;
        if self.repeated.contains(&root.id) {
            return Err(WorkspaceError::Actions(format!(
                "Habit UUID {} is also used on another line [duplicate-uuid]",
                root.id
            )));
        }
        let template = if let Some(name) = habit
            .template
            .as_deref()
            .filter(|_| habit.children.is_empty())
        {
            resolve_template(charter_dir, self.files.root, name)?
                .map(|path| self.files.document(&path).map(|doc| (path, doc)))
                .transpose()?
        } else {
            None
        };
        let excluded = match root.planned {
            Some(planned) => text_exclusions(planned.start, metadata.extra.get("habits"), root.id)
                .map_err(WorkspaceError::Actions)?,
            None => HashSet::new(), // Core reports the missing anchor.
        };
        plan_text_catch_up(
            CatchUpInput {
                source,
                habit_id: root.id,
                named_template: template
                    .as_ref()
                    .map(|(path, doc)| StampSource::new(path, doc)),
                scope: &self.scope,
                charter_id,
                occupied: &self.occupied,
                excluded: &excluded,
                budget: self.budget - self.result.occurrences,
            },
            &self.now,
        )
    }
}

/// Snapshot the action, metadata and template bytes used to stage the batch,
/// including read-only history. Revisions are checked before delivery.
struct ObservedFiles<'a> {
    root: &'a Path,
    files: BTreeMap<PathBuf, (ResourceSnapshot, ExpectedResource)>,
}

impl<'a> ObservedFiles<'a> {
    fn new(root: &'a Path) -> Self {
        Self {
            root,
            files: BTreeMap::new(),
        }
    }

    fn read(&mut self, path: &Path) -> Result<&ResourceSnapshot, WorkspaceError> {
        if !self.files.contains_key(path) {
            self.files
                .insert(path.to_owned(), snapshot(self.root, path)?);
        }
        Ok(&self.files[path].0)
    }

    fn document(&mut self, path: &Path) -> Result<ParsedDocument, WorkspaceError> {
        let source =
            std::str::from_utf8(self.read(path)?.bytes()).map_err(|error| at_path(path, error))?;
        parse_actions_with_mode(source, ParseMode::Strict)
            .map(|parsed| parsed.document)
            .map_err(|error| at_path(path, error))
    }

    fn metadata(&mut self, path: &Path) -> Result<CharterMetadata, WorkspaceError> {
        parse_sidecar_snapshot(self.read(path)?).map_err(|error| at_path(path, error))
    }

    fn occupied(&mut self, history: Vec<PathBuf>) -> Result<HashSet<Uuid>, WorkspaceError> {
        let mut occupied = HashSet::new();
        for path in history {
            self.observe_occupied(&path, &mut occupied)?;
        }
        Ok(occupied)
    }

    fn observe_occupied(
        &mut self,
        path: &Path,
        occupied: &mut HashSet<Uuid>,
    ) -> Result<(), WorkspaceError> {
        let doc = self.document(path)?;
        let metadata_path = sidecar_path(path);
        let metadata = self.metadata(&metadata_path)?;
        // A metadata-only entry may be left by an interrupted write. Only
        // real rows (live or archived) prove that a slot was delivered.
        for action in doc.actions {
            occupied.insert(action.id);
            if let Some(OccurrenceLineage::Native(lineage)) = metadata
                .actions
                .get(&action.id.to_string())
                .and_then(|meta| meta.occurrence.as_ref())
            {
                let key: SlotKey = lineage
                    .occurrence_key
                    .parse()
                    .map_err(|error| at_path(&metadata_path, error))?;
                occupied.insert(occurrence_id(lineage.habit_id, &key));
            }
        }
        Ok(())
    }

    fn write(&mut self, path: &Path, bytes: Vec<u8>) -> Result<Effect, WorkspaceError> {
        let path = ResourceLocation::workspace(self.read(path)?.path().clone());
        Ok(Effect::Write { path, bytes })
    }

    fn preconditions(self) -> Vec<ResourcePrecondition> {
        self.files
            .into_values()
            .map(|(snapshot, expected)| ResourcePrecondition {
                path: ResourceLocation::workspace(snapshot.path().clone()),
                expected,
            })
            .collect()
    }
}

fn at_path(path: &Path, error: impl std::fmt::Display) -> WorkspaceError {
    WorkspaceError::Actions(format!("{}: {error}", path.display()))
}
