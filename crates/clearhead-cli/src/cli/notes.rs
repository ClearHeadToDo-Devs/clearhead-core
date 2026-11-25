//! An entity's notes: `show notes` reads them and `jot` appends one,
//! wherever the entity now lives.

use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use anyhow::Context;
use clearhead_core::workspace::sidecar::{Note, notes_across};
use clearhead_core::workspace::{CharterMetadata, WorkspaceRead};
use clearhead_core::{ReferenceEntity, ReferenceSelection, select_reference};
use uuid::Uuid;

use super::CommandContext;
use crate::argparser::JotOptions;

/// An entity that can own notes: every live charter, action and Habit, plus
/// every id a sidecar holds notes for, so closed and archived entities resolve
/// by id without the caller naming their kind.
struct NoteOwner {
    id: Uuid,
    alias: Option<String>,
}

impl ReferenceEntity for NoteOwner {
    fn reference_id(&self) -> Uuid {
        self.id
    }

    fn reference_alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }
}

fn note_owners(read: &WorkspaceRead) -> Vec<NoteOwner> {
    let mut owners: BTreeMap<Uuid, Option<String>> = BTreeMap::new();
    for charter in &read.charters {
        owners.insert(charter.id, charter.alias.clone());
        let habit_actions = charter
            .habits
            .iter()
            .flat_map(|habit| std::iter::once(&habit.definition).chain(&habit.children));
        let actions = charter.actions.iter().map(|sourced| &sourced.action);
        for action in actions.chain(habit_actions) {
            owners.insert(action.id, action.alias.clone());
        }
    }
    let noted = read
        .sidecars
        .values()
        .flat_map(|metadata| metadata.notes.keys());
    for id in noted.filter_map(|id| id.parse().ok()) {
        owners.entry(id).or_insert(None);
    }
    owners
        .into_iter()
        .map(|(id, alias)| NoteOwner { id, alias })
        .collect()
}

fn resolve_owner(owners: &[NoteOwner], query: &str) -> anyhow::Result<Uuid> {
    match select_reference(owners, query) {
        ReferenceSelection::Unique { index, .. } => Ok(owners[index].id),
        ReferenceSelection::Ambiguous { indices, .. } => {
            let candidates: Vec<String> =
                indices.iter().map(|i| owners[*i].id.to_string()).collect();
            anyhow::bail!(
                "Ambiguous reference '{query}'; candidates: {}",
                candidates.join(", ")
            )
        }
        ReferenceSelection::NotFound => anyhow::bail!("No entity found matching '{query}'"),
    }
}

/// Print an entity's notes, oldest first so the newest is last.
pub fn show_notes(ctx: &CommandContext, query: &str, json: bool) -> anyhow::Result<()> {
    let read = clearhead_cli::filesystem::read_workspace_with_archive(&ctx.data_dir)
        .context("Failed to read workspace")?;
    warn_corrupt_sidecars(&read);
    let id = resolve_owner(&note_owners(&read), query)?;
    let notes = notes_across(read.sidecars.values(), id);
    if json {
        println!("{}", serde_json::to_string_pretty(&notes)?);
    } else if notes.is_empty() {
        println!("No notes for {id}");
    } else {
        print!("{}", render_notes(&notes));
    }
    Ok(())
}

/// The active sidecars of every workspace in view, for `show action` and
/// `show charter`, which resolve live entities only.
pub fn active_sidecars(ctx: &CommandContext) -> anyhow::Result<Vec<CharterMetadata>> {
    let mut sidecars = Vec::new();
    for (_, root) in ctx.workspace_dirs() {
        let read = clearhead_cli::filesystem::read_workspace(&root)
            .with_context(|| format!("Failed to read workspace {}", root.display()))?;
        warn_corrupt_sidecars(&read);
        sidecars.extend(read.sidecars.into_values());
    }
    Ok(sidecars)
}

/// An entity's notes for a detail view: a `notes:` block, or nothing.
pub fn notes_section(notes: &[Note]) -> Option<String> {
    (!notes.is_empty()).then(|| {
        let body: String = render_notes(notes)
            .lines()
            .map(|line| format!("  {line}\n"))
            .collect();
        format!("notes:\n{body}")
    })
}

fn warn_corrupt_sidecars(read: &WorkspaceRead) {
    for finding in read.findings.iter().filter(|f| f.code == "sidecar-corrupt") {
        eprintln!(
            "warning: skipped {}: {}",
            finding.path.display(),
            finding.message
        );
    }
}

/// A header line (`at`, then `by` when known) and the text indented below it.
fn render_notes(notes: &[Note]) -> String {
    let mut out = String::new();
    for (index, note) in notes.iter().enumerate() {
        if index > 0 {
            out.push('\n');
        }
        // The same RFC 3339 form the sidecar and --json carry.
        out.push_str(&note.at.to_rfc3339_opts(chrono::SecondsFormat::AutoSi, true));
        if let Some(by) = &note.by {
            out.push_str("  ");
            out.push_str(by);
        }
        out.push('\n');
        for line in note.text.lines() {
            out.push_str("  ");
            out.push_str(line);
            out.push('\n');
        }
    }
    out
}

/// `jot <reference> <text>`: any entity, resolved as `show notes` resolves
/// it; an archived entity with no notes yet resolves by id.
pub fn jot(
    ctx: &CommandContext,
    reference: &str,
    text: &str,
    options: &JotOptions,
) -> anyhow::Result<()> {
    let (read, sidecars) = read_note_sidecars(ctx)?;
    let mut owners = note_owners(&read);
    for id in sidecars.keys().filter_map(|id| id.parse::<Uuid>().ok()) {
        if !owners.iter().any(|owner| owner.id == id) {
            owners.push(NoteOwner { id, alias: None });
        }
    }
    let id = resolve_owner(&owners, reference)?;
    require_durable_id(&read, id)?;
    append(ctx, &sidecars, id, text, options)
}

/// `jot charter <query> <text>`: a live charter, by id, alias or title.
pub fn jot_charter(
    ctx: &CommandContext,
    query: &str,
    text: &str,
    options: &JotOptions,
) -> anyhow::Result<()> {
    let (read, sidecars) = read_note_sidecars(ctx)?;
    let charters: Vec<clearhead_core::Charter> =
        read.charters.iter().cloned().map(Into::into).collect();
    let found = super::charter::resolve_charter(&charters, query)?
        .ok_or_else(|| anyhow::anyhow!("No charter found matching '{query}'"))?;
    require_durable_id(&read, found.id)?;
    append(ctx, &sidecars, found.id, text, options)
}

/// Notes are keyed by id, so the id must be one the files keep: a declared
/// or sidecar charter id, or a written `#id` on an action or Habit line. A
/// note keyed by an id minted at load would be lost on the next read.
fn require_durable_id(read: &WorkspaceRead, id: Uuid) -> anyhow::Result<()> {
    use clearhead_core::workspace::CharterIdSource;
    let unwritten = |name: &str| {
        anyhow::anyhow!(
            "{name} has no durable id, so a note on it would be lost; \
             run `clearhead normalize file` on its file to write one"
        )
    };
    for charter in &read.charters {
        if charter.id == id
            && matches!(
                charter.id_source,
                CharterIdSource::Ephemeral | CharterIdSource::Derived
            )
        {
            return Err(unwritten(&format!("charter '{}'", charter.title)));
        }
        let action = charter
            .actions
            .iter()
            .find(|sourced| sourced.action.id == id)
            .filter(|sourced| {
                sourced
                    .source_metadata
                    .as_ref()
                    .is_some_and(|source| source.is_id_generated)
            });
        if let Some(sourced) = action {
            return Err(unwritten(&format!("action '{}'", sourced.action.name)));
        }
        if charter
            .habit_sources
            .get(&id)
            .is_some_and(|source| source.is_id_generated)
        {
            return Err(unwritten(&format!("Habit line {id}")));
        }
    }
    Ok(())
}

/// The workspace with its archive, and the sidecar each entity's notes
/// belong in, by the rule `doctor` checks.
fn read_note_sidecars(
    ctx: &CommandContext,
) -> anyhow::Result<(WorkspaceRead, HashMap<String, PathBuf>)> {
    let read = clearhead_cli::filesystem::read_workspace_with_archive(&ctx.data_dir)
        .context("Failed to read workspace")?;
    warn_corrupt_sidecars(&read);
    let evidence = clearhead_cli::filesystem::observe_doctor(&ctx.data_dir)
        .context("Failed to read workspace")?;
    let sidecars = clearhead_core::workspace::note_sidecars(&read, &evidence);
    Ok((read, sidecars))
}

fn append(
    ctx: &CommandContext,
    sidecars: &HashMap<String, PathBuf>,
    id: Uuid,
    text: &str,
    options: &JotOptions,
) -> anyhow::Result<()> {
    let relative = sidecars
        .get(&id.to_string())
        .ok_or_else(|| anyhow::anyhow!("'{id}' is stored in no file that can hold notes"))?;
    let note = Note {
        at: options.at.unwrap_or_else(|| {
            chrono::SubsecRound::trunc_subsecs(chrono::Local::now().fixed_offset(), 0)
        }),
        by: (!options.no_by).then(|| note_author(ctx, options.by.as_deref())),
        text: text.to_string(),
        extra: BTreeMap::new(),
    };
    if options.dry_run {
        print!("{}", render_notes(std::slice::from_ref(&note)));
        println!("Would jot to {id} ({})", relative.display());
        return Ok(());
    }
    let path = clearhead_cli::filesystem::workspace_data_root(&ctx.data_dir).join(relative);
    clearhead_cli::filesystem::sidecar::append_note_to(&path, id, note)
        .with_context(|| format!("Failed to jot to {}", path.display()))?;
    println!("Jotted to {id} ({})", relative.display());
    Ok(())
}

/// Who signs a note: `--by`, else the configured `name` (a config file or
/// `CLEARHEAD_NAME`), else the login name.
fn note_author(ctx: &CommandContext, by: Option<&str>) -> String {
    by.map(str::to_string)
        .or_else(|| ctx.config.name.clone())
        .unwrap_or_else(crate::environment_reader::current_username)
}
