//! `show notes`: one entity's notes, wherever the entity now lives.

use std::collections::BTreeMap;

use anyhow::Context;
use clearhead_core::workspace::WorkspaceRead;
use clearhead_core::workspace::sidecar::{Note, notes_across};
use clearhead_core::{ReferenceEntity, ReferenceSelection, select_reference};
use uuid::Uuid;

use super::CommandContext;

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
    for finding in read.findings.iter().filter(|f| f.code == "sidecar-corrupt") {
        eprintln!(
            "warning: skipped {}: {}",
            finding.path.display(),
            finding.message
        );
    }
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
