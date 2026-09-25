//! `clearhead orient` — a bounded, agent-shaped snapshot of the workspace.
//!
//! Answers "what should I be doing, and what's in the way" in one call:
//! active charters, the unscheduled queue, blockers, and recent completions.
//! Every section is bounded with an explicit omission count rather than
//! growing unbounded — output discipline agent-workspace usage motivated
//! (see the agent-surface charter). This is deliberately a plain JSON
//! document, not the index family's `@context`/`@graph` framing: it composes
//! rows from several different sources into one cross-cutting view, not one
//! query's result.

use std::io::IsTerminal;

use anyhow::Context;
use serde::Serialize;
use serde_json::Value;

use super::CommandContext;
use super::verb_result::canonical_id;
use clearhead_core::{ActionState, CharterState};

/// Cap applied independently to each section. Agent-surface's own design
/// note is explicit that every tool schema costs context in every session —
/// this keeps `orient` a snapshot, not a dump.
const SECTION_LIMIT: usize = 10;

#[derive(Serialize)]
pub struct Orient {
    pub active_charters: Bounded<CharterSummary>,
    pub unscheduled: Bounded<Value>,
    pub blockers: Bounded<BlockerEntry>,
    pub recent_completions: Bounded<CompletionEntry>,
}

#[derive(Serialize)]
pub struct Bounded<T> {
    pub items: Vec<T>,
    pub omitted: usize,
}

impl<T> Bounded<T> {
    fn take(mut items: Vec<T>, limit: usize) -> Self {
        let omitted = items.len().saturating_sub(limit);
        items.truncate(limit);
        Self { items, omitted }
    }
}

#[derive(Serialize)]
pub struct CharterSummary {
    /// Absent when the charter's document declares no id.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    pub title: String,
    pub alias: Option<String>,
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub enum BlockerEntry {
    Charter {
        #[serde(skip_serializing_if = "Option::is_none")]
        id: Option<String>,
        title: String,
    },
    Action {
        id: String,
        name: String,
        charter: String,
    },
}

#[derive(Serialize)]
pub struct CompletionEntry {
    pub id: String,
    pub name: String,
    pub state: ActionState,
    pub completed_at: Option<String>,
}

/// A charter's canonical id, unless it is only an in-process join key.
fn published_id(charter: &clearhead_core::MarkdownCharter) -> Option<String> {
    (charter.id_source == clearhead_core::workspace::CharterIdSource::Document)
        .then(|| canonical_id(charter.id))
}

pub fn build(ctx: &CommandContext) -> anyhow::Result<Orient> {
    let charters = clearhead_cli::filesystem::load_workspace(&ctx.data_dir).context("orient")?;
    let charter_root = clearhead_cli::filesystem::charter_root(&ctx.data_dir);

    let active_charters = Bounded::take(
        charters
            .iter()
            .filter(|charter| charter.state == Some(CharterState::Active))
            .map(|charter| CharterSummary {
                id: published_id(charter),
                title: charter.title.clone(),
                alias: charter.alias.clone(),
            })
            .collect(),
        SECTION_LIMIT,
    );

    let mut blockers = Vec::new();
    let mut completions = Vec::new();
    for charter in &charters {
        if charter.state == Some(CharterState::Blocked) {
            blockers.push(BlockerEntry::Charter {
                id: published_id(charter),
                title: charter.title.clone(),
            });
        }
        for sourced in &charter.actions {
            if sourced.action.state == ActionState::BlockedOrAwaiting {
                blockers.push(BlockerEntry::Action {
                    id: canonical_id(sourced.action.id),
                    name: sourced.action.name.clone(),
                    charter: charter.title.clone(),
                });
            }
        }

        let Some(actions_file) = &charter.actions_file else {
            continue;
        };
        let completed_path = clearhead_cli::filesystem::action_files::completed_actions_path(
            &charter_root.join(actions_file),
        );
        for action in clearhead_cli::filesystem::action_files::read_actions(&completed_path)? {
            completions.push(action);
        }
    }
    completions.sort_by_key(|action| std::cmp::Reverse(action.completed_at));

    let recent_completions = Bounded::take(
        completions
            .into_iter()
            .map(|action| CompletionEntry {
                id: canonical_id(action.id),
                name: action.name,
                state: action.state,
                completed_at: action.completed_at.map(|when| when.to_rfc3339()),
            })
            .collect(),
        SECTION_LIMIT,
    );

    Ok(Orient {
        active_charters,
        unscheduled: unscheduled_rows(ctx)?,
        blockers: Bounded::take(blockers, SECTION_LIMIT),
        recent_completions,
    })
}

#[cfg(feature = "sparql")]
fn unscheduled_rows(ctx: &CommandContext) -> anyhow::Result<Bounded<Value>> {
    let nodes = crate::query::sparql::index::nodes_for(ctx, "unscheduled")?;
    Ok(Bounded::take(nodes, SECTION_LIMIT))
}

#[cfg(not(feature = "sparql"))]
fn unscheduled_rows(_ctx: &CommandContext) -> anyhow::Result<Bounded<Value>> {
    // This build has no query engine; every other section is native and
    // still meaningful, so orient degrades rather than failing outright.
    Ok(Bounded {
        items: Vec::new(),
        omitted: 0,
    })
}

pub fn run(ctx: &CommandContext) -> anyhow::Result<()> {
    let orient = build(ctx)?;
    if std::io::stdout().is_terminal() {
        print_report(&orient);
    } else {
        println!("{}", serde_json::to_string_pretty(&orient)?);
    }
    Ok(())
}

fn print_report(orient: &Orient) {
    println!(
        "{} active charter(s) ({} omitted)",
        orient.active_charters.items.len(),
        orient.active_charters.omitted
    );
    for charter in &orient.active_charters.items {
        println!(
            "  - {} [{}]",
            charter.title,
            charter.alias.as_deref().unwrap_or("no alias")
        );
    }

    println!(
        "\n{} unscheduled action(s) ({} omitted)",
        orient.unscheduled.items.len(),
        orient.unscheduled.omitted
    );
    for row in &orient.unscheduled.items {
        let name = row.get("name").and_then(Value::as_str).unwrap_or("?");
        println!("  - {}", name);
    }

    println!(
        "\n{} blocker(s) ({} omitted)",
        orient.blockers.items.len(),
        orient.blockers.omitted
    );
    for blocker in &orient.blockers.items {
        match blocker {
            BlockerEntry::Charter { title, .. } => println!("  - charter blocked: {}", title),
            BlockerEntry::Action { name, charter, .. } => {
                println!("  - action blocked: {} ({})", name, charter)
            }
        }
    }

    println!(
        "\n{} recent completion(s) ({} omitted)",
        orient.recent_completions.items.len(),
        orient.recent_completions.omitted
    );
    for completion in &orient.recent_completions.items {
        println!("  - {}", completion.name);
    }
}
