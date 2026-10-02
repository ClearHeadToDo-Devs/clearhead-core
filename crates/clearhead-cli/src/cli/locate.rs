//! `clearhead locate` — where each identity is stored.
//!
//! The RDF graph holds the work, not where it is kept (platform Decision 44),
//! so a client that needs a location asks here: ids in, as arguments or one
//! per line on stdin (`clearhead query named X --format ids | clearhead
//! locate`), one JSON array out in input order. Locations come from the same
//! host evidence the workspace-snapshot projection uses, across every
//! configured workspace. An unknown id is reported, not fatal.

use std::collections::HashMap;
use std::io::{IsTerminal, Read};
use std::path::PathBuf;

use anyhow::Context;
use serde::Serialize;
use uuid::Uuid;

use super::CommandContext;

#[derive(Serialize)]
pub struct Location {
    pub id: String,
    /// `action` or `charter`; absent when nothing stored has this id.
    pub kind: Option<&'static str>,
    /// Absolute path of the file holding it.
    pub file: Option<PathBuf>,
    /// 1-based line; a charter document is located at its first line.
    pub line: Option<u32>,
}

pub fn run(ctx: &CommandContext, ids: &[String]) -> anyhow::Result<()> {
    let requested = if ids.is_empty() {
        read_stdin_ids()?
    } else {
        ids.to_vec()
    };
    let index = index_locations(ctx)?;
    let locations: Vec<Location> = requested
        .iter()
        .map(|raw| {
            let found = parse_id(raw).and_then(|id| index.get(&id));
            if found.is_none() {
                eprintln!("warning: nothing stored has id '{raw}'");
            }
            Location {
                id: raw.clone(),
                kind: found.map(|(kind, _, _)| *kind),
                file: found.map(|(_, file, _)| file.clone()),
                line: found.map(|(_, _, line)| *line),
            }
        })
        .collect();
    println!("{}", serde_json::to_string_pretty(&locations)?);
    Ok(())
}

fn read_stdin_ids() -> anyhow::Result<Vec<String>> {
    let mut stdin = std::io::stdin();
    if stdin.is_terminal() {
        anyhow::bail!("give ids as arguments, or pipe them in one per line");
    }
    let mut input = String::new();
    stdin
        .read_to_string(&mut input)
        .context("Failed to read ids from stdin")?;
    Ok(input
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(str::to_string)
        .collect())
}

/// A full UUID, bare or as a `urn:uuid:` IRI, the forms query output carries.
fn parse_id(raw: &str) -> Option<Uuid> {
    Uuid::parse_str(raw.trim().trim_start_matches("urn:uuid:")).ok()
}

type Found = (&'static str, PathBuf, u32);

fn index_locations(ctx: &CommandContext) -> anyhow::Result<HashMap<Uuid, Found>> {
    let mut index = HashMap::new();
    for (_name, path) in ctx.workspace_dirs() {
        let workspace = match clearhead_cli::filesystem::load_workspace_model(&path) {
            Ok(workspace) => workspace,
            Err(error) if path == ctx.data_dir => {
                return Err(error).context("Failed to load workspace");
            }
            Err(error) => {
                tracing::warn!("Skipping workspace '{}': {error}", path.display());
                continue;
            }
        };
        let snapshot = crate::query::dataset::workspace_snapshot(&workspace);
        let charter_root = PathBuf::from(&snapshot.charter_root);
        for (id, file) in snapshot.charter_files {
            index
                .entry(id)
                .or_insert(("charter", charter_root.join(file), 1));
        }
        for (id, file, line) in snapshot.action_sources {
            index
                .entry(id)
                .or_insert(("action", charter_root.join(file), line));
        }
    }
    Ok(index)
}

#[cfg(test)]
mod tests {
    use super::parse_id;

    #[test]
    fn ids_parse_bare_or_as_urns_and_reject_short_forms() {
        let id = "01a0fb10-0000-7000-8000-000000000001";
        assert_eq!(parse_id(id).unwrap().to_string(), id);
        assert_eq!(parse_id(&format!("urn:uuid:{id}")).unwrap().to_string(), id);
        assert!(parse_id("01a0fb10").is_none());
    }
}
