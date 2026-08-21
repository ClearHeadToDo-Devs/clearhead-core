//! Pure semantic helpers used by native charter archival.
//!
//! Archive discovery, naming, supporting-file ownership, locking, recovery,
//! moves, cleanup, and result paths are native workspace conventions owned by
//! `clearhead-workspace-fs`. Core retains only lifecycle, hierarchy, reference,
//! and surgical frontmatter decisions that any host can reuse.

use std::collections::{HashMap, HashSet};

use uuid::Uuid;

use crate::domain::Charter;
use crate::workspace::MarkdownCharter;

/// Why a charter cannot participate in an archive operation.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ArchivePolicyError {
    #[error(
        "Charter '{charter}' is not Closed or Cancelled (current state: {state}); set state: Closed or state: Cancelled before archiving"
    )]
    NotTerminal { charter: String, state: String },
    #[error(
        "Charter '{charter}' has {open_actions} open action(s); resolve them or pass --force to archive anyway"
    )]
    OpenActions {
        charter: String,
        open_actions: usize,
    },
}

/// Enforce lifecycle and open-action policy for one archive candidate.
pub fn validate_archive_candidate(
    charter: &MarkdownCharter,
    open_actions: usize,
    force: bool,
) -> Result<(), ArchivePolicyError> {
    let name = archive_charter_name(charter);
    if !charter.state.is_some_and(|state| state.is_terminal()) {
        return Err(ArchivePolicyError::NotTerminal {
            charter: name,
            state: charter
                .state
                .map(|state| state.to_string())
                .unwrap_or_else(|| "New".into()),
        });
    }
    if open_actions > 0 && !force {
        return Err(ArchivePolicyError::OpenActions {
            charter: name,
            open_actions,
        });
    }
    Ok(())
}

/// Stable human label used by archive outcomes and policy errors.
pub fn archive_charter_name(charter: &MarkdownCharter) -> String {
    charter
        .alias
        .clone()
        .unwrap_or_else(|| charter.title.clone())
}

/// Resolve an outbound parent reference to canonical charter identity.
pub fn resolve_archive_parent_uuid(parent: &str, all_charters: &[MarkdownCharter]) -> Option<Uuid> {
    match crate::reference::select_reference(all_charters, parent) {
        crate::reference::ReferenceSelection::Unique { index, .. } => Some(all_charters[index].id),
        crate::reference::ReferenceSelection::NotFound
        | crate::reference::ReferenceSelection::Ambiguous { .. } => None,
    }
}

/// Substitute or insert a top-level `parent: <uuid>` inside leading YAML
/// frontmatter while preserving every unrelated byte.
pub fn materialize_archive_parent(content: &str, parent_uuid: &Uuid) -> Option<String> {
    let desired = format!("parent: {parent_uuid}");
    let body = content.strip_prefix("---\n")?;
    let mut offset = "---\n".len();
    let mut existing = None;
    let mut close_start = None;
    for line in body.split_inclusive('\n') {
        let trimmed = line.strip_suffix('\n').unwrap_or(line);
        if trimmed == "---" {
            close_start = Some(offset);
            break;
        }
        if trimmed.starts_with("parent:") {
            existing = Some((offset, offset + trimmed.len()));
        }
        offset += line.len();
    }
    let close_start = close_start?;
    match existing {
        Some((start, end)) => {
            if content[start..end] == desired {
                None
            } else {
                Some(format!(
                    "{}{}{}",
                    &content[..start],
                    desired,
                    &content[end..]
                ))
            }
        }
        None => Some(format!(
            "{}{}\n{}",
            &content[..close_start],
            desired,
            &content[close_start..]
        )),
    }
}

fn markdown_is_child_of(child: &MarkdownCharter, parent: &MarkdownCharter) -> bool {
    Charter::from(child.clone()).is_child_of(&Charter::from(parent.clone()))
}

fn direct_children<'a>(
    parent: &MarkdownCharter,
    charters: &'a [MarkdownCharter],
) -> impl Iterator<Item = &'a MarkdownCharter> {
    charters
        .iter()
        .filter(|candidate| candidate.id != parent.id && markdown_is_child_of(candidate, parent))
}

/// Collect one charter and all semantic descendants, parent before children.
pub fn archive_charter_subtree(
    charters: &[MarkdownCharter],
    root: &MarkdownCharter,
) -> Vec<MarkdownCharter> {
    fn visit(
        node: &MarkdownCharter,
        all: &[MarkdownCharter],
        seen: &mut HashSet<Uuid>,
        out: &mut Vec<MarkdownCharter>,
    ) {
        if !seen.insert(node.id) {
            return;
        }
        out.push(node.clone());
        for child in direct_children(node, all) {
            visit(child, all, seen, out);
        }
    }

    let mut out = Vec::new();
    visit(root, charters, &mut HashSet::new(), &mut out);
    out
}

/// Whether a charter already belongs to a subtree rooted at a terminal
/// ancestor, preventing duplicate archive sweeps.
pub fn has_terminal_archive_ancestor(
    charter: &MarkdownCharter,
    charters: &[MarkdownCharter],
) -> bool {
    let mut parents = HashMap::new();
    for child in charters {
        if let Some(parent) = charters
            .iter()
            .find(|candidate| candidate.id != child.id && markdown_is_child_of(child, candidate))
        {
            parents.insert(child.id, parent.id);
        }
    }
    let by_id: HashMap<Uuid, &MarkdownCharter> = charters
        .iter()
        .map(|candidate| (candidate.id, candidate))
        .collect();
    let mut current = charter.id;
    while let Some(parent_id) = parents.get(&current) {
        let Some(parent) = by_id.get(parent_id) else {
            break;
        };
        if parent.state.is_some_and(|state| state.is_terminal()) {
            return true;
        }
        current = *parent_id;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::CharterState;
    use crate::workspace::charter::implicit_charter;

    fn charter(alias: &str, state: Option<CharterState>) -> MarkdownCharter {
        let mut charter = implicit_charter(alias);
        charter.state = state;
        MarkdownCharter::from(charter)
    }

    #[test]
    fn archive_policy_requires_terminal_state_and_explicit_force() {
        let active = charter("active", Some(CharterState::Active));
        assert!(matches!(
            validate_archive_candidate(&active, 0, false),
            Err(ArchivePolicyError::NotTerminal { .. })
        ));
        let closed = charter("closed", Some(CharterState::Closed));
        assert!(matches!(
            validate_archive_candidate(&closed, 1, false),
            Err(ArchivePolicyError::OpenActions { .. })
        ));
        assert!(validate_archive_candidate(&closed, 1, true).is_ok());
    }

    #[test]
    fn parent_materialization_is_surgical() {
        let id = Uuid::nil();
        assert_eq!(
            materialize_archive_parent("---\nalias: child\n---\n# Child\n", &id).unwrap(),
            "---\nalias: child\nparent: 00000000-0000-0000-0000-000000000000\n---\n# Child\n"
        );
        assert!(materialize_archive_parent("# no frontmatter\n", &id).is_none());
    }
}
