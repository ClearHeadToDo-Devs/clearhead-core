//! A verb-neutral handoff from client-side resolution to core's locked read.
//!
//! Every durable action verb resolves *which* action it acts on outside the
//! workspace lock (fuzzy prefix/alias/name matching is a client UX concern),
//! then re-resolves it under the lock against freshly-read, trusted state. The
//! [`ActionSelector`] carries just enough to make that second resolution stable:
//! the inline UUID when there is one, plus alias/name fallbacks so an id-less
//! plaintext line — whose in-memory UUID is regenerated on each parse — still
//! resolves to the same action after the reload.
//!
//! It is deliberately verb-neutral: a close resolves its target, an update its
//! target, an add its parent, a delete its target — all through the same type
//! and the same [`unique_selector_match`].

use uuid::Uuid;

use crate::workspace::actions::Action;
use crate::workspace::store::WorkspaceError;

/// A stable-enough reference to one action across a locked reload.
///
/// Inline UUID is preferred; alias and name let legacy id-less lines survive the
/// second parse without making the client own the filesystem mutation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionSelector {
    pub id: Uuid,
    pub alias: Option<String>,
    pub name: String,
}

impl From<&Action> for ActionSelector {
    fn from(action: &Action) -> Self {
        Self {
            id: action.id,
            alias: action.alias.clone(),
            name: action.name.clone(),
        }
    }
}

/// Resolve a selector to a unique action id against a freshly-read list.
///
/// UUID is authoritative. When it misses — an id-less line reparsed to a new
/// in-memory UUID — a unique alias match wins, then a unique name match.
/// Ambiguity is rejected rather than silently selecting by file order.
pub(crate) fn unique_selector_match(
    actions: &[Action],
    selector: &ActionSelector,
) -> Result<Option<Uuid>, WorkspaceError> {
    if actions.iter().any(|action| action.id == selector.id) {
        return Ok(Some(selector.id));
    }

    let unique = |matches: Vec<Uuid>, field: &str| match matches.as_slice() {
        [] => Ok(None),
        [id] => Ok(Some(*id)),
        _ => Err(WorkspaceError::Actions(format!(
            "action selector {field} is ambiguous after locked reload: {}",
            selector.id
        ))),
    };

    if let Some(alias) = &selector.alias
        && let Some(id) = unique(
            actions
                .iter()
                .filter(|action| action.alias.as_ref() == Some(alias))
                .map(|action| action.id)
                .collect(),
            "alias",
        )?
    {
        return Ok(Some(id));
    }

    unique(
        actions
            .iter()
            .filter(|action| action.name == selector.name)
            .map(|action| action.id)
            .collect(),
        "name",
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::ActionState;

    fn action(name: &str, alias: Option<&str>) -> Action {
        Action {
            id: Uuid::now_v7(),
            name: name.to_string(),
            alias: alias.map(str::to_string),
            state: ActionState::NotStarted,
            ..Default::default()
        }
    }

    #[test]
    fn uuid_hit_wins_before_any_fallback() {
        let target = action("shared name", None);
        // A same-name decoy would make a name fallback ambiguous; the UUID hit
        // must short-circuit past it.
        let decoy = action("shared name", None);
        let list = vec![target.clone(), decoy];

        let selector = ActionSelector::from(&target);
        assert_eq!(
            unique_selector_match(&list, &selector).unwrap(),
            Some(target.id)
        );
    }

    #[test]
    fn falls_back_to_alias_then_name_when_uuid_missed() {
        let by_alias = action("A", Some("keystone"));
        let by_name = action("distinct", None);
        let list = vec![by_alias.clone(), by_name.clone()];

        // Selector id is absent from the list (simulating an id-less reparse),
        // so resolution walks alias then name.
        let alias_sel = ActionSelector {
            id: Uuid::now_v7(),
            alias: Some("keystone".into()),
            name: "wrong".into(),
        };
        assert_eq!(
            unique_selector_match(&list, &alias_sel).unwrap(),
            Some(by_alias.id)
        );

        let name_sel = ActionSelector {
            id: Uuid::now_v7(),
            alias: None,
            name: "distinct".into(),
        };
        assert_eq!(
            unique_selector_match(&list, &name_sel).unwrap(),
            Some(by_name.id)
        );
    }

    #[test]
    fn ambiguous_name_is_rejected_not_guessed() {
        let one = action("dup", None);
        let two = action("dup", None);
        let list = vec![one, two];

        let sel = ActionSelector {
            id: Uuid::now_v7(),
            alias: None,
            name: "dup".into(),
        };
        assert!(unique_selector_match(&list, &sel).is_err());
    }

    #[test]
    fn no_match_is_none_not_error() {
        let list = vec![action("here", None)];
        let sel = ActionSelector {
            id: Uuid::now_v7(),
            alias: None,
            name: "absent".into(),
        };
        assert_eq!(unique_selector_match(&list, &sel).unwrap(), None);
    }
}
