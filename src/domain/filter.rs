use super::{Action, ActionState, DomainModel};

/// Criteria for selecting a subset of [`Action`]s from a domain model.
///
/// All fields are additive — every non-default constraint must pass.
/// An all-default `ActionFilter` passes every action.
///
/// `context_tags` must be **pre-expanded** by the caller (e.g. via
/// [`WorkspaceConfig::expand_tag`]) so that this struct stays free of
/// workspace configuration concerns.
#[derive(Debug, Clone, Default)]
pub struct ActionFilter {
    /// Exclude [`ActionState::Completed`] and [`ActionState::Cancelled`] actions.
    pub open_only: bool,
    /// Require at least one of these tags to appear in `action.contexts`.
    /// Empty means no context filter.
    pub context_tags: Vec<String>,
    /// Restrict to actions whose `plan_id` matches this UUID string or 8-char prefix.
    pub plan_ref: Option<String>,
}

impl ActionFilter {
    pub fn matches(&self, action: &Action) -> bool {
        if self.open_only
            && matches!(action.state, ActionState::Completed | ActionState::Cancelled)
        {
            return false;
        }

        if !self.context_tags.is_empty() {
            let hit = action.contexts.as_ref().map_or(false, |cs| {
                cs.iter()
                    .any(|c| self.context_tags.contains(c))
            });
            if !hit {
                return false;
            }
        }

        if let Some(plan_ref) = &self.plan_ref {
            let matched = action.plan_id.map_or(false, |pid| {
                let full = pid.to_string();
                let short = full.replace('-', "");
                full == *plan_ref
                    || short.starts_with(plan_ref.as_str())
            });
            if !matched {
                return false;
            }
        }

        true
    }
}

/// Retain only actions matching `filter` in every charter of `model`.
///
/// Plans are left intact — an empty plan node in the tree signals that all
/// its actions have been filtered out, which is useful context for the viewer.
pub fn apply_filter(model: &mut DomainModel, filter: &ActionFilter) {
    for charter in &mut model.charters {
        charter.actions.retain(|a| filter.matches(a));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{Action, ActionState, Charter, DomainModel};
    use uuid::Uuid;

    fn action(state: ActionState) -> Action {
        Action { id: Uuid::now_v7(), state, ..Default::default() }
    }

    fn action_with_contexts(contexts: Vec<&str>) -> Action {
        Action {
            id: Uuid::now_v7(),
            contexts: Some(contexts.into_iter().map(String::from).collect()),
            ..Default::default()
        }
    }

    fn model_with(actions: Vec<Action>) -> DomainModel {
        DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                id: Uuid::now_v7(),
                title: "Test".into(),
                actions,
                ..Default::default()
            }],
        }
    }

    #[test]
    fn empty_filter_passes_everything() {
        let filter = ActionFilter::default();
        let acts = vec![
            action(ActionState::NotStarted),
            action(ActionState::Completed),
            action(ActionState::Cancelled),
        ];
        assert!(acts.iter().all(|a| filter.matches(a)));
    }

    #[test]
    fn open_only_excludes_terminal_states() {
        let filter = ActionFilter { open_only: true, ..Default::default() };
        assert!(filter.matches(&action(ActionState::NotStarted)));
        assert!(filter.matches(&action(ActionState::InProgress)));
        assert!(filter.matches(&action(ActionState::BlockedOrAwaiting)));
        assert!(!filter.matches(&action(ActionState::Completed)));
        assert!(!filter.matches(&action(ActionState::Cancelled)));
    }

    #[test]
    fn context_filter_requires_overlap() {
        let filter = ActionFilter {
            context_tags: vec!["computer".into()],
            ..Default::default()
        };
        assert!(filter.matches(&action_with_contexts(vec!["computer"])));
        assert!(filter.matches(&action_with_contexts(vec!["work", "computer"])));
        assert!(!filter.matches(&action_with_contexts(vec!["work"])));
        assert!(!filter.matches(&action(ActionState::NotStarted))); // no contexts
    }

    #[test]
    fn empty_context_tags_passes_all() {
        let filter = ActionFilter { context_tags: vec![], ..Default::default() };
        assert!(filter.matches(&action(ActionState::NotStarted)));
        assert!(filter.matches(&action_with_contexts(vec!["anything"])));
    }

    #[test]
    fn plan_ref_matches_full_uuid() {
        let plan_id = Uuid::now_v7();
        let mut act = action(ActionState::NotStarted);
        act.plan_id = Some(plan_id);

        let filter = ActionFilter {
            plan_ref: Some(plan_id.to_string()),
            ..Default::default()
        };
        assert!(filter.matches(&act));
        assert!(!filter.matches(&action(ActionState::NotStarted))); // no plan_id
    }

    #[test]
    fn plan_ref_matches_short_prefix() {
        let plan_id = Uuid::now_v7();
        let short = plan_id.to_string().replace('-', "")[..8].to_string();
        let mut act = action(ActionState::NotStarted);
        act.plan_id = Some(plan_id);

        let filter = ActionFilter { plan_ref: Some(short), ..Default::default() };
        assert!(filter.matches(&act));
    }

    #[test]
    fn apply_filter_mutates_model() {
        let mut model = model_with(vec![
            action(ActionState::NotStarted),
            action(ActionState::Completed),
            action(ActionState::InProgress),
        ]);
        let filter = ActionFilter { open_only: true, ..Default::default() };
        apply_filter(&mut model, &filter);

        let remaining: Vec<_> = model.charters[0].actions.iter().map(|a| a.state).collect();
        assert_eq!(remaining, vec![ActionState::NotStarted, ActionState::InProgress]);
    }

    #[test]
    fn apply_filter_across_multiple_charters() {
        let mut model = DomainModel {
            objectives: vec![],
            charters: vec![
                Charter {
                    id: Uuid::now_v7(),
                    title: "A".into(),
                    actions: vec![action(ActionState::Completed), action(ActionState::NotStarted)],
                    ..Default::default()
                },
                Charter {
                    id: Uuid::now_v7(),
                    title: "B".into(),
                    actions: vec![action(ActionState::Cancelled)],
                    ..Default::default()
                },
            ],
        };
        apply_filter(&mut model, &ActionFilter { open_only: true, ..Default::default() });

        assert_eq!(model.charters[0].actions.len(), 1);
        assert_eq!(model.charters[1].actions.len(), 0);
    }
}
