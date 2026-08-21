use clearhead_core::{Action, ActionState, DomainModel};

/// Check if an Action should be included in calendar export.
///
/// VTODO can represent actions without DTSTART or DUE, so only lifecycle state
/// affects inclusion. When `open_only`, completed and cancelled actions are excluded.
pub fn should_include_action(action: &Action, open_only: bool) -> bool {
    if open_only {
        matches!(
            action.state,
            ActionState::NotStarted | ActionState::InProgress | ActionState::BlockedOrAwaiting
        )
    } else {
        true
    }
}

/// Convert a [`DomainModel`] to an iCalendar string.
///
/// Collects all actions from all charters and delegates to
/// [`clearhead_core::actions_to_icalendar`]. Each action becomes one individual
/// VTODO — no RRULE master components.
pub fn format_as_icalendar(model: &DomainModel, open_only: bool) -> Result<String, String> {
    let actions: Vec<Action> = model
        .charters
        .iter()
        .flat_map(|c| c.actions.iter().cloned())
        .collect();
    Ok(clearhead_core::actions_to_icalendar(&actions, open_only))
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{Local, TimeZone};
    use uuid::Uuid;

    fn make_action(state: ActionState, scheduled_at: Option<chrono::DateTime<Local>>) -> Action {
        Action {
            id: Uuid::new_v4(),
            state,
            scheduled_at,
            ..Default::default()
        }
    }

    #[test]
    fn should_include_action_without_scheduled_at() {
        let action = make_action(ActionState::NotStarted, None);
        assert!(should_include_action(&action, false));
        assert!(should_include_action(&action, true));
    }

    #[test]
    fn should_include_action_open_only_excludes_terminal_states() {
        let dt = Local.with_ymd_and_hms(2026, 1, 10, 14, 0, 0).unwrap();
        let completed = make_action(ActionState::Completed, Some(dt));
        let cancelled = make_action(ActionState::Cancelled, Some(dt));
        let open = make_action(ActionState::NotStarted, Some(dt));
        assert!(!should_include_action(&completed, true));
        assert!(!should_include_action(&cancelled, true));
        assert!(should_include_action(&open, true));
    }

    #[test]
    fn should_include_action_all_states_when_not_open_only() {
        let dt = Local.with_ymd_and_hms(2026, 1, 10, 14, 0, 0).unwrap();
        let completed = make_action(ActionState::Completed, Some(dt));
        assert!(should_include_action(&completed, false));
    }

    #[test]
    fn format_as_icalendar_empty_model() {
        let model = DomainModel {
            objectives: vec![],
            charters: vec![],
        };
        let ics = format_as_icalendar(&model, false).unwrap();
        assert!(ics.contains("BEGIN:VCALENDAR"));
        assert!(!ics.contains("BEGIN:VTODO"));
    }

    #[test]
    fn format_as_icalendar_action_produces_vtodo() {
        use clearhead_core::domain::Charter;
        let dt = Local.with_ymd_and_hms(2026, 1, 10, 14, 0, 0).unwrap();
        let action = Action {
            id: Uuid::new_v4(),
            name: "Test task".to_string(),
            state: ActionState::NotStarted,
            scheduled_at: Some(dt),
            ..Default::default()
        };
        let model = DomainModel {
            objectives: vec![],
            charters: vec![Charter {
                actions: vec![action],
                ..Default::default()
            }],
        };
        let ics = format_as_icalendar(&model, false).unwrap();
        assert!(ics.contains("BEGIN:VTODO"));
        assert!(ics.contains("Test task"));
        assert!(!ics.contains("RRULE"));
    }
}
