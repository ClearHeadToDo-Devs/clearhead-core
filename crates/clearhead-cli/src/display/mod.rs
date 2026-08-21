//! Display module — visual rendering for TTY output.
//!
//! All functions here are TTY-only. They consume `DomainModel` or other core
//! types directly and produce human-readable strings. Nothing here belongs in
//! `clearhead-core`; the library has no business knowing about terminal display.
//!
//! When stdout is a pipe, callers should emit JSON-LD instead of calling these.

pub mod detail;
pub mod tree;

pub use detail::{render_action_detail, render_charter_detail, render_plan_detail};
pub use tree::{render_charter_tree, render_domain_tree};

use clearhead_core::domain::ActionState;

pub(super) fn action_state_str(state: ActionState) -> &'static str {
    match state {
        ActionState::NotStarted => "not started",
        ActionState::InProgress => "in progress",
        ActionState::Completed => "done",
        ActionState::BlockedOrAwaiting => "blocked",
        ActionState::Cancelled => "cancelled",
    }
}
