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

use std::collections::HashMap;

use clearhead_core::domain::ActionState;
use uuid::Uuid;

pub(super) fn action_state_str(state: ActionState) -> &'static str {
    match state {
        ActionState::NotStarted => "not started",
        ActionState::InProgress => "in progress",
        ActionState::Completed => "done",
        ActionState::BlockedOrAwaiting => "blocked",
        ActionState::Cancelled => "cancelled",
    }
}

/// Map each id to the shortest hex prefix (never under 8 digits) that
/// identifies it uniquely within `ids`. UUIDv7 ids minted within about a
/// minute share their first 8 hex digits, so a fixed-width slice is ambiguous.
///
/// After sorting, an id's longest shared prefix with any other id is always
/// with a sorted neighbour: every id between two others shares at least their
/// common prefix. So comparing neighbours is enough.
pub(crate) fn unique_short_ids(ids: &[Uuid]) -> HashMap<Uuid, String> {
    const FLOOR: usize = 8;

    let mut hexes: Vec<(String, Uuid)> = ids
        .iter()
        .map(|id| (id.simple().to_string(), *id))
        .collect();
    hexes.sort();
    hexes.dedup();

    let shared = |a: &str, b: &str| a.bytes().zip(b.bytes()).take_while(|(x, y)| x == y).count();

    hexes
        .iter()
        .enumerate()
        .map(|(i, (hex, id))| {
            let before = i.checked_sub(1).map_or(0, |j| shared(hex, &hexes[j].0));
            let after = hexes.get(i + 1).map_or(0, |(next, _)| shared(hex, next));
            let len = (before.max(after) + 1).clamp(FLOOR, hex.len());
            (*id, hex[..len].to_string())
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn id(s: &str) -> Uuid {
        Uuid::parse_str(s).unwrap()
    }

    #[test]
    fn short_ids_stay_at_the_floor_when_distinct() {
        let a = id("01a0a25d-6197-79f0-a91c-113e3a4b3665");
        let b = id("ffa0a25d-86b0-7640-b315-254ef6fa76b5");
        let short = unique_short_ids(&[a, b]);
        assert_eq!(short[&a], "01a0a25d");
        assert_eq!(short[&b], "ffa0a25d");
    }

    #[test]
    fn short_ids_extend_past_a_shared_timestamp_prefix() {
        // Real ids from one `add` burst: all share their first 8 hex digits.
        let ids = [
            id("01a0a25d-86b0-7640-b315-254ef6fa76b5"),
            id("01a0a25d-86be-7d20-bead-8c865af74d6f"),
            id("01a0a25d-6197-79f0-a91c-113e3a4b3665"),
        ];
        let short = unique_short_ids(&ids);
        assert_eq!(short[&ids[0]], "01a0a25d86b0");
        assert_eq!(short[&ids[1]], "01a0a25d86be");
        assert_eq!(short[&ids[2]], "01a0a25d6");

        for prefix in short.values() {
            let matches = ids
                .iter()
                .filter(|id| id.simple().to_string().starts_with(prefix.as_str()))
                .count();
            assert_eq!(matches, 1, "{prefix} should resolve to exactly one id");
        }
    }

    #[test]
    fn short_ids_tolerate_duplicates() {
        let a = id("01a0a25d-6197-79f0-a91c-113e3a4b3665");
        assert_eq!(unique_short_ids(&[a, a])[&a], "01a0a25d");
    }
}
