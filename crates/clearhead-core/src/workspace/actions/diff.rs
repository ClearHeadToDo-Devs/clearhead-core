use super::{Action, ActionList, ActionState};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::collections::HashSet;
use uuid::Uuid;

#[derive(Debug, PartialEq, Clone)]
pub struct SemanticMerge {
    pub merged: ActionList,
    pub conflicts: Vec<Conflict>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Conflict {
    FieldConflict(FieldConflict),
    ActionDeleted(ActionDeleted),
    BothAdded(BothAdded),
    ParentCycle(ParentCycle),
    OrderConflict(OrderConflict),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldConflict {
    pub id: Uuid,
    pub fields: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ActionDeleted {
    pub id: Uuid,
    pub deleted_by: Side,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Side {
    Ours,
    Theirs,
}

impl Side {
    fn choose<T>(self, ours: T, theirs: T) -> T {
        match self {
            Self::Ours => ours,
            Self::Theirs => theirs,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BothAdded {
    pub id: Uuid,
}

/// Merged moves put these actions under their own descendants; their moves were undone.
#[derive(Debug, PartialEq, Clone)]
pub struct ParentCycle {
    pub ids: Vec<Uuid>,
}

/// Both sides reordered this parent's children differently; ours' order is the placeholder.
#[derive(Debug, PartialEq, Clone)]
pub struct OrderConflict {
    pub parent: Option<Uuid>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ActionDiff {
    pub id: Uuid,
    pub changes: Vec<FieldChange>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldChange {
    State {
        old: ActionState,
        new: ActionState,
    },
    Name {
        old: String,
        new: String,
    },
    Description {
        old: Option<String>,
        new: Option<String>,
    },
    Priority {
        old: Option<u32>,
        new: Option<u32>,
    },
    DoDate {
        old: Option<String>,
        new: Option<String>,
    },
    CompletedDate {
        old: Option<String>,
        new: Option<String>,
    },
    // We can add more fields as needed
    Generic {
        field: String,
        old: Value,
        new: Value,
    },
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Diff {
    pub added: Vec<Action>,
    pub removed: Vec<Action>,
    pub modified: Vec<ActionDiff>,
}

impl Diff {
    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty() && self.modified.is_empty()
    }
}

pub fn diff_actions(old_actions: &ActionList, new_actions: &ActionList) -> Diff {
    let mut diff = Diff::default();
    let old_map: HashMap<Uuid, &Action> = old_actions.iter().map(|a| (a.id, a)).collect();
    let new_map: HashMap<Uuid, &Action> = new_actions.iter().map(|a| (a.id, a)).collect();

    // Check for added and modified
    for new_action in new_actions {
        if let Some(old_action) = old_map.get(&new_action.id) {
            let changes = compare_action(old_action, new_action);
            if !changes.is_empty() {
                diff.modified.push(ActionDiff {
                    id: new_action.id,
                    changes,
                });
            }
        } else {
            diff.added.push(new_action.clone());
        }
    }

    // Check for removed
    for old_action in old_actions {
        if !new_map.contains_key(&old_action.id) {
            diff.removed.push(old_action.clone());
        }
    }

    diff
}

pub fn semantic_merge(base: &ActionList, ours: &ActionList, theirs: &ActionList) -> SemanticMerge {
    merge_preferred([base, ours, theirs], Side::Ours)
}

fn merge_preferred([base, ours, theirs]: [&ActionList; 3], prefer: Side) -> SemanticMerge {
    let mut merged = vec![];
    let mut conflicts = vec![];

    let index = (
        index_actions(base),
        index_actions(ours),
        index_actions(theirs),
    );

    let mut seen = HashSet::new();
    let ids: Vec<Uuid> = ours
        .iter()
        .chain(theirs)
        .chain(base)
        .map(|a| a.id)
        .filter(|id| seen.insert(*id)) // insert returns false if it was already there
        .collect();

    for id in ids {
        let base_action = index.0.get(&id);
        let our_action = index.1.get(&id);
        let their_action = index.2.get(&id);

        match (base_action, our_action, their_action) {
            (Some(base), Some(ours), Some(theirs)) => {
                let (first, second) = prefer.choose((ours, theirs), (theirs, ours));
                let (merged_action, conflict) = merge_action(base, first, second);
                merged.push(merged_action);
                if let Some(conflict) = conflict {
                    conflicts.push(conflict);
                }
            }
            (None, Some(ours), Some(theirs)) => {
                merged.push((*prefer.choose(ours, theirs)).clone());
                if ours != theirs {
                    conflicts.push(Conflict::BothAdded(BothAdded { id }));
                }
            }
            (Some(base), Some(ours), None) => {
                if ours != base {
                    // Theirs removed it, but we modified it; keep ours and note conflict
                    merged.push((*ours).clone());
                    conflicts.push(Conflict::ActionDeleted(ActionDeleted {
                        id,
                        deleted_by: Side::Theirs,
                    }));
                }
            }
            (Some(base), None, Some(theirs)) => {
                if theirs != base {
                    // Ours removed it, but they modified it; keep theirs and note conflict
                    merged.push((*theirs).clone());
                    conflicts.push(Conflict::ActionDeleted(ActionDeleted {
                        id,
                        deleted_by: Side::Ours,
                    }));
                }
            }
            (Some(_), None, None) => {
                // Both removed it; nothing to do
            }
            (None, Some(ours), None) => {
                // Only ours added it
                merged.push((*ours).clone());
            }
            (None, None, Some(theirs)) => {
                // Only theirs added it
                merged.push((*theirs).clone());
            }
            (None, None, None) => {
                unreachable!()
            }
        }
    }
    restore_deleted_parents(&mut merged, &mut conflicts, &index.1, &index.2);
    keep_descendants(
        &mut merged,
        deleted_ids(&conflicts, Side::Theirs),
        ours,
        &index.2,
    );
    keep_descendants(
        &mut merged,
        deleted_ids(&conflicts, Side::Ours),
        theirs,
        &index.1,
    );
    let (merged, order_conflicts) = order_siblings(merged, [base, ours, theirs], prefer);
    conflicts.extend(order_conflicts);
    let (merged, cycle) = order_tree(
        merged,
        prefer.choose(&index.1, &index.2),
        [base, ours, theirs],
        prefer,
    );
    conflicts.extend(cycle);
    SemanticMerge { merged, conflicts }
}

/// Project the two possible resolutions without giving up any clean edits.
/// The public merge report keeps edited actions as placeholders for a deletion
/// conflict; a written alternative must actually reflect the deletion.
pub(super) fn conflict_choices(sides: [&ActionList; 3], ours: &SemanticMerge) -> [ActionList; 2] {
    let theirs = merge_preferred(sides, Side::Theirs);
    [
        honor_deletions(sides, ours, Side::Ours),
        honor_deletions(sides, &theirs, Side::Theirs),
    ]
}

fn honor_deletions(sides: [&ActionList; 3], result: &SemanticMerge, prefer: Side) -> ActionList {
    let mut removed: HashSet<_> = deleted_ids(&result.conflicts, prefer).collect();
    if removed.is_empty() {
        return result.merged.clone();
    }
    let (edited, other) = prefer.choose((sides[1], sides[2]), (sides[2], sides[1]));
    let preferred = index_actions(edited);
    let mut choice = Vec::new();
    for mut action in result.merged.iter().cloned() {
        if removed.contains(&action.id) {
            continue;
        }
        if action
            .parent_id
            .is_some_and(|parent| removed.contains(&parent))
        {
            let Some(previous) = preferred.get(&action.id) else {
                // A child added under a deleted parent belongs only to the edit choice.
                removed.insert(action.id);
                continue;
            };
            // A child that already exists on this side goes back to where it was,
            // rather than disappearing along with the parent it was moved into.
            action.parent_id = previous.parent_id;
        }
        choice.push(action);
    }
    // Moving a child back can make its original parent necessary again.
    let mut restored = Vec::new();
    restore_deleted_parents(&mut choice, &mut restored, &preferred, &HashMap::new());
    keep_descendants(
        &mut choice,
        deleted_ids(&restored, Side::Theirs),
        edited,
        &index_actions(other),
    );
    let (choice, _) = order_siblings(choice, sides, prefer);
    order_tree(choice, &preferred, sides, prefer).0
}

fn deleted_ids(conflicts: &[Conflict], by: Side) -> impl Iterator<Item = Uuid> {
    conflicts.iter().filter_map(move |conflict| match conflict {
        Conflict::ActionDeleted(deletion) if deletion.deleted_by == by => Some(deletion.id),
        _ => None,
    })
}

/// A disputed subtree deletion includes its unchanged descendants, not just
/// the edit that caused the conflict. A surviving child moved out on the
/// deletion side is not part of that deletion.
fn keep_descendants(
    merged: &mut ActionList,
    roots: impl Iterator<Item = Uuid>,
    edited: &ActionList,
    other: &HashMap<Uuid, &Action>,
) {
    let mut kept: HashSet<_> = roots.collect();
    if kept.is_empty() {
        return;
    }
    let mut present: HashSet<_> = merged.iter().map(|action| action.id).collect();
    for action in edited {
        if action
            .parent_id
            .is_some_and(|parent| kept.contains(&parent))
            && !other.contains_key(&action.id)
        {
            kept.insert(action.id);
            if present.insert(action.id) {
                merged.push(action.clone());
            }
        }
    }
}

/// Adding a child under an action the other side deleted is an edit to that action. Keep the action,
/// and any deleted ancestors above it, and report each as edit versus delete.
fn restore_deleted_parents(
    merged: &mut ActionList,
    conflicts: &mut Vec<Conflict>,
    ours: &HashMap<Uuid, &Action>,
    theirs: &HashMap<Uuid, &Action>,
) {
    let mut present: HashSet<Uuid> = merged.iter().map(|a| a.id).collect();
    // Restored parents are pushed onto `merged`, so the loop reaches them and their parents too.
    let mut i = 0;
    while i < merged.len() {
        if let Some(parent) = merged[i].parent_id
            && !present.contains(&parent)
        {
            let kept = (ours.get(&parent).map(|a| (a, Side::Theirs)))
                .or_else(|| theirs.get(&parent).map(|a| (a, Side::Ours)));
            if let Some((action, deleted_by)) = kept {
                merged.push((*action).clone());
                present.insert(parent);
                conflicts.push(Conflict::ActionDeleted(ActionDeleted {
                    id: parent,
                    deleted_by,
                }));
            }
        }
        i += 1;
    }
}

/// Merge the order of each parent's children, so a move made on one side is kept.
///
/// Children under the same parent on all three sides are ordered like a field: if one side kept
/// base's order take the other's, and if both reordered differently report an OrderConflict and
/// keep ours. Every other child (added, restored or moved in) goes after its nearest previous
/// sibling on the side that has it there, or first when it has none.
///
/// Only the order within each parent changes; `depth_first` then writes the tree.
fn order_siblings(
    mut merged: ActionList,
    sides: [&ActionList; 3],
    prefer: Side,
) -> (ActionList, Vec<Conflict>) {
    let mut parents = Vec::new();
    let mut groups: HashMap<Option<Uuid>, HashSet<Uuid>> = HashMap::new();
    for action in &merged {
        let group = groups.entry(action.parent_id).or_default();
        if group.is_empty() {
            parents.push(action.parent_id);
        }
        group.insert(action.id);
    }

    let mut rank = HashMap::new();
    let mut conflicts = Vec::new();
    for parent in parents {
        let [base, ours, theirs] = sides.map(|side| siblings(side, parent, &groups[&parent]));
        let on_every_side = |seq: &[Uuid]| -> Vec<Uuid> {
            seq.iter()
                .copied()
                .filter(|id| base.contains(id) && ours.contains(id) && theirs.contains(id))
                .collect()
        };
        let (b, o, t) = (
            on_every_side(&base),
            on_every_side(&ours),
            on_every_side(&theirs),
        );
        let mut order = merge_field(&b, &o, &t).unwrap_or_else(|| {
            conflicts.push(Conflict::OrderConflict(OrderConflict { parent }));
            prefer.choose(o, t)
        });
        for side in [&ours, &theirs] {
            for (i, id) in side.iter().enumerate() {
                if order.contains(id) {
                    continue;
                }
                let at = side[..i]
                    .iter()
                    .rev()
                    .find_map(|prev| order.iter().position(|placed| placed == prev))
                    .map_or(0, |p| p + 1);
                order.insert(at, *id);
            }
        }
        rank.extend(order.into_iter().enumerate().map(|(i, id)| (id, i)));
    }
    // Ranks compare only within a parent, which is all depth_first reads; the sort is stable.
    merged.sort_by_key(|a| rank.get(&a.id).copied().unwrap_or(usize::MAX));
    (merged, conflicts)
}

/// The ids of `parent`'s children on one side, in that side's order, limited to `group`.
fn siblings(side: &ActionList, parent: Option<Uuid>, group: &HashSet<Uuid>) -> Vec<Uuid> {
    side.iter()
        .filter(|a| a.parent_id == parent && group.contains(&a.id))
        .map(|a| a.id)
        .collect()
}

/// Put the merged actions in tree order, refusing a parent cycle.
///
/// Each side's moves can be clean on their own and still form a cycle together (ours moves A
/// under B, theirs moves B under A). Undo the moves the preferred side did
/// not make, then order the repaired siblings in their actual parents.
fn order_tree(
    merged: ActionList,
    preferred: &HashMap<Uuid, &Action>,
    sides: [&ActionList; 3],
    prefer: Side,
) -> (ActionList, Option<Conflict>) {
    let (ordered, unreached) = depth_first(&merged);
    if unreached.is_empty() {
        return (ordered, None);
    }
    let mut undone = Vec::new();
    let placeholder: ActionList = merged
        .into_iter()
        .map(|mut action| {
            if let Some(original) = preferred.get(&action.id)
                && unreached.contains(&action.id)
                && original.parent_id != action.parent_id
            {
                action.parent_id = original.parent_id;
                undone.push(action.id);
            }
            action
        })
        .collect();
    let (placeholder, _) = order_siblings(placeholder, sides, prefer);
    let (ordered, _) = depth_first(&placeholder);
    (
        ordered,
        Some(Conflict::ParentCycle(ParentCycle { ids: undone })),
    )
}

/// Order actions depth-first, so each child comes right after its parent.
///
/// The formatter writes actions in list order and marks depth with `>`, so a child that does not
/// follow its parent is read back under the wrong one. Siblings keep their order in `list`.
/// A child whose parent is missing becomes a root. Actions not reachable from a root (a parent
/// cycle) are appended rather than dropped, and returned so the caller can report them.
fn depth_first(list: &[Action]) -> (ActionList, HashSet<Uuid>) {
    let ids: HashSet<Uuid> = list.iter().map(|a| a.id).collect();
    let mut children: HashMap<Option<Uuid>, Vec<&Action>> = HashMap::new();
    for action in list {
        let parent = action.parent_id.filter(|id| ids.contains(id));
        children.entry(parent).or_default().push(action);
    }
    let mut ordered = Vec::with_capacity(list.len());
    let mut seen = HashSet::new();
    push_subtree(None, &children, &mut seen, &mut ordered);
    let mut unreached = HashSet::new();
    for action in list {
        if seen.insert(action.id) {
            unreached.insert(action.id);
            ordered.push(action.clone());
        }
    }
    (ordered, unreached)
}

fn push_subtree(
    parent: Option<Uuid>,
    children: &HashMap<Option<Uuid>, Vec<&Action>>,
    seen: &mut HashSet<Uuid>,
    ordered: &mut ActionList,
) {
    for child in children.get(&parent).into_iter().flatten() {
        if seen.insert(child.id) {
            // Written under `parent`, so say so: a missing parent's child is read back as a root.
            ordered.push(Action {
                parent_id: parent,
                ..(*child).clone()
            });
            push_subtree(Some(child.id), children, seen, ordered);
        }
    }
}

fn index_actions(actions: &ActionList) -> HashMap<Uuid, &Action> {
    actions.iter().map(|a| (a.id, a)).collect()
}

fn merge_action(base: &Action, ours: &Action, theirs: &Action) -> (Action, Option<Conflict>) {
    let mut fields = Vec::new();
    // Three-way merge one field by name; the label always matches the field.
    macro_rules! mergeField {
        ($f:ident) => {
            pick(stringify!($f), &base.$f, &ours.$f, &theirs.$f, &mut fields)
        };
    }
    // A struct literal without `..`: adding a field to Action fails to compile here
    // until the merge decides what to do with it.
    let merged = Action {
        id: ours.id,
        recurrence_rule: mergeField!(recurrence_rule),
        parent_id: mergeField!(parent_id),
        state: mergeField!(state),
        name: mergeField!(name),
        description: mergeField!(description),
        priority: mergeField!(priority),
        contexts: mergeField!(contexts),
        planned: mergeField!(planned),
        due_date: mergeField!(due_date),
        completed_at: mergeField!(completed_at),
        created_at: mergeField!(created_at),
        predecessors: mergeField!(predecessors),
        charter: mergeField!(charter),
        alias: mergeField!(alias),
        is_sequential: mergeField!(is_sequential),
        plan_id: mergeField!(plan_id),
        external_occurrence_key: mergeField!(external_occurrence_key),
        native_occurrence: mergeField!(native_occurrence),
    };

    let conflict = (!fields.is_empty()).then_some(Conflict::FieldConflict(FieldConflict {
        id: ours.id,
        fields,
    }));
    (merged, conflict)
}

fn pick<T: PartialEq + Clone>(
    field: &str,
    base: &T,
    ours: &T,
    theirs: &T,
    conflicts: &mut Vec<String>,
) -> T {
    merge_field(base, ours, theirs).unwrap_or_else(|| {
        conflicts.push(field.to_string());
        ours.clone() // placeholder on conflict, like Git's %A
    })
}

fn merge_field<T: PartialEq + Clone>(base: &T, ours: &T, theirs: &T) -> Option<T> {
    if ours == theirs {
        Some(ours.clone())
    } else if ours == base {
        Some(theirs.clone())
    } else if theirs == base {
        Some(ours.clone())
    } else {
        None
    }
}

fn compare_action(old: &Action, new: &Action) -> Vec<FieldChange> {
    let mut changes = Vec::new();

    if old.state != new.state {
        changes.push(FieldChange::State {
            old: old.state,
            new: new.state,
        });
    }

    if old.name != new.name {
        changes.push(FieldChange::Name {
            old: old.name.clone(),
            new: new.name.clone(),
        });
    }

    if old.description != new.description {
        changes.push(FieldChange::Description {
            old: old.description.clone(),
            new: new.description.clone(),
        });
    }

    if old.priority != new.priority {
        changes.push(FieldChange::Priority {
            old: old.priority,
            new: new.priority,
        });
    }

    if old.planned != new.planned {
        changes.push(FieldChange::DoDate {
            old: old.planned.map(|p| p.to_string()),
            new: new.planned.map(|p| p.to_string()),
        });
    }

    if old.completed_at != new.completed_at {
        changes.push(FieldChange::CompletedDate {
            old: old.completed_at.map(|d| d.to_string()),
            new: new.completed_at.map(|d| d.to_string()),
        });
    }

    // Compare contexts (as sets for robust comparison, but list equality for now)
    if old.contexts != new.contexts {
        changes.push(FieldChange::Generic {
            field: "contexts".to_string(),
            old: json!(old.contexts),
            new: json!(new.contexts),
        });
    }

    if old.charter != new.charter {
        changes.push(FieldChange::Generic {
            field: "charter".to_string(),
            old: json!(old.charter),
            new: json!(new.charter),
        });
    }

    changes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workspace::actions::ActionState;

    fn make_action(id: &str, name: &str) -> Action {
        Action {
            id: Uuid::parse_str(id).unwrap(),
            state: ActionState::NotStarted,
            name: name.to_string(),
            ..Default::default()
        }
    }

    #[test]
    fn test_diff_added() {
        let old = vec![];
        let new = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "New Action",
        )];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.added.len(), 1);
        assert_eq!(diff.removed.len(), 0);
        assert_eq!(diff.modified.len(), 0);
        assert_eq!(diff.added[0].name, "New Action");
    }

    #[test]
    fn test_diff_removed() {
        let old = vec![make_action(
            "019baaec-00b6-7991-be34-94b68212619a",
            "Old Action",
        )];
        let new = vec![];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.added.len(), 0);
        assert_eq!(diff.removed.len(), 1);
        assert_eq!(diff.modified.len(), 0);
        assert_eq!(diff.removed[0].name, "Old Action");
    }

    #[test]
    fn test_diff_modified_state() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let mut old_action = make_action(id, "Task");
        old_action.state = ActionState::NotStarted;

        let mut new_action = make_action(id, "Task");
        new_action.state = ActionState::Completed;

        let old = vec![old_action];
        let new = vec![new_action];

        let diff = diff_actions(&old, &new);

        assert_eq!(diff.modified.len(), 1);
        let modification = &diff.modified[0];
        assert_eq!(modification.id.to_string(), id);
        assert_eq!(modification.changes.len(), 1);

        match &modification.changes[0] {
            FieldChange::State { old, new } => {
                assert_eq!(*old, ActionState::NotStarted);
                assert_eq!(*new, ActionState::Completed);
            }
            _ => panic!("Expected state change"),
        }
    }

    #[test]
    fn test_diff_modified_multiple_fields() {
        let id = "019baaec-00b6-7991-be34-94b68212619a";
        let mut old_action = make_action(id, "Task");
        old_action.priority = Some(1);

        let mut new_action = make_action(id, "Updated Task"); // Name change
        new_action.priority = Some(2); // Priority change

        let diff = diff_actions(&vec![old_action], &vec![new_action]);

        assert_eq!(diff.modified.len(), 1);
        let changes = &diff.modified[0].changes;
        assert_eq!(changes.len(), 2);

        // Sort changes isn't guaranteed, so check existence
        let has_name_change = changes
            .iter()
            .any(|c| matches!(c, FieldChange::Name { .. }));
        let has_prio_change = changes
            .iter()
            .any(|c| matches!(c, FieldChange::Priority { .. }));

        assert!(has_name_change);
        assert!(has_prio_change);
    }
}
