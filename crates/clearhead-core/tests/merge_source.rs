use clearhead_core::workspace::actions::{
    diff::Conflict,
    merge::{SourceMerge, merge_source},
    parse_actions,
};

fn merged(base: &str, ours: &str, theirs: &str) -> (String, Vec<Conflict>) {
    match merge_source(base, ours, theirs).unwrap() {
        SourceMerge::Merged { text, conflicts } => (text, conflicts),
        SourceMerge::NeedsLineMerge => panic!("expected a semantic merge"),
    }
}

fn task(name: &str, slot: u8) -> String {
    format!("[ ] {name} #01951111-0000-7000-8000-{slot:012}\n")
}

#[test]
fn merges_disjoint_edits_to_one_action() {
    let base = task("Read", 1);
    let ours = task("Review", 1);
    let theirs = task("Read !2", 1);
    let (text, conflicts) = merged(&base, &ours, &theirs);

    assert!(conflicts.is_empty());
    assert_eq!(text, task("Review !2", 1));
}

#[test]
fn keeps_unrelated_rows_as_written() {
    let unusual = task("Unchanged   !2   +work", 1);
    let base = format!("{unusual}\n{}{}\n", task("Read", 2), task("Publish", 3));
    let ours = base.replace("Read", "Review");
    let theirs = base.replace("Publish", "Publish !3");
    let (text, conflicts) = merged(&base, &ours, &theirs);

    assert!(conflicts.is_empty());
    assert_eq!(text, ours.replace("Publish", "Publish !3"));
}

#[test]
fn marks_only_the_child_and_keeps_clean_edits_on_both_choices() {
    let parent = task("Release", 1);
    let child = |name| format!("    >{}", task(name, 2));
    let base = format!("{parent}{}{}", child("Draft"), task("Publish", 3));
    let ours = format!("{parent}{}{}", child("Outline !1"), task("Publish", 3));
    let theirs = format!("{parent}{}{}", child("Write"), task("Publish !3", 3));
    let (text, conflicts) = merged(&base, &ours, &theirs);

    let ours_child = child("Outline !1");
    let theirs_child = child("Write !1");
    let block = format!("<<<<<<< ours\n{ours_child}=======\n{theirs_child}>>>>>>> theirs\n");
    assert_eq!(text, format!("{parent}{block}{}", task("Publish !3", 3)));
    assert_eq!(conflicts.len(), 1);
    assert!(parse_actions(&text).unwrap_err().contains("E010"));
    for choice in [ours_child, theirs_child] {
        let resolved = parse_actions(&text.replace(&block, &choice)).unwrap();
        assert_eq!(resolved.len(), 3);
        assert_eq!(resolved[1].parent_id, Some(resolved[0].id));
        assert_eq!(resolved[1].priority, Some(1));
        assert_eq!(resolved[2].priority, Some(3));
    }
}

#[test]
fn keeps_multiline_descriptions_whole_inside_markers() {
    let with_description = |description| task(&format!("Read $ {description}\n=======\nbody $"), 1);
    let base = with_description("Base");
    let ours = with_description("Ours");
    let theirs = with_description("Theirs");
    let (text, _) = merged(&base, &ours, &theirs);

    assert_eq!(
        text,
        format!("<<<<<<< ours\n{ours}=======\n{theirs}>>>>>>> theirs\n")
    );
    assert!(parse_actions(&text).unwrap_err().contains("E010"));
}

fn child(name: &str, slot: u8) -> String {
    format!("    >{}", task(name, slot))
}

fn block(ours: &str, theirs: &str) -> String {
    format!("<<<<<<< ours\n{ours}=======\n{theirs}>>>>>>> theirs\n")
}

#[test]
fn deletion_conflict_keeps_the_added_subtree_together() {
    let parent = task("Release", 1);
    let previous_child = child("Collect facts", 4);
    let subtree = format!("{parent}{previous_child}{}", child("Draft", 2));
    let next = task("Publish", 3);
    let clean_edit = task("Publish !3", 3);
    let base = format!("{parent}{previous_child}{next}");
    let added_child = format!("{subtree}{next}");

    let (text, conflicts) = merged(&base, &clean_edit, &added_child);
    assert_eq!(text, format!("{}{clean_edit}", block("", &subtree)));
    assert!(matches!(conflicts.as_slice(), [Conflict::ActionDeleted(_)]));

    let (text, conflicts) = merged(&base, &added_child, &clean_edit);
    assert_eq!(text, format!("{}{clean_edit}", block(&subtree, "")));
    assert!(matches!(conflicts.as_slice(), [Conflict::ActionDeleted(_)]));
}

#[test]
fn choosing_a_deletion_keeps_a_preexisting_child_at_its_original_placement() {
    let parent = task("Destination", 1);
    let root = task("Draft", 2);
    let moved = child("Draft", 2);
    let tail = task("After", 3);
    let (text, _) = merged(
        &format!("{parent}{root}{tail}"),
        &format!("{root}{tail}"),
        &format!("{parent}{moved}{tail}"),
    );
    assert_eq!(
        text,
        format!("{}{tail}", block(&root, &format!("{parent}{moved}")))
    );
}

#[test]
fn conflicting_reparent_keeps_both_destinations_and_the_clean_edit() {
    let left = task("Left", 1);
    let right = task("Right", 2);
    let base = format!("{left}{right}{}", task("Draft", 3));
    let ours = format!("{left}{}{right}", child("Draft", 3));
    let theirs = format!("{left}{right}{}", child("Draft !3", 3));
    let ours_choice = format!("{}{right}", child("Draft !3", 3));
    let theirs_choice = format!("{right}{}", child("Draft !3", 3));
    let (text, conflicts) = merged(&base, &ours, &theirs);

    assert_eq!(
        text,
        format!("{left}{}", block(&ours_choice, &theirs_choice))
    );
    assert!(matches!(conflicts.as_slice(), [Conflict::FieldConflict(_)]));
}

#[test]
fn conflicting_reorder_marks_the_affected_siblings_and_keeps_an_independent_conflict_separate() {
    let head = task("Head", 1);
    let a = task("A", 2);
    let b = task("B", 3);
    let c = task("C", 4);
    let tail = task("Tail", 5);
    let draft = task("Draft", 6);
    let outline = task("Outline", 6);
    let write = task("Write", 6);
    let a_with_priority = task("A !3", 2);
    let base = format!("{head}{a}{b}{c}{tail}{draft}");
    let ours = format!("{head}{c}{a}{b}{tail}{outline}");
    let theirs = format!("{head}{b}{c}{a_with_priority}{tail}{write}");
    let (text, conflicts) = merged(&base, &ours, &theirs);
    let order_choice = [
        format!("{c}{a_with_priority}{b}"),
        format!("{b}{c}{a_with_priority}"),
    ];
    let order_block = block(&order_choice[0], &order_choice[1]);
    let draft_block = block(&outline, &write);

    assert_eq!(text, format!("{head}{order_block}{tail}{draft_block}"));
    assert_eq!(conflicts.len(), 2);
    for order in order_choice {
        for draft in [&outline, &write] {
            let resolution = text
                .replace(&order_block, &order)
                .replace(&draft_block, draft);
            let actions = parse_actions(&resolution).unwrap();
            assert_eq!(actions.len(), 6);
            assert!(actions.iter().all(|action| action.parent_id.is_none()));
            assert_eq!(
                actions
                    .iter()
                    .find(|action| action.name == "A")
                    .unwrap()
                    .priority,
                Some(3)
            );
        }
    }
}

#[test]
fn both_adding_the_same_uuid_marks_one_action() {
    let base = task("Untouched", 1);
    let draft = task("Draft", 2);
    let write = task("Write", 2);
    let (text, conflicts) = merged(&base, &format!("{base}{draft}"), &format!("{base}{write}"));

    assert_eq!(text, format!("{base}{}", block(&draft, &write)));
    assert!(matches!(conflicts.as_slice(), [Conflict::BothAdded(_)]));
}

#[test]
fn refuses_malformed_input_even_if_it_is_the_same_on_both_sides() {
    let malformed = "[ ] Read $ no closing delimiter\n";
    let error = merge_source("", malformed, malformed).unwrap_err();
    assert!(error.starts_with("ours: syntax-error"), "{error}");
}

#[test]
fn requests_a_line_merge_for_a_valid_action_without_an_id() {
    let ours = "[ ] Read\n";
    assert!(matches!(
        merge_source("", ours, ""),
        Ok(SourceMerge::NeedsLineMerge)
    ));
}

#[test]
fn refuses_duplicate_ids_instead_of_dropping_a_row() {
    let ours = format!("{}{}", task("Read", 1), task("Publish", 1));
    let error = merge_source("", &ours, "").unwrap_err();
    assert!(error.starts_with("ours: duplicate UUID"), "{error}");
}
