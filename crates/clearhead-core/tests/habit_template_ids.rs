use clearhead_core::{parse_document, workspace::habit_files::lint_action_file};
use std::path::Path;

const WRITTEN: &str = "019c4f48-6441-75dd-b285-33718b9be996";

#[test]
fn habit_template_ids_named_missing_step_is_reported_at_its_source() {
    let doc = parse_document(&format!("[ ] Written #{WRITTEN}\n[ ] Missing\n")).unwrap();
    let diagnostics = lint_action_file(&doc, Path::new("templates/review.actions"));
    let missing: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.code == "habit-id-missing")
        .collect();
    assert_eq!(missing.len(), 1);
    assert_eq!(missing[0].range.start_row, 1);
}

#[test]
fn habit_template_ids_written_steps_keep_identity_and_need_no_minting() {
    let doc = parse_document(&format!("[ ] Written #{WRITTEN}\n")).unwrap();
    assert_eq!(doc.actions[0].id.to_string(), WRITTEN);
    assert!(!doc.source_map[&doc.actions[0].id].is_id_generated);
    assert!(
        lint_action_file(&doc, Path::new("templates/review.actions"))
            .iter()
            .all(|d| d.code != "habit-id-missing")
    );
}

#[test]
fn habit_template_ids_inline_children_are_still_reported() {
    let doc = parse_document(&format!(
        "[ ] Habit R:FREQ=DAILY #{WRITTEN}\n  [ ] Missing child\n"
    ))
    .unwrap();
    let diagnostics = lint_action_file(&doc, Path::new("review.habits.actions"));
    let missing: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.code == "habit-id-missing")
        .collect();
    assert_eq!(missing.len(), 1);
    assert_eq!(missing[0].range.start_row, 1);
}
