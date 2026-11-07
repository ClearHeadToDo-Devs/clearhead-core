use clearhead_cli::filesystem::templates::read_template_actions;
use clearhead_core::workspace::{
    actions::format::{OutputFormat, format},
    habit_files::lint_action_file,
};
use std::{fs, path::Path};

const WRITTEN: &str = "019c4f48-6441-75dd-b285-33718b9be996";

#[test]
fn habit_template_ids_reader_identifies_missing_steps_from_parser_records() {
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().join("templates/review.actions");
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(
        &path,
        format!("[ ] Written #{WRITTEN}\n  [ ] Missing child\n[ ] Missing root\n"),
    )
    .unwrap();

    let doc = read_template_actions(&path).unwrap();
    assert_eq!(doc.actions.len(), 3);
    assert_eq!(doc.actions[0].id.to_string(), WRITTEN);
    for (index, action) in doc.actions.iter().enumerate() {
        let record = &doc.source_map[&action.id];
        assert_eq!(record.is_id_generated, index != 0);
        assert_eq!(record.root.start_row, index);
    }
    let missing: Vec<_> = lint_action_file(&doc, Path::new("templates/review.actions"))
        .into_iter()
        .filter(|d| d.code == "habit-id-missing")
        .map(|d| d.range.start_row)
        .collect();
    assert_eq!(missing, vec![1, 2]);
}

#[test]
fn habit_template_ids_reader_written_ids_round_trip_unchanged() {
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().join("review.actions");
    fs::write(&path, format!("[ ] Written #{WRITTEN}\n")).unwrap();
    let doc = read_template_actions(&path).unwrap();
    fs::write(
        &path,
        format(&doc.actions, OutputFormat::Actions, None, None).unwrap(),
    )
    .unwrap();
    let reread = read_template_actions(&path).unwrap();
    assert_eq!(reread.actions[0].id.to_string(), WRITTEN);
    assert!(!reread.source_map[&reread.actions[0].id].is_id_generated);
}

#[test]
fn habit_template_ids_reader_still_rejects_syntax_errors() {
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().join("review.actions");
    fs::write(&path, "not valid actions syntax !!!").unwrap();
    assert!(read_template_actions(&path).is_err());
}
