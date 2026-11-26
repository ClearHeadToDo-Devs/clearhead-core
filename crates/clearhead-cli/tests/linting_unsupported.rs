mod common;

use clearhead_cli::{ParseMode, get_parsed_document, parse_actions_with_mode};

// W001, E004 and E005 have no linter rule because the grammar cannot express them: a file that
// breaks one has a syntax error, so strict parsing refuses it and workspace loads quarantine it.
// What a recovering parse makes of the rest of the tree is best effort, and tree-sitter's error
// recovery can change with any grammar edit, so these tests assert the syntax error, not a shape.
fn assert_refused(rule: &str, source: &str) {
    let document = get_parsed_document(source).expect("tree-sitter returns a recoverable tree");
    assert!(
        !document.syntax_errors.is_empty(),
        "{rule}: expected a syntax error for {source:?}"
    );
    assert!(
        parse_actions_with_mode(source, ParseMode::Strict).is_err(),
        "{rule}: strict parsing accepted {source:?}"
    );
}

#[test]
fn test_unsupported_grammar_rules() {
    // W001: Hierarchy Depth Exceeded. depth5_action has no child field, so depth 6 cannot parse.
    assert_refused(
        "W001",
        "[ ] Root\n>[ ] D1\n>>[ ] D2\n>>>[ ] D3\n>>>>[ ] D4\n>>>>>[ ] D5\n>>>>>>[ ] D6",
    );
    // E004: Orphaned Child Marker. root_action cannot start with >.
    assert_refused("E004", ">[ ] Orphan");
    // E005: Skipped Hierarchy Level. depth1_action expects depth2_action as its child.
    assert_refused("E005", "[ ] Root\n>>[ ] Skipped");
}
