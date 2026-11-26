//! Unresolved git merge conflicts (specification E010): a conflicted file
//! parses, but each conflict is an integrity issue, so strict parsing and
//! every rewrite refuse it.

use clearhead_core::{ParseMode, TrustedDocument, parse_actions_with_mode, parse_document};

const CONFLICTED: &str = "[ ] Plan the release #01951111-0000-7000-0000-000000000001
<<<<<<< ours
>[ ] Write the notes !1 #01951111-0000-7000-0000-000000000002
=======
>[ ] Write the notes !2 #01951111-0000-7000-0000-000000000002
>>>>>>> theirs
[ ] Ship it #01951111-0000-7000-0000-000000000003
<<<<<<< ours
>[ ] Tag the release
||||||| base
>[ ] Tag it
=======
>[ ] Tag the release candidate
>>>>>>> theirs
";

fn conflicts(source: &str) -> Vec<String> {
    parse_document(source)
        .expect("tree-sitter returns a tree")
        .syntax_errors
        .into_iter()
        .filter(|issue| issue.code == "E010")
        .map(|issue| issue.message)
        .collect()
}

#[test]
fn each_conflict_is_one_e010_naming_its_lines() {
    assert_eq!(
        conflicts(CONFLICTED),
        [
            "unresolved merge conflict at lines 2-6",
            "unresolved merge conflict at lines 8-14",
        ]
    );
}

#[test]
fn a_conflicted_file_is_refused_by_strict_parsing_and_rewrites() {
    let failure = parse_actions_with_mode(CONFLICTED, ParseMode::Strict).unwrap_err();
    assert_eq!(failure.code, "E010");
    let document = parse_document(CONFLICTED).unwrap();
    assert!(TrustedDocument::try_from(document).is_err());
}

#[test]
fn marker_like_description_lines_are_not_conflicts() {
    let source = "[ ] Write the notes $ Heading\n=======\n<<<<<<< not a conflict\n$\n";
    assert!(conflicts(source).is_empty());
    assert!(parse_actions_with_mode(source, ParseMode::Strict).is_ok());
}

#[test]
fn an_unclosed_conflict_still_reports_its_lines() {
    let source = "[ ] Plan\n<<<<<<< ours\n>[ ] One\n=======\n>[ ] Two\n";
    assert_eq!(
        conflicts(source),
        ["unresolved merge conflict at lines 2-4"]
    );
}
