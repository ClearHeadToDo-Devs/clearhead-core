use common::TestEnv;

mod common;

#[test]
fn locate_answers_each_id_in_order_from_stdin_and_reports_unknown_ones() {
    let env = TestEnv::new();
    env.write_text(
        "charters/alpha.md",
        "---\nid: 01951111-0000-7000-0000-0000000000a0\nalias: alpha\n---\n# Alpha\n",
    );
    env.write_actions(
        "alpha.actions",
        "[ ] First #01951111-0000-7000-0000-0000000000a1\n\
         [ ] Second #01951111-0000-7000-0000-0000000000a2\n",
    );

    let output = env
        .command()
        .arg("locate")
        .write_stdin(
            "01951111-0000-7000-0000-0000000000a2\n\
             urn:uuid:01951111-0000-7000-0000-0000000000a0\n\
             \n\
             01951111-0000-7000-0000-0000000000ff\n",
        )
        .assert()
        .success()
        .stderr(predicates::str::contains(
            "01951111-0000-7000-0000-0000000000ff",
        ))
        .get_output()
        .stdout
        .clone();
    let rows: serde_json::Value = serde_json::from_slice(&output).unwrap();
    let rows = rows.as_array().unwrap();

    assert_eq!(rows.len(), 3, "blank lines are skipped: {rows:?}");
    assert_eq!(rows[0]["kind"], "action");
    assert!(
        rows[0]["file"]
            .as_str()
            .unwrap()
            .ends_with("charters/alpha.actions")
    );
    assert_eq!(rows[0]["line"], 2);
    assert_eq!(rows[1]["kind"], "charter");
    assert!(
        rows[1]["file"]
            .as_str()
            .unwrap()
            .ends_with("charters/alpha.md")
    );
    assert!(rows[2]["file"].is_null());
}
