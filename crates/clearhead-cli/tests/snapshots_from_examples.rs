mod common;
use clearhead_cli::get_action_list_struct;
use common::get_examples;

#[test]
fn test_snapshots_from_specification_examples() {
    for (example_name, content) in get_examples() {
        // Parse the content into our ActionList struct
        let actions = get_action_list_struct(&content)
            .unwrap_or_else(|_| panic!("Failed to parse example: {}", example_name));

        // Use insta to assert against a RON snapshot
        insta::with_settings!({
            sort_maps => true,
            // Naive DSL timestamps intentionally use the machine's local zone.
            // Preserve the date/time assertion while making snapshots portable
            // across developer and CI timezone offsets.
            filters => vec![(r"(T\d{2}:\d{2}:\d{2})(?:Z|[+-]\d{2}:\d{2})", "$1[local-offset]")],
        }, {
            insta::assert_ron_snapshot!(example_name, actions, {
                "[].id" => "[uuid]",
                "[].parent_id" => "[uuid]",
                "[].predecessors[].resolved_uuid" => "[uuid]",
                "[].predecessors[].raw_ref" => "[predecessor_ref]"
            });
        });
    }
}
