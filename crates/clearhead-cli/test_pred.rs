fn main() {
    use clearhead_cli::entities::*;
    use std::str::FromStr;

    let source = "[ ] Task B < 01951111-cfa6-718d-b303-d7107f4005b3";
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_actions::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(source, None).unwrap();

    let tree_wrapper = TreeWrapper {
        tree,
        source: source.to_string(),
    };
    let actions: Vec<Action> = tree_wrapper.try_into().unwrap();

    println!("Parsed action: {:#?}", actions[0]);
}
