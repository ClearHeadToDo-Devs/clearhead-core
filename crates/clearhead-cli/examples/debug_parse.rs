use std::{fs, path::PathBuf};

fn main() {
    let spec_root = std::env::var_os("CLEARHEAD_SPEC_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("../specifications"));
    let fixture =
        spec_root.join("examples/conformance/syntax/description_unescaped_bracket.actions");
    let content = fs::read_to_string(&fixture)
        .unwrap_or_else(|error| panic!("failed to read {fixture:?}: {error}"));
    println!("Content length: {} bytes", content.len());
    println!("Line count: {}", content.lines().count());

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_actions::LANGUAGE.into())
        .unwrap();
    let tree = parser.parse(&content, None).unwrap();

    let root = tree.root_node();
    println!("\nRoot has error: {}", root.has_error());
    println!("Root kind: {}", root.kind());
    println!(
        "Root range: {:?} - {:?}",
        root.start_position(),
        root.end_position()
    );

    // Find error nodes
    fn find_errors(node: tree_sitter::Node, depth: usize) {
        if node.is_error() || node.is_missing() {
            println!(
                "{:indent$}ERROR at {:?}: {} (is_error={}, is_missing={})",
                "",
                node.start_position(),
                node.kind(),
                node.is_error(),
                node.is_missing(),
                indent = depth * 2
            );
        }
        for child in node.children(&mut node.walk()) {
            find_errors(child, depth + 1);
        }
    }

    find_errors(root, 0);
    println!("\nDone.");
}
