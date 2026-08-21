use clearhead_cli::*;
use std::fs;
use std::path::PathBuf;

fn main() {
    // Read example file from local examples directory
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let example_path = manifest_dir.join("examples").join("with_children.actions");
    let test_data =
        fs::read_to_string(&example_path).expect("Failed to read with_children.actions example");

    let actions = get_action_list_struct(&test_data).unwrap();

    println!("=== ACTIONS FORMAT ===");
    println!(
        "{}",
        format(&actions, OutputFormat::Actions, None, None).unwrap()
    );

    println!("\n=== JSON FORMAT ===");
    println!(
        "{}",
        format(&actions, OutputFormat::Json, None, None).unwrap()
    );

    println!("\n=== XML FORMAT ===");
    println!(
        "{}",
        format(&actions, OutputFormat::Xml, None, None).unwrap()
    );

    println!("\n=== TABLE FORMAT ===");
    println!("(table rendering moved to clearhead-cli display layer)");
}
