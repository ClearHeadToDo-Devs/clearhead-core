//! I8: the effectful crate has frontends (`cli`, `lsp`, and the future `mcp`)
//! and a runtime (`query`, `filesystem`); frontends depend on the runtime,
//! never on each other. This test reads the frontend source directories and
//! fails on any cross-frontend import, so the one-directional dependency rule
//! is checked by the compiler-adjacent text rather than by crate count.

use std::path::Path;

fn source_texts(dir: &Path) -> Vec<String> {
    let mut texts = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(directory) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&directory) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|ext| ext == "rs")
                && let Ok(text) = std::fs::read_to_string(&path)
            {
                texts.push(text);
            }
        }
    }
    texts
}

#[test]
fn frontends_never_import_each_other() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));

    let cli = source_texts(&root.join("src/cli"));
    assert!(
        !cli.iter()
            .any(|text| text.contains("crate::lsp") || text.contains("crate::mcp")),
        "the `cli` frontend must not import `lsp` or `mcp`"
    );

    let lsp = source_texts(&root.join("src/lsp"));
    assert!(
        !lsp.iter()
            .any(|text| text.contains("crate::cli") || text.contains("crate::mcp")),
        "the `lsp` frontend must not import `cli` or `mcp`"
    );
}
