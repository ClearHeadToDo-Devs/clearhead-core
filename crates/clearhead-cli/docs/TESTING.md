## Testing Strategy

### Three Test Levels

1. **Unit tests** (`src/entities.rs`, `src/format.rs`):
   - Test individual functions in isolation
   - Fast, focused, no I/O

2. **Integration tests** (`tests/lib.rs`):
   - Use grammar's built-in test data (`get_test_data()`)
   - Test parsing → formatting round-trips
   - Verify all metadata types work

3. **E2E tests** (`tests/integration.rs`):
   - Test the full CLI pipeline in isolated environments
   - Use `tempfile` for temporary directories
   - Override XDG env vars to avoid pollution
   - Verify config precedence, all formats, error handling

### Running Tests

```bash
# All tests (unit + integration + E2E)
cargo test

# Just library tests
cargo test --lib

# Just integration tests
cargo test --test integration

# With output
cargo test -- --nocapture
```

### Testing the Argument Parser

We use `assert_cmd` for E2E CLI testing, but you can also test argument parsing directly using `clap`'s built-in test utilities:

```rust
#[test]
fn test_cli_parsing() {
    use clap::CommandFactory;
    let app = Cli::command();
    app.debug_assert();  // Validates the CLI definition
}
```

See the [clap testing tutorial](https://docs.rs/clap/latest/clap/_derive/_tutorial/index.html) for more approaches.

### Writing E2E Tests

Use the `TestEnv` helper to create isolated environments:

```rust
#[test]
fn test_my_feature() {
    let env = TestEnv::new();  // Creates temp XDG directories

    env.write_config("format = \"json\"");
    env.write_actions("test.actions", "[ ] My task");

    env.command()
        .arg("read")
        .assert()
        .success()
        .stdout(predicate::str::contains("My task"));
}
```

### Shared Test Helpers

We maintain a suite of shared test helpers in `tests/common/mod.rs` to keep tests functional and dry:

- **`ActionBuilder`**: A fluent interface for constructing `Action` structs in code. Use this instead of manual struct literals to keep tests resilient to changes in the data model.
- **`read_example` / `get_examples`**: Standardized functions for accessing the vendored specification examples.

### Snapshot Testing

Another interesting approach we use is snapshot testing with `insta`. We do this by actually working through the individual examples provided by the `tree-sitter-actions` grammar tests, parsing them into our IR, and then generating snapshots of the resulting data structures.

You can actually see the snapshots themselves in the `snapshots` directory within the `tests` folder. These snapshots are stored in RON (Rusty Object Notation) format, which is a human-readable serialization format similar to JSON but more Rust-friendly.

With this, one can both see the structure and data of the parsed actions, and also verify that any changes to the parsing logic do not inadvertently alter the expected output.

While this does mean new examples added to the grammar tests will need corresponding snapshots, it provides a very robust way to ensure the integrity of the parsing logic over time. and to ensure that changes on the tree-sitter side are caught by the test suite if they arent caught by the other tests.

### Example Vendoring

We vendor the examples hosted at the [specification repo](https://github.com/ClearHeadToDo-Devs/specifications.git) directly to reduce explicit coupling between depenencies.

Instead, new tests and test modifications will be brought over as commits to ensure that the overview is done well while still having everything we need

## Adding New Features

### Adding a New Output Format

1. Add variant to `OutputFormat` enum in `src/format.rs`:

```rust
pub enum OutputFormat {
    Actions,
    Json,
    Xml,
    Table,
    Csv,  // New!
}
```

1. Implement formatter function:

```rust
fn format_as_csv(list: &ActionList) -> Result<String, String> {
    // Implementation
}
```

1. Add to dispatcher:

```rust
pub fn format(list: &ActionList, format: OutputFormat) -> Result<String, String> {
    match format {
        // ...
        OutputFormat::Csv => format_as_csv(list),
    }
}
```

1. Update CLI enum in `src/argparser.rs`:

```rust
pub enum Format {
    // ...
    Csv,
}

impl From<Format> for clearhead_cli::OutputFormat {
    fn from(f: Format) -> Self {
        match f {
            // ...
            Format::Csv => clearhead_cli::OutputFormat::Csv,
        }
    }
}
```

1. Write tests!

### Adding a New Command

1. Add command to `src/argparser.rs`:

```rust
pub enum Commands {
    Read { ... },
    Create {  // New!
        name: String,
        #[arg(short, long)]
        priority: Option<usize>,
    },
}
```

1. Handle in `src/main.rs`:

```rust
match &cli.command {
    Commands::Read { ... } => { ... }
    Commands::Create { name, priority } => {
        // Implementation
    }
}
```

1. Write E2E tests in `tests/integration.rs`
