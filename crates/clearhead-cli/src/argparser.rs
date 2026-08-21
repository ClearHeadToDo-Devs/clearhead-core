use clap::{Args, Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

/// CLI argument parser - returns typed structs for ergonomic access
pub fn parse_cli() -> Cli {
    Cli::parse()
}

/// CLI-specific table column filtering options (for use with clap arg parsing)
#[derive(Args, Clone, Default, Debug, PartialEq)]
pub struct CliTableOptions {
    /// Only show these columns (comma-separated: name,state,priority)
    #[arg(long, value_delimiter = ',')]
    pub columns: Option<Vec<String>>,

    /// Hide these columns (comma-separated)
    #[arg(long, value_delimiter = ',')]
    pub hide_columns: Option<Vec<String>>,

    /// List all available column names and descriptions
    #[arg(long)]
    pub list_columns: bool,
}

/// Editable fields shared by `add plan` and `update plan` commands.
#[derive(Args, Clone, Default, Debug)]
pub struct PlanFields {
    /// Priority (1-9)
    #[arg(short, long)]
    pub priority: Option<u32>,

    /// Contexts (can be specified multiple times)
    #[arg(short, long)]
    pub context: Vec<String>,

    /// Description
    #[arg(short, long)]
    pub description: Option<String>,

    /// Alias for referencing this plan
    #[arg(short, long)]
    pub alias: Option<String>,

    /// State — plans do not have state; use `update action --state` for actions
    #[arg(short, long, value_enum)]
    pub state: Option<ActionStateArg>,
}

/// Schedule fields used by plan commands. Plans live in `.ics` files and
/// describe when actions should be generated.
#[derive(Args, Clone, Default, Debug)]
pub struct PlanScheduleFields {
    /// Schedule anchor datetime (RFC3339, e.g. 2026-04-28T10:00:00-07:00)
    #[arg(long)]
    pub scheduled_at: Option<String>,

    /// RFC5545 recurrence rule, e.g. FREQ=WEEKLY;BYDAY=MO
    #[arg(long)]
    pub rrule: Option<String>,

    /// Template name to bind through recurring VTODO DESCRIPTION
    #[arg(long)]
    pub template: Option<String>,
}

/// Charter state values for CLI
#[derive(Clone, Copy, ValueEnum, Debug)]
pub enum CharterStateArg {
    /// Not yet active (default when omitted)
    New,
    /// Actively being worked on
    Active,
    /// Blocked on an external dependency
    Blocked,
    /// All work done; ready to archive
    Closed,
    /// Abandoned without completion; ready to archive
    Cancelled,
}

impl From<CharterStateArg> for clearhead_core::CharterState {
    fn from(s: CharterStateArg) -> Self {
        match s {
            CharterStateArg::New => clearhead_core::CharterState::New,
            CharterStateArg::Active => clearhead_core::CharterState::Active,
            CharterStateArg::Blocked => clearhead_core::CharterState::Blocked,
            CharterStateArg::Closed => clearhead_core::CharterState::Closed,
            CharterStateArg::Cancelled => clearhead_core::CharterState::Cancelled,
        }
    }
}

/// Action state values for CLI
#[derive(Clone, Copy, ValueEnum, Debug)]
pub enum ActionStateArg {
    /// Not started (default)
    NotStarted,
    /// In progress
    InProgress,
    /// Blocked/waiting
    Blocked,
    /// Completed
    Completed,
    /// Cancelled
    Cancelled,
}

impl From<ActionStateArg> for clearhead_core::ActionState {
    fn from(s: ActionStateArg) -> Self {
        match s {
            ActionStateArg::NotStarted => clearhead_core::ActionState::NotStarted,
            ActionStateArg::InProgress => clearhead_core::ActionState::InProgress,
            ActionStateArg::Blocked => clearhead_core::ActionState::BlockedOrAwaiting,
            ActionStateArg::Completed => clearhead_core::ActionState::Completed,
            ActionStateArg::Cancelled => clearhead_core::ActionState::Cancelled,
        }
    }
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Sets a custom config file
    #[arg(short, long, value_name = "FILE")]
    pub config: Option<PathBuf>,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    pub debug: u8,

    /// Restrict commands to a specific workspace by name (e.g. clearhead-cli)
    #[arg(long, global = true)]
    pub workspace: Option<String>,

    #[command(subcommand)]
    pub command: Verb,
}

#[derive(Subcommand)]
pub enum Verb {
    /// Read and list items (plans, actions, charters)
    Read {
        #[command(subcommand)]
        target: ReadTarget,
    },

    /// Show details of a single item
    Show {
        #[command(subcommand)]
        target: ShowTarget,
    },

    /// Add a new item
    Add {
        #[command(subcommand)]
        target: AddTarget,
    },

    /// Update an existing item
    Update {
        #[command(subcommand)]
        target: UpdateTarget,
    },

    /// Mark an item as completed
    Complete {
        #[command(subcommand)]
        target: CompleteTarget,
    },

    /// Delete an item
    Delete {
        #[command(subcommand)]
        target: DeleteTarget,
    },

    /// Apply an ordered batch of action operations atomically
    Transact {
        /// JSON request file; reads stdin when omitted
        file: Option<PathBuf>,
        /// Validate and report the result without writing anything
        #[arg(long)]
        dry_run: bool,
    },

    /// Query the workspace graph, forwarding to clearhead-graphd
    Query {
        #[command(subcommand)]
        target: QueryTarget,
    },

    /// Format a file
    Format {
        #[command(subcommand)]
        target: FormatTarget,
    },

    /// Lint a file for errors and warnings
    Lint {
        #[command(subcommand)]
        target: LintTarget,
    },

    /// Normalize a file (ensure UUIDs, format)
    Normalize {
        #[command(subcommand)]
        target: NormalizeTarget,
    },

    /// Apply changes from a patch file
    Patch {
        #[command(subcommand)]
        target: PatchTarget,
    },

    /// Archive completed items
    Archive {
        #[command(subcommand)]
        target: ArchiveTarget,
    },

    /// Export items to external formats
    Export {
        #[command(subcommand)]
        target: ExportTarget,
    },

    /// Import external formats into canonical workspace storage
    Import {
        #[command(subcommand)]
        target: ImportTarget,
    },

    /// Cancel an item (sets state to Cancelled without deleting)
    Cancel {
        #[command(subcommand)]
        target: CancelTarget,
    },

    /// Apply a template or pattern to a charter
    Apply {
        #[command(subcommand)]
        target: ApplyTarget,
    },

    /// Close a charter (sets state to Closed, creates .md if missing)
    Close {
        #[command(subcommand)]
        target: CloseTarget,
    },

    /// Start a service
    Start {
        #[command(subcommand)]
        target: StartTarget,
    },

    /// Synchronize data with external systems
    Sync {
        #[command(subcommand)]
        target: SyncTarget,
    },

    /// Show resolved config and workspace diagnostics
    Debug,

    /// Check workspace coherence: exits 0 clean, 1 warnings, 2 violations
    Doctor {
        /// Emit the diagnosis as JSON for scripting
        #[arg(long, conflicts_with = "fix")]
        json: bool,

        /// Remove fixable unowned sidecar state and calendar collections, then diagnose again
        #[arg(long)]
        fix: bool,

        /// Preview cleanup without writing (requires --fix)
        #[arg(long, requires = "fix")]
        dry_run: bool,
    },

    /// Initialize a clearhead workspace in the current directory
    Init,

    /// Generate shell completion script
    #[command(hide = true)]
    Completion {
        /// Shell to generate completions for
        shell: clap_complete::Shell,
    },

    /// Internal: list completable values (one per line) for shell integration
    #[command(name = "_complete", hide = true)]
    CompleteValues { kind: CompleteKind },
}

/// Types of values that can be completed
#[derive(clap::ValueEnum, Clone, Copy, Debug)]
pub enum CompleteKind {
    /// Charter aliases (falling back to title if no alias)
    Charters,
    /// Workspace names
    Workspaces,
}

// =============================================================================
// Read targets
// =============================================================================

#[derive(Subcommand)]
pub enum ReadTarget {
    /// Read and display plans (workspace-wide by default)
    Plans {
        /// Output mode: table forces table, jsonld emits JSON-LD, ids prints one UUID per line.
        /// Default: table in a terminal, native .ics format when piped.
        #[arg(short, long, value_enum)]
        format: Option<OutputMode>,

        /// Filter plans by charter name, alias, or UUID
        #[arg(long, conflicts_with_all = ["file", "stdio"])]
        charter: Option<String>,

        /// Include plans from sub-charters recursively (requires --charter)
        #[arg(long, requires = "charter")]
        recursive: bool,

        /// Read from a specific file instead of workspace
        #[arg(long, conflicts_with_all = ["charter", "stdio"])]
        file: Option<PathBuf>,

        /// Read from standard input instead of workspace
        #[arg(long, conflicts_with_all = ["file", "charter"])]
        stdio: bool,

        /// Table column filtering options
        #[command(flatten)]
        table_options: CliTableOptions,
    },

    /// List all discovered charters
    Charters {
        /// Output mode: table forces table, jsonld emits JSON-LD, ids prints one UUID per line.
        /// Default: tree in a terminal, markdown when piped.
        #[arg(short, long, value_enum)]
        format: Option<OutputMode>,

        /// Show only explicit (file-backed) charters
        #[arg(long)]
        explicit_only: bool,
    },

    /// Read and display actions
    Actions {
        /// Output mode: table forces table, jsonld emits JSON-LD, ids prints one UUID per line.
        /// Default: table in a terminal, .actions DSL when piped.
        #[arg(short, long, value_enum)]
        format: Option<OutputMode>,

        /// Filter actions by plan UUID, short UUID, alias, or name
        #[arg(long)]
        plan: Option<String>,

        /// Filter actions by charter (name, alias, or UUID)
        #[arg(long)]
        charter: Option<String>,

        /// Filter by context tag (can be specified multiple times; includes tag hierarchy ancestors)
        #[arg(long)]
        context: Vec<String>,

        /// Only show open actions (excludes Completed and Cancelled)
        #[arg(long)]
        open_only: bool,

        /// Filter to specific state(s) (can be specified multiple times; overrides --open-only)
        #[arg(long = "state", value_enum)]
        states: Vec<ActionStateArg>,

        /// Read from a specific .actions file (also loads sibling .actions.jsonld)
        #[arg(long)]
        file: Option<PathBuf>,
    },
}

// =============================================================================
// Show targets
// =============================================================================

#[derive(Subcommand)]
pub enum ShowTarget {
    /// Show details of a specific plan
    Plan {
        /// UUID, short UUID, alias, or name of the plan
        query: String,

        /// File containing the plan. If not provided, uses the default file
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// Output mode: table forces table, jsonld emits JSON-LD, ids prints one UUID per line.
        #[arg(short = 'F', long, value_enum)]
        format: Option<OutputMode>,

        /// Table column filtering options
        #[command(flatten)]
        table_options: CliTableOptions,
    },

    /// Show details of a specific action
    Action {
        /// UUID, short UUID, alias, or name of the action
        query: String,

        /// File containing the .actions file. If not provided, workspace search.
        #[arg(short, long)]
        file: Option<PathBuf>,
    },

    /// Show details of a specific charter
    Charter {
        /// UUID, alias, or name of the charter
        query: String,
    },
}

// =============================================================================
// Add targets
// =============================================================================

#[derive(Subcommand)]
pub enum AddTarget {
    /// Add a new recurring schedule Plan (.ics VTODO+RRULE)
    Plan {
        /// Name of the plan
        name: String,

        /// Explicit output .ics file path for the new single-event plan file
        #[arg(short, long, conflicts_with = "charter")]
        file: Option<PathBuf>,

        /// Charter to add the plan to (name, alias, or UUID). Writes into that charter's plans/ directory.
        #[arg(long, conflicts_with = "file")]
        charter: Option<String>,

        /// Parent plan reference. Plan hierarchy in ICS storage is not implemented yet.
        #[arg(long)]
        parent: Option<String>,

        /// Plan fields (priority, context, description, alias; state is action-only and rejected)
        #[command(flatten)]
        fields: PlanFields,

        /// Schedule fields (DTSTART, RRULE, template binding)
        #[command(flatten)]
        schedule: PlanScheduleFields,

        /// Preview what would be added without writing
        #[arg(long)]
        dry_run: bool,
    },

    /// Add a new action to a charter's .actions file
    Action {
        /// Name of the action
        name: String,

        /// Charter to add the action to (name, alias, or UUID). Routes to the charter's .actions file.
        #[arg(long, conflicts_with = "file")]
        charter: Option<String>,

        /// .actions file to add to. If not provided, uses the default file.
        #[arg(short, long, conflicts_with = "charter")]
        file: Option<PathBuf>,

        /// Parent action UUID or short UUID
        #[arg(long)]
        parent: Option<String>,

        /// Priority (1-9)
        #[arg(short, long)]
        priority: Option<u32>,

        /// Initial state
        #[arg(short, long, value_enum)]
        state: Option<ActionStateArg>,

        /// Alias for referencing this action
        #[arg(short, long)]
        alias: Option<String>,

        /// Description / note for the action (the `$ ... $` inline note in the DSL)
        #[arg(long)]
        description: Option<String>,

        /// Context tags (`+tag` in DSL, can be specified multiple times)
        #[arg(short, long)]
        context: Vec<String>,

        /// Predecessor reference (`< ref` in DSL, can be specified multiple times)
        #[arg(long)]
        predecessor: Vec<String>,

        /// Mark direct children as implicitly sequential (`~` marker in DSL)
        #[arg(long)]
        sequential: bool,

        /// Scheduled datetime (RFC 3339, e.g. "2026-04-28T09:00:00-07:00")
        #[arg(long)]
        scheduled_at: Option<String>,

        /// Duration in minutes
        #[arg(short, long)]
        duration: Option<u32>,

        /// Preview what would be added without writing
        #[arg(long)]
        dry_run: bool,
    },

    /// Create a new charter
    Charter {
        /// Title of the charter
        title: String,

        /// Short alias for the charter
        #[arg(short, long)]
        alias: Option<String>,

        /// Parent charter reference
        #[arg(short, long)]
        parent: Option<String>,

        /// Scaffold from a template (name of .actions file in templates/)
        #[arg(long)]
        template: Option<String>,

        /// Preview what would be created without writing
        #[arg(long)]
        dry_run: bool,
    },
}

// =============================================================================
// Update / Complete / Delete targets
// =============================================================================

#[derive(Subcommand)]
pub enum UpdateTarget {
    /// Update an existing recurring schedule Plan (.ics VTODO+RRULE)
    Plan {
        /// UUID, short UUID, alias, or name of the plan to update
        query: String,

        /// .ics file containing the plan. If not provided, searches the workspace
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// New name for the plan
        #[arg(short, long)]
        name: Option<String>,

        /// Plan fields to update (priority, context, description, alias; state is action-only and rejected)
        #[command(flatten)]
        fields: PlanFields,

        /// Schedule fields to update (DTSTART, RRULE, template binding)
        #[command(flatten)]
        schedule: PlanScheduleFields,

        /// Preview what would be updated without writing
        #[arg(long)]
        dry_run: bool,
    },

    /// Update an action's fields
    Action {
        /// UUID, short prefix (at least 4 hex digits), alias, or name of the action
        query: String,

        /// New name
        #[arg(short, long)]
        name: Option<String>,

        /// New priority (1-9)
        #[arg(short, long)]
        priority: Option<u32>,

        /// New state
        #[arg(short, long, value_enum)]
        state: Option<ActionStateArg>,

        /// New scheduled datetime (RFC 3339, e.g. "2026-04-01T09:00:00+00:00")
        #[arg(long)]
        scheduled_at: Option<String>,

        /// New duration in minutes
        #[arg(long)]
        duration: Option<u32>,

        /// New description / note (replaces the existing `$ ... $` inline note)
        #[arg(long)]
        description: Option<String>,

        /// Context tags (`+tag` in DSL, can be specified multiple times). Replaces the existing set.
        #[arg(short, long)]
        context: Vec<String>,

        /// Predecessor references (`< ref` in DSL, can be specified multiple times). Replaces the existing set.
        #[arg(long)]
        predecessor: Vec<String>,

        /// Mark direct children as implicitly sequential (`~` marker in DSL)
        #[arg(long)]
        sequential: bool,

        /// Scope search to a specific charter (name, alias, or UUID). Makes resolution deterministic when names collide across charters.
        #[arg(long)]
        charter: Option<String>,

        /// File containing the .actions file (sidecar is derived). If not provided, workspace search.
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// Preview what would be updated without writing
        #[arg(long)]
        dry_run: bool,
    },

    /// Update a charter's metadata (e.g. set state to Closed before archiving)
    Charter {
        /// UUID, alias, or name of the charter to update
        query: String,

        /// New lifecycle state
        #[arg(short, long, value_enum)]
        state: Option<CharterStateArg>,

        /// New title
        #[arg(long)]
        title: Option<String>,

        /// New alias
        #[arg(long)]
        alias: Option<String>,

        /// Preview what would be updated without writing
        #[arg(long)]
        dry_run: bool,
    },
}

#[derive(Subcommand)]
pub enum CompleteTarget {
    /// Deprecated: plans are schedules; complete actions instead
    Plan {
        /// UUID or name of the plan
        query: String,

        /// .ics file containing the plan
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// Preview what would be completed without writing
        #[arg(long)]
        dry_run: bool,
    },

    /// Mark an action as completed
    Action {
        /// UUID, short prefix (at least 4 hex digits), alias, or name of the action
        query: String,

        /// Scope search to a specific charter (name, alias, or UUID). Makes resolution deterministic when names collide across charters.
        #[arg(long)]
        charter: Option<String>,

        /// File containing the .actions file (sidecar is derived). If not provided, workspace search.
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// Preview what would be completed without writing
        #[arg(long)]
        dry_run: bool,
    },
}

#[derive(Subcommand)]
pub enum DeleteTarget {
    /// Delete a recurring schedule Plan (.ics VTODO+RRULE)
    Plan {
        /// UUID or name of the plan to delete
        query: String,

        /// .ics file containing the plan. If not provided, searches the workspace
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// Preview what would be deleted without writing
        #[arg(long)]
        dry_run: bool,
    },

    /// Delete an action from the workspace
    Action {
        /// UUID, short UUID, alias, or name of the action to delete
        query: String,

        /// Scope search to a specific charter (name, alias, or UUID). Makes resolution deterministic when names collide across charters.
        #[arg(long)]
        charter: Option<String>,

        /// File containing the .actions file. If not provided, searches the workspace.
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// Preview what would be deleted without writing
        #[arg(long)]
        dry_run: bool,
    },
}

// =============================================================================
// Query targets — forwarded to clearhead-graphd
// =============================================================================

/// Explicit query output format. Without an override, graphd chooses from the
/// query family and whether stdout is a terminal. Mirrors graphd's own set.
#[derive(Copy, Clone, Debug, ValueEnum)]
pub enum QueryFormat {
    /// Pretty-printed human table
    Table,
    /// JSON array of objects
    Json,
    /// One JSON object per line
    Ndjson,
    /// Semantic JSON-LD document (shaped query families)
    Jsonld,
    /// Bare canonical ids, one per line (feeds `clearhead transact`)
    Ids,
    /// RDF Turtle (graph families)
    Turtle,
    /// Graphviz DOT (graph families)
    Dot,
}

#[derive(Subcommand)]
pub enum QueryTarget {
    /// Run a raw SPARQL query, or a WHERE clause via --where
    Raw {
        /// Full SPARQL SELECT query
        #[arg(conflicts_with = "where_clause")]
        sparql: Option<String>,
        /// SPARQL WHERE clause (graphd auto-injects prefixes, selects all vars)
        #[arg(short = 'w', long = "where", conflicts_with = "sparql")]
        where_clause: Option<String>,
        #[arg(long, value_enum)]
        format: Option<QueryFormat>,
    },
    /// Run a named query from the queries/ registry
    #[command(name = "named")]
    Named {
        /// Name of the query (stem of the .sparql file)
        name: String,
        /// Value substituted for ?STATUS_FILTER (graphd owns its interpretation)
        #[arg(long)]
        status: Option<String>,
        #[arg(long, value_enum)]
        format: Option<QueryFormat>,
    },
    /// Run an index view (ordered, addressable rows). Omit name for "default".
    Index {
        name: Option<String>,
        #[arg(long, value_enum)]
        format: Option<QueryFormat>,
    },
    /// Run a tree view. Omit name for "work-map".
    Tree {
        name: Option<String>,
        #[arg(long, value_enum)]
        format: Option<QueryFormat>,
    },
    /// Run a CONSTRUCT graph view. Omit name for "dependencies".
    Graph {
        name: Option<String>,
        #[arg(long, value_enum)]
        format: Option<QueryFormat>,
    },
    /// Walk one action's predecessor chain; the fuzzy query is resolved to a
    /// canonical id here, then forwarded as `index chain --target <iri>`.
    Chain {
        /// UUID, short UUID, alias, or name of the starting action
        query: String,
        #[arg(long, value_enum)]
        format: Option<QueryFormat>,
    },
    /// Print a named or built-in query's SPARQL to stdout
    Show {
        /// Name of the query (index, typed, or freeform)
        name: String,
    },
    /// List available named queries
    List,
}

// =============================================================================
// File operation targets
// =============================================================================

#[derive(Subcommand)]
pub enum FormatTarget {
    /// Format an actions file with proper spacing and layout
    File {
        /// File to format. If not provided, reads from stdin
        path: Option<PathBuf>,

        /// Overwrite the input file with the formatted version
        #[arg(short, long)]
        write: bool,

        /// Formatting style (compact or list)
        #[arg(short, long, value_enum)]
        style: Option<Style>,

        /// Indentation style (spaces or tabs)
        #[arg(long, value_enum)]
        indent_style: Option<Indent>,

        /// Indentation width (for list style or child padding)
        #[arg(short, long)]
        indent_width: Option<usize>,
    },
}

#[derive(Subcommand)]
pub enum LintTarget {
    /// Lint an actions file for errors and warnings
    File {
        /// File to lint. If not provided, reads from stdin
        path: Option<PathBuf>,
    },
}

#[derive(Subcommand)]
pub enum NormalizeTarget {
    /// Ensure all actions in a file have UUIDs (formats by default)
    File {
        /// File to normalize. If not provided, reads from stdin
        path: Option<PathBuf>,

        /// Overwrite the input file with the normalized version
        #[arg(short, long)]
        write: bool,

        /// Skip formatting after adding UUIDs
        #[arg(long)]
        no_format: bool,
    },
}

#[derive(Subcommand)]
pub enum PatchTarget {
    /// Apply changes from a secondary patch file to a primary source file
    File {
        /// The primary source file (source of truth)
        #[arg(short, long)]
        primary: PathBuf,

        /// The secondary file containing updates/patches
        #[arg(short, long)]
        secondary: PathBuf,

        /// Overwrite the primary file with the patched version
        #[arg(short, long)]
        write: bool,
    },
}

// =============================================================================
// Collection operation targets
// =============================================================================

#[derive(Subcommand)]
pub enum ArchiveTarget {
    /// Explain that externally-owned schedules are not archived.
    ///
    /// Use `delete plan` for an explicit schedule deletion and manage generated
    /// actions through their own lifecycle verbs.
    Plans {
        /// Domain path to scope: "health", "health/exercise", etc.
        scope: Option<String>,

        /// Escape hatch: explicit .ics file path for out-of-workspace use.
        #[arg(long, conflicts_with = "scope")]
        file: Option<PathBuf>,

        /// Dry run
        #[arg(long)]
        dry_run: bool,
    },

    /// Move completed/cancelled actions from `.actions` to `.completed.actions`. Workspace-wide by default.
    Actions {
        /// Domain path to scope: "health", "health/exercise", etc. Workspace-wide if omitted.
        scope: Option<String>,

        /// Escape hatch: explicit file path for out-of-workspace use.
        #[arg(long, conflicts_with = "scope")]
        file: Option<PathBuf>,

        /// Dry run: show counts without writing
        #[arg(long)]
        dry_run: bool,
    },

    /// Move a closed or cancelled charter's subtree (as plaintext) into the `archive/` region.
    ///
    /// The charter must have `state: Closed` or `state: Cancelled` in its frontmatter. If the
    /// primary `.actions` file still contains open actions the command refuses unless --force
    /// is given.
    Charter {
        /// Charter to archive (name, alias, or UUID prefix). Omit with --closed to sweep all closed/cancelled charters.
        #[arg(conflicts_with_all = ["closed", "file"])]
        query: Option<String>,

        /// Infer charter from this file path (e.g. the current editor buffer)
        #[arg(short, long, conflicts_with_all = ["query", "closed"])]
        file: Option<std::path::PathBuf>,

        /// Archive all charters whose frontmatter carries `state: Closed` or `state: Cancelled`.
        #[arg(long, conflicts_with_all = ["query", "file"])]
        closed: bool,

        /// Archive even if open actions remain in the primary `.actions` file.
        #[arg(long)]
        force: bool,

        /// Show what would be archived without writing or deleting anything.
        #[arg(long)]
        dry_run: bool,
    },
}

// =============================================================================
// Apply targets
// =============================================================================

#[derive(Subcommand)]
pub enum ApplyTarget {
    /// Instantiate a template into a charter's .actions file
    Template {
        /// Template name (stem of the .actions file in templates/)
        name: String,

        /// Charter to apply to (name, alias, or UUID). Uses default charter if omitted.
        #[arg(long, conflicts_with = "file")]
        charter: Option<String>,

        /// Explicit .actions file path
        #[arg(long, conflicts_with = "charter")]
        file: Option<PathBuf>,

        /// Preview what would be applied without writing
        #[arg(long)]
        dry_run: bool,
    },
}

// =============================================================================
// Close targets
// =============================================================================

#[derive(Subcommand)]
pub enum CloseTarget {
    /// Close a charter (sets state to Closed, creates .md if missing)
    Charter {
        /// UUID, alias, or name of the charter
        #[arg(conflicts_with = "file")]
        query: Option<String>,

        /// Infer charter from this file path (e.g. the current editor buffer)
        #[arg(short, long, conflicts_with = "query")]
        file: Option<std::path::PathBuf>,

        /// Preview what would be changed without writing
        #[arg(long)]
        dry_run: bool,
    },
}

// =============================================================================
// Cancel targets
// =============================================================================

#[derive(Subcommand)]
pub enum CancelTarget {
    /// Cancel an action (sets state to Cancelled)
    Action {
        /// UUID, short prefix (at least 4 hex digits), alias, or name of the action
        query: String,

        /// Scope search to a specific charter (name, alias, or UUID). Makes resolution deterministic when names collide across charters.
        #[arg(long)]
        charter: Option<String>,

        /// File containing the .actions file (sidecar is derived). If not provided, workspace search.
        #[arg(short, long)]
        file: Option<PathBuf>,

        /// Preview what would be cancelled without writing
        #[arg(long)]
        dry_run: bool,
    },
}

#[derive(Subcommand)]
pub enum ExportTarget {
    /// Export plans to calendar format (iCalendar)
    #[command(alias = "calendar")]
    Plans {
        /// Reference to export (charter/action alias or entity UUID, or .actions file)
        #[arg(value_name = "REFERENCE")]
        reference: Option<String>,

        /// Output file path. If not provided, writes to stdout
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Only export open plans (pending, in-progress, blocked)
        #[arg(long)]
        open_only: bool,

        /// Include sub-charters when exporting a charter reference
        #[arg(long, requires = "reference")]
        recursive: bool,
    },
}

#[derive(Subcommand)]
pub enum ImportTarget {
    /// Import recurring VTODO Plan resources into vdir storage
    Plans {
        /// Source .ics file containing recurring VTODOs
        source: PathBuf,

        /// Target charter (name, alias, or UUID). Defaults to source filename stem.
        #[arg(long)]
        charter: Option<String>,

        /// Overwrite existing plan files when the imported component UID already exists.
        #[arg(long)]
        overwrite: bool,

        /// Preview what would be imported without writing
        #[arg(long)]
        dry_run: bool,
    },
}

// =============================================================================
// Service targets
// =============================================================================

#[derive(Subcommand)]
pub enum StartTarget {
    /// Start the Language Server Protocol (LSP) server
    Lsp,
}

#[derive(Clone, Copy, ValueEnum, Debug)]
pub enum ConflictResolutionArg {
    /// Prefer the action file's value
    Action,
    /// Prefer the calendar `.ics` value
    Calendar,
}

#[derive(Subcommand)]
pub enum SyncTarget {
    /// Backfill action-created telemetry records for existing actions
    Events {
        /// File to sync. If not provided, uses the default file
        file: Option<PathBuf>,

        /// Dry run: show what would be synced without writing to DB
        #[arg(long)]
        dry_run: bool,
    },
    /// Reconcile Actions with standalone VTODOs in the configured plans vdir
    Calendar {
        /// Dry run: show what would be changed without writing
        #[arg(long)]
        dry_run: bool,

        /// Resolve all calendar conflicts by explicitly choosing one side
        #[arg(long, value_enum)]
        conflict: Option<ConflictResolutionArg>,
    },
}

// =============================================================================
// Shared value enums
// =============================================================================

/// Output mode for read commands — overrides TTY-aware default.
///
/// Default (no flag): table in a terminal, native file format when piped.
#[derive(Clone, Copy, ValueEnum, Debug, PartialEq)]
pub enum OutputMode {
    /// Force human table output (even when piped)
    Table,
    /// Canonical JSON — actions conform to the specifications actions schema.
    Json,
    /// JSON-LD (RDF graph form) for semantic tools.
    JsonLd,
    /// One UUID per line — for xargs and reference piping
    Ids,
}

/// CLI-specific style enum that maps to library's FormatStyle
#[derive(Clone, Copy, ValueEnum)]
pub enum Style {
    /// Compact: metadata on same line
    Compact,
    /// List: metadata on separate indented lines
    List,
}

impl From<Style> for clearhead_core::format::FormatStyle {
    fn from(s: Style) -> Self {
        match s {
            Style::Compact => clearhead_core::format::FormatStyle::Compact,
            Style::List => clearhead_core::format::FormatStyle::List,
        }
    }
}

/// CLI-specific indent enum that maps to library's IndentStyle
#[derive(Clone, Copy, ValueEnum)]
pub enum Indent {
    /// Use spaces for indentation
    Spaces,
    /// Use tabs for indentation
    Tabs,
}

impl From<Indent> for clearhead_core::format::IndentStyle {
    fn from(i: Indent) -> Self {
        match i {
            Indent::Spaces => clearhead_core::format::IndentStyle::Spaces,
            Indent::Tabs => clearhead_core::format::IndentStyle::Tabs,
        }
    }
}
