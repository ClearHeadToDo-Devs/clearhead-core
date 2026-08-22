use anyhow::Result;
use clap::{Parser, Subcommand};
use clearhead_core::WorkspaceConfig;
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(name = "clearhead-graphd")]
#[command(about = "Standalone graph/query binary for ClearHead")]
struct Cli {
    /// Workspace root to load.
    #[arg(short, long, default_value = ".")]
    workspace: PathBuf,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Query the workspace: named views, saved queries, or ad-hoc SPARQL.
    Query(QueryArgs),
}

#[derive(clap::Args, Debug)]
struct QueryArgs {
    #[command(subcommand)]
    kind: QueryKind,
}

#[derive(Subcommand, Debug)]
enum QueryKind {
    /// Run a named index view (default: "default").
    Index {
        name: Option<String>,
        /// Canonical IRI for ?TARGET_ACTION in chain-style views.
        #[arg(long)]
        target: Option<String>,
        #[arg(long, value_enum)]
        format: Option<clearhead_graphd::query::Format>,
    },
    /// Run a named tree view (default: "work-map").
    Tree {
        name: Option<String>,
        #[arg(long, value_enum)]
        format: Option<clearhead_graphd::query::Format>,
    },
    /// Run a named CONSTRUCT graph view (default: "dependencies").
    Graph {
        name: Option<String>,
        #[arg(long, value_enum)]
        format: Option<clearhead_graphd::query::Format>,
    },
    /// Run a saved freeform query by name.
    Named {
        name: String,
        /// IRI substituted for ?STATUS_FILTER.
        #[arg(long)]
        status: Option<String>,
        #[arg(long, value_enum)]
        format: Option<clearhead_graphd::query::Format>,
    },
    /// Run ad-hoc SPARQL, or a raw WHERE clause via --where.
    Raw {
        sparql: Option<String>,
        #[arg(long)]
        r#where: Option<String>,
        #[arg(long, value_enum)]
        format: Option<clearhead_graphd::query::Format>,
    },
    /// List available named queries.
    List,
    /// Print a named query's SPARQL to stdout.
    Show { name: String },
}

fn main() -> Result<()> {
    init_tracing();
    let cli = Cli::parse();

    match cli.command {
        Command::Query(args) => run_query_command(&cli.workspace, args),
    }
}

/// Self-discover config from the shared core loader, resolving relative
/// `additional_workspaces` against the project (or global) config base.
fn discover_config() -> Result<WorkspaceConfig> {
    let mut config: WorkspaceConfig = clearhead_workspace_fs::config::config_sources(None)
        .build()
        .and_then(|c| c.try_deserialize())
        .map_err(|e| anyhow::anyhow!("Failed to load config: {e}"))?;

    let base = clearhead_workspace_fs::config::find_project_data_dir()
        .map(|root| root.join(".clearhead"))
        .unwrap_or_else(clearhead_workspace_fs::config::get_config_dir);
    config.additional_workspaces = clearhead_workspace_fs::config::resolve_workspace_paths(
        &config.additional_workspaces,
        &base,
    )
    .into_iter()
    .map(|p| p.to_string_lossy().into_owned())
    .collect();
    Ok(config)
}

fn run_query_command(workspace: &Path, args: QueryArgs) -> Result<()> {
    use clearhead_graphd::query;
    let cx = query::QueryContext {
        workspace: workspace.to_path_buf(),
        config: discover_config()?,
        config_dir: clearhead_workspace_fs::config::get_config_dir(),
    };
    match args.kind {
        QueryKind::Index {
            name,
            target,
            format,
        } => query::run_index(&cx, name.as_deref(), target.as_deref(), format),
        QueryKind::Tree { name, format } => query::run_tree(&cx, name.as_deref(), format),
        QueryKind::Graph { name, format } => query::run_graph(&cx, name.as_deref(), format),
        QueryKind::Named {
            name,
            status,
            format,
        } => query::run_named(&cx, &name, status.as_deref(), format),
        QueryKind::Raw {
            sparql,
            r#where,
            format,
        } => query::run_raw(&cx, sparql.as_deref(), r#where.as_deref(), format),
        QueryKind::List => query::list(&cx),
        QueryKind::Show { name } => query::show(&cx, &name),
    }
}

fn init_tracing() {
    let _ = tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "warn".into()),
        )
        .try_init();
}
