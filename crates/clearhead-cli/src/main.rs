use std::any::Any;
use std::io;
use std::panic;
use std::process;
use tracing::{Level, debug, error};
use tracing_subscriber::{EnvFilter, FmtSubscriber};

mod argparser;

mod display;
use argparser::{Verb, parse_cli};

pub mod environment_reader;

mod commands;
use commands::CommandContext;

fn main() {
    // Rust's print macros panic when a downstream consumer closes stdout early
    // (`clearhead read ... | head`). Treat that ordinary Unix pipeline event as
    // successful termination while preserving normal panic reporting.
    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        if !is_broken_pipe_panic(info.payload()) {
            default_hook(info);
        }
    }));

    match panic::catch_unwind(real_main) {
        Ok(()) => {}
        Err(payload) if is_broken_pipe_panic(payload.as_ref()) => {}
        Err(payload) => panic::resume_unwind(payload),
    }
}

fn is_broken_pipe_panic(payload: &(dyn Any + Send)) -> bool {
    payload
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| payload.downcast_ref::<&'static str>().copied())
        .is_some_and(|message| message.contains("failed printing to stdout: Broken pipe"))
}

fn real_main() {
    let cli = parse_cli();

    // Initialize tracing
    let log_level = match cli.debug {
        0 => Level::INFO,
        1 => Level::DEBUG,
        _ => Level::TRACE,
    };

    let subscriber = FmtSubscriber::builder()
        .with_max_level(log_level)
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(io::stderr)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    debug!(debug_level = cli.debug, "Debug mode enabled");
    if let Some(ref config_path) = cli.config {
        debug!(config = ?config_path, "Custom config file specified");
    }

    if let Err(e) = run_command(&cli) {
        // Verb failures are data (query_output.md, "Errors as data"): when a
        // machine is reading stdout, emit the typed result there so a loop can
        // branch on `kind` instead of parsing stderr prose.
        if let Some(verb_err) = e.downcast_ref::<commands::verb_result::VerbError>()
            && !std::io::IsTerminal::is_terminal(&io::stdout())
        {
            println!(
                "{}",
                serde_json::to_string(verb_err).expect("verb error serializes")
            );
            process::exit(1);
        }
        if cli.debug > 0 {
            error!(error = ?e, "Command failed");
        } else {
            // anyhow's Debug format prints the full "Caused by:" chain, not just
            // the top-level message — the whole point of adopting it here.
            eprintln!("Error: {:?}", e);
        }
        process::exit(1);
    }
}

fn run_command(cli: &argparser::Cli) -> anyhow::Result<()> {
    // Init bootstraps the workspace — runs before CommandContext to avoid
    // creating XDG dirs in a directory that isn't yet initialized.
    if let Verb::Init = &cli.command {
        return commands::init::run(cli.config.clone());
    }

    let ctx = CommandContext::new(cli)?;

    debug!(data_dir = %ctx.data_dir.display(), "Data directory resolved");

    dispatch(cli, &ctx)
}

fn dispatch(cli: &argparser::Cli, ctx: &CommandContext) -> anyhow::Result<()> {
    match &cli.command {
        Verb::Read { target } => match target {
            argparser::ReadTarget::Plans {
                format,
                charter,
                recursive,
                file,
                stdio,
                table_options,
            } => commands::plan::read_plans(
                ctx,
                format,
                charter,
                *recursive,
                file,
                *stdio,
                table_options,
            ),
            argparser::ReadTarget::Charters {
                format,
                explicit_only,
            } => commands::charter::read_charters(ctx, format, *explicit_only),
            argparser::ReadTarget::Actions {
                format,
                plan,
                charter,
                context,
                open_only,
                states,
                file,
            } => commands::action::read_actions_cmd(
                ctx,
                *format,
                plan.as_deref(),
                charter.as_deref(),
                context,
                *open_only,
                states,
                file,
            ),
        },
        Verb::Show { target } => match target {
            argparser::ShowTarget::Plan {
                query,
                file,
                format,
                table_options,
            } => commands::plan::show_plan(ctx, query, file, format, table_options),
            argparser::ShowTarget::Action { query, file } => {
                commands::action::show_action(ctx, query, file)
            }
            argparser::ShowTarget::Charter { query } => commands::charter::show_charter(ctx, query),
        },
        Verb::Add { target } => match target {
            argparser::AddTarget::Plan {
                name,
                file,
                charter,
                parent,
                fields,
                schedule,
                dry_run,
            } => commands::plan::add_plan(
                ctx, name, file, charter, parent, fields, schedule, *dry_run,
            ),
            argparser::AddTarget::Action {
                name,
                charter,
                file,
                parent,
                priority,
                state,
                alias,
                description,
                context,
                predecessor,
                sequential,
                scheduled_at,
                duration,
                dry_run,
            } => commands::action::add_action(
                ctx,
                name,
                charter,
                file,
                parent,
                *priority,
                *state,
                alias,
                description,
                context,
                predecessor,
                *sequential,
                scheduled_at,
                *duration,
                *dry_run,
            ),
            argparser::AddTarget::Charter {
                title,
                alias,
                parent,
                template,
                dry_run,
            } => commands::charter::add_charter(ctx, title, alias, parent, template, *dry_run),
        },
        Verb::Update { target } => match target {
            argparser::UpdateTarget::Plan {
                query,
                file,
                name,
                fields,
                schedule,
                dry_run,
            } => commands::plan::update_plan(ctx, query, file, name, fields, schedule, *dry_run),
            argparser::UpdateTarget::Action {
                query,
                name,
                priority,
                state,
                scheduled_at,
                duration,
                description,
                context,
                predecessor,
                sequential,
                charter,
                file,
                dry_run,
            } => commands::action::update_action(
                ctx,
                query,
                name,
                *priority,
                *state,
                scheduled_at,
                duration,
                description,
                context,
                predecessor,
                *sequential,
                charter,
                file,
                *dry_run,
            ),
            argparser::UpdateTarget::Charter {
                query,
                state,
                title,
                alias,
                dry_run,
            } => commands::charter::update_charter(ctx, query, state, title, alias, *dry_run),
        },
        Verb::Complete { target } => match target {
            argparser::CompleteTarget::Plan {
                query,
                file,
                dry_run,
            } => commands::plan::complete_plan(ctx, query, file, *dry_run),
            argparser::CompleteTarget::Action {
                query,
                charter,
                file,
                dry_run,
            } => commands::action::complete_action(ctx, query, charter, file, *dry_run),
        },
        Verb::Delete { target } => match target {
            argparser::DeleteTarget::Plan {
                query,
                file,
                dry_run,
            } => commands::plan::delete_plan(ctx, query, file, *dry_run),
            argparser::DeleteTarget::Action {
                query,
                charter,
                file,
                dry_run,
            } => commands::action::delete_action(ctx, query, charter, file, *dry_run),
        },
        Verb::Transact { file, dry_run } => commands::transact::run(ctx, file, *dry_run),
        Verb::Query { target } => match target {
            argparser::QueryTarget::Raw {
                sparql,
                where_clause,
                format,
            } => commands::query::raw(ctx, sparql.as_deref(), where_clause.as_deref(), *format),
            argparser::QueryTarget::Named {
                name,
                status,
                format,
            } => commands::query::named(ctx, name, status.as_deref(), *format),
            argparser::QueryTarget::Index { name, format } => {
                commands::query::index(ctx, name.as_deref(), *format)
            }
            argparser::QueryTarget::Tree { name, format } => {
                commands::query::tree(ctx, name.as_deref(), *format)
            }
            argparser::QueryTarget::Graph { name, format } => {
                commands::query::graph(ctx, name.as_deref(), *format)
            }
            argparser::QueryTarget::Chain { query, format } => {
                commands::query::chain(ctx, query, *format)
            }
            argparser::QueryTarget::Show { name } => commands::query::show(ctx, name),
            argparser::QueryTarget::List => commands::query::list(ctx),
        },
        Verb::Format { target } => match target {
            argparser::FormatTarget::File {
                path,
                write,
                style,
                indent_style,
                indent_width,
            } => commands::file::format_file(ctx, path, *write, style, indent_style, indent_width),
        },
        Verb::Lint { target } => match target {
            argparser::LintTarget::File { path } => commands::file::lint_file(path),
        },
        Verb::Normalize { target } => match target {
            argparser::NormalizeTarget::File {
                path,
                write,
                no_format,
            } => commands::file::normalize_file(ctx, path, *write, *no_format),
        },
        Verb::Patch { target } => match target {
            argparser::PatchTarget::File {
                primary,
                secondary,
                write,
            } => commands::file::patch_file(primary, secondary, *write),
        },
        Verb::Archive { target } => match target {
            argparser::ArchiveTarget::Plans {
                scope,
                file,
                dry_run,
            } => commands::plan::archive_plans(ctx, scope, file, *dry_run),
            argparser::ArchiveTarget::Actions {
                scope,
                file,
                dry_run,
            } => commands::action::archive_actions(ctx, scope, file, *dry_run),
            argparser::ArchiveTarget::Charter {
                query,
                file,
                closed,
                force,
                dry_run,
            } => commands::charter::archive_charter(ctx, query, file, *closed, *force, *dry_run),
        },
        Verb::Export { target } => match target {
            argparser::ExportTarget::Plans {
                reference,
                output,
                open_only,
                recursive,
            } => commands::plan::export_plans(ctx, reference, output, *open_only, *recursive),
        },
        Verb::Import { target } => match target {
            argparser::ImportTarget::Plans {
                source,
                charter,
                overwrite,
                dry_run,
            } => commands::plan::import_plans(ctx, source, charter, *overwrite, *dry_run),
        },
        Verb::Start { target } => match target {
            argparser::StartTarget::Lsp => commands::service::start_lsp(),
        },
        Verb::Sync { target } => match target {
            argparser::SyncTarget::Events { file, dry_run } => {
                commands::service::sync_events(ctx, file, *dry_run)
            }
            argparser::SyncTarget::Calendar { dry_run, conflict } => {
                commands::service::sync_calendar(ctx, *dry_run, *conflict)
            }
        },
        Verb::Debug => commands::debug::run(ctx),
        Verb::Doctor { json, fix, dry_run } => commands::doctor::run(ctx, *json, *fix, *dry_run),
        Verb::Completion { shell } => {
            use clap::CommandFactory;
            use clap_complete::generate;
            generate(
                *shell,
                &mut argparser::Cli::command(),
                "clearhead",
                &mut io::stdout(),
            );
            Ok(())
        }
        Verb::Cancel { target } => match target {
            argparser::CancelTarget::Action {
                query,
                charter,
                file,
                dry_run,
            } => commands::action::cancel_action(ctx, query, charter, file, *dry_run),
        },
        Verb::Apply { target } => match target {
            argparser::ApplyTarget::Template {
                name,
                charter,
                file,
                dry_run,
            } => commands::template::apply_template(ctx, name, charter, file, *dry_run),
        },
        Verb::Close { target } => match target {
            argparser::CloseTarget::Charter {
                query,
                file,
                dry_run,
            } => commands::charter::close_charter(ctx, query.as_deref(), file.as_deref(), *dry_run),
        },
        Verb::CompleteValues { kind } => commands::complete_values(ctx, *kind),
        Verb::Init => unreachable!("handled before CommandContext construction"),
    }
}
