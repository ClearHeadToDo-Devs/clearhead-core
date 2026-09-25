use anyhow::Context;
use tracing::{debug, info, warn};

use crate::cli::{CommandContext, load_file_for_read};
use clearhead_cli::telemetry::{TelemetryEvent, TelemetryRecord, Tool, emit};
use clearhead_core::{Reconcile, SyncEntry};

/// Compatibility shim for the standalone `clearhead-lsp` process.
///
/// stdin/stdout/stderr are inherited so the child speaks LSP directly to the
/// editor. On Unix `exec` replaces the CLI process entirely; other platforms
/// wait for the external server and return its exit status.
pub fn start_lsp() -> anyhow::Result<()> {
    let executable = std::env::var_os("CLEARHEAD_LSP").unwrap_or_else(|| "clearhead-lsp".into());
    info!(server = ?executable, "Delegating to standalone Language Server");

    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        let error = std::process::Command::new(&executable).exec();
        Err(error).with_context(|| format!("Failed to exec {:?}", executable))
    }

    #[cfg(not(unix))]
    {
        let status = std::process::Command::new(&executable)
            .status()
            .with_context(|| format!("Failed to start {:?}", executable))?;
        if !status.success() {
            anyhow::bail!("clearhead-lsp exited with {status}");
        }
        Ok(())
    }
}

pub fn sync_events(
    ctx: &CommandContext,
    file: &Option<std::path::PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let input_file = ctx.resolve_action_file(file.as_ref());
    debug!(input_file = %input_file.display(), dry_run = dry_run, "Executing Sync Events");

    let actions = load_file_for_read(&input_file, "sync events")?;
    let mut sync_count = 0;
    let skip_count = 0; // TODO: track which events already exist

    for action in &actions {
        let uuid_str = action.id.to_string();

        if dry_run {
            println!("Would sync: {} #{}", action.name, uuid_str);
        } else {
            let timestamp = action
                .created_at
                .map(|dt| dt.with_timezone(&chrono::Utc))
                .unwrap_or_else(chrono::Utc::now);

            let record = TelemetryRecord::with_timestamp(
                timestamp,
                Tool::Cli,
                Some(uuid_str.clone()),
                TelemetryEvent::ActionCreated {
                    name: action.name.clone(),
                    file_path: input_file.display().to_string(),
                },
            );

            if let Err(e) = emit(record) {
                warn!(error = %e, "Failed to emit backfill event");
            }

            debug!(action_uuid = %uuid_str, "Backfilled event for action");
        }
        sync_count += 1;
    }

    if dry_run {
        info!(sync_count, skip_count, "SyncEvents dry run complete");
        println!(
            "Dry run complete. {} actions to sync, {} already present.",
            sync_count, skip_count
        );
    } else {
        info!(sync_count, skip_count, "SyncEvents complete");
        println!(
            "Sync complete. {} events backfilled, {} already present.",
            sync_count, skip_count
        );
    }
    Ok(())
}

pub fn sync_calendar(
    ctx: &CommandContext,
    dry_run: bool,
    conflict: Option<crate::argparser::ConflictResolutionArg>,
    action: Option<String>,
) -> anyhow::Result<()> {
    ctx.require_source_integrity("sync calendar")?;
    let choice = conflict_choice(conflict, action);

    if dry_run {
        let preview = clearhead_cli::filesystem::preview_calendar_sync_with_component(
            &ctx.data_dir,
            choice.as_ref(),
            ctx.config.plan_component,
        )?;
        render_sync_report(&preview.report);
        let tally = preview.report.tally();
        info!(?tally, "Calendar sync dry run complete");
        println!(
            "Dry run complete. {} push, {} pull, {} converged, {} conflict.",
            tally.take_action, tally.take_calendar, tally.converged, tally.conflict
        );
        exit_if_unresolved(tally.conflict);
        return Ok(());
    }

    let result = clearhead_cli::filesystem::sync_calendar_with_component(
        &ctx.data_dir,
        choice.as_ref(),
        ctx.config.plan_component,
    )?;

    // Name the collections this sync materialized. A collection directory is a
    // stable path key, so the readable name lives in the vdir metadata file
    // that calendar clients and `vdirsyncer metasync` read. Done before the
    // "already in sync" early return, which is exactly the case where the vdir
    // exists but has never been named.
    let named = ctx.refresh_collection_displaynames();
    if named > 0 {
        debug!(
            collections = named,
            "Refreshed vdir collection display names"
        );
    }

    render_sync_report(&result.report);
    let rolled_forward = result.rolled_forward;

    if result.report.is_empty() {
        if rolled_forward > 0 {
            println!(
                "Ingested {rolled_forward} occurrence completion(s) from a calendar roll-forward."
            );
        } else {
            println!("Already in sync.");
        }
        return Ok(());
    }

    let applied = result.applied;
    info!(?applied, rolled_forward, "Calendar sync complete");
    println!(
        "Sync complete. {} push, {} pull, {} converged, {} conflict.",
        applied.take_action, applied.take_calendar, applied.converged, applied.conflict
    );
    if rolled_forward > 0 {
        println!(
            "Ingested {rolled_forward} occurrence completion(s) from a calendar roll-forward."
        );
    }
    exit_if_unresolved(applied.conflict);
    Ok(())
}

/// Exit status for a sync that finished but left conflicts for a person to
/// decide, distinct from `1` for a sync that could not run.
const UNRESOLVED_CONFLICTS: i32 = 2;

/// Fail a sync that left conflicts unresolved, so automation notices them.
fn exit_if_unresolved(conflicts: usize) {
    if conflicts > 0 {
        eprintln!(
            "{conflicts} conflict(s) left unresolved; choose a side with `clearhead sync calendar --conflict action|calendar [--action <id>]`"
        );
        std::process::exit(UNRESOLVED_CONFLICTS);
    }
}

fn render_sync_report(report: &clearhead_core::SyncReport) {
    for warning in &report.warnings {
        eprintln!("{}", warning);
    }
    for import in &report.imports {
        println!(
            "pull calendar → new action: {} #{} ({})",
            import.action.title, import.action.id, import.charter_name
        );
    }
    for entry in &report.entries {
        println!("{}", render_sync_entry(entry));
    }
    for lifecycle in &report.lifecycle {
        match lifecycle.kind {
            clearhead_core::SyncLifecycleKind::CalendarDeleted => println!(
                "pull calendar → action unscheduled: #{}",
                lifecycle.action_id
            ),
            clearhead_core::SyncLifecycleKind::ActionUnscheduled => println!(
                "push action → calendar Plan removed: #{}",
                lifecycle.action_id
            ),
        }
    }
}

fn conflict_choice(
    side: Option<crate::argparser::ConflictResolutionArg>,
    action: Option<String>,
) -> Option<clearhead_core::SyncConflictChoice> {
    let prefer = match side? {
        crate::argparser::ConflictResolutionArg::Action => {
            clearhead_core::SyncConflictResolution::PreferAction
        }
        crate::argparser::ConflictResolutionArg::Calendar => {
            clearhead_core::SyncConflictResolution::PreferCalendar
        }
    };
    Some(clearhead_core::SyncConflictChoice { prefer, action })
}

fn render_sync_entry(entry: &SyncEntry) -> String {
    let mut changes = Vec::new();
    render_field("scheduled_at", &entry.scheduled_at, &mut changes);
    render_field("due_date", &entry.due_date, &mut changes);
    render_field("state", &entry.state, &mut changes);
    render_field("title", &entry.title, &mut changes);
    render_field("description", &entry.description, &mut changes);
    render_field("priority", &entry.priority, &mut changes);
    render_field("contexts", &entry.contexts, &mut changes);
    format!(
        "{}: {} #{}",
        changes.join(", "),
        entry.name,
        entry.action_id
    )
}

fn render_field<T: std::fmt::Debug>(name: &str, outcome: &Reconcile<T>, output: &mut Vec<String>) {
    let text = match outcome {
        Reconcile::NoOp => return,
        Reconcile::TakeAction(_) => format!("push action → calendar {name}"),
        Reconcile::TakeCalendar(_) => format!("pull calendar → action {name}"),
        Reconcile::Converged(_) => format!("converged {name}"),
        Reconcile::Conflict { action, calendar } => {
            format!("conflict {name} action={action:?} calendar={calendar:?}")
        }
    };
    output.push(text);
}
