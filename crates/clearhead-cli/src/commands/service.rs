use anyhow::Context;
use tracing::{debug, info, warn};

use crate::commands::{CommandContext, load_file_for_read};
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
) -> anyhow::Result<()> {
    ctx.require_source_integrity("sync calendar")?;
    let plans_root = ctx.plans_root();
    // Sync reconciles owned, standalone artifacts only. Occurrences never sync as
    // standalone VTODOs — they ride their master's RRULE + deviations. The loaded
    // model is materialized-only (occurrences are never projected into it), and
    // `plan_sync` additionally excludes the materialized single-token occurrence
    // and its grafted template subtree from standalone reconciliation.
    let model = ctx.load_model()?;
    let sync_store = clearhead_core::read_plans_sync_store(&ctx.data_dir, &plans_root)?;
    let calendar_actions = clearhead_core::read_vtodo_actions(&plans_root)?;
    let report = clearhead_core::plan_sync(&model, &sync_store, &calendar_actions)?;
    let report = resolve_conflicts(report, conflict);

    // Ingest foreign roll-forwards on recurring masters (a camp-B client advancing
    // a master's DTSTART to mean "completed"). Independent of the standalone
    // reconcile above, and a real write — so it runs on non-dry-run syncs only.
    let rolled_forward = if dry_run {
        0
    } else {
        clearhead_core::sync_master_rollforwards(&ctx.data_dir, ctx.plan_override().as_deref())?
    };

    if report.is_empty() {
        if !dry_run {
            clearhead_core::apply_sync(&ctx.data_dir, ctx.plan_override().as_deref(), &report)?;
        }
        if rolled_forward > 0 {
            println!(
                "Ingested {rolled_forward} occurrence completion(s) from a calendar roll-forward."
            );
        } else {
            println!("Already in sync.");
        }
        return Ok(());
    }

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

    let tally = report.tally();
    if dry_run {
        info!(?tally, "Calendar sync dry run complete");
        println!(
            "Dry run complete. {} push, {} pull, {} converged, {} conflict.",
            tally.take_action, tally.take_calendar, tally.converged, tally.conflict
        );
        return Ok(());
    }

    let applied =
        clearhead_core::apply_sync(&ctx.data_dir, ctx.plan_override().as_deref(), &report)?;
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
    Ok(())
}

fn resolve_conflicts(
    mut report: clearhead_core::SyncReport,
    choice: Option<crate::argparser::ConflictResolutionArg>,
) -> clearhead_core::SyncReport {
    let Some(choice) = choice else {
        return report;
    };

    for entry in &mut report.entries {
        resolve_one(&mut entry.scheduled_at, choice);
        resolve_one(&mut entry.due_date, choice);
        resolve_one(&mut entry.state, choice);
        resolve_one(&mut entry.title, choice);
        resolve_one(&mut entry.description, choice);
        resolve_one(&mut entry.priority, choice);
        resolve_one(&mut entry.contexts, choice);
    }

    report
}

fn resolve_one<T: Clone>(
    outcome: &mut Reconcile<T>,
    choice: crate::argparser::ConflictResolutionArg,
) {
    if let Reconcile::Conflict { action, calendar } = outcome.clone() {
        *outcome = match choice {
            crate::argparser::ConflictResolutionArg::Action => Reconcile::TakeAction(action),
            crate::argparser::ConflictResolutionArg::Calendar => Reconcile::TakeCalendar(calendar),
        };
    }
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
