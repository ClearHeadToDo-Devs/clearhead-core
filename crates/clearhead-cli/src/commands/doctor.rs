//! `clearhead doctor` — workspace fsck and explicit repair.
//!
//! Diagnosis remains read-only by default. `--fix` removes states doctor can
//! prove have no workspace owner: stale sidecar metadata and unowned calendar
//! collections. Removing a vdir collection may propagate through vdirsyncer.

use crate::commands::CommandContext;
use anyhow::Context;
use clearhead_core::workspace::{Diagnosis, DoctorRepair, FindingSeverity};

pub fn run(ctx: &CommandContext, json: bool, fix: bool, dry_run: bool) -> anyhow::Result<()> {
    let mut diagnosis =
        clearhead_workspace_fs::diagnose_workspace(&ctx.data_dir, ctx.plan_override().as_deref())
            .context("doctor")?;

    if fix {
        repair_unowned_state(ctx, &diagnosis, dry_run)?;
        if dry_run {
            return Ok(());
        }
        diagnosis = clearhead_workspace_fs::diagnose_workspace(
            &ctx.data_dir,
            ctx.plan_override().as_deref(),
        )
        .context("doctor after repair")?;
    }

    if json {
        println!("{}", serde_json::to_string_pretty(&diagnosis)?);
    } else {
        print_report(&diagnosis);
    }

    match (diagnosis.violations(), diagnosis.warnings()) {
        (0, 0) => Ok(()),
        (0, _) => std::process::exit(1),
        (_, _) => std::process::exit(2),
    }
}

fn repair_unowned_state(
    ctx: &CommandContext,
    diagnosis: &Diagnosis,
    dry_run: bool,
) -> anyhow::Result<()> {
    if diagnosis.repairs.is_empty() {
        println!("No fixable unowned state found.");
        return Ok(());
    }

    if !dry_run {
        clearhead_workspace_fs::apply_doctor_repairs(
            &ctx.data_dir,
            ctx.plan_override().as_deref(),
            &diagnosis.repairs,
        )
        .context("apply doctor repairs")?;
    }

    let mut entries = 0;
    let mut files = 0;
    let mut collections = 0;
    for repair in &diagnosis.repairs {
        match repair {
            DoctorRepair::PruneSidecarEntry { path, id, .. } => {
                entries += 1;
                println!(
                    "{} sidecar entry {} from {}",
                    if dry_run { "Would prune" } else { "Pruned" },
                    id,
                    path
                );
            }
            DoctorRepair::RemoveSidecar { path, .. } => {
                files += 1;
                println!(
                    "{} orphaned sidecar {}",
                    if dry_run { "Would remove" } else { "Removed" },
                    path
                );
            }
            DoctorRepair::RemovePlansCollection { location, .. } => {
                collections += 1;
                println!(
                    "{} unowned calendar collection {} (vdirsyncer may propagate this deletion)",
                    if dry_run { "Would remove" } else { "Removed" },
                    location.path
                );
            }
        }
    }
    if dry_run {
        println!(
            "Dry run: {} entr{}, {} file(s), and {} calendar collection(s) would be removed.",
            entries,
            if entries == 1 { "y" } else { "ies" },
            files,
            collections
        );
        return Ok(());
    }

    Ok(())
}

fn print_report(diagnosis: &Diagnosis) {
    println!(
        "checked {} charters, {} actions",
        diagnosis.checked_charters, diagnosis.checked_actions
    );

    if diagnosis.findings.is_empty() {
        println!("workspace clean");
        return;
    }

    for severity in [FindingSeverity::Violation, FindingSeverity::Warning] {
        let group: Vec<_> = diagnosis
            .findings
            .iter()
            .filter(|f| f.severity == severity)
            .collect();
        if group.is_empty() {
            continue;
        }
        let label = match severity {
            FindingSeverity::Violation => "violations",
            FindingSeverity::Warning => "warnings",
        };
        println!("\n{} ({})", label, group.len());
        for finding in group {
            println!("  [{}] {}", finding.path.display(), finding.code);
            for line in finding.message.lines() {
                println!("    {}", line.trim_start());
            }
        }
    }

    println!(
        "\n{} violation(s), {} warning(s)",
        diagnosis.violations(),
        diagnosis.warnings()
    );
}
