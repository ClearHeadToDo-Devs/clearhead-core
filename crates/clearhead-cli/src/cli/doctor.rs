//! `clearhead doctor` — workspace fsck and explicit repair.
//!
//! Diagnosis remains read-only by default. `--fix` removes states doctor can
//! prove have no workspace owner: stale sidecar metadata and unowned calendar
//! collections. Removing a vdir collection may propagate through vdirsyncer.
//! It also mirrors the root README's id into a conflicting root sidecar when
//! nothing references the replaced id, and renames calendar collections whose
//! display name drifted from their charter's alias.

use crate::cli::CommandContext;
use anyhow::Context;
use clearhead_core::workspace::{Diagnosis, DoctorRepair, FindingSeverity};

pub fn run(ctx: &CommandContext, json: bool, fix: bool, dry_run: bool) -> anyhow::Result<()> {
    let mut diagnosis =
        clearhead_cli::filesystem::diagnose_workspace(&ctx.data_dir).context("doctor")?;

    if fix {
        repair_unowned_state(ctx, &diagnosis, dry_run)?;
        if dry_run {
            return Ok(());
        }
        diagnosis = clearhead_cli::filesystem::diagnose_workspace(&ctx.data_dir)
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
        println!("Nothing for doctor --fix to repair.");
        return Ok(());
    }

    if !dry_run {
        clearhead_cli::filesystem::apply_doctor_repairs(&ctx.data_dir, &diagnosis.repairs)
            .context("apply doctor repairs")?;
    }

    let mut entries = 0;
    let mut files = 0;
    let mut collections = 0;
    let mut identities = 0;
    let mut names = 0;
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
            DoctorRepair::MirrorRootCharterId { path, id, .. } => {
                identities += 1;
                println!(
                    "{} root charter id {} in {}",
                    if dry_run { "Would mirror" } else { "Mirrored" },
                    id,
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
            DoctorRepair::WriteCollectionDisplayname { location, name, .. } => {
                names += 1;
                println!(
                    "{} calendar collection {} as '{}'",
                    if dry_run { "Would name" } else { "Named" },
                    location.path,
                    name
                );
            }
        }
    }
    if dry_run {
        println!(
            "Dry run: {} entr{}, {} file(s), and {} calendar collection(s) would be removed; {} root charter id(s) would be mirrored; {} collection name(s) would be refreshed.",
            entries,
            if entries == 1 { "y" } else { "ies" },
            files,
            collections,
            identities,
            names
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
