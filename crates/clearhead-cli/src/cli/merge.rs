use anyhow::{Context, anyhow, bail};
use clearhead_core::workspace::actions::merge::{SourceMerge, merge_source, validate_line_merge};
use std::path::Path;
use std::process::Command;

use crate::filesystem::durability::atomic_write;

pub(super) fn run(base: &Path, ours: &Path, theirs: &Path) -> anyhow::Result<()> {
    let read = |path: &Path| {
        std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))
    };
    let ours_text = read(ours)?;
    let unchanged = || format!("{} left unchanged", ours.display());
    let (text, conflicts) = match merge_source(&read(base)?, &ours_text, &read(theirs)?)
        .map_err(|err| anyhow!(err))
        .with_context(unchanged)?
    {
        SourceMerge::Merged { text, conflicts } => (text, conflicts.len()),
        SourceMerge::NeedsLineMerge => {
            git_merge_file(base, ours, theirs).with_context(unchanged)?
        }
    };
    if text != ours_text {
        atomic_write(ours, text).with_context(|| format!("writing {}", ours.display()))?;
    }
    if conflicts != 0 {
        bail!("{conflicts} merge conflict(s) in {}", ours.display());
    }
    Ok(())
}

fn git_merge_file(base: &Path, ours: &Path, theirs: &Path) -> anyhow::Result<(String, usize)> {
    let output = Command::new("git")
        .args([
            "merge-file",
            "-p",
            "-L",
            "ours",
            "-L",
            "base",
            "-L",
            "theirs",
            "--",
        ])
        .args([ours, base, theirs])
        .output()
        .context("running git merge-file for actions without a UUID")?;
    let Some(code @ 0..=127) = output.status.code() else {
        bail!(
            "git merge-file failed ({}): {}",
            output.status,
            String::from_utf8_lossy(&output.stderr).trim()
        );
    };
    let text = String::from_utf8(output.stdout).context("reading git merge-file output")?;
    validate_line_merge(&text, code as usize).map_err(|err| anyhow!(err))?;
    Ok((text, code as usize))
}
