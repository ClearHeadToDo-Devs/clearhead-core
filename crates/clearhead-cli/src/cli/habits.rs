use anyhow::Context;

use super::CommandContext;

const BATCH_SIZE: usize = 100;

pub fn run(ctx: &CommandContext, dry_run: bool) -> anyhow::Result<()> {
    let workspaces;
    let root = if let Some(filter) = &ctx.workspace_filter {
        workspaces = ctx.workspace_dirs();
        match workspaces.as_slice() {
            [(_, path)] => path,
            [] => anyhow::bail!("no workspace matches '{filter}'"),
            _ => {
                anyhow::bail!("'{filter}' matches multiple workspaces; select one to apply Habits")
            }
        }
    } else {
        &ctx.data_dir
    };
    let now = chrono::Local::now().fixed_offset();
    let result = clearhead_cli::filesystem::habits::apply_habits(root, now, BATCH_SIZE, dry_run)
        .with_context(|| format!("applying Habits in {}", root.display()))?;
    let verb = if dry_run {
        "Would materialize"
    } else {
        "Materialized"
    };
    println!(
        "{verb} {} Habit occurrence(s) ({} action(s))",
        result.occurrences, result.actions
    );
    if result.has_backlog {
        println!("More Habit slots are due. Run the same command again to continue.");
    }
    for refusal in &result.refused {
        eprintln!("error: {refusal}");
    }
    if !result.refused.is_empty() {
        anyhow::bail!(
            "{} Habit definition(s) could not be applied",
            result.refused.len()
        );
    }
    Ok(())
}
