use std::path::PathBuf;

use anyhow::Context;
use tracing::info;
use uuid::Uuid;

use clearhead_core::workspace::{read_actions, templates};

use super::CommandContext;

pub fn apply_template(
    ctx: &CommandContext,
    name: &str,
    charter: &Option<String>,
    file: &Option<PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let actions_path = if let Some(f) = file {
        f.clone()
    } else if let Some(c) = charter {
        super::charter_to_file_path(&ctx.data_dir, c)?
    } else {
        ctx.resolve_action_file(None)
    };

    let charter_dir = actions_path.parent().unwrap_or(std::path::Path::new(""));
    let data_root = clearhead_core::workspace_data_root(&ctx.data_dir);

    let tpl_path = templates::resolve_template(charter_dir, &data_root, name)
        .context("Failed to resolve template")?
        .ok_or_else(|| anyhow::anyhow!("Template '{}' not found", name))?;

    let tpl_acts = read_actions(&tpl_path).with_context(|| {
        format!(
            "Failed to read template '{}' at {}",
            name,
            tpl_path.display(),
        )
    })?;

    if tpl_acts.is_empty() {
        anyhow::bail!("Template '{}' is empty", name);
    }

    let instantiated = templates::instantiate_template(&tpl_acts, |_| Uuid::now_v7(), None);

    if dry_run {
        println!(
            "Would apply template '{}': {} action(s) to {}",
            name,
            instantiated.len(),
            actions_path.display()
        );
        return Ok(());
    }

    let mut existing = super::load_file_for_mutation(&actions_path, "apply template")?;
    existing.extend(instantiated.iter().cloned());
    super::save_file(&actions_path, &existing)?;

    info!(template = %name, count = instantiated.len(), path = %actions_path.display(), "Template applied");
    println!(
        "Applied template '{}': added {} action(s) to {}",
        name,
        instantiated.len(),
        actions_path.display()
    );
    Ok(())
}
