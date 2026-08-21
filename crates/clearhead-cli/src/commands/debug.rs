use crate::commands::CommandContext;
use anyhow::Context;
use clearhead_core::workspace::{MarkdownCharter, diagnose_read, read_workspace_with_plans};

pub fn run(ctx: &CommandContext) -> anyhow::Result<()> {
    print_config_section(ctx);
    println!();
    print_workspace_section(ctx)
}

fn print_config_section(ctx: &CommandContext) {
    println!("config");
    println!(
        "  global_config_file: {}{}",
        ctx.config_path.display(),
        if ctx.config_path.exists() {
            ""
        } else {
            " (not found)"
        },
    );

    // Project config — written by `clearhead init`, layered on top of the global
    // config and overrides workspace-specific settings (workspace_id, etc.).
    // config.local.json sits beside it as a git-ignored personal override that
    // wins over the committed project config.
    if let Some(ref root) = ctx.project_root {
        let project_cfg = root.join(".clearhead").join("config.json");
        println!(
            "  project_config_file: {}{}",
            project_cfg.display(),
            if project_cfg.exists() {
                " (active)"
            } else {
                " (not found — run `clearhead init`)"
            },
        );
        let local_cfg = root.join(".clearhead").join("config.local.json");
        println!(
            "  project_local_config_file: {}{}",
            local_cfg.display(),
            if local_cfg.exists() {
                " (active)"
            } else {
                " (not present — optional personal override)"
            },
        );
    } else {
        println!("  project_config_file: none (not inside a clearhead workspace)");
    }

    println!(
        "  data_dir: {}  [override: CLEARHEAD_DATA_DIR | {}]",
        display_config_value(&ctx.config.data_dir, "<project-root-or-xdg-default>"),
        ctx.config_path.display()
    );
    println!(
        "  config_dir: {}  [override: CLEARHEAD_CONFIG_DIR | {}]",
        display_config_value(&ctx.config.config_dir, "<xdg-config-default>"),
        ctx.config_path.display()
    );
    println!(
        "  default_file: {}  [override: CLEARHEAD_DEFAULT_FILE | {}]",
        ctx.config.default_file,
        ctx.config_path.display()
    );

    // Workspace identity — a property of the workspace, read from its manifest
    // (workspace.json) rather than the layered config.
    let manifest = clearhead_core::workspace::WorkspaceManifest::read(&ctx.data_dir);
    match &manifest.workspace_id {
        Some(id) => println!(
            "  workspace_id: {}  (name: {})",
            id,
            manifest.workspace_name.as_deref().unwrap_or("<unnamed>")
        ),
        None => {
            println!("  workspace_id: <unset> — run `clearhead init` to assign a stable graph URI")
        }
    }

    if !ctx.config.additional_workspaces.is_empty() {
        println!("  additional_workspaces:");
        for path in &ctx.config.additional_workspaces {
            println!("    - {}", path);
        }
    }

    println!(
        "  plan_path: {}",
        ctx.config
            .plan_path
            .as_deref()
            .map(|p| format!(
                "{}  [override: CLEARHEAD_PLAN_PATH | project config.local.json]",
                p
            ))
            .unwrap_or_else(|| "<unset> — plans live under the workspace's own plans/".to_string()),
    );
}

fn print_workspace_section(ctx: &CommandContext) -> anyhow::Result<()> {
    println!("workspace");

    let workspace_source = resolve_workspace_source(ctx);
    let data_root = clearhead_core::workspace_data_root(&ctx.data_dir);
    println!(
        "  resolved_data_root: {} ({})",
        data_root.display(),
        workspace_source
    );

    let manifest = clearhead_core::collect_workspace_manifest(&ctx.data_dir)
        .context("Failed to collect workspace manifest")?;
    // Diagnostics must observe, not alter: the pure reader (no journal replay,
    // per-file failures become findings) instead of the healing load path.
    let read = read_workspace_with_plans(&ctx.data_dir, ctx.plan_override().as_deref())
        .context("Failed to read workspace")?;

    let root_alias = find_root_charter_alias(&read.charters).unwrap_or("-".to_string());
    println!("  root_charter: {}", root_alias);

    if manifest.is_empty() {
        println!("  charters: none discovered");
    } else {
        println!("  charters:");
        for entry in manifest {
            println!(
                "    - alias={} file={} parent={} source={}",
                entry.charter_name,
                entry.path,
                entry.inferred_parent.unwrap_or_else(|| "-".to_string()),
                format_source_type(&entry.source_type)
            );
        }
    }

    let charter_count = read.charters.len();
    let plan_count: usize = read.charters.iter().map(|c| c.plans.len()).sum();
    let action_count: usize = read.charters.iter().map(|c| c.actions.len()).sum();

    let diagnosis = diagnose_read(&ctx.data_dir, &read);
    println!(
        "  graph_summary: {} charters | {} plans | {} actions | {} violations, {} warnings",
        charter_count,
        plan_count,
        action_count,
        diagnosis.violations(),
        diagnosis.warnings()
    );
    if !diagnosis.findings.is_empty() {
        println!("  findings: run `clearhead doctor` for the full report");
    }

    Ok(())
}

fn display_config_value(value: &str, fallback_label: &str) -> String {
    if value.is_empty() {
        fallback_label.to_string()
    } else {
        value.to_string()
    }
}

fn resolve_workspace_source(ctx: &CommandContext) -> &'static str {
    // Mirrors Workspace Resolution (specifications/configuration.md): a
    // detected project wins unless default_to_user_scope bypasses it; env and
    // config data_dir only relocate the fallback user workspace.
    if ctx.project_root.is_some() && !ctx.config.default_to_user_scope {
        "cwd-walk"
    } else if std::env::var("CLEARHEAD_DATA_DIR").is_ok() {
        "env"
    } else if !ctx.config.data_dir.is_empty() {
        "config"
    } else {
        "xdg-default"
    }
}

fn find_root_charter_alias(charters: &[MarkdownCharter]) -> Option<String> {
    charters
        .iter()
        .find(|charter| charter.parent.is_none())
        .map(|charter| {
            charter
                .alias
                .clone()
                .unwrap_or_else(|| charter.title.clone())
        })
}

fn format_source_type(source_type: &clearhead_core::ManifestSourceType) -> &'static str {
    match source_type {
        clearhead_core::ManifestSourceType::Actions => "actions",
        clearhead_core::ManifestSourceType::Markdown => "markdown",
        clearhead_core::ManifestSourceType::Ics => "ics",
        clearhead_core::ManifestSourceType::ActionsPlusMarkdown => "actions+markdown",
        clearhead_core::ManifestSourceType::ActionsPlusIcs => "actions+ics",
        clearhead_core::ManifestSourceType::MarkdownPlusIcs => "markdown+ics",
        clearhead_core::ManifestSourceType::ActionsPlusMarkdownPlusIcs => "actions+markdown+ics",
    }
}
