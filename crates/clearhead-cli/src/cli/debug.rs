//! `clearhead debug` — a snapshot of config resolution and workspace
//! discovery for diagnosing "why is clearhead reading from there".
//!
//! Follows the same output rule as `orient`/`index`: build one
//! serde-serializable report, then render it as a human table at a terminal
//! or as JSON when piped (std::io::IsTerminal). Human output is unchanged
//! from before the report struct existed — see the byte-identical test in
//! `tests/debug.rs`.

use std::io::{IsTerminal, Write};

use anyhow::Context;
use serde::Serialize;

use crate::cli::CommandContext;
use clearhead_core::workspace::MarkdownCharter;

#[derive(Serialize)]
pub struct DebugReport {
    pub config: ConfigSection,
    pub workspace: WorkspaceSection,
}

#[derive(Serialize)]
pub struct FileStatus {
    pub path: String,
    pub found: bool,
}

#[derive(Serialize)]
pub struct ProjectFileStatus {
    pub path: String,
    pub active: bool,
}

#[derive(Serialize)]
pub struct ConfigSection {
    pub global_config_file: FileStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub project_config_file: Option<ProjectFileStatus>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub project_local_config_file: Option<ProjectFileStatus>,
    pub data_dir: String,
    pub config_dir: String,
    pub default_file: String,
    pub plan_component: String,
    pub additional_workspaces: Vec<String>,
}

#[derive(Serialize)]
pub struct CharterEntry {
    pub alias: String,
    pub file: String,
    pub parent: String,
    pub source: String,
}

#[derive(Serialize)]
pub struct GraphSummary {
    pub charters: usize,
    pub plans: usize,
    pub actions: usize,
    pub violations: usize,
    pub warnings: usize,
}

#[derive(Serialize)]
pub struct WorkspaceSection {
    pub resolved_data_root: String,
    pub resolution: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace_name: Option<String>,
    pub root_charter: String,
    pub charters: Vec<CharterEntry>,
    pub graph_summary: GraphSummary,
    /// Rendering-only: not part of the debug JSON contract.
    #[serde(skip)]
    pub plans_root: String,
    #[serde(skip)]
    pub has_findings: bool,
}

pub fn run(ctx: &CommandContext) -> anyhow::Result<()> {
    let report = build(ctx)?;
    if std::io::stdout().is_terminal() {
        print_report(&report, &mut std::io::stdout())?;
    } else {
        println!("{}", serde_json::to_string_pretty(&report)?);
    }
    Ok(())
}

fn build(ctx: &CommandContext) -> anyhow::Result<DebugReport> {
    Ok(DebugReport {
        config: build_config_section(ctx),
        workspace: build_workspace_section(ctx)?,
    })
}

fn build_config_section(ctx: &CommandContext) -> ConfigSection {
    // Project config — written by `clearhead init`, layered on top of the global
    // config and overrides workspace-specific settings (workspace_id, etc.).
    // config.local.json sits beside it as a git-ignored personal override that
    // wins over the committed project config.
    let (project_config_file, project_local_config_file) = match &ctx.project_root {
        Some(root) => {
            let project_cfg = root.join(".clearhead").join("config.json");
            let local_cfg = root.join(".clearhead").join("config.local.json");
            (
                Some(ProjectFileStatus {
                    active: project_cfg.exists(),
                    path: project_cfg.display().to_string(),
                }),
                Some(ProjectFileStatus {
                    active: local_cfg.exists(),
                    path: local_cfg.display().to_string(),
                }),
            )
        }
        None => (None, None),
    };

    ConfigSection {
        global_config_file: FileStatus {
            found: ctx.config_path.exists(),
            path: ctx.config_path.display().to_string(),
        },
        project_config_file,
        project_local_config_file,
        data_dir: display_config_value(&ctx.config.data_dir, "<project-root-or-xdg-default>"),
        config_dir: display_config_value(&ctx.config.config_dir, "<xdg-config-default>"),
        default_file: ctx.config.default_file.clone(),
        plan_component: ctx.config.plan_component.to_string(),
        additional_workspaces: ctx.config.additional_workspaces.clone(),
    }
}

fn build_workspace_section(ctx: &CommandContext) -> anyhow::Result<WorkspaceSection> {
    let workspace_source = resolve_workspace_source(ctx);
    let data_root = clearhead_cli::filesystem::workspace_data_root(&ctx.data_dir);

    let manifest = clearhead_cli::filesystem::collect_workspace_manifest(&ctx.data_dir)
        .context("Failed to collect workspace manifest")?;
    // Diagnostics must observe, not alter: the pure reader (no journal replay,
    // per-file failures become findings) instead of the healing load path.
    let read = clearhead_cli::filesystem::read_workspace(&ctx.data_dir)
        .context("Failed to read workspace")?;

    let root_charter = find_root_charter_alias(&read.charters).unwrap_or_else(|| "-".to_string());

    let charters = manifest
        .into_iter()
        .map(|entry| CharterEntry {
            alias: entry.charter_name,
            file: entry.path,
            parent: entry.inferred_parent.unwrap_or_else(|| "-".to_string()),
            source: format_source_type(&entry.source_type).to_string(),
        })
        .collect();

    let charter_count = read.charters.len();
    let plan_count: usize = read.charters.iter().map(|c| c.plans.len()).sum();
    let action_count: usize = read.charters.iter().map(|c| c.actions.len()).sum();

    let diagnosis = clearhead_cli::filesystem::diagnose_workspace_read(&ctx.data_dir, &read)
        .context("Failed to diagnose workspace")?;

    let workspace_manifest = clearhead_cli::filesystem::read_workspace_manifest(&ctx.data_dir);

    Ok(WorkspaceSection {
        resolved_data_root: data_root.display().to_string(),
        resolution: workspace_source.to_string(),
        workspace_id: workspace_manifest.workspace_id,
        workspace_name: workspace_manifest.workspace_name,
        root_charter,
        charters,
        graph_summary: GraphSummary {
            charters: charter_count,
            plans: plan_count,
            actions: action_count,
            violations: diagnosis.violations(),
            warnings: diagnosis.warnings(),
        },
        plans_root: ctx.plans_root().display().to_string(),
        has_findings: !diagnosis.findings.is_empty(),
    })
}

/// Reproduces the pre-refactor `println!` sequence exactly — pinned byte for
/// byte by `human_report_is_pinned_byte_for_byte` below — pulling each line's
/// data from wherever the report struct now keeps it; line order does not
/// follow the JSON's section boundaries because `workspace_id` moved to
/// `WorkspaceSection`. Takes a generic sink so the pin can assert against a
/// buffer instead of real stdout.
fn print_report(report: &DebugReport, w: &mut impl Write) -> anyhow::Result<()> {
    let config = &report.config;
    let workspace = &report.workspace;

    writeln!(w, "config")?;
    writeln!(
        w,
        "  global_config_file: {}{}",
        config.global_config_file.path,
        if config.global_config_file.found {
            ""
        } else {
            " (not found)"
        },
    )?;

    match &config.project_config_file {
        Some(status) => {
            writeln!(
                w,
                "  project_config_file: {}{}",
                status.path,
                if status.active {
                    " (active)"
                } else {
                    " (not found — run `clearhead init`)"
                },
            )?;
            let local = config
                .project_local_config_file
                .as_ref()
                .expect("project_local_config_file set alongside project_config_file");
            writeln!(
                w,
                "  project_local_config_file: {}{}",
                local.path,
                if local.active {
                    " (active)"
                } else {
                    " (not present — optional personal override)"
                },
            )?;
        }
        None => writeln!(
            w,
            "  project_config_file: none (not inside a clearhead workspace)"
        )?,
    }

    writeln!(
        w,
        "  data_dir: {}  [override: CLEARHEAD_DATA_DIR | {}]",
        config.data_dir, config.global_config_file.path
    )?;
    writeln!(
        w,
        "  config_dir: {}  [override: CLEARHEAD_CONFIG_DIR | {}]",
        config.config_dir, config.global_config_file.path
    )?;
    writeln!(
        w,
        "  default_file: {}  [override: CLEARHEAD_DEFAULT_FILE | {}]",
        config.default_file, config.global_config_file.path
    )?;

    match &workspace.workspace_id {
        Some(id) => writeln!(
            w,
            "  workspace_id: {}  (name: {})",
            id,
            workspace.workspace_name.as_deref().unwrap_or("<unnamed>")
        )?,
        None => writeln!(
            w,
            "  workspace_id: <unset> — run `clearhead init` to assign a stable graph URI"
        )?,
    }

    if !config.additional_workspaces.is_empty() {
        writeln!(w, "  additional_workspaces:")?;
        for path in &config.additional_workspaces {
            writeln!(w, "    - {}", path)?;
        }
    }

    writeln!(
        w,
        "  plan_component: {}  [override: CLEARHEAD_PLAN_COMPONENT | project config.local.json]",
        config.plan_component,
    )?;

    writeln!(w)?;
    writeln!(w, "workspace")?;
    writeln!(
        w,
        "  resolved_data_root: {} ({})",
        workspace.resolved_data_root, workspace.resolution
    )?;
    writeln!(w, "  plans_root: {}", workspace.plans_root)?;
    writeln!(w, "  root_charter: {}", workspace.root_charter)?;

    if workspace.charters.is_empty() {
        writeln!(w, "  charters: none discovered")?;
    } else {
        writeln!(w, "  charters:")?;
        for entry in &workspace.charters {
            writeln!(
                w,
                "    - alias={} file={} parent={} source={}",
                entry.alias, entry.file, entry.parent, entry.source
            )?;
        }
    }

    writeln!(
        w,
        "  graph_summary: {} charters | {} plans | {} actions | {} violations, {} warnings",
        workspace.graph_summary.charters,
        workspace.graph_summary.plans,
        workspace.graph_summary.actions,
        workspace.graph_summary.violations,
        workspace.graph_summary.warnings
    )?;
    if workspace.has_findings {
        writeln!(w, "  findings: run `clearhead doctor` for the full report")?;
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

fn format_source_type(source_type: &clearhead_cli::filesystem::ManifestSourceType) -> &'static str {
    match source_type {
        clearhead_cli::filesystem::ManifestSourceType::Actions => "actions",
        clearhead_cli::filesystem::ManifestSourceType::Markdown => "markdown",
        clearhead_cli::filesystem::ManifestSourceType::Ics => "ics",
        clearhead_cli::filesystem::ManifestSourceType::ActionsPlusMarkdown => "actions+markdown",
        clearhead_cli::filesystem::ManifestSourceType::ActionsPlusIcs => "actions+ics",
        clearhead_cli::filesystem::ManifestSourceType::MarkdownPlusIcs => "markdown+ics",
        clearhead_cli::filesystem::ManifestSourceType::ActionsPlusMarkdownPlusIcs => {
            "actions+markdown+ics"
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture() -> DebugReport {
        DebugReport {
            config: ConfigSection {
                global_config_file: FileStatus {
                    path: "/home/u/.config/clearhead/config.json".to_string(),
                    found: true,
                },
                project_config_file: Some(ProjectFileStatus {
                    path: "/work/proj/.clearhead/config.json".to_string(),
                    active: true,
                }),
                project_local_config_file: Some(ProjectFileStatus {
                    path: "/work/proj/.clearhead/config.local.json".to_string(),
                    active: false,
                }),
                data_dir: "/work/proj/.clearhead".to_string(),
                config_dir: "<xdg-config-default>".to_string(),
                default_file: "next.actions".to_string(),
                additional_workspaces: vec!["/other/workspace".to_string()],
                plan_component: "vevent".to_string(),
            },
            workspace: WorkspaceSection {
                resolved_data_root: "/work/proj/.clearhead".to_string(),
                resolution: "cwd-walk".to_string(),
                workspace_id: Some("01951111-0000-7000-0000-0000000000ff".to_string()),
                workspace_name: Some("proj".to_string()),
                root_charter: "root".to_string(),
                charters: vec![CharterEntry {
                    alias: "alpha".to_string(),
                    file: "charters/alpha.md".to_string(),
                    parent: "-".to_string(),
                    source: "actions+markdown".to_string(),
                }],
                graph_summary: GraphSummary {
                    charters: 1,
                    plans: 0,
                    actions: 2,
                    violations: 0,
                    warnings: 1,
                },
                plans_root: "/work/proj/.clearhead/plans".to_string(),
                has_findings: true,
            },
        }
    }

    /// Pins the human-readable report's exact text (specifications/workspace.md
    /// output rule: TTY gets prose, a pipe gets JSON) so the struct-based
    /// rendering this module now does cannot silently change it.
    #[test]
    fn human_report_is_pinned_byte_for_byte() {
        let mut out = Vec::new();
        print_report(&fixture(), &mut out).unwrap();
        let text = String::from_utf8(out).unwrap();

        assert_eq!(
            text,
            "\
config
  global_config_file: /home/u/.config/clearhead/config.json
  project_config_file: /work/proj/.clearhead/config.json (active)
  project_local_config_file: /work/proj/.clearhead/config.local.json (not present — optional personal override)
  data_dir: /work/proj/.clearhead  [override: CLEARHEAD_DATA_DIR | /home/u/.config/clearhead/config.json]
  config_dir: <xdg-config-default>  [override: CLEARHEAD_CONFIG_DIR | /home/u/.config/clearhead/config.json]
  default_file: next.actions  [override: CLEARHEAD_DEFAULT_FILE | /home/u/.config/clearhead/config.json]
  workspace_id: 01951111-0000-7000-0000-0000000000ff  (name: proj)
  additional_workspaces:
    - /other/workspace
  plan_component: vevent  [override: CLEARHEAD_PLAN_COMPONENT | project config.local.json]

workspace
  resolved_data_root: /work/proj/.clearhead (cwd-walk)
  plans_root: /work/proj/.clearhead/plans
  root_charter: root
  charters:
    - alias=alpha file=charters/alpha.md parent=- source=actions+markdown
  graph_summary: 1 charters | 0 plans | 2 actions | 0 violations, 1 warnings
  findings: run `clearhead doctor` for the full report
"
        );
    }

    /// The two "not configured" branches (no project root, no workspace
    /// identity) that the happy-path fixture above does not exercise.
    #[test]
    fn human_report_pins_the_unset_and_no_project_branches() {
        let mut report = fixture();
        report.config.project_config_file = None;
        report.config.project_local_config_file = None;
        report.config.additional_workspaces = Vec::new();
        report.workspace.workspace_id = None;
        report.workspace.workspace_name = None;
        report.workspace.charters = Vec::new();
        report.workspace.has_findings = false;

        let mut out = Vec::new();
        print_report(&report, &mut out).unwrap();
        let text = String::from_utf8(out).unwrap();

        assert_eq!(
            text,
            "\
config
  global_config_file: /home/u/.config/clearhead/config.json
  project_config_file: none (not inside a clearhead workspace)
  data_dir: /work/proj/.clearhead  [override: CLEARHEAD_DATA_DIR | /home/u/.config/clearhead/config.json]
  config_dir: <xdg-config-default>  [override: CLEARHEAD_CONFIG_DIR | /home/u/.config/clearhead/config.json]
  default_file: next.actions  [override: CLEARHEAD_DEFAULT_FILE | /home/u/.config/clearhead/config.json]
  workspace_id: <unset> — run `clearhead init` to assign a stable graph URI
  plan_component: vevent  [override: CLEARHEAD_PLAN_COMPONENT | project config.local.json]

workspace
  resolved_data_root: /work/proj/.clearhead (cwd-walk)
  plans_root: /work/proj/.clearhead/plans
  root_charter: root
  charters: none discovered
  graph_summary: 1 charters | 0 plans | 2 actions | 0 violations, 1 warnings
"
        );
    }
}
