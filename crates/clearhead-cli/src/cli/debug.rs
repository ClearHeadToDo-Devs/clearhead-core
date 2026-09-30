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
    pub exists: bool,
}

impl FileStatus {
    fn at(path: std::path::PathBuf) -> Self {
        Self {
            exists: path.exists(),
            path: path.display().to_string(),
        }
    }
}

/// Both files exist together or not at all: they are only reported inside a
/// detected project.
#[derive(Serialize)]
pub struct ProjectFiles {
    pub project_config_file: FileStatus,
    pub project_local_config_file: FileStatus,
}

#[derive(Serialize)]
pub struct ConfigSection {
    pub global_config_file: FileStatus,
    #[serde(flatten)]
    pub project_files: Option<ProjectFiles>,
    /// `None` when unset; the human report supplies the placeholder label.
    pub data_dir: Option<String>,
    pub config_dir: Option<String>,
    pub default_file: String,
    pub plan_component: String,
    pub additional_workspaces: Vec<String>,
}

#[derive(Serialize)]
pub struct CharterEntry {
    pub alias: String,
    pub file: String,
    pub parent: Option<String>,
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
    /// `None` when no charter is a root, or when the workspace read failed.
    pub root_charter: Option<String>,
    pub charters: Vec<CharterEntry>,
    /// `None` only when the workspace read failed; see `error`.
    pub graph_summary: Option<GraphSummary>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    /// Rendering-only: not part of the debug JSON contract.
    #[serde(skip)]
    pub plans_root: String,
    #[serde(skip)]
    pub has_findings: bool,
}

pub fn run(ctx: &CommandContext) -> anyhow::Result<()> {
    let report = build(ctx);
    if std::io::stdout().is_terminal() {
        print_report(&report, &mut std::io::stdout())?;
    } else {
        println!("{}", serde_json::to_string_pretty(&report)?);
    }
    Ok(())
}

fn build(ctx: &CommandContext) -> DebugReport {
    DebugReport {
        config: build_config_section(ctx),
        workspace: build_workspace_section(ctx),
    }
}

fn build_config_section(ctx: &CommandContext) -> ConfigSection {
    // Project config — written by `clearhead init`, layered on top of the global
    // config and overrides workspace-specific settings (workspace_id, etc.).
    // config.local.json sits beside it as a git-ignored personal override that
    // wins over the committed project config.
    let project_files = ctx.project_root.as_ref().map(|root| ProjectFiles {
        project_config_file: FileStatus::at(root.join(".clearhead").join("config.json")),
        project_local_config_file: FileStatus::at(
            root.join(".clearhead").join("config.local.json"),
        ),
    });

    ConfigSection {
        global_config_file: FileStatus::at(ctx.config_path.clone()),
        project_files,
        data_dir: non_empty(&ctx.config.data_dir),
        config_dir: non_empty(&ctx.config.config_dir),
        default_file: ctx.config.default_file.clone(),
        plan_component: ctx.config.plan_component.to_string(),
        additional_workspaces: ctx.config.additional_workspaces.clone(),
    }
}

/// The parts of the workspace section that need the workspace read.
struct GraphRead {
    root_charter: Option<String>,
    charters: Vec<CharterEntry>,
    graph_summary: GraphSummary,
    has_findings: bool,
}

fn read_graph(ctx: &CommandContext) -> anyhow::Result<GraphRead> {
    let manifest = clearhead_cli::filesystem::collect_workspace_manifest(&ctx.data_dir)
        .context("Failed to collect workspace manifest")?;
    // Diagnostics must observe, not alter: the pure reader (no journal replay,
    // per-file failures become findings) instead of the healing load path.
    let read = clearhead_cli::filesystem::read_workspace(&ctx.data_dir)
        .context("Failed to read workspace")?;
    let diagnosis = clearhead_cli::filesystem::diagnose_workspace_read(&ctx.data_dir, &read)
        .context("Failed to diagnose workspace")?;

    let charters = manifest
        .into_iter()
        .map(|entry| CharterEntry {
            alias: entry.charter_name,
            file: entry.path,
            parent: entry.inferred_parent,
            source: format_source_type(&entry.source_type).to_string(),
        })
        .collect();

    Ok(GraphRead {
        root_charter: find_root_charter_alias(&read.charters),
        charters,
        graph_summary: GraphSummary {
            charters: read.charters.len(),
            plans: read.charters.iter().map(|c| c.plans.len()).sum(),
            actions: read.charters.iter().map(|c| c.actions.len()).sum(),
            violations: diagnosis.violations(),
            warnings: diagnosis.warnings(),
        },
        has_findings: !diagnosis.findings.is_empty(),
    })
}

/// Never fails: a broken workspace read is reported in `error` so the config
/// section, which does not depend on it, still shows (as `orient` degrades).
fn build_workspace_section(ctx: &CommandContext) -> WorkspaceSection {
    let workspace_manifest = clearhead_cli::filesystem::read_workspace_manifest(&ctx.data_dir);
    let mut section = WorkspaceSection {
        resolved_data_root: clearhead_cli::filesystem::workspace_data_root(&ctx.data_dir)
            .display()
            .to_string(),
        resolution: resolve_workspace_source(ctx).to_string(),
        workspace_id: workspace_manifest.workspace_id,
        workspace_name: workspace_manifest.workspace_name,
        root_charter: None,
        charters: Vec::new(),
        graph_summary: None,
        error: None,
        plans_root: ctx.plans_root().display().to_string(),
        has_findings: false,
    };
    match read_graph(ctx) {
        Ok(graph) => {
            section.root_charter = graph.root_charter;
            section.charters = graph.charters;
            section.graph_summary = Some(graph.graph_summary);
            section.has_findings = graph.has_findings;
        }
        Err(err) => section.error = Some(format!("{err:#}")),
    }
    section
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
        if config.global_config_file.exists {
            ""
        } else {
            " (not found)"
        },
    )?;

    match &config.project_files {
        Some(files) => {
            let project = &files.project_config_file;
            writeln!(
                w,
                "  project_config_file: {}{}",
                project.path,
                if project.exists {
                    " (active)"
                } else {
                    " (not found — run `clearhead init`)"
                },
            )?;
            let local = &files.project_local_config_file;
            writeln!(
                w,
                "  project_local_config_file: {}{}",
                local.path,
                if local.exists {
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
        config
            .data_dir
            .as_deref()
            .unwrap_or("<project-root-or-xdg-default>"),
        config.global_config_file.path
    )?;
    writeln!(
        w,
        "  config_dir: {}  [override: CLEARHEAD_CONFIG_DIR | {}]",
        config
            .config_dir
            .as_deref()
            .unwrap_or("<xdg-config-default>"),
        config.global_config_file.path
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

    if let Some(error) = &workspace.error {
        writeln!(w, "  error: {error}")?;
        return Ok(());
    }

    writeln!(
        w,
        "  root_charter: {}",
        workspace.root_charter.as_deref().unwrap_or("-")
    )?;

    if workspace.charters.is_empty() {
        writeln!(w, "  charters: none discovered")?;
    } else {
        writeln!(w, "  charters:")?;
        for entry in &workspace.charters {
            writeln!(
                w,
                "    - alias={} file={} parent={} source={}",
                entry.alias,
                entry.file,
                entry.parent.as_deref().unwrap_or("-"),
                entry.source
            )?;
        }
    }

    if let Some(summary) = &workspace.graph_summary {
        writeln!(
            w,
            "  graph_summary: {} charters | {} plans | {} actions | {} violations, {} warnings",
            summary.charters, summary.plans, summary.actions, summary.violations, summary.warnings
        )?;
    }
    if workspace.has_findings {
        writeln!(w, "  findings: run `clearhead doctor` for the full report")?;
    }
    Ok(())
}

fn non_empty(value: &str) -> Option<String> {
    (!value.is_empty()).then(|| value.to_string())
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
                    exists: true,
                },
                project_files: Some(ProjectFiles {
                    project_config_file: FileStatus {
                        path: "/work/proj/.clearhead/config.json".to_string(),
                        exists: true,
                    },
                    project_local_config_file: FileStatus {
                        path: "/work/proj/.clearhead/config.local.json".to_string(),
                        exists: false,
                    },
                }),
                data_dir: Some("/work/proj/.clearhead".to_string()),
                config_dir: None,
                default_file: "next.actions".to_string(),
                additional_workspaces: vec!["/other/workspace".to_string()],
                plan_component: "vevent".to_string(),
            },
            workspace: WorkspaceSection {
                resolved_data_root: "/work/proj/.clearhead".to_string(),
                resolution: "cwd-walk".to_string(),
                workspace_id: Some("01951111-0000-7000-0000-0000000000ff".to_string()),
                workspace_name: Some("proj".to_string()),
                root_charter: Some("root".to_string()),
                charters: vec![CharterEntry {
                    alias: "alpha".to_string(),
                    file: "charters/alpha.md".to_string(),
                    parent: None,
                    source: "actions+markdown".to_string(),
                }],
                graph_summary: Some(GraphSummary {
                    charters: 1,
                    plans: 0,
                    actions: 2,
                    violations: 0,
                    warnings: 1,
                }),
                error: None,
                plans_root: "/work/proj/.clearhead/plans".to_string(),
                has_findings: true,
            },
        }
    }

    /// Pins the human-readable report's exact text so the struct-based
    /// rendering cannot silently change it.
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

    /// A failed workspace read keeps the config section and ends with one
    /// error line, in both renderings.
    #[test]
    fn workspace_read_failure_degrades_after_the_config_section() {
        let mut report = fixture();
        report.workspace.root_charter = None;
        report.workspace.charters = Vec::new();
        report.workspace.graph_summary = None;
        report.workspace.has_findings = false;
        report.workspace.error = Some("Failed to read workspace: boom".to_string());

        let mut out = Vec::new();
        print_report(&report, &mut out).unwrap();
        let text = String::from_utf8(out).unwrap();
        assert!(text.starts_with("config\n  global_config_file:"));
        assert!(text.ends_with(
            "workspace\n  resolved_data_root: /work/proj/.clearhead (cwd-walk)\n  plans_root: /work/proj/.clearhead/plans\n  error: Failed to read workspace: boom\n"
        ));

        let json = serde_json::to_value(&report).unwrap();
        assert_eq!(json["workspace"]["error"], "Failed to read workspace: boom");
        assert!(json["workspace"]["graph_summary"].is_null());
        assert!(json["config"]["global_config_file"]["path"].is_string());
    }

    /// Unset values are JSON null, never the human placeholder text.
    #[test]
    fn json_carries_null_not_display_placeholders() {
        let json = serde_json::to_value(fixture()).unwrap();
        assert!(json["config"]["config_dir"].is_null());
        assert!(json["workspace"]["charters"][0]["parent"].is_null());
    }

    /// The two "not configured" branches (no project root, no workspace
    /// identity) that the happy-path fixture above does not exercise.
    #[test]
    fn human_report_pins_the_unset_and_no_project_branches() {
        let mut report = fixture();
        report.config.project_files = None;
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
