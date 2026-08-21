pub mod action;
pub mod charter;
pub mod debug;
pub mod doctor;
pub mod file;
pub mod init;
pub mod plan;
pub mod query;
pub mod resolver;
pub mod service;
pub mod template;
pub mod transact;
pub mod verb_result;

use anyhow::Context;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use tracing::warn;
use uuid::Uuid;

use crate::environment_reader::{
    Config, ensure_dir_exists, find_project_data_dir, get_data_dir, load_config,
    resolve_config_path, resolve_file_path,
};
use clearhead_cli::ActionList;
use clearhead_cli::telemetry::{TelemetryEvent, Tool, emit_event};

/// Consolidated command context: config + resolved directories.
pub struct CommandContext {
    pub config: Config,
    pub data_dir: PathBuf,
    pub config_path: PathBuf,
    pub project_root: Option<PathBuf>,
    /// When set, `workspace_dirs()` returns only the workspace whose name
    /// matches (case-insensitive contains). Set via `--workspace <name>`.
    pub workspace_filter: Option<String>,
}

impl CommandContext {
    pub fn new(cli: &crate::argparser::Cli) -> anyhow::Result<Self> {
        let config_path = resolve_config_path(cli.config.clone());
        let config = load_config(cli.config.clone()).context("Failed to load config")?;

        let project_root = find_project_data_dir();

        // Workspace Resolution (specifications/configuration.md): most-local
        // context wins — a project detected from the pwd always outranks
        // config. `config.data_dir` only relocates the fallback user
        // workspace used outside any project.
        let user_data_dir = if config.data_dir.is_empty() {
            get_data_dir()
        } else {
            resolve_file_path(&config.data_dir, &get_data_dir())
        };
        let data_dir = if config.default_to_user_scope {
            user_data_dir
        } else {
            project_root.clone().unwrap_or(user_data_dir)
        };
        let config_dir = resolve_file_path(
            &config.config_dir,
            &crate::environment_reader::get_config_dir(),
        );

        ensure_dir_exists(&data_dir).context("Failed to create data dir")?;
        ensure_dir_exists(&config_dir).context("Failed to create config dir")?;

        Ok(Self {
            config,
            data_dir,
            config_path,
            project_root,
            workspace_filter: cli.workspace.clone(),
        })
    }

    /// Resolve an optional file arg against the default file from config.
    pub fn resolve_action_file(&self, file: Option<&PathBuf>) -> PathBuf {
        let charter_root = clearhead_core::charter_root(&self.data_dir);
        file.cloned()
            .unwrap_or_else(|| resolve_file_path(&self.config.default_file, &charter_root))
    }

    /// Resolve indent style and width from config.
    pub fn indent_config(&self) -> (clearhead_cli::IndentStyle, usize) {
        (
            parse_indent_style(&self.config.cli_indent_style),
            self.config.cli_indent_width,
        )
    }

    /// Return the workspace root from the configured list that owns `file`.
    ///
    /// Checks each workspace's charter tree (`<root>/.clearhead/charters/`).
    /// Falls back to `data_dir` when no configured workspace matches, so callers
    /// never have to handle a missing case.
    pub fn workspace_for_file(&self, file: &Path) -> PathBuf {
        let abs = std::fs::canonicalize(file).unwrap_or_else(|_| file.to_path_buf());
        for (_, dir) in self.workspace_dirs() {
            let charter_root = clearhead_core::charter_root(&dir);
            let abs_root = std::fs::canonicalize(&charter_root).unwrap_or(charter_root);
            if abs.starts_with(&abs_root) {
                return dir;
            }
        }
        self.data_dir.clone()
    }

    /// Return all configured workspace names and roots, primary first.
    ///
    /// This is the CLI orchestration point for multi-workspace fan-out. Core
    /// supplies canonical workspace identity, path, and configuration helpers;
    /// the CLI applies the invocation's optional workspace filter here.
    pub fn workspace_dirs(&self) -> Vec<(String, PathBuf)> {
        // Identity (including the display name) is a property of the workspace,
        // read from its manifest — not the layered config.
        let primary_name = workspace_name_for_root(&self.data_dir);
        let mut dirs = vec![(primary_name, self.data_dir.clone())];

        let wc = self.workspace_config();
        for path_str in &wc.additional_workspaces {
            let path = PathBuf::from(path_str);
            let name = workspace_name_for_root(&path);
            dirs.push((name, path));
        }

        if let Some(filter) = &self.workspace_filter {
            let f = filter.to_lowercase();
            dirs.retain(|(name, _)| name.to_lowercase().contains(&f));
        }

        dirs
    }

    /// Load a `DomainModel` for every workspace.
    ///
    /// The primary workspace is a hard failure; additional workspaces warn and
    /// are skipped on error so a single bad workspace never blocks the others.
    pub fn all_domain_models(&self) -> anyhow::Result<Vec<(String, clearhead_core::DomainModel)>> {
        let mut models = Vec::new();
        for (name, path) in self.workspace_dirs() {
            let is_primary = path == self.data_dir;
            // The primary honors plan_path; additional workspaces use their own default.
            let loaded = if is_primary {
                clearhead_core::load_domain_model_with_plans(&path, self.plan_override().as_deref())
            } else {
                clearhead_core::load_domain_model_with_plans(&path, None)
            };
            match loaded {
                Ok(m) => models.push((name, m)),
                Err(e) if is_primary => return Err(e.into()),
                Err(e) => warn!("Skipping workspace '{}': {}", path.display(), e),
            }
        }
        Ok(models)
    }

    /// Project the loaded CLI configuration into Core's shared semantic type.
    ///
    /// Tool-specific `cli_*` fields stay in [`Config`], while shared tag,
    /// expansion, workspace, and Plan-path settings cross the Core boundary.
    pub fn workspace_config(&self) -> clearhead_core::WorkspaceConfig {
        // Resolve relative additional_workspaces paths against the project
        // config location (<root>/.clearhead/).  config_path always holds the
        // global config path even when a project config is active, so we
        // prefer project_root/.clearhead/ when a project workspace exists.
        // Fall back to config_path's parent (global config dir) otherwise.
        let config_base = self
            .project_root
            .as_ref()
            .map(|r| r.join(".clearhead"))
            .or_else(|| self.config_path.parent().map(|p| p.to_path_buf()))
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_default());

        let resolved_additional = clearhead_cli::environment_reader::resolve_workspace_paths(
            &self.config.additional_workspaces,
            &config_base,
        );

        // Resolve plan_path the same way (~, $VAR, relative-to-.clearhead) so core
        // receives a ready-to-use absolute path, never a raw config string.
        let resolved_plan_path = self.config.plan_path.as_ref().and_then(|p| {
            clearhead_cli::environment_reader::resolve_workspace_paths(
                std::slice::from_ref(p),
                &config_base,
            )
            .into_iter()
            .next()
            .map(|pb| pb.to_string_lossy().into_owned())
        });

        clearhead_core::WorkspaceConfig {
            tag_hierarchies: self.config.tag_hierarchies.clone(),
            expansion_total_instances: self.config.expansion_total_instances,
            plan_path: resolved_plan_path,
            additional_workspaces: resolved_additional
                .into_iter()
                .map(|p| p.to_string_lossy().into_owned())
                .collect(),
            ..clearhead_core::WorkspaceConfig::default()
        }
    }

    /// The resolved `plan_path` override (absolute, shell-expanded) for the
    /// primary workspace, or `None` when plans live under its default `plans/`.
    ///
    /// This is the single place a command needs to think about `plan_path`; the
    /// `load_*` / `plans_root` / `collect_plan_files` helpers below all route
    /// through it, so commands stay oblivious to where plans physically live.
    pub fn plan_override(&self) -> Option<PathBuf> {
        self.workspace_config().plan_path.map(PathBuf::from)
    }

    /// Refuse a semantic workspace mutation while any action source requires
    /// parser recovery. Diagnostic reads remain relaxed, but a partial model is
    /// not a safe basis for reconciliation or identity-bearing writes.
    pub fn require_source_integrity(&self, command: &str) -> anyhow::Result<()> {
        let read = clearhead_core::workspace::read_workspace_with_plans(
            &self.data_dir,
            self.plan_override().as_deref(),
        )?;
        let quarantined: Vec<_> = read
            .findings
            .iter()
            .filter(|finding| {
                matches!(
                    finding.code.as_str(),
                    "syntax-errors" | "unowned-plans-collection"
                )
            })
            .collect();
        if !quarantined.is_empty() {
            for finding in &quarantined {
                eprintln!(
                    "error: [{}] {} cannot continue: {}",
                    finding.path.display(),
                    command,
                    finding.message
                );
            }
            anyhow::bail!(
                "{} refused: repair {} quarantined workspace source(s) first",
                command,
                quarantined.len()
            );
        }
        Ok(())
    }

    /// Load the primary workspace's domain model, honoring `plan_path`.
    ///
    /// The loaded model is materialized artifacts only — occurrences are not
    /// projected into it. The present due occurrence appears as a real `.actions`
    /// line (stamped on the write path); future occurrences are a read-only
    /// calendar concern rendered from the recurrence engine, not part of this model.
    pub fn load_model(&self) -> anyhow::Result<clearhead_core::DomainModel> {
        Ok(clearhead_core::load_domain_model_with_plans(
            &self.data_dir,
            self.plan_override().as_deref(),
        )?)
    }

    /// Load the primary workspace's charters, honoring `plan_path`.
    pub fn load_charters(&self) -> anyhow::Result<Vec<clearhead_core::MarkdownCharter>> {
        Ok(clearhead_core::load_workspace_with_plans(
            &self.data_dir,
            self.plan_override().as_deref(),
        )?)
    }

    /// Discover the primary workspace's plan `.ics` entries, honoring `plan_path`.
    pub fn collect_plan_files(
        &self,
    ) -> anyhow::Result<Vec<clearhead_core::workspace::PlanFileEntry>> {
        Ok(clearhead_core::collect_plan_files_with_plans(
            &self.data_dir,
            self.plan_override().as_deref(),
        )?)
    }

    /// The primary workspace's `plans_root`, honoring `plan_path`.
    pub fn plans_root(&self) -> PathBuf {
        self.plan_override()
            .unwrap_or_else(|| clearhead_core::plans_root(&self.data_dir))
    }
}

/// Derive a human-readable workspace name from its root path.
///
/// Strips a trailing `.clearhead` component so that both
/// `/path/to/project` and `/path/to/project/.clearhead` resolve to `project`.
pub fn workspace_name_from_path(path: &Path) -> String {
    let base = if path.ends_with(".clearhead") {
        path.parent().unwrap_or(path)
    } else {
        path
    };
    base.file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown")
        .to_string()
}

/// Resolve the human-facing workspace name for a workspace root.
///
/// Preference order:
/// 1. `<root>/.clearhead/config.json` → `workspace_name`
/// 2. fallback to the directory name (`workspace_name_from_path`)
///
/// This keeps `--workspace` filtering aligned with the workspace's own declared
/// identity instead of silently keying additional workspaces by their folder
/// names only.
pub fn workspace_name_for_root(path: &Path) -> String {
    // The manifest (workspace.json) carries the workspace's declared name; fall
    // back to the directory name.
    clearhead_core::workspace::WorkspaceManifest::read(path)
        .workspace_name
        .map(|n| n.trim().to_string())
        .filter(|n| !n.is_empty())
        .unwrap_or_else(|| workspace_name_from_path(path))
}

/// Load actions for read-only operations using recoverable parse mode.
pub fn load_file_for_read(path: &Path, command: &str) -> anyhow::Result<ActionList> {
    if !path.exists() {
        return Ok(ActionList::new());
    }

    let content = fs::read_to_string(path)
        .with_context(|| format!("Failed to read file '{}'", path.display()))?;
    parse_content_for_read(&content, &path.display().to_string(), command)
}

/// Parse actions content for read-only operations using recoverable parse mode.
pub fn parse_content_for_read(
    content: &str,
    source: &str,
    command: &str,
) -> anyhow::Result<ActionList> {
    let outcome =
        clearhead_cli::parse_actions_with_mode(content, clearhead_cli::ParseMode::Recover)
            .with_context(|| format!("Failed to parse '{}'", source))?;

    if !outcome.syntax_errors.is_empty() {
        report_parse_recovered(
            source,
            command,
            &outcome.syntax_errors,
            outcome.recovery.recoverable_actions,
        );
    }

    Ok(outcome.document.actions)
}

/// Parse source into the capability required by any operation that may
/// reserialize or semantically mutate it.
pub fn parse_content_for_rewrite(
    content: &str,
    source: &str,
    command: &str,
) -> anyhow::Result<clearhead_cli::TrustedDocument> {
    let outcome =
        clearhead_cli::parse_actions_with_mode(content, clearhead_cli::ParseMode::Recover)
            .with_context(|| format!("Failed to parse '{}'", source))?;

    match clearhead_cli::TrustedDocument::try_from(outcome.document) {
        Ok(document) => Ok(document),
        Err(error) => {
            report_mutation_parse_failure(Path::new(source), command, &error.issues);
            anyhow::bail!(
                "Parse error in '{}': {} issue(s). Source not rewritten.",
                source,
                error.issues.len()
            );
        }
    }
}

/// Parse actions content for mutating operations.
///
/// If syntax issues are present, this returns an error and callers must not write.
pub fn parse_content_for_mutation(
    content: &str,
    source: &str,
    command: &str,
) -> anyhow::Result<ActionList> {
    Ok(parse_content_for_rewrite(content, source, command)?
        .into_parsed()
        .actions)
}

/// Load actions for mutating operations.
///
/// If syntax issues are present, this returns an error and callers must not write.
pub fn load_file_for_mutation(path: &Path, command: &str) -> anyhow::Result<ActionList> {
    if !path.exists() {
        return Ok(ActionList::new());
    }

    let content = fs::read_to_string(path)
        .with_context(|| format!("Failed to read file '{}'", path.display()))?;
    parse_content_for_mutation(&content, &path.display().to_string(), command)
}

/// Format actions and write to a .actions file on disk.
///
/// Also updates the charter sidecar (best-effort — sidecar failures
/// are logged but do not prevent the actions file from being saved).
pub fn save_file(path: &Path, actions: &ActionList) -> anyhow::Result<()> {
    clearhead_core::workspace::action_files::write_actions(actions, path)?;
    if let Err(e) = update_sidecar(path, actions) {
        warn!(path = %path.display(), error = %e, "Failed to update sidecar");
    }
    Ok(())
}

/// Ensure every action in the list has an entry in the charter sidecar.
/// Delegates to `clearhead_core::workspace::sidecar::stamp_sidecar_entries`.
pub fn update_sidecar(actions_path: &Path, actions: &ActionList) -> anyhow::Result<()> {
    use clearhead_core::workspace::sidecar;
    Ok(sidecar::stamp_sidecar_entries(actions_path, actions)?)
}

/// Write content to a file if `write` is true, otherwise print to stdout.
pub fn write_or_print(content: &str, write: bool, file: Option<&PathBuf>) -> anyhow::Result<()> {
    if write {
        let path = file.context("Cannot use --write without specifying a file")?;
        fs::write(path, content).context("Failed to write to file")?;
        Ok(())
    } else {
        println!("{}", content);
        Ok(())
    }
}

/// Print completable values (one per line) for shell integration.
///
/// Intended for use with `clearhead _complete <kind>`. Shell completions can
/// call this and feed the output to their completion engine, e.g. in fish:
///   complete -c clearhead -l charter -a "(clearhead _complete charters 2>/dev/null)"
pub fn complete_values(
    ctx: &CommandContext,
    kind: crate::argparser::CompleteKind,
) -> anyhow::Result<()> {
    use crate::argparser::CompleteKind;
    match kind {
        CompleteKind::Charters => {
            for (_, ws_root) in ctx.workspace_dirs() {
                let mcs = match clearhead_core::load_workspace(&ws_root) {
                    Ok(m) => m,
                    Err(_) => continue,
                };
                for mc in mcs {
                    let label = mc.alias.as_deref().unwrap_or(&mc.title);
                    println!("{}", label);
                }
            }
        }
        CompleteKind::Workspaces => {
            for (name, _) in ctx.workspace_dirs() {
                println!("{}", name);
            }
        }
    }
    Ok(())
}

/// Emit a telemetry event, logging a warning on failure instead of propagating.
pub fn try_emit(action_id: &Uuid, event: TelemetryEvent) {
    if let Err(e) = emit_event(Tool::Cli, Some(action_id.to_string()), event) {
        warn!(error = %e, "Failed to emit telemetry event");
    }
}

/// Read input from a file or stdin
pub fn read_input(file: Option<&PathBuf>) -> anyhow::Result<String> {
    match file {
        Some(path) => fs::read_to_string(path)
            .with_context(|| format!("Failed to read file '{}'", path.display())),
        None => {
            let mut buffer = String::new();
            io::stdin()
                .read_to_string(&mut buffer)
                .context("Failed to read from stdin")?;
            Ok(buffer)
        }
    }
}

/// Resolve a charter query to the primary `.actions` file for that charter.
///
/// Scans all workspace action files and matches the inferred charter name against
/// the query (by UUID prefix, alias, or inferred file stem / directory name).
pub fn charter_to_file_path(data_dir: &Path, charter_query: &str) -> anyhow::Result<PathBuf> {
    let data_root = clearhead_core::charter_root(data_dir);
    let action_files =
        clearhead_core::list_action_files(data_dir).context("Failed to list workspace")?;

    let query_lower = charter_query.to_lowercase();

    for file_path in &action_files {
        let relative = file_path
            .strip_prefix(&data_root)
            .unwrap_or(file_path.as_path());
        let inferred = clearhead_core::infer_charter_name(relative).unwrap_or_default();
        if inferred.to_lowercase() == query_lower {
            return Ok(file_path.clone());
        }
    }

    // Fall back to model-level resolution (matches alias, UUID, partial title)
    let model = clearhead_core::load_domain_model(data_dir)?;
    let found = crate::commands::charter::resolve_charter(&model.charters, charter_query)?
        .ok_or_else(|| anyhow::anyhow!("No charter found matching '{}'", charter_query))?;

    let key = found.alias.as_deref().unwrap_or(&found.title);
    let key_lower = key.to_lowercase();

    for file_path in &action_files {
        let relative = file_path
            .strip_prefix(&data_root)
            .unwrap_or(file_path.as_path());
        let inferred = clearhead_core::infer_charter_name(relative).unwrap_or_default();
        if inferred.to_lowercase() == key_lower {
            return Ok(file_path.clone());
        }
    }

    anyhow::bail!("No actions file found for charter '{}'", charter_query)
}

fn parse_indent_style(s: &str) -> clearhead_cli::IndentStyle {
    match s.to_lowercase().as_str() {
        "tabs" => clearhead_cli::IndentStyle::Tabs,
        _ => clearhead_cli::IndentStyle::Spaces,
    }
}

fn report_parse_recovered(
    source: &str,
    command: &str,
    syntax_errors: &[clearhead_cli::LintDiagnostic],
    recoverable_actions: usize,
) {
    emit_event(
        Tool::Cli,
        None,
        TelemetryEvent::ParseRecovered {
            file_path: source.to_string(),
            error_count: syntax_errors.len(),
            recoverable_count: recoverable_actions,
        },
    )
    .unwrap_or_else(|e| warn!(error = %e, "Failed to emit parse_recovered telemetry"));

    eprintln!(
        "warning: [{}] {} parsed with {} issue(s); proceeding with {} recoverable action(s)",
        source,
        command,
        syntax_errors.len(),
        recoverable_actions
    );
    print_syntax_diagnostics(syntax_errors);
}

fn report_mutation_parse_failure(
    path: &Path,
    command: &str,
    syntax_errors: &[clearhead_cli::LintDiagnostic],
) {
    emit_event(
        Tool::Cli,
        None,
        TelemetryEvent::ParseFailed {
            file_path: path.display().to_string(),
            error_count: syntax_errors.len(),
            first_error_code: syntax_errors
                .first()
                .map(|d| d.code.clone())
                .unwrap_or_else(|| "syntax-error".to_string()),
        },
    )
    .unwrap_or_else(|e| warn!(error = %e, "Failed to emit parse_failed telemetry"));

    emit_event(
        Tool::Cli,
        None,
        TelemetryEvent::MutationSkippedDueToParse {
            command: command.to_string(),
            file_path: path.display().to_string(),
            error_count: syntax_errors.len(),
        },
    )
    .unwrap_or_else(
        |e| warn!(error = %e, "Failed to emit mutation_skipped_due_to_parse telemetry"),
    );

    eprintln!(
        "error: [{}] {} skipped due to parse issues; file not modified",
        path.display(),
        command
    );
    print_syntax_diagnostics(syntax_errors);
}

fn print_syntax_diagnostics(syntax_errors: &[clearhead_cli::LintDiagnostic]) {
    for diagnostic in syntax_errors.iter().take(5) {
        eprintln!(
            "  - line {}, col {}: {}",
            diagnostic.range.start_row + 1,
            diagnostic.range.start_col + 1,
            diagnostic.message
        );
    }

    let remaining = syntax_errors.len().saturating_sub(5);
    if remaining > 0 {
        eprintln!("  - ... and {} more issue(s)", remaining);
    }
}
