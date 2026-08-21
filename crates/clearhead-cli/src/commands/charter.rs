use anyhow::Context;
use std::io::IsTerminal;
use std::path::{Path, PathBuf};

use tracing::info;

use crate::argparser;
use crate::commands::CommandContext;
use clearhead_core::{ActionState, Charter};

use super::action::resolve_charter_across_workspaces;

/// Return the directory within `ws_root`'s charter tree where children of `parent` should live.
///
/// - Root charter (`next.actions`): children go flat in charter_root
/// - Named flat charter (`lsp.actions`): children go in `charter_root/lsp/`
/// - Nested charter (`lsp/diag.actions`): children go in `charter_root/lsp/diag/`
fn sub_charter_dir(
    ws_root: &Path,
    parent: &clearhead_core::MarkdownCharter,
) -> anyhow::Result<PathBuf> {
    let charter_root = clearhead_core::charter_root(ws_root);
    let acts_rel = parent.actions_file.as_ref().ok_or_else(|| {
        anyhow::anyhow!(
            "Parent charter '{}' has no associated actions file; cannot determine placement",
            parent.title
        )
    })?;
    let without_ext = acts_rel
        .to_str()
        .and_then(|s| s.strip_suffix(".actions"))
        .unwrap_or("");
    if without_ext == "next" {
        Ok(charter_root)
    } else {
        Ok(charter_root.join(without_ext))
    }
}

/// Find a MarkdownCharter by matching a file path against known charter files.
/// `file` may be absolute; it is made relative to `charter_root` before comparison.
fn resolve_charter_by_file<'a>(
    mcs: &'a [clearhead_core::MarkdownCharter],
    file: &std::path::Path,
    charter_root: &std::path::Path,
) -> Option<&'a clearhead_core::MarkdownCharter> {
    let abs_file = std::fs::canonicalize(file)
        .unwrap_or_else(|_| std::env::current_dir().unwrap_or_default().join(file));
    let abs_root =
        std::fs::canonicalize(charter_root).unwrap_or_else(|_| charter_root.to_path_buf());
    let rel = abs_file.strip_prefix(&abs_root).unwrap_or(&abs_file);
    mcs.iter()
        .find(|mc| mc.actions_file.as_deref() == Some(rel) || mc.md_file.as_deref() == Some(rel))
}

/// Resolve a MarkdownCharter from either a query string or a file path.
fn find_target_charter<'a>(
    mcs: &'a [clearhead_core::MarkdownCharter],
    query: Option<&str>,
    file: Option<&std::path::Path>,
    charter_root: &std::path::Path,
) -> anyhow::Result<&'a clearhead_core::MarkdownCharter> {
    if let Some(file_path) = file {
        return resolve_charter_by_file(mcs, file_path, charter_root)
            .ok_or_else(|| anyhow::anyhow!("No charter found for file: {}", file_path.display()));
    }
    if let Some(q) = query {
        let charters: Vec<Charter> = mcs.iter().cloned().map(Charter::from).collect();
        let mc = resolve_charter(&charters, q)?
            .ok_or_else(|| anyhow::anyhow!("No charter found matching '{}'", q))?;
        return mcs
            .iter()
            .find(|c| c.id == mc.id)
            .ok_or_else(|| anyhow::anyhow!("Internal: MarkdownCharter for '{}' missing", q));
    }
    anyhow::bail!("Provide a charter name/alias/UUID or --file <path>")
}

pub fn read_charters(
    ctx: &CommandContext,
    format: &Option<argparser::OutputMode>,
    explicit_only: bool,
) -> anyhow::Result<()> {
    let multi_ws = ctx.workspace_dirs().len() > 1;

    // Load full models — tree view needs plans and actions, not just charters.
    let models = ctx.all_domain_models()?;

    // Apply explicit_only filter.
    let models: Vec<(String, clearhead_core::DomainModel)> = models
        .into_iter()
        .map(|(name, mut m)| {
            if explicit_only {
                m.charters
                    .retain(|c| c.alias.is_some() || c.description.is_some());
            }
            (name, m)
        })
        .filter(|(_, m)| !m.charters.is_empty())
        .collect();

    if models.is_empty() {
        println!("No charters found.");
        return Ok(());
    }

    match format {
        Some(argparser::OutputMode::JsonLd) => {
            for (_, model) in &models {
                let jsonld = clearhead_cli::serialize_domain_to_jsonld(model)
                    .map_err(|e| anyhow::anyhow!("Failed to serialize JSON-LD: {e}"))?;
                println!("{}", jsonld);
            }
        }
        Some(argparser::OutputMode::Json) => {
            // Charters have no canonical actions-schema shape yet; emit plain
            // structured JSON of the domain charters until a charter schema exists.
            for (_, model) in &models {
                println!("{}", serde_json::to_string_pretty(&model.charters)?);
            }
        }
        Some(argparser::OutputMode::Ids) => {
            for (_, model) in &models {
                for charter in &model.charters {
                    println!("{}", charter.id);
                }
            }
        }
        Some(argparser::OutputMode::Table) => {
            let workspaces: Vec<(String, Vec<Charter>)> =
                models.into_iter().map(|(n, m)| (n, m.charters)).collect();
            print_charter_table(&workspaces, multi_ws);
        }
        None => {
            if std::io::stdout().is_terminal() {
                // TTY: charter hierarchy tree with open action counts.
                for (ws_name, model) in &models {
                    if multi_ws {
                        println!("▸ {}", ws_name);
                    }
                    print!("{}", crate::display::render_charter_tree(model));
                }
            } else {
                // Pipe/redirect: markdown — native file format for charters.
                for (_, model) in &models {
                    for charter in &model.charters {
                        println!("{}", clearhead_core::format_charter(charter));
                    }
                }
            }
        }
    }
    Ok(())
}

fn print_charter_table(workspaces: &[(String, Vec<Charter>)], multi_ws: bool) {
    use comfy_table::{Cell, Color, Table, presets::UTF8_FULL};

    let mut table = Table::new();
    let mut headers = vec![
        Cell::new("Title").fg(Color::Cyan),
        Cell::new("Alias").fg(Color::Cyan),
        Cell::new("Parent").fg(Color::Cyan),
        Cell::new("Open Actions").fg(Color::Cyan),
    ];
    if multi_ws {
        headers.insert(0, Cell::new("Workspace").fg(Color::Cyan));
    }
    table.load_preset(UTF8_FULL).set_header(headers);

    for (ws_name, charters) in workspaces {
        let refs: Vec<&Charter> = charters.iter().collect();
        let sorted = sort_charters_hierarchically(&refs);
        for charter in sorted {
            let open = open_act_count(charter);
            let mut row = vec![
                Cell::new(&charter.title),
                Cell::new(charter.alias.as_deref().unwrap_or("-")),
                Cell::new(charter.parent.as_deref().unwrap_or("-")),
                Cell::new(if open > 0 {
                    open.to_string()
                } else {
                    "-".to_string()
                }),
            ];
            if multi_ws {
                row.insert(0, Cell::new(ws_name));
            }
            table.add_row(row);
        }
    }

    println!("{table}");
}

/// Flatten charters into depth-first hierarchy order for tabular display.
///
/// Roots (charters that are not a child of any other in the set) come first,
/// each followed by its descendants. Parent resolution follows
/// [`Charter::is_child_of`] — alias or UUID, never title.
fn sort_charters_hierarchically<'a>(charters: &[&'a Charter]) -> Vec<&'a Charter> {
    let mut roots: Vec<&Charter> = charters
        .iter()
        .copied()
        .filter(|c| !charters.iter().any(|p| c.is_child_of(p)))
        .collect();
    roots.sort_by(|a, b| a.title.cmp(&b.title));

    let mut result = Vec::new();
    for root in roots {
        flatten_charter_hierarchy(root, charters, &mut result);
    }
    result
}

fn flatten_charter_hierarchy<'a>(
    charter: &'a Charter,
    all: &[&'a Charter],
    result: &mut Vec<&'a Charter>,
) {
    result.push(charter);
    let mut kids: Vec<&Charter> = all
        .iter()
        .copied()
        .filter(|c| c.is_child_of(charter))
        .collect();
    kids.sort_by(|a, b| a.title.cmp(&b.title));
    for kid in kids {
        flatten_charter_hierarchy(kid, all, result);
    }
}

fn open_act_count(charter: &Charter) -> usize {
    charter
        .actions
        .iter()
        .filter(|a| !matches!(a.state, ActionState::Completed | ActionState::Cancelled))
        .count()
}

pub fn show_charter(ctx: &CommandContext, query: &str) -> anyhow::Result<()> {
    let models = ctx.all_domain_models()?;

    let candidates: Vec<&Charter> = models
        .iter()
        .flat_map(|(_, model)| &model.charters)
        .collect();
    let found = resolve_charter(&candidates, query)?
        .ok_or_else(|| anyhow::anyhow!("No charter found matching '{}'", query))?;

    println!("{}", crate::display::render_charter_detail(found));
    Ok(())
}

/// Resolve a charter by canonical UUID/alias reference, with partial-title
/// matching retained as an explicit human-friendly command search fallback.
pub fn resolve_charter<'a, T>(charters: &'a [T], query: &str) -> anyhow::Result<Option<&'a Charter>>
where
    T: std::borrow::Borrow<Charter> + clearhead_core::ReferenceEntity,
{
    let query = query.trim_start_matches('/');
    match clearhead_core::select_reference(charters, query) {
        clearhead_core::ReferenceSelection::Unique { index, .. } => {
            Ok(Some(charters[index].borrow()))
        }
        clearhead_core::ReferenceSelection::Ambiguous { indices, .. } => {
            let candidates = indices
                .into_iter()
                .map(|index| charters[index].borrow().id.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            anyhow::bail!(
                "Ambiguous charter reference '{}'; candidates: {}",
                query,
                candidates
            )
        }
        clearhead_core::ReferenceSelection::NotFound => {
            let query_lower = query.to_lowercase();
            Ok(charters.iter().find_map(|candidate| {
                let charter = candidate.borrow();
                charter
                    .title
                    .to_lowercase()
                    .contains(&query_lower)
                    .then_some(charter)
            }))
        }
    }
}

pub fn add_charter(
    ctx: &CommandContext,
    title: &str,
    alias: &Option<String>,
    parent: &Option<String>,
    template: &Option<String>,
    dry_run: bool,
) -> anyhow::Result<()> {
    use clearhead_core::domain::Charter;
    use clearhead_core::workspace::templates;

    let id = uuid::Uuid::now_v7();
    let charter = Charter {
        id,
        title: title.to_string(),
        description: None,
        alias: alias.clone(),
        parent: parent.clone(),
        objectives: None,
        state: None,
        plans: vec![],
        actions: vec![],
    };

    if dry_run {
        let formatted = clearhead_core::format_charter(&charter);
        println!("{}", formatted);
        if let Some(tpl_name) = template {
            println!("Would apply template '{}'", tpl_name);
        }
        return Ok(());
    }

    let filename = alias
        .as_deref()
        .unwrap_or(title)
        .to_lowercase()
        .replace(' ', "-")
        .replace('&', "and");

    // If a parent is specified, resolve it to find the correct workspace and target directory.
    let (target_dir, data_root) = if let Some(parent_query) = parent {
        let (parent_mc, ws_root) = resolve_charter_across_workspaces(ctx, parent_query)?;
        let dir = sub_charter_dir(&ws_root, &parent_mc)?;
        std::fs::create_dir_all(&dir)
            .with_context(|| format!("Failed to create directory '{}'", dir.display()))?;
        (dir, clearhead_core::workspace_data_root(&ws_root))
    } else {
        (
            clearhead_core::charter_root(&ctx.data_dir),
            clearhead_core::workspace_data_root(&ctx.data_dir),
        )
    };

    let file_path = target_dir.join(format!("{}.md", filename));

    if file_path.exists() {
        anyhow::bail!("File already exists: {}", file_path.display());
    }

    let content = clearhead_core::format_charter(&charter);
    clearhead_core::workspace::durability::atomic_write(&file_path, content)
        .context("Failed to write charter")?;

    // Always create the companion .actions file so the charter is immediately usable.
    let actions_path = target_dir.join(format!("{}.actions", filename));
    if !actions_path.exists() {
        clearhead_core::workspace::durability::atomic_write(&actions_path, "")
            .context("Failed to create actions file")?;
    }

    // Record the charter's identity in the sidecar so it self-identifies in the
    // data, independent of the filename (best-effort — a sidecar failure must
    // never fail charter creation).
    if let Err(e) = clearhead_core::workspace::sidecar::stamp_charter_id(&actions_path, id) {
        tracing::warn!(path = %actions_path.display(), error = %e, "Failed to record charter id in sidecar");
    }

    info!(title = %title, id = %id, path = %file_path.display(), "Charter created");
    println!("{}", id);

    if let Some(tpl_name) = template {
        let charter_dir = file_path.parent().unwrap_or(std::path::Path::new(""));

        let tpl_path = templates::resolve_template(charter_dir, &data_root, tpl_name)
            .context("Failed to resolve template")?
            .ok_or_else(|| anyhow::anyhow!("Template '{}' not found", tpl_name))?;

        let tpl_acts = clearhead_core::workspace::read_actions(&tpl_path)
            .context("Failed to read template")?;

        let instantiated =
            templates::instantiate_template(&tpl_acts, |_| uuid::Uuid::now_v7(), None);

        super::save_file(&actions_path, &instantiated)?;

        println!(
            "Applied template '{}': {} action(s) to {}",
            tpl_name,
            instantiated.len(),
            actions_path.display()
        );
    }

    Ok(())
}

// ============================================================================
// archive charter
// ============================================================================

/// Archive a charter (or all closed/cancelled charters) into the `archive/` region.
///
/// Requires `state: Closed` or `state: Cancelled` in the charter frontmatter.
/// Open actions in the primary `.actions` file are a hard stop unless `force`
/// is true.
pub fn archive_charter(
    ctx: &CommandContext,
    query: &Option<String>,
    file: &Option<std::path::PathBuf>,
    closed: bool,
    force: bool,
    dry_run: bool,
) -> anyhow::Result<()> {
    use clearhead_core::{
        ArchiveCharterOptions, archive_charter as do_archive, archive_terminal_charters,
    };

    let opts = ArchiveCharterOptions { force, dry_run };

    if closed {
        let mut any = false;
        for (_, ws_dir) in ctx.workspace_dirs() {
            let results = archive_terminal_charters(&ws_dir, &opts)?;
            for r in &results {
                print_archive_result(r);
                any = true;
            }
        }
        if !any {
            println!("No closed or cancelled charters found to archive.");
        }
        return Ok(());
    }

    // Resolve query: from --file, explicit query, or error
    let q: String = if let Some(file_path) = file {
        let ws_dir = ctx.workspace_for_file(file_path);
        let mcs = clearhead_core::load_workspace(&ws_dir)?;
        let charter_root = clearhead_core::charter_root(&ws_dir);
        let mc_full = resolve_charter_by_file(&mcs, file_path, &charter_root)
            .ok_or_else(|| anyhow::anyhow!("No charter found for file: {}", file_path.display()))?;
        mc_full
            .alias
            .clone()
            .unwrap_or_else(|| mc_full.title.clone())
    } else {
        query
            .as_deref()
            .context("Provide a charter name/alias/UUID, --file <path>, or --closed")?
            .to_string()
    };

    for (_, ws_dir) in ctx.workspace_dirs() {
        match do_archive(&ws_dir, &q, &opts) {
            Ok(result) => {
                print_archive_result(&result);
                return Ok(());
            }
            Err(clearhead_core::ArchiveCharterError::NotFound(_)) => continue,
            Err(e) => return Err(e.into()),
        }
    }
    anyhow::bail!("Charter '{}' not found in any workspace", q)
}

fn print_archive_result(r: &clearhead_core::ArchiveCharterResult) {
    let prefix = if r.was_dry_run {
        "[dry-run] Would archive"
    } else {
        "Archived"
    };
    println!(
        "{} charter '{}': {} primary action(s), {} completed action(s) → {}",
        prefix,
        r.charter_name,
        r.primary_actions_swept,
        r.completed_actions_swept,
        r.archive_dir.display(),
    );
}

// ============================================================================
// update charter
// ============================================================================

/// Update a charter's metadata fields (state, title, alias).
///
/// Errors if the charter has no `.md` file — use `close charter` to set state
/// on an implicit charter (it will create the file).
pub fn update_charter(
    ctx: &CommandContext,
    query: &str,
    state: &Option<crate::argparser::CharterStateArg>,
    title: &Option<String>,
    alias: &Option<String>,
    dry_run: bool,
) -> anyhow::Result<()> {
    use clearhead_cli::mutations::{CharterUpdate, apply_charter_update};

    let mcs = ctx.load_charters()?;
    let charter_root = clearhead_core::charter_root(&ctx.data_dir);
    let mc_full = find_target_charter(&mcs, Some(query), None, &charter_root)?;
    let mut updated = Charter::from(mc_full.clone());

    let md_path_rel = mc_full.md_file.as_ref().ok_or_else(|| {
        anyhow::anyhow!(
            "Charter '{}' has no .md file; use 'close charter' to create one",
            updated.title
        )
    })?;
    let md_path = charter_root.join(md_path_rel);

    apply_charter_update(
        &mut updated,
        CharterUpdate {
            state: state.map(|s| s.into()),
            title: title.clone(),
            alias: alias.clone(),
        },
    );

    let formatted = clearhead_core::format_charter(&updated);

    if dry_run {
        println!("Would write to {}:\n{}", md_path.display(), formatted);
        return Ok(());
    }

    clearhead_core::workspace::durability::atomic_write(&md_path, &formatted)
        .with_context(|| format!("Failed to write '{}'", md_path.display()))?;

    info!(charter = %updated.title, path = %md_path.display(), state = ?updated.state, "Charter updated");

    if let Some(new_state) = &updated.state {
        println!("Charter '{}' updated: state → {}", updated.title, new_state);
    } else {
        println!("Charter '{}' updated.", updated.title);
    }

    Ok(())
}

// ============================================================================
// close charter
// ============================================================================

/// Close a charter by setting its state to Closed.
///
/// If the charter already has a `.md` file, updates it in place.
/// If not (implicit charter), creates a minimal `.md` file alongside the
/// existing `.actions` file.
pub fn close_charter(
    ctx: &CommandContext,
    query: Option<&str>,
    file: Option<&std::path::Path>,
    dry_run: bool,
) -> anyhow::Result<()> {
    use clearhead_cli::mutations::{CharterUpdate, apply_charter_update};
    use clearhead_core::CharterState;

    let ws_root = file
        .map(|f| ctx.workspace_for_file(f))
        .unwrap_or_else(|| ctx.data_dir.clone());
    let mcs = clearhead_core::load_workspace(&ws_root)?;
    let charter_root = clearhead_core::charter_root(&ws_root);
    let mc_full = find_target_charter(&mcs, query, file, &charter_root)?;
    let mut updated = Charter::from(mc_full.clone());

    let (md_path, is_new) = if let Some(md_rel) = &mc_full.md_file {
        (charter_root.join(md_rel), false)
    } else {
        let path = mc_full
            .actions_file
            .as_ref()
            .and_then(|p| {
                let stem = p.file_stem()?.to_str()?;
                let dir = p.parent().unwrap_or(std::path::Path::new(""));
                Some(charter_root.join(dir).join(format!("{}.md", stem)))
            })
            .unwrap_or_else(|| {
                let slug = updated
                    .title
                    .to_lowercase()
                    .replace(' ', "-")
                    .replace('&', "and");
                charter_root.join(format!("{}.md", slug))
            });
        (path, true)
    };

    apply_charter_update(
        &mut updated,
        CharterUpdate {
            state: Some(CharterState::Closed),
            ..Default::default()
        },
    );

    let formatted = clearhead_core::format_charter(&updated);

    if dry_run {
        let verb = if is_new {
            "Would create"
        } else {
            "Would update"
        };
        println!("{} {}:\n{}", verb, md_path.display(), formatted);
        return Ok(());
    }

    clearhead_core::workspace::durability::atomic_write(&md_path, &formatted)
        .with_context(|| format!("Failed to write '{}'", md_path.display()))?;

    info!(charter = %updated.title, path = %md_path.display(), created = is_new, "Charter closed");

    let verb = if is_new { "created" } else { "updated" };
    println!(
        "Charter '{}' closed: {} {}",
        updated.title,
        verb,
        md_path.display()
    );

    Ok(())
}
