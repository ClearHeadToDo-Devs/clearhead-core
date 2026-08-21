//! Handlers for action commands (expand, complete, cancel, update, read, archive).

use std::io::IsTerminal;
use std::path::{Path, PathBuf};

use anyhow::Context;
use chrono::Local;
use tracing::{info, warn};

use clearhead_core::workspace::action_files;
use clearhead_core::{Action, ActionList, ActionState, PredecessorRef};

use super::CommandContext;
use super::verb_result::{VerbError, VerbOutcome, canonical_id, emit};

// ============================================================================
// expand actions — ICS schedule → .actions file
// ============================================================================

/// Add a new standalone action to a charter's `.actions` file.
///
/// This is the CLI adapter boundary: each argument corresponds directly to a
/// clap flag before the values are assembled into the domain `Action`.
#[allow(clippy::too_many_arguments)]
pub fn add_action(
    ctx: &CommandContext,
    name: &str,
    charter: &Option<String>,
    file: &Option<PathBuf>,
    parent: &Option<String>,
    priority: Option<u32>,
    state: Option<crate::argparser::ActionStateArg>,
    alias: &Option<String>,
    description: &Option<String>,
    context: &[String],
    predecessor: &[String],
    sequential: bool,
    scheduled_at: &Option<String>,
    duration: Option<u32>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let actions_path = resolve_acts_file(ctx, charter, file)?;
    // Client-side read: resolve the fuzzy parent query to a stable selector and
    // support the dry-run preview. Core re-reads under the lock and re-resolves
    // the parent there, so this read is never the one that's written against.
    let list = action_files::read_actions(&actions_path)?;

    let parent_selector = parent
        .as_deref()
        .map(|query| {
            find_best_match(&list, query, is_open_action)?
                .map(clearhead_core::ActionSelector::from)
                .ok_or_else(|| anyhow::anyhow!("No action found matching parent '{}'", query))
        })
        .transpose()?;

    let new_scheduled = scheduled_at
        .as_deref()
        .map(|s| {
            chrono::DateTime::parse_from_rfc3339(s)
                .map(|dt| dt.with_timezone(&Local))
                .map_err(|e| anyhow::anyhow!("Invalid --scheduled-at '{}': {}", s, e))
        })
        .transpose()?;

    let action = Action {
        name: name.to_string(),
        priority,
        state: state.map(Into::into).unwrap_or(ActionState::NotStarted),
        alias: alias.clone(),
        description: description.clone(),
        contexts: if context.is_empty() {
            None
        } else {
            Some(context.to_vec())
        },
        predecessors: if predecessor.is_empty() {
            None
        } else {
            Some(predecessor_refs(predecessor))
        },
        is_sequential: if sequential { Some(true) } else { None },
        scheduled_at: new_scheduled,
        duration,
        created_at: Some(Local::now()),
        ..Default::default()
    };

    if dry_run {
        println!("Would add action '{}' to {}", name, actions_path.display());
        return Ok(());
    }

    let workspace_root = ctx.workspace_for_file(&actions_path);
    let result = clearhead_core::insert_action(
        &workspace_root,
        &actions_path,
        action,
        parent_selector.as_ref(),
    )?;

    info!(id = %result.action_id, name = %name, "Action added");
    println!(
        "Added action {} ({})",
        &result.action_id.to_string()[..8],
        name
    );
    Ok(())
}

fn predecessor_refs(references: &[String]) -> Vec<PredecessorRef> {
    references
        .iter()
        .map(|raw_ref| PredecessorRef {
            raw_ref: raw_ref.clone(),
            resolved_uuid: None,
        })
        .collect()
}

/// Resolve the `.actions` file path from a charter query or explicit file path.
fn resolve_acts_file(
    ctx: &CommandContext,
    charter: &Option<String>,
    file: &Option<PathBuf>,
) -> anyhow::Result<PathBuf> {
    if let Some(path) = file {
        return Ok(path.clone());
    }
    if let Some(query) = charter {
        let (mc, ws_root) = resolve_charter_across_workspaces(ctx, query)?;
        let rel = mc.actions_file.as_ref().ok_or_else(|| {
            anyhow::anyhow!("Charter '{}' has no associated actions file", mc.title)
        })?;
        let root = clearhead_core::charter_root(&ws_root);
        return Ok(root.join(rel));
    }

    let primary_charters = ctx.load_charters()?;
    let actionable: Vec<_> = primary_charters
        .iter()
        .filter_map(|mc| mc.actions_file.as_ref().map(|rel| (mc, rel)))
        .collect();

    if actionable.len() == 1 {
        let (_mc, rel) = actionable[0];
        let root = clearhead_core::charter_root(&ctx.data_dir);
        return Ok(root.join(rel));
    }

    let default_path = ctx.resolve_action_file(None);
    if default_path.exists() {
        return Ok(default_path);
    }

    anyhow::bail!("Specify --charter <name> or --file <path> to target a charter's actions file")
}

// ============================================================================
// Action lifecycle — complete, cancel, update
// ============================================================================

/// Mark an open action as completed (moves to `.completed.actions`).
pub fn complete_action(
    ctx: &CommandContext,
    query: &str,
    charter: &Option<String>,
    file: &Option<PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    close_action_subtree(
        ctx,
        query,
        charter,
        file,
        dry_run,
        ActionState::Completed,
        "complete",
    )
}

/// Shared body of `complete`/`cancel`: resolve the target identity, then hand
/// the locked read-plan-apply mutation to core. Only the target state and
/// message wording differ.
fn close_action_subtree(
    ctx: &CommandContext,
    query: &str,
    charter: &Option<String>,
    file: &Option<PathBuf>,
    dry_run: bool,
    closing_state: ActionState,
    verb_present: &str,
) -> anyhow::Result<()> {
    let Some((actions_path, mut open_actions)) =
        find_and_load_open_actions(ctx, file, charter, query)?
    else {
        // Not a materialized line in any file — it may be a projected recurring
        // occurrence, acted on by writing a deviation to its master rather than
        // editing a line. Materialized actions always win, so this only runs
        // after the file search comes up empty.
        if try_close_occurrence(ctx, query, closing_state, dry_run)? {
            return Ok(());
        }
        return Err(verb_target_error(ctx, query)?.into());
    };

    let (action_id, selector) = match find_action_mut(&mut open_actions, query)? {
        Some(action) => (action.id, clearhead_core::ActionSelector::from(&*action)),
        None => return Err(verb_target_error(ctx, query)?.into()),
    };

    let subtree_ids = clearhead_core::collect_subtree_ids(&open_actions, action_id);

    if dry_run {
        println!(
            "Would {} action {} and {} child(ren)",
            verb_present,
            &action_id.to_string()[..8],
            subtree_ids.len() - 1,
        );
        return Ok(());
    }

    let workspace_root = ctx.workspace_for_file(&actions_path);
    let result = clearhead_core::close_action_subtree(
        &workspace_root,
        &actions_path,
        &selector,
        closing_state,
        Local::now(),
    )?;

    // If the closed line was a materialized recurring occurrence, closing it is only
    // half the operation: write the completed/skip deviation to its master and stamp
    // the plan's next token. `resolve_materialized_occurrence` is a no-op (`false`)
    // for an ordinary action, so this is safe to attempt unconditionally.
    let occurrence_op = match closing_state {
        ActionState::Completed => Some(clearhead_core::OccurrenceOp::Complete { at: Local::now() }),
        ActionState::Cancelled => Some(clearhead_core::OccurrenceOp::Skip),
        _ => None,
    };
    if let Some(op) = occurrence_op
        && clearhead_core::resolve_materialized_occurrence(
            &workspace_root,
            ctx.plan_override().as_deref(),
            action_id,
            &op,
            Local::now(),
        )?
    {
        info!(%action_id, "Recurring occurrence: deviation written to master, next token stamped");
    }

    let children = if result.already_closed {
        subtree_ids.len().saturating_sub(1)
    } else {
        result.closed_count.saturating_sub(1)
    };
    let outcome = match closing_state {
        ActionState::Cancelled => VerbOutcome::Cancelled {
            id: canonical_id(action_id),
            children,
        },
        _ => VerbOutcome::Completed {
            id: canonical_id(action_id),
            children,
        },
    };
    info!(%action_id, children, "Action subtree closed ({:?})", closing_state);
    emit(&outcome);
    Ok(())
}

/// Close a *projected* recurring occurrence by recording a deviation on its
/// master: `complete` → completed `RECURRENCE-ID` override, `cancel` → `EXDATE`
/// (skip this instance). Returns `Ok(false)` when `query` matches no open
/// projected occurrence, so the caller can fall through to its not-found error.
///
/// Occurrences have no `.actions` line; this is the write half of the
/// operations-uniform / text-editing-not seam. The branch lives here, behind the
/// operation, not in each command.
fn try_close_occurrence(
    ctx: &CommandContext,
    query: &str,
    closing_state: ActionState,
    dry_run: bool,
) -> anyhow::Result<bool> {
    use anyhow::Context;

    let model = ctx.load_model()?; // materialized-only; the present occurrence is a real line
    let occurrences: Vec<&Action> = model
        .all_actions()
        .into_iter()
        .filter(|action| action.external_occurrence_key.is_some())
        .collect();
    let Some(occurrence) = find_best_match(&occurrences, query, is_open_action)?.cloned() else {
        return Ok(false);
    };

    let plan_id = occurrence
        .plan_id
        .context("projected occurrence is missing its plan_id handle")?;
    let key = occurrence
        .external_occurrence_key
        .clone()
        .context("projected occurrence is missing its occurrence key")?;
    let op = match closing_state {
        ActionState::Completed => clearhead_core::OccurrenceOp::Complete { at: Local::now() },
        ActionState::Cancelled => clearhead_core::OccurrenceOp::Skip,
        other => anyhow::bail!("cannot map state {other:?} to an occurrence operation"),
    };

    if dry_run {
        let verb = if closing_state == ActionState::Cancelled {
            "skip"
        } else {
            "complete"
        };
        println!(
            "Would {} occurrence {} of plan {}",
            verb,
            &occurrence.id.to_string()[..8],
            &plan_id.to_string()[..8],
        );
        return Ok(true);
    }

    clearhead_core::apply_occurrence_op(
        &ctx.data_dir,
        ctx.plan_override().as_deref(),
        plan_id,
        &key,
        &op,
    )?;

    let outcome = match closing_state {
        ActionState::Cancelled => VerbOutcome::Cancelled {
            id: canonical_id(occurrence.id),
            children: 0,
        },
        _ => VerbOutcome::Completed {
            id: canonical_id(occurrence.id),
            children: 0,
        },
    };
    info!(%occurrence.id, %plan_id, "Occurrence deviation written ({:?})", closing_state);
    emit(&outcome);
    Ok(true)
}

/// Reschedule a *projected* recurring occurrence by writing a `RECURRENCE-ID`
/// override with a new time — the update-path sibling of [`try_close_occurrence`].
/// A projected occurrence has no line to text-edit, so only reschedule is
/// supported; any other field edit is rejected. Returns `Ok(false)` when `query`
/// matches no open projected occurrence (caller falls through to not-found).
fn try_reschedule_occurrence(
    ctx: &CommandContext,
    query: &str,
    new_scheduled: Option<chrono::DateTime<Local>>,
    other_edits: bool,
    dry_run: bool,
) -> anyhow::Result<bool> {
    use anyhow::Context;

    let model = ctx.load_model()?;
    let occurrences: Vec<&Action> = model
        .all_actions()
        .into_iter()
        .filter(|action| action.external_occurrence_key.is_some())
        .collect();
    let Some(occurrence) = find_best_match(&occurrences, query, is_open_action)?.cloned() else {
        return Ok(false);
    };

    if other_edits {
        anyhow::bail!(
            "a projected recurring occurrence supports only reschedule (--scheduled-at); \
             edit the plan or a materialized action for other fields"
        );
    }
    let Some(scheduled_at) = new_scheduled else {
        anyhow::bail!("nothing to reschedule: pass --scheduled-at for a projected occurrence");
    };

    let plan_id = occurrence
        .plan_id
        .context("projected occurrence is missing its plan_id handle")?;
    let key = occurrence
        .external_occurrence_key
        .clone()
        .context("projected occurrence is missing its occurrence key")?;

    if dry_run {
        println!(
            "Would reschedule occurrence {} to {}",
            &occurrence.id.to_string()[..8],
            scheduled_at.format("%Y-%m-%d %H:%M"),
        );
        return Ok(true);
    }

    clearhead_core::apply_occurrence_op(
        &ctx.data_dir,
        ctx.plan_override().as_deref(),
        plan_id,
        &key,
        &clearhead_core::OccurrenceOp::Reschedule {
            scheduled_at: Some(scheduled_at),
            due_date: None,
        },
    )?;
    info!(%occurrence.id, %plan_id, "Occurrence rescheduled via deviation");
    emit(&VerbOutcome::Updated {
        id: canonical_id(occurrence.id),
    });
    Ok(true)
}

/// Update an open action's fields.
///
/// Kept explicit at the CLI adapter boundary so flag-to-field wiring remains
/// visible; core receives the assembled `ActionUpdate` value below.
#[allow(clippy::too_many_arguments)]
pub fn update_action(
    ctx: &CommandContext,
    query: &str,
    name: &Option<String>,
    priority: Option<u32>,
    state: Option<crate::argparser::ActionStateArg>,
    scheduled_at: &Option<String>,
    duration: &Option<u32>,
    description: &Option<String>,
    context: &[String],
    predecessor: &[String],
    sequential: bool,
    charter: &Option<String>,
    file: &Option<PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let new_scheduled = scheduled_at
        .as_deref()
        .map(|s| {
            chrono::DateTime::parse_from_rfc3339(s)
                .map(|dt| dt.with_timezone(&Local))
                .map_err(|e| anyhow::anyhow!("Invalid --scheduled-at '{}': {}", s, e))
        })
        .transpose()?;

    let Some((actions_path, mut open_actions)) =
        find_and_load_open_actions(ctx, file, charter, query)?
    else {
        // Not a materialized line — it may be a projected occurrence, which
        // supports only operations (here: reschedule via a RECURRENCE-ID override).
        let other_edits = name.is_some()
            || priority.is_some()
            || state.is_some()
            || duration.is_some()
            || description.is_some()
            || !context.is_empty()
            || !predecessor.is_empty()
            || sequential;
        if try_reschedule_occurrence(ctx, query, new_scheduled, other_edits, dry_run)? {
            return Ok(());
        }
        return Err(verb_target_error(ctx, query)?.into());
    };

    // Client-side read resolves the fuzzy query to a stable selector; core
    // re-reads under the lock and applies the update there.
    let (action_id, selector) = match find_action_mut(&mut open_actions, query)? {
        Some(action) => (action.id, clearhead_core::ActionSelector::from(&*action)),
        None => return Err(verb_target_error(ctx, query)?.into()),
    };

    let update = clearhead_core::ActionUpdate {
        name: name.clone(),
        description: description.clone(),
        priority,
        state: state.map(Into::into),
        context: if context.is_empty() {
            None
        } else {
            Some(context.to_vec())
        },
        predecessors: if predecessor.is_empty() {
            None
        } else {
            Some(predecessor_refs(predecessor))
        },
        is_sequential: if sequential { Some(true) } else { None },
        scheduled_at: new_scheduled,
        duration: *duration,
        ..Default::default()
    };

    // Fail fast — and honestly under --dry-run — on a terminal state before
    // touching the file. Core enforces the same rule at its boundary.
    if let Some(state) = clearhead_core::disallowed_terminal_update(&update) {
        anyhow::bail!(
            "cannot set state to {state:?} via update; use complete/cancel, \
             which cascade to the subtree and archive it"
        );
    }

    if dry_run {
        println!("Would update action {}", &action_id.to_string()[..8]);
        return Ok(());
    }

    let workspace_root = ctx.workspace_for_file(&actions_path);
    let result = clearhead_core::update_action(&workspace_root, &actions_path, &selector, update)?;
    info!(action_id = %result.action_id, "Action updated");
    emit(&VerbOutcome::Updated {
        id: canonical_id(result.action_id),
    });
    Ok(())
}

/// Delete an action from the workspace (open or closed).
pub fn delete_action(
    ctx: &CommandContext,
    query: &str,
    charter: &Option<String>,
    file: &Option<PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    // Try open actions first, then completed.
    let action_files: Vec<PathBuf> = if let Some(path) = file {
        vec![path.clone()]
    } else if let Some(charter_query) = charter {
        let (mc, ws_root) = resolve_charter_across_workspaces(ctx, charter_query)?;
        let rel = mc.actions_file.as_ref().ok_or_else(|| {
            anyhow::anyhow!("Charter '{}' has no associated actions file", mc.title)
        })?;
        vec![clearhead_core::charter_root(&ws_root).join(rel)]
    } else {
        let mut all = Vec::new();
        for (_, ws_dir) in ctx.workspace_dirs() {
            let files = clearhead_core::list_action_files(&ws_dir)
                .with_context(|| format!("Failed to list workspace '{}'", ws_dir.display()))?;
            all.extend(files);
        }
        all
    };

    for actions_path in &action_files {
        // Resolve the target in the active file first, then the completed file —
        // delete reaches an action wherever it lives. Either way the mutation is
        // handed to core, which re-resolves under the lock, cascades the subtree
        // in the owning file, and prunes the matching sidecar entries.
        let open = action_files::read_actions(actions_path)?;
        let resolved = match find_best_match(&open, query, |_| true)? {
            Some(action) => Some((
                clearhead_core::ActionSelector::from(action),
                clearhead_core::collect_subtree_ids(&open, action.id),
            )),
            None => {
                let completed_path = action_files::completed_actions_path(actions_path);
                let closed = action_files::read_actions(&completed_path)?;
                find_best_match(&closed, query, |_| true)?.map(|action| {
                    (
                        clearhead_core::ActionSelector::from(action),
                        clearhead_core::collect_subtree_ids(&closed, action.id),
                    )
                })
            }
        };

        let Some((selector, subtree_ids)) = resolved else {
            continue;
        };

        if dry_run {
            println!(
                "Would delete action {} (+{} children)",
                &selector.id.to_string()[..8],
                subtree_ids.len().saturating_sub(1),
            );
            return Ok(());
        }

        let workspace_root = ctx.workspace_for_file(actions_path);
        let result = clearhead_core::delete_action(&workspace_root, actions_path, &selector)?;
        let children = result.deleted_count.saturating_sub(1);
        info!(
            action_id = %result.action_id,
            children,
            from_completed = result.from_completed,
            "Action subtree deleted"
        );
        println!(
            "Deleted action {} (+{} children)",
            &result.action_id.to_string()[..8],
            children
        );
        return Ok(());
    }

    anyhow::bail!("No action found matching '{}'", query)
}

/// Cancel an open action and all its descendants (moves to `.completed.actions` with Cancelled state).
pub fn cancel_action(
    ctx: &CommandContext,
    query: &str,
    charter: &Option<String>,
    file: &Option<PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    close_action_subtree(
        ctx,
        query,
        charter,
        file,
        dry_run,
        ActionState::Cancelled,
        "cancel",
    )
}

// ============================================================================
// read actions
// ============================================================================

/// List actions, optionally filtered by charter, plan name, and/or context tags.
#[allow(clippy::too_many_arguments)]
pub fn read_actions_cmd(
    ctx: &CommandContext,
    format: Option<crate::argparser::OutputMode>,
    plan_filter: Option<&str>,
    charter_filter: Option<&str>,
    context_filter: &[String],
    open_only: bool,
    states: &[crate::argparser::ActionStateArg],
    file: &Option<PathBuf>,
) -> anyhow::Result<()> {
    let charter_acts_file: Option<PathBuf> = if let Some(query) = charter_filter {
        let (mc, ws_root) = resolve_charter_across_workspaces(ctx, query)?;
        let rel = mc.actions_file.as_ref().ok_or_else(|| {
            anyhow::anyhow!("Charter '{}' has no associated actions file", mc.title)
        })?;
        let root = clearhead_core::charter_root(&ws_root);
        Some(root.join(rel))
    } else {
        None
    };
    let effective_file = charter_acts_file.as_ref().or(file.as_ref()).cloned();

    let wc = ctx.workspace_config();
    let search_all_workspaces = effective_file.is_none()
        && (ctx.workspace_filter.is_some() || !wc.additional_workspaces.is_empty());
    let multi_ws = effective_file.is_none() && ctx.workspace_dirs().len() > 1;

    // Pre-expand context filter tags downward (general → specific) so ActionFilter::matches
    // can do a simple set-membership check. Filtering by "computer" will match actions
    // tagged "terminal" or "neovim" because those are descendants of "computer".
    let expanded_context_tags: Vec<String> = context_filter
        .iter()
        .flat_map(|t| wc.descendants_and_self(t))
        .collect();
    let action_filter = clearhead_core::ActionFilter {
        open_only,
        states: states.iter().map(|s| (*s).into()).collect(),
        context_tags: expanded_context_tags,
        plan_ref: plan_filter.map(String::from),
    };

    // ws_actions drives non-TTY output (DSL, JSON, table). collect open_only early as a
    // performance hint; action_filter.matches enforces all remaining criteria.
    let ws_actions: Vec<(Option<String>, Action)> = if search_all_workspaces {
        collect_workspace_actions(ctx, open_only)?
    } else {
        collect_all_actions(ctx, &effective_file, open_only)?
            .into_iter()
            .map(|a| (None, a))
            .collect()
    };

    let filtered: Vec<(Option<&str>, &Action)> = ws_actions
        .iter()
        .filter(|(_, a)| action_filter.matches(a))
        .map(|(ws, a)| (ws.as_deref(), a))
        .collect();

    match format {
        Some(crate::argparser::OutputMode::JsonLd) => {
            // Serialize the *filtered* model — --charter/--context/--open-only/--state
            // must narrow JSON-LD output just as they narrow the table and tree.
            let model = filtered_primary_model(ctx, charter_filter, &action_filter)?;
            let jsonld = clearhead_cli::serialize_domain_to_jsonld(&model)
                .map_err(|e| anyhow::anyhow!("Failed to serialize JSON-LD: {e}"))?;
            println!("{}", jsonld);
        }
        Some(crate::argparser::OutputMode::Json) => {
            // Canonical schema projection — the same shape Core validates against
            // the specifications actions schema. --charter/--context/--open-only/
            // --state narrow it just as they narrow the table and tree.
            let actions: Vec<Action> = filtered.iter().map(|(_, a)| (*a).clone()).collect();
            let document = clearhead_core::schema_export::to_schema_document(&actions);
            println!("{}", serde_json::to_string_pretty(&document)?);
        }
        Some(crate::argparser::OutputMode::Ids) => {
            for (_, action) in &filtered {
                println!("{}", action.id);
            }
        }
        Some(crate::argparser::OutputMode::Table) => print_acts_table(&filtered, multi_ws),
        None => {
            if !std::io::stdout().is_terminal() {
                // Pipe/redirect: emit .actions DSL so output can be saved or piped.
                let actions: Vec<&Action> = filtered.iter().map(|(_, a)| *a).collect();
                let list: clearhead_core::ActionList = actions.into_iter().cloned().collect();
                let text = clearhead_core::format(
                    &list,
                    clearhead_core::OutputFormat::Actions,
                    None,
                    None,
                )
                .map_err(|e| anyhow::anyhow!("Failed to format actions: {}", e))?;
                print!("{}", text);
            } else {
                // TTY: always render the domain hierarchy tree, filtered if needed.
                let model = filtered_primary_model(ctx, charter_filter, &action_filter)?;

                if multi_ws {
                    for (ws_name, ws_path) in ctx.workspace_dirs() {
                        let is_primary = ws_path == ctx.data_dir;
                        let mut ws_model = if is_primary {
                            model.clone()
                        } else {
                            match clearhead_core::load_domain_model(&ws_path) {
                                Ok(m) => m,
                                Err(e) => {
                                    tracing::warn!(
                                        "Skipping workspace '{}': {}",
                                        ws_path.display(),
                                        e
                                    );
                                    continue;
                                }
                            }
                        };
                        clearhead_core::apply_filter(&mut ws_model, &action_filter);
                        println!("▸ {}", ws_name);
                        print!("{}", crate::display::render_domain_tree(&ws_model));
                    }
                } else {
                    print!("{}", crate::display::render_domain_tree(&model));
                }
            }
        }
    }

    Ok(())
}

/// Load the primary domain model, optionally narrowed to a single charter, with
/// the action filter applied. Shared by the JSON-LD and TTY branches so both
/// honor --charter/--context/--open-only/--state identically — the JSON path
/// used to skip this and serialize the whole workspace unfiltered.
fn filtered_primary_model(
    ctx: &CommandContext,
    charter_filter: Option<&str>,
    action_filter: &clearhead_core::ActionFilter,
) -> anyhow::Result<clearhead_core::DomainModel> {
    let primary = ctx.load_model()?;
    let mut model = if let Some(query) = charter_filter {
        let charter = super::charter::resolve_charter(&primary.charters, query)?
            .ok_or_else(|| anyhow::anyhow!("No charter found matching '{}'", query))?
            .clone();
        clearhead_core::DomainModel {
            objectives: vec![],
            charters: vec![charter],
        }
    } else {
        primary
    };
    clearhead_core::apply_filter(&mut model, action_filter);
    Ok(model)
}

/// Collect actions from the primary workspace and all configured additional workspaces.
/// Each action is paired with its workspace name (`None` when not in multi-workspace context).
/// Same convergence as `collect_all_actions`, fanned out across every configured
/// workspace: the loader gives journal recovery, sidecar hydration, and its own
/// load-finding warnings per workspace, instead of the CLI re-deriving a coarser
/// per-file warn! from a raw parse.
fn collect_workspace_actions(
    ctx: &CommandContext,
    open_only: bool,
) -> anyhow::Result<Vec<(Option<String>, Action)>> {
    let multi_ws = ctx.workspace_dirs().len() > 1;
    let mut result = Vec::new();

    for (ws_name, ws_path) in ctx.workspace_dirs() {
        let is_primary = ws_path == ctx.data_dir;
        let label = if multi_ws { Some(ws_name) } else { None };

        let charters = match clearhead_core::load_workspace(&ws_path) {
            Ok(c) => c,
            Err(e) if is_primary => return Err(e.into()),
            Err(e) => {
                warn!("Skipping workspace '{}': {}", ws_path.display(), e);
                continue;
            }
        };
        let charter_root = clearhead_core::charter_root(&ws_path);

        for mc in &charters {
            let mut open: Vec<Action> = mc
                .actions
                .iter()
                .map(|sourced| sourced.action.clone())
                .collect();
            // Occurrences are not projected into listings — the present due
            // occurrence is a materialized `.actions` line and appears above like
            // any other action; the future is a calendar-view concern.
            if open_only {
                open.retain(is_open_action);
            }
            for action in open {
                result.push((label.clone(), action));
            }
            if !open_only && let Some(actions_file) = &mc.actions_file {
                let completed_path =
                    action_files::completed_actions_path(&charter_root.join(actions_file));
                if let Ok(completed) = action_files::read_actions(&completed_path) {
                    for action in completed {
                        result.push((label.clone(), action));
                    }
                }
            }
        }
    }

    Ok(result)
}

/// Show details for one action from open and completed action stores.
pub fn show_action(
    ctx: &CommandContext,
    query: &str,
    file: &Option<PathBuf>,
) -> anyhow::Result<()> {
    let actions: Vec<Action> =
        if file.is_none() && !ctx.workspace_config().additional_workspaces.is_empty() {
            collect_workspace_actions(ctx, false)?
                .into_iter()
                .map(|(_, a)| a)
                .collect()
        } else {
            collect_all_actions(ctx, file, false)?
        };

    let action = find_best_match(&actions, query, |_| true)?
        .ok_or_else(|| anyhow::anyhow!("No action found matching '{}'", query))?;

    println!("{}", crate::display::render_action_detail(action));
    Ok(())
}

// ============================================================================
// archive actions
// ============================================================================

/// Sweep completed/cancelled actions from `.actions` into `.completed.actions`.
pub fn archive_actions(
    ctx: &CommandContext,
    scope: &Option<String>,
    file: &Option<PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let charter_paths: Vec<PathBuf> = if let Some(f) = file {
        vec![f.clone()]
    } else if let Some(s) = scope {
        use crate::commands::resolver::{ResolvedScope, resolve_domain_ref};
        match resolve_domain_ref(ctx, s)? {
            ResolvedScope::Charter { file_path }
            | ResolvedScope::Plan { file_path }
            | ResolvedScope::Action { file_path } => vec![file_path],
        }
    } else {
        clearhead_core::list_action_files(&ctx.data_dir).context("Failed to list workspace")?
    };

    let mut total_archived = 0usize;
    let mut charters_touched = 0usize;

    for actions_path in &charter_paths {
        let archived_count = if dry_run {
            let active = action_files::read_actions(actions_path)?;
            let completed_path = action_files::completed_actions_path(actions_path);
            let completed = action_files::read_actions(&completed_path)?;
            clearhead_core::plan_action_archive(&active, &completed).archived_count
        } else {
            let workspace_root = ctx.workspace_for_file(actions_path);
            clearhead_core::archive_actions(&workspace_root, actions_path)?.archived_count
        };

        if archived_count == 0 {
            continue;
        }

        if dry_run {
            println!(
                "Would archive {} action(s) from {}",
                archived_count,
                actions_path.display()
            );
        } else {
            info!(
                count = archived_count,
                charter = %actions_path.display(),
                "Actions archived"
            );
        }

        total_archived += archived_count;
        charters_touched += 1;
    }

    if total_archived == 0 {
        println!("Nothing to archive.");
    } else if dry_run {
        println!(
            "Would archive {} action(s) across {} charter(s).",
            total_archived, charters_touched
        );
    } else {
        println!(
            "Archived {} action(s) across {} charter(s).",
            total_archived, charters_touched
        );
    }

    Ok(())
}

// ============================================================================
// Private helpers
// ============================================================================

/// Locate the `.actions` file a mutation verb should operate on. `Ok(None)`
/// means the workspace scan found no open match — the caller builds the typed
/// target error; hard errors (io, parse, unknown charter) stay `Err`.
fn find_and_load_open_actions(
    ctx: &CommandContext,
    file: &Option<PathBuf>,
    charter: &Option<String>,
    query: &str,
) -> anyhow::Result<Option<(PathBuf, ActionList)>> {
    if let Some(path) = file {
        let actions = super::load_file_for_mutation(path, "action lifecycle")?;
        return Ok(Some((path.clone(), actions)));
    }
    if let Some(charter_query) = charter {
        let (mc, ws_root) = resolve_charter_across_workspaces(ctx, charter_query)?;
        let rel = mc.actions_file.as_ref().ok_or_else(|| {
            anyhow::anyhow!("Charter '{}' has no associated actions file", mc.title)
        })?;
        let path = clearhead_core::charter_root(&ws_root).join(rel);
        let actions = super::load_file_for_mutation(&path, "action lifecycle")?;
        return Ok(Some((path, actions)));
    }
    // Search every workspace (respects --workspace); primary errors are hard,
    // additional workspaces are skipped on error like all_domain_models.
    for (_, ws_dir) in ctx.workspace_dirs() {
        match find_act_in_open_files(&ws_dir, query) {
            Ok(Some(found)) => return Ok(Some(found)),
            Ok(None) => {}
            Err(e) if ws_dir == ctx.data_dir => return Err(e),
            Err(_) => {}
        }
    }
    Ok(None)
}

/// Scan `.actions` files in the workspace for one containing an action matching
/// `query`. `Ok(None)` when no file has an open match.
fn find_act_in_open_files(
    data_dir: &Path,
    query: &str,
) -> anyhow::Result<Option<(PathBuf, ActionList)>> {
    let paths = clearhead_core::list_action_files(data_dir).context("Failed to list workspace")?;
    let mut loaded = Vec::with_capacity(paths.len());
    for path in paths {
        loaded.push((path.clone(), action_files::read_actions(&path)?));
    }

    let candidates: Vec<&Action> = loaded
        .iter()
        .flat_map(|(_, actions)| actions.iter())
        .collect();
    let Some(target_id) =
        find_best_match(&candidates, query, is_open_action)?.map(|action| action.id)
    else {
        return Ok(None);
    };

    Ok(loaded
        .into_iter()
        .find(|(_, actions)| actions.iter().any(|action| action.id == target_id)))
}

/// Build the typed error for a verb whose query matched nothing open
/// (query_output.md, "Errors as data"). A closed match may still sit in an
/// open file (not yet archived) or in a completed archive — either way the
/// action is already closed; with no match anywhere it is not found.
fn verb_target_error(ctx: &CommandContext, query: &str) -> anyhow::Result<VerbError> {
    for (_, ws_dir) in ctx.workspace_dirs() {
        let open_files = clearhead_core::list_action_files(&ws_dir).unwrap_or_default();
        let archives: Vec<PathBuf> = open_files
            .iter()
            .map(|p| action_files::completed_actions_path(p))
            .collect();
        for path in open_files.iter().chain(&archives) {
            let Ok(actions) = action_files::read_actions(path) else {
                continue;
            };
            if let Some(action) = find_best_match(&actions, query, |a| !is_open_action(a))? {
                return Ok(VerbError::AlreadyClosed {
                    id: canonical_id(action.id),
                    state: format!("{:?}", action.state),
                    query: query.to_string(),
                });
            }
        }
    }
    Ok(VerbError::NotFound {
        query: query.to_string(),
    })
}

/// Resolve a fuzzy action query to its canonical id for the query facade's
/// chain adapter, which forwards a canonical IRI to graphd rather than a name.
pub(crate) fn resolve_action_id(ctx: &CommandContext, query: &str) -> anyhow::Result<uuid::Uuid> {
    let actions = collect_all_actions(ctx, &None, true)?;
    let action = find_best_match(&actions, query, |_| true)?
        .ok_or_else(|| anyhow::anyhow!("No open action found matching '{}'", query))?;
    Ok(action.id)
}

/// Resolve canonical UUID/alias references in core, then apply the CLI-only
/// partial-name search when no reference matches. Ambiguous canonical matches
/// are errors and report candidate identities rather than choosing file order.
fn find_best_match_pos<T>(
    actions: &[T],
    query: &str,
    filter: impl Fn(&Action) -> bool,
) -> anyhow::Result<Option<usize>>
where
    T: std::borrow::Borrow<Action> + clearhead_core::ReferenceEntity,
{
    let query = query.trim_start_matches('/');
    match clearhead_core::select_reference_where(actions, query, |candidate| {
        filter(candidate.borrow())
    }) {
        clearhead_core::ReferenceSelection::Unique { index, .. } => Ok(Some(index)),
        clearhead_core::ReferenceSelection::Ambiguous { indices, .. } => {
            let candidates = indices
                .into_iter()
                .map(|index| canonical_id(actions[index].borrow().id))
                .collect();
            Err(VerbError::Ambiguous {
                query: query.to_string(),
                candidates,
            }
            .into())
        }
        clearhead_core::ReferenceSelection::NotFound if uuid::Uuid::parse_str(query).is_ok() => {
            Ok(None)
        }
        clearhead_core::ReferenceSelection::NotFound => {
            let query_lower = query.to_lowercase();
            Ok(actions.iter().position(|candidate| {
                let action = candidate.borrow();
                filter(action) && action.name.to_lowercase().contains(&query_lower)
            }))
        }
    }
}

fn find_best_match<'a, T>(
    actions: &'a [T],
    query: &str,
    filter: impl Fn(&Action) -> bool,
) -> anyhow::Result<Option<&'a Action>>
where
    T: std::borrow::Borrow<Action> + clearhead_core::ReferenceEntity,
{
    Ok(find_best_match_pos(actions, query, filter)?.map(|index| actions[index].borrow()))
}

fn find_action_mut<'a>(
    actions: &'a mut ActionList,
    query: &str,
) -> anyhow::Result<Option<&'a mut Action>> {
    let index = find_best_match_pos(actions, query, is_open_action)?;
    Ok(index.and_then(|index| actions.get_mut(index)))
}

pub(super) fn resolve_markdown_charter<'a>(
    charters: &'a [clearhead_core::MarkdownCharter],
    query: &str,
) -> anyhow::Result<Option<&'a clearhead_core::MarkdownCharter>> {
    match clearhead_core::select_reference(charters, query) {
        clearhead_core::ReferenceSelection::Unique { index, .. } => Ok(Some(&charters[index])),
        clearhead_core::ReferenceSelection::Ambiguous { indices, .. } => {
            let candidates = indices
                .into_iter()
                .map(|index| charters[index].id.to_string())
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
            Ok(charters
                .iter()
                .find(|charter| charter.title.to_lowercase().contains(&query_lower)))
        }
    }
}

/// Search configured workspaces (respecting `--workspace` filter) for a charter matching `query`.
///
/// Returns the matched charter (owned) and the workspace root it came from.
pub(super) fn resolve_charter_across_workspaces(
    ctx: &CommandContext,
    query: &str,
) -> anyhow::Result<(clearhead_core::MarkdownCharter, PathBuf)> {
    for (_, ws_root) in ctx.workspace_dirs() {
        let is_primary = ws_root == ctx.data_dir;
        let mcs = match clearhead_core::load_workspace(&ws_root) {
            Ok(m) => m,
            Err(e) if is_primary => return Err(e.into()),
            Err(e) => {
                warn!("Skipping workspace '{}': {}", ws_root.display(), e);
                continue;
            }
        };
        if let Some(mc) = resolve_markdown_charter(&mcs, query)? {
            return Ok((mc.clone(), ws_root));
        }
    }
    anyhow::bail!("No charter found matching '{}'", query)
}

/// True if `actions_file` (relative to the charter root) resolves to the same
/// file as `target` (an absolute or CWD-relative path from the caller).
fn same_actions_file(charter_root: &Path, actions_file: &Path, target: &Path) -> bool {
    let candidate = charter_root.join(actions_file);
    let candidate = std::fs::canonicalize(&candidate).unwrap_or(candidate);
    let target = std::fs::canonicalize(target).unwrap_or_else(|_| target.to_path_buf());
    candidate == target
}

/// Collect actions for the `read` command via the workspace loader — the same
/// journal recovery, sidecar hydration, and load-finding warnings every other
/// command gets. `.completed.actions` files fall outside the loader's domain
/// model by design (they're a closed-action archive, not live workspace state,
/// see `discover_action_files`), so those are still read directly per matching
/// file when `open_only` is false.
fn collect_all_actions(
    ctx: &CommandContext,
    file: &Option<PathBuf>,
    open_only: bool,
) -> anyhow::Result<Vec<Action>> {
    let charter_root = clearhead_core::charter_root(&ctx.data_dir);
    let charters = clearhead_core::load_workspace(&ctx.data_dir)?;

    let matches = |mc: &clearhead_core::MarkdownCharter| match (file, &mc.actions_file) {
        (Some(target), Some(actions_file)) => {
            same_actions_file(&charter_root, actions_file, target)
        }
        (Some(_), None) => false,
        (None, _) => true,
    };

    let matching: Vec<&clearhead_core::MarkdownCharter> =
        charters.iter().filter(|mc| matches(mc)).collect();

    let mut result: Vec<Action> = matching
        .iter()
        .flat_map(|mc| mc.actions.iter().map(|sourced| sourced.action.clone()))
        .collect();

    // Occurrences are not projected into this flat listing — the present due
    // occurrence is a materialized `.actions` line already collected above.

    if open_only {
        result.retain(is_open_action);
    } else {
        for mc in &matching {
            let Some(actions_file) = &mc.actions_file else {
                continue;
            };
            let completed_path =
                action_files::completed_actions_path(&charter_root.join(actions_file));
            result.extend(action_files::read_actions(&completed_path)?);
        }
    }
    Ok(result)
}

fn is_open_action(action: &Action) -> bool {
    !matches!(
        action.state,
        ActionState::Completed | ActionState::Cancelled
    )
}

fn print_acts_table(ws_actions: &[(Option<&str>, &Action)], multi_ws: bool) {
    use comfy_table::{Cell, Table};

    let mut table = Table::new();
    let mut headers: Vec<&str> = vec!["id", "state", "name", "scheduled_at", "duration"];
    if multi_ws {
        headers.insert(0, "workspace");
    }
    table.set_header(headers);

    for (ws, action) in ws_actions {
        let short_id = &action.id.to_string()[..8];
        let state = format!("{:?}", action.state);
        let scheduled = action
            .scheduled_at
            .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
            .unwrap_or_else(|| "—".to_string());
        let duration = action
            .duration
            .map(|d| format!("{}m", d))
            .unwrap_or_else(|| "—".to_string());

        let mut row = vec![
            Cell::new(short_id),
            Cell::new(state),
            Cell::new(&action.name),
            Cell::new(scheduled),
            Cell::new(duration),
        ];
        if multi_ws {
            row.insert(0, Cell::new(ws.unwrap_or("—")));
        }
        table.add_row(row);
    }

    println!("{}", table);
}

#[cfg(test)]
mod resolution_tests {
    use super::*;
    use uuid::Uuid;

    fn make_action(name: &str, alias: Option<&str>) -> Action {
        Action {
            id: Uuid::now_v7(),
            name: name.to_string(),
            alias: alias.map(|s| s.to_string()),
            ..Default::default()
        }
    }

    #[test]
    fn alias_beats_earlier_name_contains_match() {
        // An earlier name-contains match must not shadow a later exact alias —
        // the bug that let `complete`/`update`/`cancel`/`delete` act on the
        // wrong action when a query happened to substring-match an earlier name.
        let actions = vec![
            make_action("Fix staging server", None),
            make_action("Deploy", Some("staging")),
        ];

        let found = find_best_match(&actions, "staging", |_| true)
            .unwrap()
            .unwrap();
        assert_eq!(found.name, "Deploy");
        assert_eq!(found.alias.as_deref(), Some("staging"));
    }

    #[test]
    fn short_uuid_beats_alias_and_name() {
        let mut target = make_action("Target action", None);
        target.id = Uuid::parse_str("aaaaaaaa-0000-7000-8000-000000000000").unwrap();
        let short = &target.id.to_string()[..8];
        let actions = vec![
            make_action("Alias holder", Some(short.to_owned()).as_deref()),
            target.clone(),
        ];

        let found = find_best_match(&actions, short, |_| true).unwrap().unwrap();
        assert_eq!(found.id, target.id);
    }

    #[test]
    fn full_uuid_beats_everything() {
        let target = make_action("Target action", None);
        let decoy = make_action(&target.id.to_string(), None);
        let actions = vec![decoy, target.clone()];

        let found = find_best_match(&actions, &target.id.to_string(), |_| true)
            .unwrap()
            .unwrap();
        assert_eq!(found.id, target.id);
    }

    #[test]
    fn urn_uuid_form_resolves_by_identity() {
        // The query contract exports `id` as `urn:uuid:…` — the verb must
        // accept canonical identity exactly as exported, unpeeled.
        let target = make_action("Target action", None);
        let actions = vec![make_action("Decoy", None), target.clone()];

        let query = format!("urn:uuid:{}", target.id);
        let found = find_best_match(&actions, &query, |_| true)
            .unwrap()
            .unwrap();
        assert_eq!(found.id, target.id);
    }

    #[test]
    fn uuid_query_never_degrades_to_fuzzy_match() {
        // A UUID-shaped query that matches no id must fail, not fall through
        // to name-contains — an automated loop acting on a stale id must get
        // not-found, never a write to an unrelated action.
        let ghost = Uuid::now_v7();
        let decoy = make_action(&format!("Notes about {}", ghost), None);
        let actions = vec![decoy];

        assert!(
            find_best_match(&actions, &ghost.to_string(), |_| true)
                .unwrap()
                .is_none()
        );
        assert!(
            find_best_match(&actions, &format!("urn:uuid:{}", ghost), |_| true)
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn ambiguous_short_uuid_is_an_error() {
        let mut first = make_action("First", None);
        first.id = Uuid::parse_str("dead0000-0000-7000-8000-000000000001").unwrap();
        let mut second = make_action("Second", None);
        second.id = Uuid::parse_str("deadffff-0000-7000-8000-000000000002").unwrap();

        let error = find_best_match(&[first, second], "dead", |_| true).unwrap_err();
        assert!(error.to_string().contains("Ambiguous action reference"));
    }
}
