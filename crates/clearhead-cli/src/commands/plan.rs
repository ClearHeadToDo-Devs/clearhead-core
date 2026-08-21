use anyhow::Context;
use chrono::{DateTime, Local};
use std::fs;
use std::io::IsTerminal;
use std::path::{Path, PathBuf};
use tracing::{debug, info};

use crate::argparser;
use crate::commands::{
    CommandContext, load_file_for_read, parse_content_for_read, read_input, try_emit,
};
use clearhead_cli::telemetry::TelemetryEvent;

/// Find the index at which to insert a child action so it appears immediately
/// after the last existing descendant of `parent_id`.
///
/// Walks forward from the parent's position, collecting all actions whose
/// ancestor chain leads back to `parent_id`. Returns the index after the last
/// one, or `actions.len()` if the parent is not found.
#[cfg(test)]
fn insert_index_after_descendants(
    actions: &[clearhead_cli::Action],
    parent_id: uuid::Uuid,
) -> usize {
    let parent_idx = match actions.iter().position(|a| a.id == parent_id) {
        Some(idx) => idx,
        None => return actions.len(),
    };

    let mut descendant_ids: std::collections::HashSet<uuid::Uuid> =
        std::collections::HashSet::from([parent_id]);
    let mut last = parent_idx;

    for (offset, action) in actions[parent_idx + 1..].iter().enumerate() {
        if action
            .parent_id
            .is_some_and(|pid| descendant_ids.contains(&pid))
        {
            descendant_ids.insert(action.id);
            last = parent_idx + 1 + offset;
        }
    }

    last + 1
}

/// The canonical machine key for a charter — alias if present, otherwise title.
///
/// `charter.parent` always stores a machine key, so this is the right value to
/// use for any identity comparison or graph edge.
fn charter_key(charter: &clearhead_core::Charter) -> &str {
    charter.alias.as_deref().unwrap_or(&charter.title)
}

/// Returns the key name used in the workspace graph for a charter (owned).
fn charter_graph_name(charter: &clearhead_core::Charter) -> String {
    charter_key(charter).to_string()
}

/// All charters whose `parent` field matches `parent_key` (case-insensitive).
fn direct_children<'a>(
    charters: &'a [clearhead_core::Charter],
    parent_key: &str,
) -> Vec<&'a clearhead_core::Charter> {
    charters
        .iter()
        .filter(|c| {
            c.parent
                .as_deref()
                .map(|p| p.eq_ignore_ascii_case(parent_key))
                .unwrap_or(false)
        })
        .collect()
}

/// Collect the charter matching `root_key` plus all descendants (transitively).
///
/// Uses machine keys throughout — `charter.parent` always stores a machine key,
/// never a display title. A `visited` set guards against cyclic parent data.
fn collect_charter_tree(
    charters: &[clearhead_core::Charter],
    root_key: &str,
) -> Vec<clearhead_core::Charter> {
    let mut result = Vec::new();
    let mut queue = vec![root_key.to_string()];
    let mut visited = std::collections::HashSet::new();

    while let Some(current) = queue.pop() {
        if !visited.insert(current.clone()) {
            continue;
        }
        if let Some(node) = charters
            .iter()
            .find(|c| charter_key(c).eq_ignore_ascii_case(&current))
        {
            result.push(node.clone());
        }
        for child in direct_children(charters, &current) {
            queue.push(charter_key(child).to_string());
        }
    }
    result
}

pub fn read_plans(
    ctx: &CommandContext,
    format: &Option<argparser::OutputMode>,
    charter: &Option<String>,
    recursive: bool,
    file: &Option<std::path::PathBuf>,
    _stdio: bool,
    _table_options: &argparser::CliTableOptions,
) -> anyhow::Result<()> {
    use clearhead_core::workspace::calendar::ics::parse_ics_file;

    let plans: Vec<(String, clearhead_core::Plan)> = if let Some(path) = file {
        let charter_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();
        parse_ics_file(path)?
            .into_iter()
            .map(|ip| (charter_name.clone(), ip.plan))
            .collect()
    } else {
        let entries = ctx.collect_plan_files()?;

        let allowed: Option<std::collections::HashSet<String>> = if let Some(query) = charter {
            let model = ctx.load_model()?;
            let found = crate::commands::charter::resolve_charter(&model.charters, query)?
                .ok_or_else(|| anyhow::anyhow!("No charter found matching '{}'", query))?;
            let key = charter_graph_name(found);
            let names = if recursive {
                collect_charter_tree(&model.charters, &key)
                    .iter()
                    .map(|c| charter_key(c).to_lowercase())
                    .collect()
            } else {
                std::iter::once(key.to_lowercase()).collect()
            };
            Some(names)
        } else {
            None
        };

        let mut result = Vec::new();
        for entry in entries {
            if let Some(ref allowed) = allowed
                && !allowed.contains(&entry.charter_name.to_lowercase())
            {
                continue;
            }
            match parse_ics_file(&entry.path) {
                Ok(ps) => result.extend(
                    ps.into_iter()
                        .map(|ip| (entry.charter_name.clone(), ip.plan)),
                ),
                Err(e) => eprintln!("Warning: skipping {}: {}", entry.path.display(), e),
            }
        }
        result
    };

    if plans.is_empty() {
        if let Some(query) = charter {
            println!("No plans found for charter '{}'.", query);
        } else {
            println!("No ICS plan files found in workspace.");
        }
        return Ok(());
    }

    match format {
        Some(argparser::OutputMode::JsonLd) => {
            let model = model_containing_plans(ctx, &plans)?;
            let jsonld = clearhead_cli::serialize_domain_to_jsonld(&model)
                .map_err(|e| anyhow::anyhow!("Failed to serialize JSON-LD: {e}"))?;
            println!("{}", jsonld);
        }
        Some(argparser::OutputMode::Json) => {
            // Plans have no canonical actions-schema shape yet; emit plain
            // structured JSON of the domain plans until a plan schema exists.
            let plans: Vec<_> = plans.iter().map(|(_, plan)| plan).collect();
            println!("{}", serde_json::to_string_pretty(&plans)?);
        }
        Some(argparser::OutputMode::Ids) => {
            for (_, plan) in &plans {
                println!("{}", plan.id);
            }
        }
        Some(argparser::OutputMode::Table) => print_plans_table(&plans),
        None if !std::io::stdout().is_terminal() => {
            let plans: Vec<_> = plans.into_iter().map(|(_, plan)| plan).collect();
            print!("{}", clearhead_core::plans_to_icalendar(&plans));
        }
        None => print_plans_table(&plans),
    }
    Ok(())
}

fn print_plans_table(plans: &[(String, clearhead_core::Plan)]) {
    use comfy_table::{Cell, Table};

    let mut table = Table::new();
    table.set_header(vec!["name", "charter", "dtstart", "recurrence"]);
    for (charter_name, plan) in plans {
        let dtstart = plan
            .dtstart
            .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
            .unwrap_or_else(|| "—".to_string());
        let recurrence = plan
            .recurrence
            .as_ref()
            .map(|r| r.frequency.to_lowercase())
            .unwrap_or_else(|| "—".to_string());
        table.add_row(vec![
            Cell::new(&plan.name),
            Cell::new(charter_name),
            Cell::new(&dtstart),
            Cell::new(&recurrence),
        ]);
    }
    println!("{}", table);
}

/// Build the smallest truthful graph model containing the selected plans.
/// Workspace-backed plans retain their real charter identities; `--file` plans
/// that are not in the workspace receive a deterministic synthetic charter.
fn model_containing_plans(
    ctx: &CommandContext,
    plans: &[(String, clearhead_core::Plan)],
) -> anyhow::Result<clearhead_core::DomainModel> {
    use std::collections::{BTreeMap, HashSet};

    let selected: HashSet<_> = plans.iter().map(|(_, plan)| plan.id).collect();
    let mut model = ctx.load_model()?;
    model.objectives.clear();
    for charter in &mut model.charters {
        charter.actions.clear();
        charter.plans.retain(|plan| selected.contains(&plan.id));
    }
    model.charters.retain(|charter| !charter.plans.is_empty());

    let represented: HashSet<_> = model
        .charters
        .iter()
        .flat_map(|charter| charter.plans.iter().map(|plan| plan.id))
        .collect();
    let mut unmatched: BTreeMap<String, Vec<clearhead_core::Plan>> = BTreeMap::new();
    for (charter_name, plan) in plans {
        if !represented.contains(&plan.id) {
            unmatched
                .entry(charter_name.clone())
                .or_default()
                .push(plan.clone());
        }
    }
    for (charter_name, plans) in unmatched {
        model.charters.push(clearhead_core::Charter {
            id: uuid::Uuid::new_v5(&uuid::Uuid::NAMESPACE_URL, charter_name.as_bytes()),
            title: charter_name.clone(),
            alias: Some(charter_name),
            plans,
            ..Default::default()
        });
    }
    Ok(model)
}

pub fn show_plan(
    ctx: &CommandContext,
    query: &str,
    file: &Option<std::path::PathBuf>,
    _format: &Option<argparser::OutputMode>,
    _table_options: &argparser::CliTableOptions,
) -> anyhow::Result<()> {
    use clearhead_core::workspace::calendar::ics::parse_ics_file;

    debug!(query = %query, "Executing Show Plan");

    let candidates: Vec<(String, clearhead_core::Plan)> = if let Some(path) = file {
        let charter_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();
        parse_ics_file(path)?
            .into_iter()
            .map(|ip| (charter_name.clone(), ip.plan))
            .collect()
    } else {
        let entries = ctx.collect_plan_files()?;
        let mut result = Vec::new();
        for entry in entries {
            match parse_ics_file(&entry.path) {
                Ok(ps) => result.extend(
                    ps.into_iter()
                        .map(|ip| (entry.charter_name.clone(), ip.plan)),
                ),
                Err(e) => eprintln!("Warning: skipping {}: {}", entry.path.display(), e),
            }
        }
        result
    };

    let plan_refs: Vec<&clearhead_core::Plan> = candidates.iter().map(|(_, plan)| plan).collect();
    let index = find_plan_match_pos(&plan_refs, query)?
        .ok_or_else(|| anyhow::anyhow!("No plan found matching '{}'", query))?;
    let (charter_name, plan) = candidates
        .into_iter()
        .nth(index)
        .expect("selected Plan index comes from candidates");

    println!(
        "{}",
        crate::display::render_plan_detail(&plan, &charter_name)
    );
    Ok(())
}

fn resolve_plans_dir(
    ctx: &CommandContext,
    file: &Option<PathBuf>,
    charter: &Option<String>,
) -> anyhow::Result<PathBuf> {
    if let Some(path) = file {
        return Ok(path.clone());
    }

    let plans_root = ctx.plans_root();
    let charter_root = clearhead_core::charter_root(&ctx.data_dir);

    let charters = ctx.load_charters()?;
    if let Some(query) = charter {
        let charter = resolve_markdown_charter(&charters, query)?
            .ok_or_else(|| anyhow::anyhow!("No charter found matching '{query}'"))?;
        return Ok(plans_root.join(&charter.plans_dir));
    }

    let default_actions = ctx.resolve_action_file(None);
    let relative = default_actions
        .strip_prefix(&charter_root)
        .unwrap_or(default_actions.as_path());
    let charter = charters
        .iter()
        .find(|charter| charter.actions_file.as_deref() == Some(relative))
        .ok_or_else(|| {
            anyhow::anyhow!(
                "No charter owns default action file '{}'; initialize or create the charter first",
                default_actions.display()
            )
        })?;
    Ok(plans_root.join(&charter.plans_dir))
}

fn resolve_add_plan_output_path(
    ctx: &CommandContext,
    file: &Option<PathBuf>,
    charter: &Option<String>,
    plan: &clearhead_core::Plan,
) -> anyhow::Result<PathBuf> {
    if let Some(path) = file {
        if path.extension().and_then(|ext| ext.to_str()) != Some("ics") {
            anyhow::bail!(
                "Explicit plan output path must end with '.ics': {}",
                path.display()
            );
        }
        return Ok(path.clone());
    }

    if let Some(query) = charter {
        let charters = ctx.load_charters()?;
        let charter = resolve_markdown_charter(&charters, query)?
            .ok_or_else(|| anyhow::anyhow!("No charter found matching '{query}'"))?;
        return Ok(clearhead_core::plan_output_path(
            &ctx.plans_root(),
            charter,
            plan,
        ));
    }

    let plans_dir = resolve_plans_dir(ctx, &None, charter)?;
    Ok(plans_dir.join(clearhead_core::plan_file_name(plan)))
}

fn load_plan_file(path: &Path) -> anyhow::Result<Vec<clearhead_core::Plan>> {
    if path.exists() {
        Ok(
            clearhead_core::workspace::calendar::ics::parse_ics_file(path)
                .map(|plans| plans.into_iter().map(|ip| ip.plan).collect())?,
        )
    } else {
        Ok(Vec::new())
    }
}

fn charter_stem_from_source(source: &Path) -> anyhow::Result<String> {
    let stem = source
        .file_stem()
        .and_then(|value| value.to_str())
        .ok_or_else(|| anyhow::anyhow!("Cannot derive charter name from '{}'", source.display()))?;
    Ok(clearhead_core::slugify(stem))
}

fn save_plan_file(path: &Path, plan: &clearhead_core::Plan) -> anyhow::Result<()> {
    clearhead_core::workspace::durability::atomic_write(
        path,
        clearhead_core::plans_to_icalendar(std::slice::from_ref(plan)),
    )
    .with_context(|| format!("Failed to write plan file '{}'", path.display()))
}

fn parse_local_datetime(value: Option<&str>) -> anyhow::Result<Option<DateTime<Local>>> {
    value
        .map(|value| {
            DateTime::parse_from_rfc3339(value)
                .map(|dt| dt.with_timezone(&Local))
                .map_err(|_| anyhow::anyhow!(
                    "Invalid --scheduled-at '{}': expected ISO 8601 with timezone (e.g. 2026-05-17T18:00:00Z or 2026-05-17T11:00:00-07:00)",
                    value
                ))
        })
        .transpose()
}

fn parse_rrule(value: Option<&str>) -> anyhow::Result<Option<clearhead_core::Recurrence>> {
    value
        .map(|value| {
            clearhead_core::Recurrence::from_rrule_str(value).ok_or_else(|| {
                anyhow::anyhow!("Invalid --rrule '{}': expected RFC5545 RRULE fields", value)
            })
        })
        .transpose()
}

fn reject_act_only_plan_fields(fields: &argparser::PlanFields) -> anyhow::Result<()> {
    if fields.state.is_some() {
        anyhow::bail!(
            "Plan state is stored on actions; use `update action --state` to edit action state"
        );
    }
    Ok(())
}

fn find_plan_for_mutation(
    ctx: &CommandContext,
    file: &Option<PathBuf>,
    query: &str,
) -> anyhow::Result<(PathBuf, clearhead_core::Plan)> {
    let files = if let Some(path) = file {
        vec![path.clone()]
    } else {
        ctx.collect_plan_files()?
            .into_iter()
            .map(|entry| entry.path)
            .collect()
    };

    let mut candidates = Vec::new();
    for path in files {
        for plan in load_plan_file(&path)? {
            candidates.push((path.clone(), plan));
        }
    }
    let plan_refs: Vec<&clearhead_core::Plan> = candidates.iter().map(|(_, plan)| plan).collect();
    let index = find_plan_match_pos(&plan_refs, query)?
        .ok_or_else(|| anyhow::anyhow!("No plan found matching '{}'", query))?;
    Ok(candidates
        .into_iter()
        .nth(index)
        .expect("selected Plan index comes from candidates"))
}

fn find_plan_match_pos<T>(plans: &[T], query: &str) -> anyhow::Result<Option<usize>>
where
    T: std::borrow::Borrow<clearhead_core::Plan> + clearhead_core::ReferenceEntity,
{
    match clearhead_core::select_reference(plans, query) {
        clearhead_core::ReferenceSelection::Unique { index, .. } => Ok(Some(index)),
        clearhead_core::ReferenceSelection::Ambiguous { indices, .. } => {
            let candidates = indices
                .into_iter()
                .map(|index| plans[index].borrow().id.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            anyhow::bail!(
                "Ambiguous Plan reference '{}'; candidates: {}",
                query,
                candidates
            )
        }
        clearhead_core::ReferenceSelection::NotFound if uuid::Uuid::parse_str(query).is_ok() => {
            Ok(None)
        }
        clearhead_core::ReferenceSelection::NotFound => {
            let query_lower = query.to_lowercase();
            Ok(plans.iter().position(|candidate| {
                let plan = candidate.borrow();
                plan.external_id.as_deref().is_some_and(|uid| {
                    uid.eq_ignore_ascii_case(query) || uid.to_lowercase().contains(&query_lower)
                }) || plan.name.to_lowercase().contains(&query_lower)
            }))
        }
    }
}

fn resolve_markdown_charter<'a>(
    charters: &'a [clearhead_core::MarkdownCharter],
    query: &str,
) -> anyhow::Result<Option<&'a clearhead_core::MarkdownCharter>> {
    super::action::resolve_markdown_charter(charters, query)
}

// CLI adapter: the grouped clap field structs are intentionally passed
// explicitly before being assembled into one Plan.
#[allow(clippy::too_many_arguments)]
pub fn add_plan(
    ctx: &CommandContext,
    name: &str,
    file: &Option<PathBuf>,
    charter: &Option<String>,
    parent: &Option<String>,
    fields: &argparser::PlanFields,
    schedule: &argparser::PlanScheduleFields,
    dry_run: bool,
) -> anyhow::Result<()> {
    reject_act_only_plan_fields(fields)?;
    if parent.is_some() {
        anyhow::bail!("Plan hierarchy in ICS files is not implemented yet");
    }

    let rrule = parse_rrule(schedule.rrule.as_deref())?;
    if rrule.is_none() {
        anyhow::bail!(
            "Plans are for recurring work and require a recurrence rule (--rrule). For one-off scheduled tasks, use `add action --scheduled-at` instead."
        );
    }

    let uid = uuid::Uuid::now_v7().to_string();
    let new_id = clearhead_core::workspace::calendar::ics::plan_id_from_ics_uid(&uid);
    let new_plan = clearhead_core::Plan {
        id: new_id,
        name: name.to_string(),
        description: fields.description.clone(),
        recurrence: rrule,
        dtstart: parse_local_datetime(schedule.scheduled_at.as_deref())?,
        external_id: Some(uid.clone()),
        template_name: schedule.template.clone(),
        ..Default::default()
    };

    let output_file = resolve_add_plan_output_path(ctx, file, charter, &new_plan)?;
    debug!(name = %name, output_file = %output_file.display(), dry_run = dry_run, "Executing Add Plan");

    if dry_run {
        println!("{}", clearhead_core::plans_to_icalendar(&[new_plan]));
    } else {
        save_plan_file(&output_file, &new_plan)?;

        try_emit(
            &new_id,
            TelemetryEvent::PlanCreated {
                name: name.to_string(),
                file_path: output_file.display().to_string(),
            },
        );

        let short_uid = &uid[..8];
        info!(name = %name, uid = %uid, id = %new_id, "Plan added successfully");
        println!("Added plan {} ({})", short_uid, name);
    }
    Ok(())
}

pub fn update_plan(
    ctx: &CommandContext,
    query: &str,
    file: &Option<PathBuf>,
    name: &Option<String>,
    fields: &argparser::PlanFields,
    schedule: &argparser::PlanScheduleFields,
    dry_run: bool,
) -> anyhow::Result<()> {
    reject_act_only_plan_fields(fields)?;

    let (input_file, mut plan) = find_plan_for_mutation(ctx, file, query)?;
    debug!(query = %query, input_file = %input_file.display(), dry_run = dry_run, "Executing Update Plan");

    if let Some(name) = name {
        plan.name = name.clone();
    }
    if let Some(description) = &fields.description {
        plan.description = Some(description.clone());
    }
    if schedule.scheduled_at.is_some() {
        plan.dtstart = parse_local_datetime(schedule.scheduled_at.as_deref())?;
    }
    if schedule.rrule.is_some() {
        plan.recurrence = parse_rrule(schedule.rrule.as_deref())?;
    }
    if let Some(template) = &schedule.template {
        plan.template_name = Some(template.clone());
    }

    let updated = plan.clone();

    if dry_run {
        println!("{}", clearhead_core::plans_to_icalendar(&[updated]));
    } else {
        save_plan_file(&input_file, &updated)?;
        info!(name = %updated.name, id = %updated.id, "Plan updated successfully");
    }
    Ok(())
}

pub fn complete_plan(
    ctx: &CommandContext,
    query: &str,
    file: &Option<std::path::PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let _ = (ctx, query, file, dry_run);
    anyhow::bail!(
        "Plans are schedules and do not have completion state; use `complete action` for actions"
    )
}

pub fn delete_plan(
    ctx: &CommandContext,
    query: &str,
    file: &Option<PathBuf>,
    dry_run: bool,
) -> anyhow::Result<()> {
    let (input_file, plan) = find_plan_for_mutation(ctx, file, query)?;
    debug!(query = %query, input_file = %input_file.display(), dry_run = dry_run, "Executing Delete Plan");

    if dry_run {
        println!("{}", clearhead_core::plans_to_icalendar(&[plan]));
    } else {
        fs::remove_file(&input_file)
            .with_context(|| format!("Failed to delete plan file '{}'", input_file.display()))?;

        try_emit(
            &plan.id,
            TelemetryEvent::PlanDeleted {
                name: plan.name.clone(),
            },
        );

        info!(name = %plan.name, id = %plan.id, "Plan deleted successfully");
    }
    Ok(())
}

pub fn archive_plans(
    _ctx: &CommandContext,
    _scope: &Option<String>,
    _file: &Option<PathBuf>,
    _dry_run: bool,
) -> anyhow::Result<()> {
    anyhow::bail!(
        "Plans are externally-owned schedules and are not archived; use `delete plan` to remove a schedule and manage its actions explicitly"
    )
}

pub fn import_plans(
    ctx: &CommandContext,
    source: &Path,
    charter: &Option<String>,
    overwrite: bool,
    dry_run: bool,
) -> anyhow::Result<()> {
    let plans = load_plan_file(source)?;
    if plans.is_empty() {
        println!("No iCalendar schedules found in {}", source.display());
        return Ok(());
    }

    let target_charter = if let Some(charter) = charter {
        charter.clone()
    } else {
        charter_stem_from_source(source)?
    };

    let plans_dir = resolve_plans_dir(ctx, &None, &Some(target_charter.clone()))?;
    let mut imported = 0usize;
    let mut overwritten = 0usize;

    for plan in plans {
        let target_path = plans_dir.join(clearhead_core::plan_file_name(&plan));
        if target_path.exists() && !overwrite {
            anyhow::bail!(
                "Import would overwrite existing plan file '{}'; re-run with --overwrite",
                target_path.display()
            );
        }

        if dry_run {
            println!("Would import '{}' to {}", plan.name, target_path.display());
        } else {
            if target_path.exists() {
                overwritten += 1;
            }
            save_plan_file(&target_path, &plan)?;
        }
        imported += 1;
    }

    if dry_run {
        println!(
            "Would import {} plan(s) into charter '{}'",
            imported, target_charter
        );
    } else {
        info!(count = imported, overwritten = overwritten, charter = %target_charter, source = %source.display(), "Plans imported");
        println!(
            "Imported {} plan(s) into charter '{}' ({} overwritten)",
            imported, target_charter, overwritten
        );
    }

    Ok(())
}

pub fn export_plans(
    ctx: &CommandContext,
    reference: &Option<String>,
    output: &Option<std::path::PathBuf>,
    open_only: bool,
    recursive: bool,
) -> anyhow::Result<()> {
    use crate::environment_reader::resolve_file_path;
    use clearhead_core::reference::{
        ReferenceOptions, ReferenceTarget, filter_model_for_action, filter_model_for_charter,
        filter_model_for_plan, resolve_reference,
    };

    debug!(reference = ?reference, output = ?output, open_only = open_only, recursive = recursive, "Executing Export Plans");

    let model = if let Some(reference) = reference {
        if reference == "-" {
            let content = read_input(None)?;
            let actions = parse_content_for_read(&content, "stdin", "export plans")?;
            let charter = clearhead_core::workspace::actions::convert::from_actions_with_charter(
                &actions,
                "stdin".to_string(),
            );
            clearhead_core::DomainModel {
                objectives: vec![],
                charters: vec![charter],
            }
        } else if reference.ends_with(".actions") {
            let resolved = resolve_file_path(reference, &ctx.data_dir);
            let actions = load_file_for_read(&resolved, "export plans")?;
            let relative = resolved.strip_prefix(&ctx.data_dir).unwrap_or(&resolved);
            let charter_name = clearhead_core::infer_charter_name(relative)
                .unwrap_or_else(|| "unknown".to_string());
            let charter = clearhead_core::workspace::actions::convert::from_actions_with_charter(
                &actions,
                charter_name,
            );

            clearhead_core::DomainModel {
                objectives: vec![],
                charters: vec![charter],
            }
        } else {
            let model = ctx.load_model()?;
            let target = resolve_reference(&model, reference, &ReferenceOptions::default())?;
            match target {
                ReferenceTarget::Charter(id) => filter_model_for_charter(&model, id, recursive),
                ReferenceTarget::Plan(id) => filter_model_for_plan(&model, id),
                ReferenceTarget::Action(id) => filter_model_for_action(&model, id),
            }
        }
    } else {
        ctx.load_model()?
    };

    let icalendar =
        clearhead_cli::format_as_icalendar(&model, open_only).map_err(|e| anyhow::anyhow!(e))?;

    if let Some(output_path) = output {
        info!(output_path = %output_path.display(), "Writing iCalendar export to file");
        fs::write(output_path, icalendar).context("Failed to write to file")?;
    } else {
        println!("{}", icalendar);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use clearhead_cli::Action;
    use uuid::Uuid;

    fn action(id: Uuid, parent_id: Option<Uuid>) -> Action {
        Action {
            id,
            parent_id,
            name: id.to_string(),
            ..Default::default()
        }
    }

    #[test]
    fn insert_after_last_descendant_with_no_children() {
        let parent = Uuid::new_v4();
        let sibling = Uuid::new_v4();
        // [parent, sibling] — sibling is not a descendant of parent
        let actions = vec![action(parent, None), action(sibling, None)];
        // child should go at index 1 (immediately after parent, before sibling)
        assert_eq!(insert_index_after_descendants(&actions, parent), 1);
    }

    #[test]
    fn insert_after_last_descendant_skips_existing_children() {
        let parent = Uuid::new_v4();
        let child = Uuid::new_v4();
        let sibling = Uuid::new_v4();
        // [parent, child, sibling]
        let actions = vec![
            action(parent, None),
            action(child, Some(parent)),
            action(sibling, None),
        ];
        // new child should go at index 2 (after existing child, before sibling)
        assert_eq!(insert_index_after_descendants(&actions, parent), 2);
    }

    #[test]
    fn insert_after_last_descendant_handles_grandchildren() {
        let parent = Uuid::new_v4();
        let child = Uuid::new_v4();
        let grandchild = Uuid::new_v4();
        let sibling = Uuid::new_v4();
        // [parent, child, grandchild, sibling]
        let actions = vec![
            action(parent, None),
            action(child, Some(parent)),
            action(grandchild, Some(child)),
            action(sibling, None),
        ];
        // new child should go at index 3 (after grandchild, before sibling)
        assert_eq!(insert_index_after_descendants(&actions, parent), 3);
    }

    #[test]
    fn insert_after_last_descendant_unknown_parent_appends() {
        let unknown = Uuid::new_v4();
        let actions = vec![action(Uuid::new_v4(), None)];
        assert_eq!(
            insert_index_after_descendants(&actions, unknown),
            actions.len()
        );
    }
}
