//! Display layer for domain objects.
//!
//! Provides table rendering that operates directly on `DomainModel`,
//! preserving the charter hierarchy instead of losing it through ActionList conversion.

use crate::domain::{ActPhase, DomainModel, Plan};
use crate::workspace::actions::format::{
    TableFormatOptions, COLUMN_NAMES, columns_to_show, columns_without,
};
use comfy_table::{Cell, Color, ContentArrangement, Table, presets::UTF8_FULL};

/// Render a `DomainModel` as a human-readable table.
///
/// Charter titles come directly from the `Charter` struct — no field enrichment needed.
/// State and dates are read from the first `PlannedAct` on each plan, or synthesized
/// as `NotStarted` if the plan has no acts.
pub fn format_domain_as_table(
    model: &DomainModel,
    filters: Option<&TableFormatOptions>,
) -> Result<String, String> {
    // Handle --list-columns flag
    if let Some(opts) = filters
        && opts.list_columns
    {
        println!("Available columns for --columns and --hide-columns:");
        println!("  state       - Action state (Not Started, In Progress, Done, etc.)");
        println!("  name        - Action name with indentation");
        println!("  charter     - Charter/project this plan belongs to");
        println!("  priority    - Numerical priority (1-9, higher = more important)");
        println!("  due         - Due date/time");
        println!("  dur         - Estimated duration in minutes");
        println!("  recurrence  - Recurrence pattern (DAILY, WEEKLY, etc.)");
        println!("  context     - Context tags");
        println!("  description - Task description");
        println!("  id          - First 8 chars of UUID");
        return Ok(String::new());
    }

    // Build column index list based on filters (exclude last "Story" alias column by default)
    let all_idx: Vec<usize> = (0..COLUMN_NAMES.len() - 1).collect();
    let columns_idx: Vec<usize> = if let Some(opts) = filters {
        match (&opts.columns, &opts.hide_columns) {
            (Some(cols), None) => columns_to_show(cols),
            (None, Some(hide)) => columns_without(hide, &all_idx),
            (Some(cols), Some(hide)) => {
                let shown = columns_to_show(cols);
                columns_without(hide, &shown)
            }
            (None, None) => all_idx,
        }
    } else {
        all_idx
    };

    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .set_content_arrangement(ContentArrangement::Dynamic);

    table.set_header(
        columns_idx
            .iter()
            .map(|&i| Cell::new(COLUMN_NAMES[i]).fg(Color::Cyan)),
    );

    // Collect all plans for depth calculation
    let all_plans: Vec<&Plan> = model.all_plans();

    for charter in &model.charters {
        for plan in &charter.plans {
            // Depth: walk the parent chain through all plans in the model
            let depth = plan_depth(plan, &all_plans);
            let indent = "  ".repeat(depth);

            // State from first act, or synthesized NotStarted
            let phase = plan
                .acts
                .first()
                .map(|a| a.phase)
                .unwrap_or(ActPhase::NotStarted);

            let state_cell = match phase {
                ActPhase::NotStarted => Cell::new("Not Started"),
                ActPhase::Completed => Cell::new("Done").fg(Color::Green),
                ActPhase::InProgress => Cell::new("In Progress").fg(Color::Yellow),
                ActPhase::Blocked => Cell::new("Blocked").fg(Color::Red),
                ActPhase::Cancelled => Cell::new("Cancelled").fg(Color::DarkGrey),
            };

            let first_act = plan.acts.first();

            let all_strings: Vec<String> = vec![
                String::new(), // placeholder — state rendered as colored Cell
                format!("{}{}", indent, plan.name),
                charter.title.clone(),
                plan.priority
                    .map(|p| p.to_string())
                    .unwrap_or_else(|| "-".to_string()),
                first_act
                    .and_then(|a| a.scheduled_at)
                    .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
                    .unwrap_or_else(|| "-".to_string()),
                first_act
                    .and_then(|a| a.duration.or(plan.duration))
                    .map(|d| format!("{}m", d))
                    .unwrap_or_else(|| "-".to_string()),
                plan.recurrence
                    .as_ref()
                    .map(|r| r.frequency.to_uppercase())
                    .unwrap_or_else(|| "-".to_string()),
                plan.contexts
                    .as_ref()
                    .map(|c| c.join(", "))
                    .unwrap_or_else(|| "-".to_string()),
                plan.description
                    .as_ref()
                    .map(|d| d.to_string())
                    .unwrap_or_else(|| "-".to_string()),
                plan.id.to_string()[..8].to_string(),
                charter.title.clone(), // "Story" alias — same as Charter
            ];

            let row: Vec<Cell> = columns_idx
                .iter()
                .map(|&i| {
                    if i == 0 {
                        state_cell.clone()
                    } else {
                        Cell::new(&all_strings[i])
                    }
                })
                .collect();

            table.add_row(row);
        }
    }

    Ok(table.to_string())
}

/// Walk the parent chain to determine nesting depth.
fn plan_depth(plan: &Plan, all_plans: &[&Plan]) -> usize {
    let mut depth = 0;
    let mut current_parent = plan.parent;
    while let Some(parent_id) = current_parent {
        depth += 1;
        current_parent = all_plans
            .iter()
            .find(|p| p.id == parent_id)
            .and_then(|p| p.parent);
        if depth > 100 {
            break; // cycle guard
        }
    }
    depth
}
