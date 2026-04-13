//! Display layer for domain objects.
//!
//! Provides table rendering that operates directly on `DomainModel`,
//! preserving the charter hierarchy instead of losing it through ActionList conversion.

use crate::domain::{ActPhase, Charter, DomainModel, Plan};
use crate::workspace::actions::format::{
    TableFormatOptions, COLUMN_NAMES, columns_to_show, columns_without,
};
use comfy_table::{Cell, Color, ContentArrangement, Table, presets::UTF8_FULL};

/// Render a `DomainModel` as a human-readable table.
///
/// Charters appear as section header rows (`── Title ──`), with sub-charters
/// nested immediately after their parent's plans. Plan indentation comes from
/// the sub-plan tree depth via `plan_depth()`.
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

    // Collect all plans once for plan_depth() parent lookups
    let all_plans: Vec<&Plan> = model.all_plans();

    for charter in &model.charters {
        render_charter(charter, 0, &all_plans, &mut table, &columns_idx);
    }

    Ok(table.to_string())
}

/// Render one charter section: header row, its direct plans (DFS), then sub-charters.
///
/// `depth` is the charter nesting level — 0 for root charters, +1 for each sub-charter level.
fn render_charter(
    charter: &Charter,
    depth: usize,
    all_plans: &[&Plan],
    table: &mut Table,
    columns_idx: &[usize],
) {
    // Section header row: indented "── Title ──" in the Name column (index 1),
    // blank elsewhere. This avoids inflating the narrow State column with long titles.
    let indent = "  ".repeat(depth);
    let header = truncate(&format!("{}── {} ──", indent, charter.title), 40);
    // Find which rendered position corresponds to the Name column (COLUMN_NAMES index 1).
    let name_pos = columns_idx.iter().position(|&i| i == 1).unwrap_or(0);
    let header_row: Vec<Cell> = columns_idx
        .iter()
        .enumerate()
        .map(|(pos, _)| {
            if pos == name_pos {
                Cell::new(&header).fg(Color::Cyan)
            } else {
                Cell::new("")
            }
        })
        .collect();
    table.add_row(header_row);

    // Plans belonging to this charter only (DFS: parent before sub-plans)
    for plan in charter.all_plans() {
        let plan_indent = "  ".repeat(plan_depth(plan, all_plans));

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
            truncate(&format!("{}{}", plan_indent, plan.name), 40),
            charter.title.clone(),
            plan.priority
                .map(|p| p.to_string())
                .unwrap_or_else(|| "-".to_string()),
            first_act
                .and_then(|a| a.scheduled_at)
                .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
                .unwrap_or_else(|| "-".to_string()),
            first_act
                .and_then(|a| a.duration)
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

    // Sub-charters appear immediately after this charter's plans, one level deeper
    for sub in &charter.sub_charters {
        render_charter(sub, depth + 1, all_plans, table, columns_idx);
    }
}

/// Truncate a string to `max` chars, appending `…` if cut. Unicode-safe.
fn truncate(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let t: String = s.chars().take(max.saturating_sub(1)).collect();
        format!("{}…", t)
    }
}

/// Walk the parent chain to determine plan nesting depth.
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
