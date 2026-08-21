//! Key-value detail renderer for single-item `show` output.
//!
//! Each `render_*_detail` function returns a formatted `String` suitable for
//! TTY output. Callers are responsible for TTY detection; when piped, emit
//! JSON-LD instead.

use clearhead_core::domain::{Action, ActionState, Charter, Plan};

use super::action_state_str;

// ─────────────────────────────────────────────────────────────────────────────
// Public render functions
// ─────────────────────────────────────────────────────────────────────────────

pub fn render_action_detail(action: &Action) -> String {
    let mut rows: Vec<(&str, String)> = vec![
        ("id", action.id.to_string()),
        ("state", action_state_str(action.state).to_string()),
    ];

    opt(&mut rows, "alias", action.alias.as_deref());
    opt(&mut rows, "charter", action.charter.as_deref());
    opt_uuid(&mut rows, "parent", action.parent_id);
    opt_uuid(&mut rows, "plan", action.plan_id);
    opt_dt(&mut rows, "scheduled", action.scheduled_at);
    opt_dt(&mut rows, "due", action.due_date);
    if let Some(dur) = action.duration {
        rows.push(("duration", format!("{}m", dur)));
    }
    if let Some(p) = action.priority {
        rows.push(("priority", p.to_string()));
    }
    if let Some(ctxs) = &action.contexts
        && !ctxs.is_empty()
    {
        rows.push(("contexts", ctxs.join(", ")));
    }
    if action.is_sequential == Some(true) {
        rows.push(("sequential", "yes".to_string()));
    }
    opt_dt(&mut rows, "created", action.created_at);
    opt_dt(&mut rows, "completed", action.completed_at);

    let mut out = title_block(&action.name);
    out.push('\n');
    out.push_str(&kv_block(&rows));

    if let Some(desc) = &action.description {
        out.push('\n');
        out.push_str(&indent_block("description", desc));
    }

    out
}

pub fn render_plan_detail(plan: &Plan, charter_name: &str) -> String {
    let mut rows: Vec<(&str, String)> = vec![
        ("charter", charter_name.to_string()),
        ("id", plan.id.to_string()),
    ];

    opt_dt(&mut rows, "dtstart", plan.dtstart);
    if let Some(r) = &plan.recurrence {
        rows.push(("recurrence", r.frequency.to_lowercase()));
    }
    opt(&mut rows, "template", plan.template_name.as_deref());
    opt(&mut rows, "uid", plan.external_id.as_deref());

    let mut out = title_block(&plan.name);
    out.push('\n');
    out.push_str(&kv_block(&rows));

    if let Some(desc) = &plan.description {
        out.push('\n');
        out.push_str(&indent_block("description", desc));
    }

    out
}

pub fn render_charter_detail(charter: &Charter) -> String {
    let mut rows: Vec<(&str, String)> = vec![("id", charter.id.to_string())];

    opt(&mut rows, "alias", charter.alias.as_deref());
    opt(&mut rows, "parent", charter.parent.as_deref());
    if let Some(state) = charter.state {
        rows.push(("state", state.to_string().to_lowercase()));
    }
    if let Some(objs) = &charter.objectives
        && !objs.is_empty()
    {
        rows.push(("objectives", objs.join(", ")));
    }

    let mut out = title_block(&charter.title);
    out.push('\n');
    out.push_str(&kv_block(&rows));

    if let Some(desc) = &charter.description {
        out.push('\n');
        out.push_str(&indent_block("description", desc));
    }

    let open = charter
        .actions
        .iter()
        .filter(|a| !matches!(a.state, ActionState::Completed | ActionState::Cancelled))
        .count();
    let plans = charter.plans.len();

    if plans > 0 || open > 0 {
        out.push('\n');
        let mut parts = Vec::new();
        if plans > 0 {
            parts.push(format!(
                "{} plan{}",
                plans,
                if plans == 1 { "" } else { "s" }
            ));
        }
        if open > 0 {
            parts.push(format!(
                "{} open action{}",
                open,
                if open == 1 { "" } else { "s" }
            ));
        }
        out.push_str(&parts.join(", "));
        out.push('\n');
    }

    out
}

// ─────────────────────────────────────────────────────────────────────────────
// Shared formatting helpers
// ─────────────────────────────────────────────────────────────────────────────

fn title_block(title: &str) -> String {
    format!("{}\n{}", title, "─".repeat(title.chars().count()))
}

fn kv_block(rows: &[(&str, String)]) -> String {
    let width = rows.iter().map(|(k, _)| k.len()).max().unwrap_or(0);
    rows.iter()
        .map(|(k, v)| format!("{:<width$}  {}", k, v, width = width))
        .collect::<Vec<_>>()
        .join("\n")
}

fn indent_block(label: &str, text: &str) -> String {
    let indented = text
        .trim_end()
        .lines()
        .map(|l| format!("  {}", l))
        .collect::<Vec<_>>()
        .join("\n");
    format!("{}:\n{}", label, indented)
}

// ─────────────────────────────────────────────────────────────────────────────
// Row-building helpers — only push when value is present
// ─────────────────────────────────────────────────────────────────────────────

fn opt<'a>(rows: &mut Vec<(&'a str, String)>, label: &'a str, val: Option<&str>) {
    if let Some(v) = val {
        rows.push((label, v.to_string()));
    }
}

fn opt_uuid(rows: &mut Vec<(&str, String)>, label: &'static str, val: Option<uuid::Uuid>) {
    if let Some(v) = val {
        rows.push((label, v.to_string()));
    }
}

fn opt_dt(
    rows: &mut Vec<(&str, String)>,
    label: &'static str,
    val: Option<chrono::DateTime<chrono::Local>>,
) {
    if let Some(dt) = val {
        rows.push((label, dt.format("%Y-%m-%d %H:%M").to_string()));
    }
}
