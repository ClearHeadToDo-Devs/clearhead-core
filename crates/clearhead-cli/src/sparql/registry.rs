//! The saved-query registry: built-in queries plus user/project drop-ins.
//!
//! This is graphd's registry, moved in-process. Resolution is pure text —
//! find the `.sparql` document for a name — so it carries no oxigraph
//! dependency; execution (and the parameter binding it needs) lives in the
//! parent [`super`] module.
//!
//! Three tiers, most-local wins (graphd's precedence):
//!
//! 1. **project** drop-ins — `<workspace>/.clearhead/queries/`
//! 2. **user** drop-ins — `<config>/queries/`
//! 3. **built-in** — the queries embedded below, shipped with the CLI
//!
//! The flat namespace (`query named <name>`) and the family namespaces
//! (`index`, `tree`, `graph`) are separate: a family's drop-ins live in a
//! same-named subdirectory (`.clearhead/queries/index/…`), matching graphd.

use std::path::PathBuf;

use crate::commands::CommandContext;

/// Where a resolved query came from — surfaced by `query list`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Source {
    BuiltIn,
    User,
    Project,
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Source::BuiltIn => "built-in",
            Source::User => "user",
            Source::Project => "project",
        })
    }
}

/// Unrestricted built-ins — the flat `query named <name>` namespace.
pub const BUILT_IN: &[(&str, &str)] = &[
    (
        "actions-by-phase",
        include_str!("../queries/actions-by-phase.sparql"),
    ),
    ("all-plans", include_str!("../queries/all-plans.sparql")),
    (
        "all-plans-simple",
        include_str!("../queries/all-plans-simple.sparql"),
    ),
    (
        "completion-velocity",
        include_str!("../queries/completion-velocity.sparql"),
    ),
    (
        "dependency-chain",
        include_str!("../queries/dependency-chain.sparql"),
    ),
    (
        "high-priority",
        include_str!("../queries/high-priority.sparql"),
    ),
    (
        "next-actions",
        include_str!("../queries/next-actions.sparql"),
    ),
    (
        "orphaned-actions",
        include_str!("../queries/orphaned-actions.sparql"),
    ),
    (
        "overdue-tasks",
        include_str!("../queries/overdue-tasks.sparql"),
    ),
    ("open-plans", include_str!("../queries/open-plans.sparql")),
    (
        "plans-with-contexts",
        include_str!("../queries/plans-with-contexts.sparql"),
    ),
];

/// Built-in `index` views (client-presentation families; migrated in slice 2).
pub const BUILT_IN_INDEX: &[(&str, &str)] = &[
    ("agenda", include_str!("../queries/index/agenda.sparql")),
    ("chain", include_str!("../queries/index/chain.sparql")),
    ("default", include_str!("../queries/index/default.sparql")),
    (
        "unscheduled",
        include_str!("../queries/index/unscheduled.sparql"),
    ),
    ("weekly", include_str!("../queries/index/weekly.sparql")),
];

/// Built-in `tree` views.
pub const BUILT_IN_TREE: &[(&str, &str)] =
    &[("work-map", include_str!("../queries/tree/work-map.sparql"))];

/// Built-in `graph` views.
pub const BUILT_IN_GRAPH: &[(&str, &str)] = &[(
    "dependencies",
    include_str!("../queries/graph/dependencies.sparql"),
)];

/// A saved-query name is a plain file stem: reject anything path-shaped so a
/// command-line name can never escape the queries directories.
fn is_safe_name(name: &str) -> bool {
    !name.is_empty()
        && name != "."
        && name != ".."
        && !name.contains(['/', '\\'])
        && !name.contains("..")
}

/// The two drop-in directories for a namespace, project first (highest
/// precedence). `family` scopes into the matching subdirectory; `None` is the
/// flat namespace.
fn dropin_dirs(ctx: &CommandContext, family: Option<&str>) -> [PathBuf; 2] {
    let mut project = ctx.data_dir.join(".clearhead").join("queries");
    let mut user = crate::environment_reader::get_config_dir().join("queries");
    if let Some(family) = family {
        project.push(family);
        user.push(family);
    }
    [project, user]
}

fn read_dropin(ctx: &CommandContext, family: Option<&str>, name: &str) -> Option<(String, Source)> {
    let file = format!("{name}.sparql");
    let [project, user] = dropin_dirs(ctx, family);
    if let Ok(text) = std::fs::read_to_string(project.join(&file)) {
        return Some((text, Source::Project));
    }
    if let Ok(text) = std::fs::read_to_string(user.join(&file)) {
        return Some((text, Source::User));
    }
    None
}

fn built_in(table: &[(&str, &str)], name: &str) -> Option<String> {
    table
        .iter()
        .find(|(n, _)| *n == name)
        .map(|(_, sparql)| (*sparql).to_string())
}

/// Resolve a flat (`query named`) query: project > user drop-in, then built-in.
/// Returns `None` for an unknown or unsafe name.
pub fn resolve_flat(ctx: &CommandContext, name: &str) -> Option<String> {
    if !is_safe_name(name) {
        return None;
    }
    read_dropin(ctx, None, name)
        .map(|(text, _)| text)
        .or_else(|| built_in(BUILT_IN, name))
}

/// Resolve a family (`index`/`tree`/`graph`) query: project > user drop-in,
/// then the family's built-in table.
pub fn resolve_family(
    ctx: &CommandContext,
    family: &str,
    name: &str,
    built_ins: &[(&str, &str)],
) -> Option<String> {
    if !is_safe_name(name) {
        return None;
    }
    read_dropin(ctx, Some(family), name)
        .map(|(text, _)| text)
        .or_else(|| built_in(built_ins, name))
}

/// `query show <name>`: print the SPARQL text of any query, searching families
/// then the flat namespace (graphd's order). The raw document goes to stdout so
/// it can be piped straight into other tooling.
#[cfg(feature = "sparql")]
pub fn show(ctx: &CommandContext, name: &str) -> anyhow::Result<()> {
    let sparql = resolve_family(ctx, "index", name, BUILT_IN_INDEX)
        .or_else(|| resolve_family(ctx, "tree", name, BUILT_IN_TREE))
        .or_else(|| resolve_family(ctx, "graph", name, BUILT_IN_GRAPH))
        .or_else(|| resolve_flat(ctx, name))
        .ok_or_else(|| {
            anyhow::anyhow!("No query named '{name}'. Use `clearhead query list` to see available.")
        })?;
    crate::stdout::write_stdout(sparql.as_bytes())
}

/// `query list`: every resolvable query as a NAME / TYPE / SOURCE table, with
/// drop-ins shadowing built-ins so the listed source is the one that would run.
#[cfg(feature = "sparql")]
pub fn list(ctx: &CommandContext) -> anyhow::Result<()> {
    use comfy_table::{Cell, Color, ContentArrangement, Table, presets::UTF8_FULL};

    let mut rows: Vec<(String, &'static str, Source)> = Vec::new();

    // Flat namespace: built-in names, each overridden by a matching drop-in.
    for (name, _) in BUILT_IN {
        let source = read_dropin(ctx, None, name)
            .map(|(_, s)| s)
            .unwrap_or(Source::BuiltIn);
        rows.push(((*name).to_string(), "—", source));
    }
    push_family_rows(ctx, "index", BUILT_IN_INDEX, &mut rows);
    push_family_rows(ctx, "tree", BUILT_IN_TREE, &mut rows);
    push_family_rows(ctx, "graph", BUILT_IN_GRAPH, &mut rows);
    // Drop-ins with no built-in counterpart, so `list` shows everything runnable.
    push_extra_dropins(ctx, &mut rows);

    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .set_content_arrangement(ContentArrangement::Dynamic)
        .set_header(vec![
            Cell::new("NAME").fg(Color::Cyan),
            Cell::new("TYPE").fg(Color::Cyan),
            Cell::new("SOURCE").fg(Color::Cyan),
        ]);
    for (name, kind, source) in rows {
        table.add_row(vec![
            Cell::new(name),
            Cell::new(kind),
            Cell::new(source.to_string()),
        ]);
    }
    crate::stdout::write_stdout_line(&table.to_string())
}

#[cfg(feature = "sparql")]
fn push_family_rows(
    ctx: &CommandContext,
    family: &'static str,
    built_ins: &[(&str, &str)],
    rows: &mut Vec<(String, &'static str, Source)>,
) {
    for (name, _) in built_ins {
        let source = read_dropin(ctx, Some(family), name)
            .map(|(_, s)| s)
            .unwrap_or(Source::BuiltIn);
        rows.push(((*name).to_string(), family, source));
    }
}

/// Drop-in queries that shadow no built-in — enumerated from disk so `list`
/// reflects the full runnable set. Duplicates of built-in names are skipped
/// (already listed with their shadowing source).
#[cfg(feature = "sparql")]
fn push_extra_dropins(ctx: &CommandContext, rows: &mut Vec<(String, &'static str, Source)>) {
    let scan = |family: Option<&'static str>,
                kind: &'static str,
                rows: &mut Vec<(String, &'static str, Source)>| {
        for (dir, source) in dropin_dirs(ctx, family)
            .into_iter()
            .zip([Source::Project, Source::User])
        {
            let Ok(entries) = std::fs::read_dir(&dir) else {
                continue;
            };
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) != Some("sparql") {
                    continue;
                }
                let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
                    continue;
                };
                let already = rows.iter().any(|(n, k, _)| n == stem && *k == kind);
                if !already {
                    rows.push((stem.to_string(), kind, source));
                }
            }
        }
    };
    scan(None, "—", rows);
    scan(Some("index"), "index", rows);
    scan(Some("tree"), "tree", rows);
    scan(Some("graph"), "graph", rows);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn built_in_tables_parse_and_self_declare_prefixes() {
        // Every shipped built-in must be a complete standard SPARQL document —
        // it runs verbatim (only view variables are bound), so it cannot rely
        // on graphd-era prefix injection.
        for (name, sparql) in BUILT_IN
            .iter()
            .chain(BUILT_IN_INDEX)
            .chain(BUILT_IN_TREE)
            .chain(BUILT_IN_GRAPH)
        {
            assert!(
                sparql.to_lowercase().contains("prefix "),
                "{name} must self-declare its PREFIX lines"
            );
        }
    }

    #[test]
    fn unsafe_names_never_resolve() {
        for bad in ["", ".", "..", "../secrets", "a/b", "a\\b", "..hidden.."] {
            assert!(!is_safe_name(bad), "{bad:?} must be rejected");
        }
        for good in ["agenda", "my-query", "weekly_rollup"] {
            assert!(is_safe_name(good), "{good:?} must be accepted");
        }
    }
}
