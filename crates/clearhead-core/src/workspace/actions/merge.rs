//! Source-text boundary for semantic merges.

use super::diff::{Conflict, conflict_choices, semantic_merge};
use super::{
    Action, ActionList, ParsedDocument, parse_actions, parse_document, parse_trusted_document,
};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug)]
pub enum SourceMerge {
    Merged {
        text: String,
        conflicts: Vec<Conflict>,
    },
    /// All inputs are valid, but at least one action lacks stable identity.
    NeedsLineMerge,
}

/// Merge UUID-identified source. Mark the smallest regions that can be
/// resolved independently without losing action identity or nesting.
pub fn merge_source(base: &str, ours: &str, theirs: &str) -> Result<SourceMerge, String> {
    let [base_doc, ours_doc, theirs_doc] = [
        parse_input("base", base)?,
        parse_input("ours", ours)?,
        parse_input("theirs", theirs)?,
    ];
    if [&base_doc, &ours_doc, &theirs_doc]
        .into_iter()
        .any(|doc| doc.source_map.values().any(|meta| meta.is_id_generated))
    {
        return Ok(SourceMerge::NeedsLineMerge);
    }
    let [base, ours, theirs] = [
        Source::new("base", base, base_doc)?,
        Source::new("ours", ours, ours_doc)?,
        Source::new("theirs", theirs, theirs_doc)?,
    ];
    let sources = [&ours, &theirs, &base];
    let sides = [&base.actions, &ours.actions, &theirs.actions];
    let result = semantic_merge(sides[0], sides[1], sides[2]);
    let text = if result.conflicts.is_empty() {
        if let Some(source) = sources
            .iter()
            .find(|source| source.actions == result.merged)
        {
            source.text.into()
        } else {
            render_choice(&place(&result.merged)?, &result.merged, sources)?
        }
    } else {
        let [ours, theirs] = conflict_choices(sides, &result);
        let [ours_rows, theirs_rows] = [place(&ours)?, place(&theirs)?];
        render_choice(&ours_rows, &ours, sources)?;
        render_choice(&theirs_rows, &theirs, sources)?;
        render_conflicts([&ours_rows, &theirs_rows], sources)?
    };
    Ok(SourceMerge::Merged {
        text,
        conflicts: result.conflicts,
    })
}

/// Check the result from `git merge-file` before writing it back. Git can put
/// markers inside a multiline description, where Core would read them as text.
/// Only write a conflicted result if Core sees every conflict block.
pub fn validate_line_merge(text: &str, conflicts: usize) -> Result<(), String> {
    if conflicts == 0 {
        parse_input("text merge result", text)?;
        return Ok(());
    }
    // git merge-file caps its conflict count at 127.
    if readable_conflicts(text)?.min(127) != conflicts {
        return Err("Git placed conflict markers where ClearHead cannot read them (for example, inside a description)".into());
    }
    Ok(())
}

fn parse_input(side: &str, text: &str) -> Result<ParsedDocument, String> {
    let doc = parse_trusted_document(text)
        .map_err(|error| format!("{side}: {error}"))?
        .into_parsed();
    if doc.source_map.len() != doc.actions.len() {
        return Err(format!("{side}: duplicate UUID in action file"));
    }
    for action in &doc.actions {
        let meta = &doc.source_map[&action.id];
        if let Some(raw_id) = &meta.raw_id {
            return Err(format!(
                "{side}, line {}: invalid UUID {raw_id}",
                meta.line_range.start_row + 1
            ));
        }
    }
    Ok(doc)
}

fn readable_conflicts(text: &str) -> Result<usize, String> {
    let lines: Vec<_> = text.lines().collect();
    Ok(parse_document(text)?
        .syntax_errors
        .iter()
        .filter(|diagnostic| {
            diagnostic.code == "E010"
                && lines.get(diagnostic.range.start_row) == Some(&"<<<<<<< ours")
                && lines.get(diagnostic.range.end_row) == Some(&">>>>>>> theirs")
        })
        .count())
}

fn render_choice(
    rows: &[Placed<'_>],
    expected: &ActionList,
    sources: [&Source<'_>; 3],
) -> Result<String, String> {
    let mut text = String::new();
    for row in rows {
        append_row(&mut text, row, sources);
    }
    text.push_str(sources[0].trailing);
    if &parse_actions(&text).map_err(|err| format!("invalid merge result: {err}"))? != expected {
        return Err("merge result would change when read back".into());
    }
    Ok(text)
}

struct Source<'a> {
    text: &'a str,
    actions: ActionList,
    rows: HashMap<Uuid, Row<'a>>,
    trailing: &'a str,
}

struct Row<'a> {
    index: usize,
    text: &'a str,
    leading: &'a str,
    depth: usize,
}

impl<'a> Source<'a> {
    fn new(side: &str, text: &'a str, doc: ParsedDocument) -> Result<Self, String> {
        let starts: Vec<_> = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(at, _)| at + 1))
            .collect();
        let mut rows = HashMap::new();
        let mut previous_end = 0;
        for (index, (action, depth)) in doc.actions.iter().zip(depths(&doc.actions)?).enumerate() {
            let meta = &doc.source_map[&action.id];
            let start = starts[meta.line_range.start_row];
            let end = starts
                .get(meta.line_range.end_row + 1)
                .copied()
                .unwrap_or(text.len());
            let leading = text
                .get(previous_end..start)
                .filter(|gap| gap.trim().is_empty())
                .ok_or_else(|| format!("{side}: cannot isolate whole lines for {}", action.id))?;
            rows.insert(
                action.id,
                Row {
                    index,
                    text: &text[start..end],
                    leading,
                    depth,
                },
            );
            previous_end = end;
        }
        Ok(Self {
            text,
            actions: doc.actions,
            rows,
            trailing: &text[previous_end..],
        })
    }
}

fn depths(actions: &ActionList) -> Result<Vec<usize>, String> {
    let mut depths = HashMap::<Uuid, usize>::new();
    actions
        .iter()
        .map(|action| {
            let depth = match action.parent_id {
                Some(id) => {
                    1 + depths
                        .get(&id)
                        .ok_or_else(|| format!("missing parent {id} before {}", action.id))?
                }
                None => 0,
            };
            depths.insert(action.id, depth);
            Ok(depth)
        })
        .collect()
}

#[derive(PartialEq)]
struct Placed<'a> {
    action: &'a Action,
    ancestors: Vec<Uuid>,
}

fn place(list: &ActionList) -> Result<Vec<Placed<'_>>, String> {
    let mut ancestors = Vec::new();
    list.iter()
        .zip(depths(list)?)
        .map(|(action, depth)| {
            ancestors.truncate(depth);
            if ancestors.len() != depth || ancestors.last().copied() != action.parent_id {
                return Err(format!(
                    "merge result is not in tree order at {}",
                    action.id
                ));
            }
            let row = Placed {
                action,
                ancestors: ancestors.clone(),
            };
            ancestors.push(action.id);
            Ok(row)
        })
        .collect()
}

fn render_conflicts(rows: [&[Placed<'_>]; 2], sources: [&Source<'_>; 3]) -> Result<String, String> {
    let positions = rows.map(|side| {
        side.iter()
            .enumerate()
            .map(|(i, row)| (row.action.id, i))
            .collect()
    });
    let mut text = String::new();
    let mut start = [0; 2];
    let mut blocks = 0;
    loop {
        let [left, right] = [rows[0].get(start[0]), rows[1].get(start[1])];
        if left.is_none() && right.is_none() {
            break;
        }
        if let Some(row) = left.filter(|_| left == right) {
            append_row(&mut text, row, sources);
            start = start.map(|i| i + 1);
            continue;
        }
        let end = conflict_end(rows, &positions, start);
        for side in 0..2 {
            text.push_str(["<<<<<<< ours\n", "=======\n"][side]);
            for row in &rows[side][start[side]..end[side]] {
                append_row(&mut text, row, sources);
            }
        }
        text.push_str(">>>>>>> theirs\n");
        blocks += 1;
        start = end;
    }
    text.push_str(sources[0].trailing);
    if blocks == 0 || readable_conflicts(&text)? != blocks {
        return Err("cannot render readable markers for this merge".into());
    }
    Ok(text)
}

/// Advance until the same UUID never falls across two blocks and either block
/// can be followed by either suffix without changing the suffix's parents.
fn conflict_end(
    rows: [&[Placed<'_>]; 2],
    positions: &[HashMap<Uuid, usize>; 2],
    start: [usize; 2],
) -> [usize; 2] {
    let mut end = start;
    let mut required = start;
    let first = (0..2)
        .find(|&side| {
            rows[side]
                .get(start[side])
                .is_some_and(|row| !positions[1 - side].contains_key(&row.action.id))
        })
        .unwrap_or(0);
    required[first] += 1;
    loop {
        for side in 0..2 {
            while end[side] < required[side] {
                let id = rows[side][end[side]].action.id;
                if let Some(other) = positions[1 - side].get(&id) {
                    required[1 - side] = required[1 - side].max(other + 1);
                }
                end[side] += 1;
            }
        }
        if required != end {
            continue;
        }
        for side in 0..2 {
            let prefix = &rows[1 - side][..end[1 - side]];
            if let Some(next) = rows[side].get(end[side])
                && !next.ancestors.is_empty()
                && !prefix.last().is_some_and(|last| {
                    last.ancestors
                        .iter()
                        .chain(std::iter::once(&last.action.id))
                        .take(next.ancestors.len())
                        .eq(&next.ancestors)
                })
            {
                required[side] += 1;
            }
        }
        if required == end {
            return end;
        }
    }
}

fn append_row(text: &mut String, row: &Placed<'_>, sources: [&Source<'_>; 3]) {
    if let Some(original) = sources
        .iter()
        .find_map(|source| source.rows.get(&row.action.id))
    {
        text.push_str(original.leading);
    }
    text.push_str(&render(row.action, row.ancestors.len(), sources));
}

fn render(action: &Action, depth: usize, sources: [&Source<'_>; 3]) -> String {
    for source in sources {
        if let Some(row) = source.rows.get(&action.id)
            && row.depth == depth
            && source.actions[row.index] == *action
        {
            let mut text = row.text.to_string();
            if !text.ends_with('\n') {
                text.push('\n');
            }
            return text;
        }
    }
    format!("{}{}{action}\n", "    ".repeat(depth), ">".repeat(depth))
}
