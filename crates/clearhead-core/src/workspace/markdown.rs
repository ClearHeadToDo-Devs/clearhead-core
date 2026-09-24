//! Markdown structure for charter documents and action notes.
//!
//! Every consumer reads headings and code through this one CommonMark parser
//! (pulldown-cmark), so they agree on what is a heading, what is fenced code,
//! and where a section ends. Ranges are byte offsets into the input.

use pulldown_cmark::{Event, Parser, Tag, TagEnd};
use std::ops::Range;

/// A heading outside code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Heading {
    /// 1 for `#`, 2 for `##`, and so on.
    pub level: u8,
    /// The heading's source text without its markers, markup kept verbatim.
    pub text: String,
    /// The whole heading, markers and line ending included.
    pub range: Range<usize>,
}

/// A heading and the source after it, up to the next heading of the same or
/// higher rank.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Section<'a> {
    pub heading: Heading,
    pub body: &'a str,
    pub body_range: Range<usize>,
}

/// Every heading outside code, in document order. A heading with no text is
/// skipped: it names nothing.
pub fn headings(markdown: &str) -> Vec<Heading> {
    let mut found = Vec::new();
    let mut open: Option<(u8, Range<usize>, Option<Range<usize>>)> = None;
    for (event, range) in Parser::new(markdown).into_offset_iter() {
        match (&event, &mut open) {
            (Event::Start(Tag::Heading { level, .. }), _) => {
                open = Some((*level as u8, range, None));
            }
            (Event::End(TagEnd::Heading(_)), Some(_)) => {
                let (level, range, content) = open.take().expect("matched Some");
                let text = content.map_or("", |span| markdown[span].trim());
                if !text.is_empty() {
                    found.push(Heading {
                        level,
                        text: text.to_string(),
                        range,
                    });
                }
            }
            (_, Some((_, _, content))) => {
                let span = content.get_or_insert(range.clone());
                span.start = span.start.min(range.start);
                span.end = span.end.max(range.end);
            }
            _ => {}
        }
    }
    found
}

/// Split at headings of `max_level` or higher rank: the text before the first
/// such heading, then one section per heading.
pub fn sections(markdown: &str, max_level: u8) -> (&str, Vec<Section<'_>>) {
    let splits: Vec<Heading> = headings(markdown)
        .into_iter()
        .filter(|heading| heading.level <= max_level)
        .collect();
    let preamble = &markdown[..splits.first().map_or(markdown.len(), |h| h.range.start)];
    let sections = splits
        .iter()
        .enumerate()
        .map(|(index, heading)| {
            let end = splits
                .get(index + 1)
                .map_or(markdown.len(), |next| next.range.start);
            let body_range = heading.range.end..end;
            Section {
                heading: heading.clone(),
                body: &markdown[body_range.clone()],
                body_range,
            }
        })
        .collect();
    (preamble, sections)
}

/// Lines that are not inside a code block, without their line endings.
pub fn prose_lines(markdown: &str) -> impl Iterator<Item = &str> {
    let code: Vec<Range<usize>> = Parser::new(markdown)
        .into_offset_iter()
        .filter(|(event, _)| matches!(event, Event::Start(Tag::CodeBlock(_))))
        .map(|(_, range)| range)
        .collect();
    let mut offset = 0;
    markdown.split_inclusive('\n').filter_map(move |line| {
        let start = offset;
        offset += line.len();
        (!code.iter().any(|block| block.contains(&start)))
            .then(|| line.trim_end_matches(['\r', '\n']))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fenced_and_empty_headings_are_not_headings() {
        let text = "# Title\n\n```md\n## Not one\n```\n\n~~~~\n## Nor this\n~~~~\n\n##\n\n## Real `code` **kept**\n";
        let found: Vec<_> = headings(text)
            .into_iter()
            .map(|h| (h.level, h.text))
            .collect();
        assert_eq!(
            found,
            [(1, "Title".into()), (2, "Real `code` **kept**".into())]
        );
    }

    #[test]
    fn unclosed_fence_runs_to_the_end() {
        assert!(headings("```\n## hidden\n").is_empty());
    }

    #[test]
    fn sections_end_at_the_same_or_higher_rank() {
        let text = "intro\n# Title\nabout\n## Log\n### detail\nentry\n## Notes\nlast\n";
        let (preamble, sections) = sections(text, 2);
        assert_eq!(preamble, "intro\n");
        let parts: Vec<_> = sections
            .iter()
            .map(|s| (s.heading.text.as_str(), s.body))
            .collect();
        assert_eq!(
            parts,
            [
                ("Title", "about\n"),
                ("Log", "### detail\nentry\n"),
                ("Notes", "last\n")
            ]
        );
    }

    #[test]
    fn prose_lines_skip_code_blocks() {
        let text = "Decision 1: yes\n```\nDecision 2: no\n```\nafter\n";
        assert_eq!(
            prose_lines(text).collect::<Vec<_>>(),
            ["Decision 1: yes", "after"]
        );
    }
}
