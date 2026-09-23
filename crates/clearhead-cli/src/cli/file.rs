use anyhow::Context;
use tracing::debug;

use crate::cli::{
    CommandContext, parse_content_for_mutation, parse_content_for_read, parse_content_for_rewrite,
    read_input, write_or_print,
};

pub fn format_file(
    ctx: &CommandContext,
    path: &Option<std::path::PathBuf>,
    write: bool,
    style: &Option<crate::argparser::Style>,
    indent_style: &Option<crate::argparser::Indent>,
    indent_width: &Option<usize>,
) -> anyhow::Result<()> {
    let input_file = path.as_ref();
    debug!(input_file = ?input_file, write = write, "Executing Format File");
    let content = read_input(input_file)?;
    let source = source_label(input_file);
    // Formatting is a rewrite even when emitted to stdout: callers may pipe
    // that output over the source. Never serialize parser recovery.
    let document = parse_content_for_rewrite(&content, &source, "format file")?;

    let (config_indent_style, config_indent_width) = ctx.indent_config();

    let resolved_indent_style = indent_style
        .map(|i| i.into())
        .unwrap_or(config_indent_style);

    let resolved_indent_width = indent_width.unwrap_or(config_indent_width);

    let format_config = clearhead_cli::FormatConfig {
        style: style
            .map(|s| s.into())
            .unwrap_or(clearhead_cli::FormatStyle::Compact),
        indent_style: resolved_indent_style,
        indent_width: resolved_indent_width,
        include_id: true,
    };

    let formatted = clearhead_cli::format_trusted_source(&document, Some(format_config))
        .map_err(|e| anyhow::anyhow!(e))?;

    write_or_print(&formatted, write, input_file)?;
    Ok(())
}

pub fn lint_file(path: &Option<std::path::PathBuf>) -> anyhow::Result<()> {
    let input_file = path.as_ref();
    debug!(input_file = ?input_file, "Executing Lint File");
    let content = read_input(input_file)?;

    let parsed = clearhead_cli::get_parsed_document(&content)
        .map_err(|e| anyhow::anyhow!("Failed to parse document: {}", e))?;

    let results = clearhead_cli::lint_document(&parsed);

    if results.errors.is_empty() && results.warnings.is_empty() && results.info.is_empty() {
        println!("no issues found");
        return Ok(());
    }

    let has_errors = !results.errors.is_empty();
    for diag in results {
        let severity_str = match diag.severity {
            clearhead_cli::LintSeverity::Error => "ERROR",
            clearhead_cli::LintSeverity::Warning => "WARN",
            clearhead_cli::LintSeverity::Info => "INFO",
        };

        let file_str = input_file
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| "<stdin>".to_string());
        println!(
            "{}:{}:{}: {}: {} [{}]",
            file_str,
            diag.range.start_row + 1,
            diag.range.start_col + 1,
            severity_str,
            diag.message,
            diag.code
        );
    }

    if has_errors {
        tracing::warn!("Linting failed with errors");
        anyhow::bail!("Linting failed with errors");
    }
    Ok(())
}

pub fn normalize_file(
    ctx: &CommandContext,
    path: &Option<std::path::PathBuf>,
    write: bool,
    no_format: bool,
) -> anyhow::Result<()> {
    let input_file = path.as_ref();
    debug!(input_file = ?input_file, write = write, "Executing Normalize File");
    let content = read_input(input_file)?;
    let source = source_label(input_file);
    // Charter documents are Markdown, not actions DSL. Normalize their
    // identity surgically so unmodelled frontmatter and log text survive.
    if input_file.is_some_and(|path| path.extension().is_some_and(|ext| ext == "md")) {
        let document = clearhead_core::workspace::parse_charter(&content)
            .map_err(|error| anyhow::anyhow!("Cannot normalize charter {source}: {error}"))?;
        let output = if document.id.is_some() {
            content
        } else {
            let id = charter_sidecar_id(input_file.unwrap())?.unwrap_or_else(uuid::Uuid::now_v7);
            // Insert only the missing field; do not round-trip the document.
            stamp_charter_id(&content, id)?
        };
        // Never persist a stamped document that the charter codec cannot read
        // back with the intended identity (e.g. unusual YAML key syntax).
        let stamped = clearhead_core::workspace::parse_charter(&output).map_err(|error| {
            anyhow::anyhow!("Cannot normalize charter {source}: {error}; file not modified")
        })?;
        anyhow::ensure!(
            stamped.id.is_some(),
            "Cannot normalize charter {source}: id was not stamped; file not modified"
        );
        return write_or_print(&output, write, input_file);
    }
    let document = parse_content_for_rewrite(&content, &source, "normalize file")?;

    let output = if no_format {
        clearhead_cli::format_trusted_source(&document, None).map_err(|e| anyhow::anyhow!(e))?
    } else {
        let (resolved_indent_style, resolved_indent_width) = ctx.indent_config();

        let format_config = clearhead_cli::FormatConfig {
            style: clearhead_cli::FormatStyle::Compact,
            indent_style: resolved_indent_style,
            indent_width: resolved_indent_width,
            include_id: true,
        };
        clearhead_cli::format_trusted_source(&document, Some(format_config))
            .map_err(|e| anyhow::anyhow!(e))?
    };

    write_or_print(&output, write, input_file)?;
    if write
        && let Some(file_path) = input_file
        && let Err(e) = super::update_sidecar(file_path, document.actions())
    {
        tracing::warn!(path = %file_path.display(), error = %e, "Failed to update sidecar");
    }
    Ok(())
}

fn charter_sidecar_id(path: &std::path::Path) -> anyhow::Result<Option<uuid::Uuid>> {
    let actions = if path.file_name().is_some_and(|name| name == "README.md") {
        path.with_file_name("next.actions")
    } else {
        path.with_extension("actions")
    };
    let sidecar = clearhead_core::workspace::sidecar_path(&actions);
    Ok(clearhead_cli::filesystem::sidecar::read_sidecar(&sidecar)?
        .charter
        .and_then(|charter| charter.id))
}

fn stamp_charter_id(content: &str, id: uuid::Uuid) -> anyhow::Result<String> {
    let leading = content.len() - content.trim_start().len();
    let start = &content[leading..];
    let opening = start.find('\n').and_then(|end| {
        (start[..end].trim_end() == "---").then_some((
            end + 1,
            if start.as_bytes()[end - 1] == b'\r' {
                "\r\n"
            } else {
                "\n"
            },
        ))
    });
    if let Some((opening_len, newline)) = opening {
        let split = leading + opening_len;
        let mut offset = split;
        for line in content[split..].split_inclusive('\n') {
            if line.trim_end() == "---" {
                break;
            }
            if line.starts_with("id:") {
                // A present but null identity is still missing; replace its
                // value rather than introducing a duplicate YAML key.
                let end = offset + line.trim_end_matches(['\r', '\n']).len();
                let value_start = offset + 3;
                let scalar_start = value_start + content[value_start..end].len()
                    - content[value_start..end].trim_start().len();
                let value = &content[scalar_start..end];
                let scalar_len = if value.is_empty() || value.starts_with('#') {
                    0
                } else if value.starts_with("null") {
                    4
                } else if value.starts_with('~') {
                    1
                } else {
                    anyhow::bail!(
                        "Cannot normalize charter: unsupported null id syntax; file not modified"
                    );
                };
                let suffix = &content[scalar_start + scalar_len..end];
                anyhow::ensure!(
                    suffix.is_empty()
                        || suffix.starts_with(char::is_whitespace)
                        || suffix.starts_with('#'),
                    "Cannot normalize charter: ambiguous id value; file not modified"
                );
                return Ok(format!(
                    "{}{}{}",
                    &content[..scalar_start],
                    id,
                    &content[scalar_start + scalar_len..]
                ));
            }
            offset += line.len();
        }
        Ok(format!(
            "{}id: {id}{newline}{}",
            &content[..split],
            &content[split..]
        ))
    } else {
        let newline = if content.contains("\r\n") {
            "\r\n"
        } else {
            "\n"
        };
        Ok(format!(
            "---{newline}id: {id}{newline}---{newline}{content}"
        ))
    }
}

pub fn patch_file(
    primary: &std::path::PathBuf,
    secondary: &std::path::PathBuf,
    write: bool,
) -> anyhow::Result<()> {
    use std::fs;

    debug!(primary = %primary.display(), secondary = %secondary.display(), write = write, "Executing Patch File");
    let primary_content = fs::read_to_string(primary).context("Failed to read primary file")?;
    let secondary_content =
        fs::read_to_string(secondary).context("Failed to read secondary file")?;

    let mut primary_actions = if write {
        parse_content_for_mutation(
            &primary_content,
            &primary.display().to_string(),
            "patch file",
        )?
    } else {
        parse_content_for_read(
            &primary_content,
            &primary.display().to_string(),
            "patch file",
        )?
    };
    let secondary_actions = parse_content_for_read(
        &secondary_content,
        &secondary.display().to_string(),
        "patch file",
    )?;

    clearhead_cli::patch_action_list(&mut primary_actions, &secondary_actions);

    let formatted = clearhead_cli::format(
        &primary_actions,
        clearhead_cli::OutputFormat::Actions,
        None,
        None,
    )
    .map_err(|e| anyhow::anyhow!(e))?;

    write_or_print(&formatted, write, Some(primary))?;
    Ok(())
}

fn source_label(path: Option<&std::path::PathBuf>) -> String {
    path.map(|p| p.display().to_string())
        .unwrap_or_else(|| "stdin".to_string())
}
