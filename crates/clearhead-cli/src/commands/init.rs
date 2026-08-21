use anyhow::Context;
use std::fs;
use std::path::{Path, PathBuf};

use clearhead_core::workspace::WorkspaceManifest;

use crate::environment_reader::{get_data_dir, load_config, resolve_file_path};

/// Guard against nesting `.clearhead/` inside a user-layout workspace.
///
/// User-layout workspaces (specifications/workspace.md) keep their charters
/// directly at the resolved data_dir root, with no `.clearhead/` wrapper.
/// Running `init` at or under that root would create a shell that
/// `find_project_data_dir()` then treats as a separate (empty) project
/// workspace, silently shadowing the real charters. Returns the resolved
/// user data dir when `cwd` collides with it; `None` means it's safe to
/// proceed (including when `.clearhead/` already exists at `cwd` — that's
/// the existing-config idempotent path, not new nesting).
fn nested_in_user_workspace(
    cwd: &Path,
    clearhead_dir: &Path,
    config_path_override: Option<PathBuf>,
) -> anyhow::Result<Option<PathBuf>> {
    if clearhead_dir.exists() {
        return Ok(None);
    }
    let config = load_config(config_path_override).context("Failed to load config")?;
    let user_data_dir = if config.data_dir.is_empty() {
        get_data_dir()
    } else {
        resolve_file_path(&config.data_dir, &get_data_dir())
    };
    let canonical_cwd = cwd.canonicalize().unwrap_or_else(|_| cwd.to_path_buf());
    let canonical_user_data_dir = user_data_dir
        .canonicalize()
        .unwrap_or_else(|_| user_data_dir.clone());
    if canonical_cwd.starts_with(&canonical_user_data_dir) {
        Ok(Some(user_data_dir))
    } else {
        Ok(None)
    }
}

/// Initialize a clearhead workspace in the current directory.
///
/// Creates `.clearhead/workspace.json` (the identity manifest) with a stable
/// workspace UUID and a name derived from the current directory. Bootstraps the
/// project root charter at `.clearhead/charters/next.actions` if absent.
/// Idempotent — safe to rerun; does not overwrite existing data or identity.
pub fn run(config_path_override: Option<PathBuf>) -> anyhow::Result<()> {
    let cwd = std::env::current_dir().context("Cannot determine current directory")?;

    let clearhead_dir = cwd.join(".clearhead");

    if let Some(user_data_dir) =
        nested_in_user_workspace(&cwd, &clearhead_dir, config_path_override)?
    {
        anyhow::bail!(
            "Refusing to init: {} is at or inside the resolved user workspace root ({}).\n\
             Nesting .clearhead/ here would shadow the real charters. Run `clearhead init` \
             from a different directory, or set `data_dir` in your config to relocate the \
             user workspace first.",
            cwd.display(),
            user_data_dir.display()
        );
    }

    let charters_dir = clearhead_dir.join("charters");

    fs::create_dir_all(&clearhead_dir).context("Failed to create .clearhead/")?;
    fs::create_dir_all(&charters_dir).context("Failed to create .clearhead/charters/")?;

    // Keep config.local.json — the git-ignored personal override — out of version
    // control. A scoped .clearhead/.gitignore owns this rule so we don't touch the
    // project root's ignore conventions. Written unconditionally (independent of
    // the identity guard below) so existing workspaces pick it up on a rerun.
    let gitignore_path = clearhead_dir.join(".gitignore");
    let mut gitignore = fs::read_to_string(&gitignore_path).unwrap_or_default();
    for rule in ["config.local.json", ".clearhead.lock", "sync/"] {
        if !gitignore.lines().any(|line| line.trim() == rule) {
            if !gitignore.is_empty() && !gitignore.ends_with('\n') {
                gitignore.push('\n');
            }
            gitignore.push_str(rule);
            gitignore.push('\n');
        }
    }
    fs::write(&gitignore_path, gitignore).context("Failed to write .clearhead/.gitignore")?;

    // A project layout always has a root charter. `next.actions` is the signal
    // that lets the loader resolve flat named charters as its children. Without
    // it, a fresh `init -> add charter -> add plan` invents an unresolvable
    // parent and routes the plan into a charterless vdir slug.
    let root_actions = charters_dir.join("next.actions");
    if !root_actions.exists() {
        clearhead_core::workspace::durability::atomic_write(&root_actions, "")
            .context("Failed to create root charter actions file")?;
    }
    clearhead_core::workspace::sidecar::stamp_charter_id(&root_actions, uuid::Uuid::now_v7())
        .context("Failed to record root charter id")?;

    // Idempotent on an existing identity: init never clobbers or re-mints a
    // workspace that already has a workspace_id (which would orphan the named
    // graph). Root-charter repair above is deliberately still performed.
    if WorkspaceManifest::read(&cwd).workspace_id.is_some() {
        println!("Already initialized — workspace already has an identity.");
        return Ok(());
    }

    let workspace_name = cwd
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("workspace")
        .to_string();
    let workspace_id = uuid::Uuid::now_v7().to_string();
    let created_at = chrono::Local::now().format("%Y-%m-%d").to_string();

    WorkspaceManifest {
        workspace_id: Some(workspace_id.clone()),
        workspace_name: Some(workspace_name.clone()),
        created_at: Some(created_at),
    }
    .write(&cwd)
    .context("Failed to write workspace.json")?;

    println!(
        "Initialized workspace '{}' ({})",
        workspace_name,
        &workspace_id[..8]
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    /// Writes a config.json pointing `data_dir` at the given path. Returns
    /// the owning TempDir alongside the config path — caller must keep the
    /// guard alive for as long as the path is used.
    fn config_pointing_at(data_dir: &Path) -> (TempDir, PathBuf) {
        let tmp = TempDir::new().unwrap();
        let config_path = tmp.path().join("config.json");
        clearhead_core::workspace::durability::atomic_write(
            &config_path,
            format!(r#"{{"data_dir": "{}"}}"#, data_dir.display()),
        )
        .unwrap();
        (tmp, config_path)
    }

    #[test]
    fn refuses_at_user_workspace_root() {
        let user_ws = TempDir::new().unwrap();
        let (_guard, config_path) = config_pointing_at(user_ws.path());

        let result = nested_in_user_workspace(
            user_ws.path(),
            &user_ws.path().join(".clearhead"),
            Some(config_path),
        )
        .unwrap();

        assert_eq!(result, Some(user_ws.path().to_path_buf()));
    }

    #[test]
    fn refuses_in_subdir_of_user_workspace_root() {
        let user_ws = TempDir::new().unwrap();
        let nested = user_ws.path().join("charters").join("sub");
        fs::create_dir_all(&nested).unwrap();
        let (_guard, config_path) = config_pointing_at(user_ws.path());

        let result =
            nested_in_user_workspace(&nested, &nested.join(".clearhead"), Some(config_path))
                .unwrap();

        assert_eq!(result, Some(user_ws.path().to_path_buf()));
    }

    #[test]
    fn allows_unrelated_project_dir() {
        let user_ws = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();
        let (_guard, config_path) = config_pointing_at(user_ws.path());

        let result = nested_in_user_workspace(
            project.path(),
            &project.path().join(".clearhead"),
            Some(config_path),
        )
        .unwrap();

        assert_eq!(result, None);
    }

    #[test]
    fn allows_rerun_when_clearhead_dir_already_exists() {
        let user_ws = TempDir::new().unwrap();
        let clearhead_dir = user_ws.path().join(".clearhead");
        fs::create_dir_all(&clearhead_dir).unwrap();
        let (_guard, config_path) = config_pointing_at(user_ws.path());

        let result =
            nested_in_user_workspace(user_ws.path(), &clearhead_dir, Some(config_path)).unwrap();

        assert_eq!(result, None);
    }
}
