use anyhow::Context;
use std::fs;
use std::path::{Path, PathBuf};

use clearhead_core::workspace::resource::Effect;
use clearhead_core::workspace::{InitPlan, InitRequest, RootId};

use crate::environment_reader::{load_config, user_data_dir};

/// Guard against nesting `.clearhead/` inside the user workspace.
///
/// The user workspace keeps its charters directly at the resolved data_dir,
/// with no `.clearhead/` wrapper. Running a project `init` at or under that
/// root would create a shell that `find_project_data_dir()` then treats as a
/// separate (empty) project workspace, silently shadowing the real charters.
/// An existing `.clearhead/` at `cwd` is the idempotent rerun path, not new
/// nesting.
fn nested_in_user_workspace(cwd: &Path, clearhead_dir: &Path, user_data_dir: &Path) -> bool {
    if clearhead_dir.exists() {
        return false;
    }
    let canonical = |path: &Path| path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    canonical(cwd).starts_with(canonical(user_data_dir))
}

/// Initialize a project workspace in the current directory, or the user
/// workspace at the configured data_dir with `--user`.
///
/// Both scopes bootstrap the same data-root shape (specifications/workspace.md,
/// Initialization): the identity manifest, the root charter README, its
/// `next.actions` anchor and sidecar. Idempotent — a rerun only fills in what
/// is missing and never re-mints identity.
pub fn run(
    config_path_override: Option<PathBuf>,
    user: bool,
    name: Option<String>,
) -> anyhow::Result<()> {
    let config = load_config(config_path_override).context("Failed to load config")?;
    let user_data_dir = user_data_dir(&config);

    let (root, default_name) = if user {
        (user_data_dir, name.unwrap_or_else(current_username))
    } else {
        let cwd = std::env::current_dir().context("Cannot determine current directory")?;
        prepare_project(&cwd, &user_data_dir)?;
        let dir_name = cwd
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("workspace")
            .to_string();
        (cwd, name.unwrap_or(dir_name))
    };

    let request = InitRequest {
        name: default_name,
        workspace_id: uuid::Uuid::now_v7(),
        root_id: uuid::Uuid::now_v7(),
        created_at: chrono::Local::now().format("%Y-%m-%d").to_string(),
    };
    let plan = clearhead_cli::filesystem::init_workspace(&root, &request)
        .context("Failed to initialize workspace")?;
    report(&plan);
    Ok(())
}

/// Create `.clearhead/` and its scoped `.gitignore`, refusing to nest inside
/// the user workspace.
fn prepare_project(cwd: &Path, user_data_dir: &Path) -> anyhow::Result<()> {
    let clearhead_dir = cwd.join(".clearhead");
    if nested_in_user_workspace(cwd, &clearhead_dir, user_data_dir) {
        anyhow::bail!(
            "Refusing to init: {} is at or inside the user workspace root ({}).\n\
             Nesting .clearhead/ here would shadow the real charters. Run `clearhead init --user` \
             to initialize the user workspace itself, or `clearhead init` from a different directory.",
            cwd.display(),
            user_data_dir.display()
        );
    }
    fs::create_dir_all(&clearhead_dir).context("Failed to create .clearhead/")?;

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
    Ok(())
}

/// The login name that seeds a user workspace's name; persisted once by init.
fn current_username() -> String {
    std::env::var("USER")
        .or_else(|_| std::env::var("USERNAME"))
        .unwrap_or_else(|_| "user".to_string())
}

fn report(plan: &InitPlan) {
    let name = plan.manifest.workspace_name.as_deref().unwrap_or_default();
    let id = plan.manifest.workspace_id.as_deref().unwrap_or_default();
    if plan.minted_workspace {
        println!("Initialized workspace '{}' ({})", name, id);
    } else {
        println!("Workspace '{}' already initialized ({})", name, id);
    }
    let wrote_readme = plan.batch.effects().iter().any(|effect| {
        matches!(effect, Effect::Write { path, .. } if path.path.as_str() == clearhead_core::workspace::init::ROOT_README_PATH)
    });
    for effect in plan.batch.effects() {
        if let Effect::Write { path, .. } = effect {
            println!("  wrote {}", path.path.as_str());
        }
    }
    if wrote_readme {
        // init writes the root as New (a workspace starts in planning,
        // specifications/workspace.md, The Root Charter); say so loudly, since
        // a New root's open Actions are otherwise silently hidden from
        // engagement until someone activates it. Use the stable ID in the
        // command: names can contain shell whitespace or metacharacters.
        if let RootId::Resolved(root_id) = plan.root_id {
            println!(
                "The root charter '{}' is New; its Actions are hidden from engagement until activated. \
                 Run `clearhead update charter {root_id} --state active` when ready.",
                name
            );
        } else {
            println!(
                "The root charter '{}' is New; its Actions are hidden from engagement until activated. \
                 Resolve its identity with `clearhead doctor` before activation.",
                name
            );
        }
    }
    match plan.root_id {
        RootId::Resolved(_) => {}
        RootId::MissingFromReadme => eprintln!(
            "warning: charters/README.md declares no id; root identity left for `clearhead doctor`"
        ),
        RootId::Conflict { readme, sidecar } => eprintln!(
            "warning: root charter ids disagree (README.md {}, .next.json {}); left for `clearhead doctor`",
            readme, sidecar
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn refuses_at_user_workspace_root() {
        let user_ws = TempDir::new().unwrap();
        let root = user_ws.path();
        assert!(nested_in_user_workspace(
            root,
            &root.join(".clearhead"),
            root
        ));
    }

    #[test]
    fn refuses_in_subdir_of_user_workspace_root() {
        let user_ws = TempDir::new().unwrap();
        let nested = user_ws.path().join("charters").join("sub");
        fs::create_dir_all(&nested).unwrap();
        assert!(nested_in_user_workspace(
            &nested,
            &nested.join(".clearhead"),
            user_ws.path()
        ));
    }

    #[test]
    fn allows_unrelated_project_dir() {
        let user_ws = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();
        assert!(!nested_in_user_workspace(
            project.path(),
            &project.path().join(".clearhead"),
            user_ws.path()
        ));
    }

    #[test]
    fn allows_rerun_when_clearhead_dir_already_exists() {
        let user_ws = TempDir::new().unwrap();
        let clearhead_dir = user_ws.path().join(".clearhead");
        fs::create_dir_all(&clearhead_dir).unwrap();
        assert!(!nested_in_user_workspace(
            user_ws.path(),
            &clearhead_dir,
            user_ws.path()
        ));
    }
}
