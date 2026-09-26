mod common;
use common::TestEnv;
use predicates::prelude::*;
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

/// Every file under `root`, relative path → bytes.
fn files(root: &Path) -> BTreeMap<String, Vec<u8>> {
    fn visit(root: &Path, dir: &Path, out: &mut BTreeMap<String, Vec<u8>>) {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                visit(root, &path, out);
            } else {
                let relative = path.strip_prefix(root).unwrap().display().to_string();
                out.insert(relative, fs::read(&path).unwrap());
            }
        }
    }
    let mut out = BTreeMap::new();
    visit(root, root, &mut out);
    out
}

#[test]
fn project_and_user_init_produce_the_same_root_shape() {
    let env = TestEnv::new();

    env.command().arg("init").assert().success();
    env.command().args(["init", "--user"]).assert().success();

    let mut project: Vec<String> = files(&env.work_dir.join(".clearhead"))
        .into_keys()
        .collect();
    project.retain(|path| path != ".gitignore");
    let user: Vec<String> = files(&env.data_dir).into_keys().collect();
    assert_eq!(project, user);
    assert_eq!(
        user,
        [
            "charters/.next.json",
            "charters/README.md",
            "charters/next.actions",
            "workspace.json"
        ]
    );
}

#[test]
fn rerunning_init_changes_nothing() {
    let env = TestEnv::new();
    env.command().arg("init").assert().success();
    let before = files(&env.work_dir.join(".clearhead"));

    env.command()
        .arg("init")
        .assert()
        .success()
        .stdout(predicate::str::contains("already initialized"));

    assert_eq!(before, files(&env.work_dir.join(".clearhead")));
}

#[test]
fn fresh_init_names_the_activation_command_for_its_new_root() {
    // The root starts New (specifications/workspace.md, The Root Charter),
    // which hides its Actions from engagement; `init` must say so and name
    // the exact command, not leave the reader to discover it via `doctor`.
    let env = TestEnv::new();

    let output = env.command().arg("init").assert().success();
    let readme = fs::read_to_string(env.work_dir.join(".clearhead/charters/README.md")).unwrap();
    let id = readme
        .lines()
        .find_map(|line| line.strip_prefix("id: "))
        .expect("init should assign a root ID");
    output.stdout(
        predicate::str::contains("is New").and(predicate::str::contains(format!(
            "clearhead update charter {id} --state active"
        ))),
    );
}

#[test]
fn user_init_uses_a_stable_selector_for_a_shell_unsafe_name() {
    let env = TestEnv::new();
    let output = env
        .command()
        .args(["init", "--user"])
        .env("USER", "Some User; $(echo unsafe)")
        .assert()
        .success();
    let readme = fs::read_to_string(env.data_dir.join("charters/README.md")).unwrap();
    let id = readme
        .lines()
        .find_map(|line| line.strip_prefix("id: "))
        .expect("init should assign a root ID");
    output.stdout(predicate::str::contains(format!(
        "clearhead update charter {id} --state active"
    )));
}

#[test]
fn rerunning_init_does_not_repeat_the_activation_reminder() {
    let env = TestEnv::new();
    env.command().arg("init").assert().success();

    env.command()
        .arg("init")
        .assert()
        .success()
        .stdout(predicate::str::contains("is New").not());
}

#[test]
fn init_mirrors_an_existing_readme_id_instead_of_minting() {
    let env = TestEnv::new();
    let charters = env.work_dir.join(".clearhead/charters");
    fs::create_dir_all(&charters).unwrap();
    let id = "019c4f48-6441-75dd-b285-33718b9be996";
    fs::write(
        charters.join("README.md"),
        format!("---\nid: {id}\nalias: platform\n---\n# Platform\n"),
    )
    .unwrap();

    env.command().arg("init").assert().success();

    let sidecar = fs::read_to_string(charters.join(".next.json")).unwrap();
    assert!(
        sidecar.contains(id),
        "sidecar must mirror the README id:\n{sidecar}"
    );
}

#[test]
fn persisted_name_survives_renaming_the_project_directory() {
    let env = TestEnv::new();
    let alpha = env.work_dir.join("alpha");
    fs::create_dir_all(&alpha).unwrap();
    env.command()
        .current_dir(&alpha)
        .arg("init")
        .assert()
        .success();

    let beta = env.work_dir.join("beta");
    fs::rename(&alpha, &beta).unwrap();

    env.command()
        .current_dir(&beta)
        .args(["show", "charter", "alpha"])
        .assert()
        .success()
        .stdout(predicate::str::contains("alias  alpha"));
}

#[test]
fn name_flag_sets_the_persisted_name_and_root_alias() {
    let env = TestEnv::new();

    env.command()
        .args(["init", "--user", "--name", "personal"])
        .assert()
        .success();

    let manifest = fs::read_to_string(env.data_dir.join("workspace.json")).unwrap();
    assert!(
        manifest.contains(r#""workspace_name": "personal""#),
        "{manifest}"
    );
    let readme = fs::read_to_string(env.data_dir.join("charters/README.md")).unwrap();
    assert!(readme.contains("alias: personal"), "{readme}");
}
