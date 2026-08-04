#![allow(dead_code)]

use assert_cmd::Command;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Isolated workspace and XDG environment for graphd process tests.
pub struct TestEnv {
    pub _temp_dir: TempDir,
    pub config_dir: PathBuf,
    pub data_dir: PathBuf,
    pub state_dir: PathBuf,
    pub work_dir: PathBuf,
}

impl TestEnv {
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("create temp dir");
        let config_dir = temp_dir.path().join("config/clearhead");
        let data_dir = temp_dir.path().join("workspace");
        let state_dir = temp_dir.path().join("state/clearhead");
        let work_dir = temp_dir.path().join("work");
        for dir in [&config_dir, &data_dir, &state_dir, &work_dir] {
            fs::create_dir_all(dir).expect("create test directory");
        }
        Self {
            _temp_dir: temp_dir,
            config_dir,
            data_dir,
            state_dir,
            work_dir,
        }
    }

    pub fn with_workspace_identity(&self) -> &Self {
        let clearhead = self.data_dir.join(".clearhead");
        fs::create_dir_all(clearhead.join("charters")).expect("create charter root");
        fs::write(
            clearhead.join("workspace.json"),
            r#"{"workspace_id":"test-workspace","workspace_name":"test"}"#,
        )
        .expect("write workspace identity");
        self
    }

    pub fn write_actions(&self, filename: &str, content: &str) {
        let path = self.data_dir.join(".clearhead/charters").join(filename);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create actions parent");
        }
        fs::write(path, content).expect("write actions");
    }

    pub fn write_text(&self, relative_path: &str, content: &str) {
        let path = self.data_dir.join(relative_path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create parent");
        }
        fs::write(path, content).expect("write text");
    }

    pub fn command(&self) -> Command {
        let mut command = Command::cargo_bin("clearhead-graphd").expect("graphd binary");
        command
            .env("XDG_CONFIG_HOME", self.config_dir.parent().unwrap())
            .env("XDG_DATA_HOME", self._temp_dir.path().join("data"))
            .env("XDG_STATE_HOME", &self.state_dir)
            .current_dir(&self.work_dir)
            .args(["--workspace", self.data_dir.to_str().unwrap()]);
        command
    }
}
