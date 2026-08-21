mod common;

#[cfg(unix)]
#[test]
fn start_lsp_execs_the_external_server_with_inherited_stdio() {
    use std::os::unix::fs::PermissionsExt;

    let env = common::TestEnv::new();
    let server = env.work_dir.join("clearhead-lsp-test");
    std::fs::write(&server, "#!/bin/sh\nprintf 'external-lsp-stdio\\n'\n").unwrap();
    let mut permissions = std::fs::metadata(&server).unwrap().permissions();
    permissions.set_mode(0o755);
    std::fs::set_permissions(&server, permissions).unwrap();

    env.command()
        .env("CLEARHEAD_LSP", &server)
        .arg("start")
        .arg("lsp")
        .assert()
        .success()
        .stdout("external-lsp-stdio\n");
}
