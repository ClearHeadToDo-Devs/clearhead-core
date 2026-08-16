use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

use serde_json::{Value, json};
use tower_lsp_server::ls_types::Uri;

struct LspProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

impl LspProcess {
    fn spawn() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_clearhead-lsp"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("start clearhead-lsp");
        let stdin = child.stdin.take().expect("server stdin");
        let stdout = BufReader::new(child.stdout.take().expect("server stdout"));
        Self {
            child,
            stdin,
            stdout,
        }
    }

    fn send(&mut self, message: Value) {
        let body = serde_json::to_vec(&message).expect("serialize JSON-RPC message");
        write!(self.stdin, "Content-Length: {}\r\n\r\n", body.len()).unwrap();
        self.stdin.write_all(&body).unwrap();
        self.stdin.flush().unwrap();
    }

    fn receive(&mut self) -> Value {
        let mut content_length = None;
        loop {
            let mut line = String::new();
            self.stdout.read_line(&mut line).expect("read LSP header");
            assert!(!line.is_empty(), "server exited before sending a response");
            if line == "\r\n" || line == "\n" {
                break;
            }
            if let Some(value) = line.strip_prefix("Content-Length:") {
                content_length = Some(value.trim().parse::<usize>().unwrap());
            }
        }

        let mut body = vec![0; content_length.expect("Content-Length header")];
        self.stdout.read_exact(&mut body).expect("read LSP body");
        serde_json::from_slice(&body).expect("parse JSON-RPC response")
    }

    fn receive_until(&mut self, predicate: impl Fn(&Value) -> bool) -> Value {
        loop {
            let message = self.receive();
            if predicate(&message) {
                return message;
            }
        }
    }

    fn stop(mut self) {
        self.send(json!({"jsonrpc": "2.0", "id": 99, "method": "shutdown", "params": null}));
        let shutdown = self.receive_until(|message| message.get("id") == Some(&json!(99)));
        assert_eq!(shutdown.get("result"), Some(&Value::Null));
        self.send(json!({"jsonrpc": "2.0", "method": "exit", "params": null}));
        drop(self.stdin);
        assert!(self.child.wait().expect("wait for server").success());
    }
}

#[test]
fn stdio_lifecycle_diagnostics_formatting_and_save() {
    let temp = tempfile::tempdir().unwrap();
    let project = temp.path().join("project");
    let charters = project.join(".clearhead/charters");
    std::fs::create_dir_all(&charters).unwrap();
    let source = charters.join("next.actions");
    std::fs::write(&source, "[ ] First\n").unwrap();

    let root_uri = Uri::from_file_path(&project).unwrap().to_string();
    let source_uri = Uri::from_file_path(&source).unwrap().to_string();
    let mut lsp = LspProcess::spawn();

    lsp.send(json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "capabilities": {},
            "workspaceFolders": [{"uri": root_uri, "name": "project"}]
        }
    }));
    let initialize = lsp.receive_until(|message| message.get("id") == Some(&json!(1)));
    assert_eq!(
        initialize.pointer("/result/serverInfo/name"),
        Some(&json!("clearhead-lsp"))
    );
    assert_eq!(
        initialize.pointer("/result/capabilities/textDocumentSync"),
        Some(&json!(1))
    );
    assert_eq!(
        initialize.pointer("/result/capabilities/documentFormattingProvider"),
        Some(&json!(true))
    );
    lsp.send(json!({"jsonrpc": "2.0", "method": "initialized", "params": {}}));

    lsp.send(json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
            "textDocument": {
                "uri": source_uri,
                "languageId": "actions",
                "version": 1,
                "text": "[ ] First"
            }
        }
    }));
    let diagnostics = lsp.receive_until(|message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
    });
    assert!(
        diagnostics["params"]["diagnostics"]
            .as_array()
            .is_some_and(|items| !items.is_empty()),
        "missing UUID should produce a diagnostic: {diagnostics}"
    );

    let saved_text = "[ ] First #019f733d-4612-7770-af8f-c6e1da5214bb";
    lsp.send(json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": source_uri, "version": 2},
            "contentChanges": [{"text": saved_text}]
        }
    }));
    let changed_diagnostics = lsp.receive_until(|message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
    });
    assert_eq!(
        changed_diagnostics["params"]["diagnostics"],
        json!([]),
        "persisted UUID should clear diagnostics"
    );

    std::fs::write(&source, format!("{saved_text}\n")).unwrap();
    lsp.send(json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didSave",
        "params": {"textDocument": {"uri": source_uri}}
    }));

    lsp.send(json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "textDocument/formatting",
        "params": {
            "textDocument": {"uri": source_uri},
            "options": {"tabSize": 4, "insertSpaces": true}
        }
    }));
    let formatting = lsp.receive_until(|message| message.get("id") == Some(&json!(2)));
    assert!(
        formatting["result"]
            .as_array()
            .is_some_and(|edits| !edits.is_empty()),
        "formatting should return a text edit: {formatting}"
    );

    let sidecar = clearhead_core::workspace::sidecar::sidecar_path(&source);
    assert!(sidecar.exists(), "didSave should stamp the action sidecar");
    assert!(
        !clearhead_core::completed_actions_path(&source).exists(),
        "didSave must not archive an editor-owned buffer"
    );

    // Historical destructive shape: a half-typed link lets generic recovery
    // span into the next action. Diagnostics remain available, but formatting
    // must return no full-document edit while that recovery evidence exists.
    let malformed = concat!(
        "[ ] Read [[docs|https://example.com\n",
        "[ ] Next #019f0000-0000-7000-8000-000000000001",
    );
    lsp.send(json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": source_uri, "version": 3},
            "contentChanges": [{"text": malformed}]
        }
    }));
    let malformed_diagnostics = lsp.receive_until(|message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
    });
    assert!(
        malformed_diagnostics["params"]["diagnostics"]
            .as_array()
            .is_some_and(|items| items.iter().any(|item| item["severity"] == json!(1))),
        "incomplete link should produce an error diagnostic: {malformed_diagnostics}"
    );

    lsp.send(json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didSave",
        "params": {"textDocument": {"uri": source_uri}}
    }));
    lsp.send(json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "textDocument/formatting",
        "params": {
            "textDocument": {"uri": source_uri},
            "options": {"tabSize": 4, "insertSpaces": true}
        }
    }));
    let refused = lsp.receive_until(|message| message.get("id") == Some(&json!(3)));
    assert_eq!(
        refused["result"],
        Value::Null,
        "untrusted source must not receive a formatting edit: {refused}"
    );
    let sidecar_content = std::fs::read_to_string(&sidecar).unwrap();
    assert!(
        !sidecar_content.contains("019f0000-0000-7000-8000-000000000001"),
        "didSave must not persist UUID attachment from recovered source: {sidecar_content}"
    );

    // didClose releases document state and clears any lingering diagnostics: the
    // server no longer owns a closed file's truth.
    lsp.send(json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {"textDocument": {"uri": source_uri}}
    }));
    let closed_diagnostics = lsp.receive_until(|message| {
        message.get("method") == Some(&json!("textDocument/publishDiagnostics"))
    });
    assert_eq!(
        closed_diagnostics["params"]["diagnostics"],
        json!([]),
        "didClose must clear diagnostics for the closed document: {closed_diagnostics}"
    );

    lsp.stop();
}
