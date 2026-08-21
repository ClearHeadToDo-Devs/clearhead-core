//! Out-of-process bridge to `clearhead-graphd`.
//!
//! JSON-LD export is the CLI's one graphd bridge: a plain domain model goes
//! in and canonical JSON-LD comes back. Query execution is intentionally not
//! proxied here; consumers call graphd's public interface directly.

use std::io::Write;
use std::process::{Command, Stdio};

const GRAPHD_ENV: &str = "CLEARHEAD_GRAPHD";

/// Locate the graphd executable — the JSON-LD export bridge and the query
/// facade both spawn it (the facade forwards `query …` with inherited stdio).
pub fn graphd_command() -> Command {
    let executable = std::env::var_os(GRAPHD_ENV).unwrap_or_else(|| "clearhead-graphd".into());
    Command::new(executable)
}

/// Run graphd with `args`, piping `payload` to its stdin and capturing stdout.
fn invoke_graphd(args: &[&std::ffi::OsStr], payload: &[u8]) -> Result<Vec<u8>, String> {
    let mut command = graphd_command();
    command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = command.spawn().map_err(|e| {
        format!("Failed to start clearhead-graphd: {e}. Install it or set {GRAPHD_ENV}")
    })?;
    child
        .stdin
        .take()
        .ok_or_else(|| "Failed to open graphd stdin".to_string())?
        .write_all(payload)
        .map_err(|e| format!("Failed to write graphd request: {e}"))?;

    let output = child
        .wait_with_output()
        .map_err(|e| format!("Failed to wait for graphd: {e}"))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "clearhead-graphd exited with {}{}",
            output.status,
            if stderr.trim().is_empty() {
                String::new()
            } else {
                format!(": {}", stderr.trim())
            }
        ));
    }
    Ok(output.stdout)
}

/// Ask graphd to turn a plain JSON domain model into canonical JSON-LD.
pub fn serialize_domain_to_jsonld(model: &clearhead_core::DomainModel) -> Result<String, String> {
    let payload = serde_json::to_vec(model)
        .map_err(|e| format!("Failed to serialize domain model as JSON: {e}"))?;
    let args = [std::ffi::OsStr::new("export-jsonld")];
    let stdout = invoke_graphd(&args, &payload)?;
    String::from_utf8(stdout)
        .map_err(|e| format!("clearhead-graphd returned non-UTF-8 JSON-LD: {e}"))
}
