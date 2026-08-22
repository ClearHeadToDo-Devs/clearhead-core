//! Shared stdout writer for byte-oriented command output: a closed downstream
//! pipe (`clearhead … | head -n1`) is a clean exit, not an error.

use anyhow::Context as _;
use std::io::Write;

/// Write all bytes to stdout, treating a broken pipe as success.
pub fn write_stdout(bytes: &[u8]) -> anyhow::Result<()> {
    match std::io::stdout().lock().write_all(bytes) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == std::io::ErrorKind::BrokenPipe => Ok(()),
        Err(error) => Err(error).context("write stdout"),
    }
}

/// Write one line (value plus a trailing newline) to stdout.
pub fn write_stdout_line(value: &str) -> anyhow::Result<()> {
    let mut bytes = value.as_bytes().to_vec();
    bytes.push(b'\n');
    write_stdout(&bytes)
}
