//! Generate a structural skeleton `clearhead.1` from the clap `Cli` definition.
//!
//! The canonical man page at `man/clearhead.1` is **hand-authored** and should
//! not be overwritten by this tool.  Run this only when you need to regenerate
//! the skeleton after adding new subcommands, then merge the changes into the
//! hand-authored file by hand.
//!
//! Usage:
//!   cargo run --example gen_man                     # writes to man/clearhead.1.generated
//!   cargo run --example gen_man -- /custom/path     # writes to a custom directory

use clap::CommandFactory;
use clap_mangen::Man;
use clearhead_cli::argparser::Cli;
use std::{fs, path::PathBuf};

fn main() {
    let out_dir: PathBuf = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("man"));

    fs::create_dir_all(&out_dir).expect("failed to create output directory");

    let cmd = Cli::command().name("clearhead");
    let man = Man::new(cmd);

    let mut buf = Vec::new();
    man.render(&mut buf).expect("failed to render man page");

    let out_path = out_dir.join("clearhead.1.generated");
    fs::write(&out_path, buf).expect("failed to write man page");

    println!("Skeleton written to {}", out_path.display());
    println!("NOTE: man/clearhead.1 is hand-authored. Merge changes manually.");
}
