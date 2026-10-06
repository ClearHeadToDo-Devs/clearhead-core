//! Expose the specification release this crate implements, declared once as
//! `[package.metadata.clearhead] specification` in Cargo.toml, to the crate as
//! `CLEARHEAD_SPECIFICATION`. The declaration ships with the published crate.
fn main() {
    println!("cargo::rerun-if-changed=Cargo.toml");
    let manifest: toml::Table = std::fs::read_to_string("Cargo.toml")
        .expect("read Cargo.toml")
        .parse()
        .expect("parse Cargo.toml");
    let release = manifest
        .get("package")
        .and_then(|p| p.get("metadata"))
        .and_then(|m| m.get("clearhead"))
        .and_then(|c| c.get("specification"))
        .and_then(|s| s.as_str())
        .expect("Cargo.toml declares [package.metadata.clearhead] specification");
    println!("cargo::rustc-env=CLEARHEAD_SPECIFICATION={release}");
}
