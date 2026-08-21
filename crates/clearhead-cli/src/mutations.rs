//! Field-update appliers now live in clearhead-core's domain layer
//! (`clearhead_core::domain::update`), relocated by the durable-verbs charter
//! so the CLI, LSP, and graphd share one home for field semantics.
//!
//! This module is a thin re-export kept so existing `clearhead_cli::mutations::*`
//! call sites keep compiling; it is removed once the `add`/`update`/`delete`
//! verbs route through core's locked mutation seam (charter done gate).

pub use clearhead_core::{ActionUpdate, CharterUpdate, apply_charter_update, apply_updates};
