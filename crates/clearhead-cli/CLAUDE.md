# Project overview

See the crate [README](README.md) for user-facing behavior and the workspace
[Core architecture](../../docs/ARCHITECTURE.md) for implementation boundaries.

`clearhead-cli` is the synchronous command host. It owns command parsing,
presentation, invocation-scoped orchestration, and the optional in-process
SPARQL evaluator. Durable native loading and delivery belong to
`clearhead-workspace-fs`; semantic models, codecs, and mutation decisions belong
to `clearhead_core`. The standalone LSP is a sibling crate, not a CLI module.
