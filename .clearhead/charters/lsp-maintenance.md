---
id: 019f738f-7c3e-7401-a189-8d7b9684f6a4
alias: lsp-maintenance
---
# LSP Runtime Maintenance

The extraction established the correct ownership boundary and preserved behavior, but the new long-running process now needs the ordinary maintenance surfaces of an independent release.

## Findings

- the public repository only builds inside the platform sibling layout because both core and the parser are path dependencies
- document entries and diagnostics survive `didClose`, retaining stale state for the lifetime of the server
- workspace folders are captured once during initialization and cannot follow client folder changes
- the repository has strong local tests but no independent CI workflow or checked-in license text
- the CLI and LSP own almost identical NDJSON file emitters; this is tolerable for two adapters but should be an explicit decision before it spreads

## Done gate

- a standalone clone can build and test using documented dependency sources
- document and workspace routing state follows the corresponding LSP lifecycle
- CI enforces formatting, Clippy, unit tests, and stdio protocol tests
- release metadata and license artifacts are complete
- telemetry adapter duplication is either intentionally documented or removed
