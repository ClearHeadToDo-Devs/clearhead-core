---
id: 019e5139-ed68-78e2-99d5-517c59cd1ad1
alias: workspace-rework
---
# Workspace Layer Rework

Establish a clean three-layer architecture: `FileSystemWorkspace → DomainModel → Graph`.

The workspace layer owns all file-specific types (`ActionsFile`, `ICSPlan`, `MarkdownCharter`) and converts them to pure domain types at the boundary. The domain module is the stable contract between storage and consumers (graph, LSP, CLI). No file paths, source ranges, or sidecar metadata should leak into domain types.
