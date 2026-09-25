---
id: af439c6c-83af-5ba4-aaa9-e15a76ee2230
---
# clearhead-core

## Log

- 2026-09-25T20:45+00:00 — Gate: pass (fmt/clippy/builds/wasm-gate/pure-core-gate/tests). doctor: 8 pre-existing charter-document-without-id warnings, no new ones. git log clearhead-core 023a17c..HEAD: 77bbdb5.

77bbdb5 Refuse ambiguous partial title matches when archiving a charter — read crates/clearhead-cli/src/filesystem/archive_charter.rs:658-676 first. Risk: low (pure logic fix + one new test in a fallback path already covered by existing tests; gate green).

No new dependencies. Reviewer (fresh Claude subagent — no other-vendor reviewer was available this run, recorded on the action) found no blocking issues; one non-blocking nit (candidate-list formatting duplicated a third time in the file) left unfixed as pre-existing and out of scope.

Superproject: db01035 bumps clearhead-core to 77bbdb5.
