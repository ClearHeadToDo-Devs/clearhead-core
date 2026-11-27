# Reviewing adapter behavior with snapshots

The adapter snapshot runs the real `clearhead` binary in a disposable workspace populated from a specification example. It records the exit code, stdout, stderr, and every added, removed, or changed workspace entry with its before and after bytes. Read the snapshot diff alongside the intent and the existing invariant tests; don't accept a new snapshot just because the command ran.

From the Core checkout, run:

```sh
CLEARHEAD_SPEC_DIR=/path/to/pinned/specifications \
  INSTA_UPDATE=no cargo test --locked -p clearhead --test adapter_snapshots
```

The pilot uses the published weekly Habit bytes under `templates/weekly-review.actions`, where recurrence is invalid. Comparing Core `516fc18` to `e3fd131` with those same bytes, `lint file` changed from a scope warning and exit 0 to E002 and exit 1. The ID notices stayed the same and neither run wrote a file.

To compare a different revision against the accepted snapshot, build that revision in a separate worktree and set `CLEARHEAD_SNAPSHOT_BIN` to the binary's **absolute path** in the command above. This override is only for this test. Use `cargo insta review` to inspect proposed updates for changes you intend to keep.

Keep fixture values visible. Prefer a deterministic invocation (relative paths, no tracing timestamps) before adding redactions, and redact only generated values that would otherwise fluctuate between runs.
