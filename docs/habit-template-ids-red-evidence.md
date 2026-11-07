# Named-template ID regression: retrospective red/green evidence

Review finding: `4#1` (review session `20261107-180153/4`).
Recorded on 2026-11-07 during the fix session.

The CLI reader acceptance tests were introduced with implementation commit
`73952d590e26224f8d1939695cc396861cffe877`, not run red before it. Their
`ParsedDocument` assertions do not compile against the previous reader's
`Vec<Action>` return type. This record does not claim otherwise or rewrite
that history. It uses the review's alternative: execute and record the Core
lint regression against the pre-implementation commit.

## Red: unchanged historical Core tests

A detached worktree at test-only commit
`6d2be367b9f6d6634a66a118dc816d2d436ab2d8` contains the old implementation
and `crates/clearhead-core/tests/habit_template_ids.rs`. No shim or source
changes were made there.

```sh
git worktree add --detach /tmp/habit-template-red.34gTGi \
  6d2be367b9f6d6634a66a118dc816d2d436ab2d8
cd /tmp/habit-template-red.34gTGi
cargo test -p clearhead_core --test habit_template_ids -- --nocapture
```

Observed output (test order normalized; thread ID omitted):

```text
running 3 tests
thread 'habit_template_ids_named_missing_step_is_reported_at_its_source' panicked at crates/clearhead-core/tests/habit_template_ids.rs:14:5:
assertion `left == right` failed
  left: 0
 right: 1
test habit_template_ids_written_steps_keep_identity_and_need_no_minting ... ok
test habit_template_ids_inline_children_are_still_reported ... ok
test habit_template_ids_named_missing_step_is_reported_at_its_source ... FAILED

test result: FAILED. 2 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

Exit status: **101**. All three named tests executed; the missing named-template
ID diagnostic assertion failed at runtime, rather than at compilation or test
discovery. The fixture expects one diagnostic at source row 1; the old lint
returned none.

## Green: current implementation

The historical worktree was removed. Because the sandbox shares
`CARGO_TARGET_DIR=/srv/cache/target`, an initial current-tree invocation reused
the historical test binary and reproduced its failure. After
`cargo clean -p clearhead_core`, both current-tree commands rebuilt and passed:

```sh
cargo test -p clearhead_core --test habit_template_ids -- --nocapture
cargo test -p clearhead --test habit_template_ids -- --nocapture
```

Each ran **3 tests: 3 passed, 0 failed, 0 ignored, 0 filtered out**. The Core
missing-step regression now passes, as do the CLI reader's missing-step source
records, written-ID round trip, and syntax-error rejection tests. No production
code or existing tests were changed for this evidence record. The complete
`scripts/gate.sh` result is recorded in this evidence commit's message.
