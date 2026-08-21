#!/bin/sh
#
# WASM portability dependency gate for clearhead_core (pure-core-split charter).
#
# Core is a pure domain library that must remain buildable for a WebAssembly
# host, so its portable dependency graph may not pull any native-only
# capability: filesystem locking/staging, temp files, user-directory lookup,
# process-config loading, or an async runtime. This gate resolves Core's
# dependency tree *as the wasm32 target sees it* and fails if any forbidden
# crate appears.
#
# It is a dependency gate, not a full compile: it does not require a wasm libc
# sysroot (the tree-sitter grammar's C parser still needs one for an actual
# wasm build — tracked separately toward the charter's compile-level done gate).
# `cargo tree` only resolves the graph, so it runs anywhere cargo does.
set -eu

# Crates that only exist to touch a native host. If Core ever pulls one of these
# into its portable (no-default-features) graph again, portability has regressed.
# `cargo tree --prefix none` prints one `name vX.Y.Z` per line, so anchoring the
# crate name at the start of the line keeps the match exact (no `directories` or
# `config-*` false positives).
FORBIDDEN='^(fs2|tempfile|dirs|shellexpand|tokio|mio|async-std|config) v'

echo "wasm-gate > resolving clearhead_core dependency graph for wasm32-unknown-unknown"
tree=$(cargo tree \
  --target wasm32-unknown-unknown \
  -p clearhead_core \
  --no-default-features \
  --edges normal \
  --prefix none 2>/dev/null | sort -u)

hits=$(printf '%s\n' "$tree" | grep -iE "$FORBIDDEN" || true)

if [ -n "$hits" ]; then
  echo "wasm-gate > FAIL: native-only crate(s) in Core's portable graph:" >&2
  printf '%s\n' "$hits" >&2
  echo "wasm-gate > move the host capability behind the native adapter (clearhead-workspace-fs)." >&2
  exit 1
fi

echo "wasm-gate > OK: Core's wasm32 dependency graph is free of native-only crates"
