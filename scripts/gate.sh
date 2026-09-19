#!/bin/sh
#
# The one-command gate for the clearhead-rs workspace: what CI runs and what the
# pre-push hook enforces, quiet on success and short on failure.
#
#   sh scripts/gate.sh          run every check, report all failures
#   sh scripts/gate.sh --fast   skip the test suite (fmt, clippy, checks, gates)
#
# Each step prints one line. A failing step prints only its failure lines
# (failed tests, panics, errors), not the whole log. Exit status is 1 if any
# step failed. Do not re-type these steps by hand: the order and flags here are
# the single definition, and .githooks/pre-push calls this script.
set -u

cd "$(dirname "$0")/.."
fast=0
[ "${1:-}" = "--fast" ] && fast=1
failed=0

# A short log prints whole; a long one is cut to its failure lines (failed
# tests, panics, compiler errors), or its tail if it has none.
summarize() {
  if [ "$(printf '%s\n' "$1" | wc -l)" -le 30 ]; then printf '%s\n' "$1"; return; fi
  hits=$(printf '%s\n' "$1" | grep -E 'FAILED|panicked at|^error|^Diff in' | head -30)
  if [ -n "$hits" ]; then printf '%s\n' "$hits"; else printf '%s\n' "$1" | tail -n 25; fi
}

step() {
  name=$1
  shift
  started=$(date +%s)
  if out=$("$@" 2>&1); then
    printf 'gate > ok    %-44s %ss\n' "$name" "$(($(date +%s) - started))"
  else
    printf 'gate > FAIL  %s\n' "$name"
    summarize "$out" | sed 's/^/         /'
    failed=1
  fi
}

# Oxigraph may only enter through the CLI's optional `sparql` feature.
no_oxigraph_in_minimal_cli() {
  if cargo tree -p clearhead_cli --no-default-features -e normal --prefix none | grep -q '^oxigraph '; then
    echo "FAIL: oxigraph appears in the minimal CLI dependency graph"
    return 1
  fi
}

step "fmt"                              cargo fmt --all --check
step "clippy"                           cargo clippy --workspace --all-targets --no-deps -- -D warnings
step "core builds with no default features" cargo check -p clearhead_core --no-default-features
step "cli builds with no default features"  cargo check -p clearhead_cli --no-default-features
step "no oxigraph in the minimal cli"   no_oxigraph_in_minimal_cli
step "wasm dependency gate"             sh scripts/wasm-dependency-gate.sh
step "pure-core source gate"            sh scripts/pure-core-source-gate.sh
[ "$fast" -eq 0 ] && step "tests"       cargo test --workspace --quiet

if [ "$failed" -ne 0 ]; then
  echo "gate > FAILED"
  exit 1
fi
echo "gate > all checks passed"
