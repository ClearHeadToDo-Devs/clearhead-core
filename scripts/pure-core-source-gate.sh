#!/bin/sh
#
# Pure-core source gate (invariant I1, docs/DECISIONS.md Decision 38).
#
# clearhead_core must not observe or deliver: no clock, RNG, filesystem probe
# or environment read in production source. Ids, times and bytes enter as
# arguments. The wasm dependency gate checks crates; this checks call sites.
#
# It counts occurrences of each forbidden token per file (comments and
# `#[cfg(test)] mod` sections excluded) and requires the counts to match
# scripts/pure-core-allowlist.txt EXACTLY. So the list can only shrink: adding a
# call fails until the allowlist is edited in the same diff, where a reviewer
# sees it, and removing one fails until the allowlist is tightened.
#
#   sh scripts/pure-core-source-gate.sh           check
#   sh scripts/pure-core-source-gate.sh --update  rewrite the counts, keeping comments
#
# PURE_CORE_SRC and PURE_CORE_ALLOWLIST override the paths (used by the tests).
set -eu

cd "$(dirname "$0")/.."
src=${PURE_CORE_SRC:-crates/clearhead-core/src}
allow=${PURE_CORE_ALLOWLIST:-scripts/pure-core-allowlist.txt}

# Fixed strings, one per line: clock and RNG, then filesystem and environment.
tokens='now_v7
new_v4
SystemTime::now
Instant::now
Local::now
Utc::now
rand::
std::fs
fs::read
fs::write
File::open
.exists()
.is_dir()
.is_file()
env::var'

# Production lines of one file: drop comment lines, and everything from a
# `#[cfg(test)]` that introduces a `mod` to the end of the file (tests live at
# the bottom; a lone `#[cfg(test)] fn` does not end the production section).
production_lines() {
  awk '
    pending && /^[[:space:]]*$/ { next }
    pending { if ($0 ~ /^[[:space:]]*(pub(\([a-z]+\))? )?mod[[:space:]]/) exit; pending = 0 }
    /^[[:space:]]*#\[cfg\(test\)\]/ { pending = 1; next }
    /^[[:space:]]*\/\// { next }
    { print }
  ' "$1"
}

counts() {
  find "$src" -name '*.rs' | sort | while read -r file; do
    lines=$(production_lines "$file")
    printf '%s\n' "$tokens" | while read -r token; do
      n=$(printf '%s\n' "$lines" | grep -cF -- "$token" || true)
      if [ "$n" -gt 0 ]; then printf '%s %s %s\n' "${file#"$src"/}" "$token" "$n"; fi
    done
  done
}

actual=$(counts)

if [ "${1:-}" = "--update" ]; then
  {
    grep '^#' "$allow" 2>/dev/null || true
    printf '%s\n' "$actual"
  } >"$allow.new" && mv "$allow.new" "$allow"
  echo "pure-core-gate > rewrote $allow ($(printf '%s\n' "$actual" | grep -c . || true) entries)"
  exit 0
fi

expected=$(grep -v '^#' "$allow" | grep -v '^[[:space:]]*$' | sort || true)
actual=$(printf '%s\n' "$actual" | grep -v '^[[:space:]]*$' | sort || true)

if [ "$expected" != "$actual" ]; then
  tmp=$(mktemp -d)
  printf '%s\n' "$expected" >"$tmp/allowed"
  printf '%s\n' "$actual" >"$tmp/actual"
  echo "pure-core-gate > FAIL: Core's production source and $allow disagree." >&2
  echo "pure-core-gate > '<' is allowed, '>' is actual (path token count):" >&2
  diff "$tmp/allowed" "$tmp/actual" >&2 || true
  rm -rf "$tmp"
  echo "pure-core-gate > a new call means Core observed or delivered: move it behind the host adapter." >&2
  echo "pure-core-gate > a removed call means tighten the allowlist in the same diff." >&2
  exit 1
fi

echo "pure-core-gate > OK: $(printf '%s\n' "$actual" | grep -c . || true) allowlisted site(s), no others"
