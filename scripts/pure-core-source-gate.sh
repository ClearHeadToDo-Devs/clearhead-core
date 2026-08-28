#!/bin/sh
# Reject production filesystem observation or delivery inside clearhead_core.
# Inline #[cfg(test)] modules are excluded; tests may use native fixtures.
set -eu

root="crates/clearhead-core/src"
pattern='std::fs::|tokio::fs::|\.is_dir\(|\.is_file\(|\.exists\(|\.canonicalize\(|read_dir\(|OpenOptions|File::open\(|File::create\('
failed=0

for file in $(find "$root" -type f -name '*.rs' | sort); do
	production=$(awk '/^#\[cfg\(test\)\]/{exit} {print}' "$file")
	hits=$(printf '%s\n' "$production" | grep -nE "$pattern" || true)
	if [ -n "$hits" ]; then
		if [ "$failed" -eq 0 ]; then
			echo "pure-core-source-gate > FAIL: native filesystem capability in Core production source:" >&2
		fi
		failed=1
		printf '%s\n' "$hits" | sed "s|^|$file:|" >&2
	fi
done

if [ "$failed" -ne 0 ]; then
	echo "pure-core-source-gate > move observation and delivery to clearhead-workspace-fs." >&2
	exit 1
fi

echo "pure-core-source-gate > OK: Core production source performs no filesystem I/O"
