#!/usr/bin/env python3
"""Guard the audited reader surface; additions require an explicit inventory update.

This is a caller-inventory guard, not a substitute for habit_reader_conformance.
Counts deliberately include inline tests/comments: false positives require review,
whereas a newly introduced reader must not silently escape the inventory.
"""
import argparse
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parent.parent
INVENTORY = ROOT / "docs/habit-reader-callers.txt"
TOKENS = re.compile(r"\.actions|\b(?:parse_actions|parse_document|parse_tree|read_actions|read_template_actions|classify_action_file|is_live_actions|is_charter_action_file|is_action_history|require_action_resource|load_file_for_read|load_file_for_mutation|parse_content_for_read|parse_content_for_rewrite|parse_content_for_mutation|from_actions_with_charter|ActionResourceState|FileState|SourcedAction|ActionsFile|get_parser)\b")


def scan():
    result = {}
    for path in (ROOT / "crates").rglob("*.rs"):
        relative = path.relative_to(ROOT)
        if "src" not in relative.parts:
            continue
        count = len(TOKENS.findall(path.read_text()))
        if count:
            result[relative.as_posix()] = count
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--update", action="store_true", help="record a deliberately audited caller change")
    args = parser.parse_args()
    actual = scan()
    if args.update:
        INVENTORY.write_text("# Audited surface: docs/habit-readers.md; count then source path.\n" +
                             "".join(f"{actual[name]} {name}\n" for name in sorted(actual)))
        return 0
    expected = {}
    for line in INVENTORY.read_text().splitlines():
        if not line or line.startswith("#"):
            continue
        count, name = line.split(maxsplit=1)
        expected[name] = int(count)
    differences = [name for name in sorted(actual.keys() | expected.keys()) if actual.get(name) != expected.get(name)]
    for name in differences:
        print(f"reader inventory changed: {name}: {expected.get(name, 0)} -> {actual.get(name, 0)}", file=sys.stderr)
    if differences:
        print("Audit semantic file roles, add conformance coverage, then explicitly update the inventory.", file=sys.stderr)
        return 1
    print(f"habit-reader-gate > {len(actual)} audited caller files unchanged")
    return 0


if __name__ == "__main__":
    sys.exit(main())
