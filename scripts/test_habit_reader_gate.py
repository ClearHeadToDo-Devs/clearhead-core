"""Regression coverage for comment-insensitive caller inventory."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("reader_gate", Path(__file__).with_name("habit-reader-gate.py"))
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class InventoryTests(unittest.TestCase):
    def test_comment_lines_do_not_change_inventory(self):
        with tempfile.TemporaryDirectory() as directory:
            old_root = gate.ROOT
            self.addCleanup(setattr, gate, "ROOT", old_root)
            gate.ROOT = Path(directory)
            source = gate.ROOT / "crates/example/src/lib.rs"
            source.parent.mkdir(parents=True)
            source.write_text('fn read() { read_actions("next.actions"); }\n')
            expected = gate.scan()
            self.assertEqual(list(expected.values()), [2])
            source.write_text(source.read_text() + '\n  // read_actions("next.actions")\n/// parse_actions\n//! .actions\n/*\n * read_actions .actions\n */\n')
            self.assertEqual(gate.scan(), expected)


if __name__ == "__main__":
    unittest.main()
