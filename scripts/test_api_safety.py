"""Regression checks for inventory discovery and production invariant scanning."""

from pathlib import Path
from tempfile import TemporaryDirectory
import unittest

from scripts.check_api_safety import operations, panic_sites


class SafetyAuditTest(unittest.TestCase):
    def test_associated_members_and_private_helpers(self):
        source = """Example :: [].{
    Cursor :: { at : U64 }.{
        push = |c| {
            local = 0
            { c, local }
        }
    }
    Flag := [A, B].{
        is_eq : _
    }
    total = |x| x
}
private = |x| x
"""
        self.assertEqual(
            operations("Example", source),
            {"Example.Cursor.push", "Example.Flag.is_eq", "Example.total"},
        )

    def test_multiline_public_nominal_record(self):
        source = """Example :: {
    value : Str,
}.{
    public = |example| example.value
}
private = |value| value
"""
        self.assertEqual(operations("Example", source), {"Example.public"})

    def test_literals_comments_and_interpolation(self):
        source = r'''# crash and ...
doc = "the word crash and literal ellipsis ..."
escaped = "literal \${crash\u007d"
multiline = """quoted crash
..."""
panic = |flag| if flag { ... } else { "caught ${crash "interpolated"}" }
fallback = |value| value ??
    ...
multiplication = |value| value.times_wrap(23)
internal_from_u32_unchecked = |value| value
'''
        with TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "Example.roc").write_text(source)
            found = [line for _, line in panic_sites(root)]
        self.assertEqual(
            found,
            [
                'panic = |flag| if flag { ... } else { "caught ${crash "interpolated"}" }',
                "...",
                "multiplication = |value| value.times_wrap(23)",
                "internal_from_u32_unchecked = |value| value",
            ],
        )
