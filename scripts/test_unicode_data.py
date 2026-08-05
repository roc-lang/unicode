#!/usr/bin/env python3

from __future__ import annotations

import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

from scripts import unicode_data  # noqa: E402


class UnicodeDataTests(unittest.TestCase):
    def test_repository_data_is_valid_and_generation_is_deterministic(self) -> None:
        manifest = unicode_data.load_manifest()
        unicode_data.validate_all(manifest)
        self.assertEqual(
            unicode_data.rendered_modules(manifest), unicode_data.rendered_modules(manifest)
        )

    def test_manifest_rejects_unknown_schema(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text('{"schema_version": 2}', encoding="utf-8")
            with self.assertRaisesRegex(unicode_data.DataError, "schema_version"):
                unicode_data.load_manifest(path)

    def test_manifest_rejects_version_path_drift(self) -> None:
        manifest = json.loads(unicode_data.MANIFEST_PATH.read_text(encoding="utf-8"))
        manifest["unicode_version"] = "16.0.0"
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text(json.dumps(manifest), encoding="utf-8")
            with self.assertRaisesRegex(unicode_data.DataError, "does not match unicode_version"):
                unicode_data.load_manifest(path)

    def test_source_rejects_hash_header_and_count_drift(self) -> None:
        content = "# Header\n0041 ; A\n"
        digest = hashlib.sha256(content.encode()).hexdigest()
        base_entry = {
            "path": "fixture.txt",
            "url": "https://www.unicode.org/Public/fixture.txt",
            "sha256": digest,
            "header": "# Header",
            "records": 1,
            "role": "test",
        }
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            vendor_root = root / "vendor" / "unicode"
            vendor_root.mkdir(parents=True)
            # This fixture validates its byte hash, so avoid platform newline
            # translation when writing it on Windows.
            (vendor_root / "fixture.txt").write_bytes(content.encode("utf-8"))
            for field, bad_value, message in (
                ("sha256", "0" * 64, "SHA-256 mismatch"),
                ("header", "# Wrong", "missing header marker"),
                ("records", 2, "record-count drift"),
            ):
                entry = dict(base_entry)
                entry[field] = bad_value
                manifest = {"files": {"fixture": entry}}
                with (
                    self.subTest(field=field),
                    mock.patch.object(unicode_data, "ROOT", root),
                    mock.patch.object(unicode_data, "UNICODE_VENDOR_ROOT", vendor_root),
                ):
                    with self.assertRaisesRegex(unicode_data.DataError, message):
                        unicode_data.verify_source(manifest, "fixture")

    def test_range_parser_is_strict(self) -> None:
        valid_prefix = "# @missing: default\n"
        failures = (
            ("0041 A\n", "malformed"),
            ("0041 ; Unknown\n", "unknown property"),
            ("0042..0041 ; A\n", "reversed range"),
            ("0041..0043 ; A\n0043..0044 ; A\n", "overlaps"),
            ("0041 ; A\n", "missing required default"),
        )
        for body, message in failures:
            text = body if message.startswith("missing") else valid_prefix + body
            with self.subTest(message=message):
                with self.assertRaisesRegex(unicode_data.DataError, message):
                    unicode_data.parse_ranges(
                        text,
                        source="fixture.txt",
                        allowed_properties=("A",),
                        default_marker="# @missing: default",
                    )

    def test_grapheme_parser_rejects_malformed_cases(self) -> None:
        manifest = json.loads(json.dumps(unicode_data.load_manifest()))
        manifest["files"]["grapheme_break_test"]["cases"] = 1
        with self.assertRaisesRegex(unicode_data.DataError, "boundary marker"):
            unicode_data.parse_grapheme_tests(
                manifest,
                "÷ 0041 ? 0042 ÷ # ÷ [0.2] A ? [999.0] B ÷ [0.3]\n",
            )

    def test_skip_ledger_has_exactly_the_known_unsupported_cases(self) -> None:
        manifest = unicode_data.load_manifest()
        cases = unicode_data.parse_grapheme_tests(manifest)
        skipped = unicode_data.validate_skip_ledger(cases)
        self.assertEqual(len(skipped), 125)
        self.assertEqual(len(cases) - len(skipped), 1062)


if __name__ == "__main__":
    unittest.main()
