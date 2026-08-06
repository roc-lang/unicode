from __future__ import annotations

import re
import tempfile
import unittest
import urllib.request
from pathlib import Path

from scripts import test_bundle_examples as harness


class ExampleHarnessTests(unittest.TestCase):
    def test_real_spec_is_valid(self) -> None:
        self.assertTrue(harness.load_spec())

    def test_spec_coverage_is_semantic_not_count_based(self) -> None:
        path = "examples/example.roc"
        spec = {
            "schema_version": harness.SCHEMA_VERSION,
            "apps": [
                {
                    "path": path,
                    "cases": [
                        {"name": "works", "stdout": "ok\n"},
                        {
                            "name": "fails",
                            "exit_code": 1,
                            "stdout": "",
                            "stderr": "error\n",
                        },
                    ],
                }
            ],
        }

        self.assertEqual(harness.validate_spec(spec, {path}), spec["apps"])

    def test_spec_requires_success_and_error_paths(self) -> None:
        path = "examples/example.roc"
        base = {"schema_version": harness.SCHEMA_VERSION}
        success_only = {
            **base,
            "apps": [
                {"path": path, "cases": [{"name": "works", "stdout": "ok\n"}]}
            ],
        }
        error_only = {
            **base,
            "apps": [
                {
                    "path": path,
                    "cases": [
                        {
                            "name": "fails",
                            "exit_code": 1,
                            "stdout": "",
                            "stderr": "error\n",
                        }
                    ],
                }
            ],
        }

        with self.assertRaisesRegex(harness.TestFailure, "error path"):
            harness.validate_spec(success_only, {path})
        with self.assertRaisesRegex(harness.TestFailure, "successful path"):
            harness.validate_spec(error_only, {path})

    def test_case_validation_rejects_unknown_fields(self) -> None:
        with self.assertRaisesRegex(harness.TestFailure, "unknown fields"):
            harness.validate_case(
                "examples/example.roc",
                {"name": "typo", "stduot": "ignored"},
            )

    def test_case_validation_requires_an_assertion(self) -> None:
        with self.assertRaisesRegex(harness.TestFailure, "output assertion"):
            harness.validate_case("examples/example.roc", {"name": "silent"})

    def test_case_validation_rejects_invalid_regex(self) -> None:
        with self.assertRaisesRegex(harness.TestFailure, "invalid stdout_regex"):
            harness.validate_case(
                "examples/example.roc",
                {"name": "regex", "stdout_regex": ["("]},
            )

    def test_case_validation_requires_exact_process_output(self) -> None:
        with self.assertRaisesRegex(harness.TestFailure, "exact stdout"):
            harness.validate_case(
                "examples/example.roc",
                {"name": "partial", "stdout_contains": ["ok"]},
            )
        with self.assertRaisesRegex(harness.TestFailure, "exact stderr"):
            harness.validate_case(
                "examples/example.roc",
                {"name": "error", "exit_code": 1, "stdout": ""},
            )

    def test_output_normalization_is_text_only(self) -> None:
        raw = "alpha\r\n\x1b[31mbeta\x1b[0m\r"
        self.assertEqual(harness.normalize_output(raw), "alpha\nbeta\n")
        self.assertIn("\x1b[31m", raw)

    def test_rewrite_uses_copies_and_every_example_uses_bundle(self) -> None:
        originals = {
            path.name: path.read_bytes()
            for path in (harness.ROOT / "examples").glob("*.roc")
        }
        with tempfile.TemporaryDirectory() as temporary:
            rewritten = harness.copy_examples_with_bundle_url(
                Path(temporary), "http://127.0.0.1:12345/unicode.tar.zst"
            )
            self.assertEqual(set(rewritten), {f"examples/{name}" for name in originals})
            for source in rewritten.values():
                contents = source.read_text(encoding="utf-8")
                self.assertIn("http://127.0.0.1:12345/unicode.tar.zst", contents)
                self.assertNotIn(harness.LOCAL_PACKAGE_PATH, contents)
                self.assertNotIn("\r\n", contents)
        for name, contents in originals.items():
            self.assertEqual((harness.ROOT / "examples" / name).read_bytes(), contents)

    def test_bundle_server_serves_exact_archive_and_counts_gets(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            bundle = Path(temporary) / "unicode.tar.zst"
            bundle.write_bytes(b"bundle-fixture")
            with harness.BundleServer(bundle) as server:
                self.assertEqual(server.get_requests, 0)
                with urllib.request.urlopen(server.url, timeout=1) as response:
                    self.assertEqual(response.read(), b"bundle-fixture")
                self.assertEqual(server.get_requests, 1)

    def test_roc_environment_isolates_both_compiler_and_package_caches(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            cache = Path(temporary) / "cache"
            env = harness.isolated_roc_environment(cache)
            expected = str(cache.resolve())
            self.assertEqual(env["ROC_CACHE_DIR"], expected)
            self.assertEqual(env["XDG_CACHE_HOME"], expected)

    def test_dependency_pattern_only_matches_unicode_field(self) -> None:
        source = '    unicode: "../package/main.roc",\n    other: "../package/main.roc",\n'
        matches = list(harness.PACKAGE_DEPENDENCY_RE.finditer(source))
        self.assertEqual(len(matches), 1)
        self.assertTrue(re.fullmatch(r"\s*unicode", matches[0].group("indent") + "unicode"))

    def test_generated_sources_come_from_canonical_manifest(self) -> None:
        generated = harness.generated_roc_sources()
        self.assertIn(
            (harness.ROOT / "package" / "InternalScriptData.roc").resolve(),
            generated,
        )
        self.assertNotIn((harness.ROOT / "package" / "Scalar.roc").resolve(), generated)


if __name__ == "__main__":
    unittest.main()
