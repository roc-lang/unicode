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
    def assert_manifest_rejected(self, manifest: dict[str, object], message: str) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text(json.dumps(manifest), encoding="utf-8")
            with self.assertRaisesRegex(unicode_data.DataError, message):
                unicode_data.load_manifest(path)

    def test_repository_data_is_valid_and_generation_is_deterministic(self) -> None:
        manifest = unicode_data.load_manifest()
        unicode_data.validate_all(manifest)
        self.assertEqual(
            unicode_data.rendered_modules(manifest), unicode_data.rendered_modules(manifest)
        )

    def test_manifest_rejects_unknown_schema(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text('{"schema_version": 999}', encoding="utf-8")
            with self.assertRaisesRegex(unicode_data.DataError, "schema_version"):
                unicode_data.load_manifest(path)

    def test_manifest_rejects_version_path_drift(self) -> None:
        manifest = json.loads(unicode_data.MANIFEST_PATH.read_text(encoding="utf-8"))
        manifest["releases"]["unicode"]["vendor_prefix"] = "vendor/unicode/16.0.0"
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text(json.dumps(manifest), encoding="utf-8")
            with self.assertRaisesRegex(unicode_data.DataError, "does not match its storage release"):
                unicode_data.load_manifest(path)

    def test_manifest_rejects_release_and_dependency_relabeling(self) -> None:
        base = json.loads(unicode_data.MANIFEST_PATH.read_text(encoding="utf-8"))
        changed_versions = json.loads(json.dumps(base))
        changed_versions["releases"]["unicode"]["version"] = "7.0.0"
        changed_versions["releases"]["emoji"]["version"] = "7.0"
        self.assert_manifest_rejected(changed_versions, "exact version")

        changed_spec = json.loads(json.dumps(base))
        changed_spec["artifacts"]["general_category"]["specifications"] = ["uts_51"]
        self.assert_manifest_rejected(changed_spec, "dependencies do not exactly match")

        changed_format = json.loads(json.dumps(base))
        changed_format["sources"]["derived_general_category"]["format"] = "uax-29-test"
        self.assert_manifest_rejected(changed_format, "fields drifted|format/properties")

        changed_properties = json.loads(json.dumps(base))
        changed_properties["sources"]["derived_general_category"]["properties"] = []
        self.assert_manifest_rejected(changed_properties, "format/properties")

    def test_artifact_outputs_cannot_overwrite_sources_or_drift_from_imports(self) -> None:
        base = json.loads(unicode_data.MANIFEST_PATH.read_text(encoding="utf-8"))
        vendor_overwrite = json.loads(json.dumps(base))
        vendor_overwrite["artifacts"]["general_category"]["output"] = (
            "vendor/unicode/17.0.0/DerivedGeneralCategory.txt"
        )
        self.assert_manifest_rejected(vendor_overwrite, "authoritative generated module")

        relocated_dependency = json.loads(json.dumps(base))
        relocated_dependency["artifacts"]["emoji_properties"]["output"] = (
            "package/RelocatedEmojiData.roc"
        )
        self.assert_manifest_rejected(
            relocated_dependency, "authoritative generated module"
        )
        loaded = unicode_data.load_manifest()
        loaded["artifacts"]["unicode_version"]["output"] = (
            "vendor/unicode/17.0.0/PropertyAliases.txt"
        )
        with self.assertRaisesRegex(unicode_data.DataError, "authoritative generated module"):
            unicode_data.rendered_modules(loaded)

    def test_manifest_graph_names_are_not_a_parallel_implementation_graph(self) -> None:
        base = unicode_data.load_manifest()
        renamed = json.loads(json.dumps(base))
        renamed["sources"]["gc_snapshot"] = renamed["sources"].pop(
            "derived_general_category"
        )
        for artifact in renamed["artifacts"].values():
            artifact["sources"] = [
                "gc_snapshot" if source == "derived_general_category" else source
                for source in artifact["sources"]
            ]
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text(json.dumps(renamed), encoding="utf-8")
            loaded = unicode_data.load_manifest(path)
        unicode_data.validate_all(loaded)
        baseline = unicode_data.rendered_modules(base)
        self.assertEqual(
            tuple(baseline.values()), tuple(unicode_data.rendered_modules(loaded).values())
        )

    def test_cldr_axis_is_structural_but_unimplemented_formats_are_rejected(self) -> None:
        base = json.loads(unicode_data.MANIFEST_PATH.read_text(encoding="utf-8"))
        base["releases"]["cldr"] = {
            "version": "48",
            "authority": "unicode",
            "kind": "cldr",
            "vendor_prefix": "vendor/unicode/cldr/48",
        }
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text(json.dumps(base), encoding="utf-8")
            unicode_data.load_manifest(path)

        unsupported = json.loads(json.dumps(base))
        unsupported["sources"]["cldr_annotations"] = {
            **unsupported["sources"]["property_aliases"],
            "format": "cldr-json",
            "properties": [],
            "storage_release": "cldr",
            "release_axes": ["cldr"],
        }
        self.assert_manifest_rejected(unsupported, "no implemented parser")

        fake_sync = json.loads(json.dumps(base))
        fake_sync["releases"]["cldr"]["version"] = "17.0.0"
        fake_sync["releases"]["cldr"]["vendor_prefix"] = "vendor/unicode/cldr/17.0.0"
        fake_sync["releases"]["emoji"]["synchronized_with"] = "cldr"
        self.assert_manifest_rejected(fake_sync, "must name a Unicode UCD release")

        wrong_ucd = json.loads(json.dumps(base))
        wrong_ucd["releases"]["shadow_ucd"] = {
            "version": "17.0.0",
            "authority": "unicode",
            "kind": "ucd",
            "vendor_prefix": "vendor/unicode/shadow/17.0.0",
        }
        wrong_ucd["releases"]["emoji"]["synchronized_with"] = "shadow_ucd"
        self.assert_manifest_rejected(wrong_ucd, "storage release does not match")

    def test_required_property_defaults_reject_deleted_declarations(self) -> None:
        manifest = unicode_data.load_manifest()
        eaw_path = unicode_data.data_path(manifest, "east_asian_width")
        eaw = unicode_data.verify_source(manifest, "east_asian_width")
        eaw_without_formal = eaw.replace("# @missing: 0000..10FFFF; N\n", "")
        with self.assertRaisesRegex(unicode_data.DataError, "@missing declaration|formal full-domain default"):
            unicode_data.parse_east_asian_width_defaults(
                eaw_without_formal, source=str(eaw_path)
            )

        emoji_path = unicode_data.data_path(manifest, "emoji_data")
        emoji = unicode_data.verify_source(manifest, "emoji_data")
        emoji_without_defaults = "\n".join(
            line
            for line in emoji.splitlines()
            if not line.startswith("# All omitted code points have ")
        )
        with self.assertRaisesRegex(unicode_data.DataError, "six ordered Emoji"):
            unicode_data.parse_emoji_defaults(
                emoji_without_defaults, source=str(emoji_path)
            )

        emoji_without_component = "\n".join(
            line for line in emoji.splitlines() if "; Emoji_Component" not in line
        )
        component_records = unicode_data.parse_ranges(
            emoji_without_component,
            source=str(emoji_path),
            allowed_properties=unicode_data.EMOJI_PROPERTIES,
            default_marker=None,
            overlaps_by_property=True,
        )
        component_defaults = unicode_data.parse_emoji_defaults(
            emoji_without_component, source=str(emoji_path)
        )
        with self.assertRaisesRegex(unicode_data.DataError, "Emoji_Component has no explicit records"):
            unicode_data._validate_default_precedence(
                component_records,
                component_defaults,
                source=str(emoji_path),
                properties=unicode_data.EMOJI_PROPERTIES,
            )

    def test_bidi_default_cascade_rejects_value_drift_and_duplicates(self) -> None:
        manifest = unicode_data.load_manifest()
        canonical = unicode_data.load_canonical_properties(manifest)
        original = unicode_data.verify_source(manifest, "derived_bidi_class")
        declaration = "# @missing: 0590..05FF; Right_To_Left"
        self.assertIn(declaration, original)
        mutations = (
            original.replace(declaration, "# @missing: 0590..05FF; Arabic_Letter"),
            original + f"\n{declaration}\n",
            original + "\n# @missing: 0000..10FFFF; Joining_Type; Non_Joining\n",
        )
        verified_source = unicode_data.verify_source
        for mutated in mutations:
            with self.subTest(mutation=mutated[-90:]):
                def verify_with_mutation(
                    loaded_manifest: dict[str, object], loaded_source: str
                ) -> str:
                    if loaded_source == "derived_bidi_class":
                        return mutated
                    return verified_source(loaded_manifest, loaded_source)

                with (
                    mock.patch.object(
                        unicode_data, "verify_source", side_effect=verify_with_mutation
                    ),
                    self.assertRaisesRegex(
                        unicode_data.DataError, "exact Unicode 17.0.0 cascade"
                    ),
                ):
                    unicode_data.load_public_properties(manifest, canonical)

    def test_sparse_public_data_rejects_every_non_scalar_endpoint(self) -> None:
        for invalid in ("D800", "110000"):
            with self.subTest(parser="mapping-source", invalid=invalid):
                with self.assertRaisesRegex(unicode_data.DataError, "non-scalar"):
                    unicode_data._parse_sparse_mapping(
                        f"{invalid}; 0029\n", source="mapping"
                    )
            with self.subTest(parser="mapping-target", invalid=invalid):
                with self.assertRaisesRegex(unicode_data.DataError, "non-scalar"):
                    unicode_data._parse_sparse_mapping(
                        f"0028; {invalid}\n", source="mapping"
                    )
            with self.subTest(parser="bracket-source", invalid=invalid):
                with self.assertRaisesRegex(unicode_data.DataError, "non-scalar"):
                    unicode_data._parse_bidi_brackets(
                        f"{invalid}; 0029; o\n0029; {invalid}; c\n",
                        source="brackets",
                    )
            with self.subTest(parser="bracket-target", invalid=invalid):
                with self.assertRaisesRegex(unicode_data.DataError, "non-scalar"):
                    unicode_data._parse_bidi_brackets(
                        f"0028; {invalid}; o\n{invalid}; 0028; c\n",
                        source="brackets",
                    )
            with self.subTest(parser="variation-base", invalid=invalid):
                with self.assertRaisesRegex(unicode_data.DataError, "non-scalar"):
                    unicode_data._parse_emoji_variation_bases(
                        f"{invalid} FE0E; text style\n{invalid} FE0F; emoji style\n",
                        source="variations",
                    )

    def test_full_loaders_reject_alternate_shape_conflicting_formal_defaults(self) -> None:
        manifest = unicode_data.load_manifest()
        mutations = (
            (
                "grapheme_break_property",
                "# @missing: 0000..10FFFF; Grapheme_Cluster_Break; Extend",
                unicode_data.load_property_data,
            ),
            (
                "east_asian_width",
                "# @missing: 0000..10FFFF; East_Asian_Width; W",
                unicode_data.load_property_data,
            ),
            (
                "derived_core_properties",
                "# @missing: 0000..10FFFF; Linker",
                unicode_data.load_property_data,
            ),
            (
                "derived_combining_class",
                "# @missing: 0000..10FFFF; Canonical_Combining_Class; Above",
                unicode_data.load_canonical_properties,
            ),
            (
                "property_value_aliases",
                "# @missing: 0000..10FFFF; Uppercase_Letter",
                unicode_data.load_canonical_properties,
            ),
        )
        verified_source = unicode_data.verify_source
        for source_name, conflict, loader in mutations:
            with self.subTest(source=source_name):
                mutated = verified_source(manifest, source_name) + conflict + "\n"

                def verify_with_mutation(
                    loaded_manifest: dict[str, object], loaded_source: str
                ) -> str:
                    if loaded_source == source_name:
                        return mutated
                    return verified_source(loaded_manifest, loaded_source)

                with (
                    mock.patch.object(
                        unicode_data, "verify_source", side_effect=verify_with_mutation
                    ),
                    self.assertRaisesRegex(
                        unicode_data.DataError, "exactly one @missing declaration"
                    ),
                ):
                    loader(manifest)

    def test_line_break_loader_rejects_every_extra_missing_declaration_shape(self) -> None:
        manifest = unicode_data.load_manifest()
        mutations = (
            ("line_break", "# @missing: 0000..10FFFF; AL"),
            ("line_break", "# @missing: 0000..10FFFF; Line_Break; AL"),
            ("line_break", "# @missing: 0000..10FFFF; l-i_n e b r e a k; Alphabetic"),
            ("derived_line_break", "# @missing: 0000..10FFFF; AL"),
            ("derived_line_break", "# @missing: 0000..10FFFF; lb; AL"),
            ("derived_line_break", "# @missing: 0000..10FFFF; l-i_n e b r e a k; Alphabetic"),
        )
        verified_source = unicode_data.verify_source
        for source_name, conflict in mutations:
            with self.subTest(source=source_name, conflict=conflict):
                mutated = verified_source(manifest, source_name) + conflict + "\n"

                def verify_with_mutation(
                    loaded_manifest: dict[str, object], loaded_source: str
                ) -> str:
                    if loaded_source == source_name:
                        return mutated
                    return verified_source(loaded_manifest, loaded_source)

                with (
                    mock.patch.object(
                        unicode_data, "verify_source", side_effect=verify_with_mutation
                    ),
                    self.assertRaisesRegex(
                        unicode_data.DataError,
                        "Line_Break @missing declaration",
                    ),
                ):
                    unicode_data.load_line_break_properties(manifest)

    def test_line_break_defaults_accept_qualified_loose_aliases_once(self) -> None:
        manifest = unicode_data.load_manifest()
        verified_source = unicode_data.verify_source
        original = verified_source(manifest, "line_break")
        mutated = original.replace(
            "# @missing: 0000..10FFFF; XX",
            "# @missing: 0000..10FFFF; l-i_n e b r e a k; Unknown",
        )

        def verify_with_mutation(
            loaded_manifest: dict[str, object], loaded_source: str
        ) -> str:
            if loaded_source == "line_break":
                return mutated
            return verified_source(loaded_manifest, loaded_source)

        with mock.patch.object(
            unicode_data, "verify_source", side_effect=verify_with_mutation
        ):
            records, values = unicode_data.load_line_break_properties(manifest)
        self.assertTrue(records)
        self.assertEqual(len(values), unicode_data.MAX_CODE_POINT + 1)

    def test_formal_default_spellings_share_one_canonical_identity(self) -> None:
        cases = (
            ("Grapheme_Cluster_Break", None, "Other", "g-c_b"),
            ("East_Asian_Width", None, "N", "e-a"),
            ("Indic_Conjunct_Break", "InCB", "None", "indic-conjunct break"),
            ("Canonical_Combining_Class", None, "Not_Reordered", "c_c-c"),
            ("General_Category", "General_Category", "Unassigned", "g-c"),
        )
        for property_name, declared_property, value, loose_spelling in cases:
            with self.subTest(property=property_name):
                qualified = (
                    f"# @missing: 0000..10FFFF; {loose_spelling}; {value}\n"
                )
                self.assertEqual(
                    unicode_data._required_formal_default(
                        qualified,
                        source="fixture.txt",
                        property_name=property_name,
                        declared_property=declared_property,
                        value=value,
                    ).value,
                    value,
                )
                duplicate_across_shapes = (
                    qualified + f"# @missing: 0000..10FFFF; {value}\n"
                )
                with self.assertRaisesRegex(
                    unicode_data.DataError, "exactly one @missing declaration"
                ):
                    unicode_data._required_formal_default(
                        duplicate_across_shapes,
                        source="fixture.txt",
                        property_name=property_name,
                        declared_property=declared_property,
                        value=value,
                    )

    def test_script_sources_preserve_defaults_aliases_and_unicode_17_values(self) -> None:
        manifest = unicode_data.load_manifest()
        properties = unicode_data.load_script_properties(manifest)
        self.assertEqual(len(properties.aliases), 176)
        self.assertEqual(properties.script_default, "Zzzz")
        self.assertEqual(len({record.scripts for record in properties.extensions}), 118)

        def primary(code_point: int) -> str:
            for record in properties.scripts:
                if record.start <= code_point <= record.end:
                    return record.property
            return properties.script_default

        def extensions(code_point: int) -> tuple[str, ...]:
            for record in properties.extensions:
                if record.start <= code_point <= record.end:
                    return record.scripts
            return (primary(code_point),)

        for code_point, expected in (
            (0x0041, "Latn"),
            (0x03B1, "Grek"),
            (0x0416, "Cyrl"),
            (0x0627, "Arab"),
            (0x4E00, "Hani"),
            (0x3042, "Hira"),
            (0x30A2, "Kana"),
            (0x10940, "Sidt"),
            (0x11DB0, "Tols"),
            (0x16EA0, "Berf"),
            (0x1E6C0, "Tayo"),
            (0xE000, "Zzzz"),
            (0x10FFFF, "Zzzz"),
            (0xD800, "Zzzz"),
        ):
            with self.subTest(code_point=f"U+{code_point:04X}"):
                self.assertEqual(primary(code_point), expected)
                self.assertTrue(extensions(code_point))

        self.assertEqual(extensions(0x30FC), ("Hira", "Kana"))
        self.assertEqual(extensions(0x0363), ("Latn",))
        self.assertIn("Gara", extensions(0x060C))
        self.assertIn(primary(0x096F), extensions(0x096F))
        alias_rows = {record.identity: record for record in properties.aliases}
        self.assertIn("Qaai", alias_rows["Zinh"].aliases)
        self.assertIn("Qaac", alias_rows["Copt"].aliases)

    def test_script_extensions_parser_fails_closed(self) -> None:
        manifest = unicode_data.load_manifest()
        alias_name = unicode_data._source_for(
            manifest,
            "ucd-property-value-aliases",
            ("Canonical_Combining_Class", "General_Category", "Script"),
        )
        aliases = unicode_data.parse_property_value_aliases(
            unicode_data.verify_source(manifest, alias_name), source=alias_name
        )["sc"]
        default = "# @missing: 0000..10FFFF; <script>\n"
        failures = (
            (default + "0041 ; Latn Latn\n", "duplicate"),
            (default + "0041 ; NotAScript\n", "exactly one value"),
            (default + "0041 ; Zyyy\n", "implicit values"),
            (default + "0041..0042 ; Latn\n0042 ; Latn\n", "overlaps"),
            ("0041 ; Latn\n", "@missing declaration"),
        )
        for text, message in failures:
            with self.subTest(message=message):
                with self.assertRaisesRegex(unicode_data.DataError, message):
                    unicode_data.parse_script_extensions(
                        text, source="fixture.txt", aliases=aliases
                    )

    def test_generated_alias_access_is_static_and_allocation_free_in_shape(self) -> None:
        manifest = unicode_data.load_manifest()
        aliases = unicode_data.rendered_modules(manifest)[
            unicode_data.ROOT / "package" / "InternalPropertyAliases.roc"
        ]
        self.assertNotIn("List(Str)", aliases)
        self.assertNotIn("Iter(", aliases)
        self.assertNotIn("Iter.custom(", aliases)
        for accessor in (
            "general_category_short",
            "general_category_long",
            "general_category_alias_count",
            "general_category_alias_at",
            "canonical_combining_class_short",
            "canonical_combining_class_long",
            "canonical_combining_class_alias_count",
            "canonical_combining_class_alias_at",
        ):
            self.assertIn(accessor, aliases)

    def test_source_rejects_hash_header_and_count_drift(self) -> None:
        content = "# Header\n0041 ; A\n"
        digest = hashlib.sha256(content.encode()).hexdigest()
        base_entry = {
            "path": "vendor/unicode/fixture.txt",
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
                manifest = {"sources": {"fixture": entry}}
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
        manifest["sources"]["grapheme_break_test"]["cases"] = 1
        with self.assertRaisesRegex(unicode_data.DataError, "boundary marker"):
            unicode_data.parse_grapheme_tests(
                manifest,
                "÷ 0041 ? 0042 ÷ # ÷ [0.2] A ? [999.0] B ÷ [0.3]\n",
            )

if __name__ == "__main__":
    unittest.main()
