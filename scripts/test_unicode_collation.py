"""Focused collation source validation independent of Roc runtime lookup."""

import unittest

from scripts.unicode_collation import Element, merge_cldr_root, parse_cldr_collation, parse_ducet, tailor


class CollationDataTest(unittest.TestCase):
    def test_allkeys_rejects_version_and_duplicate_mapping(self):
        source = "@version 18.0.0\n@implicitweights 17000..187FF; FB00\n0061 ; [.2485.0020.0002]"
        entries, implicit = parse_ducet(source, "18.0.0")
        self.assertEqual(entries[(0x61,)], (Element(0x2485 * 4, 0x20 * 4, 2 * 4),))
        self.assertEqual(implicit, [(0x17000, 0x187FF, 0xFB00)])
        with self.assertRaisesRegex(ValueError, "version"):
            parse_ducet(source, "17.0.0")
        with self.assertRaisesRegex(ValueError, "repeated"):
            parse_ducet(source + "\n0061 ; [.2485.0020.0002]", "18.0.0")

    def test_cldr_flags_are_separate_and_nonzero_weight_changes_fail(self):
        base = {(0x61,): (Element(4, 8, 8, True), Element(0, 0, 0))}
        cldr = {(0x61,): (Element(4, 8, 8, False),)}
        self.assertTrue(merge_cldr_root(base, cldr)[(0x61,)][0].variable)
        self.assertFalse(merge_cldr_root(base, cldr)[(0x61,)][0].cldr_variable)
        with self.assertRaisesRegex(ValueError, "changes nonzero"):
            merge_cldr_root(base, {(0x61,): (Element(5, 8, 8, False),)})

    def test_packed_domains_and_relation_strength_are_checked(self):
        with self.assertRaisesRegex(ValueError, "packed width"):
            Element(1 << 18, 0, 0).encode()
        with self.assertRaisesRegex(ValueError, "variable-weight domain"):
            Element(0, 8, 8, True).encode()
        table = {(0x61,): (Element(4, 8, 8),), (0x301,): (Element(0, 12, 8),)}
        with self.assertRaisesRegex(ValueError, "strong enough"):
            tailor(table, "&á<b", {})

    def test_rejects_unsupported_tailoring_syntax_and_payload(self):
        table = {(0x61,): (Element(4, 8, 8),)}
        with self.assertRaisesRegex(ValueError, "unsupported"):
            tailor(table, "&a<*abc", {})
        with self.assertRaisesRegex(ValueError, "unsupported"):
            tailor(table, "&a<\\x", {})
        text = '<ldml><identity><language type="es"/></identity><collations><collation type="standard"><settings/><cr>&amp;n&lt;ñ</cr></collation></collations></ldml>'
        with self.assertRaisesRegex(ValueError, "unsupported CLDR collation payload"):
            parse_cldr_collation(text, "es", "standard")
