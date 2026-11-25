"""Focused validation of the normalization source projection."""

import unittest

from scripts.unicode_normalization import conformance_rows, parse_data, parse_properties


DEFAULTS = """
# @missing: 0000..10FFFF; NFC_QC; Yes
# @missing: 0000..10FFFF; NFD_QC; Yes
# @missing: 0000..10FFFF; NFKC_QC; Yes
# @missing: 0000..10FFFF; NFKD_QC; Yes
# @missing: 0000..10FFFF; NFKC_CF; <code point>
"""


def unicode_record(point, mapping):
    fields = [point, "TEST", "Lu", "0", "L", mapping, "", "", "", "N", "", "", "", "", ""]
    return ";".join(fields)


class NormalizationDataTest(unittest.TestCase):
    def test_recursive_mapping_and_full_composition_exclusions(self):
        data = "\n".join([
            unicode_record("00C5", "0041 030A"),
            unicode_record("01FA", "00C5 0301"),
            unicode_record("212B", "00C5"),
            unicode_record("FB03", "<compat> 0066 0066 0069"),
        ])
        props = DEFAULTS + "212B ; Full_Composition_Exclusion\n00AD ; NFKC_CF ;\n"
        parsed = parse_data(data, props)
        self.assertEqual(parsed.canonical[0x01FA], (0x41, 0x30A, 0x301))
        self.assertEqual(parsed.canonical[0x212B], (0x41, 0x30A))
        self.assertEqual(parsed.compatibility[0xFB03], (0x66, 0x66, 0x69))
        self.assertEqual(parsed.identifier_fold[0xAD], ())
        self.assertNotIn(0x212B, parsed.composition.values())
        self.assertEqual(parsed.composition[(0x41 << 21) | 0x30A], 0xC5)

    def test_rejects_cyclic_decomposition(self):
        with self.assertRaisesRegex(ValueError, "cyclic"):
            parse_data(unicode_record("00C5", "00C5"), DEFAULTS)

    def test_requires_exact_defaults_and_disjoint_rows(self):
        with self.assertRaisesRegex(ValueError, "defaults"):
            parse_properties(DEFAULTS.replace("NFD_QC; Yes", "NFD_QC; No"))
        with self.assertRaisesRegex(ValueError, "overlapping"):
            parse_properties(DEFAULTS + "00C0..00C5 ; NFD_QC ; N\n00C5 ; NFD_QC ; N\n")
        with self.assertRaisesRegex(ValueError, "surrogate"):
            parse_properties(DEFAULTS + "D800 ; NFD_QC ; N\n")

    def test_rejects_malformed_case(self):
        with self.assertRaisesRegex(ValueError, "malformed normalization test"):
            tuple(conformance_rows("@Part0\n0041;0041;0041;0041;\n"))
