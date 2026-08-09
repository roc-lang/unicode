import unittest

from scripts.bidi_reference_differential import candidate_failure_signature
from scripts.bidi_reduce import minimize


class ReduceTests(unittest.TestCase):
    def test_removes_irrelevant_prefix_and_suffix(self):
        self.assertEqual(minimize(list("abcdef"), lambda value: "c" in value and "d" in value), ["c", "d"])

    def test_preserves_single_required_value(self):
        self.assertEqual(minimize(list("abcd"), lambda value: "b" in value), ["b"])

    def test_code9_failure_signature_ignores_case_values(self):
        first = "roc_stdout='FAIL\\tcase-1\\tlevels expected [Level(0)], got [Level(1)]', roc_stderr=''"
        second = "roc_stdout='FAIL\\tcase-1\\tlevels expected [Level(2)], got [Level(3)]', roc_stderr=''"
        parser = "roc_stdout='FAIL\\tcase-1\\tinvalid scalar', roc_stderr=''"
        self.assertEqual(candidate_failure_signature(first), "levels expected")
        self.assertEqual(candidate_failure_signature(first), candidate_failure_signature(second))
        self.assertNotEqual(candidate_failure_signature(first), candidate_failure_signature(parser))


if __name__ == "__main__":
    unittest.main()
