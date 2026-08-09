import unittest

from scripts.bidi_reduce import minimize


class ReduceTests(unittest.TestCase):
    def test_removes_irrelevant_prefix_and_suffix(self):
        self.assertEqual(minimize(list("abcdef"), lambda value: "c" in value and "d" in value), ["c", "d"])

    def test_preserves_single_required_value(self):
        self.assertEqual(minimize(list("abcd"), lambda value: "b" in value), ["b"])


if __name__ == "__main__":
    unittest.main()
