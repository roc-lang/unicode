# Local benchmarks

These benchmarks are intentionally opt-in developer tools. They are not part of
the test suite or CI because their external toolchains and machine-dependent
timings are unsuitable as merge gates.

- [`grapheme`](grapheme/README.md) compares allocation-free Unicode 17 extended
  grapheme segmentation in Roc, Rust, and Go.
- [`indexed-valid-str`](indexed-valid-str/README.md) checks and measures the
  private complete-string pull cursor used by lazy scalar traversal.
- [`line-break`](line-break/README.md) measures allocation-free Unicode 17
  default line-break opportunities against an official conformance signature.

Keep correctness checks separate from timing thresholds. A benchmark must fail
when implementations disagree on its semantic checksum; performance changes do
not relax the package's conformance requirements.
