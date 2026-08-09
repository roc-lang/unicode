# Bidi retained-analysis benchmark

`run.py` measures complete paragraph analysis plus one full and 64 partitioned
logical-line L1/L2 reorders for all-LTR, mixed-script, neutral/NSM, valid
nested isolates, overflowed controls, repeated FSI, 63/64-depth paired
brackets, and many-run corpora. Each invocation reports a nonzero checksum and
input byte count, so an optimized build cannot discard retained analysis or any
line permutation. It verifies the configured compiler against `.roc-version`
before building.

```sh
python3 benchmarks/bidi/run.py --roc "$ROC"
```

The generated JSON is intentionally untracked. Its `scaling_seconds_ratio`
compares every adversarial corpus at the default 4 KiB, 16 KiB, and 64 KiB
sizes. Retained state and work should grow linearly with paragraph size.

## Optional external differential

Normal CI uses the vendored Unicode 17 BidiTest and BidiCharacterTest data.
`scripts/bidi_reference_differential.py` is an explicit local verification tool.
It downloads Unicode's official Code9 C reference source for Unicode 17.0.0,
verifies every source file against recorded SHA-256 digests, compiles an
isolated query wrapper, and compares deterministic seeded scalar cases against
the Roc test app. It verifies the Roc compiler against `.roc-version`; a failed
download, checksum, compile, or mismatch fails the command. The source is not
vendored, so the repository's canonical UCD data remains its local source of
truth.

On a differential failure, the command writes a minimized tab-separated row
under `.roc-unicode-tmp/failures`. Re-run it by placing that row after a
`ROC_UNICODE_TEST_V1` header for its documented suite, or promote the row to the
adjacent Bidi test fixture after review.
