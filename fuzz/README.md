# Coverage-guided fuzzing

This directory is the initial coverage-guided fuzzing layer for the Unicode
package. It uses the compiler-native sanitizer coverage enabled by
`roc build --fuzz` and the self-contained libFuzzer runner from
[`roc-fuzz` 0.2.0](https://github.com/lukewilliamboswell/roc-fuzz/releases/tag/0.2.0).
The compiler therefore instruments the compiled Roc application and Unicode
algorithm code, rather than observing only a platform host.

This replaces issue #50's earlier AFL++ instrumentation spike. Deterministic
Unicode conformance tests remain authoritative and continue to live in their
feature suites; fuzzing adds adversarial inputs and metamorphic invariants.

## Initial targets

`utf8` receives every fuzzer byte unchanged. It compares whole-input,
one-byte-chunk, and one-scalar-stop/resume executions of `Utf8.Cursor`. Emitted
scalars must re-encode to their exact source ranges, successful results must
agree with `Str.from_utf8`, malformed offsets must agree across driving modes,
and terminal cursors must remain sealed.

`grapheme` is a separate valid-text domain. Each three little-endian fuzzer
bytes select one scalar rank; ranks above `U+D7FF` skip the surrogate range.
The target compares iterator, collector, materializer, whole-chunk cursor, and
scalar-chunk cursor results. Ranges must form a lossless scalar-aligned
partition, and every materialized cluster must be idempotent under
segmentation.

The smoke runner limits UTF-8 artifacts to 512 bytes and grapheme artifacts to
384 entropy bytes (at most 128 scalars). Each target is pure, bounded, and
designed for libFuzzer's in-process execution model.

## Commands

Every command checks the selected compiler against the repository's sole Roc
pin in `.roc-version`:

```sh
ROC=roc python3 scripts/fuzz.py build
ROC=roc python3 scripts/fuzz.py smoke
ROC=roc python3 scripts/fuzz.py campaign grapheme -- --time=3600
ROC=roc python3 scripts/fuzz.py reproduce grapheme .roc-fuzz/crash-HASH
ROC=roc python3 scripts/fuzz.py minimize grapheme INPUT OUTPUT
```

`smoke` builds each optimized target once, materializes a fresh seed corpus,
and runs 2,000 mutations with fixed seed, size, timeout, and RSS bounds. CI runs
this smoke on Linux x86-64 only. Apple Silicon macOS remains supported for
local campaigns by the selected roc-fuzz release.

`campaign` retains discoveries under `.roc-unicode-tmp/fuzz/corpus`. The
`reproduce` command prints the raw artifact bytes, renders the typed input with
the target's `show` function, and replays it in a fresh process.

## Seeds and regressions

`seeds.json` stores reviewable hexadecimal sources. UTF-8 entries are copied
unchanged. Grapheme entries must be valid UTF-8 and are converted to the
target's stable three-byte scalar encoding. The runner also imports every
sequence from the pinned Unicode 17 `GraphemeBreakTest.txt`, plus the historical
crash inputs from issue #19 and the pirate-flag data-loss input from issue #22.

After finding a failure:

1. Replay it in a fresh process.
2. Minimize the raw artifact.
3. Add a named source to `seeds.json` so future campaigns retain it.
4. Add an exact deterministic regression to the owning feature suite once the
   expected behavior is understood.

Corpus entries are coverage inputs, not correctness oracles. Exact edge counts
and executions per second are intentionally not checked into the repository;
the smoke log must show coverage activity, but compiler and layout changes may
legitimately change those measurements.

## Deferred work

Line breaking, property scans, Script itemization, bidi, structured chunk
plans and limits, Unicode-version-matched differential oracles, corpus
reduction automation, artifact upload, and resource-guarded long scheduled
campaigns remain follow-up work under issue #50.
