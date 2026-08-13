# Coverage-guided fuzzing

This directory is the initial coverage-guided fuzzing layer for the Unicode
package. It uses the compiler-native sanitizer coverage enabled by
`roc build --fuzz` and the self-contained libFuzzer runner from
[`roc-fuzz` 0.2.1](https://github.com/lukewilliamboswell/roc-fuzz/releases/tag/0.2.1).
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

`word` shares grapheme's valid-text domain and scalar generator. It compares
`Word.ranges`, `Word.iter_ranges`, a whole-chunk `Word.Cursor`, and a
scalar-chunk `Word.Cursor` against each other. Ranges must form a lossless
scalar-aligned partition, `Word.slices` and `Word.owned` must agree with the
materialized ranges, and every materialized word range must be idempotent
under re-segmentation.

`line-break` also shares the valid-text domain and scalar generator, exercised
under the exact Unicode-default profile only (tailored-profile campaigns
remain follow-up work). It compares the exhaustive boundary stream
(`LineBreak.boundaries` / `iter_boundaries`), the opportunity stream
(`LineBreak.opportunities` / `iter_opportunities`), and a chunked
`LineBreak.Cursor` drive (whole-input and one-scalar-at-a-time). Opportunities
must equal the non-Prohibited subset of boundaries; boundary positions must be
ordered, scalar-aligned, start with the LB2 prohibited start-of-text marker,
end with the LB3 mandatory end-of-text break, and tile the source into a
lossless partition.

`case` shares the valid-text domain and scalar generator too, exercising
`Case.roc` under its default mapping profile (`to_lower`/`to_upper`/`to_title`)
and its full fold profile (`fold`), all under `Case.unlimited_limits`
(explicit profile/limit variation remains follow-up work, matching
`line-break`'s default-profile-only scope). Each result's input facts must
form a nonempty, scalar-aligned, contiguous partition of the source; its
output facts must tile the result text contiguously (an empty output span is
allowed only for a `Removed` fact); an `Unchanged` fact's input and output
slices must match exactly. `fold` and `to_lower` must be idempotent on their
own output, and a zero input-byte budget must atomically reject any nonempty
source with `LimitExceeded`.

`property` shares the valid-text domain and scalar generator too, exercising
`GeneralCategory` and `CanonicalCombiningClass` (`EastAsianWidth` is excluded;
see below). `Property.iter` must agree with `Property.fold` on the decoded
scalar sequence; each scalar's direct query must agree with the composite
`Property.Row` view; `fold_runs` must agree with `iter_runs`; runs must form a
lossless, scalar-aligned, contiguous partition where adjacent runs never
share a value (maximality) and every scalar inside a run carries that run's
directly-queried value; and every short/long/generated alias must round-trip
through `parse` back to the same value. The scalar generator's full-range
coverage exercises table boundaries and unassigned/private-use scalars
without any special-casing.

`EastAsianWidth` is deliberately excluded from `property`: its generated
lookup (`InternalEAW.roc`) is a single very large if/else boolean-OR chain
(unlike `GeneralCategory`/`CanonicalCombiningClass`, which use compact
page-table lookups), and compiling any call into it reproducibly overflows the
compiler's stack under both `roc check` and an ordinary basic-cli `roc build`.
This is tracked upstream in [roc-lang/roc#10755][roc-10755]. It is a
compiler/data-representation limitation, not a target design choice;
regenerating `InternalEAW.roc` with a page-table representation (as issue #50
or a follow-up) would remove the blocker.

[roc-10755]: https://github.com/roc-lang/roc/issues/10755

The smoke runner limits UTF-8 artifacts to 512 bytes and grapheme, word,
line-break, case, and property artifacts to 384 entropy bytes each (at most
128 scalars). Each target is pure, bounded, and designed for libFuzzer's
in-process execution model.

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
unchanged. Grapheme, word, line-break, case, and property entries must be
valid UTF-8 and are converted to the shared stable three-byte scalar
encoding. The runner also imports every sequence from the pinned Unicode 17
`GraphemeBreakTest.txt`, `WordBreakTest.txt`, and `LineBreakTest.txt`, every
`SpecialCasing.txt` and `CaseFolding.txt` source scalar, plus the historical
crash inputs from issue #19 and the pirate-flag data-loss input from issue
#22. `property` has no conformance test file to import from, so it relies
solely on its curated named seeds (table boundaries, unassigned/private-use
scalars, combining marks, noncharacters).

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

Line-break tailoring profiles, case-mapping Turkic/Lithuanian profiles and
explicit resource limits, `EastAsianWidth` fuzzing (blocked on the
`InternalEAW.roc` compiler-stack-overflow issue above), the remaining
`Property.Row` properties (`BidiClass`, `JoiningType`/`JoiningGroup`,
`IndicSyllabicCategory`/`IndicPositionalCategory`, `VerticalOrientation`,
`Emoji`), Script itemization, bidi, structured chunk plans and limits,
Unicode-version-matched differential oracles, corpus reduction automation,
artifact upload, and resource-guarded long scheduled campaigns remain
follow-up work under issue #50.
