Convenient functions for working with unicode.

⚠️ This package has only gone through limited testing. [Make an issue](https://github.com/roc-lang/unicode/issues) when you hit a bug.

:eyes: [**examples**](https://github.com/roc-lang/unicode/tree/main/examples)

:book: [**documentation**](https://roc-lang.github.io/unicode)


## Learning about Unicode

The string/unicode rabbit hole goes deep, we have a [good overview](https://www.roc-lang.org/builtins/Str) (scroll to the unicode section).

## Unicode data

Production tables and conformance tests are generated from the official,
versioned Unicode files in `vendor/unicode/17.0.0/`.
`vendor/unicode/manifest.json` pins their source URLs, SHA-256 hashes, header
markers, record counts, and relevant standard revisions. All generated runtime
views come from that one manifest, so an implementation cannot silently mix
Unicode releases. The selected release is also available at runtime as
`UnicodeVersion.current`.

Grapheme segmentation follows the default, un-tailored Unicode 17 extended
grapheme cluster algorithm. Its primary output is half-open UTF-8 byte ranges;
callers can consume them lazily with `Grapheme.iter_ranges`, collect them with
`Grapheme.ranges`, retain seamless source slices with `Grapheme.slices`, or
request independent copies with `Grapheme.owned`. `Grapheme.Cursor` carries the
same algorithm across scalar-aligned streaming chunks without treating a chunk
edge as end-of-text.

Validate the sources and check generated Roc modules with:

```sh
python3 scripts/unicode_data.py validate
python3 scripts/unicode_data.py generate --check
```

Regenerate them with `scripts/rebuild.sh`.

## Tests

Each purpose-built test app lives in `tests/apps/<suite>/main.roc` with an
adjacent `spec.json`. The runner builds optimized binaries once, sends cases
over a versioned stdin protocol, and executes deterministic shards in parallel.

```sh
ROC=roc scripts/all_tests.sh
python3 scripts/test.py grapheme
python3 scripts/test.py properties --jobs 8
python3 scripts/test.py allocations
```

The grapheme suite covers all 766 official Unicode 17 conformance cases. The
property suite covers every valid Unicode scalar for grapheme-break,
East-Asian-width, and emoji properties. A separate allocation harness records
the package's allocation behavior.

## Benchmarks

Opt-in local benchmarks live in [`benchmarks`](benchmarks/README.md). They are
kept outside CI and include a reproducible Unicode 17 grapheme comparison with
Rust and Go.
