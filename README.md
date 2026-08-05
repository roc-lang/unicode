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

## Scalars and UTF-8

`Scalar` is the sealed text-processing value. `Scalar.from_u32` rejects both
surrogates and values above `U+10FFFF`; `Scalar.iter` traverses a valid Roc
`Str` without an impossible UTF-8 error branch. Every yielded value includes a
half-open `ByteRange` and an absolute scalar index. The iterator decodes only
the visited prefix, uses constant algorithmic state and stack, and creates no
intermediate byte or scalar list. It retains the source string for the
iterator's lifetime, while yielded scalars and ranges do not retain it.

`CodePoint` remains a distinct sealed type for the complete Unicode code-point
domain, including surrogates. It deliberately has no UTF-8 encoder or decoder;
callers validate it with `Scalar.from_code_point` before entering a text API.
UTF-8 encoding accepts only `Scalar`, so it cannot emit surrogate encodings.
`Scalar.to_str` validates the exact encoded bytes and reports a stable internal
fault instead of using lossy recovery. Property-specific queries, such as
`EastAsianWidth.of_scalar`, accept `Scalar` without coupling property tables to
the scalar representation.

Arbitrary bytes use the separate `Utf8.Cursor`. `push` accepts byte chunks and
returns `Pushed`, caller-requested `Stopped`, or a terminal `Failed` with an
indexed typed error; a chunk edge is never end-of-text. `Stopped.consumed`
identifies the unvisited suffix so callers can resume it with the returned
cursor, which does not retain the chunk. `finish` explicitly returns `End` or
reports an incomplete trailing sequence. The cursor retains at most three
pending bytes, uses constant stack and auxiliary storage, and performs no
allocation itself. Error offsets and all emitted scalar/range coordinates are
absolute from the beginning of the logical byte source.

The package cannot turn a host allocator abort into a Unicode error. It avoids
predictable growth instead: scalar encoding has a fixed one-to-four-byte
result, appending accepts and checks a caller-selected output bound before
reserving, and later algorithms use operation-specific limits where they
intrinsically retain input-proportional state. There is intentionally no
universal options or limits record.

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
the package's allocation behavior. Its Linux x64 counts are exact measurements
for the compiler in `.roc-version`: when an intentional implementation change
alters them, rerun `scripts/test.py allocations` with that pinned compiler and
review the measured fixture counts before updating the adjacent baseline file.
The runner never updates or silently accepts a new allocation baseline.

## Benchmarks

Opt-in local benchmarks live in [`benchmarks`](benchmarks/README.md). They are
kept outside CI and include a reproducible Unicode 17 grapheme comparison with
Rust and Go.
