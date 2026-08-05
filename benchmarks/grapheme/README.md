# Grapheme segmentation benchmark

This local benchmark compares allocation-free extended-grapheme-cluster
counting over identical UTF-8 bytes. It uses a representative set of optimized
Unicode 17 implementations with iterator or next-boundary APIs, so the timed
loop does not need to materialize every boundary:

- Roc: `Grapheme.Cursor.push`/`finish`, using this package's Unicode 17 tables.
- Rust/ICU4X: `icu_segmenter` 2.2.0, Unicode 17, from the Unicode Consortium.
- Go: `clipperhouse/uax29` 2.7.0, Unicode 17, with a specialized forward
  iterator.
- C: `libgrapheme` 3.0.0, Unicode 17, with its UTF-8 next-break API.
- Rust/reference: `unicode-segmentation` 1.13.3, Unicode 17. This is a popular
  general-purpose, forward/reverse, partial-chunk cursor rather than the Rust
  throughput frontier.

These are implementation comparisons, not language comparisons. In
particular, the two Rust libraries deliberately show how much library and API
design can matter within one language. The set is a practical performance
frontier, not a claim to exhaust every Unicode implementation.

The runner generates ASCII, combining-mark, multilingual, emoji, and official
Unicode conformance corpora. It builds optimized binaries, calibrates each
sample to a target duration, and reports median decimal MB/s plus median
absolute deviation. On Linux it pins each process to one logical CPU by
default when `taskset` is available.

## Prerequisites

- Python 3.10 or newer
- Roc and Zig
- Rust/Cargo
- Go
- A C compiler supporting `-flto`, and POSIX `make`
- Network access on the first build

Rust and Go library versions are locked in `Cargo.lock` and `go.sum`. Override
tool locations with command-line options or set `ROC` for the Roc compiler.
The runner downloads the pinned `libgrapheme` 3.0.0 source archive into the
ignored build directory and verifies its SHA-256 before building it.

From the repository root:

```sh
ROC=/path/to/roc python3 benchmarks/grapheme/run.py
```

Generated binaries and the default result file are written beneath the ignored
`.roc-unicode-tmp/benchmarks/grapheme/` directory. For a quicker exploratory
run, select one or more cases and shorten the sample duration:

```sh
python3 benchmarks/grapheme/run.py \
    --case ascii \
    --samples 3 \
    --target-seconds 0.1
```

## Before/after comparisons

Capture a baseline on the same machine and under similar load, make the change,
then pass the earlier result back to the runner:

```sh
python3 benchmarks/grapheme/run.py --output /tmp/grapheme-before.json
# build or check out the implementation to compare
python3 benchmarks/grapheme/run.py \
    --baseline /tmp/grapheme-before.json \
    --output /tmp/grapheme-after.json
```

Absolute throughput is machine-specific. Treat same-machine deltas as the
useful signal, keep samples long enough to suppress startup noise, and inspect
the reported MAD before drawing conclusions. Benchmark results are deliberately
not checked in.

## Scope

The benchmark counts emitted ranges without storing them. It therefore measures
the segmentation core rather than allocation and reference-count behavior from
materializing lists of ranges or strings. Input reading, process startup, and
printing one checksum are included but amortized over repeated scans.

Before timing, every implementation emits a compact signature of all boundary
positions. The signatures must agree on every corpus, and the official corpus
must match the boundaries encoded in Unicode 17's `GraphemeBreakTest.txt`.
This catches misplaced boundaries even when the total cluster count happens to
match. The runner aborts rather than reporting incomparable timings when those
checks fail.
