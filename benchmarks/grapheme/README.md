# Grapheme segmentation benchmark

This local benchmark compares allocation-free extended-grapheme-cluster
counting over identical UTF-8 bytes:

- Roc: `Grapheme.Cursor.push`/`finish`, using this package's Unicode 17 tables.
- Rust: `unicode-segmentation` 1.13.3, Unicode 17.
- Go: `clipperhouse/uax29` 2.7.0, Unicode 17.

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

Rust and Go library versions are locked in `Cargo.lock` and `go.sum`. Override
tool locations with command-line options or set `ROC` for the Roc compiler.

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

Every implementation must produce the same cluster count on every corpus, and
the conformance corpus must match the Unicode 17 expected count. The runner
aborts rather than reporting incomparable timings when those checks fail.
