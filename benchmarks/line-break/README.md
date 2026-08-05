# Line-break benchmark

This opt-in benchmark measures the allocation-free `LineBreak.Cursor`
opportunity stream over both all 19,338 Unicode 17 `LineBreakTest.txt` cases
joined by hard-break separators and a 1 MiB ASCII-letter run. Before timing,
the runner validates the pinned-data manifest and compares a signature of every
emitted byte offset with an independently derived signature. Every timed count
must equal the untimed signature count multiplied by the repeat count.

The timed loop counts opportunities without materializing boundaries, so it
measures the scanner, bounded lookahead, and fused property view rather than
list growth. Process startup and input reading are included but amortized over
calibrated repeated scans. Results are machine-specific and are deliberately
written only beneath the ignored `.roc-unicode-tmp` directory by default. The
JSON records the git commit and dirty state, platform and CPU affinity, tool
versions and binary checksums, and Unicode and benchmark-source checksums.

From the repository root:

```sh
ROC=/path/to/roc python3 benchmarks/line-break/run.py
```

For a quick exploratory run:

```sh
python3 benchmarks/line-break/run.py --samples 3 --target-seconds 0.1
```

On Linux, `--cpu N` pins timed benchmark processes to one CPU so same-machine
comparisons can use a stable affinity; builds retain their normal parallelism.

To compare two revisions on the same machine and under similar load:

```sh
python3 benchmarks/line-break/run.py --output /tmp/line-break-before.json
# build or check out the implementation to compare
python3 benchmarks/line-break/run.py \
    --baseline /tmp/line-break-before.json \
    --output /tmp/line-break-after.json
```
