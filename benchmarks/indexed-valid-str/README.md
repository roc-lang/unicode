# Indexed valid-`Str` cursor probe

This focused, opt-in probe measures the complete-string pull cursor behind
`Scalar.iter`. It checks scalar identities and byte/scalar coordinates against
an independent Python UTF-8 traversal for empty, inline-boundary, BMP,
supplementary, and mixed inputs. A three-scalar variant verifies early stop.
Allocation counts are captured inside the Roc app around traversal only.
The same signatures are checked through a retained seamless slice, and
`Grapheme.iter_ranges` is compared with the independent collecting path.

The performance cases cover ASCII, combining marks, multiple scripts,
supplementary scalars, and mixed text. On Linux the runner pins processes to
one CPU when `taskset` is available. Capture a baseline and compare a change on
the same machine with:

```sh
python3 benchmarks/indexed-valid-str/run.py --output /tmp/cursor-before.json
python3 benchmarks/indexed-valid-str/run.py \
    --baseline /tmp/cursor-before.json \
    --output /tmp/cursor-after.json
```

Results are machine-specific and remain under the ignored build directory by
default. This probe is not a CI timing gate.

## Measured decision

The Roc compiler pin recorded by the repository commit that produced this
measurement showed that the former `Iter(U8)` cursor performed one allocation
per visited byte:
1, 8, and 64 ASCII bytes produced 1, 8, and 64 allocations. The indexed cursor
produced 1, 1, and 0 respectively. An inline `Str` therefore pays one fixed
`to_utf8` materialization; a heap-backed `Str` supplies a borrowed list view
and traversal allocates zero times. Early stopping after three scalars has the
same fixed cost and does not visit the suffix.

Because the old short-input path was not allocation-independent, there is no
positive byte threshold: all strings use indexed decoding (the empty view
allocates zero times). The probe explicitly covers 8 and 23 inline bytes and
24, 25, and 64 heap-backed bytes around the measured x64 representation edge.
Long seamless slices also retain the borrowed backing view and allocate zero
times while traversing.

On an AMD Ryzen 7 9700X pinned to one CPU, the old cursor measured about
0.30 MB/s in this allocation-counting host. The indexed cursor measured
19.5 MB/s for ASCII, 26.8 MB/s for combining text, 46.6 MB/s for multiscript,
72.3 MB/s for supplementary scalars, and 46.0 MB/s for mixed text. The host's
debug allocation accounting makes absolute throughput unsuitable for general
library comparison; the same-machine change is useful because it removes the
old per-byte allocator traffic. Three pinned, cache-disabled optimized builds
had median wall times of 175 ms before and 142 ms after. The optimized probe
binary shrank from 215,968 to 210,568 bytes, so the indexed representation did
not trade runtime throughput for compile time or linked size.
