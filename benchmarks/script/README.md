# Script itemization benchmark

This opt-in benchmark measures complete-string
`ScriptItemization.fold_runs` under the explicitly named
`ConservativeScxV1` package policy. It is a Roc implementation benchmark, not
a cross-language comparison: Unicode Script_Extensions alone does not define a
universal itemization policy with comparable semantics.

Before timing, the runner requires exact semantic signatures for:

- UAX #44 LM3 aliases and the single initial `is` rule;
- Common next to Unknown in both directions;
- complete/chunked itemization parity;
- zero-allocation dynamic alias lookup and heap-backed itemization; and
- deterministic progress at 23, 24, 31, 32, and the configured long size.

Every timed corpus first produces an order-sensitive signature over every run's
byte range, scalar range, and canonical Script alias. The calibrated timed loop
must reproduce its per-scan checksum on every sample. The representative
corpora cover ASCII prose, combining sequences, multi-valued scx characters,
rapidly alternating scripts, multilingual/supplementary/emoji/Unknown text,
and long neutral spans that force right-context replay.

From the repository root:

```sh
ROC=/path/to/pinned/roc python3 benchmarks/script/run.py --cpu 0
```

Use `--samples`, `--target-seconds`, `--target-bytes`, and repeated `--case`
arguments to control a run. `--validate-only` builds and runs the semantic,
allocation, and progress gates without collecting timings. Optimized binaries
and the default JSON result are written below the ignored
`.roc-unicode-tmp/benchmarks/script/` directory.

For an attributable same-machine comparison:

```sh
ROC=/path/to/pinned/roc python3 benchmarks/script/run.py \
    --output /tmp/script-before.json --cpu 0
# build or check out the implementation to compare
ROC=/path/to/pinned/roc python3 benchmarks/script/run.py \
    --baseline /tmp/script-before.json \
    --output /tmp/script-after.json --cpu 0
```

The JSON records git state, CPU affinity, tool versions and binary hashes,
Unicode source hashes, benchmark source hashes, semantic signatures, calibrated
repeat counts, every sample, medians, and median absolute deviations. Results
are machine-specific evidence and are deliberately not CI timing gates.

## Indexed-scalar baseline

On an AMD Ryzen 7 9700X with the Roc compiler pin recorded by the repository
commit that produced this measurement, CPU 0, nine samples, approximately 128
KiB per corpus, and a calibrated target of 0.4 seconds, the indexed scalar
implementation measured:

| Corpus | Median MB/s | MAD MB/s | Runs per scan |
| --- | ---: | ---: | ---: |
| ASCII prose | 4.672 | 0.035 | 1 |
| Combining sequences | 6.627 | 0.007 | 24,959 |
| Multi-valued scx | 6.782 | 0.047 | 24,191 |
| Alternating scripts | 5.788 | 0.062 | 78,629 |
| Multilingual | 7.639 | 0.015 | 22,629 |
| Long neutral replay | 5.702 | 0.014 | 509 |

The slow homogeneous ASCII result motivated an exact printable-ASCII batch
transition. The transition retains the final inspected scalar as lookahead and
uses the normal grapheme transition for it, preserving grapheme atomicity,
right-context replay, and Unknown barriers.

## Exact printable-ASCII batch

With the same compiler, CPU, corpus sizes, and sampling protocol, the exact
batch measured:

| Corpus | Median MB/s | MAD MB/s | Baseline delta |
| --- | ---: | ---: | ---: |
| ASCII prose | 7.985 | 0.079 | +70.92% |
| Combining sequences | 6.820 | 0.022 | +2.91% |
| Multi-valued scx | 6.917 | 0.069 | +2.00% |
| Alternating scripts | 5.828 | 0.068 | +0.70% |
| Multilingual | 8.192 | 0.065 | +7.24% |
| Long neutral replay | 302.904 | 2.003 | +5,211.92% |

Every complete-string signature remained identical to the indexed-scalar
baseline. The independent scalar streaming cursor also reproduced every
signature across all six corpora.

## Historical SIMD compiler finding

This scalar batch is an interim implementation, not an architectural decision.
The failed SIMD spike was recorded with a compiler preceding the repository
pin, but was later attributed here to the pin by mistake. The exact direct-load
source fails before backend lowering at `c7cfb69b24^`: postcheck expands a
record update before its open result row is finalized, so the resulting record
expression omits fields present in the final Lambda Mono type. Commit
`c7cfb69b24` preserves the update as an explicit base plus explicit fields
through postcheck. The apparent SIMD lowering and byte-list reference-count
failures were downstream symptoms of that missing-field layout.

The direct and helper-returned cursor/vector shapes, together with the complete
validation corpus below, pass on the compiler named in `.roc-version`. There is
no current compiler blocker to replacing the scalar transition with an exact
allocation-free SIMD implementation. That replacement must run:

```sh
ROC=/path/to/pinned/roc python3 benchmarks/script/run.py --validate-only
```

It must preserve the complete/scalar-cursor corpus signatures before replacing
the scalar transition.
