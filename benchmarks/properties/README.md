# Unicode property benchmark

This opt-in benchmark compares the two intentional lookup paths for the
bounded public Unicode property family:

- `direct.roc` queries every independent narrow property view.
- `composite.roc` resolves one fused row ID per scalar and reads the same
  dense columns lazily; sparse bidi mappings remain separate in both paths.

Both programs decode identical valid UTF-8 once and emit one order-sensitive
semantic signature. It includes every typed identity, exact combining-class
number, exact sparse mapping scalar, and bracket kind. Before timing, the
runner requires direct/composite agreement, exhaustively compares every direct
field with every `Row` field over all 1,112,064 scalars, checks pinned edge
values, aliases, mappings, variation pairs, and maximal run coordinates, and
checks indexed scan allocations geometrically from 1 through 256 scalars. It
then exercises ASCII, BMP-only, supplementary-only, and mixed complex-script
corpora and alternates execution order to reduce systematic drift. Timings
include input, one complete scan, signature formatting, and process startup;
use the ratios as the useful same-machine signal.

From the repository root:

```sh
ROC=/path/to/roc python3 benchmarks/properties/run.py
```

Use `--samples`, `--target-bytes`, `--case`, and `--cpu` to control a run.
Use `--validate-only` to build and run all semantic and allocation probes
without collecting timings.
Optimized binaries and JSON results are written beneath the ignored
`.roc-unicode-tmp/benchmarks/properties/` directory. The benchmark is outside
CI and does not set a timing threshold.

## Implementation measurement

On an AMD Ryzen 7 9700X (`x86_64`) with Roc nightly
`2026-August-04-1cb06bc`, pinned to CPU 0, optimized speed builds measured:

| Path | Binary bytes |
| --- | ---: |
| Independent narrow views | 447,152 |
| Fused row ID and lazy columns | 330,616 |

The composite consumer is 116,536 bytes (26.1%) smaller when it requests the
entire bounded family. This is the measured justification for retaining the
composite-only index alongside the direct narrow views. Its generated logical
payload is separately capped in `vendor/unicode/manifest.json`.

Seven alternating samples over approximately 32 KiB per corpus produced:

| Corpus | Direct median | Composite median | Direct/composite |
| --- | ---: | ---: | ---: |
| ASCII | 4.610 ms | 4.687 ms | 0.983x |
| BMP | 2.771 ms | 2.409 ms | 1.151x |
| Supplementary | 1.862 ms | 1.447 ms | 1.287x |
| Mixed | 2.686 ms | 2.373 ms | 1.132x |

This run shows ASCII parity and a composite advantage on the non-ASCII
corpora. Sparse mappings, alias formatting, input, and process costs are
deliberately identical and remain in this whole-API measurement. The fused
layout remains justified independently by compiled size and its single-index
contract; timing results are machine- and compiler-specific and should be
remeasured rather than treated as a package guarantee.
