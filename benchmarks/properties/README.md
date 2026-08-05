# Unicode property benchmark

This opt-in benchmark compares the two intentional lookup paths for the
bounded public Unicode property family:

- `direct.roc` queries every independent narrow property view.
- `composite.roc` resolves one fused row ID per scalar and reads the same
  dense columns lazily; sparse bidi mappings remain separate in both paths.

Both programs decode identical valid UTF-8 once and emit one semantic checksum.
The runner refuses to report timings if those checksums disagree. It exercises
ASCII, BMP-only, supplementary-only, and mixed complex-script corpora and
alternates execution order to reduce systematic drift. Timings include input,
one complete scan, checksum formatting, and process startup; use the ratios as
the useful same-machine signal.

From the repository root:

```sh
ROC=/path/to/roc python3 benchmarks/properties/run.py
```

Use `--samples`, `--target-bytes`, `--case`, and `--cpu` to control a run.
Optimized binaries and JSON results are written beneath the ignored
`.roc-unicode-tmp/benchmarks/properties/` directory. The benchmark is outside
CI and does not set a timing threshold.

## Implementation measurement

On an AMD Ryzen 7 9700X (`x86_64`) with Roc
`release-fast-43b19c8d`, optimized speed builds measured:

| Path | Binary bytes |
| --- | ---: |
| Independent narrow views | 451,552 |
| Fused row ID and lazy columns | 334,488 |

The composite consumer is 117,064 bytes (25.9%) smaller when it requests the
entire bounded family. This is the measured justification for retaining the
composite-only index alongside the direct narrow views. Its generated logical
payload is separately capped in `vendor/unicode/manifest.json`.

Seven alternating samples over approximately 32 KiB per corpus produced:

| Corpus | Direct median | Composite median | Direct/composite |
| --- | ---: | ---: | ---: |
| ASCII | 115.887 ms | 115.633 ms | 1.002x |
| BMP | 113.809 ms | 115.227 ms | 0.988x |
| Supplementary | 114.591 ms | 113.628 ms | 1.008x |
| Mixed | 115.478 ms | 116.257 ms | 0.993x |

This shows no material end-to-end throughput advantage. Sparse mappings,
alias formatting, input, and process costs are deliberately identical and
remain in this whole-API measurement. The fused layout is therefore justified
by compiled size and its single-index contract, not by an unsupported
runtime-speed claim. Run the script for current machine-specific timings and
dispersion.
