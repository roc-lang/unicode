# Unicode fuzz suites

These suites use the compiler's sanitizer coverage with roc-fuzz 0.8.0.
Each suite builds one executable whose members can be run independently.
The driver verifies the compiler against `.roc-version` before using Roc.
The official conformance fixtures remain the reference for Unicode semantics.
See the contributor testing reference for how to choose an oracle and review
its evidence. [Testing methodology](../docs/testing.md)

| Suite | Members | Checks |
| --- | --- | --- |
| `source_suite.roc` | `utf8`, `utf8-model`, `grapheme`, `word`, `sentence`, `line-break` | Raw-byte and scalar-aligned source contracts, ordered ranges, profiles, cursor completion, and iterator pause/resume. |
| `scalar_suite.roc` | `case`, `property` | Mapping coordinates and shape, profiles and exact limits, scalar properties, maximal runs, and aliases. |
| `analysis_suite.roc` | `bidi`, `script` | Retained paragraph and line analysis, L2 reordering and inverse mappings, grapheme-aligned script runs, preferences, and pending limits. |
| `thai_suite.roc` | `thai` | Dictionary-backed line tailoring, model agreement, coordinates, and resource limits. |

The UTF-8 model exhausts the 781 histories of at most four actions drawn from
ASCII, a multibyte lead, its tail, an empty chunk, and finish. Its independent
decoder predicts emitted values and coordinates, consumed byte counts, and
terminal diagnostics. Changing the decoder’s truncation diagnostic in a scratch
copy makes this model fail at `[Lead, Finish]`. The saved goal witnesses are checked during smoke and
campaign runs. The Solve check proves the checked-append capacity law for
8-bit lengths and limits and widths 1 through 4. A concrete solver witness
exercises both sides of the real U64-limit API.

The sentence SB8 model exhausts 120 histories beginning with `A. `. These
have up to three chunks of neutral symbols, combining marks, or empty text
before lowercase or uppercase resolution or explicit completion. It checks
the independently expected cut and when it can be emitted. A targeted
mutation that ignored the lowercase resolver produced the shortest failure
`[Append(Neutral), ResolveLower]`.

## Local checks

On Linux x64 or Apple Silicon macOS, select the pinned compiler using `ROC`.

```sh
ROC=roc python3 scripts/fuzz.py models
ROC=roc python3 scripts/fuzz.py smoke
ROC=roc python3 scripts/fuzz.py smoke line-break --runs=100
ROC=roc python3 scripts/fuzz.py campaign grapheme --time=120
ROC=roc python3 scripts/fuzz.py reproduce grapheme PATH_TO_ARTIFACT
ROC=roc python3 scripts/fuzz.py minimize grapheme PATH_TO_ARTIFACT MINIMIZED
ROC=roc python3 scripts/fuzz.py reduce grapheme INPUT_CORPUS REDUCED_CORPUS
```

`models` runs the deterministic Roc expectations loaded by the selected
suite. `smoke` builds optimized suite binaries and checks the declared
examples before executing a deterministic search in a fresh corpus. Adding
a member requires updating the driver's inventory. The driver checks the
actual binary membership before running it. The smoke configuration uses up
to 2,000 runs, a 60-second wall-clock search allowance, a 5-second per-input
timeout and a 2048-MiB RSS limit. Seed executions may exceed the run count.
Campaigns keep their discoveries and use a separate time limit per target
with the same per-input and RSS bounds. Runs, reproduction, reduction, and minimization
also have an outer deadline to bound a stalled supervisor.

## Input formats and seeds

`utf8` sends bytes unchanged to the decoder. Alongside whole-buffer, byte
chunk and scalar-at-a-time drives, it derives mixed chunk sizes, empty
chunks, and stop points from the raw bytes. The model target uses five
operations encoded by byte modulo five and caps the sequence at four
operations.

The shared valid-text format starts with one configuration byte. Each
following three-byte little-endian group uses 21 bits for a scalar rank
that skips the surrogate range, and three bits for chunk actions. The
actions flush the preceding chunk, insert an empty chunk, or flush after
the current scalar. The final short group is padded with zeroes. The
configuration chooses a profile, limit, line range or iterator pause point
according to the target. Reproduction prints the configuration, chunk
actions, decoded values, and raw bytes. Each input has at most 128 scalars.
Use the corresponding historical checkout to replay a pre-suite scalar
artifact. Its old entropy format had neither a configuration byte nor chunk
actions.

`seeds.json` retains named regression and branch-driving inputs as readable
UTF-8 hex or raw malformed bytes. The seed preparer also takes bounded,
evenly spaced samples from the pinned grapheme, word, sentence, line, bidi, and case
data. For composite property checks it groups valid scalars at and next to
property source boundaries. The full official tests run through the owning
feature suites.

The bounded segmentation and script/Bidi collectors are checked at exact
capacities and against generated undersized byte and item limits. The checks
compare successful results with the public unbounded collectors and require
the typed diagnostic for the first limit reached. A scratch mutation that
rejects equality at the item limit fails the grapheme suite's declared example.

## Findings and corpus maintenance

Each run's directory under `.roc-unicode-tmp/fuzz/reports/` contains the
runner report and evidence. CI uploads those directories and the native
artifacts for failures. The scheduled Linux campaign also uploads its
persistent corpus and build/run log. Read the report's target and source
revision to find the right checkout and target name. A revision with a
`-dirty` suffix requires the local source changes as well as that checkout. Download its original
input or failing example and pass that file to `reproduce` and `minimize`.

1. Replay the original bytes and keep them with the report.
2. Minimize the witness and verify that it still reaches the same failure.
3. Add the understood case to the owning deterministic feature suite. When
   useful to later campaigns, retain the exact bytes with `Fuzz.saved` or
   add a named source to `seeds.json`.
4. Review coverage discoveries in the relevant target's corpus. Combine
   files from that target and the same input format into a staging directory
   and run `reduce`. Use a fresh destination. The reduced directory appears
   only after reduction succeeds, leaving the original and raw findings
   available for replay. Subsequent campaigns use the reduced inputs when
   placed in the target's directory under
   `.roc-unicode-tmp/fuzz/corpus/plan-v1/`.

Corpus membership guides exploration. Coverage counters and execution rate
are diagnostic evidence and may change with compiler or code layout.
