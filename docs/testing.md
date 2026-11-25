# Testing Unicode

Start with the algorithm's contract in [design.md](../design.md) and the
source files and specification revisions in
[the Unicode manifest](../vendor/unicode/manifest.json). Identify the
conformance clause, property default, or explicit profile that defines the
expected result. Record the Unicode version and any independent tailoring
revision when they affect the result.

## Choose the check that fits the contract

| Evidence | What it checks |
| --- | --- |
| Official conformance corpus | Exact default results for the inputs and versions supplied by Unicode. Keep generated expectations independent of the runtime table representation. |
| Roc expectations and fixture applications | Concrete public behavior, regressions, rejection and completion states, output ownership, and resource limits. Compare coordinates in the units the API promises. |
| `Model` | Exhaustive exploration of a small, declared state space, such as short chunk histories or a tiny ambiguous dictionary. Give the model an independent implementation of the expected behavior. |
| `Solve` | Integer laws over a declared domain, such as coordinate addition and limits. Use production terms when possible and pass concrete endpoints or solver witnesses through the real caller. |
| Coverage-guided fuzz suites | Broader combinations of Unicode scalars, raw bytes, drive plans, and explicit policies. Seed successful and failing paths. Check relationships across public APIs and use independent oracles where practical. |
| Host allocation and work probes | Exact resource evidence for representative inputs under the repository toolchain and target. Interpret these alongside semantic checks. |

## Design and review an oracle

1. Identify the public entry point or transition that owns the contract. A
   segmentation equivalence check should compare original half-open ranges
   in order. Reassembled text alone can conceal a moved boundary, and two
   drivers of the same transition can agree on a wrong answer.
2. Derive the expected result from the input. For chunked traversal, track
   absolute coordinates and the points where output becomes irrevocable.
   Include empty chunks, explicit finish, early stops, and typed failures
   where the API supports them. For arbitrary bytes, compare the valid prefix
   and first error offset. For transformations, compare each source-mapping
   fact so a correct output string cannot conceal incorrect attribution.
3. State the scope of a model or proof next to its check. A state model records
   the action alphabet and depth, asserts exhaustion, and checks that named
   goals were reached. Include in its state key every fact that can affect
   future actions or observations. A proof records the assumed domain and
   separately exercises its arithmetic at a concrete package entry point.
4. For a new or materially changed oracle, make a targeted production mutation
   in a scratch worktree and verify that the check detects it. Pick a fault
   the claimed contract would forbid, such as accepting an interior UTF-8
   byte as a boundary or resolving a pending decision at a chunk edge.
   Record the mutation and which check failed, and discard the mutant.
5. Minimize actionable fuzz failures. Save the raw artifact and rendered
   driving plan with the suite target and toolchain provenance. Promote the
   understood defect to a deterministic regression in the owning feature.
   Retain a corpus entry where it helps the search reach the behavior.

Version-matched differential tests require equivalent Unicode semantics and
tailoring. Record the reference version, locale or profile, offset units,
and any known policy difference. Use comparisons across differing policies
as diagnostics. Official conformance remains the oracle for the default.

## Run checks

Set `ROC` to a compiler matching [`.roc-version`](../.roc-version) and use
Zig as pinned in the [test workflow](../.github/workflows/tests.yaml).
The test and fuzz drivers verify the compiler before running Roc workloads.

```sh
python3 scripts/test.py grapheme
python3 scripts/test.py data
python3 scripts/fuzz.py models
python3 scripts/fuzz.py smoke
./scripts/all_tests.sh
```

Choose an affected feature while developing. Run the deterministic oracles and
the optimized smoke build when changing suite imports. The pull-request fuzz
lane is a bounded integration check. See [fuzz operations](../fuzz/README.md)
for campaigns, corpus maintenance, and replay. Run the broader conformance,
resource, and campaign checks when reviewing the integrated change.

Keep resource tests driven by runtime input so compiler constant evaluation
cannot erase the measured work. Compare baseline changes against independently
verified output and record the cause of metric differences before updating the
baseline. Record the repository commit that names the compiler pin, the
optimization mode, and the target. See the
[allocation baselines](../tests/apps/allocation/README.md) for current evidence.

Adapted from [Testing roc-pdf](https://github.com/lukewilliamboswell/roc-pdf/blob/ce52d1509a316fa10ff6f7aa0bb9f64f0bdf3283/docs/testing.adoc).
