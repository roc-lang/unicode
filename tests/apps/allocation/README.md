# Allocation ratchet

[`ratchet.json`](ratchet.json) records the maximum number of allocation
events for each runtime fixture. CI applies the same ceilings on Linux and
macOS with the compiler in `.roc-version` and speed optimization. The
original measurements came from Linux x64. Allocation counts for the same
pure Roc code and inputs should agree across those platforms. A difference
under the same compiler and optimization settings is a compiler bug.

A fixture passes at or below its ceiling. Run `python3 scripts/test.py
allocations` with the pinned compiler. Lower a ceiling after verifying the
fixture's output and reproducing the reduction. Investigate increases before
editing a ceiling, including whether the compiler changed the generated code
or the host measured something outside the fixture. The zero-allocation
checks, host calibration, and bidi scaling check retain their own contracts.

The compiler pin required by the initial `roc-fuzz` integration changed the
five-byte ASCII grapheme fixture from two allocation events to five, and the
long SIMD grapheme collector recorded 193 instead of 12 under the ARC
regression tracked in
[roc-lang/roc#10635](https://github.com/roc-lang/roc/issues/10635).
[roc-lang/roc#10873](https://github.com/roc-lang/roc/pull/10873) closed that
ownership gap: a collection nested in an accumulator record now stays unique
across the loop that rebuilds the record, so both fixtures are back to two and
12 with the pinned compiler. Those measured counts set the current ceilings.

The `allocation-aliases` suite separately exercises property-name access,
General_Category short/long/count/index access, and exact CCC
short/long/count/index access. Its required allocation count is zero. Future
ratchet changes must be justified with pinned Host probes for the changed
implementation.

The `allocation-line-break-cursor` suite folds opportunity coordinates into a
numeric signature without collecting them. Its required allocation count is
zero across the shared empty, ASCII, combining, regional-indicator, emoji-ZWJ,
and long fixtures. This verifies the streaming core independently of the
materializing convenience APIs.

The `allocation-word-*` suites set per-fixture ceilings for lazy
iteration, cursor folding, range collection, seamless slices, and owned text.
They share the Unicode-shaped fixtures above and add a multi-segment input, so
the ratchet observes both bounded streaming state and materializer growth.

The `allocation-case-*` suites measure complete-string lower, upper, R3 title,
and fold operations under default, Turkic, Lithuanian, full, and simple
profiles. Their fixtures cover 320- and 640-scalar ASCII scaling, mapping
expansion, long case-ignorable right context, alternating sigma, Turkic and
Lithuanian conditions, and many Word-driven title segments. Under the
previously pinned compiler every successful Case mode allocated exactly two
buffers per source scalar, because the compiler copied both accumulator
collections on each record update;
[roc-lang/roc#10873](https://github.com/roc-lang/roc/pull/10873) removed those
copies. The compiler recorded three more allocation events for 640 scalars
than for 320 in each successful mode. The ratchet guards both fixture sizes
independently.

Every fixture now grows geometrically, including those whose output scalars
are outside ASCII. Those recorded roughly one allocation per emitted scalar
while the UTF-8 encoders reserved each scalar's width ahead of appending it:
an explicit `List.reserve` is sized exactly, so that reserve relocated the
buffer on every multi-byte scalar rather than amortizing over a run of them.
Removing it took the expansion-heavy fixture from 268 allocations to 16 and
the long case-ignorable fixture from 1041 to 22. A separate limits mode
measures each typed, atomic Case limit failure. Case owns its transformed
bytes and one mapping fact per source scalar, so these are result-allocation
ceilings for a transformation that owns its result.

The compiler upgrade for package 4.3.3 includes
[static long string literals on LLVM backends](https://github.com/roc-lang/roc/commit/7300d32826277bfc992b5445fc46006964613059).
The suite-name comparisons inside the measured Word and Case probes no longer
allocate these literals. All 126 exact fixture counts were measured twice with
the pinned compiler (`--opt=speed`) and Zig 0.16.0 test host
(`-Doptimize=ReleaseFast`), then verified through the ordinary allocation suite. These measurements seeded
the current ceilings.
Word counts decrease by one; iterator and cursor probes now allocate zero.
Case counts decrease by the number of long suite-name comparisons on their
dispatch path, and typed limit failures now allocate zero. Grapheme counts and
the three-allocation ASCII doubling deltas remain unchanged. These are harness
overhead reductions; the package algorithms are unchanged.
