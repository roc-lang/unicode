# Allocation baselines

`baselines-linux-x64.json` is an exact snapshot for the Roc compiler named in
the repository's `.roc-version`, instrumented test platform, target,
optimization mode, and integrated adaptive scanner. `.roc-version` is the sole
compiler-version source of truth; the baseline does not duplicate it. This is
not a cross-compiler performance threshold.

The compiler pin required by the initial `roc-fuzz` integration changed the
five-byte ASCII grapheme fixture from two allocation events to five, and the
long SIMD grapheme collector recorded 193 instead of 12 under the ARC
regression tracked in
[roc-lang/roc#10635](https://github.com/roc-lang/roc/issues/10635).
[roc-lang/roc#10873](https://github.com/roc-lang/roc/pull/10873) closed that
ownership gap: a collection nested in an accumulator record now stays unique
across the loop that rebuilds the record, so both fixtures are back to two and
12 with the pinned compiler. The baseline records the exact measured counts
rather than waiving them.

The `allocation-aliases` suite separately exercises property-name access,
General_Category short/long/count/index access, and exact CCC
short/long/count/index access. Its required allocation count is zero. Future
baseline updates must be justified with pinned Host probes for the changed
implementation.

The `allocation-line-break-cursor` suite folds opportunity coordinates into a
numeric signature without collecting them. Its required allocation count is
zero across the shared empty, ASCII, combining, regional-indicator, emoji-ZWJ,
and long fixtures. This verifies the streaming core independently of the
materializing convenience APIs.

The `allocation-word-*` suites record exact Linux x64 counts for lazy
iteration, cursor folding, range collection, seamless slices, and owned text.
They share the Unicode-shaped fixtures above and add a multi-segment input, so
the baseline observes both bounded streaming state and materializer growth.

The `allocation-case-*` suites measure complete-string lower, upper, R3 title,
and fold operations under default, Turkic, Lithuanian, full, and simple
profiles. Their fixtures cover 320- and 640-scalar ASCII scaling, mapping
expansion, long case-ignorable right context, alternating sigma, Turkic and
Lithuanian conditions, and many Word-driven title segments. Under the
previously pinned compiler every successful Case mode allocated exactly two
buffers per source scalar, because the compiler copied both accumulator
collections on each record update;
[roc-lang/roc#10873](https://github.com/roc-lang/roc/pull/10873) removed those
copies. The pinned baseline now records a three-allocation delta across the
320-to-640-scalar doubling for every successful mode, and the harness rejects a
delta that grows with the added scalar count.

Every fixture now grows geometrically, including those whose output scalars
are outside ASCII. Those recorded roughly one allocation per emitted scalar
while the UTF-8 encoders reserved each scalar's width ahead of appending it:
an explicit `List.reserve` is sized exactly, so that reserve relocated the
buffer on every multi-byte scalar rather than amortizing over a run of them.
Removing it took the expansion-heavy fixture from 268 allocations to 16 and
the long case-ignorable fixture from 1041 to 22. A separate limits mode
measures each typed, atomic Case limit failure. Case owns its transformed
bytes and one mapping fact per source scalar, so these are result-allocation
baselines rather than a claim of allocation-free transformation.
