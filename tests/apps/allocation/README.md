# Allocation baselines

`baselines-linux-x64.json` is an exact snapshot for the Roc compiler named in
the repository's `.roc-version`, instrumented test platform, target,
optimization mode, and integrated adaptive scanner. `.roc-version` is the sole
compiler-version source of truth; the baseline does not duplicate it. This is
not a cross-compiler performance threshold.

The pinned compiler currently has a known ARC regression for the long SIMD
grapheme collector, tracked in
[roc-lang/roc#10635](https://github.com/roc-lang/roc/issues/10635). The baseline
records the exact observed count rather than waiving it; a compiler upgrade
that fixes the regression must remeasure this fixture and update this note.

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
Lithuanian conditions, and many Word-driven title segments. The pinned baseline
records the exact 640-allocation delta for every successful Case mode across
that doubling, rejecting a changed or superlinear slope. It attributes the
observed linear result-buffer allocations to the pinned Roc compiler's current
COW behavior: flat Fact coordinates, eager buffer reservation, and a
specialized scalar-driver A/B did not reduce it. A separate limits mode measures
each typed, atomic Case limit failure. Case owns its transformed bytes and one
mapping fact per source scalar, so these are result-allocation baselines rather
than a claim of allocation-free transformation.
