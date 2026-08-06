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
