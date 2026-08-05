# Allocation baselines

`baselines-linux-x64.json` is an exact snapshot for the pinned Roc compiler,
instrumented test platform, target, optimization mode, and integrated adaptive
scanner. It is not a cross-compiler performance threshold.

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
