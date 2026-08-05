# Repository instructions

## Architectural authority

Before designing, implementing, reviewing, or documenting any repository
change, read `design.md` completely. Treat it as the enduring description of
the package's intended architecture, invariants, and the reasons behind them;
it is not an implementation plan or a snapshot of the current code.

Every change must preserve the relevant invariants in `design.md`, including
the distinctions between Unicode versions and policy versions, ranges and
materialized text, complete strings and chunked sources, total `Str` APIs and
fallible byte APIs, streaming algorithms and retained analyses, canonical data
and narrow runtime views, and default behavior and explicit tailoring.

If a requested change appears to conflict with `design.md`, do not silently
work around or weaken the design. Identify the conflicting invariant and
determine whether the request reveals a genuinely new invariant or changed
requirement. Change `design.md` only when that architectural understanding has
changed, and explain the reason in the same change.

Implementation discoveries may influence representation, thresholds, and
other measured choices without changing `design.md` when its stated invariants
and contracts remain intact. Do not encode experimental implementation details
as enduring architecture.

## Integration workflow

Treat `architecture/grapheme-strangler-spike` as the integration branch for
the Unicode overhaul. Develop independent substrates and features in isolated
branches and worktrees rooted at the current integration head.

Implement shared scalar, source, range, version, property-data, and scanning
substrates before sharding algorithms that depend on them. Keep the broader
requirements in GitHub issue #46 in view so normalization, casing, additional
segmentation, identifiers, locale data, and collation do not create parallel
foundations later.

No feature or substrate branch may be merged into the integration branch until
a different agent or reviewer has performed an adversarial review against
`design.md`. Resolve material findings on the feature branch and record the
review outcome before integration. Authors do not approve their own changes.
