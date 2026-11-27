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

## Pinned Roc toolchain

`.roc-version` is the repository's sole source of truth for the Roc compiler.
It names a release tag of github.com/roc-lang/roc (e.g. `0.1.0_RC1`); the daily
"Update Roc nightly" workflow only opens a PR when the latest nightly fails.
Do not duplicate a concrete Roc version tag in scripts, workflows, baselines, or
documentation. Every path that invokes Roc must either install the compiler
named by `.roc-version` or verify the selected compiler against it before doing
work. Historical measurements should identify the pin through the repository
commit that produced them; generated, untracked reports may record the actual
compiler version they observed.

## Testing policy

Prefer roc-fuzz targets that check explicit correctness invariants and use a
small set of named seeds. Keep focused expectations for concrete regressions
or semantics that a general oracle cannot distinguish. Pin upstream Unicode
and CLDR inputs by URL and checksum in the manifest and fetch them locally
for Unicode updates and exhaustive conformance. Keep newly fetched inputs and
corpora out of Git. Commit the generated Roc modules needed by package users.

## Integration workflow

Develop independent substrates and features in isolated branches and
worktrees rooted at the current designated integration branch. Do not develop
independent feature work directly on the integration branch.

Implement shared scalar, source, range, version, property-data, and scanning
substrates before sharding algorithms that depend on them. Keep the broader
requirements in GitHub issue #46 in view so normalization, casing, additional
segmentation, identifiers, locale data, and collation do not create parallel
foundations later.

No feature or substrate branch may be merged into the integration branch until
a different agent or reviewer has performed an adversarial review against
`design.md`. Resolve material findings on the feature branch and record the
review outcome before integration. Authors do not approve their own changes.

## Testing methodology

Follow [docs/testing.md](docs/testing.md) when designing or reviewing test
evidence. Identify the production contract, the independent expected result,
and the limits of the evidence. Record the result of a targeted mutation when
adding or materially changing an oracle.
