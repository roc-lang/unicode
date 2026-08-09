Convenient functions for working with unicode.

⚠️ This package has only gone through limited testing. [Make an issue](https://github.com/roc-lang/unicode/issues) when you hit a bug.

:eyes: [**examples**](https://github.com/roc-lang/unicode/tree/main/examples)

:book: [**documentation**](https://roc-lang.github.io/unicode)

The examples are complete command-line applications rather than isolated API
snippets. They include grapheme-safe display-name limiting, arbitrary-byte
UTF-8 stream decoding, bounded code-point encoding, logical line-break
opportunities, shaping-oriented script runs, and scalar property diagnostics.
Each application handles invalid arguments and package error values explicitly;
[`examples/spec.json`](examples/spec.json) exercises their happy paths,
Unicode edge cases, streaming behavior, resource limits, and error output.
Native command-line arguments remain `OsStr` until the shared example helper
validates them as Unicode text, so malformed Unix bytes or Windows UTF-16 are
reported as usage errors instead of being passed to total `Str` APIs.


## Learning about Unicode

The string/unicode rabbit hole goes deep, we have a [good overview](https://www.roc-lang.org/builtins/Str) (scroll to the unicode section).

## Unicode data

Production tables and conformance tests are generated from the official,
versioned Unicode files in `vendor/unicode/17.0.0/`.
`vendor/unicode/manifest.json` pins their source URLs, SHA-256 hashes, header
markers, and record counts. Its release snapshots, specification revisions,
sources, and generated artifacts are separate dependency-graph nodes; this
keeps the Unicode and Emoji version axes explicit and leaves room for later
data authorities such as CLDR. All generated runtime views come from that one
graph, so an implementation cannot silently mix releases. The generator checks
each source format and property projection and each artifact's source,
specification, and artifact dependencies exactly. Unicode file headers, source
URLs, and synchronized Unicode/Emoji major-minor versions anchor generated
version labels to the pinned release. A newly declared format is rejected until
there is an implementation that consumes it. Generated outputs are restricted
to their declared package modules, and downstream imports are derived from
those artifact declarations. The selected Unicode release is also available at
runtime as `UnicodeVersion.current`.

The public property layer exposes typed, scalar-only queries for
`General_Category`, exact `Canonical_Combining_Class`, `Bidi_Class`, the bidi
mirror/bracket facts, `Joining_Type`, `Joining_Group`, the Indic syllabic and
positional categories, default-ignorable and variation-selector flags,
`Vertical_Orientation`, and the six independent Emoji properties. Valid
VS15/VS16 presentation requests are an optional pair lookup rather than an
invented value for every base. These are immutable Unicode character facts;
they do not implement UAX #9 paragraph analysis, choose glyphs, or perform
font-specific shaping.

Each enumerated module exposes the official short and long Unicode aliases,
allocation-free alias enumeration, and loose ASCII alias parsing. Exact
combining classes remain opaque numeric `U8` values, including values without
well-known names. Property-specific scalar lookups use independent narrow
paged views. `Property.of_scalar` uses a separate, measured fused row index
when a consumer genuinely needs several facts. Its sealed `Property.Row`
retains only the scalar and one private row ID; typed accessors load their
columns lazily, and sparse bidi mappings are looked up only when requested.
Querying one property directly does not construct a row or require the
composite table.
`Property.fold` and `Property.iter` decode a complete valid `Str` once and
retain original half-open coordinates. Property modules also expose typed
`fold_runs` and `iter_runs` operations for maximal adjacent `TextRange` runs;
they do not materialize per-scalar substrings or an intermediate list.

Generated scalar ordinals, row IDs, and bit positions remain private.
The direct views and the composite-only fused index have independent checked
static-size budgets in the manifest. The Unicode 17 composite contains 615
rows: its U16 page index is 93,184 bytes, its eleven packed U8 columns are
6,765 bytes, and its total logical payload is bounded at 99,949 bytes.
Property-specific defaults are parsed as
ordered ranges and checked before explicit records take precedence, including
all six Emoji defaults, East_Asian_Width's five wide unassigned ranges, the 24
ordered Bidi_Class defaults, Joining_Type's transparent derivation, and
Vertical_Orientation's explicit upright unassigned ranges.
Algorithm-specific fused views, such as grapheme data, remain separate from
these canonical facts. Deduplicated page layouts are chosen deterministically;
the manifest records and enforces their page width, checked index type, logical
byte count, and growth budget. ASCII lookups bypass those page indexes.

## Scalars and UTF-8

`Scalar` is the sealed text-processing value. `Scalar.from_u32` rejects both
surrogates and values above `U+10FFFF`; `Scalar.iter` traverses a valid Roc
`Str` without an impossible UTF-8 error branch. Every yielded value includes a
half-open `ByteRange` and an absolute scalar index. The iterator decodes only
the visited prefix, uses constant algorithmic state and stack, and creates no
intermediate byte or scalar list. It retains the source string for the
iterator's lifetime, while yielded scalars and ranges do not retain it.

`CodePoint` remains a distinct sealed type for the complete Unicode code-point
domain, including surrogates. It deliberately has no UTF-8 encoder or decoder;
callers validate it with `Scalar.from_code_point` before entering a text API.
UTF-8 encoding accepts only `Scalar`, so it cannot emit surrogate encodings.
`Scalar.to_str` validates the exact encoded bytes and reports a stable internal
fault instead of using lossy recovery. Property-specific queries, such as
`EastAsianWidth.of_scalar`, accept `Scalar` without coupling property tables to
the scalar representation.

Arbitrary bytes use the separate `Utf8.Cursor`. `push` accepts byte chunks and
returns `Pushed`, caller-requested `Stopped`, or a terminal `Failed` with an
indexed typed error; a chunk edge is never end-of-text. `Stopped.consumed`
identifies the unvisited suffix so callers can resume it with the returned
cursor, which does not retain the chunk. `finish` explicitly returns `End` or
reports an incomplete trailing sequence. The cursor retains at most three
pending bytes, uses constant stack and auxiliary storage, and performs no
allocation itself. Error offsets and all emitted scalar/range coordinates are
absolute from the beginning of the logical byte source.

The package cannot turn a host allocator abort into a Unicode error. It avoids
predictable growth instead: scalar encoding has a fixed one-to-four-byte
result, appending accepts and checks a caller-selected output bound before
reserving, and later algorithms use operation-specific limits where they
intrinsically retain input-proportional state. There is intentionally no
universal options or limits record.

Grapheme segmentation follows the default, un-tailored Unicode 17 extended
grapheme cluster algorithm. Its primary output is half-open UTF-8 byte ranges;
callers can consume them lazily with `Grapheme.iter_ranges`, collect them with
`Grapheme.ranges`, retain seamless source slices with `Grapheme.slices`, or
request independent copies with `Grapheme.owned`. `Grapheme.Cursor` carries the
same algorithm across scalar-aligned streaming chunks without treating a chunk
edge as end-of-text.

Line breaking follows the exact default Unicode 17 UAX #14 revision 55
algorithm and reports logical opportunities rather than choosing a line width.
`LineBreak.iter_boundaries` lazily visits every scalar boundary with its
`TextPosition`, `Mandatory`/`Allowed`/`Prohibited` decision, and whether the
governing rule is tailorable. `LineBreak.iter_opportunities` and
`LineBreak.Cursor` emit only allowed or mandatory opportunities; the cursor
accepts scalar-aligned `Str` chunks and is explicitly finished. Exhaustive
replayable traversal uses bounded forward lookahead, while the non-replayable
cursor retains only bounded algorithm state and never a source chunk. The
Unicode default is the no-configuration path; `PreserveGraphemes` is an
explicit restriction profile that reuses the package's grapheme transition
core. Its package policy is independently versioned as
`LineBreak.preserve_graphemes_revision`, and `LineBreak.profile_revision`
distinguishes that policy axis from the Unicode/UAX version.

## Bidirectional text

`Bidi` implements Unicode 17.0.0 UAX #9 revision 51 at conformance level
UAX9-C1: P1--P3, X1--X10, W1--W7, N0--N2, I1--I2, and the line-specific
L1--L4 steps. `Bidi.analyze_paragraph` retains paragraph analysis facts rather
than replacing logical text: original scalar/text ranges, resolved levels,
logical runs, X9-removal state, paired brackets, and mirroring information.
`Bidi.paragraph_ranges` follows P1, assigning paragraph separators to their
preceding range and treating CRLF as one separator. Empty input returns one
empty paragraph range; a final separator does not add an artificial empty
paragraph.

For a paragraph selected from a larger `Str`, use
`Bidi.analyze_range(source, paragraph_range, direction, limits)`. It validates
that the supplied `TextRange` is one of the P1 ranges and retains absolute byte
and scalar coordinates from the full source; line ranges, visual-to-logical
mappings, and visual runs use those same coordinates. `Bidi.reorder_line`
then applies L1/L2 to an actual, paragraph-contained logical line. It does not
shape Arabic or replace source scalars: L4 is returned as a mirrored-glyph
requirement and optional best-fit mapping for renderers. Paragraph limits are
checked before retained analysis is committed, with typed errors identifying
the ingestion stage and source range.

## Scripts and shaping-oriented itemization

`Script` exposes Unicode 17's normative `Script` and `Script_Extensions`
properties from `Scripts.txt`, `ScriptExtensions.txt`, and
`PropertyValueAliases.txt` under UAX #24 revision 39. A script is a character
property, not a Unicode block, language, text direction, font choice, or
security classification. `Common` (`Zyyy`), `Inherited` (`Zinh`), and
`Unknown` (`Zzzz`) are ordinary property values rather than errors.

`Script.of_scalar` is total over `Scalar`. `Script.extensions_of_scalar`
returns a nonempty opaque `ScriptSet`: an absent Script_Extensions override is
the normative singleton `{ Script(cp) }`, not an empty set. Membership,
length, intersection, equality, and ordered walking use a fixed three-word
view without per-lookup allocation. `Script.to_list` is the explicit
materializing convenience. Public set traversal and comparison use canonical
short-alias lexicographic order; generated private IDs and bit positions are
not public identities.

All `sc` aliases declared by the pinned UCD are accepted with UAX44-LM3 loose
matching, including compatibility aliases such as `Qaai` and `Qaac`. Matching
is ASCII-case-insensitive, ignores Unicode whitespace, hyphens, and underscores,
and removes one initial `is` prefix after those ignored characters. It does not
guess other ISO 15924 codes that are absent from the UCD `sc` namespace.
Canonical short and long aliases are available without constructing strings.

`ScriptItemization.ConservativeScxV1` is an independently named package policy,
not a Unicode-defined universal itemization algorithm. It treats extended
grapheme clusters atomically, resolves restricted Script_Extensions candidates
from explicit neighbors and caller preferences, resolves broadly Common spans
only when their explicit context agrees, preserves Unknown as a propagation
barrier, and coalesces adjacent equal results. It deliberately does not infer
language or paired-punctuation ownership. Each run carries a `TextRange`, so
its byte and scalar coordinates describe the same source span without retaining
that source.

Complete strings can be traversed lazily with `ScriptItemization.iter_runs`,
folded without collecting with `fold_runs`, or materialized with `runs`.
Exact right-context resolution uses interval-local replay: every scalar is
classified at most twice, and the iterator retains neither an
input-proportional coordinate tape nor copied substrings. A preference list is
retained rather than copied and scanned in order for each restricted cluster,
so its resolution cost is O(P); applications should keep it short.

For non-replayable scalar-aligned chunks, `ScriptItemization.Cursor` retains
only compact unresolved grapheme descriptors under an explicit
`max_pending_units` bound. The bound is checked before retaining the crossing
descriptor. Limit and coordinate failures return the caller state unchanged
and make the returned cursor terminal; chunks and slices are never retained.
Chunk boundaries do not imply end of text.

Validate the sources and check generated Roc modules with:

```sh
python3 scripts/unicode_data.py validate
python3 scripts/unicode_data.py generate --check
```

Regenerate them with `scripts/rebuild.sh`.

## Tests

Each purpose-built test app lives in `tests/apps/<suite>/main.roc` with an
adjacent `spec.json`. The runner builds optimized binaries once, sends cases
over a versioned stdin protocol, and executes deterministic shards in parallel.
The example runner independently builds the package bundle, serves only that
archive on loopback, rewrites temporary copies of every example to use it,
formats/checks/tests/builds each application once, and then reuses those
binaries for all named spec cases. The spec discovers every example, requires
both successful and failing paths without numeric coverage thresholds, and
uses exact output snapshots for deterministic behavior. Adding an unlisted
example, omitting either outcome, duplicating a name, or leaving a spec field
unrecognized fails the suite.

```sh
ROC=roc scripts/all_tests.sh
python3 scripts/test.py grapheme
python3 scripts/test.py line-break --jobs 8
python3 scripts/test.py properties --jobs 8
python3 scripts/test.py allocations
python3 scripts/test_bundle_examples.py
```

The grapheme suite covers all 766 official Unicode 17 conformance cases, and
the line-break suite covers all 19,338 official Unicode 17 cases through both
complete-string and one-scalar-per-chunk paths. The property suite covers every
valid Unicode scalar for grapheme-break, East-Asian-width, and emoji
properties. A separate allocation harness records the package's allocation
behavior. Its Linux x64 counts are exact measurements
for the compiler in `.roc-version`: when an intentional implementation change
alters them, rerun `scripts/test.py allocations` with that pinned compiler and
review the measured fixture counts before updating the adjacent baseline file.
The runner never updates or silently accepts a new allocation baseline.
Hand-authored package modules and all examples are gated by `roc fmt --check`.
Compact generated Unicode modules remain byte-for-byte governed by
`python3 scripts/unicode_data.py generate --check`; running the formatter over
those generated tables would change the canonical generator output and expand
its dense literals substantially.

## Benchmarks

Opt-in local benchmarks live in [`benchmarks`](benchmarks/README.md). They are
kept outside CI and include a reproducible Unicode 17 grapheme comparison with
Rust and Go plus an official-conformance-corpus line-break benchmark.
