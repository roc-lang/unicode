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
versioned Unicode files in `vendor/unicode/18.0.0/`.
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
static-size budgets in the manifest. The Unicode 18 composite contains 627
rows: its U16 page index is 94,720 bytes, its eleven packed U8 columns are
6,897 bytes, and its total logical payload is bounded at 101,617 bytes.
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

Grapheme segmentation follows the default, un-tailored Unicode 18 extended
grapheme cluster algorithm. Its primary output is half-open UTF-8 byte ranges;
callers can consume them lazily with `Grapheme.iter_ranges`, collect them with
`Grapheme.ranges`, retain seamless source slices with `Grapheme.slices`, or
request independent copies with `Grapheme.split`. `Grapheme.Cursor` carries the
same algorithm across scalar-aligned streaming chunks without treating a chunk
edge as end-of-text.

Line breaking follows the exact default Unicode 18 UAX #14 revision 57
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

[Thai dictionary line breaking](docs/thai-line-breaking.md) is available for
complete strings under an explicit policy with a caller-supplied dictionary
and byte limits.

## Sentence segmentation

Sentence segmentation follows the Unicode default UAX #29 C3-1 rules.
`Sentence.iter_ranges`, `Sentence.fold_ranges`, and `Sentence.ranges` report
the same half-open UTF-8 byte ranges. `Sentence.slices` retains the source
backing storage and `Sentence.owned` makes independent copies. The default
can split at abbreviations such as “Mr.”. The `_limited` collectors enforce
input-byte and output-item budgets and fail atomically.

`Sentence.Cursor` accepts scalar-aligned chunks and emits the same ranges as
the complete-source API. After an ambiguous period its one pending boundary
can wait for arbitrarily distant lowercase right context. The cursor retains
only state and the pending coordinate. Explicit completion emits any pending
boundary and the final nonempty range.

## Bidirectional text

`Bidi` implements Unicode 18.0.0 UAX #9 revision 52 at conformance level
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
mappings, and visual runs use those same coordinates. This range API validates
through the selected P1 boundary and replays the selected paragraph for
analysis; `Bidi.analyze_paragraph` decodes its single paragraph once.
`Bidi.reorder_line`
then applies L1/L2 to an actual, paragraph-contained logical line. It does not
shape Arabic or replace source scalars: L4 is returned as a mirrored-glyph
requirement and optional best-fit mapping for renderers. Paragraph limits are
checked before retained analysis is committed, with typed errors identifying
the ingestion stage and source range.

For a requested line whose scalar range starts at `line_start`,
`visual_to_logical[visual_position]` is an absolute source scalar index, while
`logical_to_visual[absolute_scalar_index - line_start]` is the matching visual
position (or absent for X9-removed controls). This intentionally makes the
forward mapping directly usable against retained source coordinates while the
inverse stays compact for the requested line.

## Word segmentation and case mapping

Word segmentation implements the un-tailored Unicode 18.0.0 UAX #29 word
boundary rules. `Word.iter_ranges` and `Word.ranges` return every nonempty
UTF-8 range between boundaries (including whitespace and punctuation), while
`Word.Cursor` carries the same bounded transition across scalar-aligned `Str`
chunks and must be explicitly finished. Ranges partition the original source;
`slices` retains seamless views and `owned` copies selected text.

`Case` implements Unicode 18.0.0 full lowercase, uppercase, and technical R3
titlecase mappings. The default profile is locale-independent; `Turkic` and
`Lithuanian` are explicit profiles, never ambient locale choices. Folding has
explicit full, simple, Turkic-full, and Turkic-simple profiles. Every
successful Case result owns its transformed `Str` and one source-mapping fact
per input scalar, with byte/scalar ranges, shape, and contextual provenance.
Case mapping never normalizes before or after conversion. Callers provide
explicit input, output, scalar, and fact limits; a limit error is typed and
atomic rather than a partial result.

## Locale identifiers

`Locale.parse` validates Unicode BCP 47 locale identifier syntax. It supports
language, script, region and variants, Unicode `u` and transformed `t`
extensions, other singletons and trailing private use. `Locale.to_str`
formats the identifier with consistent casing and component order, and
`Locale.root` formats as `und`. Use `Locale.parse_limited` to enforce a byte
budget before parsing untrusted text. A successful parse establishes syntax.
Registered aliases, likely subtags, data availability, and locale fallback
require a consumer with the relevant locale catalog.
[UTS #35, Unicode Locale Identifier](https://www.unicode.org/reports/tr35/tr35-79/tr35.html#Unicode_locale_identifier)

```roc
import unicode.Case
import unicode.Locale

lower_for_locale = |text, locale| {
    profile = Case.mapping_profile_for_locale(locale)
    Case.to_lower(text, profile, Case.unlimited_limits)
}
```

`Case.mapping_profile_for_locale` selects the UCD's Turkic casing for Turkish
and Azerbaijani and Lithuanian casing for Lithuanian. Other languages use
default casing. `Case.full_fold_profile_for_locale` selects the Turkic full
fold for Turkish and Azerbaijani. These explicit choices use the language
subtag. For collation or other locale data consumers,
`Locale.unicode_keyword` can retrieve values such as `co=phonebk` from
`de-u-co-phonebk`.

## Scripts and shaping-oriented itemization

`Script` exposes Unicode 18's normative `Script` and `Script_Extensions`
properties from `Scripts.txt`, `ScriptExtensions.txt`, and
`PropertyValueAliases.txt` under UAX #24 revision 41. A script is a character
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

## Public API safety

Valid Roc `Str` APIs process every Unicode scalar, including NUL, noncharacters,
and unassigned values. Arbitrary bytes use `Utf8.Cursor`, which reports
malformed input at an absolute byte offset and preserves the already
irrevocable prefix. EOF and caller stop have distinct outcomes.

Caller-provided ranges are ordered before construction and checked against
the relevant source or analysis before use. Requested optional behavior
reports its own unsupported or invalid configuration error where applicable.
Collectors with explicit budgets report typed exhaustion without returning a
partial collection. Detectable implementation faults in fallible APIs are
distinct from caller errors. A host allocator failure can terminate Roc.

Use the `*_limited` segmentation collectors to cap input bytes and output
items. `Case` and `Bidi` accept feature-specific bounds on transformed or
retained results. Lazy iterators allow early stop, while streaming cursors
require explicit completion and retain no consumed chunk. The
[public API audit](docs/api-safety.md) records per-operation behavior, resource
bounds and invariant evidence. Run its focused checks with
`python3 scripts/test.py safety`.

## Tests

The [testing guide](docs/testing.md) describes how to build and review
correctness oracles, conformance checks, and resource evidence.

Purpose-built test apps live under `tests/apps` with adjacent
`spec.json` files. The word and sentence suites share an app. The runner builds optimized binaries once, sends cases
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
python3 scripts/test.py word --jobs 8
python3 scripts/test.py sentence --jobs 8
python3 scripts/test.py case --jobs 8
python3 scripts/test_case_icu.py --download
python3 scripts/test.py properties --jobs 8
python3 scripts/test.py allocations
python3 scripts/test_bundle_examples.py
```

The grapheme suite covers all 853 official Unicode 18 conformance cases, the
line-break suite covers all 19,346 official Unicode 18 cases through both
complete-string and one-scalar-per-chunk paths, and the word suite covers all
1,944 official Unicode 18 cases through complete and scalar-aligned chunked
paths. The sentence suite covers all 512 official Unicode 18 cases through
complete traversal and every scalar-aligned two-chunk split. The Case suite
covers 11,567 deterministic Unicode 18 oracle cases:
all simple mappings, all SpecialCasing witnesses and controls, all CaseFolding
records under every public fold profile, and focused source-fact, R3,
no-normalization, context, supplementary, and exact-limit checks. The property suite covers every
valid Unicode scalar for grapheme-break, East-Asian-width, and emoji
properties. A separate allocation harness records the package's allocation
behavior. Its Linux x64 counts are exact measurements
for the compiler in `.roc-version`: when an intentional implementation change
alters them, rerun `scripts/test.py allocations` with that pinned compiler and
review the measured fixture counts before updating the adjacent baseline file.
The runner never updates or silently accepts a new allocation baseline.
`scripts/test_case_icu.py` is an opt-in differential gate against ICU4J 79.1
(Unicode 18). It is deliberately outside the offline default suite: provide a
verified `ICU4J_JAR`, or pass `--download` to cache the pinned Maven artifact
under `.roc-unicode-tmp/case-icu`. It compares lower/upper/title under root,
Turkic, and Lithuanian locales plus default full folding over every pinned case
mapping source and focused context strings. ICU4J exposes an exact full-fold
oracle here; it does not provide this package's C+S simple-fold contract, so
simple profiles remain covered by the independent pinned-data oracle. ICU title uses its word-break
iterator rather than Core R3: the gate explicitly reports its known isolated
U+0345 and leading-number word-title non-comparisons, then treats every other
observed result mismatch as a failure rather than silently accepting a locale
or boundary difference.
Hand-authored package modules and all examples are gated by `roc fmt --check`.
Compact generated Unicode modules remain byte-for-byte governed by
`python3 scripts/unicode_data.py generate --check`; running the formatter over
those generated tables would change the canonical generator output and expand
its dense literals substantially.

## Benchmarks

Opt-in local benchmarks live in [`benchmarks`](benchmarks/README.md). They are
kept outside CI and include a reproducible Unicode 18 grapheme comparison with
Rust and Go plus an official-conformance-corpus line-break benchmark.
