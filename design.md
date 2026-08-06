# Unicode package architecture

## Purpose

This document defines the enduring architecture of the Roc Unicode package. It
describes the contracts that implementations and public APIs must preserve as
the package grows to cover text segmentation, line breaking, bidirectional
text, script itemization, and Unicode properties.

The design is organized around two requirements that are equally important:

1. Results must implement a named, versioned Unicode specification exactly.
2. The common path must be suitable for high-performance text engines: a
   single forward scan where the algorithm permits it, no per-scalar
   allocation, explicit ownership, bounded retained state, and specialized
   SIMD fast paths.

This is not an implementation plan, a description of the current code, or a
testing strategy. Concrete encodings, table dimensions, and SIMD thresholds
are deliberately private. They may change without changing this design.

## Vocabulary

The terms in this document have precise meanings:

- A **code point** is an integer in `U+0000..U+10FFFF`. This domain includes
  surrogate code points.
- A **scalar** is a Unicode scalar value: a code point excluding
  `U+D800..U+DFFF`. Every value in a Roc `Str` is a scalar.
- A **byte range** is a half-open interval `[start, end)` in the original
  UTF-8 byte sequence.
- A **scalar range** is a half-open interval in the sequence of decoded
  scalars.
- A **text position** carries the corresponding byte and scalar offsets at one
  scalar boundary.
- A **text range** carries both byte and scalar coordinates when both are
  required.
- A **complete source** is a `Str` whose end is known.
- A **chunk** is one part of a source whose end does not imply the end of the
  logical text.
- A **profile** is an explicit, named choice of permitted tailoring or
  higher-level policy. It is not part of the Unicode data version.

## Architectural invariants

All package features obey the following invariants.

### Correctness and identity

- The default behavior of a Unicode algorithm is the exact, un-tailored
  behavior of its named Unicode conformance clause. Convenient approximations
  never silently replace the default.
- Algorithms that identify spans or boundaries only partition the input. They
  do not normalize, insert, remove, reorder, or re-encode its bytes.
- Every reported range is monotonic, in bounds, and aligned to scalar
  boundaries. Adjacent ranges that claim to partition text cover it exactly
  without gaps or overlap.
- Unassigned and private-use scalars are ordinary valid inputs. Unicode
  property defaults apply to them; they are not errors.
- Every public operation over a valid `Str` is total with respect to text
  content. No valid sequence may reach a panic, an unreachable branch, or
  silent data loss.
- APIs over arbitrary bytes are separate from APIs over `Str`. They report
  malformed UTF-8 with an absolute byte offset and never disguise it as a
  Unicode property value.

### Work and storage

- Text is decoded at most once on a normal forward path. An API whose declared
  semantics require a second pass says so explicitly.
- Scalar traversal, property lookup, grapheme segmentation, and ordinary line
  breaking perform no heap allocation per scalar.
- A streaming core retains only algorithmic state and source coordinates. It
  does not retain consumed chunks or construct substrings.
- Algorithms use constant auxiliary state where their specification permits
  it. Algorithms that intrinsically require input-proportional state expose
  that fact in their API and provide feature-specific resource limits.
- Runtime Unicode lookup is bounded independently of the number of ranges in
  the source Unicode data. It is never a linear scan over all property ranges.
- Traversal has a constant stack bound. Recursion proportional to input length
  is not part of any processing model.
- A caller that stops a pull or fold traversal early does not pay to decode or
  classify the unvisited suffix.

### Stability and policy

- The Unicode data version, package version, and profile revision are separate
  version axes.
- Public property identities are Unicode identities, not generated table row
  numbers, bit positions, or enum ordinals.
- Ambient locale, platform, terminal, font, or CPU state never changes Unicode
  semantics. Such choices enter through explicit arguments or named profiles.
- Optimizations are semantics-preserving transitions through the same
  algorithm state as the scalar path. There is no second, approximate ASCII or
  SIMD algorithm.

## System shape

The package is a family of algorithm-specific processors built over common
source and data layers:

```text
 complete Str       scalar-aligned Str chunks       arbitrary byte chunks
      |                         |                              |
      +---------------- source adapters ----------------------+
                                |
               LocatedScalar { scalar, byte range,
                                scalar index }
                                |
                   narrow generated property view
                                |
                 algorithm-specific transition core
                     /                         \
          boundary/run events          retained paragraph analysis
                     |                         |
       Iter, folds, ranges, slices,       bidi line reordering,
       and collecting conveniences       levels, runs, mappings
```

“Common” does not mean “universal.” The source adapters, coordinate types,
version metadata, and generated property model are shared. Grapheme, line
breaking, bidi, and script itemization have different state, latency, restart,
and storage requirements; they do not inherit from one maximal text-machine
abstraction.

Similarly, the hot path is not a chain that materializes
`bytes -> scalars -> all properties -> algorithm tokens`. Source decoding,
the exact property projection needed by an algorithm, and its state transition
are fused into one loop. The boundaries between these concepts remain clear in
the implementation without forcing intermediate collections or records to
exist at runtime.

## Scalars, UTF-8, and coordinates

### Scalar is the text-processing unit

`Scalar` is the primary input type for encoding and Unicode property queries.
It is constructed only after excluding surrogates. UTF-8 decoders yield
`Scalar`, and UTF-8 encoders accept `Scalar`.

`CodePoint` remains useful for the complete Unicode code-point domain and for
interchange with UTF-16-oriented concepts. A `CodePoint` that is a surrogate
cannot be encoded as UTF-8 or passed directly to a scalar property lookup. An
API accepting `CodePoint` at such a boundary must validate it and return a
typed error; it must not give surrogates the defaults for unassigned scalars.

This distinction makes invalid states difficult to construct and removes
repeated surrogate checks from text hot paths.

### Located scalars

The common logical decoder output is a value equivalent to:

```text
LocatedScalar {
    scalar : Scalar,
    byte_range : ByteRange,
    scalar_index : U64,
}
```

Byte offsets are always relative to the beginning of the logical source, not
to the current chunk. Offset arithmetic is checked. A source adapter may omit
the scalar index internally when its consumer does not need it, but public
coordinates have the same meaning across all algorithms.

`ByteRange` is the primary currency for lossless segmentation because it is
compact, does not retain input storage, and can be converted to a seamless
slice by a caller that owns the source. `TextRange` carries both coordinate
systems for results, such as bidi mappings, that must bridge byte-oriented and
scalar-oriented consumers.

### Valid strings and arbitrary bytes

A Roc `Str` already guarantees valid UTF-8. Scanning it is total and has no
UTF-8 error channel. This keeps errors that cannot occur out of the common API
and lets algorithms rely on scalar validity.

The `Utf8` byte-decoding API serves files, networks, and other untrusted byte
sources. Its error identifies the malformed sequence and absolute byte offset.
A decoding step either produces one complete scalar and advances over all of
its bytes, or produces no scalar. A chunked decoder may retain at most the
three trailing bytes of an incomplete sequence until the next chunk or
explicit completion.

This separation prevents every Unicode algorithm from carrying a spurious
error branch while preserving a precise, reusable path for untrusted bytes.

## One transition core, several ways to drive it

Each algorithm is defined by state initialization, a transition for one
located scalar, and explicit end-of-text completion. All public entry points
for that algorithm drive these same transitions.

### Complete strings

Pull-based traversal over a complete `Str` is exposed as `Iter` where the
result is total. It is lazy, supports early termination, and does not construct
an intermediate list. A collecting convenience consumes that iterator or the
same core and allocates only for its returned result.

Internally, the decoder, property lookup, and transition use a fused,
imperative-style loop with state passed by value. The public use of `Iter` does
not require the internal hot path to be expressed as multiple composed
iterators. This preserves ergonomic composition without imposing abstraction
or callback overhead between every byte and state transition.

Fallible traversal is not forced into a plain `Iter` if the iterator has no
terminal error channel. It uses an explicit decoder or processor result whose
success, failure, and completion states cannot be confused.

### Chunked sources

Chunk processing follows a shared semantic protocol but uses
algorithm-specific opaque cursor types. Conceptually, every cursor supports:

```text
init(configuration) -> Cursor
push(cursor, chunk, emit) -> PushResult
finish(cursor, emit) -> FinishResult
```

The exact Roc spelling may use folds or returned state. The following behavior
is part of the contract:

- A chunk boundary is never interpreted as end of text.
- Empty chunks are no-ops.
- `finish` is explicit and may be called exactly once.
- No result is emitted until it is irrevocable; an emitted result is never
  revised or withdrawn.
- `finish` resolves any decision that depends on end of text.
- The cursor does not retain a consumed chunk or a slice of it.
- Offsets remain absolute across pushes and are checked for overflow and
  discontinuity.
- Scalar-aligned `Str` chunks and arbitrary byte chunks are distinct source
  types. Only the latter needs partial UTF-8 state.
- State is tied to the algorithm, Unicode version, and profile that created it.
  It cannot be resumed under different semantics.
- Pushing after completion or continuing after a terminal error returns a
  typed state error.

The protocol deliberately does not promise restart from an arbitrary scalar
boundary. A scalar boundary is not necessarily a grapheme, line, script, or
paragraph boundary, and the preceding context may be semantically required.
An algorithm may expose checkpoints only where it can state sufficient
preconditions and capture all required context.

Not every result family available for a complete source is also available on
a non-replayable chunk stream. In particular, an algorithm may be able to
retain one pending semantic decision while an unbounded number of already
known, lower-level results follow it. Preserving result order would then
require an unbounded coordinate queue even though the algorithm's recognition
state is finite. The chunk API exposes the result family that can be emitted
irrevocably with bounded state. An exhaustive complete-source API may instead
replay a source span, provided it declares the replay and retains neither the
text nor an input-proportional coordinate list.

### API families

The public surface follows consistent families without manufacturing a single
generic processor type. The following signatures illustrate the intended
shape; the distinctions they express are architectural even if names evolve:

```text
Scalar.iter : Str -> Iter(LocatedScalar)

Grapheme.iter_ranges : Str -> Iter(ByteRange)
Grapheme.ranges : Str -> List(ByteRange)

LineBreak.iter_boundaries : Str, LineBreakProfile -> Iter(BreakBoundary)
LineBreak.iter_opportunities : Str, LineBreakProfile -> Iter(BreakOpportunity)

Property.of_scalar : Scalar -> PropertyRow
Property.iter_runs : Str, projection -> Iter(PropertyRun)

Bidi.analyze_paragraph : Str, BaseDirection, BidiLimits
    -> Try(BidiAnalysis, BidiError)
Bidi.logical_runs : BidiAnalysis -> Iter(LevelRun)
Bidi.reorder_line : BidiAnalysis, ScalarRange
    -> Try(LineOrder, BidiError)
```

Algorithms expose chunk cursors only when chunking is semantically meaningful.
They share the protocol above, not a lowest-common-denominator result type.

## Ownership and materialization

Returning a range, a seamless slice, and an owned string have materially
different memory behavior. The API makes that distinction visible.

- **Ranges** are the primitive segmentation result. They allocate no text and
  retain no reference to the source.
- **Slices** are zero-copy views. They retain the source backing allocation for
  as long as any slice lives and incur the reference-count behavior of `Str`.
- **Owned strings** copy the selected bytes and can release the original source
  independently.

An API that returns `Str` documents whether it returns seamless slices or owned
values; these behaviors are not silently exchanged as optimizations. In
particular, APIs use explicit range, slice, and owned materializers rather than
making a caller infer retention behavior from a generic “split” operation.

An iterator over a `Str` may retain that source for the lifetime of the
iterator, but individual range results do not. Chunk cursors do not retain
previous chunks. Collectors allocate in proportion to the values they return,
not the number of intermediate decoding or property steps.

Bidi analysis is a special case because it intrinsically retains a paragraph.
Its opaque analysis stores compact scalar identities, coordinates, working
classes, levels, and algorithm flags; it does not retain the source `Str` merely
to recover slices later. Accessors return ranges, levels, runs, and mapping
information from this retained representation.

## Unicode data architecture

Unicode algorithms are code plus versioned data. Data provenance and runtime
representation are therefore architectural concerns rather than generator
details.

### One canonical source graph

A single machine-readable manifest declares the selected Unicode release,
Emoji release, relevant specification revisions, every source file, its stable
source location, checksum, and license/provenance information. All generated
views derive from that graph. A package cannot combine files or rules from
different Unicode releases.

The generator first builds a canonical logical model. It preserves:

- each property's source-defined default, including nontrivial `@missing`
  ranges;
- mutually exclusive enumerated property values;
- independent and overlapping binary properties;
- exact numeric canonical combining classes;
- optional mappings whose absence is semantically meaningful;
- official property and value aliases; and
- `Script_Extensions` as nonempty sets rather than one selected script.

Unknown values, malformed records, conflicting overlaps, missing declared
defaults, and mixed source versions are generation errors. New Unicode values
must become an explicit, reviewable change rather than falling into a runtime
`Unknown` bucket.

The exact Unicode version is exposed publicly as `UnicodeVersion`. It describes
data and algorithm semantics, not the package release. Upgrading it is expected
to change some property values and boundaries without requiring an API shape
change.

### Multiple narrow runtime views

The canonical model generates the runtime views that consumers actually need.
It does not force all properties into one universal scalar row.

Typical fused views include:

- grapheme cluster break, Indic conjunct break, and extended pictographic;
- effective line-break class plus only the general-category, width, and emoji
  bits required by line breaking;
- bidi class plus bracket and mirroring facts; and
- script, script-extensions identity, combining class, joining properties,
  Indic properties, vertical orientation, ignorables, variation selectors,
  and emoji facts used before shaping.

Sparse scalar-to-scalar mappings, such as mirroring and paired brackets, are
stored separately from dense property rows. Algorithmically derivable ranges,
such as Hangul syllable classes, may be computed instead of tabulated when that
reduces size without slowing the common path.

“Shared data” means shared definitions, provenance, and generated facts. It
does not require one physical lookup for every consumer. Deliberately
duplicating a few bits into two algorithm-specific rows is allowed when it
reduces total code size or hot-path work. Accidental duplication of independent
databases with separate semantics is not.

### Bounded static lookup

Dense properties use a generated indexed representation such as a two-stage,
deduplicated page table. Pages may be uniform, dense, or locally encoded, and
identical pages may share storage. Page dimensions, row widths, tags, and the
choice to use a flat table for a small property are private performance
decisions.

The representation must provide:

- bounded lookup with no scan over source ranges;
- direct fast access for ASCII;
- no heap allocation for a scalar or composite lookup;
- immutable data linked into the program, with no runtime parsing or
  decompression;
- narrow projections so an algorithm does not decode unrelated fields; and
- module boundaries that do not force programs to link large unused property
  families.

Internal row IDs, page IDs, enum ordinals, bit positions, and SIMD widths are
never public. This allows table compaction and specialization to evolve without
an API or serialized-data compatibility burden.

### Public property view

`PropertyRow` is an opaque, allocation-free view of the supported scalar
properties. Typed accessors expose facts such as general category, exact
canonical combining class, bidi class, script, joining type, vertical
orientation, and independent emoji booleans. Direct property-specific queries
are also available so asking for one property does not construct or decode the
whole public view.

Optional mappings return an option. Enumerated values use typed public
identities with their official aliases. Binary properties remain independent:
a scalar may simultaneously have several emoji properties, for example, so a
first-match tagged union would be lossy. Emoji variation presentation is an
optional lookup for a valid base-and-variation-selector pair, not a property
invented for every pair.

`ScriptSet` is opaque and backed by an interned immutable pool. Its public
operations are membership, intersection, iteration, and comparison; its
internal bitset width and script ordering are not exposed.

Property scans can return maximal adjacent runs for a selected projection.
Each run contains a `TextRange` and the selected typed value. These scans
decode once and compare only that projection rather than constructing a
complete property object for every scalar.

## Algorithm-specific architecture

### Extended grapheme clusters

The default grapheme algorithm implements un-tailored extended grapheme
clusters from UAX #29 for the selected Unicode version.

Its state is finite and independent of cluster length. It contains only the
context required by the rules: the current cluster start, previous break
class, regional-indicator parity, extended-pictographic/ZWJ context, and Indic
conjunct context. A boundary before the current scalar is emitted as soon as it
is known; completion emits the final nonempty cluster.

Ranges are the primitive output. Because the machine stores the start offset
rather than cluster content, even an arbitrarily long cluster uses constant
auxiliary memory. Debug rule traces, when desired by tooling, are a separate
observer and never part of the production token stream.

The lossless partition invariant is especially strong here: concatenating all
grapheme ranges always selects exactly the original bytes.

### Line breaking

The default line-break algorithm implements UAX #14 for the selected Unicode
version. It produces decisions at logical boundaries; it does not choose a
line width or perform layout.

A boundary decision has both a value and an authority:

```text
BreakBoundary {
    at : TextPosition,
    decision : [Mandatory, Allowed, Prohibited],
    authority : [NonTailorable, Tailorable],
}
```

Keeping authority prevents a tailoring layer from accidentally overriding
the non-tailorable rules. An opportunity-only iterator may filter prohibited
boundaries, while a boundary iterator retains the information required by
layout engines and profiles.

Some line-break rules require right context. The cursor delays such a decision
and retains the necessary coordinates, never the intervening text. Chunk ends
do not resolve pending decisions. A non-replayable chunk cursor emits only
break opportunities, so boundaries already known to be prohibited while an
earlier boundary decision remains pending do not form an ordered output queue.
Its state remains bounded even when a run of characters causes arbitrarily
long output latency.

Exhaustive traversal that reports `Prohibited` at every scalar boundary is a
replayable-source operation. It may look ahead to resolve a pending decision and
then replay that source span to emit intervening boundaries in order. The
implementation keeps total decoding and classification work linear and does
not retain the span or its boundary coordinates. This replay is part of the
declared semantics of the exhaustive API; the opportunity iterator and chunk
cursor do not pay for it. For example, in
`PR OP CM* X`, the decision at `PR|OP` can depend on `X`, while every boundary
inside the combining-mark run is already prohibited. An exact ordered chunk
stream of all of those boundaries cannot have both bounded storage and no
replay.

Grapheme preservation is an explicit line-break profile. It consumes grapheme
boundaries from the same grapheme transition logic in lockstep; it does not
copy or reimplement the grapheme rules. Other tailoring is expressed as a
named, versioned profile constrained by UAX #14, not as an ambient callback
that can violate mandatory behavior.

### Bidirectional text

The Unicode Bidirectional Algorithm is intrinsically a paragraph algorithm.
It cannot provide complete, conformant results with constant auxiliary memory
or irrevocable scalar-by-scalar output. The API makes this different shape
explicit instead of presenting bidi as another streaming boundary iterator.

Ingestion may be chunked, but analysis retains a compact tape proportional to
the number of paragraph scalars. The tape preserves original scalar identity
and coordinates while keeping original and working bidi classes, embedding
levels, and the links or flags needed by the algorithm. Operations that remove
characters for rule processing retain enough identity to map every result back
to logical input.

The analysis object is opaque. It exposes levels and logical runs, and it
accepts a chosen line range for the line-specific L1/L2 reordering stage. This
separation is required because line wrapping is decided after paragraph-level
bidi analysis, while the final resets and visual order depend on each actual
line. Mirroring is exposed as information for the rendering or shaping stage;
the package does not silently replace source scalars.

Embedding and bracket-stack limits mandated by UAX #9 are algorithm semantics,
not resource failures. Separately, `BidiLimits` gives the caller a meaningful
paragraph scalar/byte budget before proportional storage is committed. A
limit failure is typed and atomic; it never returns a partially usable
analysis.

### Script properties and itemization

`Script` and `Script_Extensions` are scalar properties from UAX #24. Script
itemization is a higher-level policy built from those facts; Unicode does not
define one universal shaping-run algorithm. Public names and documentation
preserve this distinction.

Itemization treats an extended grapheme cluster as the smallest atomic unit so
a shaping run never divides a user-perceived cluster. Grapheme boundaries and
script candidate-set calculation are fused in one traversal rather than
materializing one list to feed another.

A conservative Script_Extensions policy can require unbounded right context:
an arbitrarily long run of ambiguous units may be resolved by a later strong
script. It is impossible to combine exact right-context resolution,
irrevocable immediate output, one classification per scalar, and constant
auxiliary storage for all inputs. The API therefore makes the tradeoff
explicit:

- On a complete `Str`, the exact conservative profile uses a documented
  two-pass/rescan model and does not allocate an unresolved copy of the text.
- On a non-replayable chunk stream, the exact profile buffers compact pending
  unit descriptors under an explicit limit and returns a typed limit error if
  that bound is exceeded.
- A low-latency, constant-state left-context policy may be offered under a
  different name. It is never described as equivalent to the conservative
  profile.

This is a semantic distinction between profiles, not an implementation detail.

### Properties for shaping and layout

The property layer supplies immutable Unicode facts needed before font-specific
shaping: general category, combining class, bidi and bracket facts, joining
properties, Indic categories, default ignorables, variation selectors,
vertical orientation, scripts, and emoji presentation facts.

It does not select fonts or glyphs and does not implement OpenType, AAT,
Graphite, fallback, substitution, positioning, or language-specific shaping.
These belong to a text engine that consumes this package's scalar properties,
ranges, script runs, and bidi results.

East Asian Width is exposed as a Unicode property, not converted into a
universal display-column count. Terminal width and glyph advance depend on
emoji presentation, variation selectors, grapheme context, locale, font, and
application policy. A generic `visual_width` result would conflate those
layers and is therefore outside this package's Unicode semantics.

## SIMD and fast paths

SIMD is used to skip work that can be proven unnecessary, particularly on
ASCII-heavy text. It is not used as a separate approximation of Unicode.

The primary byte fast path loads a portable vector such as Roc's `U8x16` and
uses the high-bit mask to identify an all-ASCII block. Algorithm-specific logic
then classifies the relevant ASCII subsets and updates the same transition
state that scalar processing would have produced. ASCII is not treated as one
class: CR, LF, controls, spaces, punctuation, digits, and letters have
different semantics in several algorithms.

Useful specializations include:

- runs of ordinary printable ASCII for grapheme boundaries;
- proven ASCII line-break classes;
- Latin-script runs for script property scans and itemization;
- an exact all-left-to-right subset for bidi; and
- equal-property runs and counting operations.

SIMD is bypassed when it would add work: short strings, non-ASCII-heavy input,
algorithms that must emit a distinct value for every scalar, or state for which
no exact block transition exists. A vector path never performs a full SIMD
validation followed by a duplicate scalar decode when its consumer needs the
scalars anyway.

Vectorization must not introduce a hidden copy. A source is vectorized only
when its byte storage can be borrowed without allocation; small inline strings
and unsuitable chunks use the scalar path. SIMD width, minimum length, number
of blocks per loop, and platform-specific selection remain internal.

Unicode property tables are not designed around arbitrary SIMD gathers that
Roc does not provide portably. Scalar lookup remains cheap and bounded; SIMD
accelerates byte regions whose Unicode behavior can be established from their
bytes alone.

## Resource and error contracts

There is no universal `Options` or `Limits` record. Each feature exposes only
limits that have clear semantic meaning for its intrinsic storage:

- raw UTF-8 decoding has a fixed trailing-byte bound;
- grapheme and line cursors have fixed algorithmic state;
- bidi exposes paragraph size limits;
- exact streaming script itemization exposes a pending-unit limit; and
- collectors may expose an output-item limit when the caller cannot accept an
  input-proportional result.

Limits are checked before committing the operation that would cross them.
Fallible collectors and retained analyses either succeed completely or return
a typed error; they do not return a prefix that looks complete. Streaming
processors may retain already emitted irrevocable results, but their terminal
status distinguishes normal completion, caller-requested early stop, malformed
bytes, resource exhaustion, and invalid cursor use.

Arithmetic over byte counts, scalar counts, capacities, and absolute offsets
is checked before conversion or allocation. Error values report source
coordinates and feature-specific cause without exposing private machine states
or generated table identities.

Out-of-memory termination by the Roc runtime is not presented as a recoverable
Unicode error. The package instead avoids unnecessary allocation and gives
callers semantic limits wherever the algorithm itself requires proportional
retained storage.

## Profiles and extensibility

The exact default is always available and easy to select. Tailoring and
application policy are explicit values with stable names and their own
revision. Examples include grapheme-preserving line breaking, a script
itemization policy, or locale-informed line tailoring.

A profile may:

- refine behavior only where the relevant Unicode conformance clause permits
  tailoring;
- add restrictions, such as preserving grapheme boundaries; and
- carry explicit locale or application choices needed by that policy.

A profile may not:

- redefine scalar properties;
- override non-tailorable or mandatory rules;
- depend on ambient process state; or
- claim the identity of an un-tailored Unicode default.

Serialized checkpoints, if any are ever public, include or are statically tied
to the Unicode version, algorithm revision, and profile revision. Ordinary
opaque cursors are process-local values and make no serialization promise.

New Unicode algorithms should reuse the scalar source, coordinates, canonical
data model, ownership vocabulary, and processor protocol. They should add an
algorithm-specific transition core and narrow property view rather than
expanding a universal state machine or forcing new fields into every existing
lookup.

## Architectural decision summary

| Decision | Reason |
| --- | --- |
| Use `Scalar` for text and property APIs | Surrogates cannot enter UTF-8 or masquerade as unassigned text. |
| Make ranges the primitive segmentation output | Ranges are lossless, allocation-free, and do not retain source storage. |
| Distinguish ranges, seamless slices, and owned strings | Their copying and retention behavior is materially different. |
| Use one transition core behind complete, chunked, and collecting APIs | Semantics cannot drift between convenience and high-performance paths. |
| Expose `Iter` for total pull APIs but fuse the internal hot loop | Callers get ergonomic laziness without mandatory iterator pipelines between bytes and state. |
| Keep algorithm cursors distinct | Grapheme, line, bidi, and script have irreducibly different latency and memory behavior. |
| Use one canonical Unicode source graph and several narrow runtime views | Provenance stays coherent while hot paths avoid decoding irrelevant properties. |
| Keep physical table encodings private | Layout can evolve with Unicode data, compiler behavior, and target platforms. |
| Make Unicode versions and tailoring profiles explicit | Results remain reproducible and defaults cannot change through ambient policy. |
| Treat bidi as retained paragraph analysis | UAX #9 intrinsically needs paragraph context and line-specific final reordering. |
| Make exact right-context script itemization replay or buffer explicitly | Constant memory and immediate irrevocable output are impossible for arbitrary ambiguous runs. |
| Keep exhaustive line-boundary traversal on replayable sources | A delayed right-context decision can precede unbounded prohibited boundaries, so an ordered non-replayable stream would require an unbounded coordinate queue. |
| Use SIMD only for exact block transitions | Fast paths preserve Unicode semantics on every target. |
| Expose East Asian Width as a property, not display width | Glyph and terminal width require font, grapheme, presentation, locale, and application policy. |

## References

The package manifest, rather than this document, pins the exact release and
revision of each source.

- [The Unicode Standard](https://www.unicode.org/standard/standard.html)
- [UAX #9: Unicode Bidirectional Algorithm](https://www.unicode.org/reports/tr9/)
- [UAX #14: Unicode Line Breaking Algorithm](https://www.unicode.org/reports/tr14/)
- [UAX #24: Unicode Script Property](https://www.unicode.org/reports/tr24/)
- [UAX #29: Unicode Text Segmentation](https://www.unicode.org/reports/tr29/)
- [UAX #44: Unicode Character Database](https://www.unicode.org/reports/tr44/)
- [UAX #50: Unicode Vertical Text Layout](https://www.unicode.org/reports/tr50/)
- [UTS #51: Unicode Emoji](https://www.unicode.org/reports/tr51/)
- [ICU CodePointTrie design](https://unicode-org.github.io/icu/design/struct/utrie.html)
- [Roc `Str` built-in documentation](https://www.roc-lang.org/builtins/Str)
