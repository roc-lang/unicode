# Thai dictionary line breaking

Use `ThaiLineBreak.opportunities` when wrapping Thai text containing words
without spaces. The whole-string collector returns the same ordered
`LineBreak.BreakOpportunity` values as the line-break API, with absolute UTF-8
byte offsets and scalar offsets. Callers choose line widths, shape text, and
select the breaks they use.

```roc
import unicode.ThaiLineBreak

thai_example = || {
	dictionary = ThaiLineBreak.Dictionary.from_words(
		["ภาษา", "ไทย"],
		"local-example-v1",
		{ max_words: 2, max_total_bytes: 21, max_word_bytes: 12 },
	).map_err(DictionaryError)?
	ThaiLineBreak.opportunities(
		"ภาษาไทย",
		dictionary,
		{ max_input_bytes: 64, max_run_bytes: 64 },
	).map_err(TailoringError)
}
```

This source gains an allowed opportunity at byte 12 and scalar 4 and has a
mandatory opportunity at byte 21 and scalar 7.

## Dictionary and policy

The core package ships no Thai lexicon. Supply an application dictionary
whose vocabulary, license, and provenance you can establish. Use a stable
revision identifying the exact word set, such as a content digest or a
versioned data asset. Record `ThaiLineBreak.Dictionary.revision`,
`ThaiLineBreak.profile_revision`, and `UnicodeVersion.current` when saving
reproducible layout evidence. The index retains the revision and an owned
trie. Duplicate words count toward the input bounds and have no effect on
matching.

Dictionary entries match exact source scalars. If an application chooses a
normalization policy, normalize its text and dictionary before calling and
interpret offsets in that input. Each word must begin with a Thai spelling
letter or spacing vowel and contain only Thai spelling characters. The
policy's alphabet is U+0E01–U+0E3A and U+0E40–U+0E4E, except the abbreviation
sign U+0E2F and repetition sign U+0E46. Standalone marks, digits, punctuation,
spaces, foreign scalars, and explicit break controls separate runs. A cluster
containing a foreign combining mark also separates runs.

The policy segments maximal runs of eligible extended graphemes. Each
grapheme begins with a dictionary-eligible initial scalar and contains only
spelling characters. Dictionary matches start and end on those grapheme
edges. Matching chooses a segmentation in this order.

1. Minimize the number of unknown graphemes.
2. Minimize the total number of tokens.
3. Prefer the longer token at the first differing token position.

An unknown token consumes exactly one grapheme. Only edges between two
consecutive dictionary words are candidates for added opportunities. A
candidate opens a break only where the shared UAX #14 decision is
`Tailorable` and `Prohibited`. Existing opportunities from
`PreserveGraphemes` retain their coordinates, authority, and decision. This is
an application lexicon policy in the scope of UAX #14's complex-context
tailoring. Different lexicons or unknown-word policy can yield different
results from other Thai segmenters. [UAX #14 §5.2](https://www.unicode.org/reports/tr14/#Dictionary_Usage)

## Bounds

Dictionary limits count the submitted words, total UTF-8 bytes, and UTF-8
bytes per word. Errors identify the zero-based entry where applicable and
the offending byte offset for a spelling error. A failed constructor produces
no index.

The collector checks the input byte limit before scanning and the run byte
limit before indexing a completed eligible grapheme. `RunByteLimit` reports
the absolute start of the offending run. Both limits are inclusive and any
failure returns no opportunities. The collector uses a grapheme pass,
replays eligible graphemes into bounded per-run state, and drives the
existing line-break iterator over the source. Matching takes at most
proportional work in run scalars times the longest dictionary entry in
scalars, with a fixed Thai alphabet bound on trie child lookup. Its retained
working data is linear in the largest eligible run plus the candidate
coordinates and returned opportunities. The whole-string API allows this
bounded lookahead to select the full run's segmentation before emitting any
opportunities.

## Verification

The Thai suite exhausts 426 model states across five tiny dictionaries and
texts of up to three authored graphemes. Its reference enumerates all token
partitions of each text. The model detects reversal of the longest-first
comparison on `กขค` with the lexicon `["ก", "กข", "ขค", "ค"]`. The expected
first token is `กข`. Authored examples in the suite record Thai phrase
boundaries and offsets. The fuzz target covers longer repeated runs, exact
limits, prefix-sharing spellings, marked graphemes, and explicit controls.
The default line-break corpus continues to verify the base algorithm.

A focused growth probe fed an optimized arm64 macOS build repeated
`กขค` as runtime input, using the same tie lexicon. It counted calls to
`roc_alloc` and `roc_realloc` around the tailoring operation after dictionary
construction. Timings are medians of five fresh processes and include process
startup. The probe used `--opt=speed` and the compiler pinned by commit
`a62401f`.

| Thai scalars | UTF-8 bytes | Opportunities | Allocations and reallocations | Elapsed |
| ---: | ---: | ---: | ---: | ---: |
| 3,000 | 9,000 | 2,000 | 44 | 2.8 ms |
| 6,000 | 18,000 | 4,000 | 48 | 2.9 ms |
| 12,000 | 36,000 | 8,000 | 53 | 3.5 ms |
| 24,000 | 72,000 | 16,000 | 56 | 4.5 ms |
