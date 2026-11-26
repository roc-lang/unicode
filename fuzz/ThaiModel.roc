import fuzz.Model
import ThaiOracle
import unicode.LineBreak
import unicode.TextPosition
import unicode.ThaiLineBreak

ThaiModel := [].{}

World : { lexicon : List(Str), clusters : List(Str) }

Action : [Select(List(Str)), Add(Str)]

tie_words : List(Str)
tie_words = ["ก", "กข", "ขค", "ค"]

# Five dictionaries and four grapheme choices after dictionary selection.
# Include every text of length zero through three for each dictionary.
machine : Model.Machine(World, Action)
machine = {
	init: { lexicon: [], clusters: [] },
	actions: |world| {
		if world.lexicon.is_empty() {
			[
				Select(["ก"]),
				Select(["ก", "ข", "ค"]),
				Select(tie_words),
				Select(["ก", "กข", "ขค", "ก่"]),
				Select(["กข", "ขก", "ก่", "ก่ก"]),
			]
		} else if world.clusters.len() < 3 {
			[Add("ก"), Add("ข"), Add("ค"), Add("ก่")]
		} else {
			[]
		}
	},
	step: |world, action| match action {
		Select(lexicon) => { ..world, lexicon }
		Add(grapheme) => { ..world, clusters: world.clusters.append(grapheme) }
	},
	rules: [
		Model.always("every opportunity agrees with the enumerated paths", |world| ThaiOracle.agrees(world.clusters, world.lexicon)),
		Model.sometimes("equal-coverage paths need the longest-first tie break", |world| world.lexicon == tie_words and world.clusters == ["ก", "ข", "ค"]),
		Model.sometimes("a shorter first token covers more source than a greedy match", |world| world.lexicon == ["ก", "กข", "ขค", "ก่"] and world.clusters == ["ก", "ข", "ค"]),
		Model.sometimes("known tokens surround an unknown grapheme", |world| world.lexicon == ["ก"] and world.clusters == ["ก", "ข", "ก"]),
		Model.sometimes("a matching scalar prefix falls inside a marked grapheme", |world| world.lexicon == ["ก"] and world.clusters == ["ก", "ก่", "ก"]),
	],
}

expect {
	report = Model.explore(machine, { depth: 5, key: |world| world, verify: EveryStep })
	if report.verdict != Holds(Exhausted) {
		crash "${Model.explain(report)} ${Str.inspect(report.verdict)}"
	}
	report.states == 426 and report.steps == 425 and report.depth == 4 and report.reached.len() == 4
}

# Independently annotated Thai spans. The phrases are written here for the
# tests, with boundaries between "language | Thai", "hello | (polite ending)"
# and "road | Sukhumvit". No external dictionary or segmentation is copied.
expect {
	fixtures = [
		{ source: "ภาษาไทย", words: ["ภาษา", "ไทย"], expected: [(12, 4), (21, 7)] },
		{ source: "สวัสดีครับ", words: ["สวัสดี", "ครับ"], expected: [(18, 6), (30, 10)] },
		{ source: "ถนนสุขุมวิท", words: ["ถนน", "สุขุมวิท"], expected: [(9, 3), (33, 11)] },
	]
	fixtures.all(
		|{ source, words, expected }| {
			actual = ThaiLineBreak.opportunities(source, ThaiOracle.dictionary(words), { max_input_bytes: 72, max_run_bytes: 72 })
			actual.map_ok(|events| events.map(|event| (event.at.byte_offset(), event.at.scalar_offset()))) == Ok(expected)
		},
	)
}

expect {
	dictionary = ThaiOracle.dictionary(["ก", "ข"])
	limits = { max_input_bytes: 64, max_run_bytes: 64 }
	# Mixed scripts, punctuation, zero-width space, word joiner, newline,
	# repetition punctuation and a foreign combining mark are run barriers.
	barriers = ["a", "ກ", "!", "\u(200B)", "\u(2060)", "\n", "ๆ", "\u(0301)"]
	barriers.all(
		|barrier| {
			source = "ก${barrier}ข"
			ThaiLineBreak.opportunities(source, dictionary, limits) == Ok(LineBreak.opportunities_with(source, PreserveGraphemes))
		},
	)
}

expect {
	dictionary = ThaiOracle.dictionary(["ก", "ข"])
	exact = { max_input_bytes: 8, max_run_bytes: 6 }
	(ThaiLineBreak.opportunities("x กข", dictionary, exact).map_ok(|events| events.len()) == Ok(3)) and
		(ThaiLineBreak.opportunities("x กข", dictionary, { ..exact, max_input_bytes: 7 }) == Err(InputByteLimit({ limit: 7 }))) and
			(ThaiLineBreak.opportunities("x กข", dictionary, { ..exact, max_run_bytes: 5 }) == Err(RunByteLimit({ start: TextPosition.from_offsets(2, 2), limit: 5 })))
}

expect {
	limits = { max_words: 2, max_total_bytes: 6, max_word_bytes: 3 }
	(ThaiLineBreak.Dictionary.from_words(["ก", "ก"], { revision: "rev", limits }).map_ok(ThaiLineBreak.Dictionary.revision) == Ok("rev")) and
		(ThaiLineBreak.Dictionary.from_words(["ก"], { revision: "", limits }).map_ok(|_| {}) == Err(MissingRevision)) and
			(ThaiLineBreak.Dictionary.from_words(["ก", "ข", "ค"], { revision: "rev", limits }).map_ok(|_| {}) == Err(TooManyWords({ limit: 2 }))) and
				(ThaiLineBreak.Dictionary.from_words(["กข"], { revision: "rev", limits }).map_ok(|_| {}) == Err(WordByteLimit({ word: 0, limit: 3 }))) and
					(ThaiLineBreak.Dictionary.from_words(["ก", "ข"], { revision: "rev", limits: { ..limits, max_total_bytes: 5 } }).map_ok(|_| {}) == Err(DictionaryByteLimit({ word: 1, limit: 5 }))) and
						(ThaiLineBreak.Dictionary.from_words(["กx"], { revision: "rev", limits: { ..limits, max_word_bytes: 6 } }).map_ok(|_| {}) == Err(InvalidWord({ word: 0, byte_offset: 3 })))
}
