import fuzz.Fuzz
import ThaiOracle
import unicode.Grapheme
import unicode.LineBreak
import unicode.ThaiLineBreak

Input : { dictionary : U8, config : U8, parts : List(U8) }

ThaiTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-thai",
		examples: [
			Fuzz.example({ name: "prefix-sharing-tie", revision: "1", make: || { dictionary: 15, config: 0, parts: [0, 1, 2] } }),
			Fuzz.example({ name: "unknown-and-marked-grapheme", revision: "1", make: || { dictionary: 3, config: 0, parts: [0, 3, 2, 1, 0] } }),
			Fuzz.example({ name: "mixed-with-explicit-controls", revision: "1", make: || { dictionary: 15, config: 1, parts: [0, 1, 6, 2, 7, 0] } }),
			Fuzz.example({ name: "exact-and-short-run-limit", revision: "1", make: || { dictionary: 15, config: 2, parts: [0, 1, 2, 0] } }),
		],
		generator: Fuzz.map(Fuzz.raw_bytes, decode),
		test,
		show: |input| Str.inspect(input),
	})
}

decode : List(U8) -> Input
decode = |bytes| {
	{ dictionary: bytes.get(0) ?? 0, config: bytes.get(1) ?? 0, parts: bytes.drop_first(2).take_first(64) }
}

test : Input -> Fuzz.Outcome
test = |input| {
	lexicon = ["ก", "กข", "ขค", "ค", "ข", "ก่", "ก่ก", "กค"]
		.map_with_index(|word, index| (word, index))
		.keep_if(|(_, index)| input.dictionary.bitwise_and((1.U8).shl_wrap(index.to_u8_wrap())) > 0)
		.map(|(word, _)| word)
	plain = ["ก", "ข", "ค", "ก่"]
	if input.config % 3 == 0 {
		clusters = input.parts.take_first(6).map(|byte| plain.get((byte % 4).to_u64()) ?? "ก")
		if !ThaiOracle.agrees(clusters, lexicon) {
			crash "Thai line breaks disagreed with enumerated segmentations"
		}
	} else if input.config % 3 == 1 {
		menu = plain.concat(["!", "ກ", "\u(200B)", "\u(2060)", "\r\n", "\u(0301)", "𐌀", "ๆ"])
		source = Str.join_with(input.parts.map(|byte| menu.get(byte.to_u64() % menu.len()) ?? "ก"), "")
		check_mixed(source, lexicon)
	} else {
		clusters = input.parts.map(|byte| plain.get((byte % 4).to_u64()) ?? "ก")
		check_limits(clusters, lexicon)
	}
	Fuzz.keep
}

check_mixed : Str, List(Str) -> {}
check_mixed = |source, lexicon| {
	index = ThaiOracle.dictionary(lexicon)
	length = source.count_utf8_bytes()
	limits = { max_input_bytes: length, max_run_bytes: length }
	actual = ThaiLineBreak.opportunities(source, index, limits) ?? crash "bounded mixed input failed"
	other_order = ThaiOracle.dictionary(lexicon.fold([], |words, word| [word].concat(words)))
	if Ok(actual) != ThaiLineBreak.opportunities(source, other_order, limits) {
		crash "dictionary insertion order affected Thai opportunities"
	}
	baseline = LineBreak.opportunities_with(source, PreserveGraphemes)
	boundaries = LineBreak.boundaries_with(source, PreserveGraphemes)
	grapheme_ends = Grapheme.ranges(source).map(|range| range.end())
	if !baseline.all(|event| actual.contains(event)) {
		crash "Thai tailoring changed an existing opportunity"
	}
	var $previous = 0.U64
	for event in actual {
		at = event.at.byte_offset()
		if at <= $previous and length > 0 {
			crash "Thai opportunities were not strictly ordered"
		}
		if !baseline.contains(event) {
			if !grapheme_ends.contains(at) or !boundaries.any(|boundary|
				boundary.at == event.at and boundary.decision == Prohibited and boundary.authority == Tailorable) {
				crash "Thai tailoring opened an ineligible boundary"
			}
		}
		$previous = at
	}
	{}
}

check_limits : List(Str), List(Str) -> {}
check_limits = |clusters, lexicon| {
	source = Str.join_with(clusters, "")
	length = source.count_utf8_bytes()
	index = ThaiOracle.dictionary(lexicon)
	exact = { max_input_bytes: length, max_run_bytes: length }
	if ThaiLineBreak.opportunities(source, index, exact).is_err() {
		crash "Thai tailoring rejected the exact limits"
	}
	if length > 0 {
		short = length - 1
		input_short = ThaiLineBreak.opportunities(source, index, { ..exact, max_input_bytes: short })
		if input_short != Err(InputByteLimit({ limit: short })) {
			crash "Thai tailoring did not enforce the exact source limit"
		}
		match ThaiLineBreak.opportunities(source, index, { ..exact, max_run_bytes: short }) {
			Err(RunByteLimit({ start, limit })) => {
				if start.byte_offset() != 0 or start.scalar_offset() != 0 or limit != short {
					crash "Thai tailoring reported the wrong over-limit run"
				}
			}
			_ => crash "Thai tailoring did not enforce the exact run limit"
		}
	}
	{}
}
