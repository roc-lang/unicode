app [run!] {
	pf: platform "../../platform/main.roc",
	unicode: "../../../package/main.roc",
}

import unicode.Bidi
import unicode.BidiClass
import unicode.ByteRange
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextRange

## Both official UAX #9 files are parsed and verified by the Python harness.
## This app only accepts its compact, tab-separated protocol so the data stays
## out of the package binary. Every submitted row is one paragraph mode.
run! : Str => Str
run! = |input| {
	match verify_paragraph_api(input) {
		Err(message) => return fail("focused-paragraph-api", message)
		Ok({}) => {}
	}
	lines = input.split_on("\n").drop_if(|line| line == "")
	match lines {
		[] => fail("header", "empty input")
		[header, .. as cases] => {
			match parse_header(header, cases.len()) {
				None => fail("header", "malformed protocol header")
				Some(suite) => match run_cases(suite, cases, 0) {
					Ok(count) => "PASS\t${suite}\t${count.to_str()}"
					Err({ case_id, message }) => fail(case_id, message)
				}
			}
		}
	}
}

verify_paragraph_api : Str -> Try({}, Str)
verify_paragraph_api = |protocol_input| {
	crlf = "\r\n${protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? ""}"
	if Bidi.paragraph_ranges(crlf).len() != 1 {
		return Err("P1 did not keep CRLF in one paragraph range")
	}
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	if Bidi.paragraph_ranges(empty_input).len() != 1 or Bidi.paragraph_ranges("${crlf}${crlf}").len() != 2 {
		return Err("P1 empty/final-separator ranges are not deterministic")
	}
	match Bidi.analyze_paragraph("a\nb${empty_input}", Auto, Bidi.default_limits) {
		Err(MultipleParagraphs) => {}
		_ => return Err("paragraph ingestion accepted multiple P1 paragraphs")
	}
	match Bidi.analyze_paragraph(crlf, Auto, Bidi.default_limits) {
		Err(error) => Err("P1 range could not be analyzed: ${Str.inspect(error)}")
		Ok(_) => verify_global_range_api(protocol_input)
	}
}

verify_global_range_api : Str -> Try({}, Str)
verify_global_range_api = |protocol_input| {
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	source = "a\r\nא${empty_input}"
	ranges = Bidi.paragraph_ranges(source)
	second = ranges.get(1) ?? return Err("P1 did not return a second paragraph")
	analysis = bidi_result(Bidi.analyze_range(source, second, RightToLeft, Bidi.default_limits))?
	if !TextRange.is_eq(Bidi.paragraph_range(analysis), second) {
		return Err("range analysis rebased paragraph coordinates")
	}
	line = bidi_result(Bidi.reorder_line(analysis, TextRange.scalar_range(second)))?
	entry = Bidi.entries(analysis).get(0) ?? return Err("range analysis did not retain its scalar entry")
	if !TextRange.is_eq(entry.range, second) {
		return Err("range analysis did not retain absolute entry byte/scalar coordinates")
	}
	if Bidi.visual_to_logical(line) != [3] {
		return Err("range analysis did not retain global visual indices")
	}
	bracket_source = "a\r\n()${empty_input}"
	bracket_range = Bidi.paragraph_ranges(bracket_source).get(1) ?? return Err("P1 did not return bracket paragraph")
	bracket_analysis = bidi_result(Bidi.analyze_range(bracket_source, bracket_range, Auto, Bidi.default_limits))?
	if Bidi.matched_brackets(bracket_analysis).get(0) != Ok(Some(4)) {
		return Err("range analysis rebased paired-bracket indices")
	}

	## Exercise range validation across many completed paragraphs. This protects
	## `analyze_range` from regressing to a collecting P1 validation path while
	## retaining the first and last global coordinate contracts.
	many_short = "${repeat_text("a\n", 256)}א${empty_input}"
	many_ranges = Bidi.paragraph_ranges(many_short)
	if many_ranges.len() != 257 {
		return Err("P1 many-short-paragraph split drifted")
	}
	first_many = many_ranges.get(0) ?? return Err("P1 missing first short paragraph")
	last_many = many_ranges.get(256) ?? return Err("P1 missing final short paragraph")
	first_analysis = bidi_result(Bidi.analyze_range(many_short, first_many, Auto, Bidi.default_limits))?
	last_analysis = bidi_result(Bidi.analyze_range(many_short, last_many, RightToLeft, Bidi.default_limits))?
	if !TextRange.is_eq(Bidi.paragraph_range(first_analysis), first_many) or !TextRange.is_eq(Bidi.paragraph_range(last_analysis), last_many) {
		return Err("range analysis lost coordinates across many short paragraphs")
	}
	verify_empty_line_and_mirroring(protocol_input)?
	verify_limits_and_bracket_cap(protocol_input)?
	verify_x9_logical_runs(protocol_input)?
	verify_open_bracket_nsm(protocol_input)?
	verify_metamorphic_invariants(protocol_input)?
	verify_control_and_bracket_boundaries(protocol_input)?
	verify_l1_line_boundaries(protocol_input)?
	Ok({})
}

verify_metamorphic_invariants : Str -> Try({}, Str)
verify_metamorphic_invariants = |protocol_input| {
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	source = "אa(${empty_input}"
	analysis = bidi_result(Bidi.analyze_paragraph(source, Auto, Bidi.default_limits))?
	paragraph = TextRange.scalar_range(Bidi.paragraph_range(analysis))
	line = bidi_result(Bidi.reorder_line(analysis, paragraph))?
	verify_entry_partition(Bidi.entries(analysis), source.count_utf8_bytes(), ScalarRange.end(paragraph))?
	verify_level_parity(Bidi.line_levels(line))?
	verify_mapping_inverse(Bidi.visual_to_logical(line), Bidi.logical_to_visual(line), ScalarRange.start(paragraph))?

	before = Bidi.levels(analysis)
	first = ScalarRange.from_bounds(0, 2) ?? return Err("could not create first line")
	second = ScalarRange.from_bounds(2, ScalarRange.end(paragraph)) ?? return Err("could not create second line")
	_ = bidi_result(Bidi.reorder_line(analysis, first))?
	_ = bidi_result(Bidi.reorder_line(analysis, second))?
	if Bidi.levels(analysis) != before {
		return Err("line-specific L1/L2 changed retained paragraph levels")
	}

	plain = bidi_result(Bidi.analyze_paragraph("אa${empty_input}", Auto, Bidi.default_limits))?
	isolated = bidi_result(Bidi.analyze_paragraph("א⁨a⁩a${empty_input}", Auto, Bidi.default_limits))?
	plain_first = Bidi.entries(plain).get(0) ?? return Err("missing plain first scalar")
	plain_last = Bidi.entries(plain).get(1) ?? return Err("missing plain last scalar")
	isolated_first = Bidi.entries(isolated).get(0) ?? return Err("missing isolate first scalar")
	isolated_last = Bidi.entries(isolated).get(4) ?? return Err("missing isolate last scalar")
	if plain_first.level != isolated_first.level or plain_last.level != isolated_last.level {
		return Err("complete isolate changed surrounding scalar levels")
	}
	Ok({})
}

verify_entry_partition : List(Bidi.ScalarInfo), U64, U64 -> Try({}, Str)
verify_entry_partition = |entries, byte_end, scalar_end| {
	var next_byte = 0.U64
	var next_scalar = 0.U64
	for entry in entries {
		bytes = TextRange.byte_range(entry.range)
		scalars = TextRange.scalar_range(entry.range)
		if ByteRange.start(bytes) != next_byte or ScalarRange.start(scalars) != next_scalar {
			return Err("retained entries do not partition original coordinates")
		}
		next_byte = ByteRange.end(bytes)
		next_scalar = ScalarRange.end(scalars)
	}
	if next_byte != byte_end or next_scalar != scalar_end {
		Err("retained entries do not cover the original paragraph")
	} else {
		Ok({})
	}
}

verify_level_parity : List(Bidi.ResolvedLevel) -> Try({}, Str)
verify_level_parity = |levels| {
	for resolved in levels {
		match resolved {
			RemovedByX9 => {}
			Level(level) => {

				## BD2 caps explicit levels at 125; I2 may raise an L character
				## in that odd context to its final resolved level 126.
				if level > 126 {
					return Err("resolved level exceeds the UAX #9 final maximum")
				}
				if (level % 2 == 0 and Bidi.direction(level) != LeftToRight) or (level % 2 != 0 and Bidi.direction(level) != RightToLeft) {
					return Err("resolved level direction disagrees with parity")
				}
			}
		}
	}
	Ok({})
}

## `visual_to_logical` retains absolute scalar coordinates while
## `logical_to_visual` is compacted to the requested line's start.
verify_mapping_inverse : List(U64), List([Some(U64), None]), U64 -> Try({}, Str)
verify_mapping_inverse = |visual_to_logical, logical_to_visual, line_start| {
	var visual = 0.U64
	for logical in visual_to_logical {
		position = logical_to_visual.get(logical - line_start) ?? return Err("visual mapping refers outside logical line")
		if position != Some(visual) {
			return Err("logical/visual mappings are not inverses")
		}
		visual = visual + 1
	}
	var logical_index = 0.U64
	for position in logical_to_visual {
		match position {
			None => {}
			Some(visual_index) => {
				logical = visual_to_logical.get(visual_index) ?? return Err("logical mapping refers outside visual line")
				if logical != logical_index + line_start {
					return Err("visual/logical mappings are not inverses")
				}
			}
		}
		logical_index = logical_index + 1
	}
	Ok({})
}

verify_control_and_bracket_boundaries : Str -> Try({}, Str)
verify_control_and_bracket_boundaries = |protocol_input| {
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	controls = Str.join_with([repeat_text("‫", 126), "a", repeat_text("‬", 126), empty_input], "")
	control_analysis = bidi_result(Bidi.analyze_paragraph(controls, Auto, Bidi.default_limits))?
	verify_level_parity(Bidi.levels(control_analysis))?
	if !contains_level(Bidi.levels(control_analysis), 126) {
		return Err("deep explicit controls did not reach the permitted I2 level 126")
	}
	missing_pdi = bidi_result(Bidi.analyze_paragraph("⁨א⁨a${empty_input}", Auto, Bidi.default_limits))?
	verify_level_parity(Bidi.levels(missing_pdi))?

	for depth in [62, 63] {
		brackets = Str.join_with([repeat_text("(", depth), repeat_text(")", depth)], "")
		analysis = bidi_result(Bidi.analyze_paragraph(brackets, Auto, Bidi.default_limits))?
		if Bidi.matched_brackets(analysis).get(0) != Ok(Some(depth * 2 - 1)) {
			return Err("BD16 did not retain a bracket pair within capacity")
		}
	}
	overflow = Str.join_with([repeat_text("(", 64), repeat_text(")", 64)], "")
	overflow_analysis = bidi_result(Bidi.analyze_paragraph(overflow, Auto, Bidi.default_limits))?
	if Bidi.matched_brackets(overflow_analysis).get(0) != Ok(None) {
		return Err("BD16 bracket-stack overflow retained a pair")
	}
	for source in ["〈a〉", "〈a〉"] {
		analysis = bidi_result(Bidi.analyze_paragraph("${source}${empty_input}", Auto, Bidi.default_limits))?
		if Bidi.matched_brackets(analysis).get(0) != Ok(Some(2)) {
			return Err("canonical angle-bracket form was not paired")
		}
	}
	mirrorless = bidi_result(Bidi.analyze_paragraph("∁${empty_input}", RightToLeft, Bidi.default_limits))?
	mirrorless_range = ScalarRange.from_bounds(0, 1) ?? return Err("could not create mirrorless line")
	mirrorless_line = bidi_result(Bidi.reorder_line(mirrorless, mirrorless_range))?
	mirrorless_fact = Bidi.line_mirroring(mirrorless_line).get(0) ?? return Err("missing mirrorless fact")
	if !mirrorless_fact.needs_glyph or mirrorless_fact.glyph != None {
		return Err("L4 mirrored-without-mapping fact drifted")
	}
	Ok({})
}

contains_level : List(Bidi.ResolvedLevel), U8 -> Bool
contains_level = |levels, wanted| {
	for level in levels {
		if level == Level(wanted) {
			return Bool.True
		}
	}
	Bool.False
}

verify_l1_line_boundaries : Str -> Try({}, Str)
verify_l1_line_boundaries = |protocol_input| {
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	analysis = bidi_result(Bidi.analyze_paragraph("א a${empty_input}", LeftToRight, Bidi.default_limits))?
	first_range = ScalarRange.from_bounds(0, 2) ?? return Err("could not create first L1 line")
	second_range = ScalarRange.from_bounds(2, 3) ?? return Err("could not create second L1 line")
	first = bidi_result(Bidi.reorder_line(analysis, first_range))?
	second = bidi_result(Bidi.reorder_line(analysis, second_range))?
	if Bidi.line_levels(first).get(1) != Ok(Level(0)) or Bidi.visual_to_logical(second) != [2] {
		return Err("L1 did not respect separate logical line boundaries")
	}
	Ok({})
}

verify_x9_logical_runs : Str -> Try({}, Str)
verify_x9_logical_runs = |protocol_input| {
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	analysis = bidi_result(Bidi.analyze_paragraph("א​ב${empty_input}", Auto, Bidi.default_limits))?
	var runs = []
	for run in Bidi.logical_runs(analysis) {
		runs = runs.append(run)
	}
	if runs.len() != 1 {
		return Err("X9 controls split a logical level run")
	}
	run = runs.get(0) ?? return Err("missing X9-filtered logical run")
	if ScalarRange.start(TextRange.scalar_range(run.range)) != 0 or ScalarRange.end(TextRange.scalar_range(run.range)) != 3 {
		return Err("X9-filtered logical run does not span both visible scalars")
	}
	Ok({})
}

verify_open_bracket_nsm : Str -> Try({}, Str)
verify_open_bracket_nsm = |protocol_input| {
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	analysis = bidi_result(Bidi.analyze_paragraph("א(̀a)${empty_input}", Auto, Bidi.default_limits))?
	opening = Bidi.entries(analysis).get(1) ?? return Err("missing opening bracket")
	nsm = Bidi.entries(analysis).get(2) ?? return Err("missing opening-bracket NSM")
	if BidiClass.short(opening.working_class) != "R" or opening.working_class != nsm.working_class {
		return Err("N0 did not propagate the opening bracket class through its NSM")
	}
	Ok({})
}

verify_empty_line_and_mirroring : Str -> Try({}, Str)
verify_empty_line_and_mirroring = |protocol_input| {
	empty_input = protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? return Err("could not derive empty protocol input")
	empty = bidi_result(Bidi.analyze_paragraph(empty_input, Auto, Bidi.default_limits))?
	empty_range = ScalarRange.from_bounds(0, 0) ?? return Err("could not create empty line range")
	empty_line = bidi_result(Bidi.reorder_line(empty, empty_range))?
	if Bidi.visual_to_logical(empty_line) != [] or Bidi.visual_runs(empty_line) != [] {
		return Err("empty line did not have empty visual order/runs")
	}
	rtl = bidi_result(Bidi.analyze_paragraph("(", RightToLeft, Bidi.default_limits))?
	rtl_range = ScalarRange.from_bounds(0, 1) ?? return Err("could not create RTL line range")
	rtl_line = bidi_result(Bidi.reorder_line(rtl, rtl_range))?
	mirror = Bidi.line_mirroring(rtl_line).get(0) ?? return Err("missing mirror fact")
	if !mirror.needs_glyph or Bidi.visual_runs(rtl_line).len() != 1 {
		return Err("L4 mirroring or visual-run result is missing")
	}
	Ok({})
}

verify_limits_and_bracket_cap : Str -> Try({}, Str)
verify_limits_and_bracket_cap = |protocol_input| {
	limited = { max_scalars: 0, max_bytes: U64.highest }
	limited_source = "a${protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? ""}"
	match Bidi.analyze_paragraph(limited_source, Auto, limited) {
		Err(ScalarLimitExceeded(_)) => {}
		_ => return Err("scalar limit was not reported atomically")
	}
	byte_limited = { max_scalars: U64.highest, max_bytes: 0 }
	match Bidi.analyze_paragraph(limited_source, Auto, byte_limited) {
		Err(ByteLimitExceeded({ limit, required, stage: Ingestion, range })) if limit == 0 and required == limited_source.count_utf8_bytes() and ByteRange.start(range) == 0 and ByteRange.end(range) == limited_source.count_utf8_bytes() => {}
		_ => return Err("byte limit was not reported with the full ingestion range")
	}
	ranged_source = "a\r\nא${protocol_input.drop_last_bytes(protocol_input.count_utf8_bytes()) ?? ""}"
	foreign_range = Bidi.paragraph_ranges("x").get(0) ?? return Err("could not create foreign range")
	match Bidi.analyze_range(ranged_source, foreign_range, Auto, Bidi.default_limits) {
		Err(InvalidParagraphRange(_)) => {}
		_ => return Err("analyze_range accepted a non-P1 range")
	}
	valid_range = Bidi.paragraph_ranges(ranged_source).get(1) ?? return Err("could not find P1 range")
	match Bidi.analyze_range(ranged_source, valid_range, Auto, { max_scalars: U64.highest, max_bytes: 0 }) {
		Err(ByteLimitExceeded({ limit, required, stage: Ingestion, range })) if limit == 0 and required == 2 and ByteRange.start(range) == 3 and ByteRange.end(range) == 5 => {}
		_ => return Err("range byte limit did not retain absolute byte coordinates")
	}
	paragraph = TextRange.scalar_range(valid_range)
	overrun = ScalarRange.from_bounds(ScalarRange.start(paragraph), ScalarRange.end(paragraph) + 1) ?? return Err("could not create overrunning line")
	range_analysis = bidi_result(Bidi.analyze_range(ranged_source, valid_range, Auto, Bidi.default_limits))?
	match Bidi.reorder_line(range_analysis, overrun) {
		Err(LineOutOfBounds(_)) => {}
		_ => return Err("line reordering accepted an out-of-bounds range")
	}
	deep = Str.join_with([repeat_text("(", 64), repeat_text(")", 64)], "")
	analysis = bidi_result(Bidi.analyze_paragraph(deep, Auto, Bidi.default_limits))?
	if Bidi.matched_brackets(analysis).get(0) != Ok(None) {
		return Err("BD16 bracket-stack overflow retained a pair")
	}
	Ok({})
}

repeat_text : Str, U64 -> Str
repeat_text = |piece, count| {
	var output = ""
	var at = 0.U64
	while at < count {
		output = "${output}${piece}"
		at = at + 1
	}
	output
}

bidi_result : Try(value, Bidi.Error) -> Try(value, Str)
bidi_result = |result| match result {
	Ok(value) => Ok(value)
	Err(error) => Err(Str.inspect(error))
}

parse_header : Str, U64 -> [Some(Str), None]
parse_header = |header, actual_count| {
	match header.split_on("\t") {
		["ROC_UNICODE_TEST_V1", suite, count_str] if suite == "bidi-test" or suite == "bidi-character-test" or suite == "bidi-metamorphic" => {
			expected = U64.from_str(count_str) ?? return None
			if expected == actual_count Some(suite) else None
		}
		_ => None
	}
}

run_cases : Str, List(Str), U64 -> Try(U64, { case_id : Str, message : Str })
run_cases = |suite, remaining, count| {
	match remaining {
		[] => Ok(count)
		[line, .. as rest] => {
			match run_case(suite, line) {
				Ok({}) => run_cases(suite, rest, count + 1)
				Err(error) => Err(error)
			}
		}
	}
}

run_case : Str, Str -> Try({}, { case_id : Str, message : Str })
run_case = |suite, line| {
	if suite == "bidi-test" {
		run_bidi_test_case(line)
	} else if suite == "bidi-metamorphic" {
		run_bidi_metamorphic_case(line)
	} else {
		run_bidi_character_case(line)
	}
}

## The Python-side PRNG is seeded in the adjacent spec. These cases do not
## replace the normative Unicode oracle; they exercise invariants over mixed,
## malformed, and nested control streams between official conformance runs.
run_bidi_metamorphic_case : Str -> Try({}, { case_id : Str, message : Str })
run_bidi_metamorphic_case = |line| {
	match line.split_on("\t") {
		[case_id, code_points_hex, mode_str] => {
			code_points = keep_trys(code_points_hex.split_on(",").map(|hex| U32.from_str("0x${hex}")))
			source = match code_points {
				Err(_) => return Err({ case_id, message: "invalid metamorphic code point" })
				Ok(values) => require(source_from_code_points(values), case_id, "invalid metamorphic scalar")?
			}
			mode = require(parse_bidi_test_mode(mode_str), case_id, "invalid metamorphic paragraph mode")?
			for paragraph_range in Bidi.paragraph_ranges(source) {
				analysis = match Bidi.analyze_range(source, paragraph_range, mode, Bidi.default_limits) {
					Err(error) => return Err({ case_id, message: "metamorphic range analysis failed: ${Str.inspect(error)}" })
					Ok(value) => value
				}
				paragraph = TextRange.scalar_range(Bidi.paragraph_range(analysis))
				line_order = match Bidi.reorder_line(analysis, paragraph) {
					Err(error) => return Err({ case_id, message: "metamorphic line reordering failed: ${Str.inspect(error)}" })
					Ok(value) => value
				}
				match verify_level_parity(Bidi.line_levels(line_order)) {
					Err(message) => return Err({
						case_id,
						message: "${message}; seed-input-mode=${mode_str}; scalar-range=${ScalarRange.start(paragraph).to_str()}..${ScalarRange.end(paragraph).to_str()}; code-points=${code_points_hex}",
					})
					Ok({}) => {}
				}
				match verify_mapping_inverse(Bidi.visual_to_logical(line_order), Bidi.logical_to_visual(line_order), ScalarRange.start(paragraph)) {
					Err(message) => return Err({
						case_id,
						message: "${message}; seed-input-mode=${mode_str}; scalar-range=${ScalarRange.start(paragraph).to_str()}..${ScalarRange.end(paragraph).to_str()}; code-points=${code_points_hex}",
					})
					Ok({}) => {}
				}
			}
			Ok({})
		}
		[case_id, ..] => Err({ case_id, message: "malformed metamorphic row" })
		_ => Err({ case_id: "unknown", message: "malformed metamorphic row" })
	}
}

run_bidi_test_case : Str -> Try({}, { case_id : Str, message : Str })
run_bidi_test_case = |line| {
	match line.split_on("\t") {
		[case_id, class_names, mode_str, expected_levels_str, expected_reorder_str] => {
			classes = class_names.split_on(",")
			source = require(source_from_classes(classes), case_id, "unknown Bidi_Class representative")?
			mode = require(parse_bidi_test_mode(mode_str), case_id, "invalid paragraph mode")?
			expected_levels = require(parse_levels(expected_levels_str), case_id, "invalid expected levels")?
			expected_reorder = require(parse_reorder(expected_reorder_str), case_id, "invalid expected reorder")?
			check_case(case_id, source, mode, None, expected_levels, expected_reorder)
		}
		[case_id, ..] => Err({ case_id, message: "malformed BidiTest row" })
		_ => Err({ case_id: "unknown", message: "malformed BidiTest row" })
	}
}

run_bidi_character_case : Str -> Try({}, { case_id : Str, message : Str })
run_bidi_character_case = |line| {
	match line.split_on("\t") {
		[case_id, code_points_hex, mode_str, paragraph_level_str, expected_levels_str, expected_reorder_str] => {
			code_points = keep_trys(code_points_hex.split_on(",").map(|hex| U32.from_str("0x${hex}")))
			source = match code_points {
				Err(_) => return Err({ case_id, message: "invalid code point" })
				Ok(values) => require(source_from_code_points(values), case_id, "invalid scalar")?
			}
			mode = require(parse_bidi_character_mode(mode_str), case_id, "invalid paragraph mode")?
			paragraph_level = U8.from_str(paragraph_level_str) ?? return Err({ case_id, message: "invalid paragraph level" })
			expected_levels = require(parse_levels(expected_levels_str), case_id, "invalid expected levels")?
			expected_reorder = require(parse_reorder(expected_reorder_str), case_id, "invalid expected reorder")?
			check_case(case_id, source, mode, Some(paragraph_level), expected_levels, expected_reorder)
		}
		[case_id, ..] => Err({ case_id, message: "malformed BidiCharacterTest row" })
		_ => Err({ case_id: "unknown", message: "malformed BidiCharacterTest row" })
	}
}

check_case : Str, Str, Bidi.BaseDirection, [Some(U8), None], List(Bidi.ResolvedLevel), List(U64) -> Try({}, { case_id : Str, message : Str })
check_case = |case_id, source, base_direction, expected_paragraph_level, expected_levels, expected_reorder| {
	analysis = match Bidi.analyze_paragraph(source, base_direction, Bidi.default_limits) {
		Ok(value) => value
		Err(error) => return Err({ case_id, message: "analysis failed: ${Str.inspect(error)}" })
	}
	range = ScalarRange.from_bounds(0, expected_levels.len()) ?? return Err({ case_id, message: "invalid full paragraph range" })
	line = match Bidi.reorder_line(analysis, range) {
		Ok(value) => value
		Err(error) => return Err({ case_id, message: "line reordering failed: ${Str.inspect(error)}" })
	}
	if expected_paragraph_level != None and expected_paragraph_level != Some(Bidi.paragraph_level(analysis)) {
		Err({
			case_id,
			message: "paragraph level expected ${Str.inspect(expected_paragraph_level)}, got ${Bidi.paragraph_level(analysis).to_str()}",
		})
	} else if Bidi.line_levels(line) != expected_levels {
		Err({
			case_id,
			message: "levels expected ${Str.inspect(expected_levels)}, got ${Str.inspect(Bidi.line_levels(line))}",
		})
	} else if Bidi.visual_to_logical(line) != expected_reorder {
		Err({
			case_id,
			message: "reorder expected ${Str.inspect(expected_reorder)}, got ${Str.inspect(Bidi.visual_to_logical(line))}",
		})
	} else {
		Ok({})
	}
}

## BidiTest encodes its three bitset selections in an internal order.
parse_bidi_test_mode : Str -> [Some(Bidi.BaseDirection), None]
parse_bidi_test_mode = |value| match value {
	"0" => Some(Auto)
	"1" => Some(LeftToRight)
	"2" => Some(RightToLeft)
	_ => None
}

## BidiCharacterTest follows UAX #9's documented field order: LTR, RTL, Auto.
parse_bidi_character_mode : Str -> [Some(Bidi.BaseDirection), None]
parse_bidi_character_mode = |value| match value {
	"0" => Some(LeftToRight)
	"1" => Some(RightToLeft)
	"2" => Some(Auto)
	_ => None
}

parse_levels : Str -> [Some(List(Bidi.ResolvedLevel)), None]
parse_levels = |value| {
	keep_options(
		value.split_on(",").map(
			|token| {
				if token == "x" {
					Some(RemovedByX9)
				} else {
					match U8.from_str(token) {
						Ok(level) => Some(Level(level))
						Err(_) => None
					}
				}
			},
		),
	)
}

parse_reorder : Str -> [Some(List(U64)), None]
parse_reorder = |value| {
	if value == "-" {
		Some([])
	} else {
		match keep_trys(value.split_on(",").map(U64.from_str)) {
			Ok(values) => Some(values)
			Err(_) => None
		}
	}
}

source_from_classes : List(Str) -> [Some(Str), None]
source_from_classes = |classes| {
	match keep_options(classes.map(class_to_str)) {
		None => None
		Some(parts) => Some(Str.join_with(parts, ""))
	}
}

## These are deliberately non-bracket representatives. BidiTest guarantees its
## class sequences do not exercise N0, leaving BidiCharacterTest to cover the
## normative character-specific bracket cases.
class_representative : Str -> [Some(U32), None]
class_representative = |name| match name {
	"L" => Some(0x0061)
	"R" => Some(0x05D0)
	"AL" => Some(0x0627)
	"EN" => Some(0x0030)
	"AN" => Some(0x0660)
	"ES" => Some(0x002B)
	"ET" => Some(0x0024)
	"CS" => Some(0x002C)
	"NSM" => Some(0x0300)
	"B" => Some(0x2029)
	"S" => Some(0x0009)
	"WS" => Some(0x0020)
	"ON" => Some(0x0021)
	"LRE" => Some(0x202A)
	"RLE" => Some(0x202B)
	"PDF" => Some(0x202C)
	"LRO" => Some(0x202D)
	"RLO" => Some(0x202E)
	"LRI" => Some(0x2066)
	"RLI" => Some(0x2067)
	"FSI" => Some(0x2068)
	"PDI" => Some(0x2069)
	"BN" => Some(0x200B)
	_ => None
}

class_to_str : Str -> [Some(Str), None]
class_to_str = |name| {
	match class_representative(name) {
		None => None
		Some(code_point) => match Scalar.from_u32(code_point) {
			Err(_) => None
			Ok(scalar) => {
				if BidiClass.short(BidiClass.of_scalar(scalar)) != name {
					None
				} else {
					match Scalar.to_str(scalar) {
						Ok(encoded) => Some(encoded)
						Err(_) => None
					}
				}
			}
		}
	}
}

source_from_code_points : List(U32) -> [Some(Str), None]
source_from_code_points = |code_points| {
	scalars = match keep_trys(code_points.map(Scalar.from_u32)) {
		Ok(values) => values
		Err(_) => return None
	}
	strings = match keep_trys(scalars.map(Scalar.to_str)) {
		Ok(values) => values
		Err(_) => return None
	}
	Some(Str.join_with(strings, ""))
}

keep_options : List([Some(value), None]) -> [Some(List(value)), None]
keep_options = |items| {
	items.fold(
		Some([]),
		|state, item| match (state, item) {
			(Some(values), Some(value)) => Some(values.append(value))
			_ => None
		},
	)
}

keep_trys : List(Try(value, err)) -> Try(List(value), err)
keep_trys = |items| {
	items.fold(
		Ok([]),
		|state, item| {
			values = state?
			value = item?
			Ok(values.append(value))
		},
	)
}

require : [Some(value), None], Str, Str -> Try(value, { case_id : Str, message : Str })
require = |option, case_id, message| match option {
	Some(value) => Ok(value)
	None => Err({ case_id, message })
}

fail : Str, Str -> Str
fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}"
