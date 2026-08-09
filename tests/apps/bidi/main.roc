app [run!] {
    pf: platform "../../platform/main.roc",
    unicode: "../../../package/main.roc",
}

import unicode.Bidi
import unicode.BidiClass
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
	if Bidi.visual_to_logical(line) != [3] {
		return Err("range analysis did not retain global visual indices")
	}
	verify_empty_line_and_mirroring(protocol_input)?
	verify_limits_and_bracket_cap(protocol_input)?
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
        ["ROC_UNICODE_TEST_V1", suite, count_str]
            if suite == "bidi-test" or suite == "bidi-character-test" => {
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
    } else {
        run_bidi_character_case(line)
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
    keep_options(value.split_on(",").map(|token| {
        if token == "x" {
            Some(RemovedByX9)
        } else {
            match U8.from_str(token) {
                Ok(level) => Some(Level(level))
                Err(_) => None
            }
        }
    }))
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
    items.fold(Some([]), |state, item| match (state, item) {
        (Some(values), Some(value)) => Some(values.append(value))
        _ => None
    })
}

keep_trys : List(Try(value, err)) -> Try(List(value), err)
keep_trys = |items| {
    items.fold(Ok([]), |state, item| {
        values = state?
        value = item?
        Ok(values.append(value))
    })
}

require : [Some(value), None], Str, Str -> Try(value, { case_id : Str, message : Str })
require = |option, case_id, message| match option {
    Some(value) => Ok(value)
    None => Err({ case_id, message })
}

fail : Str, Str -> Str
fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}" 
