app [run!] {
	pf: platform "../../platform/main.roc",
	test_unicode: "../../../package/test-main.roc",
}

import test_unicode.TestProperties

run! : Str => Str
run! = |input| {
	lines = input.split_on("\n").drop_if(|line| line == "")
	match lines {
		[] => fail("header", "empty input")
		[header, .. as cases] => {
			match parse_header(header, cases.len()) {
				Err(message) => fail("header", message)
				Ok({}) => match run_cases(cases, 0) {
					Ok(count) => "PASS\tproperties\t${count.to_str()}"
					Err({ case_id, message }) => fail(case_id, message)
				}
			}
		}
	}
}

parse_header : Str, U64 -> Try({}, Str)
parse_header = |header, actual_count| {
	match header.split_on("\t") {
		["ROC_UNICODE_TEST_V1", "properties", count_str] => {
			expected_count = U64.from_str(count_str) ?? return Err("invalid header count")
			if expected_count == actual_count Ok({}) else Err("header count mismatch")
		}
		_ => Err("malformed protocol header")
	}
}

run_cases : List(Str), U64 -> Try(U64, { case_id : Str, message : Str })
run_cases = |remaining, count| {
	match remaining {
		[] => Ok(count)
		[line, .. as rest] => match run_case(line) {
			Ok({}) => run_cases(rest, count + 1)
			Err(error) => Err(error)
		}
	}
}

run_case : Str -> Try({}, { case_id : Str, message : Str })
run_case = |line| {
	match line.split_on("\t") {
		[case_id, cp_hex, gcb_str, eaw_str, emoji_str, bidi_str, mirrored_str, mirror_str, bracket_target_str, bracket_type_str] => {
			parsed = (
				U32.from_str("0x${cp_hex}"),
				U8.from_str(gcb_str),
				U8.from_str(eaw_str),
				U8.from_str(emoji_str),
				U8.from_str(bidi_str),
				U8.from_str(mirrored_str),
				U32.from_str(mirror_str),
				U32.from_str(bracket_target_str),
				U8.from_str(bracket_type_str),
			)
			match parsed {
				(Ok(cp), Ok(expected_gcb), Ok(expected_eaw), Ok(expected_emoji), Ok(expected_bidi), Ok(expected_mirrored), Ok(expected_mirror), Ok(expected_bracket_target), Ok(expected_bracket_type)) => {
					got = (
						TestProperties.gcb(cp),
						TestProperties.eaw(cp),
						TestProperties.emoji(cp),
						TestProperties.bidi_class(cp),
						TestProperties.bidi_mirrored(cp),
						TestProperties.bidi_mirroring_glyph(cp),
						TestProperties.bidi_bracket_target(cp),
						TestProperties.bidi_bracket_type(cp),
					)
					expected = (
						expected_gcb,
						expected_eaw,
						expected_emoji,
						expected_bidi,
						expected_mirrored,
						expected_mirror,
						expected_bracket_target,
						expected_bracket_type,
					)
					if got == expected {
						Ok({})
					} else {
						Err({ case_id, message: "expected ${Str.inspect(expected)}, got ${Str.inspect(got)}" })
					}
				}
				_ => Err({ case_id, message: "malformed numeric field" })
			}
		}
		[case_id, ..] => Err({ case_id, message: "malformed case row" })
		_ => Err({ case_id: "unknown", message: "malformed case row" })
	}
}

fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}"
