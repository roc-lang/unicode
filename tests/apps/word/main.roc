app [run!] {
	pf: platform "../../platform/main.roc",
	unicode: "../../../package/main.roc",
}

import unicode.ByteRange
import unicode.Scalar
import unicode.Word

run! : Str => Str
run! = |input| {
	match run_focused(input.count_utf8_bytes()) {
		Err(message) => return fail("focused", message)
		Ok({}) => {}
	}
	lines = input.split_on("\n").drop_if(|line| line == "")
	match lines {
		[] => fail("header", "empty input")
		[header, .. as cases] => {
			match parse_header(header, "word", cases.len()) {
				Err(message) => fail("header", message)
				Ok({}) => match run_cases(cases, 0) {
					Ok(count) => "PASS\tword\t${count.to_str()}"
					Err({ case_id, message }) => fail(case_id, message)
				}
			}
		}
	}
}

## Keep named probes for high-risk rules legible even though WordBreakTest.txt
## also covers them exhaustively.
run_focused : U64 -> Try({}, Str)
run_focused = |_runtime_seed| {
	verify_complete("empty complete source", "", [0])?

	hebrew_single = source_from_code_points([0x05D0, 0x0027])?
	verify_complete("WB7a Hebrew single quote", hebrew_single, [0, 3])?

	hebrew_double = source_from_code_points([0x05D0, 0x0022, 0x05D1])?
	verify_complete("WB7b/WB7c Hebrew double quote", hebrew_double, [0, 5])?
	verify_cursor_chunks("WB7b/WB7c Hebrew cursor", ["", "א", "\"", "", "ב", ""], [0, 5])?

	verify_complete("WB3 CRLF", "\r\n", [0, 2])?
	newline_with_extend = source_from_code_points([0x000D, 0x0308, 0x000A])?
	verify_complete("WB3a/b newline", newline_with_extend, [0, 1, 3, 4])?
	verify_complete("WB3d WSegSpace", "  ", [0, 2])?
	verify_cursor_chunks("WB3d empty and multi chunk cursor", ["", " ", "", " ", ""], [0, 2])?

	# Exercise the complete and chunked block-fed paths at deterministic
	# boundaries. The expected one-range shape makes dropped or duplicated
	# per-lane emissions immediately visible as the work scales by blocks.
	ascii_block = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	ascii_two_blocks = Str.join_with([ascii_block, ascii_block], "")
	verify_complete("ASCII block work 64", ascii_block, [0, 64])?
	verify_complete("ASCII block work 128", ascii_two_blocks, [0, 128])?
	verify_cursor_chunks("ASCII block cursor scaling", [ascii_block, ascii_block], [0, 128])?
	context_block = Str.join_with(["aaaaaaaaaaaaaaa:", ascii_block], "")
	verify_complete("ASCII block pending context", context_block, [0, 80])?

	ri = source_from_code_points([0x1F1E6, 0x1F1E7, 0x1F1E8])?
	verify_complete("WB15/WB16 regional indicators", ri, [0, 8, 12])?

	zwj_ep = source_from_code_points([0x200D, 0x1F600])?
	verify_complete("WB3c ZWJ extended pictographic", zwj_ep, [0, 7])?

	mixed = source_from_code_points([
		0x0061,
		0x0308,
		0x0020,
		0x05D0,
		0x0022,
		0x05D1,
		0x1F1E6,
		0x1F1E7,
		0x1F1E8,
	])?
	expected_mixed = ["ä", " ", "א\"ב", "🇦🇧", "🇨"]
	verify_complete("mixed materializers", mixed, [0, 3, 4, 9, 17, 21])?
	if Word.slices(mixed) != expected_mixed {
		return Err("Word.slices did not reconstruct mixed segments")
	}
	if Word.owned(mixed) != expected_mixed {
		return Err("Word.owned did not reconstruct mixed segments")
	}

	initial = Word.Cursor.init({})
	pushed = match Word.Cursor.push(initial, "", [], |ranges, range| ranges.append(range)) {
		Failed({ error, .. }) => return Err("empty cursor push failed: ${Str.inspect(error)}")
		Pushed(value) => {
			if value.consumed != 0 {
				return Err("empty cursor push consumed bytes")
			}
			value
		}
	}
	finished = match Word.Cursor.finish(
		pushed.cursor,
		pushed.state,
		|ranges, range| ranges.append(range),
	) {
		Failed({ error, .. }) => return Err("empty cursor finish failed: ${Str.inspect(error)}")
		End(value) => value
	}
	if finished.state != [] {
		return Err("empty cursor emitted a range")
	}
	match Word.Cursor.finish(finished.cursor, [], |ranges, range| ranges.append(range)) {
		Failed({ error: AlreadyFinished, .. }) => {}
		_ => return Err("cursor finish is not sealed")
	}
	match Word.Cursor.push(finished.cursor, "a", [], |ranges, range| ranges.append(range)) {
		Failed({ error: AlreadyFinished, consumed: 0, .. }) => Ok({})
		_ => Err("cursor accepted a chunk after finish")
	}
}

parse_header : Str, Str, U64 -> Try({}, Str)
parse_header = |header, suite, actual_count| {
	match header.split_on("\t") {
		["ROC_UNICODE_TEST_V1", got_suite, count_str] if got_suite == suite => {
			expected_count = U64.from_str(count_str) ?? return Err("invalid header count")
			if expected_count == actual_count {
				Ok({})
			} else {
				Err("header count ${expected_count.to_str()} != ${actual_count.to_str()}")
			}
		}
		_ => Err("malformed protocol header")
	}
}

run_cases : List(Str), U64 -> Try(U64, { case_id : Str, message : Str })
run_cases = |remaining, count| {
	match remaining {
		[] => Ok(count)
		[line, .. as rest] => {
			run_case(line)?
			run_cases(rest, count + 1)
		}
	}
}

run_case : Str -> Try({}, { case_id : Str, message : Str })
run_case = |line| {
	match line.split_on("\t") {
		[case_id, code_points_hex, expected_offsets_str] => {
			code_points = code_points_hex.split_on(",").map(|hex| U32.from_str("0x${hex}"))
			expected_offsets = expected_offsets_str.split_on(",").map(U64.from_str)
			match (keep_oks(code_points), keep_oks(expected_offsets)) {
				(Ok(cps), Ok(expected)) => {
					source = keep_oks(cps.map(scalar_to_str))
					match source {
						Err(_) => Err({ case_id, message: "could not encode source scalars" })
						Ok(parts) => {
							str = Str.join_with(parts, "")
							match verify_complete(case_id, str, expected) {
								Err(message) => Err({ case_id, message })
								Ok({}) => match verify_two_chunk_splits(parts, expected) {
									Err(message) => Err({ case_id, message })
									Ok({}) => Ok({})
								}
							}
						}
					}
				}
				_ => Err({ case_id, message: "malformed numeric field" })
			}
		}
		[case_id, ..] => Err({ case_id, message: "malformed case row" })
		_ => Err({ case_id: "unknown", message: "malformed case row" })
	}
}

## Verify every scalar-aligned boundary, including the two empty-edge chunks.
verify_two_chunk_splits : List(Str), List(U64) -> Try({}, Str)
verify_two_chunk_splits = |parts, expected| visit_splits(parts, "", expected, 0)

visit_splits : List(Str), Str, List(U64), U64 -> Try({}, Str)
visit_splits = |remaining, prefix, expected, split_index| {
	suffix = Str.join_with(remaining, "")
	verify_cursor_chunks("split ${split_index.to_str()}", [prefix, suffix], expected)?
	match remaining {
		[] => Ok({})
		[part, .. as rest] => visit_splits(rest, Str.join_with([prefix, part], ""), expected, split_index + 1)
	}
}

verify_complete : Str, Str, List(U64) -> Try({}, Str)
verify_complete = |label, source, expected_offsets| {
	expected = expected_range_shape(expected_offsets)?
	actual_ranges = Word.ranges(source)
	actual = range_shape(actual_ranges)
	iterated = range_shape(Word.iter_ranges(source).fold([], |ranges, range| ranges.append(range)))
	ends = break_offsets(actual_ranges)
	if ends != expected_offsets {
		Err("${label}: UTF-8 ends expected ${Str.inspect(expected_offsets)}, got ${Str.inspect(ends)}")
	} else if actual != expected {
		Err("${label}: range partition expected ${Str.inspect(expected)}, got ${Str.inspect(actual)}")
	} else if iterated != actual {
		Err("${label}: iter_ranges disagrees with ranges")
	} else {
		Ok({})
	}
}

verify_cursor_chunks : Str, List(Str), List(U64) -> Try({}, Str)
verify_cursor_chunks = |label, chunks, expected_offsets| {
	expected = expected_range_shape(expected_offsets)?
	match cursor_ranges(chunks) {
		Err(error) => Err("${label}: cursor failed: ${Str.inspect(error)}")
		Ok(ranges) => {
			actual = range_shape(ranges)
			ends = break_offsets(ranges)
			if ends != expected_offsets {
				Err("${label}: cursor UTF-8 ends expected ${Str.inspect(expected_offsets)}, got ${Str.inspect(ends)}")
			} else if actual != expected {
				Err("${label}: cursor range partition expected ${Str.inspect(expected)}, got ${Str.inspect(actual)}")
			} else {
				Ok({})
			}
		}
	}
}

cursor_ranges : List(Str) -> Try(List(ByteRange), Word.Cursor.Error)
cursor_ranges = |chunks| {
	initial = { cursor: Word.Cursor.init({}), ranges: [] }
	pushed = chunks.fold(
		Ok(initial),
		|state, chunk| {
			current = state?
			match Word.Cursor.push(
				current.cursor,
				chunk,
				current.ranges,
				|ranges, range| ranges.append(range),
			) {
				Failed({ error, .. }) => Err(error)
				Pushed(value) => Ok({ cursor: value.cursor, ranges: value.state })
			}
		},
	)?
	match Word.Cursor.finish(
		pushed.cursor,
		pushed.ranges,
		|ranges, range| ranges.append(range),
	) {
		Failed({ error, .. }) => Err(error)
		End(value) => Ok(value.state)
	}
}

range_shape : List(ByteRange) -> List((U64, U64))
range_shape = |ranges| ranges.map(|range| (ByteRange.start(range), ByteRange.end(range)))

expected_range_shape : List(U64) -> Try(List((U64, U64)), Str)
expected_range_shape = |offsets| {
	match offsets {
		[0, .. as rest] => expected_ranges_from(0, rest, [])
		_ => Err("expected offsets must begin at zero")
	}
}

expected_ranges_from : U64, List(U64), List((U64, U64)) -> Try(List((U64, U64)), Str)
expected_ranges_from = |start, remaining, ranges| {
	match remaining {
		[] => Ok(ranges)
		[end, .. as rest] => {
			if end <= start {
				Err("expected offsets are not a nonempty partition")
			} else {
				expected_ranges_from(end, rest, ranges.append((start, end)))
			}
		}
	}
}

break_offsets : List(ByteRange) -> List(U64)
break_offsets = |ranges| ranges.fold([0], |offsets, range| offsets.append(ByteRange.end(range)))

source_from_code_points : List(U32) -> Try(Str, Str)
source_from_code_points = |code_points| {
	match keep_oks(code_points.map(scalar_to_str)) {
		Err(_) => Err("could not encode focused scalar source")
		Ok(parts) => Ok(Str.join_with(parts, ""))
	}
}

scalar_to_str : U32 -> Try(Str, [InvalidScalar, InternalEncodingFault])
scalar_to_str = |value| {
	match Scalar.from_u32(value) {
		Err(_) => {
			error : [InvalidScalar, InternalEncodingFault]
			error = InvalidScalar
			Err(error)
		}
		Ok(scalar) => {
			match Scalar.to_str(scalar) {
				Ok(encoded) => Ok(encoded)
				Err(_) => {
					error : [InvalidScalar, InternalEncodingFault]
					error = InternalEncodingFault
					Err(error)
				}
			}
		}
	}
}

keep_oks : List(Try(a, err)) -> Try(List(a), err)
keep_oks = |items| {
	items.fold(
		Ok([]),
		|state, item| {
			values = state?
			value = item?
			Ok(values.append(value))
		},
	)
}

fail : Str, Str -> Str
fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}"
