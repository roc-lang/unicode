app [run!] {
	pf: platform "../../platform/main.roc",
	unicode: "../../../package/main.roc",
}

import unicode.ByteRange
import unicode.Scalar
import unicode.Word
import unicode.Sentence

Family : [Words, Sentences]

run! : Str => Str
run! = |input| {
	lines = input.split_on("\n").drop_if(|line| line == "")
	match lines {
		[] => fail("header", "empty input")
		[header, .. as cases] => {
			match parse_header(header, cases.len()) {
				Err(message) => fail("header", message)
				Ok((suite, family)) => {
					focused = match family {
						Words => run_focused(input.count_utf8_bytes())
						Sentences => sentence_focused(input.count_utf8_bytes())
					}
					match focused {
						Err(message) => return fail("focused", message)
						Ok({}) => {}
					}
					match run_cases(family, cases, 0) {
						Ok(count) => "PASS\t${suite}\t${count.to_str()}"
						Err({ case_id, message }) => fail(case_id, message)
					}
				}
			}
		}
	}
}

## Keep named probes for high-risk rules legible even though WordBreakTest.txt
## also covers them exhaustively.
run_focused : U64 -> Try({}, Str)
run_focused = |_runtime_seed| {
	verify_complete(Words, "empty complete source", "", [0])?

	hebrew_single = source_from_code_points([0x05D0, 0x0027])?
	verify_complete(Words, "WB7a Hebrew single quote", hebrew_single, [0, 3])?

	hebrew_double = source_from_code_points([0x05D0, 0x0022, 0x05D1])?
	verify_complete(Words, "WB7b/WB7c Hebrew double quote", hebrew_double, [0, 5])?
	verify_cursor_chunks(Words, "WB7b/WB7c Hebrew cursor", ["", "א", "\"", "", "ב", ""], [0, 5])?

	verify_complete(Words, "WB3 CRLF", "\r\n", [0, 2])?
	newline_with_extend = source_from_code_points([0x000D, 0x0308, 0x000A])?
	verify_complete(Words, "WB3a/b newline", newline_with_extend, [0, 1, 3, 4])?
	verify_complete(Words, "WB3d WSegSpace", "  ", [0, 2])?
	verify_cursor_chunks(Words, "WB3d empty and multi chunk cursor", ["", " ", "", " ", ""], [0, 2])?

	# Exercise the complete and chunked block-fed paths at deterministic
	# boundaries. The expected one-range shape makes dropped or duplicated
	# per-lane emissions immediately visible as the work scales by blocks.
	ascii_block = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	ascii_two_blocks = Str.join_with([ascii_block, ascii_block], "")
	verify_complete(Words, "ASCII block work 64", ascii_block, [0, 64])?
	verify_complete(Words, "ASCII block work 128", ascii_two_blocks, [0, 128])?
	verify_cursor_chunks(Words, "ASCII block cursor scaling", [ascii_block, ascii_block], [0, 128])?
	context_block = Str.join_with(["aaaaaaaaaaaaaaa:", ascii_block], "")
	verify_complete(Words, "ASCII block pending context", context_block, [0, 80])?

	ri = source_from_code_points([0x1F1E6, 0x1F1E7, 0x1F1E8])?
	verify_complete(Words, "WB15/WB16 regional indicators", ri, [0, 8, 12])?

	zwj_ep = source_from_code_points([0x200D, 0x1F600])?
	verify_complete(Words, "WB3c ZWJ extended pictographic", zwj_ep, [0, 7])?

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
	verify_complete(Words, "mixed materializers", mixed, [0, 3, 4, 9, 17, 21])?
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

sentence_focused : U64 -> Try({}, Str)
sentence_focused = |runtime_seed| {
	verify_complete(Sentences, "empty complete source", "", [0])?
	verify_complete(Sentences, "SB3: CRLF", "1\r\n2", [0, 3, 4])?
	verify_complete(Sentences, "SB4 above SB5", "A\n\u(0301)B", [0, 2, 5])?
	verify_complete(Sentences, "SB6: decimal", "1.2", [0, 3])?
	verify_complete(Sentences, "SB7: initials", "U.S.", [0, 4])?
	verify_complete(Sentences, "SB8a: continuation", "A!; B", [0, 5])?
	verify_complete(Sentences, "SB11: closing punctuation and spaces", "\"Stop!\"  Go.", [0, 9, 12])?
	verify_complete(Sentences, "default abbreviation policy", "Mr. Silva", [0, 4, 9])?

	asides = Str.join_with(List.repeat("™\u(0301)", 64), "")
	prefix = "É. "
	at = prefix.count_utf8_bytes()
	end = at + asides.count_utf8_bytes() + 1
	verify_complete(Sentences, "SB8 joins through long lowercase lookahead", Str.join_with([prefix, asides, "a"], ""), [0, end])?
	verify_complete(Sentences, "SB8 emits earlier cut at uppercase", Str.join_with([prefix, asides, "A"], ""), [0, at, end])?
	verify_cursor_chunks(Sentences, "SB8 defers across chunk ends", [prefix, "", asides, "", "a"], [0, end])?
	verify_cursor_chunks(Sentences, "SB8 resolves two ranges at finish", [prefix, asides, ""], [0, at, end - 1])?

	initial = Sentence.Cursor.init({})
	append = |items, item| items.append(item)
	pending_input = if (runtime_seed > 0) "A. ™" else ""
	pending = match Sentence.Cursor.push(initial, pending_input, [], append) {
		Pushed(result) if result.consumed == 6 and result.state.is_empty() => result
		_ => return Err("pending SB8 cut was emitted before right context")
	}
	finished = match Sentence.Cursor.finish(pending.cursor, pending.state, append) {
		End(result) if break_offsets(result.state) == [0, 3, 6] => result
		_ => return Err("finish did not resolve the pending SB8 cut and trailing range")
	}
	match Sentence.Cursor.finish(finished.cursor, [], append) {
		Failed({ error: AlreadyFinished, .. }) => {}
		_ => return Err("finish was not sealed")
	}
	match Sentence.Cursor.push(finished.cursor, "x", [], append) {
		Failed({ error: AlreadyFinished, consumed: 0, state, .. }) if state.is_empty() => {}
		_ => return Err("push after finish was not rejected")
	}

	mixed = "É. Γ!"
	expected = ["É. ", "Γ!"]
	if Sentence.slices(mixed) == expected and Sentence.owned(mixed) == expected {
		Ok({})
	} else {
		Err("sentence materializers changed the source")
	}
}

parse_header : Str, U64 -> Try((Str, Family), Str)
parse_header = |header, actual_count| {
	match header.split_on("\t") {
		["ROC_UNICODE_TEST_V1", suite, count_str] => {
			family = match suite {
				"word" => Words
				"sentence" => Sentences
				_ => return Err("unknown suite")
			}
			expected_count = U64.from_str(count_str) ?? return Err("invalid header count")
			if expected_count == actual_count {
				Ok((suite, family))
			} else {
				Err("header count ${expected_count.to_str()} != ${actual_count.to_str()}")
			}
		}
		_ => Err("malformed protocol header")
	}
}

run_cases : Family, List(Str), U64 -> Try(U64, { case_id : Str, message : Str })
run_cases = |family, remaining, count| {
	match remaining {
		[] => Ok(count)
		[line, .. as rest] => {
			run_case(family, line)?
			run_cases(family, rest, count + 1)
		}
	}
}

run_case : Family, Str -> Try({}, { case_id : Str, message : Str })
run_case = |family, line| {
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
							match verify_complete(family, case_id, str, expected) {
								Err(message) => Err({ case_id, message })
								Ok({}) => match verify_two_chunk_splits(family, parts, expected) {
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
verify_two_chunk_splits : Family, List(Str), List(U64) -> Try({}, Str)
verify_two_chunk_splits = |family, parts, expected| visit_splits(family, parts, "", expected, 0)

visit_splits : Family, List(Str), Str, List(U64), U64 -> Try({}, Str)
visit_splits = |family, remaining, prefix, expected, split_index| {
	suffix = Str.join_with(remaining, "")
	verify_cursor_chunks(family, "split ${split_index.to_str()}", [prefix, suffix], expected)?
	match remaining {
		[] => Ok({})
		[part, .. as rest] => visit_splits(family, rest, Str.join_with([prefix, part], ""), expected, split_index + 1)
	}
}

verify_complete : Family, Str, Str, List(U64) -> Try({}, Str)
verify_complete = |family, label, source, expected_offsets| {
	expected = expected_range_shape(expected_offsets)?
	append = |ranges, range| ranges.append(range)
	(actual_ranges, visited) = match family {
		Words => (Word.ranges(source), Word.iter_ranges(source).fold([], append))
		Sentences => (Sentence.ranges(source), Sentence.iter_ranges(source).fold([], append))
	}
	actual = range_shape(actual_ranges)
	iterated = range_shape(visited)
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

verify_cursor_chunks : Family, Str, List(Str), List(U64) -> Try({}, Str)
verify_cursor_chunks = |family, label, chunks, expected_offsets| {
	expected = expected_range_shape(expected_offsets)?
	match cursor_ranges(family, chunks) {
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

cursor_ranges : Family, List(Str) -> Try(List(ByteRange), Word.Cursor.Error)
cursor_ranges = |family, chunks| match family {
	Words => collect_cursor(chunks, Word.Cursor.init({}), Word.Cursor.push, Word.Cursor.finish)
	Sentences => collect_cursor(chunks, Sentence.Cursor.init({}), Sentence.Cursor.push, Sentence.Cursor.finish)
}

collect_cursor = |chunks, cursor, push, finish| {
	initial = { cursor, ranges: [] }
	pushed = chunks.fold(
		Ok(initial),
		|state, chunk| {
			current = state?
			match push(
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
	match finish(
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
