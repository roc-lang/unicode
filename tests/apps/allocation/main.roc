app [run!] {
	pf: platform "../../platform/main.roc",
	unicode: "../../../package/main.roc",
	test_unicode: "../../../package/test-main.roc",
}

import pf.Host
import test_unicode.TestPropertyAliases
import unicode.Bidi
import unicode.ByteRange
import unicode.Case
import unicode.Grapheme
import unicode.LineBreak
import unicode.TextPosition
import unicode.TextRange
import unicode.Word

run! : Str => Str
run! = |input| {
	lines = input.split_on("\n").drop_if(|line| line == "")
	match lines {
		[] => fail("header", "empty input")
		[header, .. as cases] => {
			match header.split_on("\t") {
				["ROC_UNICODE_TEST_V1", suite, count_str] => {
					expected_count = U64.from_str(count_str) ?? 0
					if expected_count != cases.len() {
						fail("header", "header count mismatch")
					} else {
						match run_cases!(suite, cases, 0) {
							Ok(count) => "PASS\t${suite}\t${count.to_str()}"
							Err({ case_id, message }) => fail(case_id, message)
						}
					}
				}
				_ => fail("header", "malformed protocol header")
			}
		}
	}
}

run_cases! : Str, List(Str), U64 => Try(U64, { case_id : Str, message : Str })
run_cases! = |suite, remaining, count| {
	match remaining {
		[] => Ok(count)
		[line, .. as rest] => match run_case!(suite, line) {
			Ok({}) => run_cases!(suite, rest, count + 1)
			Err(error) => Err(error)
		}
	}
}

run_case! : Str, Str => Try({}, { case_id : Str, message : Str })
run_case! = |suite, line| {
	match line.split_on("\t") {
		[case_id, input_hex, expectation] => {
			if suite == "allocation-bidi-scaling" {
				match input_hex.split_on("|") {
					[small_hex, large_hex] => match decode_hex(small_hex) {
						Err(_) => Err({ case_id, message: "malformed small UTF-8 hex input" })
						Ok(small) => match decode_hex(large_hex) {
							Err(_) => Err({ case_id, message: "malformed large UTF-8 hex input" })
							Ok(large) => verify_bidi_allocation_scaling!(case_id, small, large, expectation)
						}
					}
					_ => Err({ case_id, message: "allocation scaling input must contain two samples" })
				}
			} else {
				match decode_hex(input_hex) {
					Err(_) => Err({ case_id, message: "malformed UTF-8 hex input" })
					Ok(str) => {
						if suite == "allocation-aliases" {
							before = Host.alloc_count!({})
							checksum = TestPropertyAliases.allocation_probe(str.count_utf8_bytes().to_u8_wrap())
							after = Host.alloc_count!({})
							allocations = after - before
							expected = U64.from_str(expectation) ?? 18446744073709551615

							if checksum == 0 {
								Err({ case_id, message: "alias probe was optimized away or incomplete" })
							} else if allocations == expected {
								Ok({})
							} else {
								Err({ case_id, message: "expected ${expected.to_str()}, got ${allocations.to_str()} allocations" })
							}
						} else if suite == "allocation-line-break-cursor" {
							before = Host.alloc_count!({})
							signature = line_break_signature(str)
							after = Host.alloc_count!({})
							allocations = after - before
							expected = U64.from_str(expectation) ?? 18446744073709551615

							if signature == 0 {
								Err({ case_id, message: "line-break cursor probe was optimized away" })
							} else if allocations == expected {
								Ok({})
							} else {
								Err({ case_id, message: "expected ${expected.to_str()}, got ${allocations.to_str()} allocations" })
							}
						} else if suite == "allocation-bidi-analysis" {
							before = Host.alloc_count!({})
							signature = bidi_signature(str)
							after = Host.alloc_count!({})
							allocations = after - before
							if signature == 0 {
								Err({ case_id, message: "bidi analysis probe was optimized away or failed" })
							} else if expectation == "positive" and allocations > 0 {
								Ok({})
							} else {
								Err({ case_id, message: "expected ${expectation} allocations, got ${allocations.to_str()}" })
							}
						} else if suite.starts_with("allocation-word-") {
							before = Host.alloc_count!({})
							signature = word_signature(suite, str)
							after = Host.alloc_count!({})
							allocations = after - before
							expected = U64.from_str(expectation) ?? 18446744073709551615

							if signature == 0 {
								Err({ case_id, message: "word probe failed or was optimized away" })
							} else if allocations == expected {
								Ok({})
							} else {
								Err({ case_id, message: "expected ${expected.to_str()}, got ${allocations.to_str()} allocations" })
							}
						} else if suite.starts_with("allocation-case-") {
							before = Host.alloc_count!({})
							signature = case_signature(suite, case_id, str)
							after = Host.alloc_count!({})
							allocations = after - before
							expected = U64.from_str(expectation) ?? 18446744073709551615

							if signature == 0 {
								Err({ case_id, message: "Case probe failed or was optimized away" })
							} else if allocations == expected {
								Ok({})
							} else {
								Err({ case_id, message: "expected ${expected.to_str()}, got ${allocations.to_str()} allocations" })
							}
						} else {
							before = Host.alloc_count!({})
							result = if suite == "allocation-calibration" and expectation == "zero" {
								[]
							} else {
								Grapheme.ranges(str)
							}
							after = Host.alloc_count!({})
							allocations = after - before
							_ = result.len()

							if suite == "allocation-calibration" {
								if (expectation == "zero" and allocations == 0) or (expectation == "positive" and allocations > 0) {
									Ok({})
								} else {
									Err({ case_id, message: "expected ${expectation}, got ${allocations.to_str()} allocations" })
								}
							} else {
								expected = U64.from_str(expectation) ?? 18446744073709551615
								if allocations == expected {
									Ok({})
								} else {
									Err({ case_id, message: "expected ${expected.to_str()}, got ${allocations.to_str()} allocations" })
								}
							}
						}
					}
				}
			}
		}
		[case_id, ..] => Err({ case_id, message: "malformed case row" })
		_ => Err({ case_id: "unknown", message: "malformed case row" })
	}
}

## The retained analysis necessarily allocates, but its allocation count must
## grow monotonically and remain within a deliberately broad linear bound when
## the same adversarial family is quadrupled. This detects pathological growth
## in allocation *events* without baking allocator- or target-specific exact
## counts into the portable suite. It does not measure copied bytes; the
## multi-size benchmark separately provides retained-work scaling evidence.
verify_bidi_allocation_scaling! : Str, Str, Str, Str => Try({}, { case_id : Str, message : Str })
verify_bidi_allocation_scaling! = |case_id, small, large, expectation| {
	if expectation != "linear" {
		return Err({ case_id, message: "unknown bidi allocation scaling expectation" })
	}
	small_measurement = bidi_allocation_measurement!(small)
	large_measurement = bidi_allocation_measurement!(large)
	if small_measurement.signature == 0 or large_measurement.signature == 0 {
		Err({ case_id, message: "bidi allocation scaling probe was optimized away or failed" })
	} else if small_measurement.allocations == 0 or large_measurement.allocations == 0 {
		Err({ case_id, message: "bidi allocation scaling unexpectedly made no allocations" })
	} else if large_measurement.allocations < small_measurement.allocations {
		Err({ case_id, message: "larger bidi input made fewer allocations (${small_measurement.allocations.to_str()} -> ${large_measurement.allocations.to_str()})" })
	} else if large_measurement.allocations > small_measurement.allocations * 8 + 128 {
		Err({ case_id, message: "bidi allocations exceeded broad linear bound (${small_measurement.allocations.to_str()} -> ${large_measurement.allocations.to_str()})" })
	} else {
		Ok({})
	}
}

bidi_allocation_measurement! : Str => { allocations : U64, signature : U64 }
bidi_allocation_measurement! = |source| {
	before = Host.alloc_count!({})
	signature = bidi_signature(source)
	after = Host.alloc_count!({})
	{ allocations: after - before, signature }
}

bidi_signature : Str -> U64
bidi_signature = |source| {
	analysis = Bidi.analyze_paragraph(source, Auto, Bidi.default_limits) ?? return 0
	line = Bidi.reorder_line(analysis, TextRange.scalar_range(Bidi.paragraph_range(analysis))) ?? return 0
	Bidi.entries(analysis).len() + Bidi.visual_to_logical(line).len() + 1
}

line_break_signature : Str -> U64
line_break_signature = |source| {
	pushed = match LineBreak.Cursor.push(
		LineBreak.Cursor.init({}),
		source,
		{ count: 0.U64, weighted_offsets: 0.U64 },
		|state, event| {
			next_count = state.count + 1
			{
				count: next_count,
				weighted_offsets: state.weighted_offsets + next_count * TextPosition.byte_offset(event.at),
			}
		},
	) {
		Failed(_) => return 18446744073709551615
		Pushed(value) => value
	}
	finished = match LineBreak.Cursor.finish(
		pushed.cursor,
		pushed.state,
		|state, event| {
			next_count = state.count + 1
			{
				count: next_count,
				weighted_offsets: state.weighted_offsets + next_count * TextPosition.byte_offset(event.at),
			}
		},
	) {
		Failed(_) => return 18446744073709551615
		End(value) => value
	}
	finished.state.count + finished.state.weighted_offsets + 1
}

word_signature : Str, Str -> U64
word_signature = |suite, source| {
	if suite == "allocation-word-iterator" {
		Word.iter_ranges(source).fold(1, add_range_signature)
	} else if suite == "allocation-word-cursor" {
		word_cursor_signature(source)
	} else if suite == "allocation-word-ranges" {
		Word.ranges(source).fold(1, add_range_signature)
	} else if suite == "allocation-word-slices" {
		Word.slices(source).fold(1, |sum, item| sum + item.count_utf8_bytes() + 1)
	} else if suite == "allocation-word-owned" {
		Word.owned(source).fold(1, |sum, item| sum + item.count_utf8_bytes() + 1)
	} else {
		0
	}
}

add_range_signature : U64, ByteRange -> U64
add_range_signature = |sum, range| sum + ByteRange.start(range) + ByteRange.end(range) + 1

word_cursor_signature : Str -> U64
word_cursor_signature = |source| {
	pushed = match Word.Cursor.push(
		Word.Cursor.init({}),
		source,
		1,
		add_range_signature,
	) {
		Failed(_) => return 0
		Pushed(value) => value
	}
	finished = match Word.Cursor.finish(
		pushed.cursor,
		pushed.state,
		add_range_signature,
	) {
		Failed(_) => return 0
		End(value) => value
	}
	finished.state
}

case_signature : Str, Str, Str -> U64
case_signature = |suite, case_id, source| {
	max = U64.highest
	outcome = if suite == "allocation-case-lower-default" {
		Case.to_lower(source, Case.unicode_default, Case.unlimited_limits)
	} else if suite == "allocation-case-lower-turkic" {
		Case.to_lower(source, Case.turkic, Case.unlimited_limits)
	} else if suite == "allocation-case-upper-default" {
		Case.to_upper(source, Case.unicode_default, Case.unlimited_limits)
	} else if suite == "allocation-case-upper-lithuanian" {
		Case.to_upper(source, Case.lithuanian, Case.unlimited_limits)
	} else if suite == "allocation-case-title-default" {
		Case.to_title(source, Case.unicode_default, Case.unlimited_limits)
	} else if suite == "allocation-case-title-turkic" {
		Case.to_title(source, Case.turkic, Case.unlimited_limits)
	} else if suite == "allocation-case-fold-full" {
		Case.fold(source, Case.full, Case.unlimited_limits)
	} else if suite == "allocation-case-fold-simple" {
		Case.fold(source, Case.simple, Case.unlimited_limits)
	} else if suite == "allocation-case-fold-turkic-full" {
		Case.fold(source, Case.turkic_full, Case.unlimited_limits)
	} else if suite == "allocation-case-fold-turkic-simple" {
		Case.fold(source, Case.turkic_simple, Case.unlimited_limits)
	} else if suite == "allocation-case-limits" {
		limits = match case_id {
			"input-bytes" => Case.limits(0, max, max, max, max)
			"input-scalars" => Case.limits(max, 0, max, max, max)
			"output-bytes" => Case.limits(max, max, 0, max, max)
			"output-scalars" => Case.limits(max, max, max, 0, max)
			"facts" => Case.limits(max, max, max, max, 0)
			_ => return 0
		}
		Case.to_lower(source, Case.unicode_default, limits)
	} else {
		return 0
	}

	match outcome {
		Ok(result) => {
			facts = Case.result_facts(result)
			result_bytes = Case.result_text(result).count_utf8_bytes()
			facts.fold(
				result_bytes + 1,
				|sum, fact| {
					output = TextRange.byte_range(Case.fact_output(fact))
					sum + ByteRange.end(output) + 1
				},
			)
		}
		Err(error) => match (Case.error_kind(error), Case.error_limit(error)) {
			(LimitExceeded, Some(limit)) => limit.limit + limit.required + 1
			_ => 0
		}
	}
}

decode_hex = |hex| {
	if hex == "" {
		Ok("")
	} else {
		parsed = hex.split_on(",").map(|byte| U8.from_str("0x${byte}"))
		match keep_oks(parsed) {
			Err(error) => Err(error)
			Ok(bytes) => Str.from_utf8(bytes)
		}
	}
}

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

fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}"
