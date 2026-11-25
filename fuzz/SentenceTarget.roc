import fuzz.Fuzz
import FuzzSupport
import unicode.ByteRange
import unicode.Sentence

SentenceTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-sentence",
		examples: [
			Fuzz.saved({
				name: "pending-period-with-lowercase-resolution",
				revision: "1",
				bytes: [1, 0x41, 0, 0, 0x2E, 0, 0, 0x20, 0, 0x20, 0x22, 0x21, 0x60, 0x61, 0, 0x80],
			}),
		],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	parts = FuzzSupport.source_parts(input.code_points)
	source = Str.join_with(parts, "")
	ranges = Sentence.ranges(source)
	append = |items, item| items.append(item)
	if Sentence.fold_ranges(source, [], append) != ranges {
		crash "sentence fold disagreed with collected ranges"
	}
	stop = input.config.to_u64() % (parts.len() + 1)
	if FuzzSupport.pause_iter(Sentence.iter_ranges(source), stop) != ranges {
		crash "sentence iterator resume changed the ranges"
	}
	if cursor_ranges([source]) != ranges or cursor_ranges(FuzzSupport.chunks(input, parts)) != ranges {
		crash "sentence cursor changed the ranges"
	}
	var $at = 0.U64
	var $expected = []
	for range in ranges {
		if range.start() != $at or range.end() <= $at {
			crash "sentence range was empty, overlapping, or discontinuous"
		}
		$expected = $expected.append(range.slice(source) ?? crash "sentence range was not scalar-aligned and in bounds")
		$at = range.end()
	}
	if $at != source.count_utf8_bytes() {
		crash "sentence ranges did not cover every source byte"
	}
	if Sentence.slices(source) != $expected or Sentence.owned(source) != $expected {
		crash "sentence materializers did not select the ranges"
	}
	length = source.count_utf8_bytes()
	limit = input.config.to_u64() % (ranges.len() + 1)
	expected_limited = if limit == ranges.len() {
		Ok(ranges)
	} else {
		Err(LimitExceeded({ resource: OutputItems, limit, required: limit + 1 }))
	}
	if Sentence.ranges_limited(source, { max_input_bytes: length, max_items: limit }) != expected_limited {
		crash "sentence collector did not enforce the exact output-item limit"
	}
	Fuzz.keep
}

cursor_ranges : List(Str) -> List(ByteRange)
cursor_ranges = |chunks| {
	var $cursor = Sentence.Cursor.init({})
	var $ranges = []
	append = |items, item| items.append(item)
	for chunk in chunks {
		match Sentence.Cursor.push($cursor, chunk, $ranges, append) {
			Pushed(result) => {
				if result.consumed != chunk.count_utf8_bytes() {
					crash "sentence cursor consumed an unexpected byte count"
				}
				$cursor = result.cursor
				$ranges = result.state
			}
			Failed(_) => crash "sentence cursor rejected a valid chunk"
		}
	}
	done = match Sentence.Cursor.finish($cursor, $ranges, append) {
		End(result) => result
		Failed(_) => crash "sentence cursor did not finish"
	}
	match Sentence.Cursor.finish(done.cursor, [], append) {
		Failed({ error: AlreadyFinished, state, .. }) if state.is_empty() => {}
		_ => crash "sentence cursor was not sealed on finish"
	}
	match Sentence.Cursor.push(done.cursor, "a", [], append) {
		Failed({ error: AlreadyFinished, consumed: 0, state, .. }) if state.is_empty() => {}
		_ => crash "sentence cursor accepted a chunk after finish"
	}
	done.state
}
