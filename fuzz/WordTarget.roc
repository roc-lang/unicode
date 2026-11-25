import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Word

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	code_points = input.code_points
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")
	collected = Word.ranges(source)
	iterated = FuzzSupport.pause_iter(Word.iter_ranges(source), input.config.to_u64() % (parts.len() + 1))
	whole_cursor = cursor_ranges([source])
	scalar_cursor = cursor_ranges([""].concat(parts).append(""))
	planned_cursor = cursor_ranges(FuzzSupport.chunks(input, parts))

	if iterated != collected {
		crash "Word.iter_ranges disagreed with Word.ranges"
	}
	if whole_cursor != collected {
		crash "whole-chunk Word.Cursor disagreed with Word.ranges"
	}
	if scalar_cursor != collected or planned_cursor != collected {
		crash "scalar or planned chunk Word.Cursor disagreed with Word.ranges"
	}

	validate_partition(source, collected)
	expected_materialized = materialize(source, collected)
	if Word.slices(source) != expected_materialized {
		crash "Word.slices disagreed with range materialization"
	}
	if Word.owned(source) != expected_materialized {
		crash "Word.owned disagreed with range materialization"
	}
	if Str.join_with(expected_materialized, "") != source {
		crash "word materialization lost or changed source text"
	}

	FuzzSupport.check_bounded_collection("Word.ranges_limited", source, input.config, collected, |limits| Word.ranges_limited(source, limits))
	FuzzSupport.check_bounded_collection("Word.slices_limited", source, input.config, expected_materialized, |limits| Word.slices_limited(source, limits))
	FuzzSupport.check_bounded_collection("Word.owned_limited", source, input.config, expected_materialized, |limits| Word.owned_limited(source, limits))

	for word in expected_materialized {
		inner = Word.ranges(word)
		if inner.len() != 1 {
			crash "a word-boundary range was not idempotent under segmentation"
		}
		only = inner.get(0) ?? crash "one word range could not be read"
		if ByteRange.start(only) != 0 or ByteRange.end(only) != word.count_utf8_bytes() {
			crash "an idempotent word range did not cover its slice"
		}
	}

	Fuzz.keep
}

cursor_ranges : List(Str) -> List(ByteRange)
cursor_ranges = |chunks| {
	var $cursor = Word.Cursor.init({})
	var $ranges = []
	for chunk in chunks {
		pushed = Word.Cursor.push($cursor, chunk, $ranges, |ranges, range| ranges.append(range))
		match pushed {
			Failed(_) => crash "Word.Cursor.push failed for scalar-aligned input"
			Pushed(next) => {
				$cursor = next.cursor
				$ranges = next.state
			}
		}
	}
	finished = Word.Cursor.finish($cursor, $ranges, |ranges, range| ranges.append(range))
	match finished {
		Failed(_) => crash "Word.Cursor.finish failed for an open cursor"
		End(done) => {
			match Word.Cursor.finish(done.cursor, [], |ranges, range| ranges.append(range)) {
				Failed({ error: AlreadyFinished, .. }) => {}
				_ => crash "Word.Cursor was not sealed after finish"
			}
			match Word.Cursor.push(done.cursor, "a", [], |ranges, range| ranges.append(range)) {
				Failed({ error: AlreadyFinished, consumed: 0, .. }) => {}
				_ => crash "Word.Cursor accepted a push after finish"
			}
			done.state
		}
	}
}

validate_partition : Str, List(ByteRange) -> {}
validate_partition = |source, ranges| {
	if source == "" and !ranges.is_empty() {
		crash "empty text produced a word range"
	}
	if source != "" and ranges.is_empty() {
		crash "nonempty text produced no word ranges"
	}
	var $next_start = 0
	for range in ranges {
		start = ByteRange.start(range)
		end = ByteRange.end(range)
		if start != $next_start or end <= start {
			crash "word ranges were empty, overlapping, or discontinuous"
		}
		_ = ByteRange.slice(range, source) ?? crash "word range was not scalar-aligned and in bounds"
		$next_start = end
	}
	if $next_start != source.count_utf8_bytes() {
		crash "word ranges did not cover the complete source"
	}
	{}
}

materialize : Str, List(ByteRange) -> List(Str)
materialize = |source, ranges| {
	ranges.map(|range| ByteRange.slice(range, source) ?? crash "validated word range could not be sliced")
}

WordTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-word",
		examples: [Fuzz.saved({ name: "pending-midletter", revision: "1", bytes: [0x00, 0x61, 0x00, 0x00, 0x27, 0x00, 0x20, 0x01, 0x03, 0x40, 0x62, 0x00, 0x60] })],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
