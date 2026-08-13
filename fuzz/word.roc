app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.2.1/9Qpttb6LTgcMaVsSBLsnaiS2mDUrf6Bxa6dX9Rqwviz4.tar.zst",
	unicode: "../package/main.roc",
}

import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Word

test : List(U32) -> Fuzz.Outcome
test = |code_points| {
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")
	collected = Word.ranges(source)
	iterated = Word.iter_ranges(source).fold([], |ranges, range| ranges.append(range))
	whole_cursor = cursor_ranges([source])
	scalar_cursor = cursor_ranges([""].concat(parts).append(""))

	if iterated != collected {
		crash "Word.iter_ranges disagreed with Word.ranges"
	}
	if whole_cursor != collected {
		crash "whole-chunk Word.Cursor disagreed with Word.ranges"
	}
	if scalar_cursor != collected {
		crash "scalar-chunk Word.Cursor disagreed with Word.ranges"
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

target = Fuzz.target_with({
	name: "unicode-word",
	generator: FuzzSupport.scalar_sequence,
	test,
	show: FuzzSupport.show_scalars,
})
