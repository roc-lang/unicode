app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.2.1/9Qpttb6LTgcMaVsSBLsnaiS2mDUrf6Bxa6dX9Rqwviz4.tar.zst",
	unicode: "../package/main.roc",
}

import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Grapheme

test : List(U32) -> Fuzz.Outcome
test = |code_points| {
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")
	collected = Grapheme.ranges(source)
	iterated = Grapheme.iter_ranges(source).fold([], |ranges, range| ranges.append(range))
	whole_cursor = cursor_ranges([source])
	scalar_cursor = cursor_ranges([""].concat(parts).append(""))

	if iterated != collected {
		crash "Grapheme.iter_ranges disagreed with Grapheme.ranges"
	}
	if whole_cursor != collected {
		crash "whole-chunk Grapheme.Cursor disagreed with Grapheme.ranges"
	}
	if scalar_cursor != collected {
		crash "scalar-chunk Grapheme.Cursor disagreed with Grapheme.ranges"
	}

	validate_partition(source, collected)
	expected_materialized = materialize(source, collected)
	if Grapheme.slices(source) != expected_materialized {
		crash "Grapheme.slices disagreed with range materialization"
	}
	if Grapheme.split(source) != expected_materialized {
		crash "Grapheme.split disagreed with range materialization"
	}
	if Str.join_with(expected_materialized, "") != source {
		crash "grapheme materialization lost or changed source text"
	}

	for cluster in expected_materialized {
		inner = Grapheme.ranges(cluster)
		if inner.len() != 1 {
			crash "a grapheme cluster was not idempotent under segmentation"
		}
		only = inner.get(0) ?? crash "one grapheme range could not be read"
		if ByteRange.start(only) != 0 or ByteRange.end(only) != cluster.count_utf8_bytes() {
			crash "an idempotent grapheme range did not cover its cluster"
		}
	}

	Fuzz.keep
}

cursor_ranges : List(Str) -> List(ByteRange)
cursor_ranges = |chunks| {
	var $cursor = Grapheme.Cursor.init({})
	var $ranges = []
	for chunk in chunks {
		pushed = Grapheme.Cursor.push($cursor, chunk, $ranges, |ranges, range| ranges.append(range))
		match pushed {
			Err(_) => crash "Grapheme.Cursor.push failed for scalar-aligned input"
			Ok(next) => {
				$cursor = next.cursor
				$ranges = next.state
			}
		}
	}
	finished = Grapheme.Cursor.finish($cursor, $ranges, |ranges, range| ranges.append(range))
	match finished {
		Err(_) => crash "Grapheme.Cursor.finish failed for an open cursor"
		Ok(done) => {
			match Grapheme.Cursor.finish(done.cursor, [], |ranges, range| ranges.append(range)) {
				Err(AlreadyFinished) => {}
				_ => crash "Grapheme.Cursor was not sealed after finish"
			}
			done.state
		}
	}
}

validate_partition : Str, List(ByteRange) -> {}
validate_partition = |source, ranges| {
	if source == "" and !ranges.is_empty() {
		crash "empty text produced a grapheme range"
	}
	if source != "" and ranges.is_empty() {
		crash "nonempty text produced no grapheme ranges"
	}
	var $next_start = 0
	for range in ranges {
		start = ByteRange.start(range)
		end = ByteRange.end(range)
		if start != $next_start or end <= start {
			crash "grapheme ranges were empty, overlapping, or discontinuous"
		}
		_ = ByteRange.slice(range, source) ?? crash "grapheme range was not scalar-aligned and in bounds"
		$next_start = end
	}
	if $next_start != source.count_utf8_bytes() {
		crash "grapheme ranges did not cover the complete source"
	}
	{}
}

materialize : Str, List(ByteRange) -> List(Str)
materialize = |source, ranges| {
	ranges.map(|range| ByteRange.slice(range, source) ?? crash "validated grapheme range could not be sliced")
}

target = Fuzz.target_with({
	name: "unicode-grapheme",
	generator: FuzzSupport.scalar_sequence,
	test,
	show: FuzzSupport.show_scalars,
})
