app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.2.1/9Qpttb6LTgcMaVsSBLsnaiS2mDUrf6Bxa6dX9Rqwviz4.tar.zst",
	unicode: "../package/main.roc",
}

import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.LineBreak
import unicode.TextPosition

test : List(U32) -> Fuzz.Outcome
test = |code_points| {
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")

	boundaries = LineBreak.boundaries(source)
	iterated_boundaries = LineBreak.iter_boundaries(source).fold([], |items, item| items.append(item))
	opportunities = LineBreak.opportunities(source)
	iterated_opportunities = LineBreak.iter_opportunities(source).fold([], |items, item| items.append(item))
	whole_cursor = cursor_opportunities([source])
	scalar_cursor = cursor_opportunities([""].concat(parts).append(""))

	if iterated_boundaries != boundaries {
		crash "LineBreak.iter_boundaries disagreed with LineBreak.boundaries"
	}
	if iterated_opportunities != opportunities {
		crash "LineBreak.iter_opportunities disagreed with LineBreak.opportunities"
	}
	if whole_cursor != opportunities {
		crash "whole-chunk LineBreak.Cursor disagreed with LineBreak.opportunities"
	}
	if scalar_cursor != opportunities {
		crash "scalar-chunk LineBreak.Cursor disagreed with LineBreak.opportunities"
	}
	if boundaries_as_opportunities(boundaries) != opportunities {
		crash "opportunities were not the non-Prohibited subset of boundaries"
	}

	validate_boundaries(source, boundaries, code_points.len())

	Fuzz.keep
}

cursor_opportunities : List(Str) -> List(LineBreak.BreakOpportunity)
cursor_opportunities = |chunks| {
	var $cursor = LineBreak.Cursor.init({})
	var $items = []
	for chunk in chunks {
		pushed = LineBreak.Cursor.push($cursor, chunk, $items, |items, item| items.append(item))
		match pushed {
			Failed(_) => crash "LineBreak.Cursor.push failed for scalar-aligned input"
			Pushed(next) => {
				$cursor = next.cursor
				$items = next.state
			}
		}
	}
	finished = LineBreak.Cursor.finish($cursor, $items, |items, item| items.append(item))
	match finished {
		Failed(_) => crash "LineBreak.Cursor.finish failed for an open cursor"
		End(done) => {
			match LineBreak.Cursor.finish(done.cursor, [], |items, item| items.append(item)) {
				Failed({ error: AlreadyFinished, .. }) => {}
				_ => crash "LineBreak.Cursor was not sealed after finish"
			}
			done.state
		}
	}
}

## The exhaustive boundary stream reports Prohibited, Allowed, and Mandatory
## decisions; the opportunity stream reports only the non-Prohibited subset
## with the same positions and authorities.
boundaries_as_opportunities : List(LineBreak.BreakBoundary) -> List(LineBreak.BreakOpportunity)
boundaries_as_opportunities = |boundaries| {
	var $result = []
	for boundary in boundaries {
		match boundary.decision {
			Prohibited => {}
			Mandatory => {
				$result = $result.append({ at: boundary.at, decision: Mandatory, authority: boundary.authority })
			}
			Allowed => {
				$result = $result.append({ at: boundary.at, decision: Allowed, authority: boundary.authority })
			}
		}
	}
	$result
}

validate_boundaries : Str, List(LineBreak.BreakBoundary), U64 -> {}
validate_boundaries = |source, boundaries, scalar_count| {
	total_bytes = source.count_utf8_bytes()

	first = boundaries.get(0) ?? crash "LineBreak.boundaries produced no entries"
	if first.decision != Prohibited or first.authority != NonTailorable or TextPosition.byte_offset(first.at) != 0 or TextPosition.scalar_offset(first.at) != 0 {
		crash "line-break boundaries did not start with the LB2 prohibited start-of-text marker"
	}

	last = boundaries.last() ?? crash "LineBreak.boundaries produced no entries"
	if last.decision != Mandatory or last.authority != NonTailorable or TextPosition.byte_offset(last.at) != total_bytes or TextPosition.scalar_offset(last.at) != scalar_count {
		crash "line-break boundaries did not end with the LB3 mandatory end-of-text break"
	}

	var $previous_byte = 0
	var $previous_scalar = 0
	for boundary in boundaries {
		byte_offset = TextPosition.byte_offset(boundary.at)
		scalar_offset = TextPosition.scalar_offset(boundary.at)
		if byte_offset < $previous_byte or scalar_offset < $previous_scalar {
			crash "line-break boundary offsets went backwards"
		}
		if byte_offset > total_bytes or scalar_offset > scalar_count {
			crash "line-break boundary offsets exceeded the source"
		}
		$previous_byte = byte_offset
		$previous_scalar = scalar_offset
	}

	if source == "" {
		if boundaries.len() != 2 {
			crash "empty source did not produce exactly the LB2/LB3 boundary pair"
		}
	} else {
		validate_partition(source, boundary_ranges(boundaries))
	}
	{}
}

## Consecutive boundary positions bound the scalar-aligned spans between them.
boundary_ranges : List(LineBreak.BreakBoundary) -> List(ByteRange)
boundary_ranges = |boundaries| {
	var $ranges = []
	var $previous = None
	for boundary in boundaries {
		match $previous {
			None => {}
			Some(previous_byte) => {
				range = ByteRange.from_bounds(previous_byte, TextPosition.byte_offset(boundary.at)) ?? crash "line-break boundaries were not monotonic"
				$ranges = $ranges.append(range)
			}
		}
		$previous = Some(TextPosition.byte_offset(boundary.at))
	}
	$ranges
}

validate_partition : Str, List(ByteRange) -> {}
validate_partition = |source, ranges| {
	if ranges.is_empty() {
		crash "nonempty text produced no line-break spans"
	}
	var $next_start = 0
	for range in ranges {
		start = ByteRange.start(range)
		end = ByteRange.end(range)
		if start != $next_start or end <= start {
			crash "line-break spans were empty, overlapping, or discontinuous"
		}
		_ = ByteRange.slice(range, source) ?? crash "line-break span was not scalar-aligned and in bounds"
		$next_start = end
	}
	if $next_start != source.count_utf8_bytes() {
		crash "line-break spans did not cover the complete source"
	}
	{}
}

target = Fuzz.target_with({
	name: "unicode-line-break",
	generator: FuzzSupport.scalar_sequence,
	test,
	show: FuzzSupport.show_scalars,
})
