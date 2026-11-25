import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Grapheme
import unicode.LineBreak
import unicode.TextPosition

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	code_points = input.code_points
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")

	profile : LineBreak.Profile
	profile = if (input.config.bitwise_and(1) == 0) UnicodeDefault else PreserveGraphemes
	boundaries = LineBreak.boundaries_with(source, profile)
	iterated_boundaries = FuzzSupport.pause_iter(LineBreak.iter_boundaries_with(source, profile), input.config.to_u64() % (parts.len() + 1))
	opportunities = LineBreak.opportunities_with(source, profile)
	iterated_opportunities = FuzzSupport.pause_iter(LineBreak.iter_opportunities_with(source, profile), input.config.to_u64() % (parts.len() + 1))
	whole_cursor = cursor_opportunities(profile, [source])
	scalar_cursor = cursor_opportunities(profile, [""].concat(parts).append(""))
	planned_cursor = cursor_opportunities(profile, FuzzSupport.chunks(input, parts))

	if iterated_boundaries != boundaries {
		crash "LineBreak.iter_boundaries disagreed with LineBreak.boundaries"
	}
	if iterated_opportunities != opportunities {
		crash "LineBreak.iter_opportunities disagreed with LineBreak.opportunities"
	}
	if whole_cursor != opportunities {
		crash "whole-chunk LineBreak.Cursor disagreed with LineBreak.opportunities"
	}
	if scalar_cursor != opportunities or planned_cursor != opportunities {
		crash "scalar-chunk LineBreak.Cursor disagreed with LineBreak.opportunities"
	}
	if boundaries_as_opportunities(boundaries) != opportunities {
		crash "opportunities were not the non-Prohibited subset of boundaries"
	}

	FuzzSupport.check_bounded_collection("LineBreak.boundaries_limited", source, input.config, boundaries, |limits| LineBreak.boundaries_limited(source, profile, limits))
	FuzzSupport.check_bounded_collection("LineBreak.opportunities_limited", source, input.config, opportunities, |limits| LineBreak.opportunities_limited(source, profile, limits))

	validate_boundaries(source, boundaries, code_points.len())
	if profile == PreserveGraphemes {
		validate_tailoring(source, boundaries)
	}

	Fuzz.keep
}

validate_tailoring : Str, List(LineBreak.BreakBoundary) -> {}
validate_tailoring = |source, tailored| {
	original = LineBreak.boundaries(source)
	cluster_ends = Grapheme.ranges(source).map(ByteRange.end)
	var $index = 0
	for exact in original {
		got = tailored.get($index) ?? crash "tailoring omitted a scalar boundary"
		if got.at != exact.at or got.authority != exact.authority {
			crash "tailoring changed boundary coordinates or authority"
		}
		inside_cluster = !cluster_ends.contains(TextPosition.byte_offset(got.at))
		expected = if exact.decision == Allowed and exact.authority == Tailorable and inside_cluster {
			Prohibited
		} else {
			exact.decision
		}
		if got.decision != expected {
			crash "line tailoring changed an ineligible boundary or split a grapheme"
		}
		$index = $index + 1
	}
	{}
}

cursor_opportunities : LineBreak.Profile, List(Str) -> List(LineBreak.BreakOpportunity)
cursor_opportunities = |profile, chunks| {
	var $cursor = LineBreak.Cursor.init_with(profile)
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
			match LineBreak.Cursor.push(done.cursor, "a", [], |items, item| items.append(item)) {
				Failed({ error: AlreadyFinished, consumed: 0, .. }) => {}
				_ => crash "LineBreak.Cursor accepted a push after finish"
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

LineBreakTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-line-break",
		examples: [Fuzz.saved({ name: "preserve-graphemes", revision: "1", bytes: [0x01, 0x0B, 0x27, 0x00, 0xFB, 0xEB, 0x21, 0x61, 0x00, 0x40, 0x0D, 0x00, 0x60, 0x0A, 0x00, 0x80, 0x62, 0x00, 0xA0] })],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
