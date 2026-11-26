import FuzzSupport
import fuzz.Fuzz
import unicode.Bidi
import unicode.BidiClass
import unicode.ByteRange
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextPosition
import unicode.TextRange

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	source = FuzzSupport.source(input.code_points)
	base : Bidi.BaseDirection
	base = match input.config % 3 {
		0 => LeftToRight
		1 => RightToLeft
		_ => Auto
	}
	paragraphs = Bidi.paragraph_ranges(source)
	FuzzSupport.check_bounded_collection("Bidi.paragraph_ranges_limited", source, input.config, paragraphs, |limits| Bidi.paragraph_ranges_limited(source, limits))
	paused = FuzzSupport.pause_iter(Bidi.iter_paragraph_ranges(source), input.config.to_u64() % (paragraphs.len() + 1))
	if paused != paragraphs {
		crash "Bidi paragraph iterator disagreed after pause and resume"
	}

	var $byte = 0
	var $scalar = 0
	for range in paragraphs {
		bytes = TextRange.byte_range(range)
		scalars = TextRange.scalar_range(range)
		if ByteRange.start(bytes) != $byte or ScalarRange.start(scalars) != $scalar or (source != "" and ScalarRange.len(scalars) == 0) {
			crash "bidi paragraph ranges did not partition the source"
		}
		check_paragraph(source, input, base, range)
		$byte = ByteRange.end(bytes)
		$scalar = ScalarRange.end(scalars)
	}
	if $byte != source.count_utf8_bytes() or $scalar != input.code_points.len() {
		crash "bidi paragraph ranges did not reach the end of the input"
	}
	if paragraphs.len() > 1 {
		match Bidi.analyze_paragraph(source, { base_direction: base, limits: Bidi.default_limits }) {
			Err(MultipleParagraphs) => {}
			_ => crash "bidi accepted multiple paragraphs as one paragraph"
		}
	}
	Fuzz.keep
}

check_paragraph : Str, FuzzSupport.Plan, Bidi.BaseDirection, TextRange -> {}
check_paragraph = |source, input, base, range| {
	bytes = TextRange.byte_range(range)
	scalars = TextRange.scalar_range(range)
	limits : Bidi.Limits
	limits = { max_scalars: ScalarRange.len(scalars), max_bytes: ByteRange.len(bytes) }
	analysis = Bidi.analyze_range(source, range, { base_direction: base, limits }) ?? crash "bidi rejected a P1 paragraph at its exact input limit"
	if Bidi.requested_base_direction(analysis) != base or Bidi.paragraph_range(analysis) != range {
		crash "bidi analysis changed the requested paragraph or base direction"
	}
	entries = Bidi.entries(analysis)
	levels = Bidi.levels(analysis)
	if entries.len() != limits.max_scalars or entries.len() != levels.len() {
		crash "bidi retained a tape with the wrong length"
	}
	var $at = ScalarRange.start(scalars)
	var $byte = ByteRange.start(bytes)
	for entry in entries {
		point = input.code_points.get($at) ?? crash "extra bidi tape entry"
		entry_range = TextRange.scalar_range(entry.range)
		entry_bytes = TextRange.byte_range(entry.range)
		chunk = ByteRange.slice(entry_bytes, source) ?? crash "bidi tape split a UTF-8 scalar"
		if Scalar.to_u32(entry.scalar) != point or ScalarRange.start(entry_range) != $at or ScalarRange.len(entry_range) != 1 or ByteRange.start(entry_bytes) != $byte or chunk != FuzzSupport.source([point]) {
			crash "bidi tape did not preserve original scalars and absolute coordinates"
		}
		if entry.original_class != BidiClass.of_scalar(entry.scalar) {
			crash "bidi tape did not preserve the original bidi class"
		}
		$at = $at + 1
		$byte = ByteRange.end(entry_bytes)
	}
	if $byte != ByteRange.end(bytes) {
		crash "bidi tape did not cover all paragraph bytes"
	}
	case_limit = if entries.is_empty() {
		Bool.True
	} else if input.config.bitwise_and(4) == 0 {
		match Bidi.analyze_range(source, range, { base_direction: base, limits: { ..limits, max_scalars: limits.max_scalars - 1 } }) {
			Err(ScalarLimitExceeded(detail)) => detail.limit + 1 == detail.required and detail.required == limits.max_scalars and Some(detail.range) == last_entry_range(entries)
			_ => Bool.False
		}
	} else {
		match Bidi.analyze_range(source, range, { base_direction: base, limits: { ..limits, max_bytes: limits.max_bytes - 1 } }) {
			Err(ByteLimitExceeded(detail)) => detail.limit + 1 == detail.required and detail.required == limits.max_bytes and detail.range == bytes
			_ => Bool.False
		}
	}
	if !entries.is_empty() and !case_limit {
		crash "bidi did not report the expected resource limit at the paragraph range"
	}
	collected = Bidi.collect_logical_runs(analysis)
	iterated = Bidi.logical_runs(analysis).fold([], |runs, run| runs.append(run))
	if collected != iterated {
		crash "bidi logical run views disagreed"
	}
	check_logical_runs(collected, entries, levels, ScalarRange.start(scalars))
	count = ScalarRange.len(scalars)
	first = input.config.shr_wrap(3).to_u64() % (count + 1)
	rest = count - first
	last = first + input.config.shr_wrap(2).to_u64() % (rest + 1)
	check_line(analysis, scalars)
	line = ScalarRange.from_bounds(ScalarRange.start(scalars) + first, ScalarRange.start(scalars) + last) ?? crash "invalid generated line range"
	check_line(analysis, line)
	invalid = ScalarRange.from_bounds(ScalarRange.end(scalars), ScalarRange.end(scalars) + 1) ?? crash "invalid generated out-of-bounds range"
	match Bidi.reorder_line(analysis, invalid) {
		Err(LineOutOfBounds(detail)) if detail.requested == invalid and detail.paragraph == scalars => {}
		_ => crash "bidi did not preserve the out-of-bounds line diagnostic"
	}
}

last_entry_range : List(Bidi.ScalarInfo) -> [Some(TextRange), None]
last_entry_range = |entries| match entries.get(entries.len() - 1) {
	Ok(entry) => Some(entry.range)
	Err(_) => None
}

check_logical_runs : List(Bidi.LevelRun), List(Bidi.ScalarInfo), List(Bidi.ResolvedLevel), U64 -> {}
check_logical_runs = |runs, entries, levels, base| {
	var $remaining = runs
	var $offset = base
	var $inside = None
	for level in levels {
		match level {
			RemovedByX9 => {}
			Level(value) => {
				current = $inside
				continues = match current {
					Some(run) => run.level == value
					None => Bool.False
				}
				if !continues {
					match current {
						Some(run) => if ScalarRange.end(TextRange.scalar_range(run.range)) > $offset {
							crash "bidi logical runs overlapped"
						}
						None => {}
					}
					run = $remaining.get(0) ?? crash "bidi omitted a logical run"
					entry = entries.get($offset - base) ?? crash "missing bidi entry for logical run"
					if run.level != value or run.direction != Bidi.direction(value) or TextRange.start(run.range) != TextRange.start(entry.range) {
						crash "bidi run did not start at the first scalar of its level"
					}
					$inside = Some(run)
					$remaining = $remaining.drop_first(1)
				}
				match $inside {
					Some(run) => if ScalarRange.end(TextRange.scalar_range(run.range)) <= $offset {
						crash "bidi run ended before its last scalar"
					}
					None => crash "bidi logical run missing"
				}
			}
		}
		$offset = $offset + 1
	}
	if !$remaining.is_empty() {
		crash "bidi emitted extra logical runs"
	}
	{}
}

check_line : Bidi.Analysis, ScalarRange -> {}
check_line = |analysis, range| {
	order = Bidi.reorder_line(analysis, range) ?? crash "bidi rejected an in-bounds line"
	levels = Bidi.line_levels(order)
	inverse = Bidi.logical_to_visual(order)
	start = ScalarRange.start(range)
	if levels.len() != ScalarRange.len(range) or inverse.len() != levels.len() or Bidi.line_mirroring(order).len() != levels.len() {
		crash "bidi line metadata did not cover the selected line"
	}
	visual = Bidi.visual_to_logical(order)
	expected = reference_l2(levels, start)
	if visual != expected {
		crash "bidi line did not match L2 segment reversal over the line levels"
	}
	var $index = 0
	for level in levels {
		mapped = inverse.get($index) ?? crash "bidi inverse map omitted an index"
		match (level, mapped) {
			(RemovedByX9, None) => {}
			(Level(_), Some(visual_index)) => if visual.get(visual_index) != Ok(start + $index) {
				crash "bidi line mappings were not inverses"
			}
			_ => crash "bidi line mappings did not account for X9 removal"
		}
		$index = $index + 1
	}
	again = Bidi.reorder_line(analysis, range) ?? crash "bidi repeated line reordering failed"
	if Bidi.visual_to_logical(again) != visual or Bidi.line_levels(again) != levels or Bidi.visual_runs(again) != Bidi.visual_runs(order) {
		crash "bidi changed a line after repeated access"
	}
	{}
}

## UAX #9 L2: starting from the highest level and working down to the
## lowest odd level, reverse each maximal interval at or above the level.
reference_l2 : List(Bidi.ResolvedLevel), U64 -> List(U64)
reference_l2 = |levels, start| {
	var $items = []
	var $index = start
	var $max = 0.U8
	var $min_odd = U8.highest
	for level in levels {
		match level {
			RemovedByX9 => {}
			Level(value) => {
				$items = $items.append({ at: $index, level: value })
				if value > $max {
					$max = value
				}
				if value % 2 == 1 and value < $min_odd {
					$min_odd = value
				}
			}
		}
		$index = $index + 1
	}
	var $level = $max
	while $level >= $min_odd {
		var $reversed = []
		var $run = []
		for item in $items {
			if item.level >= $level {
				$run = $run.append(item)
			} else {
				$reversed = $reversed.concat(reverse_group($run)).append(item)
				$run = []
			}
		}
		$items = $reversed.concat(reverse_group($run))
		$level = $level - 1
	}
	$items.map(|item| item.at)
}

reverse_group : List({ at : U64, level : U8 }) -> List({ at : U64, level : U8 })
reverse_group = |items| items.fold([], |reversed, item| [item].concat(reversed))

BidiTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-bidi",
		examples: [
			Fuzz.saved({ name: "paragraphs-and-bidi-controls", revision: "1", bytes: [2, 0x41, 0, 0, 0x0D, 0, 0, 0x0A, 0, 0, 0x2B, 0x20, 0, 0xD0, 0x05, 0, 0x2C, 0x20, 0] }),
		],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
