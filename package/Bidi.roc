import BidiClass
import BidiProperties
import ByteRange
import Scalar
import ScalarRange
import TextPosition
import TextRange

## Retained paragraph analysis for Unicode 17 UAX #9. The retained tape keeps
## original scalar identities and coordinates; it deliberately never retains
## the input `Str`. Line reordering is a separate operation because L1 is
## defined at actual line boundaries.
Bidi :: [].{
	BaseDirection : [Auto, LeftToRight, RightToLeft]
	Direction : [LeftToRight, RightToLeft]
	LimitStage : [Ingestion]
	ResolvedLevel : [Level(U8), RemovedByX9]
	Limits : { max_scalars : U64, max_bytes : U64 }

	## Limit errors identify the operation before the retained tape is committed.
	## Their paragraph range is in the caller's original source coordinates.
	Error : [ScalarLimitExceeded({ limit : U64, required : U64, stage : LimitStage, range : TextRange }), ByteLimitExceeded({ limit : U64, required : U64, stage : LimitStage, range : ByteRange }), MultipleParagraphs, InvalidParagraphRange(TextRange), LineOutOfBounds({ requested : ScalarRange, paragraph : ScalarRange })]
	ScalarInfo : { range : TextRange, scalar : Scalar, original_class : BidiClass.Value, working_class : BidiClass.Value, level : [Some(U8), None], matched_bracket : [Some(U64), None], non_rendering : Bool, needs_mirrored_glyph : Bool, mirroring_glyph : [Some(Scalar), None] }
	LevelRun : { range : TextRange, level : U8, direction : Direction }
	VisualRun : { logical_range : ScalarRange, level : U8, direction : Direction }
	Analysis := { requested_base : BaseDirection, paragraph_range : TextRange, paragraph_level : U8, entries : List(ScalarInfo), byte_len : U64, scalar_len : U64 }
	MirrorInfo : { needs_glyph : Bool, glyph : [Some(Scalar), None] }
	LineOrder := { line_range : ScalarRange, levels : List([Some(U8), None]), visual_to_logical : List(U64), logical_to_visual : List([Some(U64), None]), visual_runs : List(VisualRun), mirroring : List(MirrorInfo) }

	## Private intermediate links for the X10/N0 implementation. No public
	## operation exposes this value.
	RunSequences := { links : List([Some(U64), None]), starts : List(U64), sos : List(BidiClass.Value), eos : List(BidiClass.Value) }

	default_limits : Limits
	default_limits = { max_scalars: U64.highest, max_bytes: U64.highest }

	## Split complete text according to P1. A paragraph separator belongs to
	## the preceding returned range. CR LF is one separator (and therefore one
	## preceding-paragraph suffix); a lone CR, lone LF, and U+001C..U+001E each
	## terminate their preceding paragraph. No artificial empty paragraph is
	## produced after a final separator.
	paragraph_ranges : Str -> List(TextRange)
	paragraph_ranges = |source| p1_ranges(source)

	analyze_paragraph : Str, BaseDirection, Limits -> Try(Analysis, Error)
	analyze_paragraph = |source, base_direction, limits| analyze_source(source, base_direction, limits, TextPosition.from_offsets(0, 0))

	## Analyze one P1 paragraph selected from a complete source. Validation scans
	## through the selected P1 boundary, then replays only the selected seamless
	## slice for retained analysis; this explicit replay keeps global coordinates
	## without retaining the source. Line ranges and visual mappings remain
	## absolute to `source`.
	analyze_range : Str, TextRange, BaseDirection, Limits -> Try(Analysis, Error)
	analyze_range = |source, selected, base_direction, limits| {
		if !is_p1_range(source, selected) {
			Err(InvalidParagraphRange(selected))
		} else {
			bytes = TextRange.byte_range(selected)
			match ByteRange.slice(bytes, source) {
				Err(_) => Err(InvalidParagraphRange(selected))
				Ok(paragraph) => analyze_source(paragraph, base_direction, limits, TextRange.start(selected))
			}
		}
	}

	paragraph_level : Analysis -> U8
	paragraph_level = |analysis| analysis.paragraph_level

	requested_base_direction : Analysis -> BaseDirection
	requested_base_direction = |analysis| analysis.requested_base

	paragraph_range : Analysis -> TextRange
	paragraph_range = |analysis| analysis.paragraph_range

	entries : Analysis -> List(ScalarInfo)
	entries = |analysis| analysis.entries

	## Matched paired-bracket partner indices after BD16/N0, or `None` for an
	## unmatched/non-bracket scalar. Indices always refer to original logical
	## scalars, never an X9-filtered working buffer.
	matched_brackets : Analysis -> List([Some(U64), None])
	matched_brackets = |analysis| analysis.entries.map(|entry| entry.matched_bracket)

	levels : Analysis -> List(ResolvedLevel)
	levels = |analysis| analysis.entries.map(
		|entry| match entry.level {
			Some(value) => Level(value)
			None => RemovedByX9
		},
	)

	direction : U8 -> Direction
	direction = |level| if level % 2 == 0 LeftToRight else RightToLeft

	## Lazily walk maximal same-level spans in the X9-filtered logical sequence.
	## Removed controls bridge equal-level runs but are not themselves yielded.
	logical_runs : Analysis -> Iter(LevelRun)
	logical_runs = |analysis| Iter.custom({ analysis, at: 0.U64 }, Unknown, next_logical_run)

	## Explicit materializing convenience for clients that need every retained
	## run at once. `logical_runs` is the primary, early-stoppable traversal.
	collect_logical_runs : Analysis -> List(LevelRun)
	collect_logical_runs = |analysis| {
		var output = []
		for run in Bidi.logical_runs(analysis) {
			output = output.append(run)
		}
		output
	}

	## L1/L2 for a scalar-aligned, paragraph-local logical line. The display
	## permutation omits only X9 controls. LRI/RLI/FSI/PDI remain in it, marked
	## `non_rendering` in the analysis for renderers that do not draw controls.
	reorder_line : Analysis, ScalarRange -> Try(LineOrder, Error)
	reorder_line = |analysis, line_range| {
		start = ScalarRange.start(line_range)
		end = ScalarRange.end(line_range)
		paragraph_scalars = TextRange.scalar_range(analysis.paragraph_range)
		paragraph_start = ScalarRange.start(paragraph_scalars)
		paragraph_end = ScalarRange.end(paragraph_scalars)
		if start < paragraph_start or end > paragraph_end {
			Err(LineOutOfBounds({ requested: line_range, paragraph: paragraph_scalars }))
		} else {
			local_start = start - paragraph_start
			local_end = end - paragraph_start
			line = copy_slice(analysis.entries, local_start, local_end)
			adjusted = l1(line, analysis.paragraph_level)
			visible = eligible(line, adjusted, start)
			visual = l2(visible, adjusted, start, analysis.paragraph_level)
			Ok({
				line_range,
				levels: adjusted,
				visual_to_logical: visual,
				logical_to_visual: inverse(line.len(), visual, start),
				visual_runs: make_visual_runs(visual, adjusted, start),
				mirroring: calculate_line_mirroring(line, adjusted),
			})
		}
	}

	line_levels : LineOrder -> List(ResolvedLevel)
	line_levels = |line| line.levels.map(
		|value| match value {
			Some(level) => Level(level)
			None => RemovedByX9
		},
	)

	visual_to_logical : LineOrder -> List(U64)
	visual_to_logical = |line| line.visual_to_logical

	logical_to_visual : LineOrder -> List([Some(U64), None])
	logical_to_visual = |line| line.logical_to_visual

	visual_runs : LineOrder -> List(VisualRun)
	visual_runs = |line| line.visual_runs

	line_mirroring : LineOrder -> List(MirrorInfo)
	line_mirroring = |line| line.mirroring
}

next_logical_run = |state| {
	analysis = state.analysis
	var at = state.at
	while at < analysis.scalar_len {
		entry = analysis.entries.get(at) ?? return Err(NoMore)
		match entry.level {
			None => {
				at = match at.plus_try(1) {
					Ok(next) => next
					Err(Overflow) => return Err(NoMore)
				}
			}
			Some(level) => {
				first = at
				var last = at
				var cursor = match at.plus_try(1) {
					Ok(next) => next
					Err(Overflow) => return Err(NoMore)
				}
				while cursor < analysis.scalar_len {
					candidate = analysis.entries.get(cursor) ?? break
					match candidate.level {
						None => {
							next_cursor = match cursor.plus_try(1) {
								Ok(next) => next
								Err(Overflow) => break
							}
							cursor = next_cursor
						}
						Some(candidate_level) => if candidate_level == level {
							last = cursor
							next_cursor = match cursor.plus_try(1) {
								Ok(next) => next
								Err(Overflow) => break
							}
							cursor = next_cursor
						} else {
							break
						}
					}
				}
				first_entry = analysis.entries.get(first) ?? return Err(NoMore)
				last_entry = analysis.entries.get(last) ?? return Err(NoMore)
				first_bytes = TextRange.byte_range(first_entry.range)
				last_bytes = TextRange.byte_range(last_entry.range)
				bytes = ByteRange.from_bounds(ByteRange.start(first_bytes), ByteRange.end(last_bytes)) ?? return Err(NoMore)
				first_scalars = TextRange.scalar_range(first_entry.range)
				last_scalars = TextRange.scalar_range(last_entry.range)
				scalars = ScalarRange.from_bounds(ScalarRange.start(first_scalars), ScalarRange.end(last_scalars)) ?? return Err(NoMore)
				run = { range: TextRange.from_ranges(bytes, scalars), level, direction: Bidi.direction(level) }
				return Ok((run, { analysis, at: cursor }))
			}
		}
	}
	Err(NoMore)
}

p1_ranges = |source| {
	var ranges = []
	var byte_start = 0.U64
	var scalar_start = 0.U64
	var final_byte = 0.U64
	var final_scalar = 0.U64
	var pending_cr = None
	for located in Scalar.iter(source) {
		final_byte = ByteRange.end(located.byte_range)
		final_scalar = match located.scalar_index.plus_try(1) {
			Ok(next) => next
			Err(Overflow) => U64.highest
		}
		value = Scalar.to_u32(located.scalar)
		match pending_cr {
			Some(cr) => {
				if value == 0x000A {
					ranges = ranges.append(range_from_offsets(byte_start, final_byte, scalar_start, final_scalar))
					byte_start = final_byte
					scalar_start = final_scalar
					pending_cr = None
				} else {
					ranges = ranges.append(range_from_offsets(byte_start, cr.byte_end, scalar_start, cr.scalar_end))
					byte_start = cr.byte_end
					scalar_start = cr.scalar_end
					pending_cr = None
					if value == 0x000D {
						pending_cr = Some({ byte_end: final_byte, scalar_end: final_scalar })
					} else if is_p1_separator(value) {
						ranges = ranges.append(range_from_offsets(byte_start, final_byte, scalar_start, final_scalar))
						byte_start = final_byte
						scalar_start = final_scalar
					}
				}
			}
			None => {
				if value == 0x000D {
					pending_cr = Some({ byte_end: final_byte, scalar_end: final_scalar })
				} else if is_p1_separator(value) {
					ranges = ranges.append(range_from_offsets(byte_start, final_byte, scalar_start, final_scalar))
					byte_start = final_byte
					scalar_start = final_scalar
				}
			}
		}
	}
	match pending_cr {
		Some(cr) => {
			ranges = ranges.append(range_from_offsets(byte_start, cr.byte_end, scalar_start, cr.scalar_end))
			byte_start = cr.byte_end
			scalar_start = cr.scalar_end
		}
		None => {}
	}
	if final_scalar == 0 {
		[range_from_offsets(0, 0, 0, 0)]
	} else if scalar_start < final_scalar {
		ranges.append(range_from_offsets(byte_start, final_byte, scalar_start, final_scalar))
	} else {
		ranges
	}
}

is_p1_separator = |scalar| scalar == 0x000A or scalar == 0x0085 or (scalar >= 0x001C and scalar <= 0x001E) or scalar == 0x2029

range_from_offsets = |byte_start, byte_end, scalar_start, scalar_end| {
	bytes = ByteRange.from_bounds(byte_start, byte_end) ?? ...
	scalars = ScalarRange.from_bounds(scalar_start, scalar_end) ?? ...
	TextRange.from_ranges(bytes, scalars)
}

is_p1_range = |source, selected| {

	## This deliberately does not call `p1_ranges`: range analysis must not
	## allocate one result per preceding paragraph just to validate `selected`.
	## A pending CR delays commitment until the next scalar determines whether
	## it is the CR half of CR LF.
	var byte_start = 0.U64
	var scalar_start = 0.U64
	var final_byte = 0.U64
	var final_scalar = 0.U64
	var pending_cr = None
	for located in Scalar.iter(source) {
		final_byte = ByteRange.end(located.byte_range)
		final_scalar = match located.scalar_index.plus_try(1) {
			Ok(next) => next
			Err(Overflow) => return Bool.False
		}
		value = Scalar.to_u32(located.scalar)
		match pending_cr {
			Some(cr) => {
				if value == 0x000A {
					status = p1_candidate(selected, byte_start, final_byte, scalar_start, final_scalar)
					if status.matches {
						return Bool.True
					}
					if status.passed {
						return Bool.False
					}
					byte_start = final_byte
					scalar_start = final_scalar
					pending_cr = None
				} else {
					status = p1_candidate(selected, byte_start, cr.byte_end, scalar_start, cr.scalar_end)
					if status.matches {
						return Bool.True
					}
					if status.passed {
						return Bool.False
					}
					byte_start = cr.byte_end
					scalar_start = cr.scalar_end
					pending_cr = None
					if value == 0x000D {
						pending_cr = Some({ byte_end: final_byte, scalar_end: final_scalar })
					} else if is_p1_separator(value) {
						separator_status = p1_candidate(selected, byte_start, final_byte, scalar_start, final_scalar)
						if separator_status.matches {
							return Bool.True
						}
						if separator_status.passed {
							return Bool.False
						}
						byte_start = final_byte
						scalar_start = final_scalar
					}
				}
			}
			None => {
				if value == 0x000D {
					pending_cr = Some({ byte_end: final_byte, scalar_end: final_scalar })
				} else if is_p1_separator(value) {
					status = p1_candidate(selected, byte_start, final_byte, scalar_start, final_scalar)
					if status.matches {
						return Bool.True
					}
					if status.passed {
						return Bool.False
					}
					byte_start = final_byte
					scalar_start = final_scalar
				}
			}
		}
	}
	match pending_cr {
		Some(cr) => p1_candidate(selected, byte_start, cr.byte_end, scalar_start, cr.scalar_end).matches
		None => if final_scalar == 0 {
			TextRange.is_eq(selected, range_from_offsets(0, 0, 0, 0))
		} else if scalar_start < final_scalar {
			p1_candidate(selected, byte_start, final_byte, scalar_start, final_scalar).matches
		} else {
			Bool.False
		}
	}
}

## Exact candidate matching plus monotonic early rejection. Once a selected
## byte start lies inside a completed P1 range, no later P1 range can match.
p1_candidate = |selected, byte_start, byte_end, scalar_start, scalar_end| {
	candidate = range_from_offsets(byte_start, byte_end, scalar_start, scalar_end)
	if TextRange.is_eq(candidate, selected) {
		{ matches: Bool.True, passed: Bool.False }
	} else {
		selected_bytes = TextRange.byte_range(selected)
		{ matches: Bool.False, passed: ByteRange.start(selected_bytes) < byte_end }
	}
}

analyze_source = |source, base_direction, limits, origin| {
	byte_len = source.count_utf8_bytes()
	byte_end = match TextPosition.byte_offset(origin).plus_try(byte_len) {
		Ok(value) => value
		Err(Overflow) => U64.highest
	}
	byte_range = ByteRange.from_bounds(TextPosition.byte_offset(origin), byte_end) ?? ...
	if byte_len > limits.max_bytes {
		Err(ByteLimitExceeded({ limit: limits.max_bytes, required: byte_len, stage: Ingestion, range: byte_range }))
	} else {
		collected = collect_entries(source, limits.max_scalars, origin)
		match collected {
			Multiple => Err(MultipleParagraphs)
			Limit(limit) => Err(ScalarLimitExceeded({ limit: limits.max_scalars, required: limit.required, stage: Ingestion, range: limit.range }))
			Entries(entries) => {
				isolates = prepare_isolates(entries)
				base = base_level(entries, base_direction, isolates.partners)
				resolved = resolve_explicit(entries, base, isolates)
				scalar_origin = TextPosition.scalar_offset(origin)
				global_resolved = if scalar_origin == 0 {
					resolved
				} else {
					rebase_matched_brackets(resolved, scalar_origin)
				}
				scalar_end = match scalar_origin.plus_try(global_resolved.len()) {
					Ok(value) => value
					Err(Overflow) => U64.highest
				}
				paragraph_range = range_from_offsets(TextPosition.byte_offset(origin), byte_end, scalar_origin, scalar_end)
				Ok({ requested_base: base_direction, paragraph_range, paragraph_level: base, entries: global_resolved, byte_len, scalar_len: global_resolved.len() })
			}
		}
	}
}

## Pairing is performed over the paragraph-local tape. Retained metadata uses
## absolute scalar indices so it agrees with every `TextRange` from
## `analyze_range`.
rebase_matched_brackets = |entries, scalar_origin| entries.map(
	|entry| {
		match entry.matched_bracket {
			None => entry
			Some(local) => match local.plus_try(scalar_origin) {
				Ok(global) => { ..entry, matched_bracket: Some(global) }
				Err(Overflow) => { ..entry, matched_bracket: None }
			}
		}
	},
)

collect_entries = |source, limit, origin| {
	var entries = []
	var count = 0.U64
	var saw_separator = Bool.False
	var pending_cr = Bool.False
	origin_range = range_from_offsets(TextPosition.byte_offset(origin), TextPosition.byte_offset(origin), TextPosition.scalar_offset(origin), TextPosition.scalar_offset(origin))
	for located in Scalar.iter(source) {
		p1_value = Scalar.to_u32(located.scalar)
		if saw_separator {
			if pending_cr and p1_value == 0x000A {
				# P1 treats CR LF as one separator, not an empty paragraph.
				pending_cr = Bool.False
			} else {
				return Multiple
			}
		} else if p1_value == 0x000D {
			saw_separator = Bool.True
			pending_cr = Bool.True
		} else if is_p1_separator(p1_value) {
			saw_separator = Bool.True
		}
		byte_start = match TextPosition.byte_offset(origin).plus_try(ByteRange.start(located.byte_range)) {
			Ok(value) => value
			Err(Overflow) => return Limit({ required: U64.highest, range: origin_range })
		}
		byte_end = match TextPosition.byte_offset(origin).plus_try(ByteRange.end(located.byte_range)) {
			Ok(value) => value
			Err(Overflow) => return Limit({ required: U64.highest, range: origin_range })
		}
		scalar_start = match TextPosition.scalar_offset(origin).plus_try(located.scalar_index) {
			Ok(value) => value
			Err(Overflow) => return Limit({ required: U64.highest, range: origin_range })
		}
		scalar_end = match scalar_start.plus_try(1) {
			Ok(value) => value
			Err(Overflow) => return Limit({ required: U64.highest, range: origin_range })
		}
		bytes = ByteRange.from_bounds(byte_start, byte_end) ?? return Limit({ required: U64.highest, range: origin_range })
		scalars = ScalarRange.from_bounds(scalar_start, scalar_end) ?? return Limit({ required: U64.highest, range: origin_range })
		count = match count.plus_try(1) {
			Ok(next) => next
			Err(Overflow) => return Limit({ required: U64.highest, range: TextRange.from_ranges(bytes, scalars) })
		}
		if count > limit {
			return Limit({ required: count, range: TextRange.from_ranges(bytes, scalars) })
		}
		class = BidiClass.of_scalar(located.scalar)
		entries = entries.append({
			range: TextRange.from_ranges(bytes, scalars),
			scalar: located.scalar,
			original_class: class,
			working_class: class,
			level: Some(0),
			matched_bracket: None,
			non_rendering: non_rendering(class),
			needs_mirrored_glyph: Bool.False,
			mirroring_glyph: None,
		})
	}
	Entries(entries)
}

## Pair isolates once, using links stored alongside the retained tape instead
## of an input-proportional temporary stack. Besides making BD9 available to
## X10, the direct-content strong fact lets X5c resolve every FSI in one pass.
prepare_isolates = |entries| {
	var partners = []
	var parents = []
	var first_strongs = []
	var top = None
	var at = 0.U64
	while at < entries.len() {
		entry = entries.get(at) ?? break
		class = entry.original_class
		partners = partners.append(None)
		parents = parents.append(None)
		first_strongs = first_strongs.append(None)
		if class == LRI or class == RLI or class == FSI {
			parents = parents.set(at, top) ?? ...
			top = Some(at)
		} else if class == PDI {
			match top {
				None => {}
				Some(open) => {
					partners = partners.set(open, Some(at)) ?? ...
					partners = partners.set(at, Some(open)) ?? ...
					top = parents.get(open) ?? None
				}
			}
		} else if class == L or class == R or class == AL {
			match top {
				None => {}
				Some(open) => if first_strongs.get(open) == Ok(None) {
					first_strongs = first_strongs.set(open, Some(class)) ?? ...
				}
			}
		}
		at = at + 1
	}
	{
		partners,
		fsi_directions: first_strongs.map(
			|strong| match strong {
				Some(R) => RLI
				Some(AL) => RLI
				_ => LRI
			},
		),
	}
}

base_level = |entries, policy, partners| {
	match policy {
		LeftToRight => 0
		RightToLeft => 1
		Auto => {
			# P2/P3: skip the complete matching isolate, even an overflow isolate.
			var at = 0.U64
			while at < entries.len() {
				entry = entries.get(at) ?? break
				class = entry.original_class
				if class == LRI or class == RLI or class == FSI {
					match partners.get(at) ?? None {
						Some(close) => {
							at = close + 1
						}
						None => {
							break
						}
					}
				} else if class == L {
					return 0
				}
					else if class == R or class == AL {
						return 1
					}
						else {
							at = at + 1
						}
			}
			0
		}
	}
}

## X1-X9. The fixed semantic depth is 125; overflow formatting controls are
## ignored according to the UAX counters rather than becoming a caller limit.
resolve_explicit = |entries, base, isolates| {
	var stack = [{ level: base, override: None, isolate: Bool.False }]
	var overflow_isolates = 0.U64
	var overflow_embeddings = 0.U64
	var valid_isolates = 0.U64
	var result = []
	for entry in entries {
		position = result.len()
		current = stack.last() ?? { level: base, override: None, isolate: Bool.False }
		class = entry.original_class
		if class == RLE or class == LRE or class == RLO or class == LRO {
			if overflow_isolates > 0 {
				# X2-X5: formatting inside an overflow isolate is ignored and
				# does not participate in the embedding-overflow counter.
			} else if overflow_embeddings > 0 {
				overflow_embeddings = overflow_embeddings + 1
			} else {
				candidate = next_embedding_level(current.level, class)
				if candidate <= 125 {
					override = if class == RLO {
						Some(R)
					} else if class == LRO {
						Some(L)
					} else {
						None
					}
					stack = stack.append({ level: candidate, override, isolate: Bool.False })
				} else {
					overflow_embeddings = overflow_embeddings + 1
				}
			}
			result = result.append({ ..entry, level: None })
		} else if class == PDF {
			if overflow_isolates > 0 {}
			else if overflow_embeddings > 0 {
				overflow_embeddings = overflow_embeddings - 1
			}
				else if stack.len() > 1 and !current.isolate {
					stack = drop_last(stack)
				}
			result = result.append({ ..entry, level: None })
		} else if class == BN {
			result = result.append({ ..entry, level: None })
		} else if class == LRI or class == RLI or class == FSI {
			if overflow_isolates > 0 or overflow_embeddings > 0 {
				overflow_isolates = overflow_isolates + 1
			} else {
				isolate_type = if class == FSI {
					isolates.fsi_directions.get(position) ?? LRI
				} else {
					class
				}
				candidate = next_embedding_level(current.level, isolate_type)
				if candidate <= 125 {
					stack = stack.append({ level: candidate, override: None, isolate: Bool.True })
					valid_isolates = valid_isolates + 1
				} else {
					overflow_isolates = overflow_isolates + 1
				}
			}
			result = result.append(resolved_entry(entry, current.level, class, current.override))
		} else if class == PDI {
			if overflow_isolates > 0 {
				overflow_isolates = overflow_isolates - 1
			} else if valid_isolates > 0 {
				overflow_embeddings = 0
				stack = drop_through_isolate(stack)
				valid_isolates = valid_isolates - 1
			}
			after = stack.last() ?? { level: base, override: None, isolate: Bool.False }
			result = result.append(resolved_entry(entry, after.level, class, after.override))
		} else if class == B {
			# X8: B terminates all scopes and is always at paragraph level.
			result = result.append(resolved_entry(entry, base, class, None))
		} else {
			result = result.append(resolved_entry(entry, current.level, class, current.override))
		}
	}
	weak_and_implicit(result, base, isolates.partners)
}

resolved_entry = |entry, level, class, override| {
	working = match override {
		Some(value) => value
		None => class
	}
	{ ..entry, working_class: working, level: Some(level) }
}

next_embedding_level = |level, class| {
	if class == RLE or class == RLO or class == RLI {
		if level % 2 == 0 {
			level + 1
		} else {
			level + 2
		}
	} else {
		if level % 2 == 0 {
			level + 2
		} else {
			level + 1
		}
	}
}

drop_last = |items| {
	var output = []
	var at = 0.U64
	while at + 1 < items.len() {
		output = output.append(items.get(at) ?? break)
		at = at + 1
	}
	output
}

drop_through_isolate = |items| {
	var output = items
	while output.len() > 1 {
		top = output.last() ?? break
		output = drop_last(output)
		if top.isolate {
			return output
		}
	}
	output
}

## X10 produces links over the X9-filtered tape. An isolate's matching PDI is
## linked directly after its initiator, so W/N see an isolating run sequence,
## not the text nested inside the isolate.
weak_and_implicit = |entries, paragraph_level, partners| {
	irs = isolating_run_sequences(entries, paragraph_level, partners)
	first = w1(entries, irs)
	second = w2(first, irs)
	third = w3(second)
	fourth = w4(third, irs)
	fifth = w5(fourth, irs)
	sixth = w6(fifth)
	seventh = w7(sixth, irs)
	bracketed = n0(seventh, irs)
	neutrals = n1_n2(bracketed, irs)
	implicit(neutrals)
}

isolating_run_sequences = |entries, paragraph_level, partners| {
	var run_at = []
	var previous_visible = []
	var previous = None
	var at = 0.U64
	while at < entries.len() {
		entry = entries.get(at) ?? break
		previous_visible = previous_visible.append(previous)
		run_at = run_at.append(None)
		match entry.level {
			Some(_) => {
				previous = Some(at)
			}
			None => {}
		}
		at = at + 1
	}

	## Build this backward, then reverse by indexed appends. `List.prepend` is
	## linear for Roc lists, so prepending once per scalar made this O(n²).
	var backwards_next_visible = []
	var following = None
	var backwards = entries.len()
	while backwards > 0 {
		backwards = backwards - 1
		entry = entries.get(backwards) ?? break
		backwards_next_visible = backwards_next_visible.append(following)
		match entry.level {
			Some(_) => {
				following = Some(backwards)
			}
			None => {}
		}
	}
	var next_visible = []
	var reverse_at = backwards_next_visible.len()
	while reverse_at > 0 {
		reverse_at = reverse_at - 1
		next_visible = next_visible.append(backwards_next_visible.get(reverse_at) ?? None)
	}
	var runs = []
	at = 0
	while at < entries.len() {
		entry = entries.get(at) ?? break
		match entry.level {
			None => {
				at = at + 1
			}
			Some(level) => {
				first = at
				var last = at
				var cursor = at + 1
				while cursor < entries.len() {
					candidate = entries.get(cursor) ?? break
					match candidate.level {
						None => {
							cursor = cursor + 1
						}
						Some(candidate_level) => if candidate_level == level {
							last = cursor
							cursor = cursor + 1
						} else {
							break
						}
					}
				}
				run_id = runs.len()
				runs = runs.append({ first, last, level })
				var mark = first
				while mark <= last {
					candidate = entries.get(mark) ?? break
					match candidate.level {
						Some(_) => {
							run_at = run_at.set(mark, Some(run_id)) ?? ...
						}
						None => {}
					}
					mark = mark + 1
				}
				at = cursor
			}
		}
	}
	var links = []
	at = 0
	while at < entries.len() {
		links = links.append(None)
		at = at + 1
	}
	var starts = []
	var sos = []
	var eos = []
	for run in runs {
		first = entries.get(run.first) ?? break
		is_continuation = first.original_class == PDI and partners.get(run.first) != Ok(None)
		if !is_continuation {
			var current_run = run
			var final = run.last
			while Bool.True {
				var index = current_run.first
				while index < current_run.last {
					next = next_visible.get(index) ?? None
					match next {
						Some(value) => {
							links = links.set(index, Some(value)) ?? ...
						}
						None => {}
					}
					index = index + 1
				}
				last_entry = entries.get(current_run.last) ?? break
				match partners.get(current_run.last) ?? None {
					Some(close) if last_entry.original_class == LRI or last_entry.original_class == RLI or last_entry.original_class == FSI => {
						next_run_id = run_at.get(close) ?? None
						match next_run_id {
							Some(id) => {
								next_run = runs.get(id) ?? break
								links = links.set(current_run.last, Some(next_run.first)) ?? ...
								current_run = next_run
								final = next_run.last
							}
							None => {
								break
							}
						}
					}
					_ => {
						break
					}
				}
			}
			starts = starts.append(run.first)
			before = previous_visible.get(run.first) ?? None
			sos_level = match before {
				Some(index) => match (entries.get(index) ?? first).level {
					Some(value) => value
					None => paragraph_level
				}
				None => paragraph_level
			}
			last = entries.get(final) ?? first
			after = if last.original_class == LRI or last.original_class == RLI or last.original_class == FSI {
				None
			} else {
				next_visible.get(final) ?? None
			}
			eos_level = match after {
				Some(index) => match (entries.get(index) ?? last).level {
					Some(value) => value
					None => paragraph_level
				}
				None => paragraph_level
			}
			sos = sos.append(direction_class(max_level(run.level, sos_level)))
			eos = eos.append(direction_class(max_level(run.level, eos_level)))
		}
	}
	{ links, starts, sos, eos }
}

max_level = |left, right| if left > right {
	left
} else {
	right
}

direction_class = |level| if level % 2 == 0 {
	L
} else {
	R
}

next_in = |links, index| links.get(index) ?? None

class_at = |entries, index, fallback| match index {
	Some(value) => match entries.get(value) {
		Ok(entry) => entry.working_class
		Err(_) => fallback
	}
	None => fallback
}

w1 = |entries, irs| {
	var current = entries
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var previous = irs.sos.get(sequence) ?? L
		var at = irs.starts.get(sequence) ?? break
		while Bool.True {
			entry = current.get(at) ?? break
			class = if entry.working_class == NSM {
				if isolate_control(previous) {
					ON
				} else {
					previous
				}
			} else {
				entry.working_class
			}
			current = current.set(at, { ..entry, working_class: class }) ?? ...
			previous = class
			match next_in(irs.links, at) {
				Some(next) => {
					at = next
				}
				None => {
					break
				}
			}
		}
		sequence = sequence + 1
	}
	current
}

w2 = |entries, irs| {
	var current = entries
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var strong = irs.sos.get(sequence) ?? L
		var at = irs.starts.get(sequence) ?? break
		while Bool.True {
			entry = current.get(at) ?? break
			class = entry.working_class
			resolved = if class == EN and strong == AL {
				AN
			} else {
				class
			}
			current = current.set(at, { ..entry, working_class: resolved }) ?? ...
			if class == L or class == R or class == AL {
				strong = class
			}
			match next_in(irs.links, at) {
				Some(next) => {
					at = next
				}
				None => {
					break
				}
			}
		}
		sequence = sequence + 1
	}
	current
}

w3 = |entries| entries.map(
	|entry| {
		..entry,
		working_class: if entry.working_class == AL {
			R
		} else {
			entry.working_class
		},
	},
)

w4 = |entries, irs| {
	var current = entries
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var at = irs.starts.get(sequence) ?? break
		var before = irs.sos.get(sequence) ?? L
		eos = irs.eos.get(sequence) ?? L
		while Bool.True {
			entry = current.get(at) ?? break
			after = class_at(current, next_in(irs.links, at), eos)
			class = entry.working_class
			resolved = if class == ES and before == EN and after == EN {
				EN
			} else if class == CS and before == EN and after == EN {
				EN
			} else if class == CS and before == AN and after == AN {
				AN
			} else {
				class
			}
			current = current.set(at, { ..entry, working_class: resolved }) ?? ...
			before = resolved
			match next_in(irs.links, at) {
				Some(next) => {
					at = next
				}
				None => {
					break
				}
			}
		}
		sequence = sequence + 1
	}
	current
}

w5 = |entries, irs| {
	var current = entries
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var previous = irs.sos.get(sequence) ?? L
		var at = irs.starts.get(sequence) ?? break
		eos = irs.eos.get(sequence) ?? L
		while Bool.True {
			entry = current.get(at) ?? break
			if entry.working_class == ET {
				var last = at
				var after = next_in(irs.links, at)
				while class_at(current, after, eos) == ET {
					match after {
						Some(next) => {
							last = next
							after = next_in(irs.links, next)
						}
						None => {
							break
						}
					}
				}
				resolved = if previous == EN or class_at(current, after, eos) == EN {
					EN
				} else {
					ET
				}
				var replace = at
				while Bool.True {
					item = current.get(replace) ?? break
					current = current.set(replace, { ..item, working_class: resolved }) ?? ...
					if replace == last {
						break
					}
					match next_in(irs.links, replace) {
						Some(next) => {
							replace = next
						}
						None => break
					}
				}
				previous = resolved
				match after {
					Some(next) => {
						at = next
					}
					None => {
						break
					}
				}
			} else {
				previous = entry.working_class
				match next_in(irs.links, at) {
					Some(next) => {
						at = next
					}
					None => {
						break
					}
				}
			}
		}
		sequence = sequence + 1
	}
	current
}

w6 = |entries| entries.map(
	|entry| {
		class = entry.working_class
		{
			..entry,
			working_class: if class == ES or class == ET or class == CS {
				ON
			} else {
				class
			},
		}
	},
)

w7 = |entries, irs| {
	var current = entries
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var strong = irs.sos.get(sequence) ?? L
		var at = irs.starts.get(sequence) ?? break
		while Bool.True {
			entry = current.get(at) ?? break
			class = entry.working_class
			resolved = if class == EN and strong == L {
				L
			} else {
				class
			}
			current = current.set(at, { ..entry, working_class: resolved }) ?? ...
			if class == L or class == R {
				strong = class
			}
			match next_in(irs.links, at) {
				Some(next) => {
					at = next
				}
				None => {
					break
				}
			}
		}
		sequence = sequence + 1
	}
	current
}

## BD16 and N0 are deliberately performed one isolating run sequence at a
## time. A bracket-stack overflow invalidates *all* pairs for that sequence.
n0 = |entries, irs| {
	var current = entries
	matches = bracket_matches(entries, irs)
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var preceding = irs.sos.get(sequence) ?? L
		var at = irs.starts.get(sequence) ?? break
		while Bool.True {
			match matches.get(at) ?? None {
				Some(close) => {
					open_entry = current.get(at) ?? break
					close_entry = current.get(close) ?? break
					current = current.set(at, { ..open_entry, matched_bracket: Some(close) }) ?? ...
					current = current.set(close, { ..close_entry, matched_bracket: Some(at) }) ?? ...
					current = resolve_bracket_pair(current, irs, sequence, { open: at, close }, preceding)
				}
				None => {}
			}
			entry = current.get(at) ?? break
			match strong_direction(entry.working_class) {
				Some(value) => {
					preceding = value
				}
				None => {}
			}
			match next_in(irs.links, at) {
				Some(next) => {
					at = next
				}
				None => {
					break
				}
			}
		}
		sequence = sequence + 1
	}
	current
}

## BD16 pairing is linear: create partner links once and then let N0 walk
## openings in logical order. This avoids both a sort of all pairs and an
## O(pairs * sequence-length) rescan for each opening bracket.
bracket_matches : List(Bidi.ScalarInfo), Bidi.RunSequences -> List([Some(U64), None])
bracket_matches = |entries, irs| {
	var matches = entries.map(|_| None)
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var stack = []
		var found = []
		var overflowed = Bool.False
		var at = irs.starts.get(sequence) ?? break
		while Bool.True {
			entry = entries.get(at) ?? break
			if entry.working_class == ON {
				match BidiProperties.paired_bracket(entry.scalar) {
					Some(pair) => match pair.kind {
						Open => if stack.len() == 63 {
							overflowed = Bool.True
						} else {
							stack = stack.append({ target: Scalar.to_u32(pair.scalar), at })
						}
						Close => {
							var cursor = stack.len()
							var match_at = None
							while cursor > 0 {
								cursor = cursor - 1
								opening = stack.get(cursor) ?? break
								if paired_equal(opening.target, Scalar.to_u32(entry.scalar)) {
									match_at = Some(cursor)
									break
								}
							}
							match match_at {
								Some(index) => {
									opening = stack.get(index) ?? break
									found = found.append({ open: opening.at, close: at })
									stack = copy_slice(stack, 0, index)
								}
								None => {}
							}
						}
					}
					None => {}
				}
			}
			match next_in(irs.links, at) {
				Some(next) => {
					at = next
				}
				None => {
					break
				}
			}
		}
		if !overflowed {
			for pair in found {
				matches = matches.set(pair.open, Some(pair.close)) ?? ...
			}
		}
		sequence = sequence + 1
	}
	matches
}

paired_equal = |target, close| target == close or ((target == 0x3009 and close == 0x232A) or (target == 0x232A and close == 0x3009))

resolve_bracket_pair = |entries, irs, sequence, pair, preceding| {
	open = entries.get(pair.open) ?? return entries
	embedding = embedding_direction(open.level)
	var saw_embedding = Bool.False
	var saw_opposite = Bool.False
	var at = next_in(irs.links, pair.open)
	while at != Some(pair.close) {
		class = class_at(entries, at, irs.eos.get(sequence) ?? L)
		strong = strong_direction(class)
		if strong == Some(embedding) {
			saw_embedding = Bool.True
		}
			else if strong != None {
				saw_opposite = Bool.True
			}
		at = match at {
			Some(index) => next_in(irs.links, index)
			None => Some(pair.close)
		}
	}
	resolved = if saw_embedding {
		Some(embedding)
	} else if saw_opposite {
		if preceding == embedding {
			Some(embedding)
		} else {
			Some(preceding)
		}
	} else {
		None
	}
	match resolved {
		None => entries
		Some(class) => {
			opening = entries.get(pair.open) ?? return entries
			closing = entries.get(pair.close) ?? return entries
			var current = entries
			current = current.set(pair.open, { ..opening, working_class: class }) ?? ...
			current = current.set(pair.close, { ..closing, working_class: class }) ?? ...
			# W1 has already converted NSMs to their preceding type. N0 must
			# carry a changed bracket class through original NSMs immediately
			# after *both* endpoints (UAX #9 rev. 51 N0).
			after_open = propagate_bracket_nsm(current, irs.links, pair.open, class)
			propagate_bracket_nsm(after_open, irs.links, pair.close, class)
		}
	}
}

strong_direction = |class| if class == L {
	Some(L)
} else if class == R or class == EN or class == AN {
	Some(R)
} else {
	None
}

propagate_bracket_nsm = |entries, links, bracket, class| {
	var current = entries
	var at = next_in(links, bracket)
	while Bool.True {
		match at {
			None => {
				break
			}
			Some(index) => {
				entry = current.get(index) ?? break
				if entry.original_class != NSM {
					break
				}
				current = current.set(index, { ..entry, working_class: class }) ?? ...
				at = next_in(links, index)
			}
		}
	}
	current
}

n1_n2 = |entries, irs| {
	var current = entries
	var sequence = 0.U64
	while sequence < irs.starts.len() {
		var previous = irs.sos.get(sequence) ?? L
		var at = irs.starts.get(sequence) ?? break
		eos = irs.eos.get(sequence) ?? L
		while Bool.True {
			entry = current.get(at) ?? break
			if neutral(entry.working_class) {
				first = at
				var last = at
				var after = next_in(irs.links, at)
				while neutral(class_at(current, after, eos)) {
					match after {
						Some(next) => {
							last = next
							after = next_in(irs.links, next)
						}
						None => {
							break
						}
					}
				}
				left = strong_for_neutral(previous, entry.level)
				right = strong_for_neutral(class_at(current, after, eos), entry.level)
				resolved = if left == right {
					left
				} else {
					embedding_direction(entry.level)
				}
				var replace = first
				while Bool.True {
					item = current.get(replace) ?? break
					current = current.set(replace, { ..item, working_class: resolved }) ?? ...
					if replace == last {
						break
					}
					match next_in(irs.links, replace) {
						Some(next) => {
							replace = next
						}
						None => break
					}
				}
				previous = resolved
				match after {
					Some(next) => {
						at = next
					}
					None => {
						break
					}
				}
			} else {
				previous = entry.working_class
				match next_in(irs.links, at) {
					Some(next) => {
						at = next
					}
					None => {
						break
					}
				}
			}
		}
		sequence = sequence + 1
	}
	current
}

isolate_control = |class| class == LRI or class == RLI or class == FSI or class == PDI

neutral = |class| class == ON or class == WS or class == S or class == B or isolate_control(class)

embedding_direction = |level| match level {
	Some(value) => direction_class(value)
	None => L
}

strong_for_neutral = |class, level| match strong_direction(class) {
	Some(value) => value
	None => embedding_direction(level)
}

implicit = |entries| entries.map(
	|entry| {
		match entry.level {
			None => entry
			Some(level) => {
				resolved = implicit_level(level, entry.working_class)
				{ ..entry, level: Some(resolved), needs_mirrored_glyph: BidiProperties.is_mirrored(entry.scalar) and resolved % 2 == 1, mirroring_glyph: BidiProperties.mirroring_glyph(entry.scalar) }
			}
		}
	},
)

implicit_level = |base, class| {
	if base % 2 == 0 {
		if class == R or class == AL {
			base + 1
		} else if class == EN or class == AN {
			base + 2
		} else {
			base
		}
	} else {
		if class == L or class == EN or class == AN {
			base + 1
		} else {
			base
		}
	}
}

x9_removed = |class| class == RLE or class == LRE or class == RLO or class == LRO or class == PDF or class == BN

non_rendering = |class| x9_removed(class) or class == LRI or class == RLI or class == FSI or class == PDI

copy_slice = |items, start, end| {
	var copied = []
	var at = start
	while at < end {
		copied = copied.append(items.get(at) ?? break)
		at = at + 1
	}
	copied
}

l1 = |line, paragraph_level| {
	var reset_at = line.len()
	while reset_at > 0 {
		entry = line.get(reset_at - 1) ?? break
		match entry.level {
			None => {
				reset_at = reset_at - 1
			}
			Some(_) => if l1_reset(entry.original_class) {
				reset_at = reset_at - 1
			} else {
				break
			}
		}
	}
	var output = []
	var at = 0.U64
	while at < line.len() {
		entry = line.get(at) ?? break
		output = output.append(
			match entry.level {
				None => None
				Some(level) => if at >= reset_at {
					Some(paragraph_level)
				} else {
					Some(level)
				}
			},
		)
		at = at + 1
	}

	## L1 additionally resets each segment/paragraph separator and the
	## immediately preceding whitespace or isolate-control sequence, not merely
	## the trailing sequence at the actual line end.
	var result = output
	var separator = 0.U64
	while separator < line.len() {
		entry = line.get(separator) ?? break
		if entry.original_class == S or entry.original_class == B {
			result = result.set(separator, Some(paragraph_level)) ?? ...
			var before = separator
			while before > 0 {
				before = before - 1
				previous = line.get(before) ?? break
				if previous.level == None {
					# X9 formatting controls are absent for L1 adjacency.
				} else if previous.original_class == WS or isolate_control(previous.original_class) {
					result = result.set(before, Some(paragraph_level)) ?? ...
				} else {
					break
				}
			}
		}
		separator = separator + 1
	}
	result
}

l1_reset = |class| class == WS or class == S or class == B or class == LRI or class == RLI or class == FSI or class == PDI

eligible = |line, levels, start| {
	var output = []
	var at = 0.U64
	while at < line.len() {
		match levels.get(at) {
			Ok(None) => {}
			Ok(Some(_)) => {
				output = output.append(start + at)
			}
			Err(_) => {}
		}
		at = at + 1
	}
	output
}

l2 = |indices, levels, start, paragraph_level| {
	var maximum = paragraph_level
	for absolute in indices {
		match levels.get(absolute - start) {
			Ok(Some(value)) => if value > maximum {
				maximum = value
			}
			_ => {}
		}
	}
	var output = indices
	var threshold = maximum
	# L2 runs down through the lowest odd level, including an RTL paragraph's
	# base level. Stopping at the paragraph level would incorrectly leave a
	# simple RTL paragraph in logical order.
	minimum = if paragraph_level % 2 == 1 {
		paragraph_level
	} else {
		paragraph_level + 1
	}
	while threshold >= minimum {
		output = reverse_level(output, levels, start, threshold)
		if threshold == 0 {
			break
		}
		threshold = threshold - 1
	}
	output
}

reverse_level = |indices, levels, start, threshold| {
	var output = []
	var at = 0.U64
	while at < indices.len() {
		absolute = indices.get(at) ?? break
		match levels.get(absolute - start) {
			Ok(Some(value)) => if value >= threshold {
				var end = at + 1
				while end < indices.len() {
					candidate = indices.get(end) ?? break
					match levels.get(candidate - start) {
						Ok(Some(candidate_level)) => if candidate_level >= threshold {
							end = end + 1
						} else {
							break
						}
						_ => break
					}
				}
				var cursor = end
				while cursor > at {
					cursor = cursor - 1
					output = output.append(indices.get(cursor) ?? break)
				}
				at = end
			} else {
				output = output.append(absolute)
				at = at + 1
			}
			_ => {
				output = output.append(absolute)
				at = at + 1
			}
		}
	}
	output
}

inverse = |length, visual, start| {
	var output = []
	var local = 0.U64
	while local < length {
		output = output.append(None)
		local = local + 1
	}
	var visual_index = 0.U64
	while visual_index < visual.len() {
		absolute = visual.get(visual_index) ?? break
		output = output.set(absolute - start, Some(visual_index)) ?? ...
		visual_index = visual_index + 1
	}
	output
}

## Maximal runs in visual order. A run is extended only while its embedding
## level is equal and its logical indices remain adjacent (in either direction),
## so its reported logical range is always an honest contiguous half-open span.
make_visual_runs = |visual, levels, start| {
	var output = []
	var at = 0.U64
	while at < visual.len() {
		first = visual.get(at) ?? break
		level = match levels.get(first - start) {
			Ok(Some(value)) => value
			_ => 0
		}
		var end = at + 1
		var previous = first
		while end < visual.len() {
			candidate = visual.get(end) ?? break
			candidate_level = match levels.get(candidate - start) {
				Ok(Some(value)) => value
				_ => 0
			}
			adjacent = candidate + 1 == previous or previous + 1 == candidate
			if candidate_level == level and adjacent {
				previous = candidate
				end = end + 1
			} else {
				break
			}
		}
		low = if first < previous {
			first
		} else {
			previous
		}
		high = if first > previous {
			first
		} else {
			previous
		}
		range = ScalarRange.from_bounds(low, high + 1) ?? ...
		output = output.append({ logical_range: range, level, direction: Bidi.direction(level) })
		at = end
	}
	output
}

## L4 is applied after the line-specific L1 reset. This records rendering
## facts without replacing the source scalar; missing best-fit mappings remain
## meaningful when a mirrored glyph is still required.
calculate_line_mirroring : List(Bidi.ScalarInfo), List([Some(U8), None]) -> List(Bidi.MirrorInfo)
calculate_line_mirroring = |entries, levels| {
	var output = []
	var at = 0.U64
	while at < entries.len() {
		entry = entries.get(at) ?? break
		needs = match levels.get(at) {
			Ok(Some(level)) => BidiProperties.is_mirrored(entry.scalar) and level % 2 == 1
			_ => Bool.False
		}
		output = output.append({ needs_glyph: needs, glyph: BidiProperties.mirroring_glyph(entry.scalar) })
		at = at + 1
	}
	output
}
