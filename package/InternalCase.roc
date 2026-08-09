import ByteRange
import InternalCaseData
import InternalUtf8
import ScalarRange
import TextPosition
import TextRange
import Word

Shape : [Unchanged, Simple, Expanded, Removed]

Fact : {
	input : TextRange,
	output : TextRange,
	shape : Shape,
	contextual : Bool,
}

Error : [
	LimitExceeded({ resource : [InputBytes, InputScalars, OutputBytes, OutputScalars, Facts], limit : U64, required : U64 }),
	CoordinateOverflow({ at : TextPosition }),
	InternalEncodingFault,
]

MappingProfile : [UnicodeDefault, Turkic, Lithuanian]

FoldProfile : [Full, Simple, TurkicFull, TurkicSimple]

Limits : {
	max_input_bytes : U64,
	max_input_scalars : U64,
	max_output_bytes : U64,
	max_output_scalars : U64,
	max_mapping_facts : U64,
}

Mapping : [Identity(U32), One(U32), Sequence(List(U32))]

LeftContext : {
	before_final_sigma : Bool,
	after_soft_dotted : Bool,
	after_i : Bool,
}

Accumulator : {
	bytes : List(U8),
	facts : List(Fact),
	input_scalars : U64,
	output_scalars : U64,
	left : LeftContext,
}

Fold : [Running(Accumulator), Failed(Error)]

TitleState : [BeforeFirstCased, AfterFirstCased]

## Complete-source Unicode case conversion core. Conditional mappings inspect
## the unchanged source. Right-context probes are interval-local: Final_Sigma
## consumes at most the next case-ignorable run and the CCC predicates consume
## at most one canonical-combining interval, so no source or scalar tape is
## retained.
InternalCase :: [].{
	Shape : Shape
	Fact : Fact
	Error : Error

	lower : Str, MappingProfile, Limits -> Try({ text : Str, facts : List(Fact) }, Error)
	lower = |source, profile, limits| run_case(source, profile, limits, Lower)

	upper : Str, MappingProfile, Limits -> Try({ text : Str, facts : List(Fact) }, Error)
	upper = |source, profile, limits| run_case(source, profile, limits, Upper)

	fold : Str, FoldProfile, Limits -> Try({ text : Str, facts : List(Fact) }, Error)
	fold = |source, profile, limits| run_fold(source, profile, limits)

	## R3 titlecasing. `Word.fold_ranges` drives the exact UAX #29 transition
	## core and each completed segment is replayed immediately; no range list is
	## materialized and no prior source segment is retained.
	title : Str, MappingProfile, Limits -> Try({ text : Str, facts : List(Fact) }, Error)
	title = |source, profile, limits| {
		match check_input_bytes(source, limits) {
			Err(error) => Err(error)
			Ok({}) => {
				folded = Word.fold_ranges(
					source,
					Running(empty_accumulator),
					|state, range| {
						match state {
							Failed(_) => state
							Running(accumulator) => {
								match ByteRange.slice(range, source) {
									Err(_) => Failed(InternalEncodingFault)
									Ok(segment) => replay_title_segment(source, segment, ByteRange.start(range), profile, limits, accumulator)
								}
							}
						}
					},
				)
				finish(folded)
			}
		}
	}
}

Operation : [Lower, Upper, Title]

run_case = |source, profile, limits, operation| {
	match check_input_bytes(source, limits) {
		Err(error) => Err(error)
		Ok({}) => {
			folded = InternalUtf8.fold_scalars(
				source,
				Running(empty_accumulator),
				|state, scalar, byte_start, byte_end, scalar_index| {
					match state {
						Failed(_) => state
						Running(accumulator) => apply_scalar(source, profile, limits, operation, Bool.False, accumulator, scalar, byte_start, byte_end, scalar_index)
					}
				},
			)
			finish(folded)
		}
	}
}

run_fold = |source, profile, limits| {
	match check_input_bytes(source, limits) {
		Err(error) => Err(error)
		Ok({}) => {
			folded = InternalUtf8.fold_scalars(
				source,
				Running(empty_accumulator),
				|state, scalar, byte_start, byte_end, scalar_index| {
					match state {
						Failed(_) => state
						Running(accumulator) => apply_fold_scalar(limits, profile, accumulator, scalar, byte_start, byte_end, scalar_index)
					}
				},
			)
			finish(folded)
		}
	}
}

replay_title_segment = |source, segment, byte_base, profile, limits, accumulator| {
	InternalUtf8.fold_scalars(
		segment,
		{ fold: Running(accumulator), title: BeforeFirstCased },
		|state, scalar, local_start, local_end, _local_index| {
			match state.fold {
				Failed(_) => state
				Running(current) => {
					byte_start = match byte_base.plus_try(local_start) {
						Err(Overflow) => return { ..state, fold: Failed(CoordinateOverflow({ at: TextPosition.from_offsets(byte_base, current.input_scalars) })) }
						Ok(value) => value
					}
					byte_end = match byte_start.plus_try(local_end - local_start) {
						Err(Overflow) => return { ..state, fold: Failed(CoordinateOverflow({ at: TextPosition.from_offsets(byte_start, current.input_scalars) })) }
						Ok(value) => value
					}
					props = InternalCaseData.lookup(scalar)
					(operation, next_title) = match state.title {
						BeforeFirstCased if props.cased => (Title, AfterFirstCased)
						BeforeFirstCased => (IdentityTitle, BeforeFirstCased)
						AfterFirstCased => (Lower, AfterFirstCased)
					}
					{
						fold: apply_title_operation(source, profile, limits, operation, current, scalar, byte_start, byte_end),
						title: next_title,
					}
				}
			}
		},
	).fold
}

TitleOperation : [IdentityTitle, Title, Lower]

apply_title_operation = |source, profile, limits, operation, accumulator, scalar, byte_start, byte_end| {
	match operation {
		IdentityTitle => {
			props = InternalCaseData.lookup(scalar)
			applied = apply_scalar_with_mapping(limits, accumulator, scalar, byte_start, byte_end, Identity(scalar), Bool.True)
			match applied {
				Failed(_) => applied
				Running(next) => Running({ ..next, left: advance_left_context(next.left, scalar, props) })
			}
		}
		Title => apply_scalar(source, profile, limits, Title, Bool.True, accumulator, scalar, byte_start, byte_end, accumulator.input_scalars)
		Lower => apply_scalar(source, profile, limits, Lower, Bool.True, accumulator, scalar, byte_start, byte_end, accumulator.input_scalars)
	}
}

apply_scalar = |source, profile, limits, operation, title_contextual, accumulator, scalar, byte_start, byte_end, scalar_index| {
	props = InternalCaseData.lookup(scalar)
	if scalar_index != accumulator.input_scalars {
		Failed(CoordinateOverflow({ at: TextPosition.from_offsets(byte_start, accumulator.input_scalars) }))
	} else {
		mapping = select_case_mapping(source, profile, operation, scalar, byte_end, props, accumulator.left)
		contextual = title_contextual or mapping.contextual
		applied = apply_scalar_with_mapping(limits, accumulator, scalar, byte_start, byte_end, mapping.value, contextual)
		match applied {
			Failed(_) => applied
			Running(next) => Running({ ..next, left: advance_left_context(next.left, scalar, props) })
		}
	}
}

apply_fold_scalar = |limits, profile, accumulator, scalar, byte_start, byte_end, scalar_index| {
	if scalar_index != accumulator.input_scalars {
		Failed(CoordinateOverflow({ at: TextPosition.from_offsets(byte_start, accumulator.input_scalars) }))
	} else {
		props = InternalCaseData.lookup(scalar)
		applied = apply_scalar_with_mapping(limits, accumulator, scalar, byte_start, byte_end, select_fold(profile, scalar, props), Bool.False)
		match applied {
			Failed(_) => applied
			Running(next) => Running({ ..next, left: advance_left_context(next.left, scalar, props) })
		}
	}
}

SelectedMapping : { value : Mapping, contextual : Bool }

select_case_mapping = |source, profile, operation, scalar, byte_end, props, left| {
	base = simple_mapping(operation, scalar, props)
	var selected = { value: base, contextual: Bool.False }
	# SpecialCasing's unconditional row is the normal full mapping. An
	# applicable language and/or source-context row overrides it. The generator
	# guarantees that equal-specificity rows which can overlap an exposed
	# profile have identical mappings, so retaining the first is semantically
	# equivalent and needs no runtime tie-break rule.
	var selected_specificity = 0.U64
	var selected_special = Bool.False
	var index = 0.U64
	while index < props.special_count.to_u64() {
		special_index = props.special_start.to_u64() + index
		match InternalCaseData.special_entries.get(special_index) {
			Err(_) => {}
			Ok(entry) => {
				specificity = entry.languages.len() + entry.contexts.len()
				if (!selected_special or specificity > selected_specificity) and languages_match(entry.languages, profile) and contexts_match(entry.contexts, source, byte_end, left) {
					selected = { value: special_mapping(entry, operation), contextual: entry.contexts.len() != 0 }
					selected_specificity = specificity
					selected_special = Bool.True
				}
			}
		}
		index = index + 1
	}
	selected
}

simple_mapping = |operation, scalar, props| {
	mapped = match operation {
		Lower => props.simple_lower
		Title => props.simple_title
		Upper => props.simple_upper
	}
	if mapped == 0 {
		Identity(scalar)
	} else {
		One(mapped)
	}
}

special_mapping = |entry, operation| match operation {
	Lower => Sequence(entry.lower)
	Title => Sequence(entry.title)
	Upper => Sequence(entry.upper)
}

select_fold = |profile, scalar, props| {
	var common = Identity(scalar)
	var full = Identity(scalar)
	var simple = Identity(scalar)
	var turkic = Identity(scalar)
	var has_common = Bool.False
	var has_full = Bool.False
	var has_simple = Bool.False
	var has_turkic = Bool.False
	var index = 0.U64
	while index < props.fold_count.to_u64() {
		fold_index = props.fold_start.to_u64() + index
		match InternalCaseData.fold_entries.get(fold_index) {
			Err(_) => {}
			Ok(entry) => match entry.status {
				Common => {
					common = Sequence(entry.mapping)
					has_common = Bool.True
				}
				Full => {
					full = Sequence(entry.mapping)
					has_full = Bool.True
				}
				Simple => {
					simple = Sequence(entry.mapping)
					has_simple = Bool.True
				}
				Turkic => {
					turkic = Sequence(entry.mapping)
					has_turkic = Bool.True
				}
			}
		}
		index = index + 1
	}
	if profile == TurkicFull or profile == TurkicSimple {
		if has_turkic {
			turkic
		} else if profile == TurkicFull {
			if has_full {
				full
			} else if has_common {
				common
			} else {
				Identity(scalar)
			}
		} else if has_simple {
			simple
		} else if has_common {
			common
		} else {
			Identity(scalar)
		}
	} else if profile == Full {
		if has_full {
			full
		} else if has_common {
			common
		} else {
			Identity(scalar)
		}
	} else if has_simple {
		simple
	} else if has_common {
		common
	} else {
		Identity(scalar)
	}
}

languages_match = |languages, profile| {
	if languages.len() == 0 {
		Bool.True
	} else {
		var matched = Bool.False
		for language in languages {
			if profile == Turkic and (language == Turkish or language == Azeri) {
				matched = Bool.True
			} else if profile == Lithuanian and language == Lithuanian {
				matched = Bool.True
			}
		}
		matched
	}
}

contexts_match = |contexts, source, byte_end, left| {
	var matched = Bool.True
	for context in contexts {
		condition = match context {
			Final_Sigma => left.before_final_sigma and !following_cased(source, byte_end)
			After_Soft_Dotted => left.after_soft_dotted
			More_Above => more_above(source, byte_end)
			Before_Dot => before_dot(source, byte_end)
			After_I => left.after_i
			Not_Before_Dot => !before_dot(source, byte_end)
		}
		matched = matched and condition
	}
	matched
}

following_cased = |source, byte_start| {
	var cursor = { ..InternalUtf8.init(source), byte_offset: byte_start }
	while Bool.True {
		match InternalUtf8.next(cursor) {
			Done => return Bool.False
			One({ item, rest }) => {
				props = InternalCaseData.lookup(item.scalar)
				if !props.case_ignorable {
					return props.cased
				}
				cursor = rest
			}
		}
	}
	Bool.False
}

more_above = |source, byte_start| {
	var cursor = { ..InternalUtf8.init(source), byte_offset: byte_start }
	while Bool.True {
		match InternalUtf8.next(cursor) {
			Done => return Bool.False
			One({ item, rest }) => {
				ccc = InternalCaseData.lookup(item.scalar).ccc
				if ccc == 230 {
					return Bool.True
				}
				if ccc == 0 {
					return Bool.False
				}
				cursor = rest
			}
		}
	}
	Bool.False
}

before_dot = |source, byte_start| {
	var cursor = { ..InternalUtf8.init(source), byte_offset: byte_start }
	while Bool.True {
		match InternalUtf8.next(cursor) {
			Done => return Bool.False
			One({ item, rest }) => {
				if item.scalar == 0x307 {
					return Bool.True
				}
				ccc = InternalCaseData.lookup(item.scalar).ccc
				if ccc == 0 or ccc == 230 {
					return Bool.False
				}
				cursor = rest
			}
		}
	}
	Bool.False
}

advance_left_context = |left, scalar, props| {
	before_final_sigma = if props.case_ignorable {
		left.before_final_sigma
	} else {
		props.cased
	}
	after_soft_dotted = if props.soft_dotted {
		Bool.True
	} else if props.ccc == 0 or props.ccc == 230 {
		Bool.False
	} else {
		left.after_soft_dotted
	}
	after_i = if scalar == 0x49 {
		Bool.True
	} else if props.ccc == 0 or props.ccc == 230 {
		Bool.False
	} else {
		left.after_i
	}
	{ before_final_sigma, after_soft_dotted, after_i }
}

apply_scalar_with_mapping : Limits, Accumulator, U32, U64, U64, Mapping, Bool -> Fold
apply_scalar_with_mapping = |limits, accumulator, scalar, byte_start, byte_end, mapping, contextual| {
	input_required = required_plus(accumulator.input_scalars, 1)
	if input_required > limits.max_input_scalars {
		return Failed(LimitExceeded({ resource: InputScalars, limit: limits.max_input_scalars, required: input_required }))
	}
	facts_required = required_plus(accumulator.facts.len(), 1)
	if facts_required > limits.max_mapping_facts {
		return Failed(LimitExceeded({ resource: Facts, limit: limits.max_mapping_facts, required: facts_required }))
	}
	metrics = mapping_metrics(mapping)
	output_scalars_required = required_plus(accumulator.output_scalars, metrics.scalars)
	if output_scalars_required > limits.max_output_scalars {
		return Failed(LimitExceeded({ resource: OutputScalars, limit: limits.max_output_scalars, required: output_scalars_required }))
	}
	output_bytes_required = required_plus(accumulator.bytes.len(), metrics.bytes)
	if output_bytes_required > limits.max_output_bytes {
		return Failed(LimitExceeded({ resource: OutputBytes, limit: limits.max_output_bytes, required: output_bytes_required }))
	}

	input_end_scalar = required_plus(accumulator.input_scalars, 1)
	output_start_byte = accumulator.bytes.len()
	input = text_range(byte_start, byte_end, accumulator.input_scalars, input_end_scalar)
	output = text_range(output_start_byte, output_bytes_required, accumulator.output_scalars, output_scalars_required)
	match input {
		Err(error) => Failed(error)
		Ok(input_range) => match output {
			Err(error) => Failed(error)
			Ok(output_range) => {
				bytes = append_mapping(accumulator.bytes, mapping)
				shape = mapping_shape(mapping, scalar)
				Running({
					..accumulator,
					bytes,
					facts: accumulator.facts.append({ input: input_range, output: output_range, shape, contextual }),
					input_scalars: input_required,
					output_scalars: output_scalars_required,
				})
			}
		}
	}
}

mapping_metrics = |mapping| {
	for_mapping(
		mapping,
		{ bytes: 0.U64, scalars: 0.U64 },
		|state, scalar| {
			{
				bytes: required_plus(state.bytes, utf8_width(scalar)),
				scalars: required_plus(state.scalars, 1),
			}
		},
	)
}

mapping_shape = |mapping, input| match mapping {
	Identity(_) => Unchanged
	One(output) => if output == input {
		Unchanged
	} else {
		Simple
	}
	Sequence(outputs) => if outputs.len() == 0 {
		Removed
	} else if outputs.len() == 1 {
		if (outputs.get(0) ?? input) == input {
			Unchanged
		} else {
			Simple
		}
	} else {
		Expanded
	}
}

append_mapping = |initial, mapping| {
	for_mapping(mapping, initial, |bytes, scalar| append_utf8(bytes, scalar))

}

for_mapping = |mapping, initial, emit| match mapping {
	Identity(scalar) => emit(initial, scalar)
	One(scalar) => emit(initial, scalar)
	Sequence(items) => {
		var state = initial
		for scalar in items {
			state = emit(state, scalar)
		}
		state
	}
}

append_utf8 = |bytes, scalar| {
	if scalar < 0x80 {
		bytes.append(scalar.to_u8_wrap())
	} else if scalar < 0x800 {
		bytes.reserve(2).append(scalar.shr_wrap(6).bitwise_or(0xC0).to_u8_wrap()).append(scalar.bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap())
	} else if scalar < 0x10000 {
		bytes.reserve(3).append(scalar.shr_wrap(12).bitwise_or(0xE0).to_u8_wrap()).append(scalar.shr_wrap(6).bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap()).append(scalar.bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap())
	} else {
		bytes.reserve(4).append(scalar.shr_wrap(18).bitwise_or(0xF0).to_u8_wrap()).append(scalar.shr_wrap(12).bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap()).append(scalar.shr_wrap(6).bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap()).append(scalar.bitwise_and(0x3F).bitwise_or(0x80).to_u8_wrap())
	}
}

utf8_width = |scalar| if scalar < 0x80 {
	1
} else if scalar < 0x800 {
	2
} else if scalar < 0x10000 {
	3
} else {
	4
}

text_range = |byte_start, byte_end, scalar_start, scalar_end| {
	match ByteRange.from_bounds(byte_start, byte_end) {
		Err(_) => Err(CoordinateOverflow({ at: TextPosition.from_offsets(byte_start, scalar_start) }))
		Ok(bytes) => match ScalarRange.from_bounds(scalar_start, scalar_end) {
			Err(_) => Err(CoordinateOverflow({ at: TextPosition.from_offsets(byte_start, scalar_start) }))
			Ok(scalars) => Ok(TextRange.from_ranges(bytes, scalars))
		}
	}
}

required_plus = |left, right| match left.plus_try(right) {
	Ok(value) => value
	Err(Overflow) => U64.highest
}

check_input_bytes = |source, limits| {
	required = source.count_utf8_bytes()
	if required > limits.max_input_bytes {
		Err(LimitExceeded({ resource: InputBytes, limit: limits.max_input_bytes, required }))
	} else {
		Ok({})
	}
}

empty_accumulator = {
	bytes: [],
	facts: [],
	input_scalars: 0,
	output_scalars: 0,
	left: { before_final_sigma: Bool.False, after_soft_dotted: Bool.False, after_i: Bool.False },
}

finish = |fold| match fold {
	Failed(error) => Err(error)
	Running(accumulator) => match Str.from_utf8(accumulator.bytes) {
		Err(_) => Err(InternalEncodingFault)
		Ok(text) => Ok({ text, facts: accumulator.facts })
	}
}
