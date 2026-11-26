import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Case
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextRange

test : FuzzSupport.Plan -> Fuzz.Outcome
test = |input| {
	code_points = input.code_points
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")

	mapping = match input.config % 3 {
		0 => Case.unicode_default
		1 => Case.turkic
		_ => Case.lithuanian
	}
	folding = match input.config.shr_wrap(2) % 4 {
		0 => Case.full
		1 => Case.simple
		2 => Case.turkic_full
		_ => Case.turkic_simple
	}
	lower = Case.to_lower(source, { profile: mapping, limits: Case.unlimited_limits }) ?? crash "Case.to_lower failed under unlimited limits"
	upper = Case.to_upper(source, { profile: mapping, limits: Case.unlimited_limits }) ?? crash "Case.to_upper failed under unlimited limits"
	title = Case.to_title(source, { profile: mapping, limits: Case.unlimited_limits }) ?? crash "Case.to_title failed under unlimited limits"
	folded = Case.fold(source, { profile: folding, limits: Case.unlimited_limits }) ?? crash "Case.fold failed under unlimited limits"

	validate_result(source, lower)
	validate_result(source, upper)
	validate_result(source, title)
	validate_result(source, folded)

	refolded = Case.fold(Case.result_text(folded), { profile: folding, limits: Case.unlimited_limits }) ?? crash "Case.fold failed on its own output"
	if Case.result_text(refolded) != Case.result_text(folded) {
		crash "Case.fold was not idempotent on its own output"
	}

	relowered = Case.to_lower(Case.result_text(lower), { limits: Case.unlimited_limits }) ?? crash "Case.to_lower failed on its own output"
	if Case.result_text(relowered) != Case.result_text(lower) {
		crash "Case.to_lower was not idempotent on its own output"
	}

	check_limits(source, code_points.len(), mapping, lower, input.config.shr_wrap(4))

	Fuzz.keep
}

check_limits : Str, U64, Case.MappingProfile, Case.Result, U8 -> {}
check_limits = |source, input_scalars, mapping, expected, selector| {
	input_bytes = source.count_utf8_bytes()
	text = Case.result_text(expected)
	output_bytes = text.count_utf8_bytes()
	output_scalars = Scalar.iter(text).fold(0.U64, |count, _| count + 1)
	facts = Case.result_facts(expected).len()
	exact = { max_input_bytes: input_bytes, max_input_scalars: input_scalars, max_output_bytes: output_bytes, max_output_scalars: output_scalars, max_mapping_facts: facts }
	fit = Case.to_lower(source, { profile: mapping, limits: exact }) ?? crash "case operation failed at the exact limit"
	if Case.result_text(fit) != text or Case.result_facts(fit) != Case.result_facts(expected) {
		crash "case operation changed text or mapping facts at the exact limit"
	}
	(tightened, required, resource) = match selector % 5 {
		0 => ({ ..exact, max_input_bytes: previous(input_bytes) }, input_bytes, InputBytes)
		1 => ({ ..exact, max_input_scalars: previous(input_scalars) }, input_scalars, InputScalars)
		2 => ({ ..exact, max_output_bytes: previous(output_bytes) }, output_bytes, OutputBytes)
		3 => ({ ..exact, max_output_scalars: previous(output_scalars) }, output_scalars, OutputScalars)
		_ => ({ ..exact, max_mapping_facts: previous(facts) }, facts, Facts)
	}
	if required != 0 {
		match Case.to_lower(source, { profile: mapping, limits: tightened }) {
			Err(error) => if Case.error_kind(error) != LimitExceeded or Case.error_limit_resource(error) != resource or Case.error_limit(error) != Some({ limit: required - 1, required }) {
				crash "case limit failure reported incorrect resource or bounds"
			}
			Ok(_) => crash "case operation accepted text exceeding its resource limit"
		}
	}
	{}
}

previous : U64 -> U64
previous = |length| if (length == 0) 0 else length - 1

validate_result : Str, Case.Result -> {}
validate_result = |source, result| {
	text = Case.result_text(result)
	facts = Case.result_facts(result)

	input_ranges = facts.map(|fact| TextRange.byte_range(Case.fact_input(fact)))
	output_ranges = facts.map(|fact| TextRange.byte_range(Case.fact_output(fact)))

	validate_source_partition(source, input_ranges)
	validate_result_partition(text, output_ranges)

	var $input_index = 0
	var $output_index = 0
	for fact in facts {
		input_range = TextRange.byte_range(Case.fact_input(fact))
		output_range = TextRange.byte_range(Case.fact_output(fact))
		input_slice = ByteRange.slice(input_range, source) ?? crash "a case fact input range was not scalar-aligned and in bounds"
		output_slice = ByteRange.slice(output_range, text) ?? crash "a case fact output range was not scalar-aligned and in bounds"
		input_scalars = TextRange.scalar_range(Case.fact_input(fact))
		output_scalars = TextRange.scalar_range(Case.fact_output(fact))
		width = Scalar.iter(output_slice).fold(0.U64, |count, _| count + 1)
		if ScalarRange.start(input_scalars) != $input_index or ScalarRange.len(input_scalars) != 1 or Scalar.iter(input_slice).fold(0.U64, |count, _| count + 1) != 1 {
			crash "case fact did not map exactly one source scalar"
		}
		if ScalarRange.start(output_scalars) != $output_index or ScalarRange.len(output_scalars) != width {
			crash "case fact reported inconsistent scalar coordinates"
		}
		$input_index = $input_index + 1
		$output_index = $output_index + width
		match Case.fact_shape(fact) {
			Removed => if width != 0 {
				crash "a Removed case fact produced a nonempty output range"
			}
			Unchanged => if width != 1 or input_slice != output_slice {
				crash "an Unchanged case fact changed its source text"
			}
			Simple => if width != 1 or input_slice == output_slice {
				crash "a Simple case fact must produce a different scalar"
			}
			Expanded => if width <= 1 {
				crash "an Expanded case fact did not produce multiple scalars"
			}
		}
	}
	{}
}

## One fact per source scalar: ranges are nonempty, scalar-aligned, contiguous,
## and cover the complete source exactly.
validate_source_partition : Str, List(ByteRange) -> {}
validate_source_partition = |source, ranges| {
	if source == "" and !ranges.is_empty() {
		crash "empty source produced a case fact"
	}
	if source != "" and ranges.is_empty() {
		crash "nonempty source produced no case facts"
	}
	var $next_start = 0
	for range in ranges {
		start = ByteRange.start(range)
		end = ByteRange.end(range)
		if start != $next_start or end <= start {
			crash "case input ranges were empty, overlapping, or discontinuous"
		}
		_ = ByteRange.slice(range, source) ?? crash "case input range was not scalar-aligned and in bounds"
		$next_start = end
	}
	if $next_start != source.count_utf8_bytes() {
		crash "case input ranges did not cover the complete source"
	}
	{}
}

## Output ranges may be empty (a `Removed` fact), but must stay contiguous and
## cover the complete result text exactly.
validate_result_partition : Str, List(ByteRange) -> {}
validate_result_partition = |text, ranges| {
	var $next_start = 0
	for range in ranges {
		start = ByteRange.start(range)
		end = ByteRange.end(range)
		if start != $next_start or end < start {
			crash "case output ranges were overlapping or discontinuous"
		}
		_ = ByteRange.slice(range, text) ?? crash "case output range was not scalar-aligned and in bounds"
		$next_start = end
	}
	if $next_start != text.count_utf8_bytes() {
		crash "case output ranges did not cover the complete result text"
	}
	{}
}

CaseTarget :: [].{
	target = Fuzz.target_with({
		name: "unicode-case",
		examples: [Fuzz.saved({ name: "turkic-dot-removal", revision: "1", bytes: [0x01, 0x49, 0x00, 0x00, 0x07, 0x03, 0x20, 0x20, 0x00, 0x40, 0x9E, 0x1E, 0x60] })],
		generator: FuzzSupport.plan,
		test,
		show: FuzzSupport.show_plan,
	})
}
