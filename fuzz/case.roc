app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.2.1/9Qpttb6LTgcMaVsSBLsnaiS2mDUrf6Bxa6dX9Rqwviz4.tar.zst",
	unicode: "../package/main.roc",
}

import FuzzSupport
import fuzz.Fuzz
import unicode.ByteRange
import unicode.Case
import unicode.TextRange

test : List(U32) -> Fuzz.Outcome
test = |code_points| {
	parts = FuzzSupport.source_parts(code_points)
	source = Str.join_with(parts, "")

	lower = Case.to_lower(source, Case.unicode_default, Case.unlimited_limits) ?? crash "Case.to_lower failed under unlimited limits"
	upper = Case.to_upper(source, Case.unicode_default, Case.unlimited_limits) ?? crash "Case.to_upper failed under unlimited limits"
	title = Case.to_title(source, Case.unicode_default, Case.unlimited_limits) ?? crash "Case.to_title failed under unlimited limits"
	folded = Case.fold(source, Case.full, Case.unlimited_limits) ?? crash "Case.fold failed under unlimited limits"

	validate_result(source, lower)
	validate_result(source, upper)
	validate_result(source, title)
	validate_result(source, folded)

	refolded = Case.fold(Case.result_text(folded), Case.full, Case.unlimited_limits) ?? crash "Case.fold failed on its own output"
	if Case.result_text(refolded) != Case.result_text(folded) {
		crash "Case.fold was not idempotent on its own output"
	}

	relowered = Case.to_lower(Case.result_text(lower), Case.unicode_default, Case.unlimited_limits) ?? crash "Case.to_lower failed on its own output"
	if Case.result_text(relowered) != Case.result_text(lower) {
		crash "Case.to_lower was not idempotent on its own output"
	}

	check_zero_budget_limit_failure(source)

	Fuzz.keep
}

## A zero input-byte budget must reject any nonempty source atomically: no
## partial text or facts, only a `LimitExceeded` error.
check_zero_budget_limit_failure : Str -> {}
check_zero_budget_limit_failure = |source| {
	tiny_limits = Case.limits(0, U64.highest, U64.highest, U64.highest, U64.highest)
	match Case.to_lower(source, Case.unicode_default, tiny_limits) {
		Ok(_) => if source != "" {
			crash "a zero input-byte budget accepted nonempty input"
		}
		Err(error) => match Case.error_kind(error) {
			LimitExceeded => {}
			_ => crash "a zero input-byte budget failed with something other than LimitExceeded"
		}
	}
	{}
}

validate_result : Str, Case.Result -> {}
validate_result = |source, result| {
	text = Case.result_text(result)
	facts = Case.result_facts(result)

	input_ranges = facts.map(|fact| TextRange.byte_range(Case.fact_input(fact)))
	output_ranges = facts.map(|fact| TextRange.byte_range(Case.fact_output(fact)))

	validate_source_partition(source, input_ranges)
	validate_result_partition(text, output_ranges)

	for fact in facts {
		input_range = TextRange.byte_range(Case.fact_input(fact))
		output_range = TextRange.byte_range(Case.fact_output(fact))
		input_slice = ByteRange.slice(input_range, source) ?? crash "a case fact input range was not scalar-aligned and in bounds"
		output_slice = ByteRange.slice(output_range, text) ?? crash "a case fact output range was not scalar-aligned and in bounds"
		match Case.fact_shape(fact) {
			Removed => if !ByteRange.is_empty(output_range) {
				crash "a Removed case fact produced a nonempty output range"
			}
			Unchanged => if input_slice != output_slice {
				crash "an Unchanged case fact changed its source text"
			}
			Simple => {}
			Expanded => {}
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

target = Fuzz.target_with({
	name: "unicode-case",
	generator: FuzzSupport.scalar_sequence,
	test,
	show: FuzzSupport.show_scalars,
})
