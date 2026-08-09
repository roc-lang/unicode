app [run!] {
	pf: platform "../../platform/main.roc",
	unicode: "../../../package/main.roc",
}

import unicode.ByteRange
import unicode.Case
import unicode.Scalar
import unicode.ScalarRange
import unicode.TextRange
import unicode.UnicodeVersion

run! : Str => Str
run! = |input| {
	match run_focused(input.count_utf8_bytes()) {
		Err(message) => fail("focused", message)
		Ok({}) => run_payload(input)
	}
}

run_payload : Str -> Str
run_payload = |input| {
	lines = input.split_on("\n").drop_if(|line| line == "")
	match lines {
		[] => fail("header", "empty input")
		[header, .. as cases] => {
			match parse_header(header, cases.len()) {
				Err(message) => fail("header", message)
				Ok({}) => match cases.fold(Ok(0), run_row) {
					Ok(count) => "PASS\tcase\t${count.to_str()}"
					Err(error) => fail(error.case_id, error.message)
				}
			}
		}
	}
}

parse_header : Str, U64 -> Try({}, Str)
parse_header = |header, actual_count| {
	match header.split_on("\t") {
		["ROC_UNICODE_TEST_V1", "case", count_str] => {
			expected = U64.from_str(count_str) ?? return Err("invalid header count")
			if expected == actual_count Ok({}) else Err("header count mismatch")
		}
		_ => Err("malformed protocol header")
	}
}

run_row : Try(U64, { case_id : Str, message : Str }), Str -> Try(U64, { case_id : Str, message : Str })
run_row = |state, line| {
	count = state?
	run_case(line)?
	Ok(count + 1)
}

run_case : Str -> Try({}, { case_id : Str, message : Str })
run_case = |line| {
	match line.split_on("\t") {
		[case_id, operation, profile, source_text, maps_text, contextual_text] => {
			source = parse_scalars(source_text) ?? return case_error(case_id, "malformed source scalar list")
			maps = parse_maps(maps_text) ?? return case_error(case_id, "malformed expected mapping list")
			contextual = parse_contextual(contextual_text) ?? return case_error(case_id, "malformed contextual list")
			if source.len() != maps.len() or source.len() != contextual.len() {
				return case_error(case_id, "fact expectation count does not match source")
			}
			source_parts = keep_oks(source.map(scalar_to_str)) ?? return case_error(case_id, "invalid source scalar")
			source_str = Str.join_with(source_parts, "")
			match invoke(operation, profile, source_str) {
				Err(message) => case_error(case_id, message)
				Ok(result) => verify_result(operation, profile, source, maps, contextual, result)
					|> Try.map_err(|message| { case_id, message })
			}
		}
		[case_id, ..] => case_error(case_id, "malformed case row")
		_ => case_error("unknown", "malformed case row")
	}
}

case_error : Str, Str -> Try(a, { case_id : Str, message : Str })
case_error = |case_id, message| Err({ case_id, message })

invoke : Str, Str, Str -> Try(Case.Result, Str)
invoke = |operation, profile, source| {
	limits = unlimited_limits
	match operation {
		"lower" => match mapping_profile(profile) {
			Err(message) => Err(message)
			Ok(value) => Case.to_lower(source, value, limits) |> Try.map_err(error_message)
		}
		"upper" => match mapping_profile(profile) {
			Err(message) => Err(message)
			Ok(value) => Case.to_upper(source, value, limits) |> Try.map_err(error_message)
		}
		"title" => match mapping_profile(profile) {
			Err(message) => Err(message)
			Ok(value) => Case.to_title(source, value, limits) |> Try.map_err(error_message)
		}
		"fold" => match fold_profile(profile) {
			Err(message) => Err(message)
			Ok(value) => Case.fold(source, value, limits) |> Try.map_err(error_message)
		}
		_ => Err("unknown operation")
	}
}

mapping_profile : Str -> Try(Case.MappingProfile, Str)
mapping_profile = |profile| match profile {
	"default" => Ok(Case.unicode_default)
	"turkic" => Ok(Case.turkic)
	"lithuanian" => Ok(Case.lithuanian)
	_ => Err("unknown mapping profile")
}

fold_profile : Str -> Try(Case.FoldProfile, Str)
fold_profile = |profile| match profile {
	"full" => Ok(Case.full)
	"simple" => Ok(Case.simple)
	"turkic-full" => Ok(Case.turkic_full)
	"turkic-simple" => Ok(Case.turkic_simple)
	_ => Err("unknown fold profile")
}

verify_result : Str, Str, List(U32), List(List(U32)), List(Bool), Case.Result -> Try({}, Str)
verify_result = |operation, profile, source, maps, contextual, result| {
	expected_parts = (keep_oks(maps.map(mapping_to_str)) |> Try.map_err(|_| "invalid expected mapping scalar"))?
	expected_text = Str.join_with(expected_parts, "")
	if Case.result_text(result) != expected_text {
		return Err("transformed text differs from independent oracle")
	}
	verify_provenance(operation, profile, result)?
	verify_facts(Case.result_facts(result), source, maps, contextual, 0, 0, 0, 0, Case.result_text(result))
}

verify_provenance : Str, Str, Case.Result -> Try({}, Str)
verify_provenance = |operation, profile, result| {
	if !UnicodeVersion.is_eq(Case.result_unicode_version(result), UnicodeVersion.current) {
		return Err("result Unicode version is not current")
	}
	match (operation, profile, Case.result_operation(result), Case.result_profile(result), Case.result_profile_revision(result)) {
		("lower", "default", Lower, MappingUnicodeDefault, NoProfileRevision) => Ok({})
		("upper", "default", Upper, MappingUnicodeDefault, NoProfileRevision) => Ok({})
		("title", "default", Title, MappingUnicodeDefault, NoProfileRevision) => Ok({})
		("lower", "turkic", Lower, MappingTurkic, MappingTurkicV1) => Ok({})
		("upper", "turkic", Upper, MappingTurkic, MappingTurkicV1) => Ok({})
		("title", "turkic", Title, MappingTurkic, MappingTurkicV1) => Ok({})
		("lower", "lithuanian", Lower, MappingLithuanian, MappingLithuanianV1) => Ok({})
		("upper", "lithuanian", Upper, MappingLithuanian, MappingLithuanianV1) => Ok({})
		("title", "lithuanian", Title, MappingLithuanian, MappingLithuanianV1) => Ok({})
		("fold", "full", Fold, FoldFull, NoProfileRevision) => Ok({})
		("fold", "simple", Fold, FoldSimple, FoldSimpleV1) => Ok({})
		("fold", "turkic-full", Fold, FoldTurkicFull, FoldTurkicV1) => Ok({})
		("fold", "turkic-simple", Fold, FoldTurkicSimple, FoldTurkicV1) => Ok({})
		_ => Err("result operation/profile provenance mismatch")
	}
}

verify_facts : List(Case.Fact), List(U32), List(List(U32)), List(Bool), U64, U64, U64, U64, Str -> Try({}, Str)
verify_facts = |facts, source, maps, contextual, input_byte, output_byte, input_scalar, output_scalar, text| {
	match (facts, source, maps, contextual) {
		([], [], [], []) => Ok({})
		([fact, .. as rest_facts], [scalar, .. as rest_source], [mapping, .. as rest_maps], [is_contextual, .. as rest_contextual]) => {
			input_text = (scalar_to_str(scalar) |> Try.map_err(|_| "invalid input scalar"))?
			output_text = (mapping_to_str(mapping) |> Try.map_err(|_| "invalid output scalar"))?
			input_end = input_byte + input_text.count_utf8_bytes()
			output_end = output_byte + output_text.count_utf8_bytes()
			input_range = TextRange.byte_range(Case.fact_input(fact))
			input_scalars = TextRange.scalar_range(Case.fact_input(fact))
			output_range = TextRange.byte_range(Case.fact_output(fact))
			output_scalars = TextRange.scalar_range(Case.fact_output(fact))
			if ByteRange.start(input_range) != input_byte or ByteRange.end(input_range) != input_end {
				return Err("input byte ranges do not partition source")
			}
			if ScalarRange.start(input_scalars) != input_scalar or ScalarRange.end(input_scalars) != input_scalar + 1 {
				return Err("input scalar ranges are not one fact per scalar")
			}
			if ByteRange.start(output_range) != output_byte or ByteRange.end(output_range) != output_end {
				return Err("output byte ranges do not partition result")
			}
			if ScalarRange.start(output_scalars) != output_scalar or ScalarRange.end(output_scalars) != output_scalar + mapping.len() {
				return Err("output scalar ranges do not reconstruct result")
			}
			selected = ByteRange.slice(output_range, text) ?? return Err("output fact does not select a scalar-aligned slice")
			if selected != output_text {
				return Err("output fact selects the wrong transformed text")
			}
			if !shape_matches(scalar, mapping, Case.fact_shape(fact)) or Case.fact_contextual(fact) != is_contextual {
				return Err("mapping fact shape or contextual provenance differs")
			}
			verify_facts(rest_facts, rest_source, rest_maps, rest_contextual, input_end, output_end, input_scalar + 1, output_scalar + mapping.len(), text)
		}
		_ => Err("result does not contain exactly one fact per source scalar")
	}
}

shape_matches : U32, List(U32), [Unchanged, Simple, Expanded, Removed] -> Bool
shape_matches = |input, mapping, shape| match (mapping, shape) {
	([], Removed) => Bool.True
	([output], Unchanged) if output == input => Bool.True
	([_], Simple) => Bool.True
	([_, _, ..], Expanded) => Bool.True
	_ => Bool.False
}

parse_scalars : Str -> Try(List(U32), [BadNumStr])
parse_scalars = |text| {
	if text == "_" {
		Ok([])
	} else {
		keep_oks(text.split_on(",").map(|item| U32.from_str("0x${item}")))
	}
}

parse_maps : Str -> Try(List(List(U32)), [BadNumStr])
parse_maps = |text| {
	if text == "_" {
		Ok([])
	} else {
		keep_oks(text.split_on("/").map(parse_scalars))
	}
}

parse_contextual : Str -> Try(List(Bool), {})
parse_contextual = |text| {
	if text == "_" {
		Ok([])
	} else {
		keep_oks(
			text.split_on("/").map(
				|item| match item {
					"0" => Ok(Bool.False)
					"1" => Ok(Bool.True)
					_ => Err({})
				},
			),
		)
	}
}

mapping_to_str : List(U32) -> Try(Str, [InvalidScalar, InternalEncodingFault])
mapping_to_str = |mapping| {
	parts = keep_oks(mapping.map(scalar_to_str))?
	Ok(Str.join_with(parts, ""))
}

scalar_to_str : U32 -> Try(Str, [InvalidScalar, InternalEncodingFault])
scalar_to_str = |value| {
	match Scalar.from_u32(value) {
		Err(_) => Err(InvalidScalar)
		Ok(scalar) => Scalar.to_str(scalar) |> Try.map_err(|_| InternalEncodingFault)
	}
}

keep_oks : List(Try(a, err)) -> Try(List(a), err)
keep_oks = |items| items.fold(
	Ok([]),
	|state, item| {
		values = state?
		value = item?
		Ok(values.append(value))
	},
)

unlimited_limits : Case.Limits
unlimited_limits = Case.unlimited_limits

run_focused : U64 -> Try({}, Str)
run_focused = |runtime_seed| {
	# Exact limits are successful; reducing each relevant bound by one reports
	# the typed resource and cannot expose a partial Case.Result.
	zero = runtime_seed - runtime_seed
	max = U64.highest
	assert_lower_success("empty exact zero", "", Case.unicode_default, Case.limits(zero, zero, zero, zero, zero), "", zero)?
	assert_lower_success("ASCII exact", "A", Case.unicode_default, Case.limits(1, 1, 1, 1, 1), "a", 1)?
	assert_lower_limit("ASCII input bytes", "A", Case.unicode_default, Case.limits(zero, max, max, max, max), InputBytes)?
	assert_lower_limit("ASCII input scalars", "A", Case.unicode_default, Case.limits(max, zero, max, max, max), InputScalars)?
	assert_lower_limit("ASCII output bytes", "A", Case.unicode_default, Case.limits(max, max, zero, max, max), OutputBytes)?
	assert_lower_limit("ASCII output scalars", "A", Case.unicode_default, Case.limits(max, max, max, zero, max), OutputScalars)?
	assert_lower_limit("ASCII facts", "A", Case.unicode_default, Case.limits(max, max, max, max, zero), Facts)?

	# U+0130 is a two-byte input whose full default lowercase expands to three
	# bytes and two scalars; each resource succeeds exactly at the boundary.
	assert_lower_success("multibyte exact", "İ", Case.unicode_default, Case.limits(2, 1, 3, 2, 1), "i̇", 1)?
	assert_lower_limit("multibyte input bytes", "İ", Case.unicode_default, Case.limits(1, max, max, max, max), InputBytes)?
	assert_lower_limit("multibyte output bytes", "İ", Case.unicode_default, Case.limits(max, max, 2, max, max), OutputBytes)?
	assert_lower_limit("multibyte output scalars", "İ", Case.unicode_default, Case.limits(max, max, max, 1, max), OutputScalars)?

	# U+00DF uppercases to two ASCII scalars while retaining one source fact.
	assert_upper_success("sharp s expansion exact", "ß", Case.unicode_default, Case.limits(2, 1, 2, 2, 1), "SS", 1)?
	assert_upper_limit("sharp s output bytes", "ß", Case.limits(max, max, 1, max, max), OutputBytes)?
	assert_upper_limit("sharp s output scalars", "ß", Case.limits(max, max, max, 1, max), OutputScalars)?

	# In Turkic casing the dot's fact has an empty output range, while the
	# complete result still has exact output and fact limits.
	assert_lower_success("Turkic deletion exact", "İ", Case.turkic, Case.limits(3, 2, 1, 1, 2), "i", 2)?
	assert_lower_limit("Turkic deletion facts", "İ", Case.turkic, Case.limits(max, max, max, max, 1), Facts)?
	Ok({})
}

assert_lower_success : Str, Str, Case.MappingProfile, Case.Limits, Str, U64 -> Try({}, Str)
assert_lower_success = |label, source, profile, limits, expected, expected_facts| {
	match Case.to_lower(source, profile, limits) {
		Ok(result) if Case.result_text(result) == expected and Case.result_facts(result).len() == expected_facts => Ok({})
		Ok(_) => Err("${label}: exact limit success returned wrong result")
		Err(error) => Err("${label}: exact limit unexpectedly failed ${Str.inspect(error)}")
	}
}

assert_upper_success : Str, Str, Case.MappingProfile, Case.Limits, Str, U64 -> Try({}, Str)
assert_upper_success = |label, source, profile, limits, expected, expected_facts| {
	match Case.to_upper(source, profile, limits) {
		Ok(result) if Case.result_text(result) == expected and Case.result_facts(result).len() == expected_facts => Ok({})
		Ok(_) => Err("${label}: exact limit success returned wrong result")
		Err(error) => Err("${label}: exact limit unexpectedly failed ${Str.inspect(error)}")
	}
}

assert_lower_limit : Str, Str, Case.MappingProfile, Case.Limits, [NoLimitResource, InputBytes, InputScalars, OutputBytes, OutputScalars, Facts] -> Try({}, Str)
assert_lower_limit = |label, source, profile, limits, expected| {
	match Case.to_lower(source, profile, limits) {
		Err(error) if Case.error_limit_resource(error) == expected => Ok({})
		Err(error) => Err("${label}: wrong limit failure ${Str.inspect(error)}")
		Ok(_) => Err("${label}: limit returned a partial/success result")
	}
}

assert_upper_limit : Str, Str, Case.Limits, [NoLimitResource, InputBytes, InputScalars, OutputBytes, OutputScalars, Facts] -> Try({}, Str)
assert_upper_limit = |label, source, limits, expected| {
	match Case.to_upper(source, Case.unicode_default, limits) {
		Err(error) if Case.error_limit_resource(error) == expected => Ok({})
		Err(error) => Err("${label}: wrong limit failure ${Str.inspect(error)}")
		Ok(_) => Err("${label}: limit returned a partial/success result")
	}
}

error_message : Case.Error -> Str
error_message = |error| "Case call failed: ${Str.inspect(error)}"

fail : Str, Str -> Str
fail = |case_id, message| "FAIL\t${case_id}\t${message.replace_each("\t", " ").replace_each("\n", " ")}"
