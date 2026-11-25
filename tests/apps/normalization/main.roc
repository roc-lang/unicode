app [run!] {
	pf: platform "../../platform/main.roc",
	unicode: "../../../package/main.roc",
}

import unicode.Case
import unicode.Normalization
import unicode.Scalar

limits : Normalization.Limits
limits = { max_input_bytes: 4096, max_intermediate_scalars: 4096, max_output_bytes: 4096 }

run! : Str => Str
run! = |input| {
	match input.split_on("\n").drop_if(|line| line == "") {
		[header, .. as rows] => {
			count = rows.len().to_str()
			if header != "ROC_UNICODE_TEST_V1\tnormalization\t${count}" {
				return "FAIL\tnormalization\theader"
			}
			for row in rows {
				match check_row(row) {
					Ok({}) => {}
					Err(message) => return "FAIL\tnormalization\t${message}\t${row}"
				}
			}
			"PASS\tnormalization\t${count}"
		}
		_ => "FAIL\tnormalization\theader"
	}
}

check_row : Str -> Try({}, Str)
check_row = |row| {
	match row.split_on("\t") {
		["Identity", field] => {
			source = decode(field)?
			for form in [NFC, NFD, NFKC, NFKD] {
				check(source, source, form)?
			}
			Ok({})
		}
		[_id, first, second, third, fourth, fifth] => {
			c1 = decode(first)?
			c2 = decode(second)?
			c3 = decode(third)?
			c4 = decode(fourth)?
			c5 = decode(fifth)?
			for input in [c1, c2, c3] {
				check(input, c2, NFC)?
				check(input, c3, NFD)?
				check(input, c4, NFKC)?
				check(input, c5, NFKD)?
			}
			for input in [c4, c5] {
				check(input, c4, NFC)?
				check(input, c5, NFD)?
				check(input, c4, NFKC)?
				check(input, c5, NFKD)?
			}
			Ok({})
		}
		_ => Err("invalid conformance row")
	}
}

check : Str, Str, Normalization.Form -> Try({}, Str)
check = |source, expected, form| {
	normalized = Normalization.normalize(source, form, limits).map_err(|error| Str.inspect(error))?
	if normalized != expected {
		return Err("${Str.inspect(form)} unexpected output ${Str.inspect(normalized)}")
	}
	correct = match Normalization.quick_check(source, form) {
		Yes => source == expected
		No => source != expected
		Maybe => Normalization.is_normalized(source, form, limits) == Ok(source == expected)
	}
	if correct {
		Ok({})
	} else {
		Err("${Str.inspect(form)} incorrect quick check")
	}
}

decode : Str -> Try(Str, Str)
decode = |field| {
	var $bytes = []
	for decimal in field.split_on(",") {
		code = U32.from_str(decimal).map_err(|_| "invalid code point")?
		scalar = Scalar.from_u32(code).map_err(|_| "invalid Unicode scalar")?
		$bytes = Scalar.append_utf8($bytes, scalar, limits.max_input_bytes).map_err(|_| "fixture field too long")?
	}
	Str.from_utf8($bytes).map_err(|_| "fixture encoding failed")
}

expect {
	Normalization.normalize("A\u(030A)\u(0301)", NFC, limits) == Ok("\u(01FA)") and
		Normalization.normalize("\u(01FA)", NFD, limits) == Ok("A\u(030A)\u(0301)") and
			Normalization.normalize("\u(AC01)", NFD, limits) == Ok("\u(1100)\u(1161)\u(11A8)") and
				Normalization.normalize("\u(1100)\u(1161)\u(11A8)", NFC, limits) == Ok("\u(AC01)")
}

expect {
	Normalization.normalize("\u(FB03)", NFKC, limits) == Ok("ffi") and
		Normalization.nfkc_case_fold("Ｓtraße\u(00AD)", limits) == Ok("strasse") and
			Normalization.canonically_equivalent("A\u(030A)", "Å", limits) == Ok(Bool.True) and
				Normalization.canonically_case_insensitive_equal("A\u(030A)", "å", limits, Case.unlimited_limits) == Ok(Bool.True)
}

expect {
	Normalization.normalize("a\u(0315)\u(0300)\u(0301)", NFD, limits) == Ok("a\u(0300)\u(0301)\u(0315)") and
		Normalization.quick_check("a\u(0315)\u(0300)", NFD) == No and
			Normalization.quick_check("a\u(0300)", NFC) == Maybe and
				Normalization.is_normalized("a\u(0300)", NFC, limits) == Ok(Bool.False)
}

expect {
	exact = { max_input_bytes: 2.U64, max_intermediate_scalars: 2.U64, max_output_bytes: 3.U64 }
	zero = { max_input_bytes: 0.U64, max_intermediate_scalars: 0.U64, max_output_bytes: 0.U64 }
	Normalization.normalize("", NFD, zero) == Ok("") and
		Normalization.normalize("Å", NFD, exact) == Ok("A\u(030A)") and
			Normalization.normalize("Å", NFD, { ..exact, max_input_bytes: 1 }) == Err(LimitExceeded({ resource: InputBytes, limit: 1, required: 2 })) and
				Normalization.normalize("Å", NFD, { ..exact, max_intermediate_scalars: 1 }) == Err(LimitExceeded({ resource: IntermediateScalars, limit: 1, required: 2 })) and
					Normalization.normalize("Å", NFD, { ..exact, max_output_bytes: 2 }) == Err(LimitExceeded({ resource: OutputBytes, limit: 2, required: 3 }))
}
