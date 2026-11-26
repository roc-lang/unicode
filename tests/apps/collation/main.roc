app [run!] {
	pf: platform "../../platform/main.roc",
	unicode: "../../../package/main.roc",
}

import unicode.Collation
import unicode.Locale
import unicode.Normalization
import unicode.Scalar

limits : Collation.Limits
limits = {
	normalization: { max_input_bytes: 1024, max_intermediate_scalars: 1024, max_output_bytes: 4096 },
	max_elements: 16384,
	max_numeric_digits: 0,
	max_key_bytes: 65536,
}

run! : Str => Str
run! = |input| match input.split_on("\n").drop_if(|line| line == "") {
	[header, .. as rows] => match header.split_on("\t") {
		["ROC_UNICODE_TEST_V1", suite, count] if count == rows.len().to_str() => {
			alternate = match suite {
				"collation-non-ignorable" => NonIgnorable
				"collation-shifted" => Shifted
				_ => return "FAIL\tcollation\tunknown suite"
			}
			options : Collation.Options
			options = { alternate, strength: Identical }
			collator = Collation.collator(Locale.root, options) ?? return "FAIL\tcollation\troot configuration"
			for row in rows {
				match row.split_on("\t") {
					[case_id, left, right] => match compare(collator, left, right) {
						Err(message) => return "FAIL\t${suite}\t${case_id}\t${message}\t${left}\t${right}"
						Ok({}) => {}
					}
					_ => return "FAIL\t${suite}\tinvalid row\t${row}"
				}
			}
			"PASS\t${suite}\t${count}"
		}
		_ => "FAIL\tcollation\tinvalid header"
	}
	_ => "FAIL\tcollation\tmissing header"
}

compare : Collation.Collator, Str, Str -> Try({}, Str)
compare = |collator, left_text, right_text| {
	left = decode(left_text)?
	right = decode(right_text)?
	left_nfd = Normalization.normalize(left, NFD, limits.normalization).map_err(|error| Str.inspect(error))?
	right_nfd = Normalization.normalize(right, NFD, limits.normalization).map_err(|error| Str.inspect(error))?
	expected = if left_nfd == right_nfd {
		Eq
	} else {
		Lt
	}
	match Collation.compare(collator, left, right, limits) {
		Ok(order) if order == expected => Ok({})
		Ok(order) => Err("adjacent official records require ${Str.inspect(expected)}, got ${Str.inspect(order)}")
		Err(error) => Err(Str.inspect(error))
	}
}

decode : Str -> Try(Str, Str)
decode = |input| {
	var $bytes = []
	for field in input.split_on(",") {
		code = U32.from_str(field).map_err(|_| "bad code point")?
		scalar = Scalar.from_u32(code).map_err(|_| "not a scalar")?
		$bytes = Scalar.append_utf8($bytes, scalar, limits.normalization.max_input_bytes).map_err(|_| "fixture exceeds byte limit")?
	}
	Str.from_utf8($bytes).map_err(|_| "fixture encoding fault")
}
