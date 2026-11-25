app [run!] {
	pf: platform "../platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.Numeric
import unicode.Property
import unicode.Scalar

run! = |input| input

expect {
	# Independent rows selected from DerivedCoreProperties.txt and PropList.txt
	# cover the distinctions that matter to identifier and lexer callers.
	# The columns are Alphabetic, XID_Start, XID_Continue, White_Space,
	# Pattern_Syntax and Pattern_White_Space.
	cases : List((U32, (Bool, Bool, Bool, Bool, Bool, Bool)))
	cases = [
		(0x0041, (Bool.True, Bool.True, Bool.True, Bool.False, Bool.False, Bool.False)),
		(0x0030, (Bool.False, Bool.False, Bool.True, Bool.False, Bool.False, Bool.False)),
		(0x005F, (Bool.False, Bool.False, Bool.True, Bool.False, Bool.False, Bool.False)),
		(0x0021, (Bool.False, Bool.False, Bool.False, Bool.False, Bool.True, Bool.False)),
		(0x0020, (Bool.False, Bool.False, Bool.False, Bool.True, Bool.False, Bool.True)),
		(0x00A0, (Bool.False, Bool.False, Bool.False, Bool.True, Bool.False, Bool.False)),
		(0x200E, (Bool.False, Bool.False, Bool.False, Bool.False, Bool.False, Bool.True)),
		(0x0345, (Bool.True, Bool.False, Bool.True, Bool.False, Bool.False, Bool.False)),
		(0x0378, (Bool.False, Bool.False, Bool.False, Bool.False, Bool.False, Bool.False)),
		(0x18E00, (Bool.True, Bool.True, Bool.True, Bool.False, Bool.False, Bool.False)),
		(0x10FFFF, (Bool.False, Bool.False, Bool.False, Bool.False, Bool.False, Bool.False)),
	]
	cases.all(
		|(code_point, expected)| {
			match Scalar.from_u32(code_point) {
				Ok(scalar) => (
					Property.is_alphabetic(scalar),
					Property.is_xid_start(scalar),
					Property.is_xid_continue(scalar),
					Property.is_white_space(scalar),
					Property.is_pattern_syntax(scalar),
					Property.is_pattern_white_space(scalar),
				) == expected
				Err(_) => Bool.False
			}
		},
	)
}

expect {
	# The exact rational column in DerivedNumericValues.txt is authoritative.
	# U+4EAC is derived from Unihan and its value exceeds the consecutive-integer
	# range of Float64. U+2153 exercises a non-terminating decimal fraction.
	cases : List((U32, Numeric.Type, [None, Some(Numeric.Rational)]))
	cases = [
		(0x0041, None, None),
		(0x0030, Decimal, Some({ numerator: 0, denominator: 1 })),
		(0x0669, Decimal, Some({ numerator: 9, denominator: 1 })),
		(0x00B2, Digit, Some({ numerator: 2, denominator: 1 })),
		(0x2153, Numeric, Some({ numerator: 1, denominator: 3 })),
		(0x0F33, Numeric, Some({ numerator: -1, denominator: 2 })),
		(0x4EAC, Numeric, Some({ numerator: 10000000000000000, denominator: 1 })),
		(0x10FFFF, None, None),
	]
	cases.all(
		|(code_point, kind, value)| {
			match Scalar.from_u32(code_point) {
				Ok(scalar) =>
					Numeric.type_of(scalar) == kind and
						Numeric.is_decimal_digit(scalar) == (kind == Decimal) and
							Numeric.value_of(scalar) == value
				Err(_) => Bool.False
			}
		},
	)
}

expect {
	kinds : List(Numeric.Type)
	kinds = [None, Decimal, Digit, Numeric]
	Numeric.type_property == { short: "nt", long: "Numeric_Type" } and
		Numeric.value_property == { short: "nv", long: "Numeric_Value" } and
			Numeric.parse("  IS-De_CiMal  ") == Ok(Decimal) and
				Numeric.parse("false") == Err(UnknownAlias) and
					kinds.all(
						|kind| {
							Numeric.parse(Numeric.short(kind)) == Ok(kind) and
								Numeric.parse(Numeric.long(kind)) == Ok(kind) and
									Numeric.alias_count(kind) == (if (kind == None) 1 else 2) and
										Numeric.alias_at(kind, Numeric.alias_count(kind)) == None
						},
					)
}
