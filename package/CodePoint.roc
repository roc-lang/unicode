## A Unicode code point in `U+0000..U+10FFFF`.
##
## This domain includes the surrogate range `U+D800..U+DFFF`. Surrogates are
## code points, but they are not Unicode scalar values and cannot be encoded as
## UTF-8. Text processing and UTF-8 APIs therefore use `Scalar` instead.
CodePoint :: { value : U32 }.{

	## Construct a code point after checking the upper Unicode bound.
	##
	## This is constant time, does not allocate, and is total for every `U32`.
	from_u32 : U32 -> Try(CodePoint, [InvalidCodePoint])
	from_u32 = |value| {
		if value <= 0x10FFFF {
			Ok({ value: value })
		} else {
			Err(InvalidCodePoint)
		}
	}

	## Return the numeric Unicode code point value.
	##
	## This is constant time and does not allocate.
	to_u32 : CodePoint -> U32
	to_u32 = |{ value }| value

	## Whether this code point is a Unicode scalar value. This is constant time
	## and does not allocate.
	is_scalar : CodePoint -> Bool
	is_scalar = |code_point| !CodePoint.is_surrogate(code_point)

	## Whether this code point is in `U+D800..U+DFFF`. This is constant time and
	## does not allocate.
	is_surrogate : CodePoint -> Bool
	is_surrogate = |code_point| {
		value = CodePoint.to_u32(code_point)
		value >= 0xD800 and value <= 0xDFFF
	}

	## Whether this code point is a high surrogate in `U+D800..U+DBFF`. This is
	## constant time and does not allocate.
	is_high_surrogate : CodePoint -> Bool
	is_high_surrogate = |code_point| {
		value = CodePoint.to_u32(code_point)
		value >= 0xD800 and value <= 0xDBFF
	}

	## Whether this code point is a low surrogate in `U+DC00..U+DFFF`. This is
	## constant time and does not allocate.
	is_low_surrogate : CodePoint -> Bool
	is_low_surrogate = |code_point| {
		value = CodePoint.to_u32(code_point)
		value >= 0xDC00 and value <= 0xDFFF
	}

	## Compare two code points. This is constant time and does not allocate.
	is_eq : CodePoint, CodePoint -> Bool
	is_eq = |left, right| CodePoint.to_u32(left) == CodePoint.to_u32(right)
}
