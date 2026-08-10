import fuzz.Fuzz
import unicode.Scalar

FuzzSupport := [].{

	## Stable fuzz-only encoding for valid Unicode scalar sequences.
	##
	## Every three little-endian entropy bytes select one of the 1,112,064
	## Unicode scalar values. Ranks above U+D7FF skip the surrogate range. A
	## trailing group is zero-padded, so every input byte contributes to a value.
	scalar_sequence : Fuzz.Generator(List(U32))
	scalar_sequence = Fuzz.map(Fuzz.raw_bytes, scalar_sequence_from_bytes)

	scalar_sequence_from_bytes : List(U8) -> List(U32)
	scalar_sequence_from_bytes = |bytes| {
		var $remaining = bytes
		var $scalars = []
		while !$remaining.is_empty() {
			(b0, b1, b2, rest) = match $remaining {
				[first, second, third, .. as tail] => (first, second, third, tail)
				[first, second] => (first, second, 0, [])
				[first] => (first, 0, 0, [])
				[] => (0, 0, 0, [])
			}
			raw = b0.to_u32().bitwise_or(b1.to_u32().shl_wrap(8)).bitwise_or(b2.to_u32().shl_wrap(16))
			rank = raw % 0x10F800
			code_point = if rank <= 0xD7FF rank else rank + 0x800
			$scalars = $scalars.append(code_point)
			$remaining = rest
		}
		$scalars
	}

	source_parts : List(U32) -> List(Str)
	source_parts = |code_points| {
		code_points.map(
			|code_point| {
				scalar = Scalar.from_u32(code_point) ?? crash "fuzz scalar encoding admitted an invalid scalar"
				Scalar.to_str(scalar) ?? crash "fuzz scalar encoding could not produce UTF-8"
			},
		)
	}

	source : List(U32) -> Str
	source = |code_points| Str.join_with(source_parts(code_points), "")

	show_scalars : List(U32) -> Str
	show_scalars = |code_points| {
		labels = code_points.map(|code_point| "U+${hex_scalar(code_point)}")
		"code_points=[${Str.join_with(labels, ", ")}] text=${Str.inspect(source(code_points))}"
	}

	show_bytes : List(U8) -> Str
	show_bytes = |bytes| {
		hex = bytes.map(|byte| pad_hex(byte.to_u32(), 2))
		"bytes=${bytes.len().to_str()} hex=${Str.join_with(hex, " ")}"
	}

	hex_scalar : U32 -> Str
	hex_scalar = |value| pad_hex(value, 4)

	pad_hex : U32, U64 -> Str
	pad_hex = |value, minimum| {
		var $remaining = value
		var $text = ""
		while $remaining > 0 {
			digit = hex_digit($remaining % 16)
			$text = "${digit}${$text}"
			$remaining = $remaining / 16
		}
		if $text == "" {
			$text = "0"
		}
		while $text.count_utf8_bytes() < minimum {
			$text = "0${$text}"
		}
		$text
	}

	hex_digit : U32 -> Str
	hex_digit = |value| match value {
		0 => "0"
		1 => "1"
		2 => "2"
		3 => "3"
		4 => "4"
		5 => "5"
		6 => "6"
		7 => "7"
		8 => "8"
		9 => "9"
		10 => "A"
		11 => "B"
		12 => "C"
		13 => "D"
		14 => "E"
		15 => "F"
		_ => "?"
	}
}
