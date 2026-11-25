app [run!] {
	pf: platform "../platform/main.roc",
	unicode: "../../package/main.roc",
}

import unicode.ByteRange
import unicode.Scalar
import unicode.Utf8

run! : Str => Str
run! = |_| ""

Observed : { count : U64, matches : Bool }

empty : Observed
empty = { count: 0, matches: Bool.True }

inspect_scalar : Observed, Scalar.LocatedScalar, U32, U64 -> Observed
inspect_scalar = |state, located, value, width| {
	{
		count: state.count + 1,
		matches: state.matches and
			Scalar.to_u32(located.scalar) == value and
				ByteRange.start(located.byte_range) == 0 and
					ByteRange.end(located.byte_range) == width and
						located.scalar_index == 0,
	}
}

round_trips : U32 -> Bool
round_trips = |value| {
	scalar = Scalar.from_u32(value) ?? return Bool.False
	bytes = Scalar.to_utf8(scalar)
	width = bytes.len()
	# The built-in validator independently verifies the emitted UTF-8 form.
	text = Str.from_utf8(bytes) ?? return Bool.False
	if Scalar.to_str(scalar) != Ok(text) or Scalar.utf8_len(scalar).to_u64() != width {
		return Bool.False
	}

	expected = { count: 1.U64, matches: Bool.True }
	scanned = Scalar.iter(text).fold(empty, |state, located| inspect_scalar(state, located, value, width))
	if scanned != expected {
		return Bool.False
	}

	result = Utf8.Cursor.push(
		Utf8.Cursor.init({}),
		bytes,
		empty,
		|state, located| Continue(inspect_scalar(state, located, value, width)),
	)
	match result {
		Pushed({ cursor, state, consumed }) if state == expected and consumed == width =>
			match Utf8.Cursor.finish(cursor) {
				End({ byte_offset, scalar_count, .. }) => byte_offset == width and scalar_count == 1
				Failed(_) => Bool.False
			}
		_ => Bool.False
	}
}

first_failure : U32, U32 -> [Some(U32), None]
first_failure = |start, end| {
	var $value = start
	while $value <= end {
		if !round_trips($value) {
			return Some($value)
		}
		$value = $value + 1
	}
	None
}

# These two intervals cover every valid scalar. traversal.roc separately
# checks every surrogate and the exact encodings around all width boundaries.
expect {
	lowest_failing_scalar = first_failure(0x0000, 0xD7FF)
	lowest_failing_scalar == None
}

expect {
	lowest_failing_scalar = first_failure(0xE000, 0x10FFFF)
	lowest_failing_scalar == None
}
