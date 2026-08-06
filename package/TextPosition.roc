## One Unicode-scalar boundary in a logical text source.
##
## Both offsets are absolute from the beginning of that source. `byte_offset`
## counts UTF-8 bytes and `scalar_offset` counts decoded Unicode scalars.
TextPosition :: { byte_offset : U64, scalar_offset : U64 }.{
	from_offsets : U64, U64 -> TextPosition
	from_offsets = |byte_offset, scalar_offset| { byte_offset, scalar_offset }

	byte_offset : TextPosition -> U64
	byte_offset = |position| position.byte_offset

	scalar_offset : TextPosition -> U64
	scalar_offset = |position| position.scalar_offset

	is_eq : TextPosition, TextPosition -> Bool
	is_eq = |left, right| {
		left.byte_offset == right.byte_offset and left.scalar_offset == right.scalar_offset
	}
}
