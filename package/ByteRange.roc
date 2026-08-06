## A half-open range of UTF-8 bytes in an original text source.
ByteRange :: { start : U64, end : U64 }.{
	BoundsError : [EndBeforeStart]
	SliceError : [NotScalarBoundary, OutOfBounds]

	## Construct `[start, end)`. Empty ranges are valid.
	from_bounds : U64, U64 -> Try(ByteRange, BoundsError)
	from_bounds = |start, end| {
		if start <= end {
			Ok({ start, end })
		} else {
			Err(EndBeforeStart)
		}
	}

	start : ByteRange -> U64
	start = |range| range.start

	end : ByteRange -> U64
	end = |range| range.end

	len : ByteRange -> U64
	len = |range| range.end - range.start

	is_empty : ByteRange -> Bool
	is_empty = |range| range.start == range.end

	is_eq : ByteRange, ByteRange -> Bool
	is_eq = |left, right| {
		left.start == right.start and left.end == right.end
	}

	## Select this range as a seamless slice of `source`.
	##
	## The returned string retains the source backing allocation and does not
	## copy the selected bytes.
	slice : ByteRange, Str -> Try(Str, SliceError)
	slice = |range, source| {
		source_len = source.count_utf8_bytes()

		if range.end > source_len {
			Err(OutOfBounds)
		} else {
			match source.drop_first_bytes(range.start) {
				Err(_) => Err(NotScalarBoundary)
				Ok(after_start) => {
					match after_start.drop_last_bytes(source_len - range.end) {
						Err(_) => Err(NotScalarBoundary)
						Ok(selected) => Ok(selected)
					}
				}
			}
		}
	}
}
