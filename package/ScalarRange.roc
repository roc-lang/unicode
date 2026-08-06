## A half-open range in the decoded-scalar sequence of a logical text source.
ScalarRange :: { start : U64, end : U64 }.{
	BoundsError : [EndBeforeStart]

	from_bounds : U64, U64 -> Try(ScalarRange, BoundsError)
	from_bounds = |start, end| {
		if start <= end {
			Ok({ start, end })
		} else {
			Err(EndBeforeStart)
		}
	}

	start : ScalarRange -> U64
	start = |range| range.start

	end : ScalarRange -> U64
	end = |range| range.end

	len : ScalarRange -> U64
	len = |range| range.end - range.start

	is_empty : ScalarRange -> Bool
	is_empty = |range| range.start == range.end

	is_eq : ScalarRange, ScalarRange -> Bool
	is_eq = |left, right| left.start == right.start and left.end == right.end
}
