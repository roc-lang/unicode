import ByteRange
import ScalarRange
import TextPosition

## The same half-open source span in UTF-8 byte and decoded-scalar coordinates.
##
## This value does not retain the source. Algorithms that return a `TextRange`
## guarantee that both component ranges describe the same scalar-aligned span.
TextRange :: { byte_range : ByteRange, scalar_range : ScalarRange }.{
    BoundsError : [ByteEndBeforeStart, ScalarEndBeforeStart]

    from_ranges : ByteRange, ScalarRange -> TextRange
    from_ranges = |byte_range, scalar_range| { byte_range, scalar_range }

    from_positions : TextPosition, TextPosition -> Try(TextRange, BoundsError)
    from_positions = |start, end| {
        byte_range = match ByteRange.from_bounds(TextPosition.byte_offset(start), TextPosition.byte_offset(end)) {
            Err(_) => return Err(ByteEndBeforeStart)
            Ok(range) => range
        }
        scalar_range = match ScalarRange.from_bounds(TextPosition.scalar_offset(start), TextPosition.scalar_offset(end)) {
            Err(_) => return Err(ScalarEndBeforeStart)
            Ok(range) => range
        }
        Ok({ byte_range, scalar_range })
    }

    start : TextRange -> TextPosition
    start = |range| TextPosition.from_offsets(
        ByteRange.start(range.byte_range),
        ScalarRange.start(range.scalar_range),
    )

    end : TextRange -> TextPosition
    end = |range| TextPosition.from_offsets(
        ByteRange.end(range.byte_range),
        ScalarRange.end(range.scalar_range),
    )

    byte_range : TextRange -> ByteRange
    byte_range = |range| range.byte_range

    scalar_range : TextRange -> ScalarRange
    scalar_range = |range| range.scalar_range

    is_empty : TextRange -> Bool
    is_empty = |range| {
        ByteRange.is_empty(range.byte_range) and ScalarRange.is_empty(range.scalar_range)
    }

    is_eq : TextRange, TextRange -> Bool
    is_eq = |left, right| {
        ByteRange.is_eq(left.byte_range, right.byte_range) and ScalarRange.is_eq(left.scalar_range, right.scalar_range)
    }
}
