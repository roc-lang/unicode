import Grapheme as LegacyGrapheme

## Allocation-conscious Unicode text processing.
##
## This namespace is the strangler boundary around the package's legacy API.
## Its contracts are intended to remain stable while their implementations are
## replaced algorithm by algorithm.
Text :: [].{
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
        ## The returned string retains the source backing allocation and does
        ## not copy the selected bytes.
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

    ## Extended grapheme cluster boundaries and materializers.
    Grapheme :: [].{
        ## Visit half-open byte ranges in source order through `Iter`.
        ##
        ## This temporary wrapper obtains the legacy owned result eagerly
        ## before constructing the iterator. The Unicode 17 transition core
        ## will make traversal lazy without changing this API.
        iter_ranges : Str -> Iter(ByteRange)
        iter_ranges = |source| grapheme_ranges(source).iter()

        ## Collect half-open byte ranges in source order.
        ranges : Str -> List(ByteRange)
        ranges = |source| grapheme_ranges(source)

        ## Return seamless slices of the source, retaining its backing storage.
        slices : Str -> List(Str)
        slices = |source| grapheme_slices(source)

        ## Return independently materialized cluster strings.
        owned : Str -> List(Str)
        owned = |source| grapheme_owned(source)
    }
}

# This adapter is deliberately narrow. It is the only place the fresh API
# depends on the legacy grapheme result and disappears with the replacement
# transition core.
grapheme_owned : Str -> List(Str)
grapheme_owned = |source| {
    match LegacyGrapheme.split(source) {
        Err(_) => {
            # A Roc Str is valid UTF-8, so the fresh Str API has no error
            # channel for this legacy-only failure.
            ...
        }
        Ok(parts) => {
            # Never allow a legacy partition bug to become an invalid range in
            # the fresh API. This temporary check is intentionally outside the
            # eventual hot path.
            if Str.join_with(parts, "") == source {
                parts
            } else {
                ...
            }
        }
    }
}

grapheme_ranges : Str -> List(Text.ByteRange)
grapheme_ranges = |source| {
    initial : { end : U64, ranges : List(Text.ByteRange) }
    initial = { end: 0, ranges: [] }

    final = grapheme_owned(source).fold(
        initial,
        |state, part| {
            next_end = state.end + part.count_utf8_bytes()
            range = match Text.ByteRange.from_bounds(state.end, next_end) {
                Ok(valid_range) => valid_range
                Err(_) => ...
            }

            {
                end: next_end,
                ranges: state.ranges.append(range),
            }
        },
    )

    final.ranges
}

grapheme_slices : Str -> List(Str)
grapheme_slices = |source| {
    grapheme_ranges(source).map(|range| {
        match Text.ByteRange.slice(range, source) {
            Ok(slice) => slice
            Err(_) => ...
        }
    })
}
