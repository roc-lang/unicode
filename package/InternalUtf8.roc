## Allocation-free traversal of the Unicode scalars in a valid Roc `Str`.
##
## This private view deliberately carries raw integers instead of public
## `Scalar` and `ByteRange` wrappers. Algorithm hot loops can therefore fuse
## decoding with their narrow property lookup and transition without building
## public records for every scalar.
InternalUtf8 :: [].{
    LocatedScalar : {
        scalar : U32,
        byte_start : U64,
        byte_end : U64,
        scalar_index : U64,
    }

    Cursor : {
        bytes : Iter(U8),
        byte_offset : U64,
        scalar_index : U64,
    }

    init : Str -> Cursor
    init = |source| {
        {
            bytes: source.iter_utf8(),
            byte_offset: 0,
            scalar_index: 0,
        }
    }

    ## Decode the next scalar from a valid `Str` byte iterator.
    ##
    ## Every byte shape is handled exhaustively. The replacement branches are
    ## defensive totality for an impossible broken-`Str` invariant; no branch
    ## panics, silently discards consumed bytes, or indexes past the iterator.
    next : Cursor -> [One({ item : LocatedScalar, rest : Cursor }), Done]
    next = |cursor| {
        match next_byte(cursor.bytes) {
            End => Done
            Byte({ value: first, rest: after_first }) => {
                (scalar, rest, width) = if first < 0x80 {
                    (first.to_u32(), after_first, 1.U64)
                } else if first < 0xE0 {
                    match next_byte(after_first) {
                        End => (0xFFFD, after_first, 1)
                        Byte({ value: second, rest: after_second }) => {
                            value = first.bitwise_and(0x1F).to_u32()
                                .shl_wrap(6)
                                .bitwise_or(second.bitwise_and(0x3F).to_u32())
                            (value, after_second, 2)
                        }
                    }
                } else if first < 0xF0 {
                    match next_byte(after_first) {
                        End => (0xFFFD, after_first, 1)
                        Byte({ value: second, rest: after_second }) => {
                            match next_byte(after_second) {
                                End => (0xFFFD, after_second, 2)
                                Byte({ value: third, rest: after_third }) => {
                                    value = first.bitwise_and(0x0F).to_u32()
                                        .shl_wrap(6)
                                        .bitwise_or(second.bitwise_and(0x3F).to_u32())
                                        .shl_wrap(6)
                                        .bitwise_or(third.bitwise_and(0x3F).to_u32())
                                    (value, after_third, 3)
                                }
                            }
                        }
                    }
                } else {
                    match next_byte(after_first) {
                        End => (0xFFFD, after_first, 1)
                        Byte({ value: second, rest: after_second }) => {
                            match next_byte(after_second) {
                                End => (0xFFFD, after_second, 2)
                                Byte({ value: third, rest: after_third }) => {
                                    match next_byte(after_third) {
                                        End => (0xFFFD, after_third, 3)
                                        Byte({ value: fourth, rest: after_fourth }) => {
                                            value = first.bitwise_and(0x07).to_u32()
                                                .shl_wrap(6)
                                                .bitwise_or(second.bitwise_and(0x3F).to_u32())
                                                .shl_wrap(6)
                                                .bitwise_or(third.bitwise_and(0x3F).to_u32())
                                                .shl_wrap(6)
                                                .bitwise_or(fourth.bitwise_and(0x3F).to_u32())
                                            (value, after_fourth, 4)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                # A Roc `Str` length bounds both counters. The fallback values
                # make even an impossible counter invariant total rather than
                # introducing an unchecked extraction or panic seam.
                byte_end = match cursor.byte_offset.plus_try(width) {
                    Ok(value) => value
                    Err(Overflow) => U64.highest
                }
                next_scalar_index = match cursor.scalar_index.plus_try(1) {
                    Ok(value) => value
                    Err(Overflow) => U64.highest
                }

                One({
                    item: {
                        scalar,
                        byte_start: cursor.byte_offset,
                        byte_end,
                        scalar_index: cursor.scalar_index,
                    },
                    rest: {
                        bytes: rest,
                        byte_offset: byte_end,
                        scalar_index: next_scalar_index,
                    },
                })
            }
        }
    }

    fold_scalars : Str, state, (state, U32, U64, U64, U64 -> state) -> state
    fold_scalars = |source, initial, step| {
        var $result = initial
        var $accumulator = 0.U32
        var $expected_width = 0.U8
        var $seen_width = 0.U8
        var $sequence_start = 0.U64
        var $byte_offset = 0.U64
        var $scalar_index = 0.U64

        # This is the fused algorithm hot path. Roc `Str` validity guarantees
        # every leading byte's continuations, and the source byte length bounds
        # both counters, so the loop needs neither fallible extraction nor
        # public per-scalar wrappers.
        for byte in source.iter_utf8() {
            byte_end = $byte_offset + 1
            if $expected_width == 0 {
                if byte < 0x80 {
                    $result = step($result, byte.to_u32(), $byte_offset, byte_end, $scalar_index)
                    $scalar_index = $scalar_index + 1
                } else {
                    $sequence_start = $byte_offset
                    if byte < 0xE0 {
                        $accumulator = byte.bitwise_and(0x1F).to_u32()
                        $expected_width = 2
                    } else if byte < 0xF0 {
                        $accumulator = byte.bitwise_and(0x0F).to_u32()
                        $expected_width = 3
                    } else {
                        $accumulator = byte.bitwise_and(0x07).to_u32()
                        $expected_width = 4
                    }
                    $seen_width = 1
                }
            } else {
                $accumulator = $accumulator
                    .shl_wrap(6)
                    .bitwise_or(byte.bitwise_and(0x3F).to_u32())
                $seen_width = $seen_width + 1

                if $seen_width == $expected_width {
                    $result = step($result, $accumulator, $sequence_start, byte_end, $scalar_index)
                    $scalar_index = $scalar_index + 1
                    $accumulator = 0
                    $expected_width = 0
                    $seen_width = 0
                }
            }

            $byte_offset = byte_end
        }

        $result
    }
}

next_byte : Iter(U8) -> [Byte({ value : U8, rest : Iter(U8) }), End]
next_byte = |initial| {
    var $iterator = initial

    while Bool.True {
        match Iter.next($iterator) {
            Done => return End
            Skip({ rest }) => {
                $iterator = rest
            }
            One({ item, rest }) => return Byte({ value: item, rest })
        }
    }

    End
}
