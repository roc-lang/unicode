## Allocation-free traversal of the Unicode scalars in a valid Roc `Str`.
##
## This private view deliberately carries raw integers instead of public
## `Scalar` and `ByteRange` wrappers. Algorithm hot loops can therefore fuse
## decoding with their narrow property lookup and transition without building
## public records for every scalar.
##
## `fold_with_ascii_blocks` is the internal high-throughput seam. It keeps the
## scalar transition and the algorithm-specific ASCII transition adjacent in
## one loop instead of building an `Iter(U8) -> Iter(Scalar) -> Iter(Property)`
## pipeline. The SIMD width and threshold are intentionally private.
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

    ## Fold a complete valid string, offering proven all-ASCII blocks to an
    ## algorithm-specific consumer and every other scalar to `step_scalar`.
    ##
    ## Strings below the private threshold stay on `Str.iter_utf8`, which is
    ## allocation-free for inline strings. Longer strings are necessarily
    ## heap-backed on every currently supported target, so `Str.to_utf8`
    ## creates a borrowed list view rather than copying the bytes. It still
    ## performs reference-count bookkeeping; a direct `Str` vector-load
    ## primitive would remove that residual cost.
    ##
    ## A failed ASCII probe falls back for at least one whole vector window, so
    ## non-ASCII text does not pay for a vector load once per scalar.
    ##
    ## Callback byte offsets and scalar indices are segment-local: zero means
    ## the beginning of this `source` argument, even when `source` is one chunk
    ## of a larger logical input. Chunked callers must add their tracked byte
    ## and scalar base offsets before storing or exposing logical coordinates.
    ## This fold does not retain coordinate state between source segments.
    fold_with_ascii_blocks : Str, state, (state, U32, U64, U64, U64 -> state), (state, U8x16, U64, U64 -> state) -> state
    fold_with_ascii_blocks = |source, initial, step_scalar, step_ascii| {
        byte_len = source.count_utf8_bytes()

        if byte_len < simd_min_bytes {
            InternalUtf8.fold_scalars(source, initial, step_scalar)
        } else {
            bytes = source.to_utf8()
            var state = initial
            var byte_offset = 0.U64
            var scalar_index = 0.U64

            while byte_offset < byte_len {
                remaining = byte_len - byte_offset

                if remaining >= vector_bytes {
                    match U8x16.load(bytes, byte_offset) {
                        Err(_) => {
                            # The bounds proof above should make this branch
                            # unreachable. Scalar completion preserves total
                            # behavior if a backend fails to eliminate it.
                            folded = fold_byte_range(
                                bytes,
                                byte_offset,
                                byte_len,
                                scalar_index,
                                state,
                                step_scalar,
                            )
                            state = folded.state
                            byte_offset = folded.byte_offset
                            scalar_index = folded.scalar_index
                        }
                        Ok(vector) => {
                            if vector.to_bitmask() == 0 {
                                state = step_ascii(
                                    state,
                                    vector,
                                    byte_offset,
                                    scalar_index,
                                )
                                byte_offset = byte_offset + vector_bytes
                                scalar_index = scalar_index + vector_bytes
                            } else {
                                # Decode through the probed window. The last
                                # scalar may end up to three bytes beyond it;
                                # the next probe starts at that scalar boundary.
                                folded = fold_byte_range(
                                    bytes,
                                    byte_offset,
                                    byte_offset + vector_bytes,
                                    scalar_index,
                                    state,
                                    step_scalar,
                                )
                                state = folded.state
                                byte_offset = folded.byte_offset
                                scalar_index = folded.scalar_index
                            }
                        }
                    }
                } else {
                    folded = fold_byte_range(
                        bytes,
                        byte_offset,
                        byte_len,
                        scalar_index,
                        state,
                        step_scalar,
                    )
                    state = folded.state
                    byte_offset = folded.byte_offset
                    scalar_index = folded.scalar_index
                }
            }

            state
        }
    }
}

# 64 bytes is above Roc's inline-string capacity on 32- and 64-bit targets and
# amortizes one Str/List view conversion over at least four vector probes.
# Throughput tuning may move this private threshold without changing semantics.
simd_min_bytes : U64
simd_min_bytes = 64

vector_bytes : U64
vector_bytes = 16

fold_byte_range : List(U8), U64, U64, U64, state, (state, U32, U64, U64, U64 -> state) -> { state : state, byte_offset : U64, scalar_index : U64 }
fold_byte_range = |bytes, start, end, initial_index, initial, step| {
    var state = initial
    var byte_offset = start
    var scalar_index = initial_index

    while byte_offset < end {
        decoded = decode_valid_at(bytes, byte_offset)
        byte_end = byte_offset + decoded.width
        state = step(
            state,
            decoded.scalar,
            byte_offset,
            byte_end,
            scalar_index,
        )
        byte_offset = byte_end
        scalar_index = scalar_index + 1
    }

    { state, byte_offset, scalar_index }
}

# Preconditions are established by `fold_with_ascii_blocks`: `bytes` is a
# fresh view of a valid `Str`, and `byte_start` is a scalar boundary below its
# length. Consequently the leading byte determines an available complete
# sequence. Keeping this decoder private prevents arbitrary bytes from relying
# on that invariant.
decode_valid_at : List(U8), U64 -> { scalar : U32, width : U64 }
decode_valid_at = |bytes, byte_start| {
    first = bytes.get(byte_start) ?? ...

    if first < 0x80 {
        { scalar: first.to_u32(), width: 1 }
    } else if first < 0xE0 {
        second = bytes.get(byte_start + 1) ?? ...
        value = first.bitwise_and(0x1F).to_u32()
            .shl_wrap(6)
            .bitwise_or(second.bitwise_and(0x3F).to_u32())
        { scalar: value, width: 2 }
    } else if first < 0xF0 {
        second = bytes.get(byte_start + 1) ?? ...
        third = bytes.get(byte_start + 2) ?? ...
        value = first.bitwise_and(0x0F).to_u32()
            .shl_wrap(6)
            .bitwise_or(second.bitwise_and(0x3F).to_u32())
            .shl_wrap(6)
            .bitwise_or(third.bitwise_and(0x3F).to_u32())
        { scalar: value, width: 3 }
    } else {
        second = bytes.get(byte_start + 1) ?? ...
        third = bytes.get(byte_start + 2) ?? ...
        fourth = bytes.get(byte_start + 3) ?? ...
        value = first.bitwise_and(0x07).to_u32()
            .shl_wrap(6)
            .bitwise_or(second.bitwise_and(0x3F).to_u32())
            .shl_wrap(6)
            .bitwise_or(third.bitwise_and(0x3F).to_u32())
            .shl_wrap(6)
            .bitwise_or(fourth.bitwise_and(0x3F).to_u32())
        { scalar: value, width: 4 }
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
