## Allocation-free traversal of the Unicode scalars in a valid Roc `Str`.
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

    next : Cursor -> [One({ item : LocatedScalar, rest : Cursor }), Done]
    next = |cursor| {
        match next_byte(cursor.bytes) {
            Err(NoMore) => Done
            Ok((first, after_first)) => {
                width = if first < 0x80 1.U64 else if first < 0xE0 2 else if first < 0xF0 3 else 4

                (scalar, rest) = match width {
                    1 => (first.to_u32(), after_first)
                    2 => {
                        (second, after_second) = next_byte(after_first) ?? ...
                        value = first.bitwise_and(0x1F).to_u32()
                            .shl_wrap(6)
                            .bitwise_or(second.bitwise_and(0x3F).to_u32())
                        (value, after_second)
                    }
                    3 => {
                        (second, after_second) = next_byte(after_first) ?? ...
                        (third, after_third) = next_byte(after_second) ?? ...
                        value = first.bitwise_and(0x0F).to_u32()
                            .shl_wrap(6)
                            .bitwise_or(second.bitwise_and(0x3F).to_u32())
                            .shl_wrap(6)
                            .bitwise_or(third.bitwise_and(0x3F).to_u32())
                        (value, after_third)
                    }
                    4 => {
                        (second, after_second) = next_byte(after_first) ?? ...
                        (third, after_third) = next_byte(after_second) ?? ...
                        (fourth, after_fourth) = next_byte(after_third) ?? ...
                        value = first.bitwise_and(0x07).to_u32()
                            .shl_wrap(6)
                            .bitwise_or(second.bitwise_and(0x3F).to_u32())
                            .shl_wrap(6)
                            .bitwise_or(third.bitwise_and(0x3F).to_u32())
                            .shl_wrap(6)
                            .bitwise_or(fourth.bitwise_and(0x3F).to_u32())
                        (value, after_fourth)
                    }
                    _ => ...
                }

                byte_end = cursor.byte_offset + width
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
                        scalar_index: cursor.scalar_index + 1,
                    },
                })
            }
        }
    }

    fold_scalars : Str, state, (state, U32, U64, U64, U64 -> state) -> state
    fold_scalars = |source, initial, step| {
        var result = initial
        var cursor = InternalUtf8.init(source)

        while Bool.True {
            match InternalUtf8.next(cursor) {
                Done => {
                    break
                }
                One({ item, rest }) => {
                    result = step(
                        result,
                        item.scalar,
                        item.byte_start,
                        item.byte_end,
                        item.scalar_index,
                    )
                    cursor = rest
                }
            }
        }

        result
    }
}

next_byte : Iter(U8) -> Try((U8, Iter(U8)), [NoMore])
next_byte = |initial| {
    var iterator = initial

    while Bool.True {
        match Iter.next(iterator) {
            Done => return Err(NoMore)
            Skip({ rest }) => {
                iterator = rest
            }
            One({ item, rest }) => return Ok((item, rest))
        }
    }

    Err(NoMore)
}
