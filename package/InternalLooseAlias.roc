## Allocation-free Unicode loose matching for ASCII property aliases.
##
## UAX #44 property aliases are ASCII. Case, ASCII space, hyphen, and
## underscore are ignored; no normalized string is materialized.
InternalLooseAlias :: [].{
    matches : Str, Str -> Bool
    matches = |left, right| {
        var left_bytes = left.iter_utf8()
        var right_bytes = right.iter_utf8()

        while Bool.True {
            left_next = next_significant(left_bytes)
            right_next = next_significant(right_bytes)

            match (left_next, right_next) {
                (End, End) => return Bool.True
                (Byte(_), End) | (End, Byte(_)) => return Bool.False
                (Byte(left_byte), Byte(right_byte)) => {
                    if ascii_lower(left_byte.value) != ascii_lower(right_byte.value) {
                        return Bool.False
                    }
                    left_bytes = left_byte.rest
                    right_bytes = right_byte.rest
                }
            }
        }

        Bool.False
    }
}

next_significant : Iter(U8) -> [Byte({ value : U8, rest : Iter(U8) }), End]
next_significant = |initial| {
    var iterator = initial

    while Bool.True {
        match Iter.next(iterator) {
            Done => return End
            Skip({ rest }) => {
                iterator = rest
            }
            One({ item, rest }) => {
                iterator = rest
                if item != 0x20 and item != 0x2D and item != 0x5F {
                    return Byte({ value: item, rest })
                }
            }
        }
    }

    End
}

ascii_lower : U8 -> U8
ascii_lower = |byte| if byte >= 0x41 and byte <= 0x5A byte + 0x20 else byte
