import InternalEAW exposing [east_asian_width, east_asian_width_property]

## A [Unicode code point](http://www.unicode.org/glossary/#code_point).
CodePoint :: { cp : U32 }.{
    Utf8ParseErr : [OverlongEncoding, ExpectedContinuation, EncodesSurrogateHalf, InvalidUtf8, ListWasEmpty, CodepointTooLarge]

    ## Converts a [CodePoint] to its underlying [Unicode code point](http://www.unicode.org/glossary/#code_point)
    ## integer representation.
    to_u32 : CodePoint -> U32
    to_u32 = |{ cp }| cp

    ## Converts a `U32` to a [CodePoint] by verifying that it is a valid [Unicode code point](http://www.unicode.org/glossary/#code_point)
    ## (that is, it's between `0` and `0x10FFFF`).
    from_u32 : U32 -> Try(CodePoint, [InvalidCodePoint])
    from_u32 = |u32| {
        if u32 <= 0x10FFFF {
            Ok({ cp: u32 })
        } else {
            Err(InvalidCodePoint)
        }
    }

    ## Returns False if this is [is_high_surrogate] or [is_low_surrogate]
    is_valid_scalar : CodePoint -> Bool
    is_valid_scalar = |code_point| {
        !(is_high_surrogate(code_point) or is_low_surrogate(code_point))
    }

    ## Returns True if this is a [high-surrogate code point](http://www.unicode.org/glossary/#high_surrogate_code_point)
    ## from U+D800 to U+DBFF
    is_high_surrogate : CodePoint -> Bool
    is_high_surrogate = |code_point| {
        u32 = code_point.to_u32()
        u32 >= 0xD800 and u32 <= 0xDBFF
    }

    ## Returns True if this is a [low-surrogate code point](https://www.unicode.org/glossary/#low_surrogate_code_point)
    ## from U+DC00 to U+DFFF
    is_low_surrogate : CodePoint -> Bool
    is_low_surrogate = |code_point| {
        u32 = code_point.to_u32()
        u32 >= 0xDC00 and u32 <= 0xDFFF
    }

    ## Returns the number of bytes needed to encode this [CodePoint] as UTF-8.
    utf8_len : CodePoint -> Try(U8, [InvalidCodePoint])
    utf8_len = |code_point| {
        u32 = code_point.to_u32()

        if u32 < 0x80 {
            Ok(1)
        } else if u32 < 0x800 {
            Ok(2)
        } else if u32 < 0x10000 {
            Ok(3)
        } else if u32 < 0x110000 {
            Ok(4)
        } else {
            Err(InvalidCodePoint)
        }
    }

    ## Encode a [CodePoint] as UTF-8 bytes and append those bytes to an existing list of UTF-8 bytes.
    append_utf8 : List(U8), CodePoint -> List(U8)
    append_utf8 = |bytes, code_point| {
        u32 = code_point.to_u32()
        if u32 < 0x80 {
            bytes.append(u32.to_u8_wrap())
        } else if u32 < 0x800 {
            byte1 =
                u32
                    .shift_right_by(6)
                    .bitwise_or(0b11000000)
                    .to_u8_wrap()

            byte2 =
                u32
                    .bitwise_and(0b111111)
                    .bitwise_or(0b10000000)
                    .to_u8_wrap()

            bytes
                .reserve(2)
                .append(byte1)
                .append(byte2)
        } else if u32 < 0x10000 {
            byte1 =
                u32
                    .shift_right_by(12)
                    .bitwise_or(0b11100000)
                    .to_u8_wrap()

            byte2 =
                u32
                    .shift_right_by(6)
                    .bitwise_and(0b111111)
                    .bitwise_or(0b10000000)
                    .to_u8_wrap()

            byte3 =
                u32
                    .bitwise_and(0b111111)
                    .bitwise_or(0b10000000)
                    .to_u8_wrap()

            bytes
                .reserve(3)
                .append(byte1)
                .append(byte2)
                .append(byte3)
        } else {

            byte1 =
                u32
                    .shift_right_by(18)
                    .bitwise_or(0b11110000)
                    .to_u8_wrap()

            byte2 =
                u32
                    .shift_right_by(12)
                    .bitwise_and(0b111111)
                    .bitwise_or(0b10000000)
                    .to_u8_wrap()

            byte3 =
                u32
                    .shift_right_by(6)
                    .bitwise_and(0b111111)
                    .bitwise_or(0b10000000)
                    .to_u8_wrap()

            byte4 =
                u32
                    .bitwise_and(0b111111)
                    .bitwise_or(0b10000000)
                    .to_u8_wrap()

            bytes
                .reserve(4)
                .append(byte1)
                .append(byte2)
                .append(byte3)
                .append(byte4)
        }
    }

    ## The number of UTF-8 bytes it takes to represent this [CodePoint].
    count_utf8_bytes : CodePoint -> U8
    count_utf8_bytes = |code_point| {
        u32 = code_point.to_u32()

        if u32 < 0x80 {
            1
        } else if u32 < 0x800 {
            2
        } else if u32 < 0x10000 {
            3
        } else {
            4
        }
    }

    ## Parses a list of bytes into a list of code points
    parse_utf8 : List(U8) -> Try(List(CodePoint), Utf8ParseErr)
    parse_utf8 = |bytes| {
        list_with_capacity : List(CodePoint)
        list_with_capacity = List.with_capacity(bytes.len())
        parse_utf8_help(bytes, list_with_capacity)
    }

    ## Parses the first code point found in a list of bytes encoded as UTF-8. Returns `ListWasEmpty`
    ## if the list was empty, or `InvalidUtf8` if the bytes were not valid UTF-8.
    parse_partial_utf8 : List(U8) -> Try({ code_point : CodePoint, bytes_parsed : U64 }, Utf8ParseErr)
    parse_partial_utf8 = |bytes| {
        # We always try to get the first byte, and if it fails with Err(ListWasEmpty), then
        # the whole function should return Err(ListWasEmpty). This tells the caller "there's nothing
        # else to parse" as opposed to "There are bytes here, but they're invalid UTF-8."
        first_byte = bytes.first()?

        # Get the byte at the index, or return Err(InvalidUtf8) if that's past the end of the list.
        byte_at = |index| {
            bytes.get(index)
                .map_err(|OutOfBounds| InvalidUtf8)
        }

        # Some of the bits in the first byte tell us the total byte length of the code point.
        if first_byte <= 0b0111_1111 {
            bytes_parsed = 1

            Try.Ok({ code_point: internal_from_u32_unchecked(first_byte.to_u32()), bytes_parsed })
        } else if first_byte >= 0b1100_0000 and first_byte <= 0b1101_1111 {
            # 2-byte code point
            second_byte = byte_at(1)?
            bytes_parsed = 2

            parse2(first_byte, second_byte)
                .map_ok(|u32| { code_point: internal_from_u32_unchecked(u32), bytes_parsed })
        } else if first_byte >= 0b1110_0000 and first_byte <= 0b1110_1111 {
            # 3-byte code point
            second_byte = byte_at(1)?
            third_byte = byte_at(2)?
            bytes_parsed = 3

            if U8.bitwise_and(first_byte, 0b11110000) == 0b11100000 {
                parse3(first_byte, second_byte, third_byte)
                    .map_ok(|u32| { code_point: internal_from_u32_unchecked(u32), bytes_parsed })
            } else {
                Err(InvalidUtf8)
            }
        } else if first_byte >= 0b1111_0000 and first_byte <= 0b1111_0111 {
            # 4-byte code point
            second_byte = byte_at(1)?
            third_byte = byte_at(2)?
            fourth_byte = byte_at(3)?
            bytes_parsed = 4

            if
                U8.bitwise_and(first_byte, 0b11111000)
                    == 0b11110000
                    and U8.bitwise_and(second_byte, 0b11000000)
                        == 0b10000000
                        and U8.bitwise_and(third_byte, 0b11000000)
                            == 0b10000000
                            and U8.bitwise_and(fourth_byte, 0b11000000)
                                == 0b10000000
                    {
                        parse4(first_byte, second_byte, third_byte, fourth_byte)
                            .map_ok(|u32| { code_point: internal_from_u32_unchecked(u32), bytes_parsed })
                    } else {
                        Err(InvalidUtf8)
                    }
        } else {
            Err(InvalidUtf8)
        }
    }

    # allocated extra space for the extra bytes as some CPs expand into
    # multiple U8s, so this minimises extra allocations
    to_str : List(CodePoint) -> Try(Str, [BadUtf8])
    to_str = |cps| {
        # allocated extra space for the extra bytes as some CPs expand into
        # multiple U8s, so this minimises extra allocations
        capacity = List.with_capacity((50 + cps.len()))

        cps
            ->cps_to_str_help(capacity)
            ->Str.from_utf8()
            .map_err(|_| BadUtf8)
    }

    EastAsianProperty := [Fullwidth, Wide, Ambiguous, Halfwidth, Neutral, Narrow]

    east_asian_width_property : CodePoint -> EastAsianProperty
    east_asian_width_property = |cp| {
        eaw = cp.to_u32()->east_asian_width_property()
        match eaw {
            F => Fullwidth
            W => Wide
            A => Ambiguous
            H => Halfwidth
            N => Neutral
            Na => Narrow
        }
    }

    visual_width : CodePoint -> U32
    visual_width = |cp| {
        cp.to_u32()->east_asian_width()
    }

    internal_from_u32_unchecked = |safe_u32| {
        from_u32(safe_u32) ?? {
            crash "Unreachable: the u32 is known to be a valid codepoint."
        }
    }
}

# private methods
is_continuation_byte : U8 -> Bool
is_continuation_byte = |byte| {
    U8.bitwise_and(byte, 0b11000000) == 0b10000000
}

add_continuation : U32, U8 -> U32
add_continuation = |original, continuation_byte| {
    original
        .shift_left_by(6)
        .bitwise_or(U8.bitwise_and(continuation_byte, 0b00111111).to_u32())
}

## parse a 2-byte code point
parse2 : U8, U8 -> Try(U32, [OverlongEncoding, ExpectedContinuation, ..])
parse2 = |first, second| {
    if is_continuation_byte(second) {
        answer =
            U8.bitwise_and(first, 0b00011111)
                .to_u32()
                ->add_continuation(second)

        if answer < 0x80 {
            Err(OverlongEncoding)
        } else {
            Ok(answer)
        }
    } else {
        Err(ExpectedContinuation)
    }
}

## parse a 3-byte code point
parse3 : U8, U8, U8 -> Try(U32, [OverlongEncoding, ExpectedContinuation, EncodesSurrogateHalf, ..])
parse3 = |first, second, third| {
    if is_continuation_byte(second) and is_continuation_byte(third) {
        answer =
            U8.bitwise_and(first, 0b00001111)
                .to_u32()
                ->add_continuation(second)
                ->add_continuation(third)

        if answer < 0x80 {
            Err(OverlongEncoding)
        } else if 0xd800 <= answer and answer <= 0xdfff {
            Err(EncodesSurrogateHalf)
        } else {
            Ok(answer)
        }
    } else {
        Err(ExpectedContinuation)
    }
}

## parse a 4-byte code point
parse4 : U8, U8, U8, U8 -> Try(U32, [OverlongEncoding, ExpectedContinuation, CodepointTooLarge, ..])
parse4 = |first, second, third, fourth| {
    if is_continuation_byte(second) and is_continuation_byte(third) and is_continuation_byte(fourth) {
        answer =
            U8.bitwise_and(first, 0b00001111)
                .to_u32()
                ->add_continuation(second)
                ->add_continuation(third)
                ->add_continuation(fourth)

        if answer < 0x10000 {
            Err(OverlongEncoding)
        } else if answer > 0x10FFFF {
            Err(CodepointTooLarge)
        } else {
            Ok(answer)
        }
    } else {
        Err(ExpectedContinuation)
    }
}

parse_utf8_help : List(U8), List(CodePoint) -> Try(List(CodePoint), Utf8ParseErr)
parse_utf8_help = |rest, cps| {
    match rest {
        [] => Ok(cps)
        _ => {
            { code_point, bytes_parsed } = CodePoint.parse_partial_utf8(rest)?
            parse_utf8_help(rest.drop_first(bytes_parsed), cps.append(code_point))
        }
    }
}

cps_to_str_help : List(CodePoint), List(U8) -> List(U8)
cps_to_str_help = |cps, bytes| {
    match cps {
        [] => bytes
        [cp, ..] => {
            cps_to_str_help(
                cps.drop_first(1),
                CodePoint.append_utf8(bytes, cp),
            )
        }
    }
}

# test simple ASCII "Hello"
expect {
    expected = Ok([72, 101, 108, 108, 111, 33]->keep_oks(CodePoint.from_u32))
    actual = CodePoint.parse_utf8([72, 101, 108, 108, 111, 33])
    actual == expected
}

expect {
    cr = CodePoint.from_u32(13)?
    lf = CodePoint.from_u32(10)?
    CodePoint.to_str([cr, lf]) == Ok("\r\n")
}

expect CodePoint.parse_partial_utf8([]) == Err(ListWasEmpty)
expect CodePoint.parse_partial_utf8([0xC3, 0x28]) == Err(ExpectedContinuation)
expect CodePoint.parse_partial_utf8([0xC0, 0xA1]) == Err(OverlongEncoding)
expect CodePoint.parse_partial_utf8([0xE0, 0x80, 0xAF]) == Err(OverlongEncoding)
expect CodePoint.parse_partial_utf8([0xF0, 0x80, 0x80, 0xA6]) == Err(OverlongEncoding)
expect CodePoint.parse_partial_utf8([0xF4, 0x90, 0x80, 0x80]) == Err(CodepointTooLarge)
expect CodePoint.parse_partial_utf8([0xF8, 0xA1, 0xA2, 0xA3, 0xA4]) == Err(InvalidUtf8)
expect CodePoint.parse_partial_utf8([0xC0, 0x80]) == Err(OverlongEncoding)

expect CodePoint.parse_partial_utf8("hello".to_utf8()) == Ok({ code_point: CodePoint.from_u32('h')?, bytes_parsed: 1 })
expect CodePoint.parse_partial_utf8("世界".to_utf8()) == Ok({ code_point: CodePoint.from_u32('世')?, bytes_parsed: 3 })
expect CodePoint.parse_partial_utf8("lé".to_utf8()) == Ok({ code_point: CodePoint.from_u32('l')?, bytes_parsed: 1 })
expect CodePoint.parse_partial_utf8("élan".to_utf8()) == Ok({ code_point: CodePoint.from_u32('é')?, bytes_parsed: 2 })
expect CodePoint.parse_partial_utf8("界s".to_utf8()) == Ok({ code_point: CodePoint.from_u32('界')?, bytes_parsed: 3 })
expect CodePoint.parse_partial_utf8("𠜎a".to_utf8()) == Ok({ code_point: CodePoint.from_u32('𠜎')?, bytes_parsed: 4 })
expect CodePoint.parse_partial_utf8("𠜎爱".to_utf8()) == Ok({ code_point: CodePoint.from_u32('𠜎')?, bytes_parsed: 4 })

expect CodePoint.parse_partial_utf8(List.concat("h".to_utf8(), [0xC0, 0x80])) == Ok({ code_point: CodePoint.from_u32('h')?, bytes_parsed: 1 })
expect CodePoint.parse_partial_utf8(List.concat("é".to_utf8(), [0xC0, 0x80])) == Ok({ code_point: CodePoint.from_u32('é')?, bytes_parsed: 2 })
expect CodePoint.parse_partial_utf8(List.concat("爱".to_utf8(), [0xC0, 0x80])) == Ok({ code_point: CodePoint.from_u32('爱')?, bytes_parsed: 3 })
expect CodePoint.parse_partial_utf8(List.concat("𠜎".to_utf8(), [0xC0, 0x80])) == Ok({ code_point: CodePoint.from_u32('𠜎')?, bytes_parsed: 4 })

expect CodePoint.parse_partial_utf8([0xC3, 0xFF]) == Err(ExpectedContinuation)
expect CodePoint.parse_partial_utf8([0xE2, 0x82, 0xFF]) == Err(ExpectedContinuation)
expect CodePoint.parse_partial_utf8([0xF0, 0x9F, 0x98, 0xFF]) == Err(InvalidUtf8)

expect CodePoint.parse_partial_utf8([0xC2, 0x80]) == Ok({ code_point: CodePoint.from_u32(0x0080)?, bytes_parsed: 2 })
expect CodePoint.parse_partial_utf8([0xDF, 0xBF]) == Ok({ code_point: CodePoint.from_u32(0x07FF)?, bytes_parsed: 2 })
expect CodePoint.parse_partial_utf8([0xE0, 0xA0, 0x80]) == Ok({ code_point: CodePoint.from_u32(0x0800)?, bytes_parsed: 3 })
expect CodePoint.parse_partial_utf8([0xEF, 0xBF, 0xBF]) == Ok({ code_point: CodePoint.from_u32(0xFFFF)?, bytes_parsed: 3 })
expect CodePoint.parse_partial_utf8([0xF0, 0x90, 0x80, 0x80]) == Ok({ code_point: CodePoint.from_u32(0x10000)?, bytes_parsed: 4 })
expect CodePoint.parse_partial_utf8([0xF4, 0x8F, 0xBF, 0xBF]) == Ok({ code_point: CodePoint.from_u32(0x10FFFF)?, bytes_parsed: 4 })

# TODO: the following should soon be available in Roc's builtins
keep_oks = |list, func| {
    var $result = []
    for item in list {
        match func(item) {
            Ok(ok) => {
                $result = $result.append(ok)
            }
            _ => {}
        }
    }
    $result
}
