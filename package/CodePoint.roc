module [
    Utf8ParseErr,
    CodePoint,
    utf8_len,
    to_u32,
    from_u32,
    is_high_surrogate,
    is_low_surrogate,
    is_valid_scalar,
    parse_partial_utf8,
    append_utf8,
    parse_utf8,
    count_utf8_bytes,
    to_str,
    east_asian_width_property,
    visual_width,
]

import InternalCP exposing [CP, from_u32_unchecked]
import InternalEAW

## A [Unicode code point](http://www.unicode.org/glossary/#code_point).
CodePoint : CP

## Converts a [CodePoint] to its underlying [Unicode code point](http://www.unicode.org/glossary/#code_point)
## integer representation.
to_u32 : CodePoint -> U32
to_u32 = InternalCP.to_u32

## Converts a [U32] to a [CodePoint] by verifying that it is a valid [Unicode code point](http://www.unicode.org/glossary/#code_point)
## (that is, it's between `0` and `0x10FFFF`).
from_u32 : U32 -> Result CodePoint [InvalidCodePoint]
from_u32 = \u32 ->
    if u32 <= 0x10FFFF then
        Ok(from_u32_unchecked(u32))
    else
        Err(InvalidCodePoint)

## Returns false if this is [isHighSurrogate] or [isLowSurrogate]
is_valid_scalar : CodePoint -> Bool
is_valid_scalar = \code_point -> !(is_high_surrogate(code_point) || is_low_surrogate(code_point))

## Returns true if this is a [high-surrogate code point](http://www.unicode.org/glossary/#high_surrogate_code_point)
## from U+D800 to U+DBFF
is_high_surrogate : CodePoint -> Bool
is_high_surrogate = \code_point ->
    u32 = to_u32(code_point)
    u32 >= 0xD800 && u32 <= 0xDBFF

## Returns true if this is a [low-surrogate code point](https://www.unicode.org/glossary/#low_surrogate_code_point)
## from U+DC00 to U+DFFF
is_low_surrogate : CodePoint -> Bool
is_low_surrogate = \code_point ->
    u32 = to_u32(code_point)
    u32 >= 0xDC00 && u32 <= 0xDFFF

## Zig docs: bytes the UTF-8 representation would require
## for the given codepoint.
utf8_len : CodePoint -> Result U8 [InvalidCodePoint]
utf8_len = \code_point ->
    u32 = to_u32(code_point)

    if u32 < 0x80 then
        Ok(1)
    else if u32 < 0x800 then
        Ok(2)
    else if u32 < 0x10000 then
        Ok(3)
    else if u32 < 0x110000 then
        Ok(4)
    else
        Err(InvalidCodePoint)

## Encode a Scalar as UTF-8 bytes and append those bytes to an existing list of UTF-8 bytes.
append_utf8 : List U8, CodePoint -> List U8
append_utf8 = \bytes, code_point ->
    u32 = to_u32(code_point)
    if u32 < 0x80 then
        List.append(bytes, Num.to_u8(u32))
    else if u32 < 0x800 then
        byte1 =
            u32
            |> Num.shift_right_by(6)
            |> Num.bitwise_or(0b11000000)
            |> Num.to_u8

        byte2 =
            u32
            |> Num.bitwise_and(0b111111)
            |> Num.bitwise_or(0b10000000)
            |> Num.to_u8

        bytes
        |> List.reserve(2)
        |> List.append(byte1)
        |> List.append(byte2)
    else if u32 < 0x10000 then
        byte1 =
            u32
            |> Num.shift_right_by(12)
            |> Num.bitwise_or(0b11100000)
            |> Num.to_u8

        byte2 =
            u32
            |> Num.shift_right_by(6)
            |> Num.bitwise_and(0b111111)
            |> Num.bitwise_or(0b10000000)
            |> Num.to_u8

        byte3 =
            u32
            |> Num.bitwise_and(0b111111)
            |> Num.bitwise_or(0b10000000)
            |> Num.to_u8

        bytes
        |> List.reserve(3)
        |> List.append(byte1)
        |> List.append(byte2)
        |> List.append(byte3)
    else
        ## This was an invalid Unicode scalar value, even though it had the Roc type Scalar.
        ## This should never happen!
        # expect u32 < 0x110000
        byte1 =
            u32
            |> Num.shift_right_by(18)
            |> Num.bitwise_or(0b11110000)
            |> Num.to_u8

        byte2 =
            u32
            |> Num.shift_right_by(12)
            |> Num.bitwise_and(0b111111)
            |> Num.bitwise_or(0b10000000)
            |> Num.to_u8

        byte3 =
            u32
            |> Num.shift_right_by(6)
            |> Num.bitwise_and(0b111111)
            |> Num.bitwise_or(0b10000000)
            |> Num.to_u8

        byte4 =
            u32
            |> Num.bitwise_and(0b111111)
            |> Num.bitwise_or(0b10000000)
            |> Num.to_u8

        bytes
        |> List.reserve(4)
        |> List.append(byte1)
        |> List.append(byte2)
        |> List.append(byte3)
        |> List.append(byte4)

is_continuation_byte : U8 -> Bool
is_continuation_byte = \byte -> Num.bitwise_and(byte, 0b11000000) == 0b10000000

add_continuation : U32, U8 -> U32
add_continuation = \original, continuation_byte ->
    original
    |> Num.shift_left_by(6)
    |> Num.bitwise_or(Num.to_u32(Num.bitwise_and(continuation_byte, 0b00111111)))

## The number of UTF-8 bytes it takes to represent this Scalar.
count_utf8_bytes : CodePoint -> U8
count_utf8_bytes = \code_point ->
    u32 = to_u32(code_point)

    if u32 < 0x80 then
        1
    else if u32 < 0x800 then
        2
    else if u32 < 0x10000 then
        3
    else
        # If this expectation fails, it was an invalid Scalar and shouldn't have been allowed!
        # expect u32 < 0x110000
        4

## parse a 2-byte code point
parse2 : U8, U8 -> Result U32 [OverlongEncoding, ExpectedContinuation]
parse2 = \first, second ->
    if is_continuation_byte(second) then
        answer =
            first
            |> Num.bitwise_and(0b00011111)
            |> Num.to_u32
            |> add_continuation(second)

        if answer < 0x80 then
            Err(OverlongEncoding)
        else
            Ok(answer)
    else
        Err(ExpectedContinuation)

## parse a 3-byte code point
parse3 : U8, U8, U8 -> Result U32 [OverlongEncoding, ExpectedContinuation, EncodesSurrogateHalf]
parse3 = \first, second, third ->
    if is_continuation_byte(second) && is_continuation_byte(third) then
        answer =
            first
            |> Num.bitwise_and(0b00001111)
            |> Num.to_u32
            |> add_continuation(second)
            |> add_continuation(third)

        if answer < 0x80 then
            Err(OverlongEncoding)
        else if 0xd800 <= answer && answer <= 0xdfff then
            Err(EncodesSurrogateHalf)
        else
            Ok(answer)
    else
        Err(ExpectedContinuation)

## parse a 4-byte code point
parse4 : U8, U8, U8, U8 -> Result U32 [OverlongEncoding, ExpectedContinuation, CodepointTooLarge]
parse4 = \first, second, third, fourth ->
    if is_continuation_byte(second) && is_continuation_byte(third) && is_continuation_byte(third) then
        answer =
            first
            |> Num.bitwise_and(0b00001111)
            |> Num.to_u32
            |> add_continuation(second)
            |> add_continuation(third)
            |> add_continuation(fourth)

        if answer < 0x10000 then
            Err(OverlongEncoding)
        else if answer > 0x10FFFF then
            Err(CodepointTooLarge)
        else
            Ok(answer)
    else
        Err(ExpectedContinuation)

Utf8ParseErr : [OverlongEncoding, ExpectedContinuation, EncodesSurrogateHalf, InvalidUtf8, ListWasEmpty, CodepointTooLarge]

## Parses a list of bytes into a list of code points
parse_utf8 : List U8 -> Result (List CodePoint) Utf8ParseErr
parse_utf8 = \bytes ->
    # we will have at most List.len bytes code points
    list_with_capacity : List CodePoint
    list_with_capacity = List.with_capacity(List.len(bytes))
    parse_utf8_help(bytes, list_with_capacity)

parse_utf8_help : List U8, List CodePoint -> Result (List CodePoint) Utf8ParseErr
parse_utf8_help = \rest, cps ->
    if List.is_empty(rest) then
        Ok(cps)
    else
        parse_partial_utf8(rest)
        |> Result.try(
            \{ code_point, bytes_parsed } ->
                parse_utf8_help(List.drop_first(rest, bytes_parsed), List.append(cps, code_point)),
        )

# test simple ASCII "Hello"
expect
    expected = [72, 101, 108, 108, 111, 33] |> List.map(from_u32_unchecked) |> Ok
    actual = parse_utf8([72, 101, 108, 108, 111, 33])
    actual == expected

## Parses the first code point found in a list of bytes encoded as UTF-8. Returns `ListWasEmpty`
## if the list was empty, or `InvalidUtf8` if the bytes were not valid UTF-8.
parse_partial_utf8 : List U8 -> Result { code_point : CodePoint, bytes_parsed : U64 } Utf8ParseErr
parse_partial_utf8 = \bytes ->
    # We always try to get the first byte, and if it fails with Err ListWasEmpty, then
    # the whole function should return Err ListWasEmpty. This tells the caller "there's nothing
    # else to parse" as opposed to "There are bytes here, but they're invalid UTF-8."
    first_byte = List.first(bytes)?

    # Get the byte at the index, or return Err InvalidUtf8 if that's past the end of the list.
    byte_at = \index ->
        List.get(bytes, index)
        |> Result.map_err(\OutOfBounds -> InvalidUtf8)

    # Some of the bits in the first byte tell us the total byte length of the code point.
    if first_byte <= 0b0111_1111 then
        # 1-byte code point
        bytes_parsed = 1

        Ok(
            {
                code_point: from_u32_unchecked(Num.to_u32(first_byte)),
                bytes_parsed,
            },
        )
    else if first_byte >= 0b1100_0000 && first_byte <= 0b1101_1111 then
        # 2-byte code point
        second_byte = byte_at(1)?
        bytes_parsed = 2

        parse2(first_byte, second_byte)
        |> Result.map(\u32 -> { code_point: from_u32_unchecked(u32), bytes_parsed })
    else if first_byte >= 0b1110_0000 && first_byte <= 0b1110_1111 then
        # 3-byte code point
        second_byte = byte_at(1)?
        third_byte = byte_at(2)?
        bytes_parsed = 3

        if Num.bitwise_and(first_byte, 0b11110000) == 0b11100000 then
            parse3(first_byte, second_byte, third_byte)
            |> Result.map(\u32 -> { code_point: from_u32_unchecked(u32), bytes_parsed })
        else
            Err(InvalidUtf8)
    else if first_byte >= 0b1111_0000 && first_byte <= 0b1111_0111 then
        # 4-byte code point
        second_byte = byte_at(1)?
        third_byte = byte_at(2)?
        fourth_byte = byte_at(3)?
        bytes_parsed = 4

        if
            Num.bitwise_and(first_byte, 0b11111000)
            == 0b11110000
            && Num.bitwise_and(second_byte, 0b11000000)
            == 0b10000000
            && Num.bitwise_and(third_byte, 0b11000000)
            == 0b10000000
            && Num.bitwise_and(fourth_byte, 0b11000000)
            == 0b10000000
        then
            parse4(first_byte, second_byte, third_byte, fourth_byte)
            |> Result.map(\u32 -> { code_point: from_u32_unchecked(u32), bytes_parsed })
        else
            Err(InvalidUtf8)
    else
        Err(InvalidUtf8)

to_str : List CodePoint -> Result Str [BadUtf8]
to_str = \cps ->

    # allocated extra space for the extra bytes as some CPs expand into
    # multiple U8s, so this minimises extra allocations
    capacity = List.with_capacity((50 + List.len(cps)))

    cps
    |> cps_to_str_help(capacity)
    |> Str.from_utf8
    |> Result.on_err(\_ -> Err(BadUtf8))

cps_to_str_help : List CodePoint, List U8 -> List U8
cps_to_str_help = \cps, bytes ->
    when cps is
        [] -> bytes
        [cp, ..] ->
            cps_to_str_help(
                List.drop_first(cps, 1),
                CodePoint.append_utf8(bytes, cp),
            )

## The East Asian Width property in Unicode categorizes characters based on
## their typical width and display behavior in East Asian typography, helping
## to ensure proper alignment and spacing in text layout for languages like
## Chinese, Japanese, and Korean.
EastAsianProperty : [Fullwidth, Wide, Ambiguous, Halfwidth, Neutral, Narrow]

## Computes the "east asian width" property for a given code point.
## See https://www.unicode.org/Public/15.1.0/ucd/EastAsianWidth.txt
east_asian_width_property : CodePoint -> EastAsianProperty
east_asian_width_property = \cp ->
    eaw = cp |> to_u32 |> InternalEAW.east_asian_width_property
    when eaw is
        F -> Fullwidth
        W -> Wide
        A -> Ambiguous
        H -> Halfwidth
        N -> Neutral
        Na -> Narrow

## Computes the visual width of a code point as assigned by the Unicode Character Database
visual_width : CodePoint -> U32
visual_width = \cp ->
    cp |> to_u32 |> InternalEAW.east_asian_width

expect
    # test toStr
    cr = from_u32_unchecked(13)
    lf = from_u32_unchecked(10)

    to_str([cr, lf]) == Ok("\r\n")

## Empty input
expect [] |> parse_partial_utf8 == Err(ListWasEmpty)

## Incorrect continuation byte
expect [0xC3, 0x28] |> parse_partial_utf8 == Err(ExpectedContinuation)

## Overlong encoding for ASCII character
expect [0xC0, 0xA1] |> parse_partial_utf8 == Err(OverlongEncoding)

## Overlong encoding for 2-byte character
expect [0xE0, 0x80, 0xAF] |> parse_partial_utf8 == Err(OverlongEncoding)

## Overlong encoding for 3-byte character
expect [0xF0, 0x80, 0x80, 0xA6] |> parse_partial_utf8 == Err(OverlongEncoding)

## Invalid four-byte encoding (scalar value too large)
expect [0xF4, 0x90, 0x80, 0x80] |> parse_partial_utf8 == Err(CodepointTooLarge)

## Invalid first byte (>= 0xF8)
expect [0xF8, 0xA1, 0xA2, 0xA3, 0xA4] |> parse_partial_utf8 == Err(InvalidUtf8)

## Invalid first byte (>= 0xC0 and <= 0xC1)
expect [0xC0, 0x80] |> parse_partial_utf8 == Err(OverlongEncoding)

## Multiple valid 1-byte scalars
expect "hello" |> Str.to_utf8 |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('h'), bytes_parsed: 1 })

## Multiple valid 3-byte scalars
expect "世界" |> Str.to_utf8 |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('世'), bytes_parsed: 3 })

## Valid 1-byte followed by valid multi-byte scalar
expect "lé" |> Str.to_utf8 |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('l'), bytes_parsed: 1 })

## Valid 2-byte followed by multiple single-byte scalar
expect "élan" |> Str.to_utf8 |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('é'), bytes_parsed: 2 })

## Valid 3-byte followed by valid single-byte scalar
expect "界s" |> Str.to_utf8 |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('界'), bytes_parsed: 3 })

## Valid 4-byte followed by valid single-byte scalar
expect "𠜎a" |> Str.to_utf8 |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('𠜎'), bytes_parsed: 4 })

## Valid 4-byte followed by valid multi-byte scalar
expect "𠜎爱" |> Str.to_utf8 |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('𠜎'), bytes_parsed: 4 })

## Valid 1-byte followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat(Str.to_utf8("h"), [0xC0, 0x80])) |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('h'), bytes_parsed: 1 })

## Valid 2-byte followed by multiple single-byte scalar
expect (List.concat(Str.to_utf8("é"), [0xC0, 0x80])) |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('é'), bytes_parsed: 2 })

## Valid 3-byte followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat(Str.to_utf8("爱"), [0xC0, 0x80])) |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('爱'), bytes_parsed: 3 })

## Valid 4-byte followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat(Str.to_utf8("𠜎"), [0xC0, 0x80])) |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked('𠜎'), bytes_parsed: 4 })

## Invalid byte within a valid 2-byte character
expect [0xC3, 0xFF] |> parse_partial_utf8 == Err(ExpectedContinuation)

## Invalid byte within a valid 3-byte character
expect [0xE2, 0x82, 0xFF] |> parse_partial_utf8 == Err(ExpectedContinuation)

## Invalid byte within a valid 4-byte character
expect [0xF0, 0x9F, 0x98, 0xFF] |> parse_partial_utf8 == Err(InvalidUtf8)

## Minimum valid 2-byte character
expect [0xC2, 0x80] |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked(0x0080), bytes_parsed: 2 })

## Maximum valid 2-byte character
expect [0xDF, 0xBF] |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked(0x07FF), bytes_parsed: 2 })

## Minimum valid 3-byte character
expect [0xE0, 0xA0, 0x80] |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked(0x0800), bytes_parsed: 3 })

## Maximum valid 3-byte character
expect [0xEF, 0xBF, 0xBF] |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked(0xFFFF), bytes_parsed: 3 })

## Minimum valid 4-byte character
expect [0xF0, 0x90, 0x80, 0x80] |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked(0x10000), bytes_parsed: 4 })

## Maximum valid 4-byte character
expect [0xF4, 0x8F, 0xBF, 0xBF] |> parse_partial_utf8 == Ok({ code_point: from_u32_unchecked(0x10FFFF), bytes_parsed: 4 })
