module [
    Utf8ParseErr,
    CodePoint,
    utf8Len,
    toU32,
    fromU32,
    isHighSurrogate,
    isLowSurrogate,
    isValidScalar,
    appendUtf8,
    parseUtf8,
    countUtf8Bytes,
    toStr,
    eastAsianWidthProperty,
    visualWidth,
]

import InternalCP exposing [CP, fromU32Unchecked]
import InternalEAW

## A [Unicode code point](http://www.unicode.org/glossary/#code_point).
CodePoint : CP

## Converts a [CodePoint] to its underlying [Unicode code point](http://www.unicode.org/glossary/#code_point)
## integer representation.
toU32 : CodePoint -> U32
toU32 = InternalCP.toU32

## Converts a [U32] to a [CodePoint] by verifying that it is a valid [Unicode code point](http://www.unicode.org/glossary/#code_point)
## (that is, it's between `0` and `0x10FFFF`).
fromU32 : U32 -> Result CodePoint [InvalidCodePoint]
fromU32 = \u32 ->
    if u32 <= 0x10FFFF then
        Ok (fromU32Unchecked u32)
    else
        Err InvalidCodePoint

## Returns false if this is [isHighSurrogate] or [isLowSurrogate]
isValidScalar : CodePoint -> Bool
isValidScalar = \codePoint -> !(isHighSurrogate codePoint || isLowSurrogate codePoint)

## Returns true if this is a [high-surrogate code point](http://www.unicode.org/glossary/#high_surrogate_code_point)
## from U+D800 to U+DBFF
isHighSurrogate : CodePoint -> Bool
isHighSurrogate = \codePoint ->
    u32 = toU32 codePoint
    u32 >= 0xD800 && u32 <= 0xDBFF

## Returns true if this is a [low-surrogate code point](https://www.unicode.org/glossary/#low_surrogate_code_point)
## from U+DC00 to U+DFFF
isLowSurrogate : CodePoint -> Bool
isLowSurrogate = \codePoint ->
    u32 = toU32 codePoint
    u32 >= 0xDC00 && u32 <= 0xDFFF

## Zig docs: bytes the UTF-8 representation would require
## for the given codepoint.
utf8Len : CodePoint -> Result U8 [InvalidCodePoint]
utf8Len = \codePoint ->
    u32 = toU32 codePoint

    if u32 < 0x80 then
        Ok 1
    else if u32 < 0x800 then
        Ok 2
    else if u32 < 0x10000 then
        Ok 3
    else if u32 < 0x110000 then
        Ok 4
    else
        Err InvalidCodePoint

## Encode a Scalar as UTF-8 bytes and append those bytes to an existing list of UTF-8 bytes.
appendUtf8 : List U8, CodePoint -> List U8
appendUtf8 = \bytes, codePoint ->
    u32 = toU32 codePoint
    if u32 < 0x80 then
        List.append bytes (Num.toU8 u32)
    else if u32 < 0x800 then
        byte1 =
            u32
            |> Num.shiftRightBy 6
            |> Num.bitwiseOr 0b11000000
            |> Num.toU8

        byte2 =
            u32
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        bytes
        |> List.reserve 2
        |> List.append byte1
        |> List.append byte2
    else if u32 < 0x10000 then
        byte1 =
            u32
            |> Num.shiftRightBy 12
            |> Num.bitwiseOr 0b11100000
            |> Num.toU8

        byte2 =
            u32
            |> Num.shiftRightBy 6
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        byte3 =
            u32
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        bytes
        |> List.reserve 3
        |> List.append byte1
        |> List.append byte2
        |> List.append byte3
    else
        ## This was an invalid Unicode scalar value, even though it had the Roc type Scalar.
        ## This should never happen!
        expect
            u32 < 0x110000

        byte1 =
            u32
            |> Num.shiftRightBy 18
            |> Num.bitwiseOr 0b11110000
            |> Num.toU8

        byte2 =
            u32
            |> Num.shiftRightBy 12
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        byte3 =
            u32
            |> Num.shiftRightBy 6
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        byte4 =
            u32
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        bytes
        |> List.reserve 4
        |> List.append byte1
        |> List.append byte2
        |> List.append byte3
        |> List.append byte4

isContinuationByte : U8 -> Bool
isContinuationByte = \byte -> Num.bitwiseAnd byte 0b11000000 == 0b10000000

addContinuation : U32, U8 -> U32
addContinuation = \original, continuationByte ->
    original
    |> Num.shiftLeftBy 6
    |> Num.bitwiseOr (Num.toU32 (Num.bitwiseAnd continuationByte 0b00111111))

## The number of UTF-8 bytes it takes to represent this Scalar.
countUtf8Bytes : CodePoint -> U8
countUtf8Bytes = \codePoint ->
    u32 = toU32 codePoint

    if u32 < 0x80 then
        1
    else if u32 < 0x800 then
        2
    else if u32 < 0x10000 then
        3
    else
        # If this expectation fails, it was an invalid Scalar and shouldn't have been allowed!
        expect
            u32 < 0x110000

        4

## parse a 2-byte code point
parse2 : U8, U8 -> Result U32 [OverlongEncoding, ExpectedContinuation]
parse2 = \first, second ->
    if isContinuationByte second then
        answer =
            first
            |> Num.bitwiseAnd 0b00011111
            |> Num.toU32
            |> addContinuation second

        if answer < 0x80 then
            Err OverlongEncoding
        else
            Ok answer
    else
        Err ExpectedContinuation

## parse a 3-byte code point
parse3 : U8, U8, U8 -> Result U32 [OverlongEncoding, ExpectedContinuation, EncodesSurrogateHalf]
parse3 = \first, second, third ->
    if isContinuationByte second && isContinuationByte third then
        answer =
            first
            |> Num.bitwiseAnd 0b00001111
            |> Num.toU32
            |> addContinuation second
            |> addContinuation third

        if answer < 0x80 then
            Err OverlongEncoding
        else if 0xd800 <= answer && answer <= 0xdfff then
            Err EncodesSurrogateHalf
        else
            Ok answer
    else
        Err ExpectedContinuation

## parse a 4-byte code point
parse4 : U8, U8, U8, U8 -> Result U32 [OverlongEncoding, ExpectedContinuation, CodepointTooLarge]
parse4 = \first, second, third, fourth ->
    if isContinuationByte second && isContinuationByte third && isContinuationByte third then
        answer =
            first
            |> Num.bitwiseAnd 0b00001111
            |> Num.toU32
            |> addContinuation second
            |> addContinuation third
            |> addContinuation fourth

        if answer < 0x10000 then
            Err OverlongEncoding
        else if answer > 0x10FFFF then
            Err CodepointTooLarge
        else
            Ok answer
    else
        Err ExpectedContinuation

Utf8ParseErr : [OverlongEncoding, ExpectedContinuation, EncodesSurrogateHalf, InvalidUtf8, ListWasEmpty, CodepointTooLarge]

## Parses a list of bytes into a list of code points
parseUtf8 : List U8 -> Result (List CodePoint) Utf8ParseErr
parseUtf8 = \bytes ->
    # we will have at most List.len bytes code points
    listWithCapacity : List CodePoint
    listWithCapacity = List.withCapacity (List.len bytes)
    parseUtf8Help bytes listWithCapacity

parseUtf8Help : List U8, List CodePoint -> Result (List CodePoint) Utf8ParseErr
parseUtf8Help = \rest, cps ->
    if List.isEmpty rest then
        Ok cps
    else
        parsePartialUtf8 rest
        |> Result.try \{ codePoint, bytesParsed } ->
            parseUtf8Help (List.dropFirst rest bytesParsed) (List.append cps codePoint)

# test simple ASCII "Hello"
expect
    expected = [72, 101, 108, 108, 111, 33] |> List.map fromU32Unchecked |> Ok
    actual = parseUtf8 [72, 101, 108, 108, 111, 33]
    actual == expected

## Parses the first code point found in a list of bytes encoded as UTF-8. Returns `ListWasEmpty`
## if the list was empty, or `InvalidUtf8` if the bytes were not valid UTF-8.
parsePartialUtf8 : List U8 -> Result { codePoint : CodePoint, bytesParsed : U64 } Utf8ParseErr
parsePartialUtf8 = \bytes ->
    # We always try to get the first byte, and if it fails with Err ListWasEmpty, then
    # the whole function should return Err ListWasEmpty. This tells the caller "there's nothing
    # else to parse" as opposed to "There are bytes here, but they're invalid UTF-8."
    firstByte <- List.first bytes |> Result.try

    # Get the byte at the index, or return Err InvalidUtf8 if that's past the end of the list.
    byteAt = \index ->
        List.get bytes index
        |> Result.mapErr \OutOfBounds -> InvalidUtf8

    # Some of the bits in the first byte tell us the total byte length of the code point.
    if firstByte <= 0b0111_1111 then
        # 1-byte code point
        bytesParsed = 1

        Ok {
            codePoint: fromU32Unchecked (Num.toU32 firstByte),
            bytesParsed,
        }
    else if firstByte >= 0b1100_0000 && firstByte <= 0b1101_1111 then
        # 2-byte code point
        secondByte <- byteAt 1 |> Result.try
        bytesParsed = 2

        parse2 firstByte secondByte
        |> Result.map \u32 -> { codePoint: fromU32Unchecked u32, bytesParsed }
    else if firstByte >= 0b1110_0000 && firstByte <= 0b1110_1111 then
        # 3-byte code point
        secondByte <- byteAt 1 |> Result.try
        thirdByte <- byteAt 2 |> Result.try
        bytesParsed = 3

        if Num.bitwiseAnd firstByte 0b11110000 == 0b11100000 then
            parse3 firstByte secondByte thirdByte
            |> Result.map \u32 -> { codePoint: fromU32Unchecked u32, bytesParsed }
        else
            Err InvalidUtf8
    else if firstByte >= 0b1111_0000 && firstByte <= 0b1111_0111 then
        # 4-byte code point
        secondByte <- byteAt 1 |> Result.try
        thirdByte <- byteAt 2 |> Result.try
        fourthByte <- byteAt 3 |> Result.try
        bytesParsed = 4

        if
            Num.bitwiseAnd firstByte 0b11111000
            == 0b11110000
            && Num.bitwiseAnd secondByte 0b11000000
            == 0b10000000
            && Num.bitwiseAnd thirdByte 0b11000000
            == 0b10000000
            && Num.bitwiseAnd fourthByte 0b11000000
            == 0b10000000
        then
            parse4 firstByte secondByte thirdByte fourthByte
            |> Result.map \u32 -> { codePoint: fromU32Unchecked u32, bytesParsed }
        else
            Err InvalidUtf8
    else
        Err InvalidUtf8

toStr : List CodePoint -> Result Str [BadUtf8]
toStr = \cps ->

    # allocated extra space for the extra bytes as some CPs expand into
    # multiple U8s, so this minimises extra allocations
    capacity = List.withCapacity (50 + List.len cps)

    cps
    |> cpsToStrHelp capacity
    |> Str.fromUtf8
    |> Result.onErr \_ -> Err BadUtf8

cpsToStrHelp : List CodePoint, List U8 -> List U8
cpsToStrHelp = \cps, bytes ->
    when cps is
        [] -> bytes
        [cp, ..] ->
            cpsToStrHelp
                (List.dropFirst cps 1)
                (CodePoint.appendUtf8 bytes cp)

## The East Asian Width property in Unicode categorizes characters based on
## their typical width and display behavior in East Asian typography, helping
## to ensure proper alignment and spacing in text layout for languages like
## Chinese, Japanese, and Korean.
EastAsianProperty : [Fullwidth, Wide, Ambiguous, Halfwidth, Neutral, Narrow]

## Computes the "east asian width" property for a given code point.
## See https://www.unicode.org/Public/15.1.0/ucd/EastAsianWidth.txt
eastAsianWidthProperty : CodePoint -> EastAsianProperty
eastAsianWidthProperty = \cp ->
    eaw = cp |> toU32 |> InternalEAW.eastAsianWidthProperty
    when eaw is
        F -> Fullwidth
        W -> Wide
        A -> Ambiguous
        H -> Halfwidth
        N -> Neutral
        Na -> Narrow

## Computes the visual width of a code point as assigned by the Unicode Character Database
visualWidth : CodePoint -> U32
visualWidth = \cp ->
    cp |> toU32 |> InternalEAW.eastAsianWidth

expect
    # test toStr
    cr = fromU32Unchecked 13
    lf = fromU32Unchecked 10

    toStr [cr, lf] == Ok "\r\n"

## Empty input
expect [] |> parsePartialUtf8 == Err ListWasEmpty

## Incorrect continuation byte
expect [0xC3, 0x28] |> parsePartialUtf8 == Err ExpectedContinuation

## Overlong encoding for ASCII character
expect [0xC0, 0xA1] |> parsePartialUtf8 == Err OverlongEncoding

## Overlong encoding for 2-byte character
expect [0xE0, 0x80, 0xAF] |> parsePartialUtf8 == Err OverlongEncoding

## Overlong encoding for 3-byte character
expect [0xF0, 0x80, 0x80, 0xA6] |> parsePartialUtf8 == Err OverlongEncoding

## Invalid four-byte encoding (scalar value too large)
expect [0xF4, 0x90, 0x80, 0x80] |> parsePartialUtf8 == Err CodepointTooLarge

## Invalid first byte (>= 0xF8)
expect [0xF8, 0xA1, 0xA2, 0xA3, 0xA4] |> parsePartialUtf8 == Err InvalidUtf8

## Invalid first byte (>= 0xC0 and <= 0xC1)
expect [0xC0, 0x80] |> parsePartialUtf8 == Err OverlongEncoding

## Multiple valid 1-byte scalars
expect "hello" |> Str.toUtf8 |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 'h', bytesParsed: 1 }

## Multiple valid 3-byte scalars
expect "世界" |> Str.toUtf8 |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked '世', bytesParsed: 3 }

## Valid 1-byte followed by valid multi-byte scalar
expect "lé" |> Str.toUtf8 |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 'l', bytesParsed: 1 }

## Valid 2-byte followed by multiple single-byte scalar
expect "élan" |> Str.toUtf8 |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 'é', bytesParsed: 2 }

## Valid 3-byte followed by valid single-byte scalar
expect "界s" |> Str.toUtf8 |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked '界', bytesParsed: 3 }

## Valid 4-byte followed by valid single-byte scalar
expect "𠜎a" |> Str.toUtf8 |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked '𠜎', bytesParsed: 4 }

## Valid 4-byte followed by valid multi-byte scalar
expect "𠜎爱" |> Str.toUtf8 |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked '𠜎', bytesParsed: 4 }

## Valid 1-byte followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat (Str.toUtf8 "h") [0xC0, 0x80]) |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 'h', bytesParsed: 1 }

## Valid 2-byte followed by multiple single-byte scalar
expect (List.concat (Str.toUtf8 "é") [0xC0, 0x80]) |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 'é', bytesParsed: 2 }

## Valid 3-byte followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat (Str.toUtf8 "爱") [0xC0, 0x80]) |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked '爱', bytesParsed: 3 }

## Valid 4-byte followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat (Str.toUtf8 "𠜎") [0xC0, 0x80]) |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked '𠜎', bytesParsed: 4 }

## Invalid byte within a valid 2-byte character
expect [0xC3, 0xFF] |> parsePartialUtf8 == Err ExpectedContinuation

## Invalid byte within a valid 3-byte character
expect [0xE2, 0x82, 0xFF] |> parsePartialUtf8 == Err ExpectedContinuation

## Invalid byte within a valid 4-byte character
expect [0xF0, 0x9F, 0x98, 0xFF] |> parsePartialUtf8 == Err InvalidUtf8

## Minimum valid 2-byte character
expect [0xC2, 0x80] |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 0x0080, bytesParsed: 2 }

## Maximum valid 2-byte character
expect [0xDF, 0xBF] |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 0x07FF, bytesParsed: 2 }

## Minimum valid 3-byte character
expect [0xE0, 0xA0, 0x80] |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 0x0800, bytesParsed: 3 }

## Maximum valid 3-byte character
expect [0xEF, 0xBF, 0xBF] |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 0xFFFF, bytesParsed: 3 }

## Minimum valid 4-byte character
expect [0xF0, 0x90, 0x80, 0x80] |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 0x10000, bytesParsed: 4 }

## Maximum valid 4-byte character
expect [0xF4, 0x8F, 0xBF, 0xBF] |> parsePartialUtf8 == Ok { codePoint: fromU32Unchecked 0x10FFFF, bytesParsed: 4 }
