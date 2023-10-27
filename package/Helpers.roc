interface Helpers 
    exposes [
        removeTrailingSlash,
        takeHexBytes,
        isHex,
        hexToDec,
        startsWithHex,
        hexBytesToU32,
    ]
    imports []

removeTrailingSlash : Str -> Str
removeTrailingSlash = \str ->
    trimmed = str |> Str.trim
    reversed = trimmed |> Str.toUtf8 |> List.reverse

    when reversed is
        [a, ..] if a == '/' ->
            reversed
            |> List.drop 1
            |> List.reverse
            |> Str.fromUtf8
            |> Result.withDefault ""

        _ -> trimmed

expect removeTrailingSlash "abc  " == "abc"
expect removeTrailingSlash "  abc/package/  " == "abc/package"

takeHexBytes : { val : List U8, rest : List U8 } -> { val : List U8, rest : List U8 }
takeHexBytes = \input ->
    when input.rest is
        [] -> input
        [first, ..] ->
            if first |> isHex then
                # take the first hex byte and continue
                takeHexBytes {
                    val: input.val |> List.append first,
                    rest: input.rest |> List.drop 1,
                }
            else
                input

expect
    bytes = [35, 32, 61, 61, 61] # "# ==="
    takeHexBytes { val: [], rest: bytes } == { val: [], rest: bytes }

expect
    bytes = [68, 54, 69, 49, 46, 46, 68, 54, 70, 66, 32, 32] # "D6E1..D6FB  "
    takeHexBytes { val: [], rest: bytes } == { val: [68, 54, 69, 49], rest: [46, 46, 68, 54, 70, 66, 32, 32] }

isHex : U8 -> Bool
isHex = \u8 -> u8 == '0' || u8 == '1' || u8 == '2' || u8 == '3' || u8 == '4' || u8 == '5' || u8 == '6' || u8 == '7' || u8 == '8' || u8 == '9' || u8 == 'A' || u8 == 'B' || u8 == 'C' || u8 == 'D' || u8 == 'E' || u8 == 'F'

expect isHex '0'
expect isHex 'A'
expect isHex 'F'
expect !(isHex ';')
expect !(isHex '#')

startsWithHex : Str -> Result Str [NonHex]
startsWithHex = \str ->
    when Str.toUtf8 str is
        [a, ..] if isHex a -> Ok str
        _ -> Err NonHex

expect startsWithHex "# ===" == Err NonHex
expect startsWithHex "0000.." == Ok "0000.."

hexBytesToU32 : List U8 -> U32
hexBytesToU32 = \bytes ->
    bytes
    |> List.reverse
    |> List.walkWithIndex 0 \accum, byte, i -> accum + (Num.powInt 16 (Num.toU32 i)) * (hexToDec byte)
    |> Num.toU32

expect hexBytesToU32 ['0', '0', '0', '0'] == 0
expect hexBytesToU32 ['0', '0', '0', '1'] == 1
expect hexBytesToU32 ['0', '0', '0', 'F'] == 15
expect hexBytesToU32 ['0', '0', '1', '0'] == 16
expect hexBytesToU32 ['0', '0', 'F', 'F'] == 255
expect hexBytesToU32 ['0', '1', '0', '0'] == 256
expect hexBytesToU32 ['0', 'F', 'F', 'F'] == 4095
expect hexBytesToU32 ['1', '0', '0', '0'] == 4096
expect hexBytesToU32 ['1', '6', 'F', 'F', '1'] == 94193

hexToDec : U8 -> U32
hexToDec = \byte ->
    when byte is
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'A' -> 10
        'B' -> 11
        'C' -> 12
        'D' -> 13
        'E' -> 14
        'F' -> 15
        _ -> 0

expect hexToDec '0' == 0
expect hexToDec 'F' == 15
