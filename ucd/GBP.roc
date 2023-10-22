app "gen"
    packages { 
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.1.0/vPU-UZbWGIXsAfcJvAnmU3t3SWlHoG_GauZpqzJiBKA.tar.br",
    }
    imports [
        pf.Stdout, 
        pf.Stderr, 
        pf.Task.{ Task },
        pf.Path.{Path },
        pf.Arg,
        pf.File,
        parser.Core.{ Parser, buildPrimitiveParser },
        parser.String.{ parseStr },
        # "GraphemeBreakProperty-15.1.0.txt" as gbpFile : Str,
        "GBPTemplate.roc" as template : Str,
    ]
    provides [main] to pf

CodePoint : U32
GraphemeBreakProperty : [
    CR,
    LF, 
    Control,
    Extend,
    ZWL,
    RI,
    Prepend,
    SpacingMark,
    L,
    V,
    T,
    LV,
    LVT,
    Other,
]

main : Task {} I32
main =
    getFilePath 
    |> Task.await writeToFile 
    |> Task.onErr \err -> Stderr.line "\(err)"

getFilePath : Task Path Str
getFilePath = 
    args <- Arg.list |> Task.await

    when args |> List.get 1 is 
        Ok arg -> Task.ok (Path.fromStr "\(removeTrailingSlash arg)/InternalGBP.roc")
        Err _ -> Task.err "USAGE: roc run InternalGBP.roc -- path/to/package/"

writeToFile : Path -> Task {} Str 
writeToFile = \path ->
    File.writeUtf8 path template
    |> Task.mapErr \_ -> "ERROR: unable to write to \(Path.display path)"
    |> Task.await \_ -> Stdout.line "\nSucessfully wrote \(lineCountStr) lines to \(Path.display path)\n"

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

lineCountStr = 
    template
    |> Str.split "\n"
    |> List.len
    |> Num.toStr

props : List {bytes : List U8, property : GraphemeBreakProperty, len : Nat}
props =
    # NOTE ordering matters here, e.g. L after LV and LVT
    # to match on longest first
    [
        { bytes: Str.toUtf8 "CR", property: CR},
        { bytes: Str.toUtf8 "Control", property: Control},
        { bytes: Str.toUtf8 "Extend", property: Extend},
        { bytes: Str.toUtf8 "ZWL", property: ZWL},
        { bytes: Str.toUtf8 "RI", property: RI},
        { bytes: Str.toUtf8 "Prepend", property: Prepend},
        { bytes: Str.toUtf8 "SpacingMark", property: SpacingMark},
        { bytes: Str.toUtf8 "V", property: V},
        { bytes: Str.toUtf8 "T", property: T},
        { bytes: Str.toUtf8 "LF", property: LF}, 
        { bytes: Str.toUtf8 "LVT", property: LVT},
        { bytes: Str.toUtf8 "LV", property: LV},
        { bytes: Str.toUtf8 "L", property: L},
        { bytes: Str.toUtf8 "Other", property: Other},
    ]
    |> List.map \{bytes, property} -> {bytes, property, len: List.len bytes}

graphemePropertyParser : Parser (List U8) GraphemeBreakProperty
graphemePropertyParser = 

    input <- buildPrimitiveParser

    matches : List { val : GraphemeBreakProperty, input : List U8}
    matches = 
        props
        |> List.keepOks \prop ->
            if List.startsWith input prop.bytes then 
                Ok prop
            else 
                Err "not used"
        |> List.map \{property, len} -> 
            { val : property, input : List.drop input len }
        
    when matches is 
        [a, ..] -> Ok a # take the longest match
        _ -> Err (ParsingFailure "Not a GBP")

expect parseStr graphemePropertyParser "L" == Ok L
expect parseStr graphemePropertyParser "LF" == Ok LF
expect parseStr graphemePropertyParser "LV" == Ok LV
expect parseStr graphemePropertyParser "LVT" == Ok LVT
expect parseStr graphemePropertyParser "Other" == Ok Other
expect parseStr graphemePropertyParser "# ===" == Err (ParsingFailure "Not a GBP")

codePointParser : Parser (List U8) CodePoint
codePointParser =
    input <- buildPrimitiveParser

    { val: hexBytes, rest} = takeHexBytes {val: [], rest:input}

    when hexBytes is
        [] -> Err (ParsingFailure "No hex bytes")
        _ -> Ok {
            val: hexBytesToU32 hexBytes, 
            input: List.drop rest (List.len hexBytes),
        }

expect parseStr codePointParser "0000" == Ok 0
expect parseStr codePointParser "16FF1" == Ok 94193
expect parseStr codePointParser "# ===" == Err (ParsingFailure "No hex bytes")

hexBytesToU32 : List U8 -> CodePoint
hexBytesToU32 = \bytes ->
    bytes 
    |> List.reverse 
    |> List.walkWithIndex 0 \accum, byte, i -> accum + (Num.powInt 16 (Num.toU32 i))*(hexToDec byte)
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

takeHexBytes : { val : List U8, rest : List U8} -> { val : List U8, rest : List U8}
takeHexBytes = \input ->
    when input.rest is 
        [] -> input 
        [first, ..] -> 
            if first |> isHex then 
                # take the first hex byte and continue 
                takeHexBytes { 
                    val :  input.val |> List.append first, 
                    rest : input.rest |> List.drop 1,
                }
            else 
                input

expect 
    bytes = [35, 32, 61, 61, 61] # "# ==="
    takeHexBytes {val: [], rest: bytes} == {val: [], rest: bytes} 

expect 
    bytes = [68, 54, 69, 49, 46, 46, 68, 54, 70, 66, 32, 32] # "D6E1..D6FB  "
    takeHexBytes {val: [], rest: bytes} == {val: [68, 54, 69, 49], rest: [46, 46, 68, 54, 70, 66, 32, 32]}

isHex : U8 -> Bool
isHex = \u8 ->
    u8 == '0' ||
    u8 == '1' ||
    u8 == '2' ||
    u8 == '3' ||
    u8 == '4' ||
    u8 == '5' ||
    u8 == '6' ||
    u8 == '7' ||
    u8 == '8' ||
    u8 == '9' ||
    u8 == 'A' ||
    u8 == 'B' ||
    u8 == 'C' ||
    u8 == 'D' ||
    u8 == 'E' ||
    u8 == 'F'

expect isHex '0'
expect isHex 'A'
expect isHex 'F'
expect !(isHex ';')
expect !(isHex '#')

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