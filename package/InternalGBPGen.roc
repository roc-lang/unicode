app "gen"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Path.{ Path },
        pf.Arg,
        pf.File,
        "GraphemeBreakProperty-15.1.0.txt" as gbpFile : Str,
        Helpers,
    ]
    provides [main] to pf

main : Task {} I32
main =
    getFilePath
    |> Task.await writeToFile
    |> Task.onErr \err -> Stderr.line "\(err)"

# TASKS
# TODO move these to a common helper file once module changes and builtin Task are available

getFilePath : Task Path Str
getFilePath =
    args <- Arg.list |> Task.await

    when args |> List.get 1 is
        Ok arg -> Task.ok (Path.fromStr "\(Helpers.removeTrailingSlash arg)/InternalGBP.roc")
        Err _ -> Task.err "USAGE: roc run InternalGBP.roc -- path/to/package/"

writeToFile : Path -> Task {} Str
writeToFile = \path ->
    File.writeUtf8 path template
    |> Task.mapErr \_ -> "ERROR: unable to write to \(Path.display path)"
    |> Task.await \_ -> Stdout.line "\nSucessfully wrote to \(Path.display path)\n"

# 1. PROCESS FILE

gbpFilePropertyMap : List { cp : CPMeta, prop : GBP }
gbpFilePropertyMap =
    gbpFile
    |> Str.split "\n"
    |> List.keepOks Helpers.startsWithHex
    |> List.map \l ->
        when Str.split l ";" is
            [hexPart, propPart] ->
                when (parseHexPart hexPart, parsePropPart propPart) is
                    (Ok cp, Ok prop) -> { cp, prop }
                    _ -> crash "Error parsing line -- \(l)"

            _ -> crash "Error unexpected ';' on line -- \(l)"

parseHexPart : Str -> Result CPMeta [ParsingError]
parseHexPart = \hexPart ->
    when hexPart |> Str.trim |> Str.split ".." is
        [single] ->
            when codePointParser single is
                Ok a -> Ok (Single a)
                Err _ -> Err ParsingError

        [start, end] ->
            when (codePointParser start, codePointParser end) is
                (Ok a, Ok b) -> Ok (Range a b)
                _ -> Err ParsingError

        _ -> Err ParsingError

expect parseHexPart "0890..0891    " == Ok (Range 2192 2193)
expect parseHexPart "08E2          " == Ok (Single 2274)

parsePropPart : Str -> Result GBP [ParsingError]
parsePropPart = \str ->
    when Str.split str "#" is
        [propStr, ..] -> graphemePropertyParser (Str.trim propStr)
        _ -> Err ParsingError

expect parsePropPart " Prepend # Cf   [6] ARABIC NUMBER SIGN..ARABIC NUMBER MARK ABOVE" == Ok Prepend
expect parsePropPart " CR # Cc       <control-000D>" == Ok CR
expect parsePropPart " Regional_Indicator # So  [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z" == Ok RI

cpMapToExpression : CPMeta -> Str
cpMapToExpression = \cp ->
    when cp is
        Single a -> "(u32 == \(Num.toStr a))"
        Range a b -> "(u32 >= \(Num.toStr a) && u32 <= \(Num.toStr b))"

cpsForProperty : GBP -> List CPMeta
cpsForProperty = \current ->
    gbpFilePropertyMap
    |> List.keepOks \{ cp, prop } -> if prop == current then Ok cp else Err NotNeeded

isXYZTemplate : Str
isXYZTemplate =

    help : Str, GBP -> Str
    help = \name, current ->
        exp =
            cpsForProperty current
            |> List.map cpMapToExpression
            |> Str.joinWith " || "

        """

        \(name) : U32 -> Bool
        \(name) = \\u32 -> \(exp)
        """

    # For each GBP define a function that returns true if the given code point has that property
    listGBP
    |> List.keepOks \{ property } ->
        when property is
            CR -> help "isCR" CR |> Ok
            LF -> help "isLF" LF |> Ok
            Control -> help "isControl" Control |> Ok
            Extend -> help "isExtend" Extend |> Ok
            ZWJ -> help "isZWJ" ZWJ |> Ok
            RI -> help "isRI" RI |> Ok
            Prepend -> help "isPrepend" Prepend |> Ok
            SpacingMark -> help "isSpacingMark" SpacingMark |> Ok
            L -> help "isL" L |> Ok
            V -> help "isV" V |> Ok
            T -> help "isT" T |> Ok
            LV -> help "isLV" LV |> Ok
            LVT -> help "isLVT" LVT |> Ok
            Other -> Err NotUsed
    |> Str.joinWith "\n"

gbpDefinitionTemplate : Str
gbpDefinitionTemplate =

    propStrs =
        listGBP
        |> List.map .toStr
        |> List.map \str -> "\(str)"
        |> Str.joinWith ", "

    """
    GBP : [\(propStrs)]
    """

fromCPTemplate : Str
fromCPTemplate =
    """
    fromCP : CP -> GBP
    fromCP = \\cp -> 
        
        u32 = toU32 cp

        \(isXtemp listGBP "")
    """

isXtemp : List GBPMeta, Str -> Str
isXtemp = \props, buf ->
    when List.first props is
        Err ListWasEmpty ->
            "\(buf)\n        Other\n"

        Ok prop ->
            when prop.property is
                CR ->
                    next = ifGbpStr "isCR" "CR"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                LF ->
                    next = ifGbpStr "isLF" "LF"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                Control ->
                    next = ifGbpStr "isControl" "Control"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                Extend ->
                    next = ifGbpStr "isExtend" "Extend"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                ZWJ ->
                    next = ifGbpStr "isZWJ" "ZWJ"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                RI ->
                    next = ifGbpStr "isRI" "RI"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                Prepend ->
                    next = ifGbpStr "isPrepend" "Prepend"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                SpacingMark ->
                    next = ifGbpStr "isSpacingMark" "SpacingMark"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                L ->
                    next = ifGbpStr "isL" "L"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                V ->
                    next = ifGbpStr "isV" "V"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                T ->
                    next = ifGbpStr "isT" "T"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                LV ->
                    next = ifGbpStr "isLV" "LV"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                LVT ->
                    next = ifGbpStr "isLVT" "LVT"
                    isXtemp (List.drop props 1) ("\(buf)\(next)")

                Other ->
                    isXtemp (List.drop props 1) buf

ifGbpStr : Str, Str -> Str
ifGbpStr = \funcStr, gbpStr ->
    "if \(funcStr) u32 then\n        \(gbpStr)\n    else "

unicodeHexToTest : (Str, Str) -> Str 
unicodeHexToTest = \(hex, gbpExpected) ->
    u32 = hex |> Str.toUtf8 |> Helpers.hexBytesToU32

    """
    expect # test U+\(hex) gives \(gbpExpected)
        gbp = fromCP (fromU32Unchecked \(Num.toStr u32)) 
        gbp == \(gbpExpected)
    """

testsTemplate : Str
testsTemplate = 
    [
        ("000D", "CR"),
        ("000A", "LF"),
        ("200D", "ZWJ"),
        ("17BF", "SpacingMark"),
        ("A960", "L"),
        ("D7C6", "V"),
        ("11A8", "T"),
        ("AC00", "LV"),
        ("AC04", "LVT"),
        ("B93D", "LVT"),
        ("0041", "Other"),
        ("D7CD", "T"),
        ("1160", "V"),
        ("D7C6", "V"),
        ("1E2AE", "Extend"),
        ("FFF0", "Control"),
        ("1D17A", "Control"),
        ("06DD", "Prepend"),
    ]
    |> List.map unicodeHexToTest
    |> Str.joinWith "\n\n"

# 2. BUILD TEMPLATE

template =
    """
    ## WARNING This file is automatically generated. Do not edit it manually. ##
    interface InternalGBP
        exposes [GBP, fromCP]
        imports [InternalCP.{ CP, toU32, fromU32Unchecked }]

    \(gbpDefinitionTemplate)
    \(isXYZTemplate)

    \(fromCPTemplate)
    \(testsTemplate)
    """

# HELPERS

graphemePropertyParser : Str -> Result GBP [ParsingError]
graphemePropertyParser = \input ->

    startsWithProp : GBPMeta -> Result GBP [NonGBP]
    startsWithProp = \prop ->
        if input |> Str.toUtf8 |> List.startsWith prop.fromBytes then
            Ok prop.property
        else
            Err NonGBP

    # see which properties match
    matches : List GBP
    matches = listGBP |> List.keepOks startsWithProp

    when matches is
        # take the longest match
        [a, ..] -> Ok a
        _ -> Err ParsingError

expect graphemePropertyParser "L" == Ok L
expect graphemePropertyParser "LF" == Ok LF
expect graphemePropertyParser "LV" == Ok LV
expect graphemePropertyParser "LVT" == Ok LVT
expect graphemePropertyParser "Other" == Ok Other
expect graphemePropertyParser "# ===" == Err ParsingError

codePointParser : Str -> Result U32 [ParsingError]
codePointParser = \input ->

    { val: hexBytes } = Helpers.takeHexBytes { val: [], rest: Str.toUtf8 input }

    when hexBytes is
        [] -> Err ParsingError
        _ -> Ok (Helpers.hexBytesToU32 hexBytes)

expect codePointParser "0000" == Ok 0
expect codePointParser "16FF1" == Ok 94193
expect codePointParser "# ===" == Err ParsingError

GBP : [CR, LF, Control, Extend, ZWJ, RI, Prepend, SpacingMark, L, V, T, LV, LVT, Other]

GBPMeta : { fromBytes : List U8, property : GBP, toStr : Str }
CPMeta : [Single U32, Range U32 U32]

listGBP : List GBPMeta
listGBP =
    # NOTE ordering matters here, e.g. L after LV and LVT
    # to match on longest first
    [
        { fromBytes: Str.toUtf8 "CR", property: CR, toStr: "CR" },
        { fromBytes: Str.toUtf8 "Control", property: Control, toStr: "Control" },
        { fromBytes: Str.toUtf8 "Extend", property: Extend, toStr: "Extend" },
        { fromBytes: Str.toUtf8 "ZWJ", property: ZWJ, toStr: "ZWJ" },
        { fromBytes: Str.toUtf8 "Regional_Indicator", property: RI, toStr: "RI" },
        { fromBytes: Str.toUtf8 "Prepend", property: Prepend, toStr: "Prepend" },
        { fromBytes: Str.toUtf8 "SpacingMark", property: SpacingMark, toStr: "SpacingMark" },
        { fromBytes: Str.toUtf8 "V", property: V, toStr: "V" },
        { fromBytes: Str.toUtf8 "T", property: T, toStr: "T" },
        { fromBytes: Str.toUtf8 "LF", property: LF, toStr: "LF" },
        { fromBytes: Str.toUtf8 "LVT", property: LVT, toStr: "LVT" },
        { fromBytes: Str.toUtf8 "LV", property: LV, toStr: "LV" },
        { fromBytes: Str.toUtf8 "L", property: L, toStr: "L" },
        { fromBytes: Str.toUtf8 "Other", property: Other, toStr: "Other" },
    ]
