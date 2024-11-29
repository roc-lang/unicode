## The purpose of this file is to generate the InternalGBP.roc file.
##
## This file will read the test data from `data/GraphemeBreakProperty-15.1.0.txt`
## parse it and then generate the implementation for each of the GBP properties.
app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.2/1usTzOOACTpnkarBX0ED3gFESzR4ROdAlt1Llf4WFzo.tar.br",
}

import pf.Arg
import pf.File
import "data/GraphemeBreakProperty-15.1.0.txt" as file : Str
import Helpers exposing [CPMeta, PropertyMap]

main =
    when Arg.list! {} |> List.get 1 is
        Err _ -> Task.err (InvalidArguments "USAGE: roc run InternalGBP.roc -- path/to/package/")
        Ok arg -> File.writeUtf8 "$(Helpers.removeTrailingSlash arg)/InternalGBP.roc" template

GBPProp : [CR, LF, Control, Extend, ZWJ, RI, Prepend, SpacingMark, L, V, T, LV, LVT, Other]
GBPMeta : { fromBytes : List U8, property : GBPProp, toStr : Str }

listMeta : List GBPMeta
listMeta =
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

template =
    """
    ## WARNING This file is automatically generated. Do not edit it manually. ##
    module [GBP, fromCP, isExtend, isZWJ]
    import InternalCP exposing [CP, toU32, fromU32Unchecked]

    $(propDefTemplate)
    $(isFuncTemplate)

    $(fromCPTemplate)
    $(testsTemplate)
    """

propDefTemplate : Str
propDefTemplate =

    propStrs =
        listMeta
        |> List.map .toStr
        |> List.map \str -> "$(str)"
        |> Str.joinWith ", "

    """
    GBP : [$(propStrs)]
    """

isFuncTemplate : Str
isFuncTemplate =

    help : Str, GBPProp -> Str
    help = \name, current ->
        exp =
            cpsForProperty current
            |> List.map Helpers.metaToExpression
            |> Str.joinWith " || "

        """

        $(name) : U32 -> Bool
        $(name) = \\u32 -> $(exp)
        """

    # For each GBPProp define a function that returns true if the given code point has that property
    listMeta
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

fromCPTemplate : Str
fromCPTemplate =
    """
    fromCP : CP -> GBP
    fromCP = \\cp ->

        u32 = toU32 cp

        $(isXtemp listMeta "")
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

# HELPERS

parsePropPart : Str -> Result GBPProp [ParsingError]
parsePropPart = \str ->
    when Str.splitOn str "#" is
        [propStr, ..] -> gbpPropParser (Str.trim propStr)
        _ -> Err ParsingError

expect parsePropPart " Prepend # Cf   [6] ARABIC NUMBER SIGN..ARABIC NUMBER MARK ABOVE" == Ok Prepend
expect parsePropPart " CR # Cc       <control-000D>" == Ok CR
expect parsePropPart " Regional_Indicator # So  [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z" == Ok RI

# Parse the file to map between code points and properties
fileMap : List (PropertyMap GBPProp)
fileMap = Helpers.propertyMapFromFile file parsePropPart

# Make a helper that returns a list of code points for the given property
cpsForProperty : GBPProp -> List CPMeta
cpsForProperty = \current ->
    Helpers.filterPropertyMap
        fileMap
        \{ cp, prop } -> if prop == current then Ok cp else Err NotNeeded

# For each property, generate a function that returns true if the given code
# point has that property
isXtemp : List GBPMeta, Str -> Str
isXtemp = \props, buf ->
    when List.first props is
        Err ListWasEmpty ->
            "$(buf)\n        Other\n"

        Ok prop ->
            when prop.property is
                CR ->
                    next = ifXStr "isCR" "CR"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                LF ->
                    next = ifXStr "isLF" "LF"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                Control ->
                    next = ifXStr "isControl" "Control"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                Extend ->
                    next = ifXStr "isExtend" "Extend"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                ZWJ ->
                    next = ifXStr "isZWJ" "ZWJ"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                RI ->
                    next = ifXStr "isRI" "RI"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                Prepend ->
                    next = ifXStr "isPrepend" "Prepend"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                SpacingMark ->
                    next = ifXStr "isSpacingMark" "SpacingMark"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                L ->
                    next = ifXStr "isL" "L"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                V ->
                    next = ifXStr "isV" "V"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                T ->
                    next = ifXStr "isT" "T"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                LV ->
                    next = ifXStr "isLV" "LV"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                LVT ->
                    next = ifXStr "isLVT" "LVT"
                    isXtemp (List.dropFirst props 1) ("$(buf)$(next)")

                Other ->
                    isXtemp (List.dropFirst props 1) buf

ifXStr : Str, Str -> Str
ifXStr = \funcStr, str ->
    "if $(funcStr) u32 then\n        $(str)\n    else "

# Helper to manually generate a test
unicodeHexToTest : (Str, Str) -> Str
unicodeHexToTest = \(hex, gbpExpected) ->
    u32 = hex |> Str.toUtf8 |> Helpers.hexBytesToU32

    """
    expect # test U+$(hex) gives $(gbpExpected)
        gbp = fromCP (fromU32Unchecked $(Num.toStr u32))
        gbp == $(gbpExpected)
    """

gbpPropParser : Str -> Result GBPProp [ParsingError]
gbpPropParser = \input ->
    startsWithProp : GBPMeta -> Result GBPProp [NonPropSequence]
    startsWithProp = \prop ->
        if input |> Str.toUtf8 |> List.startsWith prop.fromBytes then
            Ok prop.property
        else
            Err NonPropSequence

    # see which properties match
    matches : List GBPProp
    matches = listMeta |> List.keepOks startsWithProp

    when matches is
        # take the longest match
        [a, ..] -> Ok a
        _ -> Err ParsingError

expect gbpPropParser "L" == Ok L
expect gbpPropParser "LF" == Ok LF
expect gbpPropParser "LV" == Ok LV
expect gbpPropParser "LVT" == Ok LVT
expect gbpPropParser "Other" == Ok Other
expect gbpPropParser "# ===" == Err ParsingError
