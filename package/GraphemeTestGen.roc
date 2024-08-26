## The purpose of this file is to generate the GraphemeTest.roc test suite.
##
## This file will read the test data from `data/GraphemeBreakTest-15.1.0.txt`
## parse it and then generate the individual tests.
app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.2/1usTzOOACTpnkarBX0ED3gFESzR4ROdAlt1Llf4WFzo.tar.br",
}

import pf.Task exposing [Task]
import pf.Arg
import pf.File
import parser.Core exposing [Parser, const, keep, skip, oneOf, oneOrMore, many, chompWhile]
import parser.String exposing [parseStr, string, codeunit, codeunitSatisfies]
import "data/GraphemeBreakTest-15.1.0.txt" as inputFile : Str
import Helpers exposing [hexBytesToU32]
import CodePoint exposing [CodePoint]
import InternalCP exposing [fromU32Unchecked]

Rule : [GB1, GB2, GB3, GB4, GB5, GB6, GB7, GB8, GB9, GB9a, GB9b, GB9c, GB11, GB12, GB13, GB999]
TestTokens : List [BR Rule, NB Rule, CP CodePoint]

main =
    when Arg.list! {} |> List.get 1 is
        Err _ -> Task.err (InvalidArguments "USAGE: roc run GraphemeTest.roc -- path/to/package/")
        Ok arg -> File.writeUtf8 "$(Helpers.removeTrailingSlash arg)/GraphemeTest.roc" template

template : Str
template =

    tests =
        testFile
        |> List.keepIf \test ->
            # filter out any tests which aren't passing yet
            # progressively add these as support is added
            test.parsed
            |> List.keepIf \token ->
                when token is
                    BR rule if rule == GB9a || rule == GB9b || rule == GB9c -> Bool.true
                    NB rule if rule == GB9a || rule == GB9b || rule == GB9c -> Bool.true
                    _ -> Bool.false
            |> List.isEmpty
        |> List.map \test ->
            # test : {lineNo : U16, lineStr : Str, parsed : List [BR Rule, NB Rule, CP CodePoint]}

            sanitisedLine =
                test.lineStr
                |> Str.replaceEach "÷" "%" # replace %
                |> Str.replaceEach "×" "x" # replace X
                |> Str.replaceEach "	" " " # replace tabs with a space

            codePointsList = test.parsed |> toU32List

            """

            # GraphemeBreakTest-15.1.0.txt:line $(Num.toStr test.lineNo)
            # $(sanitisedLine)
            expect
                exp = Ok $(codePointsList |> Inspect.toStr)
                got =
                    $(codePointsList
            |> List.join
            |> Inspect.toStr)
                    |> List.map InternalCP.fromU32Unchecked
                    |> CodePoint.toStr
                    |> Result.try Grapheme.split
                    |> Result.map toCodePointList

                got == exp
            """
        |> Str.joinWith "\n"

    """
    ## WARNING This file is automatically generated. Do not edit it manually. ##
    module []

    import CodePoint
    import Grapheme
    import InternalCP

    toCodePointList : List Str -> List (List U32)
    toCodePointList = \\strings ->
        strings |> List.map \\str ->
            when str |> Str.toUtf8 |> CodePoint.parseUtf8 is
                Ok cps -> List.map cps CodePoint.toU32
                Err _ -> crash \"expected valid utf8\"

    $(tests)
    """

toU32List : TestTokens -> List (List U32)
toU32List = \tokens ->

    toU32s = \cps -> cps |> List.map CodePoint.toU32

    go : TestTokens, List CodePoint, List (List U32) -> List (List U32)
    go = \remaining, acc, strs ->
        next = List.dropFirst remaining 1
        when remaining is
            [] -> if List.isEmpty acc then strs else List.append strs (toU32s acc)
            [BR _, ..] -> go next [] (if List.isEmpty acc then strs else List.append strs (toU32s acc))
            [NB _, ..] -> go next acc strs
            [CP cp, ..] -> go next (List.append acc cp) strs

    go tokens [] []

testFile : List { lineNo : U16, lineStr : Str, parsed : TestTokens }
testFile =
    inputFile
    |> Str.split "\n"
    |> List.mapWithIndex \lineStr, idx -> { lineNo: Num.toU16 (idx + 1), lineStr, parsed: [] }
    |> List.keepIf \test -> Str.startsWith test.lineStr "÷ "
    |> List.map \test ->
        when parseStr testParser test.lineStr is
            Ok (Ok parsed) -> { test & parsed }
            Err err | Ok (Err err) -> crash "Unable to parse line $(Num.toStr test.lineNo) got err $(Inspect.toStr err)"

testParser : Parser (List U8) (Result TestTokens _)
testParser =
    const (\first -> \second -> zip first second |> Result.map List.reverse)
    |> keep firstHalfParser
    |> skip (string "	#  ")
    |> keep secondHalfParser

expect
    parseStr testParser "÷ 0020 ÷ 0020 ÷	#  ÷ [0.2] SPACE (Other) ÷ [999.0] SPACE (Other) ÷ [0.3]"
    == Ok (Ok [BR GB1, CP (fromU32Unchecked 32), BR GB999, CP (fromU32Unchecked 32), BR GB2])

expect
    parseStr testParser "÷ 0020 ÷ 000D ÷	#  ÷ [0.2] SPACE (Other) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
    == Ok (Ok [BR GB1, CP (fromU32Unchecked 32), BR GB5, CP (fromU32Unchecked 13), BR GB2])

codePointParser : Parser (List U8) CodePoint
codePointParser =

    isHex = \b -> (b >= '0' && b <= '9') || (b >= 'A' && b <= 'F')

    const
        (\bytes ->
            bytes
            |> hexBytesToU32
            |> fromU32Unchecked
        )
    |> keep (oneOrMore (codeunitSatisfies isHex))

expect parseStr codePointParser "094D" == Ok (fromU32Unchecked 2381)
expect parseStr codePointParser "1F1E6" == Ok (fromU32Unchecked 127462)

breakRuleParser : Parser (List U8) Rule
breakRuleParser =
    oneOf [
        const GB1 |> skip (string "[0.2]"),
        const GB2 |> skip (string "[0.3]"),
        const GB3 |> skip (string "[3.0] "),
        const GB4 |> skip (string "[4.0]"),
        const GB5 |> skip (string "[5.0]"),
        const GB6 |> skip (string "[6.0]"),
        const GB7 |> skip (string "[7.0]"),
        const GB8 |> skip (string "[8.0]"),
        const GB9 |> skip (string "[9.0]"),
        const GB9a |> skip (string "[9.1]"),
        const GB9b |> skip (string "[9.2]"),
        const GB9c |> skip (string "[9.3]"),
        const GB11 |> skip (string "[11.0]"),
        const GB12 |> skip (string "[12.0]"),
        const GB13 |> skip (string "[13.0]"),
        const GB999 |> skip (string "[999.0]"),
    ]

expect parseStr breakRuleParser "[999.0]" == Ok GB999
expect parseStr breakRuleParser "[0.2]" == Ok GB1

firstHalfParser : Parser (List U8) (List [BR, NB, CP CodePoint])
firstHalfParser =
    many
        (
            oneOf [
                const BR |> skip (string "÷"),
                const NB |> skip (string "×"),
                const CP |> skip (codeunit ' ') |> keep codePointParser |> skip (codeunit ' '),
            ]
        )

expect parseStr firstHalfParser "÷ 0020 ÷ 0020 ÷" == Ok [BR, CP (fromU32Unchecked 32), BR, CP (fromU32Unchecked 32), BR]

secondHalfParser : Parser (List U8) (List [BR Rule, NB Rule])
secondHalfParser =
    # NOTE Str.toUtf8 " ÷ " == [32, 195, 183, 32] : List U8
    # NOTE Str.toUtf8 " × " == [32, 195, 151, 32] : List U8
    many
        (
            oneOf [
                const BR
                |> skip (string "÷")
                |> skip (codeunit ' ')
                |> keep breakRuleParser
                |> skip (chompWhile \b -> b != 195),
                const NB
                |> skip (string "×")
                |> skip (codeunit ' ')
                |> keep breakRuleParser
                |> skip (chompWhile \b -> b != 195),
            ]
        )

expect parseStr secondHalfParser "÷ [0.2] SPACE (Other) ÷ [999.0] SPACE (Other) ÷ [0.3]" == Ok [BR GB1, BR GB999, BR GB2]

# First half of the line with the test has the CodePoints we want, but the second half
# has the rules used which we also want, let's combine these to make life simpler and
# so we can test our implementation with more confidence by checking we apply the correct
# rule in the correct locations
zip : List [BR, NB, CP CodePoint], List [BR Rule, NB Rule] -> Result (List [BR Rule, NB Rule, CP CodePoint]) [Invalid Str]
zip = \first, second ->
    when (List.first first, List.first second) is
        (Ok BR, Ok (BR rule)) ->
            next = zip? (List.dropFirst first 1) (List.dropFirst second 1)

            Ok (List.append next (BR rule))

        (Ok NB, Ok (NB rule)) ->
            next = zip? (List.dropFirst first 1) (List.dropFirst second 1)

            Ok (List.append next (NB rule))

        (Ok (CP cp), _) ->
            next = zip? (List.dropFirst first 1) second

            Ok (List.append next (CP cp))

        (Err _, Err _) -> Ok [] # base case
        _ -> Err (Invalid "expected first and second lists to match exactly got $(Inspect.toStr (T first second))")

expect
    answer =
        zip [BR, CP (fromU32Unchecked 35), BR, CP (fromU32Unchecked 32), BR] [BR GB1, BR GB999, BR GB2]
        |> Result.map List.reverse
        |> Result.withDefault []

    answer == [BR GB1, CP (fromU32Unchecked 35), BR GB999, CP (fromU32Unchecked 32), BR GB2]
