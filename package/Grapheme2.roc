interface Grapheme2
    exposes [
        Grapheme,
        split,
    ]
    imports [
        CodePoint.{ CodePoint, Utf8ParseErr },
        InternalGBP.{ GBP },
        InternalCP.{fromU32Unchecked},
        # InternalEmoji,
    ]

## Extended Grapheme Cluster
Grapheme : InternalGBP.GBP

## Split a string into extended grapheme clusters
split : Str -> Result (List Str) Utf8ParseErr
split = \str ->
    
    # Note I'm not sure if we should return an error here or just crash. 
    # A Roc Str should be be valid utf8 and so in theory it should not be possible 
    # for split to have invalid utf8 in it. To be discussed.
    codePoints <- str |> Str.toUtf8 |> CodePoint.parseUtf8 |> Result.map

    breakPoints = codePoints |> List.map InternalGBP.fromCP

    splitHelp Next codePoints breakPoints [BR GB1] |> toListStr
    
# Refer to https://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules
Rule : [GB1, GB2, GB3, GB4, GB5, GB6, GB7, GB8, GB9, GB9a, GB9b, GB9c, GB11, GB12, GB13, GB999]

OutState : List [
    BR Rule,
    NB Rule, 
    CP CodePoint,
]

toListStr : OutState -> List Str 
toListStr = \outState ->
    List.walk outState { acc : [], cps : [] } \state, curr ->
        when curr is 
            NB _ -> state 
            BR _ -> if List.isEmpty state.acc then state else { acc : [], cps: List.append state.cps state.acc}
            CP cp -> { acc : List.append state.acc cp, cps: state.cps}
    |> .cps
    |> List.map \cpList -> 
        # Again I'm not sure if we should crash here... I dont think CodePoint.toStr
        # should be returning a result... to be discussed.
        when CodePoint.toStr cpList is 
            Ok str -> str
            Err _ -> crash "unreachable got invalid utf8 when converting CodePoints to Str"

splitHelp : State, List CodePoint, List GBP, OutState -> OutState
splitHelp = \state, codePoints, breakPoints, acc ->
    nextCPs = List.dropFirst codePoints 1
    nextBPs = List.dropFirst breakPoints 1
    when (state, codePoints, breakPoints) is
        (_, [], []) -> List.append acc (BR GB2) # base case
        (Next, [cp, ..], [bp, ..]) if bp == CR -> splitHelp (AfterCR cp) nextCPs nextBPs acc
        (Next, [cp, ..], _) -> splitHelp Break nextCPs nextBPs (List.append acc (CP cp))
        (AfterCR prev, [cp, ..], [bp, ..]) if bp == LF -> splitHelp (AfterCR cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB3, CP cp])
        (AfterCR prev, [cp, ..], _) -> splitHelp (AfterCR cp) nextCPs nextBPs (List.concat acc [CP prev, BR GB4, CP cp])
        (Break, _, _) -> splitHelp Next codePoints breakPoints (List.append acc (BR GB999))

        _ -> splitHelp Break nextCPs nextBPs acc

State : [
    Next,
    Break, # otherwise break everywhere
    AfterCR CodePoint, # need to look ahead to next gbp
    AfterHungulL,
    AfterHungulLVorV,
    AfterHungulLVTorT,
    AfterRI,
]

# Test break everywhere
expect 
    a = testHelp [[888], [888]]
    b = [BR GB1, CP (fromU32Unchecked 888), BR GB999, CP (fromU32Unchecked 888), BR GB2]
    a == b

# Test no break between CRLF starting character
expect 
    a = testHelp [[888], [13, 10]]
    b = [BR GB1, CP (fromU32Unchecked 888), BR GB999, CP (fromU32Unchecked 13), NB GB3, CP (fromU32Unchecked 10), BR GB2]
    a == b

# Test no break between CRLF in the middle of the sequence
expect 
    a = testHelp [[888], [13, 10]]
    b = [BR GB1, CP (fromU32Unchecked 888), BR GB999, CP (fromU32Unchecked 13), NB GB3, CP (fromU32Unchecked 10), BR GB2]
    a == b

testHelp : List (List U32) -> OutState
testHelp = \u32List -> 
    codePoints = u32List |> List.join |> List.map fromU32Unchecked
    breakPoints = codePoints |> List.map InternalGBP.fromCP
    
    splitHelp Next codePoints breakPoints [BR GB1]
    