interface Grapheme2
    exposes [
        Grapheme,
        split,
    ]
    imports [
        CodePoint.{ CodePoint, Utf8ParseErr },
        InternalGBP.{ GBP },
        InternalCP.{fromU32Unchecked},
        InternalEmoji,
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

    splitHelp Start codePoints breakPoints |> toListStr
    
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
            BR _ -> { acc : [], cps: List.append state.cps state.acc}
            CP cp -> { acc : List.append state.acc cp, cps: state.cps}
    |> .cps
    |> List.map \cpList -> 
        # Again I'm not sure if we should crash here... I dont think CodePoint.toStr
        # should be returning a result... to be discussed.
        when CodePoint.toStr cpList is 
            Ok str -> str
            Err _ -> crash "unreachable got invalid utf8 when converting CodePoints to Str"

splitHelp : State, List CodePoint, List GBP -> OutState
splitHelp = \state, codePoints, breakPoints ->
    next = List.dropFirst combined 1
    when (state, code) is 
        [] -> [BR GB2] # base case
        [T cp _, ..] -> List.concat [CP cp, BR GB999] (splitHelp next) 

State : [
    Start,
]

expect 
    a = testHelp "abc"
    b = [
        BR GB1, 
        CP (cpFromU8 'a'), 
        BR GB999, 
        CP (cpFromU8 'b'), 
        BR GB999, 
        CP (cpFromU8 'c'), 
        BR GB999, 
        BR GB2,
    ]
    a == b


cpFromU8 : U8 -> CodePoint
cpFromU8 = \b -> b |> Num.toU32 |> fromU32Unchecked

testHelp : Str -> OutState
testHelp = \str -> 
    when str |> Str.toUtf8 |> CodePoint.parseUtf8 is 
        Err _ -> crash "expected valid Utf8"
        Ok codePoints -> 
            breakPoints = (List.map codePoints InternalGBP.fromCP)
            splitHelp Start codecodePoints breakPoints