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

# Note we include the Break Rules here so that it is feasible to debug this and ensure algorithm correctness
# We could remove these and reduce the number of allocations, however it is very difficult then 
# to understand if the implementation is applying each rule correctly
splitHelp : _, List CodePoint, List GBP, OutState -> OutState
splitHelp = \state, codePoints, breakPoints, acc ->
    nextCPs = List.dropFirst codePoints 1
    nextBPs = List.dropFirst breakPoints 1

    # HELPFUL FOR DEBUGGIN - REMOVE ME 
    # dbgMe = T (List.first codePoints |> Result.map CodePoint.toU32) (List.first breakPoints)
    # dbg dbgMe

    when (state, codePoints, breakPoints) is 

        # Special handling for last codepoint
        (Next, [cp], _) -> (List.concat acc [CP cp, BR GB2])
        (AfterHungulL prev, [cp], [bp]) if bp == L || bp == V || bp == LV || bp == LVT -> (List.concat acc [CP prev, NB GB6, CP cp, BR GB2])
        (AfterHungulLVorV prev, [cp], [bp]) if bp == V || bp == T -> (List.concat acc [CP prev, NB GB7, CP cp, BR GB2])
        (AfterHungulLVTorT prev, [cp], [bp]) if bp == T -> (List.concat acc [CP prev, NB GB8, CP cp, BR GB2])
        (AfterHungulL prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc
        (AfterHungulLVorV prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc
        (AfterHungulLVTorT prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc
        (LastWithPrev prev, [cp], [bp]) if bp == Control || bp == CR || bp == LF -> (List.concat acc [CP prev, BR GB5, CP cp, BR GB2])
        (LastWithPrev prev, [cp], [bp]) if bp == ZWJ || bp == Extend -> (List.concat acc [CP prev, NB GB9, CP cp, BR GB2])
        (LastWithPrev prev, [cp], [_]) -> (List.concat acc [CP prev, BR GB999, CP cp, BR GB2])

        # Looking at current breakpoint property 
        (Next, [cp, ..], [bp, ..]) if bp == CR -> splitHelp (AfterCR cp) nextCPs nextBPs acc
        (Next, [cp, ..], [bp, ..]) if bp == Control || bp == LF -> splitHelp Next nextCPs nextBPs (List.concat acc [CP cp, BR GB4])
        (Next, [cp, ..], [bp, ..]) if bp == L -> splitHelp (AfterHungulL cp) nextCPs nextBPs acc
        (Next, [cp, ..], [bp, ..]) if bp == V || bp == LV -> splitHelp (AfterHungulLVorV cp) nextCPs nextBPs acc
        (Next, [cp, ..], [bp, ..]) if bp == LVT || bp == T -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs acc

        # Advance to next, this is requred so that we can apply rules which break before
        (Next, [cp, ..], _) -> splitHelp (LookAtNext cp) nextCPs nextBPs acc

        # Looking ahead at next, given previous
        (LookAtNext prev, _, [bp, ..]) if bp == Control || bp == CR || bp == LF -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, BR GB5])
        (LookAtNext prev, _, [bp, ..]) if bp == ZWJ || bp == Extend -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, NB GB9])
        (LookAtNext prev, _, _) -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, BR GB999])
        
        # Looking ahead, given previous was CR
        (AfterCR prev, _, [bp, ..]) if bp == LF -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, NB GB3])
        (AfterCR prev, _, _) -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, BR GB4])

        # Looking ahead, given previous was Hangul
        (AfterHungulL prev, [cp, ..], [bp, ..]) if bp == L -> splitHelp (AfterHungulL cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB6])
        (AfterHungulL prev, [cp, ..], [bp, ..]) if bp == V || bp == LV -> splitHelp (AfterHungulLVorV cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB6])
        (AfterHungulL prev, [cp, ..], [bp, ..]) if bp == LVT -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB6])
        (AfterHungulL prev, _, _) -> splitHelp (LookAtNext prev) codePoints breakPoints acc
        (AfterHungulLVorV prev, [cp, ..], [bp, ..]) if bp == V -> splitHelp (AfterHungulLVorV cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB7])
        (AfterHungulLVorV prev, [cp, ..], [bp, ..]) if bp == T -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB7])
        (AfterHungulLVorV prev, _, _) -> splitHelp (LookAtNext prev) codePoints breakPoints acc
        (AfterHungulLVTorT prev, [cp, ..], [bp, ..]) if bp == T -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB8])
        (AfterHungulLVTorT prev, _, _) -> splitHelp (LookAtNext prev) codePoints breakPoints acc

        _ ->
            crash 
                """
                This is definitely a bug in the roc-lang/unicode package, caused by an unhandled edge case in grapheme text segmentation.

                It is difficult to track down and catch every possible combination, so it would be helpful if you could log this as an issue with a reproduction.

                Grapheme.split state machine state at the time was:
                \(Inspect.toStr (state, List.map codePoints CodePoint.toU32, breakPoints))
                """

# GB999 Test break everywhere
expect 
    a = testHelp [[888], [888]]
    b = [BR GB1, CP (fromU32Unchecked 888), BR GB999, CP (fromU32Unchecked 888), BR GB2]
    a == b

# GB3 & GB5 Test no break between CRLF starting character
expect 
    a = testHelp [[888], [13, 10]]
    b = [BR GB1, CP (fromU32Unchecked 888), BR GB5, CP (fromU32Unchecked 13), NB GB3, CP (fromU32Unchecked 10), BR GB2]
    a == b

# GB4 Test break after control with a CR as last character
expect 
    a = testHelp [[1], [13]]
    b = [BR GB1, CP (fromU32Unchecked 1), BR GB4, CP (fromU32Unchecked 13), BR GB2]
    a == b

# GB4 Break after starting CR, don't break remaining characters 
expect 
    a = testHelp [[13], [776], [888]]
    b = [BR GB1, CP (fromU32Unchecked 13), BR GB4, CP (fromU32Unchecked 776), BR GB999, CP (fromU32Unchecked 888), BR GB2]
    a == b

# GB5 Break before CR 
# % 0020 % 000D % #  % [0.2] SPACE (Other) % [5.0] <CARRIAGE RETURN (CR)> (CR) % [0.3]
expect 
    a = testHelp [[32], [13]]
    b = [BR GB1, CP (fromU32Unchecked 32), BR GB5, CP (fromU32Unchecked 13), BR GB2]
    a == b

# GB4 & GB5 Break after LF and before CR in last position
# % 000A % 0308 % 000D % #  % [0.2] <LINE FEED (LF)> (LF) % [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) % [5.0] <CARRIAGE RETURN (CR)> (CR) % [0.3]
expect 
    a = testHelp [[10], [776], [13]]
    b = [BR GB1, CP (fromU32Unchecked 10), BR GB4, CP (fromU32Unchecked 776),BR GB5, CP (fromU32Unchecked 13), BR GB2]
    a == b

# GB6 Don't break Hangul sequences
# % 1100 x 1100 % #  % [0.2] HANGUL CHOSEONG KIYEOK (L) x [6.0] HANGUL CHOSEONG KIYEOK (L) % [0.3]
expect 
    a = testHelp [[4352, 4352]]
    b = [BR GB1, CP (fromU32Unchecked 4352), NB GB6, CP (fromU32Unchecked 4352), BR GB2]
    a == b

# GB6 Don't break Hangul sequences L then V
expect 
    a = testHelp [[4352, 4448]]
    b = [BR GB1, CP (fromU32Unchecked 4352), NB GB6, CP (fromU32Unchecked 4448), BR GB2]
    a == b

# GB6 Don't break Hangul sequences L then LV
expect 
    a = testHelp [[4352, 44509]]
    b = [BR GB1, CP (fromU32Unchecked 4352), NB GB6, CP (fromU32Unchecked 44509), BR GB2]
    a == b

# GB6 Don't break Hangul sequences L then LVT
expect 
    a = testHelp [[4352, 45739]]
    b = [BR GB1, CP (fromU32Unchecked 4352), NB GB6, CP (fromU32Unchecked 45739), BR GB2]
    a == b

# GB6 Break after Hangul L
expect 
    a = testHelp [[4352, 888]]
    b = [BR GB1, CP (fromU32Unchecked 4352), BR GB999, CP (fromU32Unchecked 888), BR GB2]
    a == b

# GB7 Don't break Hangul sequences V then V
expect 
    a = testHelp [[4448, 4448]]
    b = [BR GB1, CP (fromU32Unchecked 4448), NB GB7, CP (fromU32Unchecked 4448), BR GB2]
    a == b

# GB7 Don't break Hangul sequences LV then T
expect 
    a = testHelp [[45236, 55243]]
    b = [BR GB1, CP (fromU32Unchecked 45236), NB GB7, CP (fromU32Unchecked 55243), BR GB2]
    a == b

# GB7 Break after Hangul LV
expect 
    a = testHelp [[45236, 888]]
    b = [BR GB1, CP (fromU32Unchecked 45236), BR GB999, CP (fromU32Unchecked 888), BR GB2]
    a == b

# GB8 Don't break Hangul sequences LVT then T
expect 
    a = testHelp [[44619, 55243]]
    b = [BR GB1, CP (fromU32Unchecked 44619), NB GB8, CP (fromU32Unchecked 55243), BR GB2]
    a == b

# GB8 Don't break Hangul sequences T then T
expect 
    a = testHelp [[4607, 55243]]
    b = [BR GB1, CP (fromU32Unchecked 4607), NB GB8, CP (fromU32Unchecked 55243), BR GB2]
    a == b

# GB8 & GB5 Break after Hangul sequence T before CR
expect 
    a = testHelp [[4607, 13]]
    b = [BR GB1, CP (fromU32Unchecked 4607), BR GB5, CP (fromU32Unchecked 13), BR GB2]
    a == b

# GB9 Do not break before extending characters or ZWJ in the middle of a sequence
# % 003F x 094D % 0924 % #  % [0.2] QUESTION MARK (Other) x [9.0] DEVANAGARI SIGN VIRAMA (Extend_ConjunctLinkingScripts_ConjunctLinker_ExtCccZwj) % [999.0] DEVANAGARI LETTER TA (ConjunctLinkingScripts_LinkingConsonant) % [0.3]
expect 
    a = testHelp [[63, 2381], [2340]]
    b = [BR GB1, CP (fromU32Unchecked 63), NB GB9, CP (fromU32Unchecked 2381), BR GB999, CP (fromU32Unchecked 2340), BR GB2]
    a == b

# GB9 Do not break before extending characters or ZWJ in last position
# % AC01 x 200D % #  % [0.2] HANGUL SYLLABLE GAG (LVT) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) % [0.3]
expect 
    a = testHelp [[44033, 8205]]
    b = [BR GB1, CP (fromU32Unchecked 44033), NB GB9, CP (fromU32Unchecked 8205), BR GB2]
    a == b

testHelp : List (List U32) -> OutState
testHelp = \u32List -> 
    codePoints = u32List |> List.join |> List.map fromU32Unchecked
    breakPoints = codePoints |> List.map InternalGBP.fromCP
    
    splitHelp Next codePoints breakPoints [BR GB1]
    