interface Grapheme
    exposes [
        Grapheme,
        split,
    ]
    imports [
        CodePoint.{ CodePoint, Utf8ParseErr },
        InternalGBP.{ GBP },
        InternalCP.{ fromU32Unchecked },
        InternalEmoji,
    ]

## Extended Grapheme Cluster
Grapheme : InternalGBP.GBP

# Used internally for implementing text segmentation algorithm for [split]
# Refer to https://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules
# Note GB13 is not used, it has been merged with GB12 as they are identical as far as I can tell
Rule : [GB1, GB2, GB3, GB4, GB5, GB6, GB7, GB8, GB9, GB9a, GB9b, GB9c, GB11, GB12, GB999]

# User internally to represent the text segmentation algorithm. We include the 
# Rules here so that it is feasible to debug this and ensure algorithm correctness
# We could remove these and reduce the number of allocations, however it is very 
# difficult then to understand if the implementation is applying each rule correctly
Tokens : List [BR Rule,NB Rule,CP CodePoint]

## Split a string into extended grapheme clusters
split : Str -> Result (List Str) Utf8ParseErr
split = \str ->

    # TODO DISCUSS
    # I'm not sure if we should return an error here or just crash.
    # A Roc Str should be be valid utf8 and so in theory it should not be possible
    # for split to have invalid utf8 in it. To be discussed.
    codePoints <- str |> Str.toUtf8 |> CodePoint.parseUtf8 |> Result.map

    breakPoints = codePoints |> List.map InternalGBP.fromCP

    splitHelp Next codePoints breakPoints [BR GB1] |> toListStr

# Used internally to filter out the break/nobreak tokens and separate CPs into a List Str
toListStr : Tokens -> List Str
toListStr = \tokens ->
    List.walk tokens { acc: [], cps: [] } \state, curr ->
        when curr is
            NB _ -> state
            BR _ -> if List.isEmpty state.acc then state else { acc: [], cps: List.append state.cps state.acc }
            CP cp -> { acc: List.append state.acc cp, cps: state.cps }
    |> .cps
    |> List.map \cpList ->
        # TODO DISCUSS
        # Again I'm not sure if we should crash here... I dont think CodePoint.toStr
        # should be returning a result... to be discussed.
        when CodePoint.toStr cpList is
            Ok str -> str
            Err _ -> crash "unreachable got invalid utf8 when converting CodePoints to Str"

# Used internally to implement the [UNICODE TEXT SEGMENTATION](https://www.unicode.org/reports/tr29/)
# algorithm.
splitHelp : _, List CodePoint, List GBP, Tokens -> Tokens
splitHelp = \state, codePoints, breakPoints, acc ->
    nextCPs = List.dropFirst codePoints 1
    nextBPs = List.dropFirst breakPoints 1

    when (state, codePoints, breakPoints) is

        # Special handling for last codepoint
        (Next, [cp], _) -> List.concat acc [CP cp, BR GB2]
        (AfterHungulL prev, [cp], [bp]) if bp == L || bp == V || bp == LV || bp == LVT -> List.concat acc [CP prev, NB GB6, CP cp, BR GB2]
        (AfterHungulLVorV prev, [cp], [bp]) if bp == V || bp == T -> List.concat acc [CP prev, NB GB7, CP cp, BR GB2]
        (AfterHungulLVTorT prev, [cp], [bp]) if bp == T -> List.concat acc [CP prev, NB GB8, CP cp, BR GB2]
        (AfterHungulL prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc
        (AfterHungulLVorV prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc
        (AfterHungulLVTorT prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc
        (LastWithPrev prev, [cp], [bp]) if bp == Control || bp == CR || bp == LF -> List.concat acc [CP prev, BR GB5, CP cp, BR GB2]
        (LastWithPrev prev, [cp], [bp]) if bp == Extend -> List.concat acc [CP prev, NB GB9, CP cp, BR GB2]
        (LastWithPrev prev, [cp], [bp]) if bp == ZWJ ->

            if prev |> CodePoint.toU32 |> InternalEmoji.isPictographic then
                List.concat acc [CP prev, NB GB11, CP cp, BR GB2]
            else
                List.concat acc [CP prev, NB GB9, CP cp, BR GB2]

        (LastWithPrev prev, [cp], [_]) -> List.concat acc [CP prev, BR GB999, CP cp, BR GB2]
        (AfterExtend prev, [], []) -> List.concat acc [CP prev, BR GB2]
        (AfterExtend prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc
        (EmojiSeqNext prev, [], []) -> List.concat acc [CP prev, BR GB2]
        (EmojiSeqNext prev, [cp], [_]) -> List.concat acc [CP prev, NB GB11, CP cp, BR GB2]
        (EmojiSeqZWJ prev, [_], [_]) -> splitHelp (LastWithPrev prev) codePoints breakPoints acc

        (AfterEvenRI prev, [], []) -> List.concat acc [CP prev, BR GB2]
        (AfterOddRI prev, [], []) -> List.concat acc [CP prev, BR GB2]

        # Looking at current breakpoint property
        (Next, [cp, ..], [bp, ..]) if bp == CR -> splitHelp (AfterCR cp) nextCPs nextBPs acc
        (Next, [cp, ..], [bp, ..]) if bp == Control || bp == LF -> splitHelp Next nextCPs nextBPs (List.concat acc [CP cp, BR GB4])
        (Next, [cp, ..], [bp, ..]) if bp == L -> splitHelp (AfterHungulL cp) nextCPs nextBPs acc
        (Next, [cp, ..], [bp, ..]) if bp == V || bp == LV -> splitHelp (AfterHungulLVorV cp) nextCPs nextBPs acc
        (Next, [cp, ..], [bp, ..]) if bp == LVT || bp == T -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs acc
        (Next, [cp, ..], [bp, ..]) if bp == RI -> splitHelp (AfterOddRI cp) nextCPs nextBPs acc
        
        # Advance to next, this is requred so that we can apply rules which break before
        (Next, [cp, ..], _) -> splitHelp (LookAtNext cp) nextCPs nextBPs acc
        
        # Looking ahead at next, given previous
        (LookAtNext prev, _, [bp, ..]) if bp == Control || bp == CR || bp == LF -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, BR GB5])
        (LookAtNext prev, [cp, ..], [bp, ..]) if bp == Extend -> splitHelp (AfterExtend cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB9])
        (LookAtNext prev, [cp, ..], [bp, ..]) if bp == ZWJ ->
            
            if prev |> CodePoint.toU32 |> InternalEmoji.isPictographic then
                # enter emoji sequence
                splitHelp (EmojiSeqNext cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB9])
            else
                splitHelp Next codePoints breakPoints (List.concat acc [CP prev, NB GB9])

        # Look ahead, given previous was Emoji related
        (EmojiSeqZWJ prev, [cp, ..], [bp, ..]) ->
            
            if bp == ZWJ then
                # got another ZWJ continue the sequence
                splitHelp (EmojiSeqNext cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB11])
            else
                splitHelp Next codePoints breakPoints acc

        (EmojiSeqNext prev, [cp, ..], [_, ..]) ->

            if cp |> CodePoint.toU32 |> InternalEmoji.isPictographic then
                # got another emoji, continue the sequence
                splitHelp (EmojiSeqZWJ cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB11])
            else
                splitHelp (LookAtNext cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB999])

        (AfterExtend prev, [cp, ..], [bp, ..]) if bp == ZWJ -> splitHelp (EmojiSeqNext cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB9])
        (AfterExtend prev, [cp, ..], [bp, ..]) if bp == Extend -> splitHelp (AfterExtend cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB9])
        (AfterExtend prev, [_, ..], [_, ..]) -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, BR GB999])
        (LookAtNext prev, _, _) -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, BR GB999])

        # Looking ahead, given previous was a Regional Indicator
        (AfterOddRI prev, [cp, ..], [bp, ..]) if bp == RI -> splitHelp (AfterEvenRI cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB12])
        (AfterOddRI prev, [_, ..], [_, ..]) -> splitHelp (LookAtNext prev) codePoints breakPoints acc
        (AfterEvenRI prev, [cp, ..], [bp, ..]) if bp == RI -> splitHelp (AfterOddRI cp) nextCPs nextBPs (List.concat acc [CP prev, BR GB999])
        (AfterEvenRI prev, [_, ..], [_, ..]) -> splitHelp (LookAtNext prev) codePoints breakPoints acc
        
        # Looking ahead, given previous was CR
        (AfterCR prev, _, [bp, ..]) if bp == LF -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, NB GB3])
        (AfterCR prev, _, _) -> splitHelp Next codePoints breakPoints (List.concat acc [CP prev, BR GB4])

        # Looking ahead, given previous was Hangul
        (AfterHungulL prev, [cp, ..], [bp, ..]) if bp == L -> splitHelp (AfterHungulL cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB6])
        (AfterHungulL prev, [cp, ..], [bp, ..]) if bp == V || bp == LV -> splitHelp (AfterHungulLVorV cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB6])
        (AfterHungulL prev, [cp, ..], [bp, ..]) if bp == LVT -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB6])
        (AfterHungulL prev, _, [bp, ..]) if bp == ZWJ -> splitHelp (AfterZWJ prev) codePoints breakPoints acc
        (AfterHungulL prev, _, _) -> splitHelp (LookAtNext prev) codePoints breakPoints acc
        (AfterHungulLVorV prev, [cp, ..], [bp, ..]) if bp == V -> splitHelp (AfterHungulLVorV cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB7])
        (AfterHungulLVorV prev, [cp, ..], [bp, ..]) if bp == T -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB7])
        (AfterHungulLVorV prev, _, [bp, ..]) if bp == ZWJ -> splitHelp (AfterZWJ prev) codePoints breakPoints acc
        (AfterHungulLVorV prev, _, _) -> splitHelp (LookAtNext prev) codePoints breakPoints acc
        (AfterHungulLVTorT prev, [cp, ..], [bp, ..]) if bp == T -> splitHelp (AfterHungulLVTorT cp) nextCPs nextBPs (List.concat acc [CP prev, NB GB8])
        (AfterHungulLVTorT prev, _, [bp, ..]) if bp == ZWJ -> splitHelp (AfterZWJ prev) codePoints breakPoints acc
        (AfterHungulLVTorT prev, _, _) -> splitHelp (LookAtNext prev) codePoints breakPoints acc

        # Print out a helpful error message requesting users report the unhandled case.
        _ ->
            crash
                """
                This is definitely a bug in the roc-lang/unicode package, caused by an unhandled edge case in grapheme text segmentation.

                It is difficult to track down and catch every possible combination, so it would be helpful if you could log this as an issue with a reproduction.

                Grapheme.split state machine state at the time was:
                \(Inspect.toStr (state, List.map codePoints CodePoint.toU32, breakPoints))
                """

# Used internally as a test helper to generate the expected answer for a given 
# input. Most of the test inputs come from the test data, some are manually developed
# to cover additional edge cases not found in the test data file.
testHelp : List (List U32) -> Tokens
testHelp = \u32List ->
    codePoints = u32List |> List.join |> List.map fromU32Unchecked
    breakPoints = codePoints |> List.map InternalGBP.fromCP

    splitHelp Next codePoints breakPoints [BR GB1]

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
    b = [BR GB1, CP (fromU32Unchecked 10), BR GB4, CP (fromU32Unchecked 776), BR GB5, CP (fromU32Unchecked 13), BR GB2]
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

# GB9
# % [0.2] <reserved-0378> (Other) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) % [0.3]
expect
    a = testHelp [[888, 8205]]
    b = [BR GB1, CP (fromU32Unchecked 888), NB GB9, CP (fromU32Unchecked 8205), BR GB2]
    a == b

# GB9 Do not break before extending characters or ZWJ in the middle of a sequence
# % [0.2] QUESTION MARK (Other) x [9.0] DEVANAGARI SIGN VIRAMA (Extend_ConjunctLinkingScripts_ConjunctLinker_ExtCccZwj) % [999.0] DEVANAGARI LETTER TA (ConjunctLinkingScripts_LinkingConsonant) % [0.3]
expect
    a = testHelp [[63, 2381], [2340]]
    b = [BR GB1, CP (fromU32Unchecked 63), NB GB9, CP (fromU32Unchecked 2381), BR GB999, CP (fromU32Unchecked 2340), BR GB2]
    a == b

# GB9 Do not break before extending characters or ZWJ in last position
# % [0.2] HANGUL SYLLABLE GAG (LVT) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) % [0.3]
expect
    a = testHelp [[44033, 8205]]
    b = [BR GB1, CP (fromU32Unchecked 44033), NB GB9, CP (fromU32Unchecked 8205), BR GB2]
    a == b

# GB9
# % [0.2] <reserved-0378> (Other) x [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) % [0.3]
expect
    a = testHelp [[888, 776, 8205]]
    b = [BR GB1, CP (fromU32Unchecked 888), NB GB9, CP (fromU32Unchecked 776), NB GB9, CP (fromU32Unchecked 8205), BR GB2]
    a == b

# GB11 Do not break before extending characters or ZWJ
# % [0.2] UPPER BLADE SCISSORS (Other) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) x [11.0] UPPER BLADE SCISSORS (Other) % [0.3]
expect
    a = testHelp [[9985, 8205, 9985]]
    b = [BR GB1, CP (fromU32Unchecked 9985), NB GB9, CP (fromU32Unchecked 8205), NB GB11, CP (fromU32Unchecked 9985), BR GB2]
    a == b

# GB9 & GB11 stress test lots of emoji with both ZWJ and Extends
# % [0.2] BABY (ExtPict) x  [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) x  [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) x  [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) x  [11.0] BABY (ExtPict) x  [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) %  [0.3]
expect
    a = testHelp [[128118, 127999, 776, 8205, 128118, 127999]]
    b = [
        BR GB1,
        CP (fromU32Unchecked 128118), # BABY (ExtPict) >> has GBP of (Other)
        NB GB9,
        CP (fromU32Unchecked 127999), # EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) >> has GBP of (Extend)
        NB GB9,
        CP (fromU32Unchecked 776), # COMBINING DIAERESIS (Extend_ExtCccZwj) >> has GBP of (Extend)
        NB GB9,
        CP (fromU32Unchecked 8205), # ZERO WIDTH JOINER (ZWJ_ExtCccZwj) >> has GBP of (ZWJ)
        NB GB11,
        CP (fromU32Unchecked 128118), # BABY (ExtPict) >> has GBP of (Other)
        NB GB9,
        CP (fromU32Unchecked 127999), # EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) >> has GBP of (Extend)
        BR GB2,
    ]
    a == b

# GB11 emoji another complicated example 
# % [0.2] LATIN SMALL LETTER A (Other) x [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) % [999.0] BABY (ExtPict) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) x [11.0] OCTAGONAL SIGN (ExtPict) % [0.3]
expect
    a = testHelp [[97, 127999], [128118, 8205, 128721]]
    b = [
        BR GB1,
        CP (fromU32Unchecked 97), # Other
        NB GB9,
        CP (fromU32Unchecked 127999), # Extend
        BR GB999,
        CP (fromU32Unchecked 128118), # Other
        NB GB9,
        CP (fromU32Unchecked 8205), # ZWJ
        NB GB11,
        CP (fromU32Unchecked 128721), # Other
        BR GB2,
    ]
    a == b

# GB12 Last is even sequence
# % [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) x [12.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) % [0.3]
expect
    a = testHelp [[127462, 127462]]
    b = [BR GB1, CP (fromU32Unchecked 127462), NB GB12, CP (fromU32Unchecked 127462), BR GB2]
    a == b

# GB12 Last is odd sequence
# % [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) x [12.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) % [0.3]
expect
    a = testHelp [[127462, 127462, 127462]]
    b = [BR GB1, CP (fromU32Unchecked 127462), NB GB12, CP (fromU32Unchecked 127462), BR GB999, CP (fromU32Unchecked 127462), BR GB2]
    a == b

# GB12 Combination
# % [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) x [12.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) % [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) % [999.0] LATIN SMALL LETTER B (Other) % [0.3]
expect
    a = testHelp [[127462, 127463], [127464], [98]]
    b = [BR GB1, CP (fromU32Unchecked 127462), NB GB12, CP (fromU32Unchecked 127463), BR GB999, CP (fromU32Unchecked 127464), BR GB999, CP (fromU32Unchecked 98), BR GB2]
    a == b

expect split "🥷🏼" == Ok ["🥷🏼"]
expect split "🇦🇺🦘🪃" == Ok ["🇦🇺", "🦘", "🪃"]