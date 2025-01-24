module [
    Grapheme,
    split,
]

import CodePoint exposing [CodePoint, Utf8ParseErr]
import InternalGBP exposing [GBP]
import InternalCP exposing [from_u32_unchecked]
import InternalEmoji

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
Tokens : List [BR Rule, NB Rule, CP CodePoint]

## Split a string into extended grapheme clusters
split : Str -> Result (List Str) Utf8ParseErr
split = |str|

    # TODO DISCUSS
    # I'm not sure if we should return an error here or just crash.
    # A Roc Str should be be valid utf8 and so in theory it should not be possible
    # for split to have invalid utf8 in it. To be discussed.
    code_points = str |> Str.to_utf8 |> CodePoint.parse_utf8?

    break_points = code_points |> List.map(InternalGBP.from_cp)

    Ok((split_help(Next, code_points, break_points, [BR(GB1)]) |> to_list_str))

# Used internally to filter out the break/nobreak tokens and separate CPs into a List Str
to_list_str : Tokens -> List Str
to_list_str = |tokens|
    List.walk(
        tokens,
        { acc: [], cps: [] },
        |state, curr|
            when curr is
                NB(_) -> state
                BR(_) -> if List.is_empty(state.acc) then state else { acc: [], cps: List.append(state.cps, state.acc) }
                CP(cp) -> { acc: List.append(state.acc, cp), cps: state.cps },
    )
    |> .cps
    |> List.map(
        |cp_list|
            # TODO DISCUSS
            # Again I'm not sure if we should crash here... I dont think CodePoint.toStr
            # should be returning a result... to be discussed.
            when CodePoint.to_str(cp_list) is
                Ok(str) -> str
                Err(_) -> crash("unreachable got invalid utf8 when converting CodePoints to Str"),
    )

# Used internally to implement the [UNICODE TEXT SEGMENTATION](https://www.unicode.org/reports/tr29/)
# algorithm.
split_help : _, List CodePoint, List GBP, Tokens -> Tokens
split_help = |state, code_points, break_points, acc|
    next_c_ps = List.drop_first(code_points, 1)
    next_b_ps = List.drop_first(break_points, 1)

    when (state, code_points, break_points) is
        # Special handling for empty list
        (Next, [], _) -> acc
        # Special handling for last codepoint
        (Next, [cp], _) -> List.concat(acc, [CP(cp), BR(GB2)])
        (AfterHangulL(prev), [cp], [bp]) if bp == L or bp == V or bp == LV or bp == LVT -> List.concat(acc, [CP(prev), NB(GB6), CP(cp), BR(GB2)])
        (AfterHangulLVorV(prev), [cp], [bp]) if bp == V or bp == T -> List.concat(acc, [CP(prev), NB(GB7), CP(cp), BR(GB2)])
        (AfterHangulLVTorT(prev), [cp], [bp]) if bp == T -> List.concat(acc, [CP(prev), NB(GB8), CP(cp), BR(GB2)])
        (AfterHangulL(prev), [_], [_]) -> split_help(LastWithPrev(prev), code_points, break_points, acc)
        (AfterHangulLVorV(prev), [_], [_]) -> split_help(LastWithPrev(prev), code_points, break_points, acc)
        (AfterHangulLVTorT(prev), [_], [_]) -> split_help(LastWithPrev(prev), code_points, break_points, acc)
        (LastWithPrev(prev), [cp], [bp]) if bp == Control or bp == CR or bp == LF -> List.concat(acc, [CP(prev), BR(GB5), CP(cp), BR(GB2)])
        (LastWithPrev(prev), [cp], [bp]) if bp == Extend -> List.concat(acc, [CP(prev), NB(GB9), CP(cp), BR(GB2)])
        (LastWithPrev(prev), [cp], [bp]) if bp == ZWJ ->
            if prev |> CodePoint.to_u32 |> InternalEmoji.is_pictographic then
                List.concat(acc, [CP(prev), NB(GB11), CP(cp), BR(GB2)])
            else
                List.concat(acc, [CP(prev), NB(GB9), CP(cp), BR(GB2)])

        (LastWithPrev(prev), [cp], [_]) -> List.concat(acc, [CP(prev), BR(GB999), CP(cp), BR(GB2)])
        (AfterExtend(prev), [], []) -> List.concat(acc, [CP(prev), BR(GB2)])
        (AfterExtend(prev), [_], [_]) -> split_help(LastWithPrev(prev), code_points, break_points, acc)
        (EmojiSeqNext(prev), [], []) -> List.concat(acc, [CP(prev), BR(GB2)])
        (EmojiSeqNext(prev), [cp], [_]) -> List.concat(acc, [CP(prev), NB(GB11), CP(cp), BR(GB2)])
        (EmojiSeqZWJ(prev), [_], [_]) -> split_help(LastWithPrev(prev), code_points, break_points, acc)
        (AfterEvenRI(prev), [], []) -> List.concat(acc, [CP(prev), BR(GB2)])
        (AfterOddRI(prev), [], []) -> List.concat(acc, [CP(prev), BR(GB2)])
        # Looking at current breakpoint property
        (Next, [cp, ..], [bp, ..]) if bp == CR -> split_help(AfterCR(cp), next_c_ps, next_b_ps, acc)
        (Next, [cp, ..], [bp, ..]) if bp == Control or bp == LF -> split_help(Next, next_c_ps, next_b_ps, List.concat(acc, [CP(cp), BR(GB4)]))
        (Next, [cp, ..], [bp, ..]) if bp == L -> split_help(AfterHangulL(cp), next_c_ps, next_b_ps, acc)
        (Next, [cp, ..], [bp, ..]) if bp == V or bp == LV -> split_help(AfterHangulLVorV(cp), next_c_ps, next_b_ps, acc)
        (Next, [cp, ..], [bp, ..]) if bp == LVT or bp == T -> split_help(AfterHangulLVTorT(cp), next_c_ps, next_b_ps, acc)
        (Next, [cp, ..], [bp, ..]) if bp == RI -> split_help(AfterOddRI(cp), next_c_ps, next_b_ps, acc)
        # Advance to next, this is required so that we can apply rules which break before
        (Next, [cp, ..], _) -> split_help(LookAtNext(cp), next_c_ps, next_b_ps, acc)
        # Looking ahead at next, given previous
        (LookAtNext(prev), _, [bp, ..]) if bp == Control or bp == CR or bp == LF -> split_help(Next, code_points, break_points, List.concat(acc, [CP(prev), BR(GB5)]))
        (LookAtNext(prev), [cp, ..], [bp, ..]) if bp == Extend -> split_help(AfterExtend(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB9)]))
        (LookAtNext(prev), [cp, ..], [bp, ..]) if bp == ZWJ ->
            if prev |> CodePoint.to_u32 |> InternalEmoji.is_pictographic then
                # enter emoji sequence
                split_help(EmojiSeqNext(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB9)]))
            else
                split_help(Next, code_points, break_points, List.concat(acc, [CP(prev), NB(GB9)]))

        # Look ahead, given previous was Emoji related
        (EmojiSeqZWJ(prev), [cp, ..], [bp, ..]) ->
            if bp == ZWJ then
                # got another ZWJ continue the sequence
                split_help(EmojiSeqNext(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB11)]))
            else
                split_help(Next, code_points, break_points, acc)

        (EmojiSeqNext(prev), [cp, ..], [_, ..]) ->
            if cp |> CodePoint.to_u32 |> InternalEmoji.is_pictographic then
                # got another emoji, continue the sequence
                split_help(EmojiSeqZWJ(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB11)]))
            else
                split_help(LookAtNext(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB999)]))

        (AfterExtend(prev), [cp, ..], [bp, ..]) if bp == ZWJ -> split_help(EmojiSeqNext(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB9)]))
        (AfterExtend(prev), [cp, ..], [bp, ..]) if bp == Extend -> split_help(AfterExtend(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB9)]))
        (AfterExtend(prev), [_, ..], [_, ..]) -> split_help(Next, code_points, break_points, List.concat(acc, [CP(prev), BR(GB999)]))
        (LookAtNext(prev), _, _) -> split_help(Next, code_points, break_points, List.concat(acc, [CP(prev), BR(GB999)]))
        # Looking ahead, given previous was a Regional Indicator
        (AfterOddRI(prev), [cp, ..], [bp, ..]) if bp == RI -> split_help(AfterEvenRI(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB12)]))
        (AfterOddRI(prev), [_, ..], [_, ..]) -> split_help(LookAtNext(prev), code_points, break_points, acc)
        (AfterEvenRI(prev), [cp, ..], [bp, ..]) if bp == RI -> split_help(AfterOddRI(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), BR(GB999)]))
        (AfterEvenRI(prev), [_, ..], [_, ..]) -> split_help(LookAtNext(prev), code_points, break_points, acc)
        # Looking ahead, given previous was CR
        (AfterCR(prev), _, [bp, ..]) if bp == LF -> split_help(Next, code_points, break_points, List.concat(acc, [CP(prev), NB(GB3)]))
        (AfterCR(prev), _, _) -> split_help(Next, code_points, break_points, List.concat(acc, [CP(prev), BR(GB4)]))
        # Looking ahead, given previous was Hangul
        (AfterHangulL(prev), [cp, ..], [bp, ..]) if bp == L -> split_help(AfterHangulL(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB6)]))
        (AfterHangulL(prev), [cp, ..], [bp, ..]) if bp == V or bp == LV -> split_help(AfterHangulLVorV(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB6)]))
        (AfterHangulL(prev), [cp, ..], [bp, ..]) if bp == LVT -> split_help(AfterHangulLVTorT(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB6)]))
        (AfterHangulL(prev), _, [bp, ..]) if bp == ZWJ -> split_help(AfterZWJ(prev), code_points, break_points, acc)
        (AfterHangulL(prev), _, _) -> split_help(LookAtNext(prev), code_points, break_points, acc)
        (AfterHangulLVorV(prev), [cp, ..], [bp, ..]) if bp == V -> split_help(AfterHangulLVorV(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB7)]))
        (AfterHangulLVorV(prev), [cp, ..], [bp, ..]) if bp == T -> split_help(AfterHangulLVTorT(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB7)]))
        (AfterHangulLVorV(prev), _, [bp, ..]) if bp == ZWJ -> split_help(AfterZWJ(prev), code_points, break_points, acc)
        (AfterHangulLVorV(prev), _, _) -> split_help(LookAtNext(prev), code_points, break_points, acc)
        (AfterHangulLVTorT(prev), [cp, ..], [bp, ..]) if bp == T -> split_help(AfterHangulLVTorT(cp), next_c_ps, next_b_ps, List.concat(acc, [CP(prev), NB(GB8)]))
        (AfterHangulLVTorT(prev), _, [bp, ..]) if bp == ZWJ -> split_help(AfterZWJ(prev), code_points, break_points, acc)
        (AfterHangulLVTorT(prev), _, _) -> split_help(LookAtNext(prev), code_points, break_points, acc)
        # Print out a helpful error message requesting users report the unhandled case.
        _ ->
            crash(
                """
                This is definitely a bug in the roc-lang/unicode package, caused by an unhandled edge case in grapheme text segmentation.

                It is difficult to track down and catch every possible combination, so it would be helpful if you could log this as an issue with a reproduction.

                Grapheme.split state machine state at the time was:
                ${Inspect.to_str((state, List.map(code_points, CodePoint.to_u32), break_points))}
                """,
            )

# Used internally as a test helper to generate the expected answer for a given
# input. Most of the test inputs come from the test data, some are manually developed
# to cover additional edge cases not found in the test data file.
test_help : List (List U32) -> Tokens
test_help = |u32_list|
    code_points = u32_list |> List.join |> List.map(from_u32_unchecked)
    break_points = code_points |> List.map(InternalGBP.from_cp)

    split_help(Next, code_points, break_points, [BR(GB1)])

# GB999 Test break everywhere
expect
    a = test_help([[888], [888]])
    b = [BR(GB1), CP(from_u32_unchecked(888)), BR(GB999), CP(from_u32_unchecked(888)), BR(GB2)]
    a == b

# GB3 & GB5 Test no break between CRLF starting character
expect
    a = test_help([[888], [13, 10]])
    b = [BR(GB1), CP(from_u32_unchecked(888)), BR(GB5), CP(from_u32_unchecked(13)), NB(GB3), CP(from_u32_unchecked(10)), BR(GB2)]
    a == b

# GB4 Test break after control with a CR as last character
expect
    a = test_help([[1], [13]])
    b = [BR(GB1), CP(from_u32_unchecked(1)), BR(GB4), CP(from_u32_unchecked(13)), BR(GB2)]
    a == b

# GB4 Break after starting CR, don't break remaining characters
expect
    a = test_help([[13], [776], [888]])
    b = [BR(GB1), CP(from_u32_unchecked(13)), BR(GB4), CP(from_u32_unchecked(776)), BR(GB999), CP(from_u32_unchecked(888)), BR(GB2)]
    a == b

# GB5 Break before CR
# % 0020 % 000D % #  % [0.2] SPACE (Other) % [5.0] <CARRIAGE RETURN (CR)> (CR) % [0.3]
expect
    a = test_help([[32], [13]])
    b = [BR(GB1), CP(from_u32_unchecked(32)), BR(GB5), CP(from_u32_unchecked(13)), BR(GB2)]
    a == b

# GB4 & GB5 Break after LF and before CR in last position
# % 000A % 0308 % 000D % #  % [0.2] <LINE FEED (LF)> (LF) % [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) % [5.0] <CARRIAGE RETURN (CR)> (CR) % [0.3]
expect
    a = test_help([[10], [776], [13]])
    b = [BR(GB1), CP(from_u32_unchecked(10)), BR(GB4), CP(from_u32_unchecked(776)), BR(GB5), CP(from_u32_unchecked(13)), BR(GB2)]
    a == b

# GB6 Don't break Hangul sequences
# % 1100 x 1100 % #  % [0.2] HANGUL CHOSEONG KIYEOK (L) x [6.0] HANGUL CHOSEONG KIYEOK (L) % [0.3]
expect
    a = test_help([[4352, 4352]])
    b = [BR(GB1), CP(from_u32_unchecked(4352)), NB(GB6), CP(from_u32_unchecked(4352)), BR(GB2)]
    a == b

# GB6 Don't break Hangul sequences L then V
expect
    a = test_help([[4352, 4448]])
    b = [BR(GB1), CP(from_u32_unchecked(4352)), NB(GB6), CP(from_u32_unchecked(4448)), BR(GB2)]
    a == b

# GB6 Don't break Hangul sequences L then LV
expect
    a = test_help([[4352, 44509]])
    b = [BR(GB1), CP(from_u32_unchecked(4352)), NB(GB6), CP(from_u32_unchecked(44509)), BR(GB2)]
    a == b

# GB6 Don't break Hangul sequences L then LVT
expect
    a = test_help([[4352, 45739]])
    b = [BR(GB1), CP(from_u32_unchecked(4352)), NB(GB6), CP(from_u32_unchecked(45739)), BR(GB2)]
    a == b

# GB6 Break after Hangul L
expect
    a = test_help([[4352, 888]])
    b = [BR(GB1), CP(from_u32_unchecked(4352)), BR(GB999), CP(from_u32_unchecked(888)), BR(GB2)]
    a == b

# GB7 Don't break Hangul sequences V then V
expect
    a = test_help([[4448, 4448]])
    b = [BR(GB1), CP(from_u32_unchecked(4448)), NB(GB7), CP(from_u32_unchecked(4448)), BR(GB2)]
    a == b

# GB7 Don't break Hangul sequences LV then T
expect
    a = test_help([[45236, 55243]])
    b = [BR(GB1), CP(from_u32_unchecked(45236)), NB(GB7), CP(from_u32_unchecked(55243)), BR(GB2)]
    a == b

# GB7 Break after Hangul LV
expect
    a = test_help([[45236, 888]])
    b = [BR(GB1), CP(from_u32_unchecked(45236)), BR(GB999), CP(from_u32_unchecked(888)), BR(GB2)]
    a == b

# GB8 Don't break Hangul sequences LVT then T
expect
    a = test_help([[44619, 55243]])
    b = [BR(GB1), CP(from_u32_unchecked(44619)), NB(GB8), CP(from_u32_unchecked(55243)), BR(GB2)]
    a == b

# GB8 Don't break Hangul sequences T then T
expect
    a = test_help([[4607, 55243]])
    b = [BR(GB1), CP(from_u32_unchecked(4607)), NB(GB8), CP(from_u32_unchecked(55243)), BR(GB2)]
    a == b

# GB8 & GB5 Break after Hangul sequence T before CR
expect
    a = test_help([[4607, 13]])
    b = [BR(GB1), CP(from_u32_unchecked(4607)), BR(GB5), CP(from_u32_unchecked(13)), BR(GB2)]
    a == b

# GB9
# % [0.2] <reserved-0378> (Other) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) % [0.3]
expect
    a = test_help([[888, 8205]])
    b = [BR(GB1), CP(from_u32_unchecked(888)), NB(GB9), CP(from_u32_unchecked(8205)), BR(GB2)]
    a == b

# GB9 Do not break before extending characters or ZWJ in the middle of a sequence
# % [0.2] QUESTION MARK (Other) x [9.0] DEVANAGARI SIGN VIRAMA (Extend_ConjunctLinkingScripts_ConjunctLinker_ExtCccZwj) % [999.0] DEVANAGARI LETTER TA (ConjunctLinkingScripts_LinkingConsonant) % [0.3]
expect
    a = test_help([[63, 2381], [2340]])
    b = [BR(GB1), CP(from_u32_unchecked(63)), NB(GB9), CP(from_u32_unchecked(2381)), BR(GB999), CP(from_u32_unchecked(2340)), BR(GB2)]
    a == b

# GB9 Do not break before extending characters or ZWJ in last position
# % [0.2] HANGUL SYLLABLE GAG (LVT) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) % [0.3]
expect
    a = test_help([[44033, 8205]])
    b = [BR(GB1), CP(from_u32_unchecked(44033)), NB(GB9), CP(from_u32_unchecked(8205)), BR(GB2)]
    a == b

# GB9
# % [0.2] <reserved-0378> (Other) x [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) % [0.3]
expect
    a = test_help([[888, 776, 8205]])
    b = [BR(GB1), CP(from_u32_unchecked(888)), NB(GB9), CP(from_u32_unchecked(776)), NB(GB9), CP(from_u32_unchecked(8205)), BR(GB2)]
    a == b

# GB11 Do not break before extending characters or ZWJ
# % [0.2] UPPER BLADE SCISSORS (Other) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) x [11.0] UPPER BLADE SCISSORS (Other) % [0.3]
expect
    a = test_help([[9985, 8205, 9985]])
    b = [BR(GB1), CP(from_u32_unchecked(9985)), NB(GB9), CP(from_u32_unchecked(8205)), NB(GB11), CP(from_u32_unchecked(9985)), BR(GB2)]
    a == b

# GB9 & GB11 stress test lots of emoji with both ZWJ and Extends
# % [0.2] BABY (ExtPict) x  [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) x  [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) x  [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) x  [11.0] BABY (ExtPict) x  [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) %  [0.3]
expect
    a = test_help([[128118, 127999, 776, 8205, 128118, 127999]])
    b = [
        BR(GB1),
        CP(from_u32_unchecked(128118)), # BABY (ExtPict) >> has GBP of (Other)
        NB(GB9),
        CP(from_u32_unchecked(127999)), # EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) >> has GBP of (Extend)
        NB(GB9),
        CP(from_u32_unchecked(776)), # COMBINING DIAERESIS (Extend_ExtCccZwj) >> has GBP of (Extend)
        NB(GB9),
        CP(from_u32_unchecked(8205)), # ZERO WIDTH JOINER (ZWJ_ExtCccZwj) >> has GBP of (ZWJ)
        NB(GB11),
        CP(from_u32_unchecked(128118)), # BABY (ExtPict) >> has GBP of (Other)
        NB(GB9),
        CP(from_u32_unchecked(127999)), # EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) >> has GBP of (Extend)
        BR(GB2),
    ]
    a == b

# GB11 emoji another complicated example
# % [0.2] LATIN SMALL LETTER A (Other) x [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) % [999.0] BABY (ExtPict) x [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) x [11.0] OCTAGONAL SIGN (ExtPict) % [0.3]
expect
    a = test_help([[97, 127999], [128118, 8205, 128721]])
    b = [
        BR(GB1),
        CP(from_u32_unchecked(97)), # Other
        NB(GB9),
        CP(from_u32_unchecked(127999)), # Extend
        BR(GB999),
        CP(from_u32_unchecked(128118)), # Other
        NB(GB9),
        CP(from_u32_unchecked(8205)), # ZWJ
        NB(GB11),
        CP(from_u32_unchecked(128721)), # Other
        BR(GB2),
    ]
    a == b

# GB12 Last is even sequence
# % [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) x [12.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) % [0.3]
expect
    a = test_help([[127462, 127462]])
    b = [BR(GB1), CP(from_u32_unchecked(127462)), NB(GB12), CP(from_u32_unchecked(127462)), BR(GB2)]
    a == b

# GB12 Last is odd sequence
# % [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) x [12.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) % [0.3]
expect
    a = test_help([[127462, 127462, 127462]])
    b = [BR(GB1), CP(from_u32_unchecked(127462)), NB(GB12), CP(from_u32_unchecked(127462)), BR(GB999), CP(from_u32_unchecked(127462)), BR(GB2)]
    a == b

# GB12 Combination
# % [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) x [12.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) % [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) % [999.0] LATIN SMALL LETTER B (Other) % [0.3]
expect
    a = test_help([[127462, 127463], [127464], [98]])
    b = [BR(GB1), CP(from_u32_unchecked(127462)), NB(GB12), CP(from_u32_unchecked(127463)), BR(GB999), CP(from_u32_unchecked(127464)), BR(GB999), CP(from_u32_unchecked(98)), BR(GB2)]
    a == b

expect split("🥷🏼") == Ok(["🥷🏼"])
expect split("🇦🇺🦘🪃") == Ok(["🇦🇺", "🦘", "🪃"])
