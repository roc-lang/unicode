interface Grapheme
    exposes [
        Grapheme,
        split,
    ]
    imports [
        CodePoint.{ CodePoint, Utf8ParseErr },
        InternalGBP.{ GBP },
        InternalCP,
        InternalEmoji,
    ]

## Extended Grapheme Cluster
Grapheme : InternalGBP.GBP

# TODO: benchmark to find a better capacity, just guessing at a reasonable 
# capacity here for the number of substrings
defaultCapacity = 50

## Split a string into extended grapheme clusters
## 
## This typically associated with "characters" in a string, for example:
## TODO ADD EXAMPLES
split : Str -> Result (List Str) Utf8ParseErr
split = \str ->
    str 
    |> Str.toUtf8 
    |> CodePoint.parseUtf8
    |> Result.map withGBP 
    |> Result.map \cpsWithGpbs -> splitHelp cpsWithGpbs Break (List.withCapacity defaultCapacity) (List.withCapacity defaultCapacity) 
    
expect # test split breaks between ASCII
    actual = split "abc123"
    actual == Ok ["a", "b","c","1","2","3"]

expect # test split doesn't break between CRLF 
    actual = split "\r\n"
    actual == Ok ["\r\n"]

withGBP : List CodePoint -> List (CodePoint, GBP) 
withGBP = \cps ->
    List.map cps \cp -> (cp, InternalGBP.fromCP cp)

expect # test withGBP 
    cr = (InternalCP.fromU32Unchecked 13)
    lf = (InternalCP.fromU32Unchecked 10)

    withGBP [cr, lf] == [(cr, CR), (lf, LF)]

isExtendOrZWJOrSpacing : (CodePoint, GBP) -> Bool
isExtendOrZWJOrSpacing = \(_, gbp) -> 
    # GB9 Do not break before extending characters or ZWJ.
    # GB9a Do not break before SpacingMarks
    gbp == Extend || gbp == ZWJ || gbp == SpacingMark

extractCP : (CodePoint, GBP) -> CodePoint
extractCP = .0

extractGBP : (CodePoint, GBP) -> GBP
extractGBP = .1
        
splitHelp : List (CodePoint, GBP), GCBPState, List CodePoint, List Str  -> List Str
splitHelp = \cpsWithGpbs, prevState, acc, strs ->

    # Set up helper to advance to the next CP recursively and update state 
    advance = \nextState, nextAcc, nextStrs -> 
        splitHelp
            (List.dropFirst cpsWithGpbs 1)
            nextState
            nextAcc
            nextStrs

    when cpsWithGpbs is 
        # Return the accumulated strings if we have no more CodePoints
        [] -> strs

        # Look ahead if we have more than one CodePoint left
        [current, next, ..] ->
            if isExtendOrZWJOrSpacing next then 

                # GB11

                # GB9 Do not break before extending characters, ZWJ, or spacing marks.
                advance
                    DontBreak
                    (List.append acc (extractCP current))
                    strs

                

            else 
                nextState = gbpRules prevState (extractGBP current)
                    
                # If we are breaking on this CodePoint, then we need to reset accumulated CPs
                nextAcc = 
                    when nextState is
                        Break -> []
                        _ -> List.append acc (extractCP current)

                # If we are breaking on this CodePoint, then we need to add the current
                nextStrs =
                    when nextState is
                        Break -> 
                            when CodePoint.toStr (List.append acc (extractCP current)) is 
                                Ok str -> List.append strs str
                                Err _ -> crash "UNREACHABLE: got invalid UTF-8 codepoints in extended grapheme cluster"
                        _ -> strs

                advance
                    nextState
                    nextAcc
                    nextStrs    

        # We have only one CodePoint left
        [last] -> 
            
            nextState = gbpRules prevState (extractGBP last)
                
            # If we are breaking on this CodePoint, then we need to reset accumulated CPs
            nextAcc = 
                when nextState is
                    Break -> []
                    _ -> List.append acc (extractCP last)

            # If we are breaking on this CodePoint, then we need to add the current
            nextStrs =
                when nextState is
                    Break -> 
                        when CodePoint.toStr (List.append acc (extractCP last)) is 
                            Ok str -> List.append strs str
                            Err _ -> crash "UNREACHABLE: got invalid UTF-8 codepoints in extended grapheme cluster"
                    _ -> strs

            advance
                nextState
                nextAcc
                nextStrs          

# TODO GB9c Do not break within certain combinations. ref [Indic_Conjunct_Break](https://unicode.org/reports/tr44/#Indic_Conjunct_Break)
# TODO GB11 Do not break within emoji modifier sequences or emoji zwj sequences. ref [Extended_Pictographic](https://unicode.org/reports/tr44/#Extended_Pictographic)
# TODO GB12, GB13 Do not break within emoji flag sequences
gbpRules : GCBPState, GBP -> GCBPState
gbpRules = \prevState, gbp ->
    when (prevState, gbp) is
        # GB3, GB4, GB5 Do not break between a CR and LF. Otherwise, break before and after controls
        (AfterCR, LF) -> Break
        (_, CR) -> AfterCR
        # GB6, GB7, GB8 Do not break Hangul syllable sequences.
        (_, L) -> AfterHungulL
        (AfterHungulL, V) -> AfterHungulLVorV
        (AfterHungulL, LV) -> AfterHungulLVorV
        (AfterHungulL, LVT) -> AfterHungulLVTorT
        (AfterHungulLVorV, V) -> AfterHungulLVorV
        (AfterHungulLVorV, T) -> AfterHungulLVTorT
        (AfterHungulLVTorT, T) -> AfterHungulLVTorT
        # GB9b Do not break after Prepend characters
        (_, Prepend) -> DontBreak
        # GB1, GB2 Break at the start and end of text, unless the text is empty
        _ -> Break
        
GCBPState : [
    DontBreak,
    Break, # can break on current
    AfterCR, # need to look ahead to next gbp
    AfterHungulL,
    AfterHungulLVorV,
    AfterHungulLVTorT,
    AfterRI,
]

# WIP add emoji handling to text segmentation
takeGb11 : List CodePoint -> Result { acc: List CodePoint, rest : List CodePoint } [NotEmojiSequence] 
takeGb11 = \cps ->
    takeGb11Help 
        {
            acc: List.withCapacity 10, # TODO is there a better default capacity?
            state: Start,
            rest: cps,
        }

Gb11State : [Start, AfterPictographic, AfterExtend, AfterZWJ]

# State Machine for GB11 "Do not break within emoji modifier sequences or emoji zwj sequences"
takeGb11Help : { acc: List CodePoint, state: Gb11State, rest : List CodePoint} -> Result { acc: List CodePoint, rest : List CodePoint } [NotEmojiSequence] 
takeGb11Help = \{ acc, state, rest } ->
    when rest is 
        [] -> Err NotEmojiSequence
        [cp, ..] ->
            when state is 
                Start if InternalEmoji.isPictographic (CodePoint.toU32 cp) -> 
                    {
                        acc: List.append acc cp,
                        state: AfterPictographic,
                        rest: List.dropFirst rest 1,
                    }
                    |> takeGb11Help # next cp 
                AfterPictographic if InternalGBP.isExtend (CodePoint.toU32 cp) -> 
                    {
                        acc: List.append acc cp,
                        state: AfterExtend,
                        rest: List.dropFirst rest 1,
                    }
                    |> takeGb11Help # next cp 
                AfterPictographic if InternalGBP.isZWJ (CodePoint.toU32 cp) -> 
                    {
                        acc: List.append acc cp,
                        state: AfterZWJ,
                        rest: List.dropFirst rest 1,
                    }
                    |> takeGb11Help # next cp 
                AfterPictographic -> 
                    {
                        acc: List.append acc cp,
                        rest: List.dropFirst rest 1,
                    }
                    |> Ok
                AfterExtend if InternalGBP.isExtend (CodePoint.toU32 cp) -> 
                    {
                        acc: List.append acc cp,
                        state: AfterExtend,
                        rest: List.dropFirst rest 1,
                    }
                    |> takeGb11Help # next cp 
                AfterExtend if InternalGBP.isZWJ (CodePoint.toU32 cp) -> 
                    {
                        acc: List.append acc cp,
                        state: AfterZWJ,
                        rest: List.dropFirst rest 1,
                    }
                    |> takeGb11Help # next cp 
                AfterZWJ if InternalEmoji.isPictographic (CodePoint.toU32 cp) -> 
                    {
                        acc: List.append acc cp,
                        state: AfterPictographic,
                        rest: List.dropFirst rest 1,
                    }
                    |> takeGb11Help # next cp 
                _ -> 
                    Err NotEmojiSequence

expect InternalEmoji.isPictographic 0x1F468
expect InternalGBP.isZWJ 0x200D


