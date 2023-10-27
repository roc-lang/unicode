interface Grapheme
    exposes [
        Grapheme,
        split,
    ]
    imports [
        CodePoint.{ CodePoint, Utf8ParseErr },
        InternalGBP.{ GBP },
        InternalCP,
    ]

## Extended Grapheme Cluster
Grapheme : InternalGBP.GBP

# TODO: benchmark to find a better capacity, just guessing at a reasonable 
# capacity here for the number of substrings
defaultCapacity = 50

## Split a string into extended graphemes clusters
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

isExtendOrZWJ : (CodePoint, GBP) -> Bool
isExtendOrZWJ = \(_, gbp) -> gbp == Extend || gbp == ZWJ

extractCP : (CodePoint, GBP) -> CodePoint
extractCP = .0

extractGBP : (CodePoint, GBP) -> GBP
extractGBP = .1
        
splitHelp : List (CodePoint, GBP), GCBPState, List CodePoint, List Str  -> List Str
splitHelp = \cpsWithGpbs, prevState, acc, strs ->

    # Set up helper to advance to the next CP recursively and update state 
    advance = \nextState, nextAcc, nextStrs -> 
        splitHelp
            (List.drop cpsWithGpbs 1)
            nextState
            nextAcc
            nextStrs

    when cpsWithGpbs is 
        # Return the accumulated strings if we have no more CodePoints
        [] -> strs

        # Look ahead if we have more than one CodePoint left
        [current, next, ..] ->
            if isExtendOrZWJ next then 
                # GB9 Do not break before extending characters or ZWJ.
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

# GBP : [CR, Control, Extend, ZWJ, RI, Prepend, SpacingMark, V, T, LF, LVT, LV, L, Other]
gbpRules : GCBPState, GBP -> GCBPState
gbpRules = \prevState, gbp ->
    when (prevState, gbp) is
        # GB6, GB7, GB8 Do not break Hangul syllable sequences.
        (_, L) -> AfterHungulL
        (AfterHungulL, V) -> AfterHungulLVorV
        (AfterHungulL, LV) -> AfterHungulLVorV
        (AfterHungulL, LVT) -> AfterHungulLVTorT
        (AfterHungulLVorV, V) -> AfterHungulLVorV
        (AfterHungulLVorV, T) -> AfterHungulLVTorT
        (AfterHungulLVTorT, T) -> AfterHungulLVTorT
        # GB3, GB4, GB5 Do not break between a CR and LF. Otherwise, break before and after controls
        (AfterCR, LF) -> Break
        (_, CR) -> AfterCR
        # GB1, GB2 Break at the start and end of text, unless the text is empty
        _ -> Break
    
        
GCBPState : [
    DontBreak,
    Break, # can break on current
    AfterCR, # need to look ahead to next gbp
    AfterHungulL,
    AfterHungulLVorV,
    AfterHungulLVTorT, 
]

 