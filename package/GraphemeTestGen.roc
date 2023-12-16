app "gen"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Path.{ Path },
        pf.Arg,
        pf.File,
        "data/GraphemeBreakTest-15.1.0.txt" as file : Str,
        Helpers,
        CodePoint,
        InternalCP,
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
        Ok arg -> Task.ok (Path.fromStr "\(Helpers.removeTrailingSlash arg)/GraphemeTest.roc")
        Err _ -> Task.err "USAGE: roc run GraphemeTest.roc -- path/to/package/"

writeToFile : Path -> Task {} Str
writeToFile = \path ->
    File.writeUtf8 path template
    |> Task.mapErr \_ -> "ERROR: unable to write to \(Path.display path)"
    |> Task.await \_ -> Stdout.line "\nSucessfully wrote to \(Path.display path)\n"

lines : Str
lines =
    file
    |> Str.split "\n"
    |> List.mapWithIndex \line, idx -> (line, idx)
    |> List.keepIf \(line, _) -> Str.startsWith line "÷ "
    |> List.map \(l, idx) ->
        when Str.split l "#" is 
            [exampleStr, ..] -> 
                when splitIntoCPsAndStrs exampleStr is 
                    Ok (cps, strs) -> 
                        
                        cpsStr = 
                            cps 
                            |> List.map Num.toStr 
                            |> Str.joinWith ","
                        
                        strsStr =
                            strs
                            |> toCodePointList
                            |> List.map \subStr -> subStr |> List.map Num.toStr |> Str.joinWith ","
                            |> Str.joinWith "],["

                        sanitised = 
                            l
                            |> Str.replaceEach "÷" "%" # replace %
                            |> Str.replaceEach "×" "x" # replace X
                            |> Str.replaceEach "	" "" # remove tabs

                        testFileLineNumber = Num.toStr (idx + 1)
                            
                        """

                        expect # line \(testFileLineNumber) of GraphemeBreakTest-15.1.0.txt -- \(sanitised)
                            answer = Ok [[\(strsStr)]]
                            result = [\(cpsStr)] |> List.map InternalCP.fromU32Unchecked |> CodePoint.toStr |> Result.try Grapheme.split |> Result.map toCodePointList
                            result == answer
                        """

                    Err ParsingError -> crash "Error parsing line \(l)"
            _ -> crash "Error could not find '#' character in line \(l)"
    |> Str.joinWith "\n"

toCodePointList : List Str -> List (List U32)
toCodePointList = \strings ->
    strings |> List.map \str -> 
        when str |> Str.toUtf8 |> CodePoint.parseUtf8 is 
            Ok cps -> List.map cps CodePoint.toU32
            Err _ -> crash "expected valid utf8"

# we want to split into a list of code points and a list of strings that 
# represent the code points e.g.
splitIntoCPsAndStrs : Str -> Result (List U32, List Str) [ParsingError]
splitIntoCPsAndStrs = \str ->
    splitHelp (Str.toUtf8 str) [] ([],[])

splitHelp : List U8, List U32, (List U32, List Str) -> Result (List U32, List Str) [ParsingError]
splitHelp = \bytes, acc, (cps, strs) -> 
    when bytes is 
        [] -> Ok (cps, strs)
        [first, second, ..] -> 
            # ignore whitespace
            if first == ' ' then 
                splitHelp (List.dropFirst bytes 1) acc (cps, strs)
            # NoBREAK
            else if first == 195 && second == 151 then 
            
                # do nothing, code points are added to accum when seen
                splitHelp (List.dropFirst bytes 2) acc (cps, strs)

            # BREAK
            else if first == 195 && second == 183 then 

                # take accum and make a str
                str =
                    when List.map acc InternalCP.fromU32Unchecked |> CodePoint.toStr is 
                        Ok s1 -> s1
                        Err _ -> crash "UNREACHABLE: got invalid UTF-8 in test data file"
                        
                if str == "" then 
                    # do nothing, no code points in accum
                    splitHelp (List.dropFirst bytes 2) acc (cps, strs) 
                else 
                    splitHelp (List.dropFirst bytes 2) [] (cps, List.append strs str)

            # parse code point add to accum
            else if Helpers.isHex first then 
                { val: hexBytes, rest } = Helpers.takeHexBytes { val: [], rest: bytes }
            
                cp = Helpers.hexBytesToU32 hexBytes

                splitHelp rest (List.append acc cp) ((List.append cps cp), strs)
            else 
                Err ParsingError
        [_] -> Ok (cps, strs) # last byte is always a space ' ' 
        
template = 
    """
    ## WARNING This file is automatically generated. Do not edit it manually. ##
    interface GraphemeTest
        exposes []
        imports [CodePoint, Grapheme, InternalCP]


    toCodePointList : List Str -> List (List U32)
    toCodePointList = \\strings ->
        strings |> List.map \\str -> 
            when str |> Str.toUtf8 |> CodePoint.parseUtf8 is 
                Ok cps -> List.map cps CodePoint.toU32
                Err _ -> crash \"expected valid utf8\"

    \(lines)
    """