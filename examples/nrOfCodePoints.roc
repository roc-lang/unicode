app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import pf.Task exposing [Task]
import unicode.CodePoint exposing [Utf8ParseErr]

## Get the number of code points for a given Str
nrOfCodePoints : Str -> Result U64 Utf8ParseErr
nrOfCodePoints = \str ->
    str |> Str.toUtf8 |> CodePoint.parseUtf8 |> Result.map List.len

main =
    word = "ẇ͓̞͒͟͡ǫ̠̠̉̏͠͡ͅr̬̺͚̍͛̔͒͢d̠͎̗̳͇͆̋̊͂͐"

    when nrOfCodePoints word is
        Ok nr ->
            Stdout.line "String \"$(word)\" consists of $(Num.toStr nr) code points."

        Err _ ->
            Task.err (Exit 1 "Failed to parse string $(word) as Utf8.")

