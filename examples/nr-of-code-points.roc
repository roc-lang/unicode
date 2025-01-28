app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.CodePoint exposing [Utf8ParseErr]

## Get the number of code points for a given Str
nr_of_code_points : Str -> Result U64 Utf8ParseErr
nr_of_code_points = |str|
    str |> Str.to_utf8 |> CodePoint.parse_utf8 |> Result.map_ok(List.len)

main! = |_args|
    word = "ẇ͓̞͒͟͡ǫ̠̠̉̏͠͡ͅr̬̺͚̍͛̔͒͢d̠͎̗̳͇͆̋̊͂͐"

    when nr_of_code_points(word) is
        Ok(nr) ->
            Stdout.line!("String \"${word}\" consists of ${Num.to_str(nr)} code points.")

        Err(_) ->
            Err(Exit(1, "Failed to parse string ${word} as Utf8."))
