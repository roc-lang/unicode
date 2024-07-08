app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.11.0/SY4WWMhWQ9NvQgvIthcv15AUeA7rAIJHAHgiaSHGhdY.tar.br",
    unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
}

import pf.Stdout
import unicode.CodePoint

word = "ẇ͓̞͒͟͡ǫ̠̠̉̏͠͡ͅr̬̺͚̍͛̔͒͢d̠͎̗̳͇͆̋̊͂͐"

maybeLength : Result U64 CodePoint.Utf8ParseErr
maybeLength = word |> Str.toUtf8 |> CodePoint.parseUtf8 |> Result.map List.len

main =
    when maybeLength is
        Ok count -> Stdout.line "\n\nThere are a total of $(Num.toStr count) code points in $(word)\n\n"
        Err _ -> crash "ERROR: Unable to parse $(word)!"

